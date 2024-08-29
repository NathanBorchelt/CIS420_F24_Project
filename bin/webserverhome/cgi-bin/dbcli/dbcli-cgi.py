#!/usr/bin/env python3
# (Or specify exact path on some platforms)

#    Copyright (C) 2013 Konstantin S. Solnushkin
#
#    "dbcli-cgi.py", the web interface to the "dbcli" tool.
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU Affero General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU Affero General Public License for more details.
#
#    You should have received a copy of the GNU Affero General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

import cgi
import cgitb; cgitb.enable()
import sys
import os
import subprocess
import tempfile
import shutil

# Name of this design module (used for error messages):
module_name_str = 'Module "dbcli-cgi"'
# Some constants
# Name of the executable that we will call
dbcli = "dbcli"
# If we are on Windows, add the executable extension
if os.name == 'nt':
  dbcli = dbcli + ".exe"
# Use the executable in the current directory:
path_to_dbcli = ""

# Define a function that prints the main HTML form
def print_main_form():
  """Prints main form in HTML format."""
  print("Content-type: text/html")
  print()
  with open('main-form.html', 'r') as main_form_file:
    main_form_str = main_form_file.read()
    print(main_form_str)

# Define a function that prints a machine-readable error status
def print_error(error_str):
  """Prints error status."""
  print("Content-type: text/plain")
  print()
  print("Status=Error")
  print("Error_message=" + module_name_str + ': ' + error_str)

def print_header():
  """Prints a "text/plain" content type and a blank line"""
  print("Content-type: text/plain")
  print()
  
def save_file(dirname, fileid):
    """Accepts a cgi.FieldStorage() instance. Saves contents of the specified file
    on the disk under a supplied filename"""
    # Strip any leading directory names to prevent directory traversal attacks
    name = os.path.basename(fileid.filename)
    fullpath = os.path.join(dirname, name)
    with open(fullpath, mode="w+b") as f:
        f.write(fileid.file.read())

# -- End of function definitions --

# Get our own path
my_base_path = os.path.dirname(sys.argv[0])
# Change to that directory if we are not already there
if len(my_base_path) != 0:
  os.chdir(my_base_path)

# Prepare the path to "dbcli": join the current directory, path to "dbcli",
# and the executable name itself; then normalise the resulting path.
dbcli = os.path.normpath(os.path.join(my_base_path, path_to_dbcli, dbcli))
# Make sure that the executable exists
if not os.path.isfile(dbcli):
  print_error('File not found: ' + dbcli)
  exit()

# Read parameters of the submitted HTML form
form = cgi.FieldStorage()

# When a file is submitted for handling, item "masterfile" must be defined, otherwise print main HTML form
if "masterfile" not in form:
  print_main_form()
  exit()
  
# The user could also press the "Upload" button without selecting any file.
# In this case, the contents is an empty byte string (b''), and we also print the HTML form
if form.getfirst("masterfile") == b'':
  print_main_form()
  exit()

# If we reached here, then some file(s) were uploaded. Start with the master file.
masterfile = form["masterfile"]
# If we were mistakenly supplied multiple files in the "masterfile" field, then we have a list.
# This could be an attack, so we exit with a message.
if isinstance(masterfile, list):
  print_error("Only one file can be supplied as a master file")
  exit()
# If the file name was not supplied, this is again an incorrect request
if not masterfile.filename:
  print_error("You have to post a master file")
  exit()

# Create a temporary directory
with tempfile.TemporaryDirectory() as tempdir:
    # Create the master file in the temporary directory and write its contents
    save_file(tempdir, masterfile)
    # Some auxillary files could also have been uploaded
    auxfiles = form["auxfiles"]
    if isinstance(auxfiles, list):
        for i in auxfiles:
            if i.filename:
                save_file(tempdir, i)
    else:
        # A single auxillary file was uploaded
        if auxfiles.filename:
            save_file(tempdir, auxfiles)
    # Change to our temporary directory
    os.chdir(tempdir)
    # Construct the list of arguments
    arguments_list = [dbcli, '-i', os.path.basename(masterfile.filename)]
    try:
        # Run "dbcli" in that directory
        dbcli_output = subprocess.check_output(arguments_list, universal_newlines=True, stdin=subprocess.DEVNULL)
        # Change back to original directory (or we won't be able to remove the temporary one)
        os.chdir(my_base_path)
        # Print HTTP header and output
        print_header()
        print(dbcli_output)
    except subprocess.CalledProcessError as e:
        print_error('Command dbcli returned error code ' + str(e.returncode))
        print(e.output)
