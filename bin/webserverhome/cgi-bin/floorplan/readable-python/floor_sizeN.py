#This my rendition of tkaing Konstantin's original python code and trying to make it more readable
#as he uses lots of abriviations that can be difficult to recofnize at a glance

#!/usr/bin/env python3
# (Or specify exact path on some platforms)

#    Copyright (C) 2013 Konstantin S. Solnushkin
#
#    This file is part of "floor sizer", the tool to determine the floor space
#    required for a certain number of racks with computer equipment.
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

# Our own modules
import floorplan

# Helpful advice: to test this script in Windows without starting
# any webserver whatsoever, use this command:
# set /P QUERY_STRING=
# then type value of the query string, such as:
# task=design&racks=12
# (without quotes!) and press "Enter". Now, your environment
# has been modified. Run the script:
# python floor_size.py
# The output will appear on your console.
# In GNU/Linux, use a single command line:
# REQUEST_METHOD="GET" QUERY_STRING="task=design&racks=12" ./floor_size.py
#
#
# Name of this design module (used for error messages):
module_name_str = 'Module "floor_size"'
# Some constants
# Name of the file that stores the number of remaining runs
remaining_runs_file = "runs-remaining.txt"
# Whether we need to check for remaining runs. (Use "False" if you
# are running this CGI script on your own web server, and are not limited
# by available resources. Use "True" if you want to allow a limited number
# of runs during a specific time interval, and then set up a "cron" job to
# write a number of allowed runs into this file every 10 minutes or one hour).
need_remaining_runs_check = False


# Define a function that prints help
def print_help():
    """Prints help in HTML format."""
    print("Content-type: text/html")
    print()
    with open('help.html', 'r') as help_file:
        help_str = help_file.read()
        print(help_str)

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

# Define a function that prints a dictionary
def print_dict(my_dict):
    """Prints dictionary in "key=value" format."""
    # The so called "list comprehension":
    my_list = [my_key + '=' + str(my_dict[my_key]) for my_key in my_dict]
    # Sort the list for consistent output
    my_list.sort()
    # Print the items
    for item in my_list:
        print(item)

# Define a function that checks if we still can run the CGI script
def check_remaining_runs():
    """Checks a file to see if the number of remaining run attempts is not zero"""
    # Default is no success
    result = False
    try:
        # Try to read the contents
        with open(remaining_runs_file, 'r') as runs_file:
            runs = int(runs_file.readline())
            if runs > 0:
                # Returns "True", decrements the counter and writes it back to the file
                result = True
                runs = runs - 1

        with open(remaining_runs_file, 'w') as runs_file:
            runs_file.write(str(runs))

    except:
        # File not found, or empty, or has erroneous content, or...
        # Return "False" and write zero
        with open(remaining_runs_file, 'w') as runs_file:
            runs_file.write('0')
    finally:
        return result

# -- Default values: --
defaultRackWidth = 0.6   # Rack width
defaultRackDepth = 1.2   # Rack depth
# Clearances:
defaultRackSideClearence = 1.0 # Side
defaultRackFrontClearence = 1.0 # Front
defaultRackAisleClearence = 1.0 # Aisle
# Maximal length of a contiguous block of racks:
defaultRackMaxRowLength = 6.0

# -- End of function definitions --

# Get our own path
my_base_path = os.path.dirname(sys.argv[0])
# Change to that directory if we are not already there
if len(my_base_path) != 0:
    os.chdir(my_base_path)

# Read parameters of the submitted HTML form
form = cgi.FieldStorage()

# "task" must be defined, otherwise print main HTML form
if "task" not in form:
    print_main_form()
    exit()

# If "task" is anything else than "design", then print help and exit
if form.getfirst("task") != "design":
    print_help()
    exit()

# Create a new dictionary to store script output
output_dict = {}

# Check if we have some remaining runs
if need_remaining_runs_check:
    if not check_remaining_runs():
        print_error('Server temporarily throttled, please come back later')
        exit()

# Get the value of "racks"
if "racks" in form:
    racks_str = form.getfirst("racks")
    racks_int = int(racks_str)
    output_dict["racks"] = racks_int
else:
    print_error('Required parameter "racks" not specified')
    exit()
# "racks" must be positive
if racks_int <= 0:
    print_error('Parameter "racks" must be positive')
    exit()

# Note: all form values gathered below are optional, and all of them are floating-point values.

# Get the value of "w" (rack width)
if "w" in form:
    rackWidth= float(form.getfirst("w"))
else:
    rackWidth= defaultRackWidth
# Save for future output
output_dict["w"] = rackWidth
# "w" must be positive
if rackWidth<= 0:
    print_error('Parameter "w" must be positive')
    exit()

# Get the value of "d" (rack depth)
if "d" in form:
    rackDepth = float(form.getfirst("d"))
else:
    rackDepth = defaultRackDepth
# Save for future output
output_dict["d"] = rackDepth
# "d" must be positive
if rackDepth <= 0:
    print_error('Parameter "d" must be positive')
    exit()

# Get the value of "c_s" (side clearance)
if "c_s" in form:
    clearanceSide = float(form.getfirst("c_s"))
else:
    clearanceSide = defaultRackSideClearence
# Save for future output
output_dict["c_s"] = clearanceSide
# "clearanceSide" must be non-negative
if clearanceSide < 0:
    print_error('Parameter "c_s" must be non-negative')
    exit()

# Get the value of "c_f" (front clearance)
if "c_f" in form:
    clearanceFront = float(form.getfirst("c_f"))
else:
    clearanceFront = defaultRackFrontClearence
# Save for future output
output_dict["c_f"] = clearanceFront
# "clearanceFront" must be non-negative
if clearanceFront < 0:
    print_error('Parameter "c_f" must be non-negative')
    exit()

# Get the value of "c_a" (aisle width)
if "c_a" in form:
    clearanceAisle = float(form.getfirst("c_a"))
else:
    clearanceAisle = defaultRackAisleClearence
# Save for future output
output_dict["c_a"] = clearanceAisle
# "c_a" must be non-negative
if clearanceAisle < 0:
    print_error('Parameter "c_a" must be non-negative')
    exit()

# Get the value of "l_xc" (max length of long rows)
if "l_xc" in form:
    maxRowLength = float(form.getfirst("l_xc"))
else:
    maxRowLength = defaultRackMaxRowLength
# Save for future output
output_dict["l_xc"] = maxRowLength
# "l_xc" must be non-negative
if maxRowLength < 0:
    print_error('Parameter "l_xc" must be non-negative')
    exit()
# A special case of "l_xc=0" denotes to ignore the maximal length of rows:
if maxRowLength == 0:
    maxRowLength = float('inf')


# Calculate the floor space
floorplan.calculate_floor_space_size(output_dict)

# Print the output dictionary
print_header()
print_dict(output_dict)
