#    Copyright (C) 2013, 2014 Konstantin S. Solnushkin
#
#    This file is part of "SADDLE", a scripting language used to design
#    high-performance computer clusters.
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.


# If this file is called directly (not imported as a module):
if __name__ == "__main__":
    exit()

import urllib.request
import multipart

# The URL of the script that serves as a web interface to the "dbcli" tool
predefined_dbcli_url='http://localhost:8000/cgi-bin/dbcli/dbcli-cgi.py'

def dbcliquery(filenames, url=None):
    """Gets the list of file names, and queries the "dbcli" tool via web.
    Returns the output of "dbcli" -- lines in CSV format.
    """
    #print(filenames)
    
    # If no files specified, exit
    if len(filenames) == 0:
        print(__name__ + ': Please specify input file(s).')
        return
    
    # There are no form fields to send, so the dictionary is empty:
    fields = {}
    # Create a new list to store files to be uploaded
    files=[]    
    # The first file name is the "master" file of the database
    files.append(('masterfile', multipart.file_request_dict(filenames[0])))
    # The rest are auxiliary files, referenced by the master file using "include" directives:
    for f in filenames[1:]:
      files.append(('auxfiles', multipart.file_request_dict(f)))
    # Encode as a multipart HTML form data
    (data, headers) = multipart.encode_multipart(fields, files)
    
    # If no URL was supplied, use the predefined one
    if url is None:
        url = predefined_dbcli_url
    #print(url)

    try:
        # Send the request and parse the response
        request = urllib.request.Request(url, data=data, headers=headers)
        with urllib.request.urlopen(request) as resp:
            response = resp.read()
    except:
        print('"dbcli" service could not be contacted. Did you forget to run "startwebserver" script?')
        return None

    # Decode response from the byte string and split the lines nicely, then return the result
    return [l for l in bytes.decode(bytes(response)).splitlines()]
