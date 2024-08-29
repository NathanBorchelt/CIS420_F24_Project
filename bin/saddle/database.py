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

import csv
import re
# Our own modules:
import dbcliquery
import conflist
import common
from strconst import *

def combine_fields(header_list, config_list):
    """Combines fields from two lists into a dictionary.
    Used to match CSV headers with corresponding fields.
    Borrowed almost intact from the "ups_sizer.py" tool.
    """
    result = dict()
    # Use of numerical index "i" appears to be unavoidable in this case:
    for i in range(len(header_list)):
        result[header_list[i]] = config_list[i]
    # Return this dictionary as the function result
    return result

def open_database(files):
    '''Open the equipment database using "files", which is a list of file names represented as strings.
    The first file name listed is assumed to be the "main" file of the database, the others (if present)
    are auxiliary files referenced by the main one.
    '''
    # If we were provided a string, convert it to a list
    if isinstance(files, str):
        files = [files]
    
    # If no files specified, exit
    if len(files) == 0:
        print(__name__ + ': Please specify file(s) to open')
        return
    
    # Fetch the first file name and try to determine its extension (the last 3 characters)
    # Lowercase the extension
    ext = (str(files[0])[-3:]).lower()
    
    if ext == 'xml':
        # In case of an XML file, query the "dbcli" tool and process all the files
        db = dbcliquery.dbcliquery(files)
        
        # In case of error, "None" value is returned
        if not db:
            print(__name__ + ': Call to "dbcli" service failed')
            return
    else:
        # Else assume it is a CSV file (no matter what the extension),
        # and simply load this first file, ignoring the others, if any
        with open(files[0], 'r', newline='') as f:
            db = f.read().splitlines()

    # If we reached here, "db" contains a list of lines in CSV format.
    # Strip whitespace, get rid of empty lines and those starting with the
    # comment character
    db_temp = []
    for line in db:
        # Strip whitespace
        line = line.strip()
        # If the line becomes empty, don't add; continue with the next iteration
        if len(line) == 0:
            continue
        # The line is non-empty, but could be a comment
        if line[0] == cCommentCharacter:
            continue
        # A normal line, save it
        db_temp.append(line)
    
    # If content is missing, exit
    if len(db_temp) == 0:
        print(__name__ + ': Content not found when loading XML or CSV file')
        return []

    # Read lines in CSV format into fields.
    # Each line becomes a list of string-typed fields.
    csv_reader_object = csv.reader(db_temp)
    
    # Parse lines and form a new list whose items are dictionaries
    i = 0
    db_list = []
    # Iterate until all items are read
    for row in csv_reader_object:
        i += 1
        if i == 1:
            # First line is the header
            header = row
            header_len = len(header)
        else:
            # This is the second or subsequent lines, so "header" is already defined
            temp_dict = {}
            for j in range(header_len):
                # Try to convert the string to a suitable type and save it to the dictionary.
                temp_dict[header[j]] = common.convert_str(row[j])
            # Add the dictionary into the list. Now, the line has been read.
            db_list.append(temp_dict)
    
    # Return the list of dictionaries
    return db_list

def save_database(config_list, filename):
    '''Saves the list of configurations under a specified file name'''
    # Make sure the list of configurations is not empty
    if len(config_list) == 0:
        print('List of configurations is empty, nothing to save')
        return
    # Make sure we were passed a single string as a file name
    if not isinstance(filename, str):
        print('Expecting a string as a file name')
        return
    # We don't know what fields may be available in all
    # configurations. The safest way is to step through all
    # configurations and add all fields into a set. This will
    # automatically result in a list of all possible fields
    # without repetitions
    column_names_set = set()
    for c in config_list:
        # Add each and every possible key
        for k in c.keys():
            column_names_set.add(k)
    # By the time we reached here, the set contains all possible
    # field names without repetitions. Convert it to a list
    # and sort the list for later usage
    column_names = sorted(list(column_names_set))
    # If there is a key that matches the regular expression, then it's
    # probably the configuration ID, and we want it to be in the first column
    first_field = ''
    for s in column_names:
        if id_field_regexp.match(s) != None:
            # One of the fields matched the regular expression, use it as the first field
            first_field = s
            break
    # If something matched, we want it to be the first field:
    if first_field != '':
        # First field, plus all other fields in sorted order
        column_names = [first_field] + sorted([s for s in column_names if s != first_field])
    # Open the file for writing. Note the use of "universal newlines"
    with open(filename, 'w', newline='') as f:
        # Create the writer object; quote as much as we can
        csv_dict_writer = csv.DictWriter(f, column_names, quoting=csv.QUOTE_ALL)
        # Write the header first
        csv_dict_writer.writeheader()
        # Write all rows
        csv_dict_writer.writerows(config_list)
    return

# Module initialisation

# Compile a regular expression that matches a string ending with letters "ID"
# (in any case), which can be preceded by a space or an underscore,
# or be the only two letters in a string. This will match strings such as
# "ID", "Configuration ID", "Config_id" and so on
id_field_regexp=re.compile('(^|.*[ _])[Ii][Dd]$')
