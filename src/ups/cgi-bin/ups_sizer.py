#!/usr/bin/env python3.3
# (Or specify exact path on some platforms)

#    Copyright (C) 2012 Konstantin S. Solnushkin
#
#    "ups_sizer.py", the tool to choose an Uninterrupted Power Supply (UPS)
#    of appropriate size.
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
import csv

# Helpful advice: to test this script in Windows without starting
# any webserver whatsoever, use this command:
# set /P QUERY_STRING=
# then type value of the query string, such as:
# task=design&power=6500
# (without quotes!) and press "Enter". Now, your environment
# has been modified. Run the script:
# python ups_sizer.py
# The output will appear on your console.
# In GNU/Linux, use a single command line:
# REQUEST_METHOD="GET" QUERY_STRING="task=design&power=6500" ./ups_sizer.py
#
# Name of this design module (used for error messages):
module_name_str = 'Module "ups"'
# Some constants
# Name of the executable that we will call
dbcli = "dbcli"
# If we are on Windows, add the executable extension
if os.name == 'nt':
  dbcli = dbcli + ".exe"
# Use "dbcli" that is located in the upper-level directory that is also named "dbcli"
path_to_dbcli = ".." + os.sep + "dbcli"
# Name of the database. 2nd component can be a vendor name (say, "liebert"),
# and the 3rd component can be a product line name (say, "apm").
# This way, you can build an intricate directory structure to accommodate
# all your hardware. You can then get the 2nd and the 3rd components
# from the web form, which allows a user to select them from the browser.
xml_db_str = os.path.join('db','default','default.xml')
# Name of the file that stores the number of remaining runs
remaining_runs_file = "runs-remaining.txt"
# Whether we need to check for remaining runs. (Use "False" if you
# are running this CGI script on your own web server, and are not limited
# by available resources. Use "True" if you want to allow a limited number
# of runs during a specific time interval, and then set up a "cron" job to
# write a number of allowed runs into this file every 10 minutes or one hour).
need_remaining_runs_check = False
# Some UPS metrics change, and some do not, when we combine several UPS blocks
metrics_that_change = ["ups_cost", "ups_heat", "ups_power_rating", "ups_size_racks", "ups_weight"]
metrics_that_dont_change = ["ups_backup_time", "ups_model", "ups_partitioning"]


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
  my_list = [my_key + '=' + my_dict[my_key] for my_key in my_dict]
  # Sort the list for consistent output
  my_list.sort()
  # Print the items
  for item in my_list:
    print(item)
    
# Define a function that combines fields from two lists into a dictionary
def combine_fields(header_list, config_list):
  """Combines fields from two lists into a dictionary."""
  # Ignore words in "stop_list"
  stop_list = ["Configuration ID", "enabled"]
  result = dict()
  # Use of numerical index "i" appears to be unavoidable in this case:
  for i in range(len(header_list)):
    if header_list[i] in stop_list:
      continue
    result[header_list[i]] = config_list[i]
  # Return this dictionary as the function result
  return result

# Define a function that receives a list of lines in CSV format,
# breaks each line into separate fields, and returns two lists --
# one for the first line, and the other one for the second line
def get_two_lines(lines_list):
  """Parses lines in CSV format, and returns the first two lines."""
  # Split the lines into fields
  csv_reader = csv.reader (lines_list)
  # Read only two lines (the header with field names, and the first
  # configuration) into separate lists
  # XXX: Can we do it in a neat way?
  i = 0
  for row in csv_reader:
    i += 1
    if i == 1:
      first_line_list = row
    elif i == 2:
      second_line_list = row
    else:
    # Don't read any more
      break
  # Return both lists as sub-lists
  return [first_line_list, second_line_list]

# Define a function that queries the database and returns the best configuration,
# according to the specified metric, in a dictionary object
def get_ups_dict(sortby_str, reverse_sort, constraint_str):
  """Queries the database with sorting."""
  # Construct the list of arguments
  arguments_list = [dbcli, "-i", xml_db_str, "-s", sortby_str, "-d"]
  # If the string with constraints is non-empty, add the corresponding argument:
  if constraint_str != '':
    arguments_list.append("-c")
    arguments_list.append(constraint_str)
  # If reverse sort was requested, add one more argument
  if reverse_sort:
    arguments_list.append("-r")
  # XXX: Use the statement below for a debug output, to see what arguments "dbcli" receives:
  #print(repr(arguments_list))
  #
  # Call the tool. Notice that stdin is DEVNULL, otherwise the call will block
  dbcli_output = subprocess.check_output(arguments_list, universal_newlines=True, stdin=subprocess.DEVNULL)
  # Split the output into separate lines
  dbcli_lines_list = dbcli_output.splitlines()
  # Return the first two lines in separate sub-lists, broken into fields
  two_line_list = get_two_lines (dbcli_lines_list)
  # Combine fields from two lists into a dictionary
  ups_dict = combine_fields(two_line_list[0], two_line_list[1])
  # Return this as our result
  return ups_dict

# Define a function that recomputes the cost per kW of a UPS
def update_cost_per_kw(my_ups_dict):
  """Recalculates the cost per 1kW of UPS power rating"""
  my_ups_dict["ups_cost_per_kw"] = format(int(my_ups_dict["ups_cost"]) / int(my_ups_dict["ups_power_rating"]) * 1000, ".1f")
  return my_ups_dict

# Define a function that "multiplies" the UPS dictionary by a given number.
# It is used to recompute technical and economic characteristics of a set of
# identical UPS blocks.
def multiply_ups_dict(my_ups_dict, number_of_blocks):
  """Multiplies some metrics by a given factor."""
  # We only know how to handle certain types of metrics: some of them
  # should be multiplied, some should be kept as they are. Anything else
  # we don't know how to handle, so for safety we don't include them in
  # the output.
  # Create a new dictionary
  new_dict = {}
  # Only include metrics that we know how to handle:
  # either "metrics_that_change" or "metrics_that_dont_change"
  for my_metric in my_ups_dict:
    if my_metric in metrics_that_change:
      # Multiply it by the number of blocks and include:
      new_dict[my_metric] = str(int(my_ups_dict[my_metric]) * number_of_blocks)
    elif my_metric in metrics_that_dont_change:
      # Include as is:
      new_dict[my_metric] = my_ups_dict[my_metric]
    else:
      # Don't include for safety
      pass
  # Update the "cost per kW" metric
  update_cost_per_kw(new_dict)
  # Now, return the result
  return new_dict

# Define a function that takes metrics from the first dictionary, and "adds"
# to them values of metrics from the second dictionary
def add_ups_dict(my_ups_dict, second_dict):
  """Adds values of metrics from the second dictionary to the first one."""
  # Search through metrics in the second dictionary
  for my_metric in second_dict:
    # Only process those that can change
    if my_metric in metrics_that_change:
      # Add its value to the corresponding metric in the first dictionary
      # XXX: Only works for integer metrics...
      my_ups_dict[my_metric] = str(int(my_ups_dict[my_metric]) + int(second_dict[my_metric]))
  # Update the "ups_cost_per_kw" metric -- it probably has changed
  my_ups_dict = update_cost_per_kw(my_ups_dict)
  return my_ups_dict

# Define a function that checks if we still can run the CGI script
def check_remaining_runs():
  """Checks a file to see if the number of remaining run attempts is not zero"""
  # Default is no success
  result = False
  try:
    # Try to read the contents
    runs_file = open(remaining_runs_file, 'r')
    runs = int(runs_file.readline())
    if runs > 0:
      # Returns "True", decrements the counter and writes it back to the file
      result = True
      runs = runs - 1
      runs_file.close()
      runs_file = open(remaining_runs_file, 'w')
      runs_file.write(str(runs))
      runs_file.close()
  except:
    # File not found, or empty, or has erroneous content, or...
    # Return "False" and write zero
    runs_file = open(remaining_runs_file, 'w')
    runs_file.write('0')
    runs_file.close()
  finally:
    return result

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

# "task" must be defined, otherwise print main HTML form
if "task" not in form:
  print_main_form()
  exit()

# If "task" is anything else than "design", then print help and exit
if form.getfirst("task") != "design":
  print_help()
  exit()

# Check if we have some remaining runs
if need_remaining_runs_check:
  if not check_remaining_runs():
    print_error('Server temporarily throttled, please come back later')
    exit()

# Get the value of "power"
if "power" in form:
  power_int = int(form.getfirst("power"))
else:
  print_error('Required parameter "power" not specified')
  exit()
  
# "Power" must be positive
if power_int <= 0:
  print_error('Parameter "power" must be positive')
  exit()

# Get the optional value of "min_ups_backup_time"
if "min_ups_backup_time" in form:
  min_ups_backup_time_int = int(form.getfirst("min_ups_backup_time"))
else:
  min_ups_backup_time_int = 0

# "min_ups_backup_time" must be non-negative
if min_ups_backup_time_int < 0:
  print_error('Parameter "min_ups_backup_time" must be non-negative')
  exit()

# If we reached here, all checks were passed, do the serious stuff.

# Parameter "power" passed by the user will be used to construct the
# constraint for the minimum power rating of the UPS system
min_ups_power_rating_constraint = "min_ups_power_rating=" + str(power_int)

# Value of "min_ups_backup_time" is also used as a constraint,
# if it is non-zero
if min_ups_backup_time_int > 0:
  min_ups_backup_time_constraint = "min_ups_backup_time=" + str(min_ups_backup_time_int)
else:
  min_ups_backup_time_constraint = ''

# Is there an UPS configuration big enough to meet all our power needs?
# Get the list of configurations, sorted by UPS cost. If the list is
# non-empty, get the first one, which is the cheapest.
try:
  # Use the constraint on minimum power rating
  constr_str = min_ups_power_rating_constraint
  # If minimum backup time is specified, use it as well
  if min_ups_backup_time_constraint != '':
    constr_str += ', ' + min_ups_backup_time_constraint

  # Query the tool, in attempt to find a configuration with the lowest cost.
  # Pass the constraints.
  ups_dict = get_ups_dict(sortby_str="ups_cost", reverse_sort=False, constraint_str=constr_str)
  # Save the changes made to UPS partitioning info
  ups_partitioning_str = '1*' + ups_dict["ups_power_rating"]
  ups_dict["ups_partitioning"] = ups_partitioning_str
  # Then, print the resulting dictionary
  print_header()
  print_dict(ups_dict)
  # All done, user is happy
  exit()
except subprocess.CalledProcessError:
  # If we reached here, then probably "dbcli" returned a non-zero error code.
  # This happens when the list returned by "dbcli" is empty: there are no UPS
  # configurations that suit our constraints. We will try to use multiple UPS
  # blocks (We had to catch this exception, otherwise the script would bail out).
  pass

# Now that there was no single UPS that would be sufficient, we have to use
# several UPS blocks (either combined into a single large system in parallel,
# or simply each of them serving their own part of your cluster computer).
# We employ a "greedy algorithm": choose the UPS configuration that has the
# lowest cost per kW of power rating (lowest "ups_cost_per_kw"), and use as
# many of them as necessary to meet almost all of the power requirements.
# Then, deduce how much more power we still need, and supply that with the
# suitable cheapest UPS (lowest "ups_cost").
# Example: a 45kW configuration has the cheapest cost per kW. To supply 200kW
# of power, we will need 200//45=4 such configurations. This provides
# 45*4=180 kW. The remaining 200-180=20 kW are best provided with a 30kW
# configuration (this is cheaper than another 45kW block).
# Thefore, the solution is four 45kW blocks + one 30kW block.

try:
  # Query the tool to find a configuration with the lowest cost per kW.
  # This time we don't impose any constraints on power rating, but still use
  # the constraint on backup time (for example, if you need backup time of
  # 2500 seconds, you can only get it with 15kW blocks -- not with 30kW or
  # 45kW blocks)
  ups_dict = get_ups_dict(sortby_str="ups_cost_per_kw", reverse_sort=False, constraint_str=min_ups_backup_time_constraint)
  # Number of big blocks required to almost completely satisfy the requirements ("//" indicates integer division)
  big_blocks_int = power_int // int(ups_dict["ups_power_rating"])
  # Power rating of a single such big block
  big_block_power_rating_int = int(ups_dict["ups_power_rating"])
  # "Multiply" the current configuration by a given factor
  ups_dict = multiply_ups_dict(ups_dict, big_blocks_int)
  # Save the trivial UPS partitioning
  ups_partitioning_str = str(big_blocks_int) + '*' + str(big_block_power_rating_int)
  # How much power do we still need?
  remaining_power_int = power_int - int(ups_dict["ups_power_rating"])

  # If remaining power is not zero, we need to add one more UPS block
  if remaining_power_int > 0:
    # Find the cheapest single UPS that can cover the remaining needs
    # Include constraints for power and for backup time
    # Use the constraint on minimum power rating
    constr_str = "min_ups_power_rating=" + str(remaining_power_int)
    # If minimum backup time is specified, use it as well
    if min_ups_backup_time_constraint != '':
      constr_str += ', ' + min_ups_backup_time_constraint
    small_ups_dict = get_ups_dict(sortby_str="ups_cost", reverse_sort=False, constraint_str=constr_str)
    # Add metrics from the single small UPS to the main set of UPS blocks
    ups_dict = add_ups_dict (ups_dict, small_ups_dict)

    # Record the partitioning of the system into blocks for user's convenience
    # Is the last block the same as the previous "big" blocks?
    # XXX: Comparison is based on power ratings
    if int(small_ups_dict["ups_power_rating"]) == big_block_power_rating_int:
      # The same, add "1" to the current number of blocks
      ups_partitioning_str = str(big_blocks_int + 1) + '*' + str(big_block_power_rating_int)
    else:
      # The last, smaller block, is different
      ups_partitioning_str = str(big_blocks_int) + '*' + str(big_block_power_rating_int) + '+' + '1' + '*' + small_ups_dict["ups_power_rating"]
	  
  # Save the changes made to UPS partitioning info
  ups_dict["ups_partitioning"] = ups_partitioning_str
  
  # Print the result and exit
  print_header()
  print_dict(ups_dict)
  exit()
except subprocess.CalledProcessError:
  # If we reached here, we were ultimately unsuccessful. As for the power
  # requirements, they can be satisfied by simly taking more UPS blocks.
  # The most probable reason for failure is a too strict constraint
  # on backup time, which could not be met by any configuration that we have
  # at hand (the other reason could be that something was wrong with the
  # "dbcli" tool).
  print_error('No UPS configurations match your constraints')
