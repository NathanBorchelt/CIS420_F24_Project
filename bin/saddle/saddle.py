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


# Import standard Python modules
import copy
import sys

# Our own units
import database
import conflist
import eqgroups
import racks
import cabling
import bom
import svgoutput
from strconst import *
from version import saddle_version_str

# If there are more than the below specified number of overhead
# cables above the row of racks, don't draw them to prevent clutter
overhead_cables_limit = 20

def status():
    '''Prints status of the current database'''
    # Use the globally defined variables
    global db, undo_stack
    # Print each string from the received list
    print('\n'.join(conflist.status(db, undo_stack)))

def display(number):
    '''Display a configuration with a given number. The number starts from one
    like people count, not machines'''
    # Use the globally defined variable "db"
    global db
    if number <= 0:
        print('Minimal value for index is one')
        return
    # Make sure we do have that entry in the database
    if number > len(db):
        print('Index out of range: we only have {} configuration(s)'.format(len(db)))
        return
    # All went well, display (n-1)th configuration
    conflist.display(db[number-1])

def d():
    '''A very basic function to display the database'''
    # Use the globally defined variable "db"
    global db
    for conf in db:
        print(conf)
        print()

def open_db(files):
    '''Opens the database of configurations'''
    # Use the globally defined variable "db"
    global db
    result = database.open_database(files)
    if len(result) == 0:
        print('> Error: No content found when opening database, hence not doing anything')
    else:
        # There was some content
        clear()
        db.extend(result)
        print('> Database opened')
        status()

def save_db(filename):
    '''Saves the current database in CSV format'''
    # Use the globally defined variable "db"
    global db
    database.save_database(db, filename)
    print('> Database saved')

def metric(expression):
    '''Calculates metric(s) specified in the expression'''
    # Use the globally defined variable "db"
    global db
    # Allow undo action
    save_undo()
    added_names = conflist.calculate_metric(db, expression)
    if added_names == None:
        # There was a gross error
        print('> Error calculating metric')
        return
    print('> Metric(s) "{}" calculated'.format(expression))
    # Print statistics for metrics that were evaluated
    for m in added_names:
        metric_stats(m)
    status()

def metric_stats(metric):
    '''Prints simple statistics for the specified metric'''
    # Use the globally defined variable "db"
    global db
    # Check if there are enabled configurations
    if len([1 for c in db if c[cEnabled] == True]) == 0:
        print('No enabled configurations, nothing to compute')
        return
    result = conflist.metric_statistics(db, metric)
    if result == None:
        # There was a gross error
        print('> Error calculating metric statistics')
        return
    # Number of enabled configurations is returned under key "length":
    num_enabled = result['length']
    if num_enabled > 1:
        # Provide more detailed statistics
        print('> Statistics on metric "{}" computed over {} enabled configuration(s):'.format(metric, num_enabled))
        print('> Minimal value: {}'.format(result['min']))
        print('> Median: {}'.format(result['median']))
        print('> Maximal value: {}'.format(result['max']))
    else:
        # Only one configuration is enabled, so there is not much to report.
        # "min", "max" and "median" are all the same. Use "median" since it
        # returns an integer number when the fractional part is zero
        print('> In one enabled configuration, metric "{}" has value "{}"'.format(metric, result['median']))

def constraint(expression):
    '''Checks whether each configuration satisfies constraint(s)
    specified in the expression; if not, disables the configuration'''
    # Use the globally defined variable "db"
    global db
    # Allow undo action
    save_undo()
    result = conflist.impose_constraint(db, expression)
    if result == None:
        # There was a gross error
        print('> Error imposing constraint(s)')
        return
    print('> Constraint(s) "{}" imposed'.format(expression))
    status()

def delete():
    '''Deletes configurations marked as disabled'''
    # Use the globally defined variable "db"
    global db
    # Allow undo action
    save_undo()
    result = conflist.delete_disabled(db)
    db.clear()
    db.extend(result)
    print('> Disabled configurations were deleted')
    status()

def undo(steps=1):
    '''Undoes the last action by popping the list of configurations from the stack'''
    # Use the globally defined variables
    global db, undo_stack
    if len(undo_stack) == 0:
        print('> Nothing to undo')
        return
    if steps < 1:
        print('> Number of undo steps must be >= 1')
        return
    # Now, steps >= 1. Pop and discard steps-1 entries,
    # but make sure at least one entry remains. This allows
    # the user to simply specify a large number in "steps" to undo everything
    # (If steps = 1, this loop is not executed)
    for i in range(steps-1):
        # If only one entry remains, exit the loop prematurely
        if len(undo_stack) == 1:
            break
        # Pop, but discard the result
        undo_stack.pop()
    # By now, only one entry remains on the undo stack.
    # Pop this entry from the stack and return it
    result = undo_stack.pop()
    db.clear()
    db.extend(result)
    print('> Undo complete')
    status()

def save_undo():
    '''Saves current list of configuration to the stack for future undo actions'''
    # Use the globally defined variables
    global db, undo_stack
    # Store a deep copy
    undo_stack.append(copy.deepcopy(db))

def clear():
    '''Clears internal structures: the list of configurations, the undo stack, etc.
    Used when opening a new database, to work with a new group of hardware.'''
    global db, undo_stack
    db.clear()
    undo_stack.clear()

def select_best(metric, best='min'):
    '''Selects the best configuration according to the metric,
    and marks the rest as disabled'''
    # Use the globally defined variable "db"
    global db
    # Check if there are enabled configurations
    if len([1 for conf in db if conf[cEnabled] == True]) == 0:
        print('No enabled configurations, nothing to select')
        return
    # Allow undo action
    save_undo()
    result = conflist.select_best(db, metric, best)
    if result == None:
        # There was a gross error
        print('> Error when selecting the best configuration')
        return
    print('> Best configuration according to metric "{}" selected'.format(metric))
    status()

def select_random(count=1):
    '''Selects a specified number of random configurations,
    (by default, one) and disables the rest.'''
    # Use the globally defined variable "db"
    global db
    # Check if there are enabled configurations
    if len([1 for conf in db if conf[cEnabled] == True]) == 0:
        print('No enabled configurations, nothing to select')
        return
    # Allow undo action
    save_undo()
    result = conflist.select_random(db, count)
    if result == None:
        # There was a gross error
        print('> Error when selecting random configuration(s)')
        return
    print('> Random configuration(s) were selected')
    status()

def select(index=1):
    '''Selects only one specified configuration
    and disables the rest.'''
    # Use the globally defined variable "db"
    global db
    # Check if there are enabled configurations
    if len([1 for conf in db if conf[cEnabled] == True]) == 0:
        print('No enabled configurations, nothing to select')
        return
    # Allow undo action
    save_undo()
    result = conflist.select_one(db, index)
    if result == None:
        # There was a gross error
        print('> Error when selecting configuration')
        return
    print('> Configuration {} was selected'.format(index))
    status()

def add_group(name, debug=0, custom_db=None):
    '''Takes a single enabled configuration from the global database "db"
    (or from "custom_db", if specified) and adds it as a hardware group
    with a specified "name" to the list of equipment to be placed.'''
    # Use the globally defined variables
    # "rs" is rack size
    global eg, rs
    if custom_db is None:
        # No custom database specified; use the globally defined "db"
        global db
        # Choose enabled configuration(s)
        my_db = [conf for conf in db if conf[cEnabled] == True]
    else:
        # Use the supplied database. The use of the field "Enabled=True"
        # is not necessary, so we don't check for it.
        my_db = custom_db
    # Make sure there is only one configuration in the resulting list
    if len(my_db) != 1:
        print('> Expecting only one configuration while {} were specified'.format(len(my_db)))
        return
    # Use this configuration
    conf = my_db[0]
    if not isinstance(name, str):
        print('> Please specify a string value for the group name')
        return
    groups_tmp = eqgroups.add_equipment_group(name, conf, rs, debug)
    if groups_tmp == None:
        # There was a gross error
        print('> Error when adding equipment group "{}"'.format(name))
        return
    # All went well, add all created groups into a new dictionary
    new_groups = {'name': name, 'c': conf, cGroupList: groups_tmp}
    # Update the global variable
    eg.append(new_groups)
    print('> Group "{}" added'.format(name))

def place(place_params=None, other_first=False, debug=0):
    '''Places equipment into racks.
    "other_first" specifies whether all other equipment
    should be placed first (default is False, so it's
    placed last).'''
    # Use globally defined variables
    global eg, rs, rp, r, m
    result = racks.place_equipment(eg, rs, rp, m, place_params, other_first, debug)
    if result == None:
        # There was a gross error
        print('> Error when placing equipment into racks')
        return
    # If we reached here, there was no error
    # Save the list of racks into the global variable "r"
    r.clear()
    r.extend(result)
    print('> Equipment placed into racks; design-wide metrics updated')

def cables(debug=0):
    '''Routes cables between equipment groups.'''
    # Use globally defined variables
    global eg, rs, rp, c
    result = cabling.route_cables(eg, rs, rp, cl, debug)
    if result == None:
        # There was a gross error
        print('> Error when routing cables')
        return
    # If we reached here, there was no error.
    # Replace contents of "c" with the returned cable list
    c.clear()
    c.extend(result)
    print('> Cables routed')
    
def update_metrics(debug=0):
    '''Updates technical and economic metrics.'''
    # Use globally defined variables
    global db, m
    # Check if there are enabled configurations
    if len([1 for conf in db if conf[cEnabled] == True]) == 0:
        print('> No enabled configurations, nothing to process')
        return
    # Allow undo action
    save_undo()
    db = conflist.update_tech_econ_metrics(db, m, debug)
    print('> Technical and economic metrics were updated')
    status()
    
def performance(debug=0):
    '''For each enabled configuration, calls the performance model;
    the result is written back into the configuration.'''
    # Use the globally defined variables
    global db, performance_module
    # Check if there are enabled configurations
    if len([1 for conf in db if conf[cEnabled] == True]) == 0:
        print('No enabled configurations, nothing to process')
        return
    # Allow undo action
    save_undo()
    (db, error_count) = conflist.calculate_performance(db, performance_module, debug)
    if db == None:
        # There was a gross error
        print('Error when calculating performance, performing the undo')
        undo()
        return
    if error_count > 0:
        # There were errors
        print('> {} error(s) detected'.format(error_count))
        print('> (Possible action: undo and retry)')
        return
    print('> Performance calculated')
    status()

def inverse_performance(performance, debug=0):
    '''For each enabled configuration, calls the _inverse_ performance model;
    the result is written back into the configuration.'''
    # Use the globally defined variables
    global db, m, performance_module, inverse_performance_module
    # Check if there are enabled configurations
    if len([1 for conf in db if conf[cEnabled] == True]) == 0:
        print('No enabled configurations, nothing to process')
        return
    # Allow undo action
    save_undo()
    (db, error_count) = conflist.calculate_inverse_performance(
        db, performance, inverse_performance_module, performance_module, debug)
    if db == None:
        # There was a gross error
        print('Error when running the inverse performance model, performing the undo')
        undo()
        return
    if error_count > 0:
        # There were errors
        print('> {} error(s) detected'.format(error_count))
        print('> (Possible action: undo and retry)')
        return
    # If we reached here, there were no errors,
    # and we can update important metrics
    db = conflist.update_tech_econ_metrics(db, m, debug)
    print('> Number of cores and nodes calculated')
    status()

def performance_probe(config_num=1, debug=0):
    '''Makes a trial call to a performance model,
    supplying a specified configuration, and
    reports useful information from performance model.
    Configuration counting starts from 1;
    default is to use the first configuration.'''
    # XXX: I believe this function needs to be redesigned;
    # it's too long and obscure.
    # Use the globally defined variables
    global db, performance_module
    # Choose a configuration; counting starts from one, hence the decrement
    conf = db[config_num - 1]
    # Check that it is enabled
    if conf[cEnabled] == False:
        print('The specified configuration is not marked as enabled')
        return
    # Create a temporary list of configurations with just one item
    l = [ conf ]
    # Iterate through the list and print what we are going to send
    print('Specified metrics:')
    for m in performance_module['send']:
        try:
            print(' {}={}'.format(m, conf[m]))
        except KeyError:
            print('Cannot perform probe: required metric "{}" not found in the configuration'.format(m))
            return
    print()
    # Create a new dictionary from the existing performance model description
    pm = copy.deepcopy(performance_module)
    # Additional fields that we want to receive
    fields = ['time_to_solution',
              'max_rating_at_cores',
              'max_rating']
    # Append the fields
    for m in fields:
        pm['receive'].append(m)
    # Query performance with "cores=1"
    (l, error_count) = conflist.calculate_performance(l, pm, debug)
    if l == None:
        # There was a gross error
        print('Error when calculating performance')
        return
    if error_count > 0:
        # There was an error
        print('> {} error(s) detected'.format(error_count))
        print('> (Possible action: retry)')
        return
    # Analogously, iterate through the list and print what we received
    print('Received metrics:')
    for m in performance_module['send']:
        print(' {}={}'.format(m, conf[m]))
    print()
    # The performance model might have returned additional information;
    # print it if available
    print('Additional information:')
    # A flag variable
    additional_info_available = False
    for m in fields:
        if m in conf:
            print(' {}={}'.format(m, conf[m]))
            additional_info_available = True
    # If nothing was printed, inform the user
    if not additional_info_available:
        print(' (none returned)')
    print()

def network(debug=0):
    '''For each enabled configuration, calls the network design module;
    the result is written back into the configuration.
    The field "network_topology" must already be specified in the configuration,
    otherwise the network design module will use its own default value.'''
    # Use the globally defined variables
    global db, m, network_module
    # Check if there are enabled configurations
    if len([1 for conf in db if conf[cEnabled] == True]) == 0:
        print('> No enabled configurations, nothing to process')
        return
    # Allow undo action
    save_undo()
    (db, error_count) = conflist.design_network(db, network_module, debug)
    if db == None:
        # There was a gross error
        print('> Error when running the network design module, performing the undo')
        undo()
        return
    if error_count > 0:
        # There were errors
        print('> {} error(s) detected'.format(error_count))
        print('> (Possible action: undo and retry)')
        return
    # If we reached here, there were no errors,
    # and we can update important metrics
    db = conflist.update_tech_econ_metrics(db, m, debug)
    print('> Network designed')
    status()

def ups(debug=0):
    '''For each enabled configuration, calls the UPS design module;
    the result is written back into the configuration.'''
    # Use the globally defined variables
    global db, m, ups_module
    # Check if there are enabled configurations
    if len([1 for conf in db if conf[cEnabled] == True]) == 0:
        print('> No enabled configurations, nothing to process')
        return
    # Allow undo action
    save_undo()
    (db, error_count) = conflist.design_ups(db, ups_module, debug)
    if db == None:
        # There was a gross error
        print('> Error when running the UPS design module, performing the undo')
        undo()
        return
    if error_count > 0:
        # There were errors
        print('> {} error(s) detected'.format(error_count))
        print('> (Possible action: undo and retry)')
        return
    # If we reached here, there were no errors,
    # and we can update important metrics
    db = conflist.update_tech_econ_metrics(db, m, debug)
    print('> UPS subsystem designed')
    status()

def draw_rows(filename, row_list=[], max_cables=overhead_cables_limit, debug=0):
    '''Prints specified rows of racks in SVG format.'''
    # Use the globally defined variables
    global eg, r, rs, rp
    # Draw the rows
    result = svgoutput.draw_rows_svg(eg, r, rs, rp, filename, row_list, max_cables, debug)
    if result == None:
        # There was a gross error
        print('> Drawing rows failed')
        return
    # If we reached here, there was no error
    print('> Rows drawn')

def print_design(filename='', debug=0):
    '''Prints information about the current design. Includes bills
    of materials for all groups of equipment and cables, as well as
    rack placement information.'''
    # Use the globally defined variables
    global eg, c, m, rs, rp
    result = bom.print_all(eg, c, m, rs, rp, filename, debug)
    if result == None:
        # There was a gross error
        print('> Could not print design')
        return
    # If we reached here, there was no error
    if filename == '':
        # No filename -- printed to screen
        print('> Design information printed')
    else:
        print('> Design information saved to file "{}"'.format(filename))

def print_welcome_banner():
    print("""
Welcome to SADDLE ver. {}. Copyright (C) 2013-2014 Konstantin S. Solnushkin.
This program comes with ABSOLUTELY NO WARRANTY. This is free software,
distributed under the terms of the GNU General Public License.

To stay in interactive mode, always use the "-i" argument to Python
when invoking SADDLE from command like:

python -i {}

""".format(saddle_version_str, sys.argv[0]))


# --------------- INITIALISATION ---------------

# Welcome the user
# If this file is called directly (not imported as a module),
# then the user wanted to run SADDLE interactively rather than
# call it from a script:
if __name__ == "__main__":
    print_welcome_banner()

# "db" holds the database of configurations
db = []

# "eg" refers to the list of equipment groups
eg = []

# "r" is for racks
r = []

# "c" is for cables
c = []

# Length of standard cables
# Note: these particular lengths reflect InfiniBand cables made by Mellanox Technologies
cl = [ 0.5, 1, 2, 3, 5, 10, 15, 15, 20, 30, 50, 100 ]

# Undo stack, implemented as a list
undo_stack = []

# Rack size, in rack mount units:
rs = 42

# Rack parameters (also contains default clearances around racks):
#  Rack dimensions:
#   w -- width
#   d -- depth
#   h -- height
#  Rack clearances:
#   c_s -- side clearance
#   c_f -- front (and back) clearance
#   c_a -- aisle width
#  Auxiliary:
#   l_xc -- max length of a segment of racks
#   o_t  -- height of the overhead cable tray above the rack
rp = {'w': 0.6, 'd': 1.2, 'h': 2.0, 'c_s': 1.0, 'c_f': 1.0, 'c_a': 1.0, 'l_xc': 6.0, 'o_t': 0.5}

# The dictionary of preset constants and design-wide metrics (for all groups of compute equipment,
# including those in different configurations, plus all infrastructural equipment).
# Initially defined are:
#  * Cost of stationing one rack in the data centre per year;
#  * System lifetime, in years;
#  * Electricity price
# Attention! Use the same units for prices as in the other parts of the
# program: i.e., if equipment is priced in US dollars, use US dollars for
# electricity prices, too. You can take electricity prices for
# industrial consumers in Europe (in Euro) here: 
# http://epp.eurostat.ec.europa.eu/tgm/table.do?tab=table&init=1&language=en&pcode=ten00114&plugin=1
# and convert them to other currencies, including US dollars, here: http://www.xe.com/
# Here, electricity price for EU on average is 0.0940 EUR per kWh, which is about $0.13 per kWh.
m = {
    cRackStationingPerYear: 3000,
    cSystemLifetime:        3,
    cElectricityPrice:      0.13,
}

# Parameters for calling the performance modelling module.
# Members added manually to the "send" and "receive" fields should be
# in lower case for consistency; see examples in module "strconst.py"
#
# Fill the template
performance_template = {
    'name':     'performance',
    cUrl:       'http://localhost:8000/cgi-bin/performance/fluentperf/fluentperf.exe?task=design&benchmark=truck_111m&allow_throughput_mode=true',
    'send':     [cCpuFrequency, cNetworkTech],
    'receive':  [cPerformanceThroughputMode, cNetworkTech, cPerformance]
}

# Use the template to populate data structures for
# direct and inverse performance modelling module:
# 1. Direct performance model:
performance_module = copy.deepcopy(performance_template)
# Send the number of cores
performance_module['send'].append(cCores)

# 2. Inverse performance model:
inverse_performance_module = copy.deepcopy(performance_template)
inverse_performance_module['name'] = 'inverse_performance'
# Send performance...
inverse_performance_module['send'].append(cPerformance)
# ...and receive the number of cores
inverse_performance_module['receive'].append(cCores)

network_module = {
    'name':     'network',
    cUrl:       'http://localhost:8000/cgi-bin/network/network.exe?task=design&network_vendor=mellanox',
    'send':     [cNodes],
    'try_send': ['network_vendor', 'network_topology', 'max_network_blocking_factor'],
    'receive':  [cNetworkTopology, cNetworkBlockingFactor, cNetworkCost, cNetworkPower, cNetworkWeight,
                 cNetworkEdgeSwitchCount, cNetworkEdgePortsToNodes, cNetworkEdgePortsToCoreLevel,
                 cNetworkEdgeSwitchSize, cNetworkCoreSwitchCount, cNetworkCoreSwitchSize, cNetworkCorePorts,
                 cNetworkEdgeSwitchModel, cNetworkCoreSwitchModel, cNetworkExpandableTo]
}

ups_module = {
    'name':     'ups',
    'url':      'http://localhost:8000/cgi-bin/ups/ups_sizer.py?task=design',
    'send':     [cPower],
    'receive':  [cUpsCost, cUpsBackupTime, cUpsHeat, cUpsModel, cUpsPartitioning,
                 cUpsPowerRating, cUpsSizeRacks, cUpsWeight]
}

# TODO:
# 1. Let the user fill the list of constraints to be checked automatically at every stage. Create a function that checks them. Call this function at every stage.
# 2. There are blade switches where the number of ports going to nodes is not equal to the number of ports going to the outside world. Example: 34-port switches where 16 ports go to blade servers and 18 ports go outside. The network design module doesn't understand such switches and tries to allocate ports evenly (17+17). The network module needs to be changed so that the number of ports going to servers could be specified in the switch configuration and then used (via characteristics such as Eptn and Eptc).
# 3. If enclosure size is defined, then in update_metrics() we should calculate the cost, weight, power consumption of enclosures
# 4. If the network design module failed, emit why. E.g., for exascale machines, with the switches currently in the database, we need to use 4:1 blocking fat-tree networks. In this case, the network design module will fail mysteriously. Instead, it should report something like "too many nodes specified".
