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

import sys
import math
import random

# Our own modules:
import evaluate
import common
import callmod
from strconst import *

# This module handles the internal representation of the configuration list:
# the list whose items -- individual configurations -- are dictionaries.

def status(db, undo_stack):
    '''Returns some status information about the list of configurations.'''
    x = len(db)
    y = 0
    for item in db:
        # Make sure the configuration is enabled
        if item[cEnabled]:
            # Increment the number of enabled configurations
            y += 1
    r = ['> Enabled configurations: {} of {}'.format(y, x)]
    x = len(undo_stack)
    if x != 0:
        r.append('> {} undo step(s) available'.format(x))
    else:
        r.append('> No undo steps')
    # Return the list of strings
    return r

def display(config):
    '''Displays a specified configuration in "name=value" pairs.
    Simply put, prints the dictionary.'''
    # The so called "list comprehension":
    my_list = [my_key + '=' + str(config[my_key]) for my_key in config]
    # Sort the list for consistent output
    my_list.sort()
    # Print the items
    for item in my_list:
        print(item)
    # One last empty line for readability
    print()

def calculate_metric(db, expression):
    '''For each configuration in the list, calculates metrics defined in expression and
    adds their values into configuration.'''
    # Make sure that expression is not empty
    if expression.strip() == '':
        return True
    # We are expecting a list
    if not isinstance(db, list):
        print(__name__ + ': Expecting a list of configurations')
        return
    added_metric_names = []
    # For each configuration in the list:
    for c in db:
        # Skip disabled configurations
        if c[cEnabled] == False:
            continue
        # Pass the configuration to be used as the list of names for expression evaluation
        (result, added_metrics) = evaluate.evaluate(expression, c)
        # Now, "added_metrics" is a dictionary of evaluated metrics that
        # need to be added back into the original configuration.
        # For example, "added_metrics" could now be equal to {'cost': 15}
        c.update(added_metrics)
        # Also return the names of metric(s) that were calculated
        # XXX: We don't need to do it in every iteration of the loop,
        # but outside of the loop "added_metrics" seems to no longer be defined.
        added_metric_names = [name for name in added_metrics.keys()]
    # Return the updated list, and the names of metrics that were added
    return added_metric_names

def impose_constraint(db, expression):
    '''For each configuration in the list, evaluates specified logical expressions;
    if any of them is False, disables the configuration.'''
    # Works very much like "calculate_metric()" above.
    # Make sure that expression is not empty
    if expression.strip() == '':
        return True
    # We are expecting a list
    if not isinstance(db, list):
        print(__name__ + ': Expecting a list of configurations')
        return
    # Multiple expressions can be specified, all of them must evaluate to "True"; only then
    # we can state that a configuration satisfies given constraints.
    # Function evaluate() only returns one value, for the last expression evaluated, so we
    # have to manually break up multiple expressions into a list:
    expression_list = expression.split(cExpressionSeparator)
    # For each configuration in the list:
    for c in db:
        # If the configuration is already disabled, don't bother evaluating; skip to the next one
        if c[cEnabled] == False:
            continue
        # So, the configuration is enabled. Let's see if any constraints are violated
        # Initially, nothing is violated
        violated = ''
        # Evaluate each expression in the list
        for expression in expression_list:
            # Skip empty expressions, if any
            if expression.strip() == '':
                continue
            (result, added_names) = evaluate.evaluate(expression, c)
            # We need the result of the logical evaluation, but don't really need "added_names"
            if result == False:
                # This constraint is violated, add it to the list
                violated += (expression + ', ')
        # Now, all expressions have been evaluated. If "violated" is not empty,
        # then something was violated
        if violated != '':
            # Disable the configuration
            c[cEnabled] = False
            # Strip the trailing two characters ', '
            violated = violated[:len(violated)-2]
            violated = cConstraintViolated + ': ' + violated
            c[cReasonDisabled] = violated
    # All went well
    return True

def update_tech_econ_metrics(db, m, debug=0):
    '''Updates technical and economic metrics.'''
    # Keep track of processed configurations
    num = 0
    # Process each configuration separately
    for c in db:
        # Increment the number of the current configuration
        num += 1
        
        # Skip disabled configurations
        if c[cEnabled] == False:
            if debug:
                print('>> Skipping configuration {} since it is disabled'.format(num))
            continue
        
        # Try to calculate characteristics of compute nodes.
        # If the number of nodes is specified:
        if cNodes in c:
            nodes = c[cNodes]
            # Re-calculate "cores", it does no harm but allows the user to set
            # the number of nodes manually, without running an inverse performance model,
            # and then just update the metrics.
            c[cCores] = nodes * c[cNodeCpuCount] * c[cCpuCores]
            # Compute everything else
            c[cCapex] =  nodes * c[cNodeCost]
            c[cPower] = nodes * c[cNodePower]
            c[cWeight] = nodes * c[cNodeWeight]
        else:
            # But if not, then there is nothing we can update; skip this configuration
            if debug:
                print('>> Skipping configuration {}, "{}" is not set'.format(num, cNodes))
            continue

        # If "network_cost" is specified, then probably the network was designed,
        # and we can update corresponding metrics
        if cNetworkCost in c:
            c[cCapex] += c[cNetworkCost]
            c[cPower] += c[cNetworkPower]
            c[cWeight] += c[cNetworkWeight]
            
        # If "ups_cost" is specified, then probably the UPS subsystem was designed
        if cUpsCost in c:
            c[cCapex] += c[cUpsCost]
            # The tricky "ups_heat" metric should best be described in the documentation
            c[cPower] += c[cUpsHeat]
            c[cWeight] += c[cUpsWeight]
        
        # If system lifetime is specified, we can calculate operating costs
        if cSystemLifetime in m:
            # Check also that electricity price is specified
            if cElectricityPrice in m:
                # Power, in kWh, times system lifetime, in hours, time electricity price
                c[cOpex] = round((c[cPower] / 1000) * (m[cSystemLifetime] * 365 * 24) * m[cElectricityPrice])
                c[cTco] = c[cCapex] + c[cOpex]
            else:
                if debug:
                    print('>> Skipping electricity cost calculation, "{}" is not set'.format(cElectricityPrice))                    
        else:
            if debug:
                print('>> Skipping operating costs calculation, "{}" is not set'.format(cSystemLifetime))
    
    # All went well, return the updated list
    return db

def delete_disabled(db):
    '''Deletes disabled configurations'''
    # Select from "db" all configurations which are enabled,
    # and keep only them
    db = [c for c in db if c[cEnabled] == True]
    return db

def metric_statistics(db, metric):
    '''Provides simple statistics on the specified metric'''
    try:
        # Get values of the desired metric. Only process enabled configurations
        values = [c[metric] for c in db if c[cEnabled] == True]
    except KeyError:
        # No such metric defined (yet)! Print error message and return "None"
        print(__name__ + ': Metric "{}" not found'.format(metric))
        return
    # All went well. Sort values (this might take long, but with 1 million
    # values it takes about a couple of seconds)
    values.sort()
    length = len(values)
    # Now, compute the median
    if length % 2 == 1:
        # Remainder equals "1", so it's an odd number.
        # Take value from the middle of the array
        median = values[(length-1) // 2]
    else:
        # Length is an even number. Compute an arithmetic mean of two elements
        # in the middle. But if either of them is a string, then there is no
        # "arithmetic mean", and we just report the left element as the median
        left_middle = values[(length // 2) - 1]
        right_middle = values[length // 2]
        if any([isinstance(left_middle, str), isinstance(right_middle, str)]):
            median = left_middle
        else:
            median = (left_middle + right_middle) / 2
        # If the fractional part is zero, return an integer number
        if isinstance(median, float):
            if median.is_integer():
                median = int(median)
    result = {'length': length, 'min': min(values), 'max': max(values), 'median': median}
    return result

def select_best(db, metric, best='min'):
    '''Leaves only configurations whose metric value is the best (by default,
    minimal), and disables all other configurations.'''
    # Get values of the desired metric. Only process enabled configurations
    values = [c[metric] for c in db if c[cEnabled] == True]
    # Is the best value the minimal or the maximal?
    if best == 'min':
        sought = min(values)
    elif best == 'max':
        sought = max(values)
    else:
        print('{}: Unrecognised specification of best function: "{}", use either "min" or "max"'.format(__name__, best))
        return
    for c in db:
        # Skip disabled configurations
        if c[cEnabled] == False:
            continue
        # XXX: Caveat: comparisons with floating-point values may not be exact
        if c[metric] != sought:
            # Not the value we are looking for: disable the configuration and explain why
            c[cEnabled] = False
            c[cReasonDisabled] = 'Inferior according to metric {}'.format(metric)
    # All went well
    return True

def select_random(db, count=1):
    '''Select the specified number of configurations randomly,
    and disable the remaining ones.'''
    # How many configurations are enabled?
    num_enabled = 0
    for c in db:
        if c[cEnabled] == True:
            num_enabled += 1
    # If less configurations are enabled than required, inform the user
    if num_enabled < count:
        print(__name__ + ': Cannot select {} random configurations since only {} are enabled'.format(count, num_enabled))
        return
    # Get a list of random numbers, each ranging from 0 to the number of
    # enabled configurations. The length of the list equals to the number
    # of configurations that must remain enabled. Each list item is
    # therefore the ID of configuration that must remain enabled.
    random_list = random.sample(range(num_enabled), count)
    # Process configurations and disable those that are not on the list:
    current = 0
    for c in db:
        # Skip an already disabled configuration
        if c[cEnabled] == False:
            continue
        if current not in random_list:
            # If configuration is not on the list, disable it
            c[cEnabled] = False
            c[cReasonDisabled] = 'Disabled by random choice'
        # Increment the counter
        current += 1
    # All went well
    return True

def select_one(db, index=1):
    '''Select only the specified configuration,
    and disable the remaining ones.'''
    # Check that the index is valid. If not, exit with error
    if any([index < 1, index > len(db)]):
        print(__name__ + ': Index {} is out of valid range 1..{}'.format(index, len(db)))
        return
    # Check that the required configuration is not disabled.
    # If it is, exit with error
    if db[index-1][cEnabled] == False:
        print(__name__ + ': Configuration {} is disabled, cannot proceed')
        return
    # All is well. Iterate through all configurations and
    # disable all of them except the one we need
    for i in range(len(db)):
        # "i" starts from zero, while the configuration index
        # specified by the user starts from one, hence "i+1":
        if i + 1 != index:
            # Not the one we need -- we'll disable it
            # Skip an already disabled configuration
            if db[i][cEnabled] == False:
                continue
            db[i][cEnabled] = False
            db[i][cReasonDisabled] = 'Disabled when selecting only one configuration'    
    # All went well
    return True

def calculate_performance(db, performance_module, debug=0):
    '''Calculates performance by calling a performance model.'''
    # If the user requested the built-in "peak performance" model, then we
    # do not need to call anything and can calculate it right here
    if performance_module[cUrl] == cPeak:
        # Calculate the total peak performance for all nodes.
        # Note: metric "node_peak_performance" must be already available.
        expression = "performance = nodes * node_peak_performance"
        added_names = calculate_metric(db, expression)
        # Assume that no errors could occur when calculating these
        # simple expressions; errors mainly occur when calling
        # a design module over the network
        error_count = 0
    else:
        # Otherwise, call the performance module over the network
        (db, error_count) = callmod.call_design_module(db, performance_module, debug)    
    # Return the updated list
    return (db, error_count)

def calculate_inverse_performance(db, performance, inverse_performance_module, performance_module, debug=0):
    '''Calculates the number of cores by calling an inverse performance model.'''
    # We have to populate the field "performance"
    for c in db:
        # Skip disabled configurations
        if c[cEnabled] == False:
            continue
        c[cPerformance] = performance

    # Call the module; if there are errors, offending configurations will be disabled
    (db, error_count) = callmod.call_design_module(db, inverse_performance_module, debug, disable=1)
    
    # Hopefully, the field "cores" has been populated via the call to the design module.
    # Now, we need to calculate the number of nodes and set the corresponding field.
    for c in db:
        # Skip disabled configurations
        if c[cEnabled] == False:
            continue
        cores = c[cCores]
        cores_per_node = c[cNodeCpuCount] * c[cCpuCores]
        nodes = math.ceil(cores / cores_per_node)
        c[cNodes] = nodes
        # Now that the number of nodes has been rounded upwards,
        # the number of cores needs to be recalculated
        cores_new = nodes * cores_per_node
        c[cCores] = cores_new
        # The problem is that now the number of cores might have changed,
        # and in this case we must recompute performance. That requires
        # one additional call to the design module.
        if cores != cores_new:
            # Create a temporary list with just this configuration
            t = [ c ]
            if debug:
                print('Number of cores has been rounded up from "{}" to "{}", recomputing performance'.format(cores, cores_new))
                print()
            # Recompute performance
            (t, error_count) = calculate_performance(t, performance_module, debug)
            # In case of gross error, pass it on to the caller
            if t == None:
                return (None, 0)
            # Otherwise, use the updated configuration
            c = t[0]
        print('> In configuration {}, you now have {} cores in {} nodes, with {} cores per node'.format(c[cConfigurationId], c[cCores], c[cNodes], cores_per_node))
    
    # Return the updated list
    return (db, error_count)

def design_network(db, network_module, debug=0):
    '''Designs a network by calling the corresponding module.'''
    # Call the network design module
    (db, error_count) = callmod.call_design_module(db, network_module, debug)
    # Return the updated list
    return (db, error_count)

def design_ups(db, ups_module, debug=0):
    '''Designs a UPS subsystem by calling the corresponding module.'''
    # Call the UPS design module
    (db, error_count) = callmod.call_design_module(db, ups_module, debug)
    # Return the updated list
    return (db, error_count)
