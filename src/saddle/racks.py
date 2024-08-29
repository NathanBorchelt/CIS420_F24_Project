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

import math

# Our own modules:
import common
import floorplan
from strconst import *

def racks_dict(racks):
    '''Returns the dictionary of racks that allows their easy look-up by name.'''
    # Create an empty dictionary
    rd = {}
    # For each rack in the list of racks
    for rack in racks:
        rd.update({rack['name']: rack})
    # Return the resulting dictionary
    return rd

def place_equipment(eg, rack_size, rack_params, metrics, place_params=None, other_first=False, debug=0):
    '''Places equipment into racks. Modifies "eg"
    and "metrics" in place.
    "eg" is the list of equipment groups,
    "metrics" is a dictionary of design-wide metrics that
    we can only update after the placement stage.'''
    
    # "eg": list with groups of equipment to be placed
    # "rack_size": in rack mount units
    # "rack_params": contains parameters for placing racks on the floor:
    #   rack's sizes and clearances
    # "place_params": optional; may specify placement strategy

    # Make sure that at least an empty initial list of racks exists
    racks = []
    
    # Step through the supplied list, "eg".
    # Each entry is the result of one "eqgroups.add_equipment_group"
    # operation, and under the field "group_list" it stores the list of
    # equipment groups, for example: 5 server groups, 5 edge switches
    # and 2 core switches. We collect all such groups into one list
    # to place them into racks.
    group_list = [group for equipment_group in eg for group in equipment_group[cGroupList]]

    # Debug output
    if debug:
        print('>> List of equipment groups to be placed:')
        print('>> ')
        for g in group_list:
            print('>> {}, size {}'.format(g['name'], g['size']))
        print()
    
    # Calculate the total size of all equipment, except edge switches in enclosures
    equipment_sizes = [ g['size'] for g in group_list ]
    sum_sizes = sum(equipment_sizes)
    
    # Print this size -- and how many racks we will need at the minimum.
    # "ceil" acts as a "round upwards"
    if debug:
        need_racks_min = math.ceil(sum_sizes / rack_size)
        print('>> Sum of all equipment to be placed is {} units'.format(sum_sizes))
        print('>> With racks {} units high and the densest possible placement, we need at least {} racks'.format(rack_size, need_racks_min))

    # Detect "all other equipment" that we will place
    # in the most general way possible. We specifically
    # exclude all equipment types that we place using
    # their own routines:
    other_eq_group_list = [g for g in group_list if all([
        g['type'] == cGroupNameIndivisible,
        g['type'] != cGroupNameEdgeSwitch,
        g['type'] != cGroupNameServers,
        g['type'] != cGroupNameEnclosure,
        g['type'] != cGroupNameUps
    ])]

    # If the user requested to add all other equipment first,
    # do it now.
    if other_first:
        result = place_other_equipment(other_eq_group_list, racks, rack_size, place_params, debug)
        # If there was a gross error, pass it on to the caller
        if result == None: return
    
    # Now, place three types of equipment that we know
    # how to place according to their specifics:
    # (1) core switches;
    # (2) edge switches and servers;
    # (3) UPS systems
    # Place core switches
    result = place_core_switches(group_list, racks, rack_size, place_params, debug)
    # If there was a gross error, pass it on to the caller
    if result == None: return
    
    # Place edge switches and their servers
    # XXX: Note: we need to pass the whole "eg" to "place_edge_switches_servers"
    # rather than an artificially prepared "group_list" as we do with other
    # procedures, because "eg" will be modified in place. Perhaps should do the same
    # with other procedures: "place_core_switches", etc.
    result = place_edge_switches_servers(eg, racks, rack_size, debug)
    # If there was a gross error, pass it on to the caller
    if result == None: return
    
    # Place UPS equipment
    result = place_ups(group_list, racks, rack_size, place_params, debug)
    # If there was a gross error, pass it on to the caller
    if result == None: return
    
    # Finally, if the user requested "all other equipment"
    # to be placed _last_, this is the right time to do it.
    # This block is mutually exclusive with the previous
    # block "if other_first"
    if not other_first:
        result = place_other_equipment(other_eq_group_list, racks, rack_size, place_params, debug)
        # If there was a gross error, pass it on to the caller
        if result == None: return
    
    # Delete empty racks, if any
    delete_empty_racks(racks)
    # Renumber racks if necessary
    renumber_racks(racks)
    
    # If no equipment was placed, the list of racks is still empty
    # Nothing to do, return this empty list
    if len(racks) == 0: return racks
    
    # Initially, the list of rack parameters contains mostly
    # rack's dimensions (width and depth) and clearances.
    # Add there the number of racks that we need to place
    rack_params['racks'] = len(racks)
    # Compute parameters of rack placement, updating "rack_params"
    floorplan.calculate_floor_space_size(rack_params)
    # Fill rack positions on a 2D plane
    fill_rack_positions(racks, rack_params)
    
    # Calculate how much equipment we placed and compare
    # it to "sum_sizes" that was calculated earlier
    # First, get all placed blocks except empty
    blocks = [ block for r in racks for block in r['block_list'] if block['type'] != cEmpty ]
    # Calculate size of each block, skipping zero-sized blocks
    placed_sizes = [ (block['end'] - block['start'] + 1) for block in blocks if block['size'] != 0 ]
    sum_placed = sum(placed_sizes)
    if sum_placed < sum_sizes:
        print('>> Warning: placed {} units of equipment out of required {}'.format(sum_placed, sum_sizes))
    elif sum_placed == sum_sizes:
        print('>> Placed all {} units of equipment successfully'.format(sum_placed))
    else:
        # Otherwise, placed is more than total, which should not happen
        print(common.whoami() + ': Warning! Placed {} units of equipment when only {} were requested'.format(sum_placed, sum_sizes))
    
    # Debug output
    if debug:
        group_list_new = [group for equipment_group in eg for group in equipment_group[cGroupList]]
        print('>> List of equipment groups _after_ placement:')
        print('>> ')
        for g in group_list_new:
            print('>> {}, size {}'.format(g['name'], g['size']))
        print()
    
    # The last thing to do is to update design-wide metrics
    update_design_wide_metrics(eg, racks, metrics)
    
    # Return the list of racks
    return racks

def update_design_wide_metrics(eg, r, m):
    '''Updates design-wide metrics that only become
    known after the placement stage. Modifies
    equipment groups ("eg") and metrics dictionary ("m")
    in place. "r" holds the list of racks.'''
    # Now that equipment has been placed, we know the number of racks.
    # This allows to calculate operating costs. But first we add up all
    # metrics from all equipment groups
    # Calculate metrics by iterating through all equipment groups
    # Start with all zeros
    weight = power = capex = opex = 0
    for group in eg:
        # Fetch the configuration
        conf = group['c']
        # If the corresponding per-configuration metrics are
        # defined, update design-wide metrics
        if cWeight in conf:   weight += conf[cWeight]
        if cPower in conf:    power += conf[cPower]
        if cCapex in conf:    capex += conf[cCapex]
        if cOpex in conf:     opex += conf[cOpex]
    # The final step: add operating cost of stationing racks
    # over the system's lifetime. Racks are stored in list "r";
    # its length is the number of racks
    opex += len(r) * m[cRackStationingPerYear] * m[cSystemLifetime]
    # Operating costs need to be rounded due to non-integer electricity price
    opex = round(opex)
    # Update the global dictionary.
    m[cWeight] = weight
    m[cPower] = power
    m[cCapex] = capex
    m[cOpex] = opex
    # Calculate the total cost of ownership
    m[cTco] = capex + opex

def place_with_strategy(group_list, racks, rack_size, place_params=None, debug=0):
    '''Places arbitrary equipment according to one of predefined strategies:
    1. Consolidate all equipment in one place, placing it in the
       minimal number of racks (strategy == 'consolidate'),
    2. Separate equipment by placing groups into consecutive
       racks (strategy == 'separate'),
    3. Spread equipment across several non-consecutive racks
       (strategy == 'spread', and then also specify "interval == N",
       where N is the number of racks between blocks).
    
    This function relies on "place()", which takes care of printing debug output.'''
    # If no placement parameters were specified, set the default
    if place_params == None:
        place_params = {'strategy': 'consolidate'}
    # Placement strategy must be specified in the parameters
    # passed by the user, otherwise exit with failure
    if 'strategy' not in place_params:
        print(common.whoami() + ': Warning! You have to specify placement strategy')
        return
    # If we reached here, "strategy" is defined
    strategy = place_params['strategy']
    if strategy == 'consolidate':
        # Try to place everything into one rack (more
        # racks will be automatically created if necessary)
        for g in group_list:
            rack_num = place(g, racks, rack_size, debug, preferred_rack=1)
            # If there was a gross error, pass it on to the caller
            if rack_num == None:
                print('>> Placing group "{}" failed'.format(g['name']))
                return
    elif (strategy == 'separate') or (strategy == 'spread'):
        # These two strategies are similar; "separate" is a particular
        # case of "spread" with "interval=1"
        if strategy == 'separate':
            # Try to place equipment groups into consecutive racks
            interval = 1
        elif strategy == 'spread':
            # Each group is placed into a rack which is
            # "interval" positions apart from the previous rack
            # Interval must be specified, otherwise exit with failure
            if 'interval' not in place_params:
                print(common.whoami() + ': Warning! When using strategy "{}", interval must be specified'.format(strategy))
                return
            # If we reached here, interval was specified; extract it
            interval = place_params['interval']
        # Now, we can start actual placement
        # Start with this rack
        place_into = 1
        for g in group_list:
            rack_num = place(g, racks, rack_size, debug, preferred_rack=place_into)
            # If there was a gross error, pass it on to the caller
            if rack_num == None:
                print('Placing group "{}" failed'.format(g['name']))
                return
            # Next group will be placed into "interval" rack apart
            place_into += interval
    else:
        # Unrecognised strategy
        print(common.whoami() + ': Warning! Placement strategy "{}" is not recognised'.format(strategy))
        return
    # If we reached here, all went well.
    # Return "True" to indicate success
    return True
        
def place_other_equipment(other_eq_group_list, racks, rack_size, place_params=None, debug=0):
    '''Places "all other" into racks.'''
    # If no placement parameters were specified, set the default
    if place_params == None:
        place_params = {'strategy': 'consolidate'}
    # Select only core switches from the supplied group list
    result = place_with_strategy(other_eq_group_list, racks, rack_size, place_params, debug)
    # If there was a gross error, pass it on to the caller
    if result == None: return
    # Return "True" to indicate success
    return True

def place_core_switches(group_list, racks, rack_size, place_params=None, debug=0):
    '''Places core switches into racks.'''
    # If no placement parameters were specified, set the default
    if place_params == None:
        place_params = {'strategy': 'consolidate'}
    # Select only core switches from the supplied group list
    core_switches = [g for g in group_list if g['type'] == cGroupNameCoreSwitch]
    result = place_with_strategy(core_switches, racks, rack_size, place_params, debug)
    # If there was a gross error, pass it on to the caller
    if result == None: return
    # Return "True" to indicate success
    return True

def place_edge_switches_servers(eg, racks, rack_size, debug=0):
    '''Places edge switches -- and servers connected to them -- into racks.'''
    for equipment_group in eg:
        # Iterate over a _copy_, since we will be changing the
        # contents of "equipment_group[cGroupList]" in the loop
        for e in equipment_group[cGroupList][:]:
            # Skip all equipment which is not an edge switch
            if e['type'] != cGroupNameEdgeSwitch:
                continue
            # Now, "e" is definitely an edge switch.
            # Each edge switch has the list of other groups
            # to which it is connected
            conn_list = e[cConnections]
            
            # Iterate through all connections of this edge switch
            # (some of them could be to servers, others to core switches)
            # (Iterate over a _copy_ of the list, since we are going to
            # change the list in the loop)
            for connection in conn_list[:]:
                # Get the name of the group
                connected_group_name = connection['to_group']
                
                # Find the group with this name in the list
                for connected_group in equipment_group[cGroupList]:
                    if connected_group['name'] == connected_group_name:
                        break
                # At this moment, "connected_group" points to one of the
                # groups to which this edge switch is connected
                
                # Is this a group of servers or an enclosure (i.e., not a core switch)?
                # If not, go to the beginning of the loop
                if not connected_group['type'] in [cGroupNameServers, cGroupNameEnclosure]:
                    continue

                # If we are placing an enclosure (rather than just a group of 
                # servers), then the edge switch must be placed into the enclosure. 
                # We have to set its "start" and "end" positions manually, because
                # the place_into_rack() routine cannot figure them out (although
                # this somewhat contradicts the idea of software modularity).
                # Beware that in certain cases an edge switch can be connected
                # to more than one enclosure (e.g., one blade switch for two
                # enclosures), and in this case the location of the edge switch
                # will be that of the last enclosure.
                if connected_group['type'] == cGroupNameEnclosure:
                    e[cSwitchType] = cEnclosureSwitch
                    e[cStart] = connected_group[cStart]
                    e[cEnd] = connected_group[cEnd]
                else:
                    # Else, it was a group of servers (not an enclosure),
                    # and we mark the switch as standalone
                    e[cSwitchType] = cStandaloneSwitch

                # If it is a group of servers, check that it is not too large for the rack size
                if (connected_group['type'] == cGroupNameServers) and (connected_group['size'] > rack_size):
                    # It's too large. We need to split it into smaller groups
                    # How many servers can we place per rack?
                    servers_per_rack = rack_size // connected_group[cServerSize]
                    # Current size of this oversized group, in units
                    cur_size = connected_group['size']
                    # Size of a single server, in units
                    server_size = connected_group[cServerSize]
                    # While the group is too large, keep creating smaller groups
                    subgroup_index = 1
                    while cur_size > 0:
                        new_group = connected_group.copy()
                        if cur_size > rack_size:
                            new_group['servers'] = servers_per_rack
                        else:
                            new_group['servers'] = cur_size
                        new_group['size'] = server_size * new_group['servers']
                        new_group['name'] += '_sub' + str(subgroup_index)
                        # Add the newly created group to the global group list
                        equipment_group[cGroupList].append(new_group)
                        # Successfully placed some equipment, so decrease "cur_size"
                        cur_size -= new_group['size']
                        # Increment the index for this subgroup
                        subgroup_index += 1
                        # Place the subgroup and get the rack number where it was placed
                        rack_num = place(new_group, racks, rack_size, debug)
                        # If there was a gross error, pass it on to the caller
                        if rack_num == None:
                            print(common.whoami() + ': Placing server subgroup "{}" failed'.format(new_group['name']))
                            return
                        # In the connections if the current edge switch, make a copy
                        # of the connection to this oversized group of servers and
                        # update the number of links
                        new_connection = connection.copy()
                        new_connection['to_group'] = new_group['name']
                        new_connection['links'] = new_group['servers']
                        # Add this new connection to the edge switch's connection list
                        conn_list.append(new_connection)
                    
                    # Now that the "connected_group" has been split into smaller groups,
                    # it's time to delete it
                    equipment_group[cGroupList].remove(connected_group)
                    # Same with the old connection to the oversized group:
                    # it has been replaced by several smaller connections,
                    # so delete the original one
                    conn_list.remove(connection)
                else:
                    # It is not an oversized group of servers, or it is simply an enclosure.
                    # Place it as usual and get the rack number where the group is placed
                    rack_num = place(connected_group, racks, rack_size, debug)
                    # If there was a gross error, pass it on to the caller
                    if rack_num == None:
                        print(common.whoami() + ': Placing server group "{}" failed'.format(connected_group['name']))
                        return

            # Now, place the edge switch itself, preferably to the same rack
            # as the last server group or enclosure
            rack_num = place(e, racks, rack_size, debug, preferred_rack=rack_num, add_fields=[cSwitchType])
            # If there was a gross error, pass it on to the caller
            if rack_num == None:
                print(common.whoami() + ': Placing edge switch "{}" failed'.format(e['name']))
                return

    # Return "True" to indicate success
    return True
    
def place_ups(group_list, racks, rack_size, place_params=None, debug=0):
    '''Places UPS equipment into racks.'''
    # Select only UPS equipment from the supplied group list
    ups_groups = [g for g in group_list if g['type'] == cGroupNameUps]
    result = place_with_strategy(ups_groups, racks, rack_size, place_params, debug)
    # If there was a gross error, pass it on to the caller
    if result == None:
        return
    # Return "True" to indicate success
    return True

def place(group, racks, rack_size, debug=0, preferred_rack=1, add_fields=[]):
    '''Places the specified group of equipment into a rack.
    Preferred rack can be specified; if not used, backward and
    forward search for suitable racks is performed automatically.
    If all else fails, a new rack will be created.
    Any fields specified in "add_fields" will be looked up in
    the equipment group and copied to the block in the rack.'''
    # Size of the group we need to place
    need_size = group['size']
    # Oversized blocks cannot be placed at all; return an error
    if need_size > rack_size:
        print(common.whoami() + ': Warning! Block "{}" of size {} is too large for rack size {}, could not place.'.format(group['name'], need_size, rack_size))
        return
    # If preferred rack was specified, make sure we have enough racks
    if preferred_rack > len(racks):
        # Need to create more empty racks
        start_with_num = len(racks) + 1
        need_racks = preferred_rack - len(racks)
        empty_racks = [ create_empty_rack(start_with_num + r, rack_size) for r in range(need_racks) ]
        # Append newly created racks to the main list
        racks.extend(empty_racks)

    # Try to place into rack specified by the user
    # Select only one rack based on its number
    # (Create a list and then choose its -- hopefully -- single element)
    if place_into_rack(group, [r for r in racks if r['number'] == preferred_rack][0], debug, add_fields) == True:
        # If successful, return the rack number
        return preferred_rack
    
    # If failed, search backward first
    # Select only racks whose number is lower than
    # the preferred rack, and sort them in reverse order.
    # Note that if "preferred_rack" is not specified, it
    # defaults to "1", and therefore no backward search
    # is performed, because the list of "preceding" racks is empty.
    for r in sorted([r for r in racks if r['number'] < preferred_rack], key=get_rack_number, reverse=True):
        # If placement was successful, exit
        if place_into_rack(group, r, debug, add_fields) == True:
            return(r['number'])
    
    # If we reached here, then the backward search was
    # unsuccessful. Now, search forward.
    for r in sorted([r for r in racks if r['number'] > preferred_rack], key=get_rack_number, reverse=False):
        # If placement was successful, exit
        if place_into_rack(group, r, debug, add_fields) == True:
            return(r['number'])
    
    # And if we reached here, existing racks cannot fit
    # our equipment. Create a new rack; its number is
    # the current number of racks plus one.
    empty_rack = create_empty_rack(len(racks) + 1, rack_size)
    # Append it to the list of existing racks
    racks.append(empty_rack)
    if place_into_rack(group, empty_rack, debug, add_fields) == True:
        # If placement was successful, exit
        return(empty_rack['number'])
    else:
        # Should not reach here
        print(common.whoami() + ': Warning! Despite all efforts, could not place group "{}" into racks'.format(group['name']))

def create_empty_rack(num, rack_size, params=None):
    '''Creates an empty rack as a dictionary and adds user-supplied parameters.
    
    "params" is also a dictionary.'''
    # What must be present in any newly created rack:
    # it's number and name
    template = {'number': num, 'name': 'rack_{}'.format(num)}
    # Define one large empty block, with the size of the whole rack
    empty_block = {'start': 1, 'end': rack_size, 'type': cEmpty, 'name': cEmpty,
                   }
    # Initially, the rack contains only this single block
    block_list = {'block_list': [ empty_block ] }
    # If nothing was specified in "params", create an empty dictionary
    if params == None:
        params = {}
    # Update user-specified parameters with the block list that we created
    params.update(block_list)
    # Add also other required parameters
    params.update(template)
    # Return the resulting dictionary
    return params

def get_rack_number(r):
    '''Used mainly for sorting.'''
    # Called by the built-in function sorted() to sort racks in a list
    return r['number']
    
def place_into_rack(group, r, debug=0, add_fields=[]):
    '''Places equipment into the specified rack.
    Only one rack is specified, and if the equipment group
    cannot fit there, error is returned to the caller.'''
    # Get the size of the group we need to place
    need_size = group['size']
    # Create a new block for the group we are placing
    new_block = {'name': group['name'], 'type': group['type']}
    
    # There is one very special case when we can place groups
    # that take zero space. This is the case of edge servers in
    # enclosures. In other cases, equipment with zero size looks
    # suspicious. Let's check if it is an edge switch; if so,
    # allow placement
    if need_size == 0:
        if group['type'] == cGroupNameEdgeSwitch:
            if group[cSwitchType] == cEnclosureSwitch:
                # Get start and end positions of the new block
                # from the block itself
                new_block[cStart] = group[cStart]
                new_block[cEnd] = group[cEnd]
                # Indicate that it is an enclosure switch; this way
                # we can skip it during rack consistency checks
                new_block[cSwitchType] = cEnclosureSwitch
        else:
            # Not an edge switch in enclosure, but size is
            # zero -- report an error
            print(common.whoami() + ': Error! Found equipment block "{}" with zero size -- cannot place.'.format(group['name']))
            return
    else:
        # General case: the group size is not zero.
        # Find (hopefully, one) empty block in this rack
        empty_blocks = [block for block in r['block_list'] if block['type'] == cEmpty]
        # The rack may already be completely filled and
        # have no empty blocks. In this case, exit with a failure
        if len(empty_blocks) == 0:
            return
        # On the other hand, if there is more than one empty block,
        # complain -- it's obviously a bug in the program. There should
        # be only one large empty block rather than several small ones.
        if len(empty_blocks) > 1:
            print(common.whoami() + ': Warning! Rack "{}" has {} empty block(s) instead of expected one.'.format(r['name'], len(empty_blocks)))
        # Use the first found empty block, anyway
        empty_block = empty_blocks[0]
        empty_size = empty_block['end'] - empty_block['start'] + 1
        # If the empty block is not large enough, exit with failure
        if empty_size < need_size:
            return
        # Otherwise, continue. Preferred location defaults to bottom.
        prefer_top = False
        # Use top location only if explicitly specified.
        # (May not be declared for the group, so we check this first)
        if cPreferLocation in group:
            if group[cPreferLocation] == cTop:
                prefer_top = True
        # Now, place according to the preference: top or bottom
        if prefer_top:
            # If the preferred location of the group is on the top:
            # Set the bottom ("start") and top ("end") of the new block
            new_block['start'] = empty_block['end'] - need_size + 1
            new_block['end'] = empty_block['end']
            # Move the top of the empty block downward
            empty_block['end'] -= need_size
        else:
            # If the preferred location is at the bottom or not specified,
            # place the group at the bottom
            # Set the bottom ("start") and top ("end") of the new block
            new_block['start'] = empty_block['start']
            new_block['end'] = empty_block['start'] + need_size - 1
            # Move the bottom end of the empty block upward
            empty_block['start'] += need_size
        # Sometimes the size of an empty block becomes zero,
        # let's check for this condition
        empty_size = empty_block['end'] - empty_block['start'] + 1
        # If indeed zero, remove the empty block
        if empty_size == 0:
            r['block_list'].remove(empty_block)

    # Some fields from the group, specified in "add_fields", need to
    # be copied into the new block. Size is always copied
    for field in ['size'] + add_fields:
        if field in group:
            new_block[field] = group[field]

    # Whether is was a zero-size block or not, add it into the current rack
    r['block_list'].append(new_block)
    
    # Now, update the group of equipment that we were passed.
    # Record in which rack it was placed
    group['rack'] = r
    # Also save bottom ("start") and top ("end") positions of the group
    # within the rack
    group['start'] = new_block['start']
    group['end'] = new_block['end']
    # Finished placement, can exit with success
    if debug:
        print('>>> Group "{}" with size {} placed into rack {}'.format(group['name'], group['size'], r['number']))
    # Return success
    return True
                        
def delete_empty_racks(racks):
    '''Deletes racks that are empty.'''
    # More precisely, we _select_ racks that contain something rather
    # than just empty space
    racks = [r for r in racks if len([bl for bl in r['block_list'] if bl['type'] != cEmpty]) != 0]

def renumber_racks(racks):
    '''Makes sure rack numbering starts from 1.'''
    # Sometimes the system can create excess empty racks according
    # to user's request (for example, when calling "place()"
    # and specifying too large a number for "preferred_rack".
    # In this case, there can be some empty racks in the beginning
    # (rack 1, 2, etc.) that need to be renumbered.
    # We also make sure that rack numbers contain zeroes if
    # required (e.g., rack_01, ..., rack_25).
    #
    # Sort racks according to their current numbers
    racks = sorted(racks, key=get_rack_number)
    # If the list is empty, there is nothing to renumber
    if len(racks) == 0: return
    # How many decimal places we should reserve for the rack number
    p = math.floor(math.log10(len(racks))) + 1
    # Prepare the format specifier
    fs = '{:0' + str(p) + 'd}'        
    num = 0
    # Iterate through the sorted list
    for r in racks:
        num += 1
        # Assign a new number
        r['number'] = num
        # And a pretty new name
        r['name'] = 'rack_' + fs.format(num)

def fill_rack_positions(racks, rack_params):
    '''Fill rack positions on a 2D plane in a serpentine pattern.'''
    # How many racks there are in a row
    racks_per_row = rack_params['racks_per_row']
    # Initial positions along "X" and "Y" axes
    row = 1
    column = 1
    # Initially, the "snake" crawls to the right:
    to_the_right = True
    # Iterate through racks
    for r in racks:
        r['r_x'] = column
        r['r_y'] = row
        if to_the_right:
            # Crawl further to the right
            column += 1
            if column > racks_per_row:
                # Crawled too far, remedy
                column -= 1
                # Go to the next row
                row += 1
                # Now, crawl to the left
                to_the_right = False
        else:
            # Else, crawl to the left
            column -= 1
            if column == 0:
                # Crawled too far, correct
                column += 1
                # Go to the next row
                row += 1
                # Now, crawl to the right
                to_the_right = True

def get_block_top(block):
    '''Gets the position of a top of the block of equipment in a rack.
    Used for sorting blocks of equipment.'''
    return block['end']
    
def is_rack_consistent(rack, rack_size):
    '''Makes sure the rack is self-consistent: e.g., blocks do not overlap, etc.'''
    # Extract block list from the rack description, explicitly skipping
    # edge switches in enclosures
    blocks = [ block for block in rack['block_list'] if not (cSwitchType in block and block[cSwitchType] == cEnclosureSwitch)]
    # Sort remaining normal blocks so that the topmost block goes first
    blocks = sorted(blocks, key=get_block_top, reverse=True)
    # Make sure there are at least some blocks (even in
    # an empty rack at least one giant empty block must exist)
    if len(blocks) == 0:
        print(common.whoami() + ': Warning! There is neither equipment nor empty blocks placed in rack "{}"'.format(rack['name']))
        return False
    # The upper border of the topmost block must coincide with rack's height
    if blocks[0]['end'] != rack_size:
        print(common.whoami() + ': Warning! The topmost block ends at position "{}" while the rack height is only "{}"'.format(blocks[0]['end'], rack_size))
        return False
    # Analogously, the lower border of the bottom block must be equal to "1"
    if blocks[-1]['start'] != 1:
        print(common.whoami() + ': Warning! The bottom block starts at position "{}", while position "1" is expected'.format(blocks[-1]['start']))
        return False
    # Now comes the tricky part. We memorise the bottom of
    # the topmost block in the rack, and then descend down
    # the rack, making sure that the top of each block is
    # exactly one unit less that the bottom of the previous
    # block. This ensures there are neither gaps nor overlaps.
    previous_bottom = blocks[0]['start']
    previous_name = blocks[0]['name']
    # Iterate, skipping the first block
    for block in blocks[1:]:
        this_top = block['end']
        if previous_bottom != this_top + 1:
            print(common.whoami() + ': Warning! In rack "{}", blocks "{}" and "{}" are not adjacent'.format(rack['name'], previous_name, block['name']))
            return False
        # If we reached here, the two blocks are adjacent;
        # memorise current values
        previous_bottom = block['start']
        previous_name = block['name']
        
    # If we reached here, all checks passed successfully
    return True

def rack_text_representation(rack, rack_size, width, compact=True):
    '''Prepares a human-readable representation of rack's contents.
    "width" is the width of printed output, in characters.'''
    # "out" is the list that will be used to hold our output
    out = []
    # Print the rack name as a header
    out.extend(common.get_header(rack['name'], width))
    # Make sure the rack is self-consistent
    if not is_rack_consistent(rack, rack_size):
        print('Inconsistency in rack "{}" detected, cannot proceed'.format(rack['name']))
        return
    # Extract the list of equipment blocks and sort it so that the topmost
    # block goes first
    blocks = sorted(rack['block_list'], key=get_block_top, reverse=True)
    # The upper side of the topmost block determines the rack height
    top = blocks[0]['end']
    out.append('Height: {}'.format(top))
    # How many units in the rack are occupied: add up non-empty blocks
    occupied = sum([ (b['end'] - b['start'] + 1) for b in blocks if b['type'] != cEmpty])
    out.append('Units occupied: {}'.format(occupied))
    # Print position on the floor, if already known
    if ('r_x' in rack) and ('r_y' in rack):
        out.append('Position: column {} in row {}'.format(rack['r_x'], rack['r_y']))
    else:
        out.append('Position on the floor is unknown')
    out.append('')
    # String constants for printing empty space in a rack
    # and a horizontal  line
    space = '|' + ' ' * (width - 2) + '|'
    hline = '|' + '_' * (width - 2) + '|'
    # Print the top edge of the rack
    out.append(' ' + '_' * (width - 2) + ' ')
    # Iterate through the blocks, starting with
    # the topmost (because blocks have been sorted)
    for b in blocks:
        # Get the top and the bottom of the current block
        # and calculate its size
        top = b['end']
        bottom = b['start']
        size = top - bottom + 1
        # If the user requested full-height rather than compact output
        if not compact:
            # How much free space to leave above the block name
            upper_space = (size - 1) // 2
            # How much to leave below. Together, we have:
            # "upper_space" strings above the block name,
            # one string for the block name itself, and
            # "lower_space" strings below the block name.
            # Therefore, "size" = "lower_space+upper_space+1",
            # and from this we arrive to:
            lower_space = size - upper_space - 1
            # Print a pattern of empty space
            for i in range(upper_space):
                out.append(space)
        # Print the block name
        block_name = b['name'] + ' (' + str(size) + 'U)'
        # In case of compact output, only print the block name
        # and continue with the next block
        if compact:
            out.append('|' + block_name.center(width - 2, ' ') + '|')
            continue
        # If we reached here, then full-height output was requested
        if lower_space == 0:
            # No lower space; print the block name with underscores
            # that indicate the end of this block
            out.append('|' + block_name.center(width - 2, '_') + '|')
        else:
            # There is some lower space, hence print
            # the block name without underscores
            out.append('|' + block_name.center(width - 2, ' ') + '|')
            # Now, print the lower empty space below the block name
            # Print one line less than indicated in "lower_space"
            for i in range(lower_space - 1):
                out.append(space)
            # And print a horizontal line, indicating the end of the block
            out.append(hline)
    
    # Finished printing individual blocks;
    # print the bottom edge of the rack
    if compact:
        out.append(hline)
    # Return the list
    return out
        
def print_rack(rack, rack_size, compact=True):
    '''Prints rack's contents in human-readable format.'''
    # Width of output, in characters
    w = 40
    # Get the human-readable text representation
    # with the specified width
    text_repr = rack_text_representation(rack, rack_size, w, compact)
    # If there was a gross error, pass it on to the caller
    if text_repr == None: return
    # Print every line of it
    for line in text_repr:
        print(line)
    # Return "True" to indicate success
    return True
