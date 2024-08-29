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
import eqgroups
from strconst import *

def cable_needed(source_group, target_group):
    '''Returns True only if the cable is needed between two
    groups. The cable is _not_ needed if the source group is an
    edge blade switch and the target group is an enclosure: in
    this case, the connection is through the enclosure and doesn't
    require a cable.'''
    # If the source group is an edge switch, and it's type is
    # "enclosure_switch",and the target group is an enclosure
    # (rather than a core switch), the connection doesn't need a cable
    # XXX: In the infamous case of one blade edge switch and two enclosures,
    # this function should be rewritten to return "True" for the second
    # (and following) enclosures -- otherwise the cable(s) will not be drawn.
    # Consider using this: source_group['switch_in_enclosure'] = 'compute_enclosure_2'...
    try:
        if (source_group['type'] == cGroupNameEdgeSwitch and
            source_group[cSwitchType] == cEnclosureSwitch and
            target_group['type'] == cGroupNameEnclosure):
            return False
        # In all other cases, assume a cable is needed
        return True
    except:
        print(source_group)
        input('exception!')

def round_up_cables(cable_list, std_cable_lengths):
    '''Rounds up each cable's length to the nearest standard length.'''
    # Make sure that the list of standard cable
    # lengths is sorted
    std_cable_lengths.sort()
    # What is the longest available cable length
    max_available_length = max(std_cable_lengths)
    # Iterate through all cables
    for c in cable_list:
        # Find the length
        l = c['cable_length']
        # In case of zero-length cables (e.g., between blade servers
        # and a blade switch), there is no need to round up the
        # zero length
        if l == 0:
            continue
        # If the cable is equal or longer than all the standard cables
        # that we have, leave it as is and continue with the next cable
        if l >= max_available_length:
            continue
        # Otherwise, try standard cable lengths until
        # we find one that is equal or just a bit longer than required
        for std_len in std_cable_lengths:
            if std_len >= l:
                # Set new length
                c['cable_length'] = std_len
                # Done, no need to check other lengths
                break

def get_cable_list(group_1, group_2, rack_size, rack_plan, debug=0):
    '''Calculates cable length between two groups of equipment.'''
    # A simplified implementation. When connecting servers,
    # it assumes that the whole group of servers
    # will be connected to the corresponding switch
    # with cables of the same length, while in practice some
    # servers will require longer cables than others.
    # As a result, this procedure builds a list with
    # several identical cables. If you wish, you can
    # modify it to precisely calculate the length of each
    # individual cable.
    #
    # How many links go from "group_1" to "group_2"
    links = [conn['links'] for conn in group_1['connections'] if conn['to_group'] == group_2['name']][0]
    # Positions of groups within the rack:
    # find the geometric centre of each group.
    # If such entries do not exist, this most probably
    # signifies that equipment has not been placed into
    # racks yet.
    try:
        p_1 = round((group_1['start'] + group_1['end']) / 2)
        p_2 = round((group_2['start'] + group_2['end']) / 2)
    except KeyError:
        # Exit, returning "None" and thereby indicating an error
        print('>>> ' + common.whoami() + ': Could not calculate group positions in racks, exiting.')
        print('>>> Probable cause: equipment has not been placed into racks yet. If so, use place()')
        return
    # Create dictionaries with positions of groups
    pos_1 = {'pos': p_1, 'r_x': group_1['rack']['r_x'], 'r_y': group_1['rack']['r_y']}
    pos_2 = {'pos': p_2, 'r_x': group_2['rack']['r_x'], 'r_y': group_2['rack']['r_y']}
    if debug:
        print('>>> Calculating cable length beween groups:')
        print(group_1['name'], pos_1)
        print(group_2['name'], pos_2)
    # Calculate distance
    d = distance(pos_1, pos_2, rack_size, rack_plan, debug)
    if debug:
        print('>>> Approximate cable length: {:1.2f}, {} links'.format(d, links))
    # Construct the list of cables. In this simplified version,
    # the list contains just a single item
    return [ {'from_group':     group_1['name'],
              'to_group':       group_2['name'],
              'cable_length':   round(d, 2),
              'links':          links } ]
    
def distance(item_1, item_2, rack_size, rack_plan, debug=0):
    '''Calculates distance between two items of equipment.
    
    Uses positions of items within the rack ("pos")
    and on the floor ("r_x" and "r_y"), e.g.:
    
    item = {'pos': 10, 'r_x': 2, 'r_y': 7}
    '''
    # Rack height, in metres
    h = rack_plan['h']
    # Height of the overhead tray above the rack
    o_t = rack_plan['o_t']
    # Height of one rack-mount unit, in metres:
    one_unit = h / rack_size
    # One particular case is when both groups are in the same rack;
    # then the distance is just between the two items:
    if (item_1['r_x'] == item_2['r_x']) and (item_1['r_y'] == item_2['r_y']):
        d = abs(item_1['pos'] - item_2['pos']) * one_unit
        return d
    # Now check the general case.
    # Racks in the row are grouped in segments. All segments
    # but the last one have the length of "racks_per_segment".
    # Let us calculate in which segment each rack is located.
    rps = rack_plan['racks_per_segment']
    s_1 = math.ceil(item_1['r_x'] / rps)
    s_2 = math.ceil(item_2['r_x'] / rps)
    # Distance between two consecutive segments is equal
    # to "c_s", which is the rack side clearance. We
    # need to cover gaps between (s_1 - s_2) segments
    # (can be zero if they are in the same segment).
    # The additional cable length is therefore:
    d = abs(s_1 - s_2) * rack_plan['c_s']
    # Now, add the difference between rack numbers in the row,
    # multiplied by rack width (can be zero if both racks have
    # the same "r_x" position):
    d += abs(item_1['r_x'] - item_2['r_x']) * rack_plan['w']
    # Now, we need to calculate cable length along the "Y" axis.
    # If racks are in different rows, we need to jump by rack depth ("d")
    # plus front clearance of the rack ("c_f"), times the number of rows
    # we need to cross (can be zero if both racks are in the same row):
    d += (rack_plan['d'] + rack_plan['c_f']) * abs(item_1['r_y'] - item_2['r_y'])
    # Since equipment items are in different racks, we need to
    # route the cable upward from each group to the top of the rack
    # Add distance from the first item to the top of the rack
    d += (rack_size - item_1['pos']) * one_unit
    # Add distance from the second item to the top of the rack
    d += (rack_size - item_2['pos']) * one_unit
    # Add twice the distance to the overhead tray, since we need
    # to ascend to the tray at one rack and descend at the other
    d += 2 * o_t
    # Return the calculated distance
    return d
    
def route_cables(eg, rack_size, rack_plan, std_cable_lengths, debug=0):
    '''Routes cables between equipment groups.
    
    Uses rack positions on the floor and group positions
    within the racks to determine cable lengths.
    Returns the list of cables.'''
    
    # Build the dictionary of equipment groups for easier look-up
    groups = eqgroups.groups_dict(eg)
    
    # Create an empty list of cables
    cable_list = []
    
    # Iterate through all groups of equipment
    for (source_group_name, source_group) in groups.items():
        # If "connections" is defined for the group, then there
        # are probably some cables that need to be connected
        if 'connections' in source_group:
            if debug:
                print()
            # Iterate through all connection of this group
            for conn in source_group['connections']:
                # Find to which group the connection goes
                target_group_name = conn['to_group']
                target_group = groups[target_group_name]
                if debug:
                    print('>> Group "{}" has a connection to group "{}"'.format(source_group_name, target_group_name))
                # If for this particular connection a cable is not
                # needed, continue with the next connection of the
                # source group
                if not cable_needed(source_group, target_group):
                    if debug:
                        print('>> Not adding a direct connection between "{}" and "{}"'.format(source_group_name, target_group_name))
                    continue
                # Otherwise, handle the general case.
                # Get the list of cables and their lengths between the groups
                c_list = get_cable_list(source_group, target_group, rack_size, rack_plan, debug)
                # In case of gross error, pass it to the caller
                if c_list == None:
                    print('>> Could not establish a connection from group "{}" to "{}"'.format(source_group_name, target_group_name))
                    return
                # In case of success, extend the global cable list with the result
                cable_list.extend(c_list)
            
    # Now the list of cables contains several entries, and
    # cable lengths need to be rounded up to the nearest
    # longer cable.
    round_up_cables(cable_list, std_cable_lengths)
    
    # Return the list of cables
    return cable_list
    
def get_from_group(cable):
    '''When passed a cable definition in a dictionary,
    returns the value of "from_group". Used for sorting.'''
    return cable['from_group']
    
def cables_connections_text(c, eg):
    '''Prepares a human-readable representation of cable connections.'''
    # Build the dictionary of groups for easier look-up
    groups = eqgroups.groups_dict(eg)
    # "out" is the list that will be used to hold our output
    out = []
    # Print the header
    # Use this width for centring headers
    width = 60
    header = 'Cable Connections'
    out.extend(common.get_header(header, width))
    # Sort the list of cables according to the name 
    # of the group from which the cable(s) originate
    c = sorted(c, key=get_from_group)
    # Name of the current group being handled; initially empty
    # This is used so that the name of each group from where
    # cables originate is only printed once
    current_group = ''
    # Iterate through all cables
    for cable in c:
        # Determine to which group this cable belongs
        from_group = cable['from_group']
        # Extract other vital parameters
        to_group = cable['to_group']
        links = cable['links']
        cable_length = cable['cable_length']
        # Start printing output.
        # Is it the same group we are already handling?
        if current_group != from_group:
            # No, not the same. Need to print the new group's name
            out.append('')
            out.append('From: {} Rack: {}'.format(from_group, groups[from_group]['rack']['name']))
            # Set this group as the current one
            current_group = from_group
        # Whether it is a new group or not, start printing cables to other groups
        out.append('  To: {} | Rack: {}, {} links, length {}'.format(to_group,
            groups[to_group]['rack']['name'],
            links, cable_length))
    # Return the list
    return out
    
def cables_bom_text(c, eg):
    '''Prepares the bill of materials for cables.'''
    # Build the dictionary of groups for easier look-up
    groups = eqgroups.groups_dict(eg)
    # "out" is the list that will be used to hold our output
    out = []
    # Print the bill of materials. Start with the header
    # Use this width for centring headers
    width = 60
    header = 'Bill of Materials for Cables'
    out.extend(common.get_header(header, width))
    
    # If no cables have been routed yet, there is not much to print
    if len(c) == 0:
        out.append('No cables found. Cables have to be routed first.')
        return out

    # Bill of materials for cables: how many cables of each type to use
    cable_dict = {}
    # Iterate through all cables
    for cable in c:
        # Extract vital parameters
        links = cable['links']
        cable_length = cable['cable_length']
        # Update the bill of materials: the number of cables of this length:
        # fetch the current number of such cables (or zero, if none are there yet)
        # and add "links" more cables.
        cable_dict[cable_length] = cable_dict.get(cable_length, 0) + links

    # We need to sort cable lengths before printing cable counts.
    # For this, we insert lengths into a list and sort this list.
    # Start with an empty list of cable lengths
    len_list = []
    # Append each cable length to the list
    for cable_length in cable_dict.keys():
        len_list.append(cable_length)
    # Sort the list so that shorter cables go first
    len_list = sorted(len_list)

    # Iterate through the sorted list
    for cable_length in len_list:
        # If the cable length is, in fact, an integer -- that is, a "float"
        # with a zero decimal part -- then additionally strip the trailing
        # zero decimal part by converting in to an integer
        if isinstance(cable_length, float):
            if cable_length.is_integer():
                cable_length = int(cable_length)
        # Don't print fictitious zero-length cables
        if cable_length == 0: continue
        # Print how many cables of this length are required
        out.append('Length {:4}:   {:5} cable(s)'.format(
            cable_length,
            cable_dict[cable_length]))

    # Calculate the number of cables and their total length
    total_cables = 0
    total_length = 0.0
    # Iterate once more through the bill of materials
    for cable_length in cable_dict.keys():
        # Do not count fictitious zero-length cables
        if cable_length == 0: continue
        cables_of_this_length = cable_dict[cable_length]
        total_cables += cables_of_this_length
        total_length += cable_length * cables_of_this_length
    out.append('')
    out.append('Total number of cables to order: {:7}'.format(total_cables))
    # If one of the cables had a floating-point length, then the total
    # cable length is also a floating-point value, but it could have
    # a zero decimal part. If so, strip it by converting to an integer
    if isinstance(total_length, float):
        if total_length.is_integer():
            total_length = int(total_length)
    out.append('Total length of all cables: {:12}'.format(total_length))
    # Return the list
    return out
    
def print_cables(c, eg):
    '''Print a human-readable roster of cable connections.'''
    # Get the human-readable text representation
    text_repr = cables_connections_text(c, eg)
    # Print every line of it
    for line in text_repr:
        print(line)

def print_cable_bom(c, eg):
    '''Print the bill of materials for cables.'''
    # Get the human-readable text representation
    text_repr = cables_bom_text(c, eg)
    # Print every line of it
    for line in text_repr:
        print(line)
