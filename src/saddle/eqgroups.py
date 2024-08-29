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
from strconst import *

def groups_dict(equipment_groups):
    '''Returns the dictionary of equipment groups that allows their easy look-up by name.'''
    # Create an empty dictionary
    gd = {}
    # For each item in the list of equipment groups
    for eq_item in equipment_groups:
        # Find the list called "group_list" and add items
        # from it to the dictionary
        for group in eq_item['group_list']:
            gd.update({group['name']: group})
    # Return the resulting dictionary
    return gd

def connect_edge_to_servers(c, equipment_groups, debug=0):
    '''Connects edge switches to servers. Modifies "equipment_groups"
    in-place, adding or updating the element called "connections". '''
    
    def connect_e_to_s(e, s, debug=0):
        '''A service function that does all the real work: connects
        the edge switch in "e" to servers in "s".'''
        # The list of connections of the edge switch could already
        # be present from earlier invocations of this procedure
        if cConnections in e:
            conn_list = e[cConnections]
        else:
            # If not, create a new one
            conn_list = []
            # And also fill out the number of available links
            # from this edge switch to servers. This value
            # will be decremented as servers are connected.
            e[cLinksAvailable] = c[cNetworkEdgePortsToNodes]
        # How many links will go to servers
        links_available = e[cLinksAvailable]
        # Sanity check: the number of links should be enough to connect
        # all servers in the group
        servers = s['servers']
        if links_available < servers:
            # Not enough links available to connect this number of servers
            print(common.whoami() + ': Error! There are {} servers in group "{}", but edge switch "{}" only has {} free ports going to servers.'.format(servers, s['name'], e['name'], links_available))
            # Only deploy as many links as we have available
            links = links_available
        else:
            # On the other hand, if there are more links than servers,
            # then it can be a half-empty server group. In this case,
            # we deploy as many links as the number of servers.
            links = servers
        # Decrease the number of links of the edge switch that are
        # still available for connecting servers
        links_available -= links
        # Write back the new value
        e[cLinksAvailable] = links_available
        # Define the connection, setting the preferable colour for
        # drawing. The second element is empty, so the cable will be
        # drawn non-dashed
        conn_list.append({
            'to_group': s['name'],
            'links': links,
            'cable_colours': ('green', '')
        })
        # The list of connections has been updated, write it back
        e[cConnections] = conn_list
        # Print debug output
        if debug:
            print('>>> Connected edge switch "{}" to server group "{}"'.format(e['name'], s['name']))

    # Select a group of edge switches
    edge_groups = [g for g in equipment_groups if g['type'] == cGroupNameEdgeSwitch]
    # Now we need to select server groups. If enclosure size is
    # defined, then we are dealing with enclosures and will select
    # enclosures; otherwise, we will select servers
    if cEnclosureSize in c:
        group_name = cGroupNameEnclosure
    else:
        group_name = cGroupNameServers
    # Select server groups of the type determined above,
    # either separate servers or enclosures
    srv_groups = [g for g in equipment_groups if g['type'] == group_name]
    # Sanity check: the number of edge switches designed by the
    # network module should be equal to the number of server groups created.
    # There is a rare special case when two enclosures use only one switch:
    # the switch is installed into the first enclosure, while the second is
    # connected with cables. If we detect it, we inform the user that this case
    # is OK, but otherwise print a warning.
    if len(edge_groups) != len(srv_groups):
        if all([len(edge_groups) == 1,
           len(srv_groups) == 2,
           group_name == cGroupNameEnclosure]):
            print('>> Detected a special case of star network: 1 edge switch and 2 blade enclosures')
        else:
            print(common.whoami() + ': Warning! The number of edge switches {} is not equal to the number of server groups {}.'.format(len(edge_groups), len(srv_groups)))
    # Define the connection of edge switches to their equipment.
    # Even if the number of edge switches and server groups do not match,
    # try to connect as many as we can -- hence function "min".
    min_groups_to_connect = min(len(edge_groups), len(srv_groups))
    for i in range(min_groups_to_connect):
        e = edge_groups[i]
        s = srv_groups[i]
        connect_e_to_s(e, s, debug)
    # If there are more server groups than edge groups, try to connect
    # the remaining server groups to the last edge switch. This catches
    # that special rare case of one edge switch and two blade enclosures,
    # but will likely fail in other cases
    for i in range(len(srv_groups) - len(edge_groups)):
        # This loop will not be executed at all if the number of edge
        # switches and server groups is equal, which is almost always
        # the case.
        # Select the last edge switch:
        e = edge_groups[-1]
        # Connect each of the remaining server groups
        # (server groups in the range 0..len(edge_groups)-1
        # have already been connected to corresponding edge switches)
        s = srv_groups[len(edge_groups) + i]
        connect_e_to_s(e, s, debug)
        
def connect_edge_to_core(c, equipment_groups, debug=0):
    '''Connects edge switches to core switches.
    Modifies "equipment_groups" in-place.'''
    
    def print_debug_output(ename, cname, links):
        '''Prints debug output: which edge switch was connected to
        which core switch, and with how many links.'''
        if debug:
            print('>>> Connected edge switch "{}" to core switch "{}" with {} links'.format(ename,
            cname, links))
        
    recognised_topologies = [ cFatTree ]
    if c[cNetworkTopology] not in recognised_topologies:
        print(common.whoami() + ': Warning! Cannot connect core level. Only the following topologies are supported: ', end='')
        for t in recognised_topologies:
            print ('{} '.format(t), end='')
        print()
        return
    # Select groups of edge and core switches
    edge_groups = [g for g in equipment_groups if g['type'] == cGroupNameEdgeSwitch]
    core_groups = [g for g in equipment_groups if g['type'] == cGroupNameCoreSwitch]
    # Links can run in bundles; how many links do we have per bundle?
    b = c[cNetworkCorePorts] // c[cNetworkEdgeSwitchCount]
    # Define the connection between core switches and edge switches
    # Step through edge switches
    for e in edge_groups:
        # The list of connections of the edge switch could already be present
        if cConnections in e:
            edge_conn_list = e[cConnections]
        else:
            # If not, create a new one
            edge_conn_list = []
        # Connect each edge switch to all core switches
        # How many ports need to be connected to the core level
        edge_ports_remaining = c[cNetworkEdgePortsToCoreLevel]
        
        # Treat (a) the general case and (b) the specific case of only two core switches
        if len(core_groups) != 2:
            # The general case
            for core_num in range(len(core_groups)):
                # If it is not the last core switch, connect all "b" links in a bundle
                if core_num != len(core_groups)-1:
                    links = b
                    # Decrease the number of remaining ports by "b"
                    edge_ports_remaining -= b
                else:
                    # Connect whatever remains
                    links = edge_ports_remaining
                # Define the connection
                edge_conn_list.append({'to_group': core_groups[core_num]['name'], 'links': links})
                # Print debug output
                print_debug_output(e['name'], core_groups[core_num]['name'], links)
        else:
            # There are two core switches, split the links as evenly as possible
            links_to_core_1 = edge_ports_remaining // 2
            links_to_core_2 = edge_ports_remaining - links_to_core_1
            edge_conn_list.append({'to_group': core_groups[0]['name'], 'links': links_to_core_1})
            edge_conn_list.append({'to_group': core_groups[1]['name'], 'links': links_to_core_2})
            # Print debug output
            print_debug_output(e['name'], core_groups[0]['name'], links_to_core_1)
            print_debug_output(e['name'], core_groups[1]['name'], links_to_core_2)
        
        # The list of connections of this edge switch has been updated, write it back
        e[cConnections] = edge_conn_list

def add_equipment_group(name, c, rack_size, debug=0):
    '''Creates groups with names starting with "name",
    to be placed into racks of size "rack_size", from hardware
    specified in configuration "c".'''

    def create_groups(num_groups, group_name, group_size):
        '''Creates the specified number of identical groups
        of specified size, with their number in the end of
        the name.'''
        result = []
        # How many decimal places we should reserve for the group number
        p = math.floor(math.log10(num_groups)) + 1
        # Prepare the format specifier
        fs = '{:0' + str(p) + 'd}'
        # Start with this group number
        group_number = 1
        while group_number <= num_groups:
            # Add the group
            result.append({'name': group_name + '_' + fs.format(group_number),
                'size': group_size})
            # Increment the group number
            group_number += 1
        return result

    def add_edge_switches(name, c):
        '''Adds each edge switch into its own equipment group
        and connects edge switches to groups of servers.'''
        # Create a temporary variable
        edge_groups = []
        # The number of groups is equal to the number of edge switches
        groups = c[cNetworkEdgeSwitchCount]
        # Equipment size of edge switches (size of an edge switch
        # can be zero in case of blade enclosures,
        # because the switch doesn't occupy rack space on its own).
        edge_sw_size = c[cNetworkEdgeSwitchSize]
        group_name_template = name + '_' + cGroupNameEdgeSwitch
        edge_groups = create_groups(groups, group_name_template, edge_sw_size)
        # The groups have been created, now we need to add some fields
        for g in edge_groups:
            g['type'] = cGroupNameEdgeSwitch
            g[cPreferLocation] = cTop
        # Report the result
        print('>> Created {} group(s) of edge switches'.format(len(edge_groups)))
        return edge_groups
    
    def add_core_switches(name, c):
        '''Adds each core switch into its own equipment group
        and connects core switches to edge switches.'''
        recognised_topologies = [ cFatTree ]
        if c[cNetworkTopology] not in recognised_topologies:
            print(common.whoami() + ': Warning! Cannot add core switches. Only the following topologies are supported: ', end='')
            for t in recognised_topologies:
                print ('{} '.format(t), end='')
            print()
            return
        # Create a temporary variable
        core_groups = []
        # The number of groups is equal to the number of core switches
        groups = c[cNetworkCoreSwitchCount]
        # Links can run in bundles; how many links do we have per bundle?
        # Divide the number of ports on core switches by the number of edge switches
        b = c[cNetworkCorePorts] // c[cNetworkEdgeSwitchCount]
        # Sanity check: calculate the required number of core switches and compare it
        # to the one already stored in the configuration
        core_sw_count = math.ceil(c[cNetworkEdgePortsToCoreLevel] / b)
        if core_sw_count != groups:
            print(common.whoami() + ': Warning! Calculated that we need {} core switches while we have {}.'.format(core_sw_count, groups))
        core_sw_size = c[cNetworkCoreSwitchSize]
        group_name_template = name + '_' + cGroupNameCoreSwitch
        # Create identical groups
        core_groups = create_groups(groups, group_name_template, core_sw_size)
        # The groups have been created, now we need to add some fields
        for g in core_groups:
            g['type'] = cGroupNameCoreSwitch
            g[cPreferLocation] = cTop
        # Report the result
        print('>> Created {} group(s) of core switches'.format(len(core_groups)))
        return core_groups

    def add_server_groups(name, c):
        '''Adds groups of servers.'''
        # Create a temporary variable
        srv_groups = []
        # Form the group name
        group_name = name + '_'

        # Get the number of nodes to place and size of an individual node
        nodes = c[cNodes]
        node_size = c[cNodeEquipmentSize]

        # Are we dealing with enclosures?
        if cEnclosureSize in c:
            use_enclosures = True
            enclosure_size = c[cEnclosureSize]
        else:
            use_enclosures = False

        if use_enclosures:
            # Number of nodes in each group is determined purely by
            # enclosure size, to fill enclosures as densely as possible
            nodes_per_group = enclosure_size / node_size
            # The result of division should be an integer number.
            # If not, issue a warning
            if nodes_per_group.is_integer():
                nodes_per_group = int(nodes_per_group)
            else:
                nodes_per_group = round(nodes_per_group)
                print(common.whoami() + ': Warning: rounding number of servers per enclosure to {}'.format(nodes_per_group))
            # Complete the group name
            group_name += cGroupNameEnclosure
            group_type = cGroupNameEnclosure
            group_size = enclosure_size
        else:
            # If not using enclosures, the number of nodes in
            # a group is determined by the number of ports
            # on edge-level switches that go to compute nodes.
            # However, there is a corner case when the network
            # is not defined (the user only has servers and no
            # switches). In this case, just add one big group.
            if cNetworkEdgePortsToNodes in c:
                nodes_per_group = c[cNetworkEdgePortsToNodes]
            else:
                # Consider network undefined, add all nodes into one group
                print(common.whoami() + 'Note: network not defined, adding all nodes into one group')
                nodes_per_group = nodes
            # With regular rack-mounted servers, server size passed
            # to us is expected to be an integer. If not, produce a warning.
            if not isinstance(node_size, int):
                print(common.whoami() + ': Warning! Server size "{}" is not an integer'.format(node_size))
            # Complete the group name
            group_name += cGroupNameServers
            group_type = cGroupNameServers
            group_size = nodes_per_group * node_size

        # Now we are done with deciding on the size of groups and
        # their names. Calculate how many groups will be added.
        # "ceil" acts as a "round upwards"
        groups = math.ceil(nodes / nodes_per_group)
        # Create those groups
        srv_groups = create_groups(groups, group_name, group_size)

        # Add some more fields to each group: group type (servers or enclosure),
        # number of servers and their size, and preferred location
        for g in srv_groups:
            g['type'] = group_type
            g['servers'] = nodes_per_group
            g[cServerSize] = node_size
            g[cPreferLocation] = cBottom

        # Now groups have been filled, but they are all identical.
        # The last group might have less servers than all the others
        # Find the "remainder"; if it is not zero, then the last group
        # is not the same as all others and needs to be fixed
        srv_in_last_group = nodes % nodes_per_group
        if srv_in_last_group != 0:
            # Patch the last group, as it contains less servers
            last_group = srv_groups[len(srv_groups)-1]
            last_group['servers'] = srv_in_last_group
            # If we are not using enclosures, then we should also patch the group size
            if not use_enclosures:
                last_group['size'] = srv_in_last_group * node_size

        print('>> Created {} group(s) of servers'.format(len(srv_groups)))
        # Return the result
        return srv_groups

    def add_ups_groups(name, c, rack_size):
        '''Adds UPS system blocks as groups, assuming they
        already exist in the configuration.'''
        # XXX: Currently it only handles UPS equipment in separate racks
        if cUpsCost not in c:
            # Probably the UPS system is not designed yet,
            # nothing to do
            return []
        # The number of UPS groups is equal to the number of UPS racks
        groups = c[cUpsSizeRacks]
        # Equipment size is a full rack
        ups_size = rack_size
        group_name_template = name + '_' + cGroupNameUps
        ups_groups = create_groups(groups, group_name_template, ups_size)
        # The groups have been created, now we need to add some fields
        for g in ups_groups:
            g['type'] = cGroupNameUps
            g[cPreferLocation] = cBottom
        # Report the result
        print('>> Created {} group(s) of UPS equipment'.format(len(ups_groups)))
        return ups_groups

    # Here we will store the result of operations
    eq_groups = []

    # Check first if the size of compute nodes is defined in
    # configuration "c"; if so, then we are dealing with
    # compute equipment, rather than, say, a UPS
    if cNodeEquipmentSize in c:
        # Add servers
        server_groups = add_server_groups(name, c)
        # If there was a gross error, pass it on to the caller
        if server_groups == None: return
        # Otherwise, save the results
        eq_groups.extend(server_groups)

        # If there are edge switches defined, add them
        if cNetworkEdgeSwitchCount in c:
            edge_sw_groups = add_edge_switches(name, c)
            # If there was a gross error, pass it on to the caller
            if edge_sw_groups == None: return
            eq_groups.extend(edge_sw_groups)

            # If there are edge switches, there may also be core switches
            # In case of "fat-tree" topologies, add core switches
            if c[cNetworkTopology] == cFatTree:
                # Add core switches
                core_sw_groups = add_core_switches(name, c)
                # If there was a gross error, pass it on to the caller
                if core_sw_groups == None: return
                eq_groups.extend(core_sw_groups)
                
        # If network topology is defined, make connections as necessary
        if cNetworkTopology in c:
            # In both torus and fat-tree networks, we need to connect edge
            # switches to servers
            connect_edge_to_servers(c, eq_groups, debug)
            # In fat-tree networks, we additionally need to connect
            # edge switches to core switches
            if c[cNetworkTopology] == cFatTree:
                connect_edge_to_core(c, eq_groups, debug)
        
        # Add UPS equipment, if any
        ups_groups = add_ups_groups(name, c, rack_size)
        # If there was a gross error, pass it on to the caller
        if ups_groups == None: return
        # Add the returned UPS groups
        eq_groups.extend(ups_groups)

    elif cEquipmentSize in c:
        # Node size was not defined, it's likely we are dealing
        # with a single block of non-compute equipment such as a UPS.
        # Get its size and create a group for it, marking it an
        # indivisible equipment
        equipment_size = c[cEquipmentSize]
        print('Creating a generic group of equipment with size {}'.format(equipment_size))
        eq_groups.append({'name': name, 'size': equipment_size, 'type': cGroupNameIndivisible})
    else:
        # Emit an error:
        print('{}: Neither of the following is defined: "{}" nor "{}". Don\'t know how to add this group'.format(__name__, cNodeEquipmentSize, cEquipmentSize))
        return
    # Return the updated list of equipment groups
    return eq_groups
