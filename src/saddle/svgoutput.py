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

# Attention!!! This module tries to import module "svgwrite";
# see "Initialisation" section in the end of the file.

# Our own modules:
import eqgroups
import racks
import cabling
import colours
from strconst import *

def get_rack_x(rack):
    '''Returns rack's "r_x" coordinate. Used for sorting.'''
    return rack['r_x']

def calculate_length_pixels(l):
    '''Given the length of an object (e.g., rack height),
    calculates the number of pixels for drawing this object'''
    return round(l * 10 * delta)

def calculate_bundles(r, groups):
    '''Calculates how many bundles of cables are attached to blocks
    in a rack. This is used merely for drawing cables. Calculated
    values are saved in the rack description.'''
    # Iterate through racks
    for rack in r:
        cable_bundles = 0
        # Select only non-empty blocks (because empty space in a rack
        # cannot be connected with a cable to anything)
        blocks = [b for b in rack['block_list'] if b['type'] != cEmpty]
        # Iterate through selected non-empty blocks
        for b in blocks:
            # The block can be found in the dictionary of equipment 
            # groups, together with its additional parameters such
            # as cable connections from this block to other blocks.
            # Look up the group by its name and check if any
            # connections are defined
            group = groups[b['name']]
            if 'connections' in group:
                # Each cable bundle to some other block will require
                # to reserve some space for drawing. Each such bundle,
                # no matter how many links or cables it is comprised of,
                # will be drawn as a single line in our drawing. So we
                # reserve space for as many lines as there are bundles
                # that go from this block
                bundles_in_block = len(group['connections'])
                cable_bundles += bundles_in_block
                
                # There is one special case: if the current block is
                # an edge switch in enclosure, then it is connected to
                # its blade servers without cables, and we should
                # decrease the number of bundles we have just calculated
                # by one. In rare cases, this edge switch can be connected
                # to more than one enclosure, but we still only subtract
                # one bundle, since connections to other enclosure(s) will
                # still require cables
                if (group['type'] == cGroupNameEdgeSwitch and group[cSwitchType] == cEnclosureSwitch):
                    cable_bundles -= 1
                
                # A cable bundle going to some other rack also
                # requires to reserve space for drawing at _that_
                # rack, so we need to save in that rack's description
                # how many bundles go there
                for conn in group['connections']:
                    # Where the cable bundle goes: the destination group
                    dst_group_name = conn['to_group']
                    # Which rack is it in? Look up the group by name
                    # and extract its "rack" element
                    dst_rack = groups[dst_group_name]['rack']
                    # If the destination rack is _different_ from the current
                    # one, update the destination rack's count of bundles
                    # coming into it, by adding one bundle from the current
                    # equipment block.
                    # (This check is necessary because we don't want to
                    # reserve space in the _same_ rack twice)
                    if dst_rack != rack:
                        # If the two racks are in the same row, update
                        # destination rack's number of incoming bundles.
                        # (But if they are _not_ in the same row, do
                        # nothing, because the other row will be drawn
                        # completely independently and doesn't require
                        # reservation)
                        if dst_rack['r_y'] == rack['r_y']:
                            # Create the field if it is not there yet
                            if 'cable_bundles' not in dst_rack:
                                dst_rack['cable_bundles'] = 0
                            # Update, adding one bundle
                            dst_rack['cable_bundles'] += 1
        # The number of bundles in the current rack has been accumulated
        # in "cable_bundles", save it in the rack's description.
        # Create the field if it is not there yet
        if 'cable_bundles' not in rack:
            rack['cable_bundles'] = 0
        rack['cable_bundles'] += cable_bundles

def draw_rows_svg(eg, r, rack_size, rack_params, filename=None, row_list=None, max_cables=0, debug=0):
    '''Draws all rows from the specified list "row_list",
    or simply all rows if none are specified.
    "r" contains data for all racks.
    If there are more overhead cables than "max_cables",
    don't draw overhead cables at all.'''
    
    # Make sure that SVG library is available
    if not svg_output_available:
        print('> SVG library not available, cannot draw anything.')
        return
    
    # Drawing will start from this vertical "y" position on the canvas:
    start_y = delta * 2
    # 
    # Get rack height for drawing, in pixels
    rack_h = calculate_length_pixels(rack_params['h'])

    # Create a master SVG object
    svg = svgwrite.Drawing()
    # Specify initial canvas width that will be updated later
    svg['width'] = delta
    
    # Examine the list of rows "row_list" supplied by the user.
    # If "None" or an empty list was specified, use all rows,
    # i.e., insert all rows into "row_list"
    if (row_list == None) or (len(row_list) == 0):
        # Create an empty set
        rows = set()
        # Iterate through all racks, choose their "y" coordinate
        # (the row), and add it to a set. This way, we save
        # row numbers without repetitions.
        for rack in r:
            y_pos = rack['r_y']
            rows.add(y_pos)
        # The set now contains row numbers, without repetitions.
        # Put them into a list and sort it
        row_list = sorted(list(rows))
        # By now, even if "row_list" was not specified in the beginning,
        # now it contains only valid row numbers, so we can proceed
        # with drawing.
    
    # Build the dictionary of equipment groups for easier look-up.
    # This one will be used when drawing cables
    groups = eqgroups.groups_dict(eg)
    
    # Calculate cable bundles to be drawn in each rack
    calculate_bundles(r, groups)
    
    # Keep track of the number of cable bundles already drawn
    # in each rack in this dictionary:
    cable_bundles_drawn = {}
    # Start with zero bundles in each rack
    for rack in r:
        cable_bundles_drawn.update({ rack['name']: 0})
    
    # Iterate through the list of rows to be drawn supplied
    # by the user. Assume that the rows are already listed in
    # the required order. For example, the user might wish to draw
    # only the 5th and the 3rd row, in that order: "row_list=[5, 3]"
    # Therefore we should not sort the passed list.
    for row_number in row_list:
        # Draw a row by appending necessary data to the end
        # of SVG object, "svg"
        result = draw_single_row(groups, r, rack_size, rack_params, row_number, start_y, svg, cable_bundles_drawn, max_cables, debug)
        # In case of gross error, pass it to the caller
        if result == None:
            return
        # Advance the "y" position on the canvas to the next row:
        # simply move to the current bottom of the SVG document,
        # which has been updated in the end of "draw_single_row()"
        start_y = svg['height']

    # If filename to save the drawing was specified, then save
    if (filename != None) and (filename != ''):
        svg.saveas(filename)
    
    # Whether saved to a file or not, return to the caller
    # a string representation of the SVG object
    return svg.tostring()

def draw_single_row(groups, r, rack_size, rack_params, row_num, start_y, svg, cable_bundles_drawn, max_cables=0, debug=0):
    '''Draws an SVG representation of a row of racks.
    
    This function should not be called directly, but rather
    through "draw_rows()", because it depends on the calculation
    of cable bundles drawn in each rack which is done there.
    
    Equipment groups are passed in the dictionary "groups",
    rack data in "r", rack parameters in "rack_params",
    the row number to be drawn in "row_num",
    and SVG object to be appended in "svg".
    "start_y" is the vertical position of row tops on the drawing canvas.'''

    def x_position_rack(rack):
        '''Returns the "x" position of the left border of the given rack.'''
        # Distance from the left side of the canvas to the left border
        # of the rack: add up the left margin ("start_x"), plus an adjustment
        # for border thickness, plus a displacement due to the rack's "x" place
        # in the row
        return start_x + rack_border_thickness // 2 + rack_w * (rack['r_x'] - 1)
    
    def x_position_cable(rack):
        '''Calculates "x" position of a cable bundle to be drawn next.'''
        # Initial horizontal ("x") position: move to the left
        # border of the rack ("x_position_rack") and add some slack,
        # in this case 15% of rack's width will be reserved
        slack = round(0.15 * rack_w)
        x = x_position_rack(rack) + slack
        
        # If there is more than one bundle in this rack (and
        # there usually is), then step further to the right by
        # several small intervals. This allows to draw bundles at
        # uniform distances
        if rack['cable_bundles'] > 1:
            # The number of intervals is the number
            # of cable bundles drawn so far, minus one -- since drawing
            # the first bundle does not require stepping to the right
            num_intervals = cable_bundles_drawn[rack['name']] - 1
            # If we are not drawing overhead cables, the number of vertical
            # bundles drawn inside the rack is decreased. This allows looser
            # drawing of remaining cables
            bundles_in_rack = rack['cable_bundles']
            if not draw_overhead_cables:
                bundles_in_rack -= overhead_cables
            # The length of each interval
            # is the inner width of the rack, in pixels, minus two
            # margins on its sides, each "slack" pixels wide, divided
            # by the number of gaps between bundles (which is the number
            # of vertical bundles within the rack minus one)
            interval_length = (rack_w_inside - slack * 2) / (bundles_in_rack - 1)
            # Step to the right by so many pixels
            x += round(num_intervals * interval_length)
            
        # Return cable's "x" position
        return x
    
    def draw_blocks():
        '''Draw a carcass of the current rack and equipment blocks inside it.'''
        # Draw a carcass first. Extract rack name
        rack_name = rack['name']
        # Make sure the rack is self-consistent
        if not racks.is_rack_consistent(rack, rack_size):
            print('> Inconsistency in rack "{}" detected, cannot proceed.'.format(rack_name))
            return
        # Initial "x" position on the canvas is the left border
        # of the current rack
        x_pos = x_position_rack(rack)
        # Create an SVG group specifically for the current rack
        # and set its ID equal to the rack name
        rack_group = svg.add(svg.g(id=rack_name))
        # Add to this group whatever belongs there:
        # Add a rack carcass (reuse from definitions) at a proper position
        rack_group.add(svg.use(
            '#rack',
            x = x_pos,
            y = start_y))
        # Add a label below the rack, "x"-position is in the middle
        # of the rack, and the text is centred
        rack_group.add(svg.text(
            rack_name,
            x = [x_pos + rack_w // 2],
            y = [start_y + rack_h + delta * 2],
            font_size = delta,
            font_family = 'monospace',
            text_anchor = 'middle'))
    
        # Extract the list of equipment blocks and sort it
        # so that the topmost block goes first
        blocks = sorted(rack['block_list'], key=racks.get_block_top, reverse=True)
        
        # Iterate through the blocks, starting with
        # the topmost one (because blocks have been sorted)
        for b in blocks:
            # Get the top and the bottom of the current block
            # and calculate its size, all in rack-mount units
            top = b['end']
            bottom = b['start']
            size = top - bottom + 1
            
            # Do not draw edge switches in enclosures, because
            # their enclosure is drawn instead
            if (b['type'] == cGroupNameEdgeSwitch and b[cSwitchType] == cEnclosureSwitch):
                continue
            
            # Create an SVG group specifically for this block.
            # If this is not an empty block, then assume
            # its name is unique, and save it in block's ID
            if b['type'] != cEmpty:
                id = b['name']
            else:
                # It's an empty block, they are not unique,
                # hence pass "None" to not assign any ID
                id = None
            # SVG group for the block is created within the rack,
            # so the user can move the entire rack in their vector
            # graphics editor
            block_group = rack_group.add(svg.g(id=id))
            # Add a rectangle denoting this block of equipment
            # into the newly-created group for this block.
            # "x" position is calculated in the parent function.
            # "y" position: current "y" position, plus an adjustment
            # for border thickness, plus a displacement due to the
            # block's position within the rack. Choose filling colour
            # automatically based on block name.
            y_pos = start_y + rack_border_thickness // 2 + round((rack_size - top) * one_unit)
            block_group.add(svg.rect(
                # Move the "x" position of the block slightly to the right
                # to account for rack border thickness
                insert = (x_pos + rack_border_thickness // 2, y_pos),
                size = (rack_w_inside, round(size * one_unit)),
                fill = colours.get_colours(b['name']),
                opacity = '1',
                stroke = 'black',
                stroke_width = 1
            ))
            # Add a label with block's name; "x"-position is
            # in the middle of the rack, and the text is centred.
            # "y" position is also in the middle of the block, plus
            # an arbitrary adjustment of 5 pixels
            block_label = b['name'] + ' (' + str(size) + 'U)'
            block_group.add(svg.text(
                block_label,
                x = [x_pos + rack_w // 2],
                y = [y_pos + round((size * one_unit) / 2 + 5)],
                font_size = round(0.35 * delta),
                font_family = 'monospace',
                text_anchor = 'middle'))

    def draw_cables(current_overhead_num, draw_overhead_cables):
        '''Draw cables in the current rack, both those completely inside
        the rack and those that exit the rack and go to other racks.
        This routine is called after "draw_blocks", to draw cables
        _on_top_ of blocks, or cables will appear shaded or overlapped
        if placed underneath.'''

        def draw_paths():
            '''Draw two SVG paths: the first is solid, and the second is dashed, lying on top of the first one.'''
            # Draw the first SVG path
            svg.add(svg.path(
                d = d,
                fill = 'none',
                stroke = colour_solid,
                stroke_width = cable_thickness
            ))
            # If the dashed colour is defined, we need to draw the second path
            if colour_dashed != '':
                # We also set the path ID to refer to it in the animation statement
                path_id = 'from_' + group['name'] + '_to_' + to_group['name']
                overlying_path = svg.add(svg.path(
                    d = d,
                    id = path_id,
                    fill = 'none',
                    stroke = colour_dashed,
                    stroke_dasharray = delta // 2,
                    stroke_width = cable_thickness
                ))
                # Animate the overlying path by adding to it the "animate" attribute
                # When mouse is hovered over the cable, it starts to "move"
                # (dash length is gradually increased)
                overlying_path.add(svg.animate(
                    attributeName = 'stroke-dasharray',
                    begin = path_id + '.mouseover',
                    end = path_id + '.mouseout',
                    dur = '1s',
                    values = str(delta // 2) + ';' + str(round(delta * 1.5 // 2)),
                    repeatCount = 'indefinite'
                ))

        def y_position(equipment_group, addition):
            '''Calculates position of cable's end within a given equipment block.'''
            # There is one special case: we do not draw blade edge switches,
            # we instead draw the enclosures where they are located, therefore
            # if we are called with such a blade switch as an argument,
            # we recursively call ourselves with the name of the enclosure
            # this switch is in
            if (equipment_group['type'] == cGroupNameEdgeSwitch and equipment_group[cSwitchType] == cEnclosureSwitch):
                # Iterate through all connections until we find the enclosure
                # where this switch belongs
                for conn in equipment_group['connections']:
                    # Where the cable bundle goes: the destination group name
                    dst_group_name = conn['to_group']
                    # Lookup the destination group by name
                    dst_group = groups[dst_group_name]
                    # Exit the loop as soon as we find the first enclosure
                    if dst_group['type'] == cGroupNameEnclosure:
                        break
                return y_position(dst_group, addition)
            
            # Calculate size of equipment block, in rack-mount units
            size = equipment_group['end'] - equipment_group['start'] + 1
            # To get to the middle of the block, we need to step down
            # from the top of the rack ("y_initial") by as many units
            # as necessary to reach the block's top, then add half
            # of block's size, and step slightly up ("addition"):
            return y_initial + round((rack_size - equipment_group['end'] + size / 2 - addition) * one_unit)
        
        # Extract the list of equipment blocks and sort it
        # so that the topmost block goes first
        blocks = sorted(rack['block_list'], key=racks.get_block_top, reverse=True)

        # Iterate through the blocks, starting with
        # the topmost one (because blocks have been sorted)
        for b in blocks:
            # Skip empty blocks
            if b['type'] == cEmpty:
                continue
            # Look up the equipment group by its name and check
            # if any connections are defined
            group = groups[b['name']]
            # If "connections" is not defined for the group, skip it
            # (it is only edge switches that have connections
            # defined; they are connected to core switches and
            # servers/enclosures, and this connection is unidirectional)
            if 'connections' not in group:
                continue

            # So, we came across a group (likely an edge switch)
            # that has some connections defined.
            # Iterate through all connection of this group
            for conn in group['connections']:
                # Find to which group the connection goes
                to_group_name = conn['to_group']
                # Look up the destination group by name
                to_group = groups[to_group_name]
                # Sometimes the cable needs not be drawn, so
                # continue with the next connection of this group
                if not cabling.cable_needed(group, to_group):
                    continue
                # Otherwise, handle the general case.
                # Increment the number of cable bundles drawn in the current rack
                cable_bundles_drawn[rack['name']] += 1
                # If the connection goes to a _different_ rack, then
                # also increment the number of cable bundles drawn in
                # _that_ rack. This check is to make sure that a
                # connection within a single rack is not counted twice
                dst_rack = to_group['rack']
                if rack != dst_rack:
                    cable_bundles_drawn[dst_rack['name']] += 1
                
                # Check if colours are already defined for this connection
                if 'cable_colours' in conn:
                    # Use supplied colours
                    colour_solid, colour_dashed = conn['cable_colours']
                else:
                    # If not, assign colours automatically, based on group names.
                    # Get the first colour: use group names in one order:
                    colour_solid = colours.get_colours([group['name'], to_group['name']])
                    # Get the second colour: interchange group names to get a completely
                    # different colour
                    colour_dashed = colours.get_colours([to_group['name'], group['name']])
                    # Save the colours for future reuse. They may be needed,
                    # for example, for printing the cabling table in SVG format
                    conn['cable_colours'] = (colour_solid, colour_dashed)
                
                # The first thing to do is to move the drawing pen to its
                # initial place. We start by calculating initial "x" and "y"
                # coordinates.                
                x_src = x_position_cable(group['rack'])
                    
                # There is also the destination rack (possibly the same as
                # the source rack), and we want to calculate the "x" position
                # of the end of the cable in that rack.
                x_dst = x_position_cable(dst_rack)
                
                # The initial "y" position is at the top of the rack,
                # with an adjustment for rack border. We will move down later
                y_initial = start_y + rack_border_thickness // 2
                
                # Vertical ("y") position of the cable's start is at
                # the middle of the block, plus/minus a small addition.
                # This addition is for the sake of beauty, so that
                # the cable does not overlap with block's label unnecessarily.
                # The addition is specified in rack-mount units
                addition = 0.3
                
                # Now, we have to act differently depending on whether
                # the destination group is located:
                # 1. In the same rack: draw a cable within the rack
                # 2. In the other rack in the same row: draw the cable
                #    upward to the overhead tray, draw it along the tray,
                #    and descend at the destination rack (the most
                #    elaborate case).
                # 3. In a different row: draw upward, then along
                #    the tray until the corresponding column, then stop.
                
                # First, check whether the racks are in the same row
                # (this covers cases 1 and 2 above)
                if rack['r_y'] == dst_rack['r_y']:                      
                    # Maybe the racks are also in the same column ("r_x"), which means
                    # that the source and the destination blocks are essentially in the
                    # same rack? In this case, the cable lies entirely within the rack.
                    # (This covers case 1 above)
                    if rack['r_x'] == dst_rack['r_x']:
                        if debug:
                            print('> Group "{}" connects to group "{}" in the same rack'.format(group['name'], to_group['name']))
                        # If the source block is _above_ the destination block
                        # (e.g., edge switch above its servers), then the cable
                        # goes downward, and it should start _below_ the block's
                        # label: invert the addition
                        if group['start'] > to_group['end']:
                            addition = -addition
                        # Calculate cable position in the source block
                        y_src = y_position(group, addition)
                        # Same with the destination block, only the
                        # "addition" is inverted
                        y_dst = y_position(to_group, -addition)
                        # Move the pen to its initial position in the source block,
                        # then draw a vertical line to the "y" position of the
                        # destination block
                        d = ['M', x_src, y_src, 'L', x_src, y_dst]
                        # Draw the paths, using the path specification in "d"
                        # and colours in "colour_solid" and "colour_dashed"
                        draw_paths()
                    else:
                        # No, not in the same rack; just in the same row.
                        # (This covers case 2 above)
                        # This section is only executed if we decided to
                        # draw overhead cables:
                        if draw_overhead_cables:
                            if debug:
                                print('> Group "{}" connects to group "{}" in the same row'.format(group['name'], to_group['name']))
                            # Every time some cable bundle exits the rack, it must
                            # be routed through the overhead tray, and we keep track
                            # of this number
                            current_overhead_num += 1
                            # In this case, the cable goes upward, and there is
                            # no need to invert the addition if one block is higher
                            # than the other. "y" positions in source and destination
                            # blocks are calculated in the same way:
                            y_src = y_position(group, addition)
                            y_dst = y_position(to_group, addition)
                            # The "y" position of the tray is therefore above
                            # the top border of racks, "start_y"
                            y_tray = start_y - h_tray
                            # The horizontal segment of the cable will lie
                            # yet higher, determined by the cable's number
                            y_cable = y_tray - (current_overhead_num - 1) * cable_thickness * 2
                            # The path starts in the source rack, then proceeds upward,
                            # then goes horizontally, and finally descends down
                            d = ['M', x_src, y_src, 'L', x_src, y_cable, 'L', x_dst, y_cable, 'L', x_dst, y_dst]
                            draw_paths()
                else:
                    # The two groups are in different rows. This covers case 3 above.
                    # It's very similar to case 2 but simpler, because we don't have
                    # to descend to the destination rack: the cable just ends abruptly
                    # at the overhead tray.
                    # This section is only executed if we decided to
                    # draw overhead cables:
                    if draw_overhead_cables:
                        if debug:
                            print('> Group "{}" connects to group "{}" in different rows'.format(group['name'], to_group['name']))
                        # As with the case 2, increment the number of cable
                        # bundles routed through the overhead tray
                        current_overhead_num += 1
                        # The rest is also calculated like in case 2
                        y_src = y_position(group, addition)
                        # The "y" position of the tray
                        y_tray = start_y - h_tray
                        # The horizontal segment of the cable will lie
                        # yet higher, determined by the cable's number
                        y_cable = y_tray - (current_overhead_num - 1) * cable_thickness * 2
                        # The "x" position where our cable "ends abruptly" is defined
                        # in the middle of the rack
                        x_dst = x_position_rack(dst_rack) + rack_w // 2
                        # The path starts in the source rack, then proceeds upward,
                        # then goes horizontally
                        d = ['M', x_src, y_src, 'L', x_src, y_cable, 'L', x_dst, y_cable]
                        draw_paths()
        return current_overhead_num

    def calculate_overhead_bundles(row, groups):
        '''Calculates how many bundles of cables must be drawn above
        the current row. "row" is the list of racks in the current row.
        This function is similar to "calculate_bundles()", but must be
        applied to each row in sequence.'''
        # Count the number of overhead bundles
        overhead_bundles = 0
        # Iterate through each rack in list "row", which
        # contains racks belonging to only one row
        for rack in row:
            # Select only non-empty blocks (because empty space in a rack
            # cannot be connected with a cable to anything)
            blocks = [b for b in rack['block_list'] if b['type'] != cEmpty]
            # Iterate through selected non-empty blocks
            for b in blocks:
                # Look up the group by its name and check
                # if any connections are defined
                group = groups[b['name']]
                # Skip groups with no connections
                if not 'connections' in group:
                    continue
                # Iterate through connections
                for conn in group['connections']:
                    # Where the cable bundle goes: the destination group
                    dst_group_name = conn['to_group']
                    # What rack is it in? Look up the group by name
                    # and extract its "rack" element
                    dst_rack = groups[dst_group_name]['rack']
                    # If the destination rack is _different_ from the current one,
                    # increment the number of inter-rack cables to be drawn
                    # (because if the connection is within one rack, it will not use
                    # space in the overhead tray)
                    if dst_rack != rack:
                        overhead_bundles += 1
        if debug:
            # Print the number of cable bundles that will be drawn
            # Take row number from any rack in this row,
            # e.g., the first rack (that is, with index 0)
            print('> Overhead cable bundles above row {}: {}'.format(row[0]['r_y'], overhead_bundles))
        # Return the result
        return overhead_bundles
    
    # Start with defining some constants. This is placed in the
    # beginning of the function for easy changing.
    
    # From rack parameters, calculate rack's width and height for drawing, in pixels
    rack_h = calculate_length_pixels(rack_params['h'])
    rack_w = calculate_length_pixels(rack_params['w'])
    # Rack border thickness, in pixels
    rack_border_thickness = round(delta / 20)
    # Cable thickness
    cable_thickness = rack_border_thickness * 2
    # Set the left margin: the "x" position on the canvas
    start_x = delta

    # Only select racks whose "r_y" position (that is, the row
    # in which they are located) is equal to the specified row number
    row = [rack for rack in r if rack['r_y'] == row_num]
    
    # Calculate the number of overhead cable bundles above this row
    overhead_cables = calculate_overhead_bundles(row, groups)
    
    # There are two situations when we want to draw overhead cables:
    # (1) The number of overhead cable bundles calculated above
    # is more than zero (that is, there is something to draw) -AND-
    # (2) Either of the following holds true:
    #   (a) This number of cables is less than the maximal allowed
    #       number of cables stipulated in "max_cables", -OR-
    #   (b) "max_cables" is zero, which means to always draw all cables
    draw_overhead_cables = (overhead_cables > 0) and (overhead_cables < max_cables or max_cables == 0)
    
    # If we find that we will NOT draw the overhead cables, inform the user
    if not draw_overhead_cables:
        # Do not warn if there was nothing to draw in the first place
        if overhead_cables > 0:
            print('>> Warning! In row {}, the number of overhead cables {} is larger than the maximal allowed number, {}. Not drawing overhead cables for this row'.format(row_num, overhead_cables, max_cables))
    
    # If we are to draw overhead cables, add some space above racks
    if draw_overhead_cables:
        # Height of the overhead tray above the rack, in pixels
        h_tray = calculate_length_pixels(rack_params['o_t'])
        # Add it to the starting "y" position
        start_y += h_tray
        # Reserve space for drawing cables, proportional to their number
        # Add the number of cables times twice the thickness of each cable, in pixels
        start_y += overhead_cables * cable_thickness * 2

    # Make sure there are, indeed, some racks in this row
    if len(row) == 0:
        print('> There are no racks in row {}'.format(row_num))
        return
    
    # "row" is now the list of rack. Sort it according to rack's "r_x" coordinate
    row = sorted(row, key=get_rack_x)

    # Create a definition for a rack carcass so that we can reuse it often
    # Warning! The "svgwrite" library seemingly doesn't have means to check
    # whether a specific object has already been added, therefore we are forced
    # to create the same rack definition for each row over and over again. It
    # doesn't hurt the output, but the resulting SVG file will contain some
    # redundant information in the "definitions" section.
    rack_def = svg.rect(
        id = 'rack',
        size = (rack_w, rack_h),
        fill = 'none',
        stroke = 'black',
        stroke_width = rack_border_thickness
    )
    # Add the carcass to the definitions section of SVG file
    svg.defs.add(rack_def)
    
    # As we will be consequently drawing _inside_ the rack, we need
    # to slightly decrease our drawing area to account for rack border
    # thickness, because we don't want rectangles denoting equipment to
    # overlap with rack borders (one border width seems to be enough).
    rack_w_inside = rack_w - rack_border_thickness
    rack_h_inside = rack_h - rack_border_thickness
    # Size of one rack-mount unit, in pixels. Beware: it's a
    # floating-point value (required for exactness of measurement),
    # therefore don't forget to round() results involving this variable
    one_unit = rack_h_inside / rack_size
    
    # Iterate through all racks in the row
    for rack in row:
        # Draw rack's carcass, equipment blocks inside it, and
        # the rack's label
        draw_blocks()
        
    # Draw cables _after_ blocks, so that they appear in the foreground
    # and don't get overlapped by blocks.
    # Also, keep track of the number of overhead cable bundles
    # drawn so far above the current row. Start with zero and
    # update with each call
    current_overhead_num = 0
    for rack in row:
        current_overhead_num = draw_cables(current_overhead_num, draw_overhead_cables)

    # The right border of the SVG document must be set to the right border
    # of the rightmost rack, plus some slack, to make sure the SVG document
    # is completely rendered in viewing programs. Multiply rack width by the
    # number of racks in the row, and add "start_x" twice for symmetry, to
    # create empty space on the left and on the right.
    right_border = start_x + rack_w * len(row) + start_x
    # If the current SVG document's width is lower, update it
    if svg['width'] < right_border:
        svg['width'] = right_border
    
    # Also update the bottom border of the SVG document: set it to the top
    # of the row, plus rack height, plus some slack
    svg['height'] = start_y + rack_h + 5 * delta

    # If we reached here, all went well.
    # Return "True" to indicate success
    return True
    
# -------------- Module Initialisation --------------

# Try to import the corresponding module for SVG output,
# set the flag accordingly
try:
    import svgwrite
    # If reached here, import was successful
    svg_output_available = True
except ImportError:
    # Issue a hint to the user
    print('> Hint: Library "svgwrite" not found, SVG output will be unavailable')
    svg_output_available = False

# A small slack to be inserted between objects on the screen, in pixels
# Also used as a scaling factor. The meaning is how many pixels to draw
# for each decimetre (10 centimetres) of physical space:
delta = 50
