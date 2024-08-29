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

import svgwrite
import hashlib

# Our own units
from strconst import *

def get_colours(name_list):
    '''Returns a colour. Can be passed an _arbitrary_ argument.
    If the argument is a string (or a list with one string),
    then calls get_single_colour(). In all other cases, represents
    the argument as a byte string, computes its hash, and returns
    the colour based on this hash. This way, passing even slightly
    different arguments will result in significantly different colours.'''
    # If we were passed a string
    if isinstance(name_list, str):
        return get_single_colour(name_list)
    # If we were passed a list, but with a single item
    if len(name_list) == 1:
        return get_single_colour(name_list[0])
    # If we reached here, it is the general case.
    # Represent the passed argument as a string,
    # encode it as bytes, calculate the hash function
    # of those bytes. The result is a long string
    # of hexadecimal characters, e.g.:
    # 'd8216c6d73375682950aa2f1be01c43e'
    hash_str = hashlib.md5(str.encode(str(name_list))).hexdigest()
    # Use the first hexadecimal character (e.g., 'd') to
    # represent the colour index. Convert it from hex (hence base '16')
    # to a decimal number to get a number from 0 to 15
    val = int(hash_str[0:1], 16)
    # Choose a colour from the array
    return recognised_colours[val]

def get_single_colour(name):
    if name == cEmpty:
        return 'white'
    elif cGroupNameCoreSwitch in name:
        # Light yellow for core switches
        return svgwrite.utils.rgb(252, 255, 145)
    elif cGroupNameEdgeSwitch in name:
        # Green for edge switches
        return svgwrite.utils.rgb(79, 255, 87)
    elif (cGroupNameServers in name) or (cGroupNameEnclosure in name):
        # Light green for servers and enclosures
        return svgwrite.utils.rgb(157, 255, 163)
    elif cGroupNameUps in name:
        # Light blue for UPS blocks
        return svgwrite.utils.rgb(147, 201, 255)
    else:
        # In all other cases use life-asserting orange
        return 'orange'

# Initialisation

recognised_colours = [
    'blue',
    'blueviolet',
    'cornflowerblue',
    'cyan',
    'deeppink',
    'gold',
    'greenyellow',
    'lightblue',
    'orange',
    'tomato',
    'violet',
    'white',
    'red',
    'yellow',
    'saddlebrown',
    'lightsalmon'
]
