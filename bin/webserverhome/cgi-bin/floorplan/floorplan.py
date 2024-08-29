# If this file is called directly (not imported as a module):
if __name__ == "__main__":
    exit()

#    Copyright (C) 2013 Konstantin S. Solnushkin
#
#    This file is part of "floor sizer", the tool to determine the floor space
#    required for a certain number of racks with computer equipment.
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

import math

# Define a function that calculates the floor space
def calculate_floor_space_size(r):
    '''Calculates the required floor space size for equipment. Uses values passed in "r". '''
    # Define local variables to make formulae easier to read
    racks = r["racks"]
    w = r["w"]
    d = r["d"]
    c_s = r["c_s"]
    c_f = r["c_f"]
    c_a = r["c_a"]
    l_xc = r["l_xc"]
    # Coefficients of the quadratic equation
    a = d + c_a
    b = 2 * c_f - 2 * c_s - c_a
    c = - racks * w * (1 + c_s / l_xc)
    # Discriminant
    discr = b * b - 4 * a * c
    # Roots
    r_y_1 = (- b + math.sqrt(discr)) / (2 * a)
    r_y_2 = (- b - math.sqrt(discr)) / (2 * a)
    # We take the positive root using "max", and round it to the nearest integer
    r_y_int = round(max(r_y_1, r_y_2))
    # The result is the number of rows
    r["rows"] = r_y_int
    # Floor width
    l_y = r_y_int * d + (r_y_int - 1) * c_a + 2 * c_f
    l_y_rounded = round(l_y, 2)
    r["floor_space_y_dimension"] = l_y_rounded
    # Number of racks per row
    r_x_int = math.ceil(racks / r_y_int)
    r["racks_per_row"] = r_x_int
    # Number of segments
    segments = math.ceil(r_x_int *  w / l_xc)
    # Account for a special case when "l_xc=0", and the user requested to ignore max length of rows:
    if segments == 0:
        segments = 1
    # Number of gaps between segments
    gaps = segments - 1
    r["segments"] = segments
    r["gaps"] = gaps
    # Floor length
    l_x = r_x_int * w + 2 * c_s + gaps * c_s
    l_x_rounded = round(l_x, 2)
    r["floor_space_x_dimension"] = l_x_rounded
    # Floor space
    floor_space = l_x * l_y
    # Round to two decimal digits
    floor_space_rounded = round(floor_space, 2)
    r["floor_space_size"] = floor_space_rounded
    # Prepare the floor space formula:
    # Length of big segments, in racks:
    big_segment_racks = math.ceil(r_x_int / segments)
    # Save it in the output
    r["racks_per_segment"] = big_segment_racks
    # Start filling the floor space formula:
    floor_space_plan_str = str(big_segment_racks)
    # Number of big segments:
    big_segments = r_x_int // big_segment_racks
    if big_segments > 1:
        floor_space_plan_str = str(big_segments) + "*" + floor_space_plan_str
    # Last, smaller segment (if any):
    last_segment_racks = r_x_int - big_segments * big_segment_racks
    # If the last segment is non-emtpy, add info about it
    if last_segment_racks != 0:
        floor_space_plan_str = floor_space_plan_str + "+" + str(last_segment_racks)
    # If the number of racks is bigger than one, add info about it
    if r_y_int > 1:
        floor_space_plan_str = "(" + floor_space_plan_str + ")*" + str(r_y_int)
    r["floor_space_plan_formula"] = floor_space_plan_str
