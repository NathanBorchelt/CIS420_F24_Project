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

import re
import inspect

# Useful functions, used by all modules of the project

def convert_str(s):
    '''Converts an input string into a floating-point number; if that fails, tries to
    convert to an integer or a boolean value, in case of yet another failure
    returns the string intact.'''
    # There is a special case when the input string is a floating-point number,
    # _but_ with a comma as a decimal separator. Detect this case and replace the comma
    # with the decimal point.
    if fp_with_comma_regexp.match(s) != None:
        # Match found, replace the comma with the decimal point
        s = s.replace(',', '.')
    # Now, do the usual processing
    # Try to convert to a floating-point number; if that fails, try integer
    try:
        c = float(s)
        # A special case is when the value is actually an integer, and
        # then it makes no sense to retain the zero fractional part
        if c.is_integer():
            c = int(c)
    except:
        # Failed; try integer (we can't try the integer conversion first,
        # because the fractional part will then be lost: int(4.7) yields 4)
        try:
            c = int(s)
        except:
            # Also failed, so it's probably a string. But it could also be
            # a boolean value. Do one last test
            if s.lower() == str(True).lower(): c = True
            elif s.lower() == str(False).lower(): c = False
            else: c = s # It's just a string
    # When we reach here, the type of "c" is either a float,
    # an integer, a boolean value or a string.
    return c

def get_header(header, width):
    '''Forms a beautiful underlined header, centred with a specified width.'''
    # The output is initially empty
    out = []
    out.append('')
    out.append(header.center(width))
    out.append(('-' * len(header)).center(width))
    out.append('')
    return out

def whoami():
    '''Return the name of the function being called.
    See this excellent recipe by Stefaan Lippens:
    http://stefaanlippens.net/python_inspect
    '''
    return inspect.stack()[1][3]

# Module initialisation

# Compile a regular expression for a floating-point value using comma
# as a separator. This matches an entire string (note the starting '^' and
# the closing '$') consisting of zero or more digits, then a comma, and then
# one or more digits. This will match "0,6" and ",35", but not "1,2,3"
fp_with_comma_regexp=re.compile('^\d*,\d+$')
