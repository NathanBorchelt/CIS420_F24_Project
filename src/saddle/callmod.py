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

import urllib.parse
import urllib.request

# Our own modules:
import common
from strconst import *

def call_design_module(db, module, debug=0, disable=0):
    '''A generic function to call web-based design modules.
    If errors are encountered, a configuration is skipped, and
    if "disable" is non-zero, then it is additionally disabled.'''
    
    # Parse the URL of the module
    (scheme, netloc, path, params, original_query, fragment) = urllib.parse.urlparse(module[cUrl])
    # Split the original query string into fields
    q_dict = urllib.parse.parse_qs(original_query)
    
    # Keep track of the number of configurations processed
    # so far and the number of errors
    count = 0
    error_count = 0
    
    # Process configurations
    for c in db:
        count += 1
        # Skip disabled configurations
        if c[cEnabled] == False:
            continue
        
        # Iterate through each metric that we MUST send
        for m in module['send']:
            try:
                # Add metrics to the dictionary: add a list with a single item;
                # this is required by the "urllib" module
                q_dict[m] = [ c[m] ]
            except KeyError:
                print('Configuration No. {}: Error when performing the request in module "{}": metric "{}" needs to be sent but not found'.format(count, module['name'], m))
                return (None, error_count)
        
        # There might also be metrics that we should TRY to find
        # in the configuration and send, but if they are not found,
        # that's OK and we should not raise an error
        if 'try_send' in module:
            for m in module['try_send']:
                try:
                    # Add metrics to the dictionary: add a list with a single item;
                    # this is required by the "urllib" module
                    q_dict[m] = [ c[m] ]
                except KeyError:
                    pass
        
        # Form the new query string and then the URL
        query = urllib.parse.urlencode(q_dict, doseq=True)
        url = urllib.parse.urlunparse((scheme, netloc, path, params, query, fragment))
        # If debug was requested, inform the user
        if debug:
            print('Processing configuration No. {} with design module "{}"'.format(count, module['name']))
            print('Query URL: ', url)
            print()
        # Perform the request
        try:
            with urllib.request.urlopen(url) as resp:
                response = resp.read()
        except urllib.error.URLError:
            error_count += 1
            answer = input('Configuration No. {}: Error when performing the request in module "{}", interrupt (Y/N)? '.format(count,
                module['name']))
            print('Failed to call the design module')
            if answer in ['Y', 'y']:
                # Exit the function immediately
                return (None, error_count)
            else:
                # If need to disable
                if disable == 1:
                    c[cEnabled] = False
                    c[cReasonDisabled] = 'Error when calling design module {}'.format(module['name'])
                # Continue with the next configuration
                continue
        # Split lines of the response
        resp_list = [l for l in bytes.decode(bytes(response)).splitlines()]
        if debug:
            print('Server responded: ')
            for l in resp_list:
                print(l)
            print()
        # Establish an empty dictionary
        r = {}
        # Parse the lines in the response
        for l in resp_list:
            # Skip empty lines
            if len(l) == 0:
                continue
            # Split into "key=value" pairs
            [key, value] = l.split('=', maxsplit=1)
            # Lower the case
            key = key.lower()
            # Convert the value to the proper type
            value = common.convert_str(value)
            # Add to the dictionary
            r[key] = value
        # Did the model return an error?
        # (Indicated by the presence of "status=error" in the output)
        if (cStatus in r) and (r[cStatus] == cError):
            # Disable the configuration with the relevant error message
            # from the model output
            c[cEnabled] = False
            error_count += 1
            if (cErrorMessage in r):
                c[cReasonDisabled] = r[cErrorMessage]
            else:
                # String with error message not found in the output,
                # report a generic error
                c[cReasonDisabled] = 'Web module "{}" reported generic error'.format(module['name'])
            # Continue with the next configuration
            continue
        # There was no error; check all output strings and see which we need to save
        for key in r:
            # Compare: is this something we need to save?
            if key in module['receive']:
                # Add the metric returned by the model into the configuration
                c[key] = r[key]
    # All configurations have been processed, and errors (if any) counted.
    # Return the updated list
    return (db, error_count)
