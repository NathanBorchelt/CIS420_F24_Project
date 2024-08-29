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

# Our own modules:
import cabling
import common
from strconst import *

def get_performance_str(perf):
    '''Receives performance in GFLOPS, returns
    a nicely formatted string with a correct unit
    of measurement: e.g., "1,25 TFLOPS" or "2,9 PFLOPS".
    '''
    # Choose an appropriate measurement unit
    perf_units = ['G', 'T', 'P', 'E']
    i = 0
    while i < len(perf_units) - 1:
        # If less than 1000, exit immediately
        if perf < 1000:
            break
        # More than 1000, divide
        perf /= 1000;
        # Increment the counter
        i += 1
    # Use the index of the unit when the loop terminated
    unit = perf_units[i] + 'FLOPS'
    # Round to two decimal digits
    perf = round(perf, 2)
    # If it is a float without a fractional part, return an integer
    if isinstance(perf, float):
        if perf.is_integer():
            perf = int(perf)
    # The resulting string
    result = '{} {}'.format(perf, unit)
    return result
    
def get_power_str(power):
    '''Receives power in Watts, returns
    a nicely formatted string with a correct unit
    of measurement: e.g., "2,3 kW" or "1,6 MW".
    '''
    # Choose an appropriate measurement unit
    # to be added to Watts (W) on the left
    power_units = ['', 'k', 'M']
    i = 0
    while i < len(power_units) - 1:
        # If less than 1000, exit immediately
        if power < 1000:
            break
        # More than 1000, divide
        power /= 1000;
        # Increment the counter
        i += 1
    # Use the index of the unit when the loop terminated
    unit = power_units[i] + 'W'
    # Round to two decimal digits
    power = round(power, 2)
    # If it is a float without a fractional part, return an integer
    if isinstance(power, float):
        if power.is_integer():
            power = int(power)
    # The resulting string
    result = '{} {}'.format(power, unit)
    return result
    
def get_config_text(config):
    '''Prints all available metrics for a single configuration.'''
    # The output is initially empty
    out = []
    
    # Check if it is compute equipment; if so, print details
    if cNodeModel in config:
        out.append(cIndent + 'Number of compute nodes: {:,}'.format(config[cNodes]))
        out.append(cIndent + 'Compute node model: {}'.format(config[cNodeModel]))

        # If node peak performance is available
        if cNodePeakPerformance in config:
            # Get it
            node_peak_perf = config[cNodePeakPerformance]
            # Calculate peak performance of the entire group
            group_peak_perf = node_peak_perf * config[cNodes]
            # Format strings for pretty printing
            s1 = get_performance_str(node_peak_perf)
            s2 = get_performance_str(group_peak_perf)
        else:
            # Just not available
            s1 = cNotAvailable
            s2 = s1
        # Print whatever we have available
        out.append(cIndent + 'Node peak performance:  ' + s1)
        out.append(cIndent + 'Group peak performance: ' + s2)
        
        # Group power
        out.append(cIndent + 'Group power: ' + get_power_str(config[cPower]))

        out.append('')
        out.append(cIndent + 'Node form factor: {}'.format(config[cNodeFormFactor]))
        out.append(cIndent + 'Node cost: {:,}'.format(config[cNodeCost]))
        out.append(cIndent + 'CPU model: {}'.format(config[cCpuModel]))
        out.append(cIndent + 'CPU clock frequency, GHz: {}'.format(config[cCpuFrequency]))
        out.append(cIndent + 'Cores per CPU: {}'.format(config[cCpuCores]))
        out.append(cIndent + 'CPU count: {}'.format(config[cNodeCpuCount]))

        # Power of a compute node
        out.append(cIndent + 'Node power: ' + get_power_str(config[cNodePower]))

        # Memory information
        out.append(cIndent + 'Main memory:')
        out.append(cIndent * 2 + 'Size: {}'.format(config[cNodeMainMemorySize]))
        out.append(cIndent * 2 + 'Type: {}'.format(config[cMainMemoryType]))
        out.append(cIndent * 2 + 'Speed: {}'.format(config[cMainMemorySpeed]))
        
        # Network characteristics
        out.extend(get_network_text(config))
                
        # Add UPS characteristics
        out.extend(get_ups_text(config))

        # Add economic characteristics
        out.extend(get_config_economic_text(config))

    else:
        # Unknown equipment
        # We might try to print equipment size at least
        if cEquipmentSize in config:
            out.append(cIndent + 'Equipment size: {}'.format(c[cEquipmentSize]))
        out.append(cIndent + '(More information not available)')
    
    # Return the output
    return out

def get_network_text(c):
    '''Get information about network characteristics of a configuration.'''
    # The output is initially empty
    out = []
    # Proceed if network topology is defined
    if cNetworkTopology in c:
        out.append(cIndent + 'Network:')
        out.append(cIndent * 2 + 'Topology: {}'.format(c[cNetworkTopology]))
        out.append(cIndent * 2 + 'Cost: {:,}'.format(c[cNetworkCost]))
        # Print info about edge switches
        if cNetworkEdgeSwitchCount in c:
            out.append(cIndent * 2 + 'Edge switches:')
            out.append(cIndent * 3 + 'Model: {}'.format(c[cNetworkEdgeSwitchModel]))
            out.append(cIndent * 3 + 'Count: {:,}'.format(c[cNetworkEdgeSwitchCount]))
            out.append(cIndent * 3 + 'Size, RU: {}'.format(c[cNetworkEdgeSwitchSize]))
        # Same with core switches: if their count is present...
        if cNetworkCoreSwitchCount in c:
            # ...and not zero
            if c[cNetworkCoreSwitchCount] != 0:
                out.append(cIndent * 2 + 'Core switches:')
                out.append(cIndent * 3 + 'Model: {}'.format(c[cNetworkCoreSwitchModel]))
                out.append(cIndent * 3 + 'Count: {:,}'.format(c[cNetworkCoreSwitchCount]))
                out.append(cIndent * 3 + 'Size, RU: {}'.format(c[cNetworkCoreSwitchSize]))
    # Return the output, whether empty or not
    return out

def get_ups_text(c):
    '''Get information about the UPS system of a configuration.'''
    # The output is initially empty
    out = []
    # Proceed if the UPS has been designed
    if cUpsCost in c:
        out.append(cIndent + 'UPS System:')
        # Check the existence of every metric before printing it
        if cUpsModel in c:
            out.append(cIndent * 2 + 'UPS model: {}'.format(c[cUpsModel]))
        if cUpsBackupTime in c:
            out.append(cIndent * 2 + 'Backup time: {:,}'.format(c[cUpsBackupTime]))
        if cUpsPowerRating in c:
            out.append(cIndent * 2 + 'Power rating: {:,}'.format(c[cUpsPowerRating]))
        if cUpsPartitioning in c:
            out.append(cIndent * 2 + 'Partitioning into blocks: {}'.format(c[cUpsPartitioning]))
        
        # Size: can be racks or small blocks
        if cUpsSizeRacks in c:
            out.append(cIndent * 2 + 'Size (racks): {:,}'.format(c[cUpsSizeRacks]))
        elif cUpsSize in c:
            out.append(cIndent * 2 + 'Size, RU: {:,}'.format(c[cUpsSize]))

        if cUpsWeight in c:
            out.append(cIndent * 2 + 'Weight: {:,}'.format(c[cUpsWeight]))
        
        # Cost is always available, due to the earlier check
        out.append(cIndent * 2 + 'UPS cost: {:,}'.format(c[cUpsCost]))

    # Return the output, whether empty or not
    return out

def get_config_economic_text(c):
    '''Get information about economic characteristics
    of an individual configuration.'''
    # The output is initially empty
    out = []
    
    # Proceed only if CapEx is defined; if not, exit, because
    # there is nothing to print
    if not cCapex in c:
        return out
    
    # If we reached here, we can proceed
    out.append(cIndent + 'Economic characteristics:')

    # With OpEx and TCO we should check that characteristics
    # exist before using them. It's because if system lifetime
    # is not specified, then OpEx and TCO are not calculated.
    # Try to fetch TCO
    if cTco in c:
        tco = c[cTco]

    # Capital expenditures; certain to exist due to earlier check
    capex = c[cCapex]
    if cTco in c:
        # Also print CapEx share in TCO
        capex_share = round(capex / tco * 100, 2)
        out.append(cIndent * 2 + 'Capital expenditures: {:,} ({}% of TCO)'.format(capex, capex_share))
    else:
        out.append(cIndent * 2 + 'Capital expenditures: {:,}'.format(capex))
    
    # Operating expenditures. Check first that it exists
    if cOpex in c:
        opex = c[cOpex]
        # Print OpEx share in TCO
        opex_share = round(opex / tco * 100, 2)
        out.append(cIndent * 2 + 'Operating expenditures: {:,} ({}% of TCO)'.format(opex, opex_share))
    else:
        out.append(cIndent * 2 + 'Operating expenditures: {:,}'.format(opex))

    # TCO
    if cTco in c:
        s = c[cTco]
    else:
        s = cNotAvailable
    out.append(cIndent * 2 + 'Total cost of ownership: {:,}'.format(s))    
    
    # Return the output
    return out

def get_design_wide_economic_text(m):
    '''Get information about economic characteristics of the
    entire design. Works with a dictionary of metrics, "m".'''
    # The output is initially empty
    out = []
    
    # Print header first
    width = 60
    header = 'Design-wide metrics'
    out.extend(common.get_header(header, width))

    # We print metrics from dictionary "m". Initially, it contains
    # only constants. However, as the number of compute nodes becomes
    # known and infrastructural equipment is added, things like
    # capital and operating costs become available.
    
    # Print initial constants
    if cSystemLifetime in m:
        out.append('{:35} {}'.format('System lifetime, years:', m[cSystemLifetime]))
    if cElectricityPrice in m:
        out.append('{:35} {:.2}'.format('Electricity price per kWh*hour:', m[cElectricityPrice]))
    if cRackStationingPerYear in m:
        out.append('{:35} {:,}'.format('Rack stationing costs per year:', m[cRackStationingPerYear]))
    
    # If capital costs are not yet calculated, exit with what we already have
    if not cCapex in m:
        return out
    
    capex_str = 'Capital expenditures:'
    opex_str = 'Operating expenditures:'
    tco_str = 'Total cost of ownership:'
    
    # If we reached here, we can proceed with printing costs
    # With OpEx and TCO we should check that characteristics
    # exist before using them. It's because if system lifetime
    # is not specified, then OpEx and TCO are not calculated.
    # Try to fetch TCO
    if cTco in m:
        tco = m[cTco]

    # Capital expenditures; certain to exist due to earlier check
    capex = m[cCapex]
    if cTco in m:
        # Also print CapEx share in TCO
        capex_share = round(capex / tco * 100, 2)
        out.append('{:35} {:,} ({}% of TCO)'.format(capex_str, capex, capex_share))
    else:
        out.append('{:35} {:,}'.format(capex_str, capex))
    
    # Operating expenditures. Check first that it exists
    if cOpex in m:
        opex = m[cOpex]
        if cTco in m:
            # Print OpEx share in TCO
            opex_share = round(opex / tco * 100, 2)
            out.append('{:35} {:,} ({}% of TCO)'.format(opex_str, opex, opex_share))
        else:
            out.append('{:35} {:,}'.format(opex_str, opex))
    else:
        out.append('{:35} {}'.format(opex_str, cNotAvailable))

    # TCO
    if cTco in m:
        s = m[cTco]
    else:
        s = cNotAvailable
    out.append('{:35} {:,}'.format(tco_str, s))
    
    # Power and tomato equivalent
    out.append('{:35} {:,}'.format('Power, W:', m[cPower]))
    out.append('{:35} {:,}'.format('Tomato equivalent, kg/day:', get_tomato_equivalent(m[cPower])))
    
    # Finally, weight
    out.append('{:35} {:,}'.format('Weight:', m[cWeight]))
    
    # Return the output
    return out

def get_rack_text(m, rs, rp):
    '''Prepares a text representation of rack information:
    how many racks we have, floor space size, etc.'''
    # Here we will hold our output
    out = []
    # Print header first
    width = 60
    header = 'Racks and floor space'
    out.extend(common.get_header(header, width))
    
    # If equipment hasn't been placed into racks yet, there is nothing to print
    # We make sure by checking whether "racks" is defined
    if not cRacks in rp:
        out.append('No racks found. Equipment has to be placed into racks first.')
        return out
    
    # If we reached here, racks were found
    out.append('Rack count: {:,}'.format(rp[cRacks]))
    
    # Parameters of racks (constants set in the main module)
    out.append('Rack size, in rack-mount units: {}'.format(rs))
    out.append('Rack physical dimensions and clearances:')
    out.append(cIndent + 'Width: {}'.format(rp['w']))
    out.append(cIndent + 'Depth: {}'.format(rp['d']))
    out.append(cIndent + 'Height: {}'.format(rp['h']))
    out.append(cIndent + 'Side clearance: {}'.format(rp['c_s']))
    out.append(cIndent + 'Front and back clearance: {}'.format(rp['c_f']))
    out.append(cIndent + 'Aisle width: {}'.format(rp['c_a']))
    out.append(cIndent + 'Maximal length of row segments: {}'.format(rp['l_xc']))
    out.append(cIndent + 'Overhead tray elevation: {}'.format(rp['o_t']))
    
    # Floor plan parameters: only available after equipment had
    # been placed into racks with place_equipment().
    if cRacks in rp:
        out.append('Floor space:')
        out.append(cIndent + 'Dimensions: {} x {}'.format(rp[cFloorSpaceXDimension],
            rp[cFloorSpaceYDimension]))
        out.append(cIndent + 'Size: {}'.format(rp[cFloorSpaceSize]))
        out.append(cIndent + 'Rows of racks: {}'.format(rp[cRows]))
        out.append(cIndent + 'Racks per row: {}'.format(rp[cRacksPerRow]))
        out.append(cIndent + 'Row formula: "{}"'.format(rp[cFloorSpacePlanFormula]))
    
    # Return the output
    return out
    
def get_design_text(eg, c, m, rs, rp, debug):
    '''Prepares a text representation of design information
    for further printing or saving to a file.'''
    # Here we will hold our output
    out = []
    # Start with printing equipment groups
    # How many groups do we have?
    len_eg = len(eg)
    # If no groups, exit immediately
    if len(eg) == 0:
        print('> No groups of equipment found, nothing to print.')
        return
    # Print header first
    width = 60
    header = 'Groups of Equipment ({} group(s) total)'.format(len_eg)
    out.extend(common.get_header(header, width))
    # Iterate through equipment groups
    # Keep track of the group number
    group_num = 0
    for group in eg:
        group_num += 1
        out.append('{}. Group "{}"'.format(group_num, group['name']))
        out.append('')
        # Fetch the configuration of equipment of this group
        config = group['c']
        # Add all available metrics of this configuration
        out.extend(get_config_text(config))        
        # A whitespace after each group of equipment
        out.append('')
    
    # Finished with printing equipment groups.
    # Now it's time to print design-wide characteristics
    
    # Print rack information
    out.extend(get_rack_text(m, rs, rp))
    
    # Print bill of materials for cables
    out.extend(cabling.cables_bom_text(c, eg))
    
    # Print design-wide economic characteristics
    out.extend(get_design_wide_economic_text(m))

    # Finally, return the result
    out.append('')
    return out

def get_tomato_equivalent(power):
    '''Return the so-called "tomato equivalent" --
    the amount of tomato crops that could be harvested
    annually if heat generated by a cluster would be
    used in a tomato greenhouse. See this for details:
    http://clusterdesign.org/2012/08/fruits-of-computing-redefining-green-in-hpc-energy-usage/
    The calculations for heat reuse were performed by
    Dr. Joshua M. Pearce and Rob Andrews from
    Queen’s University, Canada.'''
    # The value, of course, depends on climate,
    # and the current value for climate in Ontario,
    # Canada, is 400 kg per day per MW of power:
    kg_per_day_per_mw = 400
    # Round the result to 1 decimal digit
    result = round((power / 1000000) * kg_per_day_per_mw, 1)
    return result

def print_all(eg, c, m, rs, rp, filename, debug):
    '''Prints all available information about the current design.
    If "filename" is non-empty, save to a file, otherwise print on
    the screen.'''
    text_repr = get_design_text(eg, c, m, rs, rp, debug)
    # If there was a gross error, pass it on to the caller
    if text_repr == None: return
    # If "filename" is empty, print on the screen
    if filename == '':
        for line in text_repr:
            print(line)
    else:
        with open(filename, 'w') as f:
            for line in text_repr:
                print(line, file=f)
    # Return "True" to indicate success
    return True
