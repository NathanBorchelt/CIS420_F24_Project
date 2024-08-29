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

# This module ONLY contains string constants
# It is intended to be imported with a simple statement:
#
# from strconst import *

# String constants:
cBottom                     = 'bottom'
cCommentCharacter           = '#'
cConfigurationId            = 'Configuration ID'
cConnections                = 'connections'
cConstraintViolated         = 'Constraint(s) violated'
cCapex                      = 'capex'
cCores                      = 'cores'
cCpuCores                   = 'cpu_cores'
cCpuFrequency               = 'cpu_frequency'
cCpuModel                   = 'cpu_model'
cElectricityPrice           = 'electricity_price'
cEmpty                      = 'empty'
cEnabled                    = 'enabled'
cEnclosureSize              = 'enclosure_size'
cEnclosureSwitch            = 'enclosure_switch'
cEnd                        = 'end'
cEquipmentSize              = 'equipment_size'
cError                      = 'error'
cErrorMessage               = 'error_message'
cExpressionSeparator        = ';'
cFatTree                    = 'fat-tree'
cFloorSpacePlanFormula      = 'floor_space_plan_formula'
cFloorSpaceSize             = 'floor_space_size'
cFloorSpaceXDimension       = 'floor_space_x_dimension'
cFloorSpaceYDimension       = 'floor_space_y_dimension'
cGroupList                  = 'group_list'
cGroupNameCoreSwitch        = 'core_switch'
cGroupNameEdgeSwitch        = 'edge_switch'
cGroupNameEnclosure         = 'enclosure'
cGroupNameIndivisible       = 'indivisible'
cGroupNameServers           = 'servers'
cGroupNameUps               = 'ups'
cLinksAvailable             = 'links_available'
cMainMemorySpeed            = 'main_memory_speed'
cMainMemoryType             = 'main_memory_type'
cNetworkBlockingFactor      = 'network_blocking_factor'
cNetworkCorePorts           = 'network_core_ports'
cNetworkCoreSwitchCount     = 'network_core_switch_count'
cNetworkCoreSwitchModel     = 'network_core_switch_model'
cNetworkCoreSwitchSize      = 'network_core_switch_size'
cNetworkCost                = 'network_cost'
cNetworkEdgePortsToCoreLevel= 'network_edge_ports_to_core_level'
cNetworkEdgePortsToNodes    = 'network_edge_ports_to_nodes'
cNetworkEdgeSwitchCount     = 'network_edge_switch_count'
cNetworkEdgeSwitchModel     = 'network_edge_switch_model'
cNetworkEdgeSwitchSize      = 'network_edge_switch_size'
cNetworkEquipmentSize       = 'network_equipment_size'
cNetworkExpandableTo        = 'network_expandable_to'
cNetworkPower               = 'network_power'
cNetworkTech                = 'network_tech'
cNetworkTopology            = 'network_topology'
cNetworkWeight              = 'network_weight'
cNodeCost                   = 'node_cost'
cNodeCpuCount               = 'node_cpu_count'
cNodeEquipmentSize          = 'node_equipment_size'
cNodeFormFactor             = 'node_form_factor'
cNodeMainMemorySize         = 'node_main_memory_size'
cNodeModel                  = 'node_model'
cNodePeakPerformance        = 'node_peak_performance'
cNodePower                  = 'node_power'
cNodes                      = 'nodes'
cNodeWeight                 = 'node_weight'
cNotAvailable               = 'N/A'
cOpex                       = 'opex'
cPeak                       = 'peak'
cPerformance                = 'performance'
cPerformanceThroughputMode  = 'performance_throughput_mode'
cPower                      = 'power'
cPreferLocation             = 'prefer_location'
cProjectedPerformance       = 'projected_performance'
cRacks                      = 'racks'
cRacksPerRow                = 'racks_per_row'
cRackStationingPerYear      = 'rack_stationing_per_year'
cReasonDisabled             = 'reason_disabled'
cRows                       = 'rows'
cServerSize                 = 'server_size'
cStandaloneSwitch           = 'standalone_switch'
cStar                       = 'star'
cStart                      = 'start'
cStatus                     = 'status'
cSwitchType                 = 'switch_type'
cSystemLifetime             = 'system_lifetime'
cTco                        = 'tco'
cTop                        = 'top'
cUpsCost                    = 'ups_cost'
cUpsBackupTime              = 'ups_backup_time'
cUpsHeat                    = 'ups_heat'
cUpsModel                   = 'ups_model'
cUpsPartitioning            = 'ups_partitioning'
cUpsPowerRating             = 'ups_power_rating'
cUpsSize                    = 'ups_size'
cUpsSizeRacks               = 'ups_size_racks'
cUpsWeight                  = 'ups_weight'
cUrl                        = 'url'
cWeight                     = 'weight'

# Indentation: use the specified number of space characters
cIndent = ' ' * 2
