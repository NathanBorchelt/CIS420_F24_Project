#    "example.py" - Example SADDLE program
#
#    Written in 2013-2014 by Konstantin S. Solnushkin (http://clusterdesign.org)
#
#    To the extent possible under law, the author has dedicated all copyright and related and
#    neighboring rights to this software to the public domain worldwide. This software is distributed
#    without any warranty. 
#
#    For the complete text of the "CC0 Public Domain Dedication",
#    see <http://creativecommons.org/publicdomain/zero/1.0/>. 

# --------------- SAMPLE PROGRAM ---------------

from saddle import *

# Open the database of configurations from specified files
open_db(['db/generic.xml', 'db/generic-server.xml', 'db/intel-xeon-2600.xml', 'db/intel-xeon-phi.xml', 'db/memory.xml', 'db/network.xml', 'db/hdd.xml', 'db/ssd.xml'])
save_db('generic_server.csv')
#open_db('generic_server.csv')

# Only allow configurations with InfiniBand connectivity
constraint("'InfiniBand' in network_tech")

# Only 2 CPUs
constraint("node_cpu_count == 2")

# Only the largest amount of memory per core
select_best("node_main_memory_per_core")

metric_stats("node_main_memory_per_core")

# Has three Intel solid-state drives
constraint("ssd_count == 3")

# Has zero Intel Xeon Phi coprocessors
constraint("accelerator_count==0")

# Use a heuristic to filter out unpromising configurations
metric("node_cost_to_peak_performance = node_cost / node_peak_performance")

# Select the best configuration according to the heuristic
select_best("node_cost_to_peak_performance")

# Delete inferior configurations
delete()

# Save the intermediate result so that we don't have to repeat the steps above every time
save_db('generic_server_best.csv')

# Require performance of at least 240 tasks per day
inverse_performance(240)

# Set the number of nodes and update metrics to calculate the number of cores automatically
# (Note: Tianhe-2 has roughly a 55 PFLOPS peak performance)
##metric("nodes = ceil(55000000 / node_peak_performance)")
##update_metrics()
##performance_module['url']='peak'
# Calculate performance for the said number of nodes
##performance()

# Design a network
network()
# Design a UPS system
ups()

# Save designed equipment in a group
add_group('compute')

# Place equipment (the default is to place as densely as possible)
place()

# Route cables and calculate their length
cables()

# Print technical and economic metrics of the design
print_design()

# Save also to a file
print_design('generic_server.txt')

# Draw and save front view of rows
draw_rows('generic_server.svg')
