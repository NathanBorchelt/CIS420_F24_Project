#!/usr/bin/python3

#    "locator.py" - Web service locator script
#
#    Written in 2012 by Konstantin S. Solnushkin (http://clusterdesign.org)
#
#    To the extent possible under law, the author has dedicated all copyright and related and
#    neighboring rights to this software to the public domain worldwide. This software is distributed
#    without any warranty. 
#
#    For the complete text of the "CC0 Public Domain Dedication",
#    see <http://creativecommons.org/publicdomain/zero/1.0/>. 

import os

# If we are on Windows, need to add an extension to executable files
if os.name == 'nt':
  file_extension = '.exe'
else:
  file_extension = ''

# Specify a network equipment vendor to use
vendor_name = 'mellanox'
vendor = 'network_vendor=' + vendor_name

# Define the complete URL
url = 'http://localhost:8000/cgi-bin/network/network' + file_extension + '?task=design&' + vendor
  
# Define network types
alist = ['Torus=' + url + '&network_topology=torus',
'Fat-Tree, non-blocking=' + url + '&network_topology=fat-tree',
'Fat-Tree, 2:1 blocking=' + url + '&network_topology=fat-tree' + '&max_network_blocking_factor=2',
'Fat-Tree, 3:1 blocking=' + url + '&network_topology=fat-tree' + '&max_network_blocking_factor=3',
'Fat-Tree, 4:1 blocking=' + url + '&network_topology=fat-tree' + '&max_network_blocking_factor=4']

# Print content type and an empty line

print("""Content-type: text/plain
""")

for x in alist:
  print(x)
