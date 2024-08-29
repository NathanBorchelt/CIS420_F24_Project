#!/usr/bin/env python3.3

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

# Define floor planning models

alist = ['Standard racks=http://localhost:8000/cgi-bin/floorplan/floor_size.py?task=design&d=1.2&w=0.6',
'IBM iDataPlex racks=http://localhost:8000/cgi-bin/floorplan/floor_size.py?task=design&d=0.6&w=1.2']

# Print content type and an empty line

print("""Content-type: text/plain
""")

for x in alist:
  print(x)
