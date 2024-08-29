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

# Define networks

alist = ['ANSYS Fluent 13 (truck_111m)=http://localhost:8000/cgi-bin/performance/fluentperf/fluentperf'+file_extension+'?task=design&benchmark=truck_111m']

# Print content type and an empty line

print("""Content-type: text/plain
""")

for x in alist:
  print(x)
