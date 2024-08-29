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

# Define UPSes

alist = ['Liebert APM (up to 45 kW)=http://localhost:8000/cgi-bin/ups/ups_sizer.py?task=design']

# Print content type and an empty line

print("""Content-type: text/plain
""")

for x in alist:
  print(x)
