#!/usr/bin/env python3

#    "cgiwebserver.py" - A script to launch a Python-based web server
#
#    Written in 2012 by Konstantin S. Solnushkin (http://clusterdesign.org)
#
#    To the extent possible under law, the author has dedicated all copyright and related and
#    neighboring rights to this software to the public domain worldwide. This software is distributed
#    without any warranty. 
#
#    For the complete text of the "CC0 Public Domain Dedication",
#    see <http://creativecommons.org/publicdomain/zero/1.0/>. 

import http.server
import os
import sys
import platform
import webbrowser

# Get current Python version
current_version = platform.python_version_tuple()
print("Using Python version " + current_version[0] + "." + current_version[1] + "." + current_version[2])

# Check version.
# Must be at least this one:
m = ('3', '4', '2')
if platform.python_version_tuple() < m:
    print("Need python version to be at least " + m[0] + "." + m[1] + "." + m[2])
    exit(1)

# Get our own path
my_base_path = os.path.dirname(sys.argv[0])
# Change to that directory if we are not already there
if len(my_base_path) != 0:
    os.chdir(my_base_path)

# Copy the appropriate files
if os.name == 'nt':
    os.system("copy index-win.html index.html")
else:
    # Assume we are under GNU/Linux
    os.system("cp index-linux.html index.html")

# Start the web server
handler = http.server.CGIHTTPRequestHandler
server_address = ('', 8000)
httpd = http.server.HTTPServer(server_address, handler)

# All OK, open the web browser
# (this page will subsequently perform a redirection)
webbrowser.open("start.html")

# Start serving documents
try:
	httpd.serve_forever()
except KeyboardInterrupt:
	httpd.server_close()
