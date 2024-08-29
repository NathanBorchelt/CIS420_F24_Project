#!/bin/sh

find ./published/bin/webserverhome/ -name '*.py' | xargs -L 1 -t -r chmod +x

chmod +x ./published/bin/webserverhome/startwebserver.sh
