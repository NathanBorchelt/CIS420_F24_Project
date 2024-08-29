#!/bin/bash

curdir=`pwd`

# Run "make publish" in each of the following directories,
# exit immediately on any error encountered:
make -C clusterdesign publish && \
make -C webserverhome publish && \
make -C dbcli publish && \
make -C floorplan publish && \
make -C network publish && \
make -C performance publish && \
make -C ups publish || \
{ echo "Error detected: please inspect, correct, and rerun"; exit 1; }

# Set executable permissions on Python programs and some shell scripts:
sh ./set_perm.sh

# Copy essential files (should be there if "make.bat" was
# already run to compile Windows binaries, but just make sure
# to copy them if the user only wants GNU/Linux binaries)
cp Changelog.txt make.bat setpath.bat README.txt set_perm.sh unix.sh version.txt published/src

# Make an additional copy of the Changelog in case the directory
# with sources is later deleted from the archive (this will not be
# necessary in the future)
cp Changelog.txt published/

# Create a distribution archive
name="clusterdesign-"`cat version.txt`
mv published $name
zip -r9 $name".zip" $name
mv $name published
