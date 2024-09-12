#!/usr/bin/env bash

pyVersion=$(python -V)
pyVerList=($pyVersion)
pyNum=${pyVerList[1]}

#debug input
#read pyNum


oIFS="$IFS"
IFS="\."

pyVerSplit=($pyNum)

IFS="$oIFS"
unset oIFS

pythonAutoPull=1

if ! command -v python &> /dev/null
then
    echo -e "python could not be found\nThe base program, ClusterDesign, has a minimum requirement of python 3.4.2\nWhile I recomend a newer version, you can get the bare minimum version here\nhttps://www.python.org/downloads/release/python-342/"
    exit -1
fi

if [[ "${pyVerSplit[0]}" -eq "3" ]]; then
	if [[ "${pyVerSplit[1]}" -lt "4" ]]; then
		echo -e "The base program, ClusterDesign, has a minimum requirement of python 3.4.2\nWhile I recomend a newer version, you can get the bare minimum version here\nhttps://www.python.org/downloads/release/python-342/"
		exit -1
	elif [[ "${pyVerSplit[1]}" -eq "4" ]]; then
		if [[ "${pyVerSplit[2]}" -lt "2" ]]; then
			echo -e "The base program, ClusterDesign, has a minimum requirement of python 3.4.2\nWhile I recomend a newer version, you can get the bare minimum version here\nhttps://www.python.org/downloads/release/python-342/"
			exit -1
		fi
	elif [[ "${pyVerSplit[1]}" -lt "8" ]]; then
		echo -e "Automated Collection/Updating of processors requires a minimum version of Python 3.8.\nPlease update to a newer version to enable these features.\nYou can get the latest versions of python from\nhttps://www.python.org/downloads/\nWhile I would recoment a newer version, the bare minimum version for this feature is 3.8.0, downloadable here\nhttps://www.python.org/downloads/release/python-380/\nIf you are running a version less than 3.8.0, the minimum requirement is 3.4.2"
		pythonAutoPull="0"
	fi
elif [[ "${pyVerSplit[0]}" -lt "3" ]]; then
	echo -e "The base program, ClusterDesign, has a minimum requirement of python 3.4.2\nWhile I recomend a newer version, you can get the bare minimum version here\nhttps://www.python.org/downloads/release/python-342/"
	exit -1
fi

if ! command -v pip &> /dev/null
then
    echo "pip could not be found, this is required for ensuring all packages are installed"
    exit -1
fi

if [[ $pythonAutoPull -ne "0" ]]; then
	pip install scrapy
	cd cpuScraping_0_1

	echo -e "\n\n\n\n\nThere is currently an issue with Scrapy and it using MD5 for one of the algoriths.\nI did a quick and dirty patch to the souce code, but I do not remember what it was.\nWhoever runs this and runs into this issue please report it as an issue with a copy of the error message from the terminal."

	read -p "Thank you for understanding (Press enter to continue)" temp

	unset temp

	cat ../cpuSpiders.txt | while read line || [[ -n $line ]];
	do
		scrapy crawl "$line"
		python "cpuDataCleaning_$line.py"
	done

	python "rack_ex4000_chassis.py"

fi

cd ../

cp "./slingshot.xml" ./clusterdesign/
cp "./slingshot.xml" ./saddle/db/

cp "./hpe_pc4_memory.xml" ./clusterdesign/
cp "./hpe_pc4_memory.xml" ./saddle/db/

cp "./hpe_pc5_memory.xml" ./clusterdesign/
cp "./hpe_pc5_memory.xml" ./saddle/db/

echo -e "\n\nDONE\n\n"
