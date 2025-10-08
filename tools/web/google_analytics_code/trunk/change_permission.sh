#****************************************************************************************************
# change_permission.sh
# 
# This script is used to recursively change the permission of all the files and directories under input directory
# The permmsion of files will be changed to "-rw-rw-r--"
# The permission of directories will be changed to "drwxrwsr-x"
# Pre-request: The runner should have permmsion to change the fileownerships
# 
# Syntax: ./change_fileowner.sh
# 
#****************************************************************************************************
#!/bin/bash

echo -n "Enter Directory Name: "
read DIRECTORY
#MYDIR="/net/www/docs/$DIRECTORY"
MYDIR=$DIRECTORY


function recurse() {
	for i in "$1"/*; do
		if [ -d "$i" ]; then
			chmod 2775 "$i"
			if [ "$?" -eq "0" ];then
				echo "[$i permission has been change to drwxrwsr-x]"
			else
				echo "[$i exit with failure]"
			fi
			recurse "$i"
		elif [ -f "$i" ]; then
			chmod 664 "$i"
			if [ "$?" -eq "0" ];then
				echo "[$i permission has been change to -rw-rw-r--]"
			else
				echo "[$i exit with failure]"
			fi
		fi
	done
}

recurse "$MYDIR"
