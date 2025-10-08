#****************************************************************************************************
# change_fileowner.sh
# 
# This script is used to recursively change the ownership of all the files under input directory
# Pre-request: The runner should have permmsion to change the fileownerships
# 
# Syntax: ./change_fileowner.sh
# 
#****************************************************************************************************
#!/bin/bash

echo -n "Enter Directory Name: "
read DIRECTORY
echo -n "Enter Current Owner Name: "
read OWNER
echo -n "Enter New Owner Name: "
read NEW_OWNER
#MYDIR="/net/www/docs/$DIRECTORY"
MYDIR=$DIRECTORY


#DIRS=`ls -l $MYDIR | awk '{print $9}'`

# "ls -l $MYDIR"      = get a directory listing
# "| egrep '^d'"           = pipe to egrep and select only the directories
# "awk '{print $8}'" = pipe the result from egrep to awk and print only the 8th field

#cd $PWD
# and now loop through the directories:
function recurse() {
	for i in "$1"/*; do
		if [ -d "$i" ]; then
			recurse "$i"
		elif [ -f "$i" ]; then
			own=`ls -l $i | awk '{print $3}'`
			if [ $own == $OWNER ];then
				chown $NEW_OWNER "$i"
				if [ "$?" -eq "0" ];then
					echo "[$i owner has been changed to $NEW_OWNER]"
				else
					echo "[$i exit with failure]"
				fi
			fi
		fi
	done
}

recurse "$MYDIR"
