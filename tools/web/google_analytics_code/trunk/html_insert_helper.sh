#****************************************************************************************************
# html_insert_helper.sh
# 
# This script is used to find all the files or directory under input directory name, and run html_insert.py
# on each of them.
# It will also generate error_summary_log.csv under /net/work/HTML_INSERT, which only included failed updates
# in last run.
# All symlinks will be skipped
# 
# Syntax: ./html_insert_helper.sh
# 
#****************************************************************************************************
#!/bin/bash

echo -n "Enter Directory Name: "
read DIRECTORY
MYDIR="/net/www/docs/$DIRECTORY"
LOGDIR="/net/work/HTML_INSERT"
#MYDIR="$PWD/$DIRECTORY"
#LOGDIR="$PWD/$DIRECTORY/log"



for D in $MYDIR/*
do
DIR=${D/' '/'\ '}
if [ -L "$DIR" ];then
	echo "$DIR is a symlink, skipped."
else
	python html_insert.py $DIR
	if [ "$?" -eq "0" ];then
		echo "[$DIR exit successfully]"$'\n'$'\n'
	else
		echo "[$DIR exit with failure]"$'\n'$'\n'
	fi
fi
done

rm -rf $LOGDIR/error_summary_log.csv
LOGDIRS=`ls -l $LOGDIR | awk '{print $9}'`

for DIR in $LOGDIRS
do
if [ -f "$LOGDIR/$DIR" ];then	
	#cat $LOGDIR/$DIR
	while IFS="," read f1 f2
	do
		if [[ $f2 == *Failure* ]];then
			echo "$f1, $f2" 
		fi
	done < $LOGDIR/$DIR
fi
done > $LOGDIR/error_summary_log.csv
