#!/bin/bash

#*******************************************************************************************
# parent-child-plat.sh
#
# Script to find parent-child platform relationships in ML and recreate in zith9
#
#
# Iterates through ML classifications parent-child relationships
#   Of these relationships, find ones that are categories defined in class-platform.csv
#   Recreate these relationships with their associated zith9 counterparts.
# IMPORTANT: This script is called by the link_platform.sh script as well
#
# Will need to set two mysql login-paths(ml_read and zith-move) with mysql-config-editor
#   (https://dev.mysql.com/doc/refman/5.6/en/mysql-config-editor.html)
#   versions of MySQL before 5.6 can use '--defaults-group-suffix=' instead
#   (https://dev.mysql.com/doc/refman/8.0/en/option-file-options.html)
#
# ./parent-child-plat.sh ml_read <ml db name> zith_move <zith db name>
#
# --Hee Su Chang
#   Nov 2018
#*******************************************************************************************

CONTACTID='149'   # for Don

MLLOGIN=$1
MLDB=$2

ZITHLOGIN=$3
ZITHDB=$4

#*******************************************************************************************
# class-platform.csv
#   a text file with two columns separated by "|"
# First column: ML Classification type 1 platform name
# Second column: Corresponding zith9 platform name to be used
#*******************************************************************************************

PLATIN=../CSV_files/class-platform.csv

# find parent-child relationships in ML and recreate in zith9
while read -r output;
do
  ml_parent_name=$( echo "$output" | awk -F"\t" '{print $1}')
  ml_child_name=$( echo "$output" | awk -F"\t" '{print $2}')

  parent=$(grep -hr "^$ml_parent_name|" $PLATIN)

  if [ -z "$parent" ];then
    continue
  fi

  child=$(grep -hr "^$ml_child_name|" $PLATIN)

  if [ -z "$child" ];then
    continue
  fi

  IFS='|' read -a parentArray <<< "${parent}"
  IFS='|' read -a childArray <<< "${child}"

  echo "Child:${childArray[1]}(ML:${childArray[0]}) -> Parent:${parentArray[1]}(ML:${parentArray[0]})" >> logs/parent_child_platform.log

  new_parent_id=$(mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "SELECT id FROM platform WHERE name='${parentArray[1]}';" | sed -n 2p)

  mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "UPDATE platform SET parent_platform_id=$new_parent_id, row_revise_contact_id=$CONTACTID WHERE name='${childArray[1]}' AND NOT id=$new_parent_id;"

done< <(mysql --login-path=$MLLOGIN -D $MLDB -e "SELECT (SELECT name FROM classification WHERE classification.class_id=cp.parent_class_id) AS parent, c.name FROM classification AS c LEFT JOIN classification_parent AS cp ON c.class_id=cp.class_id WHERE type_id=4 ORDER BY parent;" | sed 1d)
