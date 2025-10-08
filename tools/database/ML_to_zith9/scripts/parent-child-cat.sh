#!/bin/bash

#*******************************************************************************************
# parent-child-cat.sh
#
# Script to find parent-child category relationships in ML and recreate in zith9
#
#
# Iterates through ML classifications parent-child relationships
#   Of these relationships, find ones that are categories defined in class-category.csv
#   Recreate these relationships with their associated zith9 counterparts.
# IMPORTANT: This script is called by the link_category.sh script as well
#
# Will need to set two mysql login-paths(ml_read and zith-move) with mysql-config-editor
#   (https://dev.mysql.com/doc/refman/5.6/en/mysql-config-editor.html)
#   versions of MySQL before 5.6 can use '--defaults-group-suffix=' instead
#   (https://dev.mysql.com/doc/refman/8.0/en/option-file-options.html)
#
# ./parent-child-cat.sh ml_read <ml db name> zith_move <zith db name>
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
# class-category.csv
#   a text file with two columns separated by "|"
# First column: ML Classification type 1 category name
# Second column: Corresponding zith9 category name to be used
#*******************************************************************************************

CATIN=../CSV_files/class-category.csv

# read line from list inputted by done statement at the bottom
while read -r output;
do
  ml_parent_name=$( echo "$output" | awk -F"\t" '{print $1}')
  ml_child_name=$( echo "$output" | awk -F"\t" '{print $2}')

  # Match the parent's ML name in class-category.csv
  # Return the entire line in class-category.csv that contains the match
  parent=$(grep -hr "^$ml_parent_name|" $CATIN)

  # If a match exists for the parent ML name in class-category, go on with the iteration
  if [ -z "$parent" ];then
    continue
  fi

  # Match the parent's ML name in class-category.csv
  # Return the entire line in class-category.csv that contains the match
  child=$(grep -hr "^$ml_child_name|" $CATIN)

  # If a match exists for the parent ML name in class-category, go on with the iteration
  if [ -z "$child" ];then
    continue
  fi

  # Split both parent and child into arrays with delimeter |
  # [0] will be the ML name, [1] will be the equivalent zith9 name
  IFS='|' read -a parentArray <<< "${parent}"
  IFS='|' read -a childArray <<< "${child}"

  echo "Child:${childArray[1]}(ML:${childArray[0]}) -> Parent:${parentArray[1]}(ML:${parentArray[0]})" >> logs/parent_child_category.log

  # Find the id of the parent in zith9 using the zith9 name
  new_parent_id=$(mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "SELECT id FROM category WHERE name='${parentArray[1]}';" | sed -n 2p)

  # Find zith9 category that matches the zith9 name of the child and set parent_category_id to $new_parent_id as long as $new_parent_id != the id of the category itself
  mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "UPDATE category SET parent_category_id=$new_parent_id, row_revise_contact_id=$CONTACTID WHERE name='${childArray[1]}' AND NOT id=$new_parent_id;"

# return list of parent, child relationships of all type 1 classifications in ML
done< <(mysql --login-path=$MLLOGIN -D $MLDB -e "SELECT (SELECT name FROM classification WHERE classification.class_id=cp.parent_class_id) AS parent, c.name FROM classification AS c LEFT JOIN classification_parent AS cp ON c.class_id=cp.class_id WHERE type_id=1 ORDER BY parent;" | sed 1d)
