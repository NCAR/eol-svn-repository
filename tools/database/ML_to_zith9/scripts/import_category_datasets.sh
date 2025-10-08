#!/bin/bash

#*******************************************************************************************
# import_category_datasets.sh
#
# Script that finds all datasets in a category and connects them to another catgory
# Find all datasets in the zith9 dataset_category table with specified category ID
# and create a new entry in the table with the dataset ID and a second specified category
# ID, if one doesn't already exist. Useful if merging datasets under one category to
# another, possibly before deleting the first set.
#
# Iterate through all datasets in a category (specified by id)
# Check if the dataset is linked to the newly desired category (specified by id) already
# If so, move on to next dataset_id to match. If not, create a new dataset-category relationship
#
# Will need to set two mysql login-paths(ml_read and zith-move) with mysql-config-editor
#   (https://dev.mysql.com/doc/refman/5.6/en/mysql-config-editor.html)
#   versions of MySQL before 5.6 can use '--login-path=' instead
#   (https://dev.mysql.com/doc/refman/8.0/en/option-file-options.html)
#
# ./import_category_datasets.sh zith_move <zith db name> <category id to copy from> <new category id to copy to>
#
# --Hee Su Chang
#   Feb 2019
#*******************************************************************************************

CONTACTID='149'   # for Don

ZITHLOGIN=$1
ZITHDB=$2

IN_CATEGORY=$3
NEW_CATEGORY=$4

echo "<-------------For all datasets in category $3, connect to category $4 as well------------->" >> logs/import_category_datasets.log

while read -r did
do

  link_exists=$(mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "SELECT COUNT(*) FROM (SELECT 1 FROM dataset_category WHERE dataset_id=$did AND category_id=$NEW_CATEGORY LIMIT 1) AS existing" | sed -n 2p)
# or "SELECT COUNT(*) FROM dataset_category WHERE dataset_id=did AND category_id=$NEW_CATEGORY"

  if [[ $link_exists -eq 0 ]];then
    mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "INSERT INTO dataset_category (dataset_id, category_id) VALUE ($did, $NEW_CATEGORY);"
    echo "--> Dataset $did linked to category $NEW_CATEGORY" >> logs/import_category_datasets.log

  else
    echo "Dataset $did already in category $NEW_CATEGORY" 
  fi

done< <(mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "SELECT dataset_id FROM dataset_category WHERE category_id=$IN_CATEGORY;" | sed 1d)
