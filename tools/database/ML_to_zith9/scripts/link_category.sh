#!/bin/bash

#*******************************************************************************************
# link_category.sh
#
# Script to integrate Master List type 1 classifications into zith9 category table
#
# Iterates through ML categories listed in class-category.csv and finds all ML datasets linked to them.
# Recreates all dataset<->category classification links in dmg_merged_ml to dataset<->category in zith9.
# Call script to create parent-child relationships in zith9.
#
# Will need to set two mysql login-paths(ml_read and zith-move) with mysql-config-editor
#   (https://dev.mysql.com/doc/refman/5.6/en/mysql-config-editor.html)
#   versions of MySQL before 5.6 can use '--defaults-group-suffix=' instead
#   (https://dev.mysql.com/doc/refman/8.0/en/option-file-options.html)
#
# ./link_category.sh ml_read <ml db name> zith_move <zith db name>
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
# First column: ML Classification type 1 category name (ml_cat)
# Second column: Corresponding zith9 category name (zith_cat)
#*******************************************************************************************

CATIN=../CSV_files/class-category.csv

{
# connect ML classifications to zith9 categories
while IFS='|' read -r ml_cat zith_cat
do
  cat=$(mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "SELECT name FROM category WHERE name='$zith_cat'" | sed -n 2p)
  echo "first match, cat = "$cat >> logs/checks.log

  if [ -z "$cat" ];then
    echo "cat is null, so will insert new category "$zith_cat" into category table" >> logs/checks.log
    mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "INSERT INTO category (name, row_revise_contact_id) VALUE ('$zith_cat', $CONTACTID);"
  fi

  echo "*************** Working on ML Classification $ml_cat --> $zith_cat ***************"
  while read -r output;
  do
    # the dataset_id from dmg_merged_ml becomes the archive_ident in zith9
    archive_ident=$( echo "$output" | awk -F"\t" '{print $1}')

    if [[ $archive_ident == "ML."* ]];then
      echo "skipping ML.xx dataset"
      echo "skipping $archive_ident" >> logs/cat_errors.log
    else
      # connect existing dataset to corresponding zith9 category
      mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "INSERT INTO dataset_category (dataset_id, category_id) VALUES ((SELECT id FROM dataset WHERE archive_ident='$archive_ident'), (SELECT id FROM category WHERE name='$zith_cat'));" 2>tmp
      echo $archive_ident

      result=$(cat tmp)

      if [[ $result == "ERROR"* ]];then
        if [[ $result != "ERROR 1062"* ]];then     #ignore 1062. This means that the link is already in zinc
          echo "Error connecting $archive_ident to zinc $zith_cat from ML classification $ml_cat: "$result >> logs/errors.log
        fi
      else
        echo "$archive_ident : $result" >> logs/dataset_category.log
      fi
    fi

  # iterate through datasets linked to ML classification category
  done< <(mysql --login-path=$MLLOGIN -D $MLDB -e "SELECT dataset_id FROM dataset_classification AS dc JOIN classification AS c ON dc.class_id=c.class_id WHERE c.name='$ml_cat' AND dataset_id NOT LIKE '%.fc%';" | sed 1d)

# iterate through ML classification category and corresponding zith9 category
done < $CATIN

# recreate parent-child relationships
bash parent-child-cat.sh $1 $2 $3 $4

} &> category.log
