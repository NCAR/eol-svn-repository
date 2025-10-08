#!/bin/bash

#*******************************************************************************************
# link_sites.sh
#
# Script to integrate Master List sites in classifications into zith9 sites table
#
# Iterates through ML sites listed in class-site.csv and creates each site in zith9.
# Recreates all dataset<->site classification links in dmg_merged_ml to dataset<->site links in zith9.
# Finds all parent<->child relationships in dmg_merged_ml and recreate in zith9.
#
# Will need to set two mysql login-paths(ml_read and zith-move) with mysql-config-editor
#   (https://dev.mysql.com/doc/refman/5.6/en/mysql-config-editor.html)
#   versions of MySQL before 5.6 can use '--defaults-group-suffix=' instead
#   (https://dev.mysql.com/doc/refman/8.0/en/option-file-options.html)
#
# ./link_sites.sh ml_read [ml db name] zith_move [zith db name]
#
# --Hee Su Chang
#   Dec 2018
#*******************************************************************************************

CONTACTID='149'   # for Don

MLLOGIN=$1
MLDB=$2

ZITHLOGIN=$3
ZITHDB=$4

#*******************************************************************************************
# class-site.csv
#   a text file with two columns separated by "|"
# First column: ML Classification type 3 site name
# Second column: Corresponding zith9 site name to be used
#*******************************************************************************************

SITEIN=../CSV_files/class-site.csv

echo "Move sites from ML to zith9"

# recreate ML sites in zith9
while IFS='|' read -r ml_site zith_site
do
  echo $zith_site >> logs/new_sites.log

  mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "INSERT INTO site (name, row_revise_contact_id) VALUES ('$zith_site', $CONTACTID);"

  echo "Link datasets to $zith_site"

  while read -r output;
  do
    # the archive_ident stored in dmg_merged_ml
    archive_ident=$( echo "$output" | awk -F"\t" '{print $1}')

    if [[ $archive_ident == "ML."* ]];then
      echo "skipping ml dataset"
    else
      mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "INSERT INTO dataset_site (dataset_id, site_id) VALUES ((SELECT id FROM dataset WHERE archive_ident='$archive_ident'), (SELECT id FROM site WHERE name='$zith_site'));" 2>tmp

      result=$(cat tmp)

      if [[ $result == "ERROR"* ]];then
        if [[ $result != "ERROR 1062"* ]];then #ignore 1062. This means that the link is already in zinc
          echo "Error connecting $archive_ident to zinc $zith_site: "$result >> logs/errors.log
        fi
      else
        echo "$archive_ident : $result" >> logs/dataset_site.log
      fi

    fi

  # iterate through datasets linked to ML classification category
  done< <(mysql --login-path=$MLLOGIN -D $MLDB -e "SELECT dataset_id FROM dataset_classification AS dc JOIN classification AS c ON dc.class_id=c.class_id WHERE c.name='$zith_site' AND dataset_id NOT LIKE '%.fc%';" | sed 1d)

done< $SITEIN

# find parent-child relationships in ML and recreate in zith9
while read -r output;
do
  site_name=$( echo "$output" | awk -F"\t" '{print $1}')
  parent_id=$( echo "$output" | awk -F"\t" '{print $2}')

  parent_name=$(mysql --login-path=$MLLOGIN -D $MLDB -e "SELECT name FROM classification_parent as cp JOIN classification AS c ON cp.parent_class_id=c.class_id WHERE cp.parent_class_id='$parent_id';" | sed -n 2p)
  new_parent_id=$(mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "SELECT id FROM site WHERE name='$parent_name';" | sed -n 2p)

  mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "UPDATE site SET parent_site_id=$new_parent_id WHERE name='$site_name';"

done< <(mysql --login-path=$MLLOGIN -D $MLDB -e "SELECT name, cp.parent_class_id FROM classification as c JOIN classification_parent AS cp ON c.class_id=cp.class_id where type_id=3;" | sed 1d)
