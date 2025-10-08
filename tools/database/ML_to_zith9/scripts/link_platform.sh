#!/bin/bash

#*******************************************************************************************
# link_platform.sh
#
# Script to integrate Master List platforms in classifications into zith9 platforms table
#
# Iterates through ML platforms listed in class-platform.csv and finds all ML datasets linked to them.
# Recreates all dataset<->platform classification links in dmg_merged_ml to dataset<->platform in zith9.
# Call script to create parent-child relationships in zith9.
#
# Will need to set two mysql login-paths(ml_read and zith-move) with mysql-config-editor
#   (https://dev.mysql.com/doc/refman/5.6/en/mysql-config-editor.html)
#   versions of MySQL before 5.6 can use '--defaults-group-suffix=' instead
#   (https://dev.mysql.com/doc/refman/8.0/en/option-file-options.html)
#
# ./link_platform.sh ml_read [ml db name] zith_move [zith db name]
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
# First column: ML Classification type 4 platform name
# Second column: Corresponding zith9 platform name
#*******************************************************************************************

PLATIN=../CSV_files/class-platform.csv

{
# connect ML classifications to zith9 platform
while IFS='|' read -r ml_plat zith_plat
do
  plat=$(mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "SELECT name FROM platform WHERE name='$zith_plat'" | sed -n 2p)
  echo "first match, plat = "$plat >> logs/checks.log

  if [ -z "$plat" ];then
    echo "plat is null, so will insert new platform "$zith_plat" into platform table" >> logs/checks.log
    mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "INSERT INTO platform (name, row_revise_contact_id) VALUE ('$zith_plat', $CONTACTID);"
  fi

  echo "*************** Working on ML Classification $ml_plat --> $zith_plat ***************"
  while read -r output;
  do
    # the archive_ident stored in dmg_merged_ml
    archive_ident=$( echo "$output" | awk -F"\t" '{print $1}')

    if [[ $archive_ident == "ML."* ]];then
      echo "skipping ML.xx dataset"
      echo "skipping $archive_ident" >> logs/plat_errors.log
    else
      # connect existing dataset to corresponding zith9 category
      mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "INSERT INTO dataset_platform (dataset_id, platform_id) VALUES ((SELECT id FROM dataset WHERE archive_ident='$archive_ident'), (SELECT id FROM platform WHERE name='$zith_plat'));" 2>tmp
      echo $archive_ident
      result=$(cat tmp)
      if [[ $result == "ERROR"* ]];then
        if [[ $result != "ERROR 1062"* ]];then #ignore 1062. This means that the link is already in zinc
          echo "Error connecting $archive_ident to zinc $zith_plat from ML classification $ml_plat: "$result >> logs/errors.log
        fi
      else
        echo "$archive_ident : $result" >> logs/dataset_platform.log
      fi

    fi

  # iterate through datasets linked to ML classification category
  done< <(mysql --login-path=$MLLOGIN -D $MLDB -e "SELECT dataset_id FROM dataset_classification AS dc JOIN classification AS c ON dc.class_id=c.class_id WHERE c.name='$ml_plat' AND dataset_id NOT LIKE '%.fc%';" | sed 1d)

# iterate through ML classification category and corresponding zith9 category
done < $PLATIN

# recreate parent-child relationships
bash parent-child-plat.sh $1 $2 $3 $4

} &> logs/platform.log
