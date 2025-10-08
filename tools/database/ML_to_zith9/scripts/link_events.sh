#!/bin/bash

#*******************************************************************************************
# link_events.sh
#
# Script to integrate Master List events in classifications into zith9 events table
#
# Iterates through ML events listed in class-event.csv and creates each event in zith9.
# Recreates all datase<->event classification links in dmg_merged_ml to dataset<->event links in zith9.
#
# Will need to set two mysql login-paths(ml_read and zith-move) with mysql-config-editor
#   (https://dev.mysql.com/doc/refman/5.6/en/mysql-config-editor.html)
#   versions of MySQL before 5.6 can use '--defaults-group-suffix=' instead
#   (https://dev.mysql.com/doc/refman/8.0/en/option-file-options.html)
#
# ./link_events.sh ml_read [ml db name] zith_move [zith db name]
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
# class-event.csv
#   a text file with two columns separated by "|"
# First column: ML Classification type 2 event name
# Second column: Corresponding zith9 event name to be used
#*******************************************************************************************

EVENTIN=../CSV_files/class-event.csv

echo "Move events from ML to zith9"

# recreate ML events in zith9
while IFS='|' read -r ml_event zith_event
do
  echo $zith_event >> logs/new_events.log
  mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "INSERT INTO event (name, project_id, row_revise_contact_id) VALUES ('$zith_event', (SELECT id FROM project where name='$zith_event'), $CONTACTID);"
  echo "Link datasets to $zith_event"

  while read -r output;
  do
    # the archive_ident stored in dmg_merged_ml
    archive_ident=$( echo "$output" | awk -F"\t" '{print $1}')

    if [[ $archive_ident == "ML."* ]];then
      echo "skipping ml dataset"
    else
      mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "INSERT INTO dataset_event (dataset_id, event_id) VALUES ((SELECT id FROM dataset WHERE archive_ident='$archive_ident'), (SELECT id FROM event WHERE name='$zith_event'));" 2>tmp
      result=$(cat tmp)

      if [[ $result == "ERROR"* ]];then
        if [[ $result != "ERROR 1062"* ]];then #ignore 1062. This means that the link is already in zinc
          echo "Error connecting $archive_ident to zinc $zith_event: "$result >> logs/errors.log
        fi
      else
        echo "$archive_ident : $result" >> logs/dataset_event.log
      fi

    fi

  # iterate through datasets linked to ML classification category
  done< <(mysql --login-path=$MLLOGIN -D $MLDB -e "SELECT dataset_id FROM dataset_classification AS dc JOIN classification AS c ON dc.class_id=c.class_id WHERE c.name='$zith_event' AND dataset_id NOT LIKE '%.fc%';" | sed 1d)

done< $EVENTIN
