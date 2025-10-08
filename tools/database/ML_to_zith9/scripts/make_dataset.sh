#!/bin/bash

#*******************************************************************************************
# make_dataset.sh
#
# Script to create ML.xxx Master List datasets in zith9 with proper archive ident
#   Updated to add project lat/lons, begin/end dates, note added to archive section for
#   those with no links ("not received"), hidden for those projects 3 years or more past
#   end of funding with note in summary.
#
#  	add_note="This dataset links to supplemental information for the project."
#  	no_link_txt='This dataset has not yet been received.'
#  	hidden_txt='Since the project ended over 3 years ago, this dataset has been hidden.'
#  	ml_hidden_txt="This link in the Master List was hidden."
#
# Will need to set two mysql login-paths(ml_read and zith-move) with mysql-config-editor
#   (https://dev.mysql.com/doc/refman/5.6/en/mysql-config-editor.html)
#   versions of MySQL before 5.6 can use '--login-path=' instead
#   (https://dev.mysql.com/doc/refman/8.0/en/option-file-options.html)
#
# ./make_dataset.sh ml_read [ml db name] zith_move [zith db name]
#
# The flow is:
#   while read dataset_id
#   make_dataset
#    calculate_archive_ident
#      make next guess
#    create new dataset
#    link new dataset to projects
#    create info xlink
#    create download xlink
#
#  IMPORTANT NOTE!
#   Do not add anything to, or change the formatting of the logs/new_archive_idents.log file.
#   It is used as an input file to the update_MLxxxx.pl script.
#
# --Hee Su Chang
#   Dec 2018
# --updated Jan 2019, ds
#      "    Apr 2019, hsc
#      "    Jun 2019, ds 
#			changed quality="final" if prelim=0 so links > 3 years old get made
#*******************************************************************************************
CONTACT=149   # for Don

MLLOGIN=$1
MLDB=$2

ZITHLOGIN=$3
ZITHDB=$4

#************************
# make next guess
#************************
make_next_guess() {
  prefix=$1

  if [[ $prefix != *"."* ]];then
    prefix+="."
  fi

  # exclude field data catalogs
  notpfx="${prefix}fc.%"

  if [[ $prefix != *"_" ]] && [[ $prefix != *"%" ]];then
    prefix+="%"
  fi

  # make guess on new archive ident (part after '.')
  next=$(mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "SELECT MAX(substring_index(archive_ident,'.',-1)+0)+1 AS next FROM dataset WHERE archive_ident LIKE '$prefix' AND archive_ident NOT LIKE '$notpfx';" | sed -n 2p)

  # echo "prefix and next =" $prefix", " $next >> logs/idents.log

  if [ -n "$next" ];then
    if ! [[ $next =~ ^[0-9]+$ ]]
    then
      next=1
    fi
  else
    next=1
  fi

  if [ "$next" = "NULL" ];then
    next=1
  fi

  # match project_id from prefix
  if [[ $prefix =~ ^.*[.] ]];then
    prefix=${BASH_REMATCH[0]}
  fi

  # reformat guess into 3 digits left padded by 0
  # if 102.256 and 102.1660 exists, the next archive_ident should be 102.257 not 102.1661
  guess=$prefix$(printf "%03d" $next | cut -c 1-3)
  echo "next guess = "$guess >> logs/idents.log

  # Check if guess is being used
  exists=$(mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "SELECT COUNT(*) FROM (SELECT 1 FROM dataset WHERE archive_ident='$guess' LIMIT 1) AS collision;" | sed -n 2p)

  if [[ $exists -eq 1 ]];then
    next_length=$((${#next}-1))
    for ((i=1; i<=$next_length; i++))
    do
      prefix+="_"
    done
    guess=$(make_next_guess $prefix)
  fi

  echo $guess
}
#************************
# end make next guess
#************************

#*************************************************************************************
# calculate archive ident
# calculates new archive_ident for ML.xx dataset according to project of ML.xx dataset
#*************************************************************************************
calculate_archive_ident() {
  #search for project name in historical prefix table
  prefix=$(mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "SELECT dataset_id_prefix AS prefix FROM dataset_prefix_project WHERE project_name='$1';" | sed -n 2p)
  if [ -z "$prefix" ];then #newer projects use primary key
    prefix=$(mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "SELECT id from project WHERE name='$1';" | sed 1d)
  fi

  next_ident=$(make_next_guess $prefix)
  echo "next ident: "$next_ident >> logs/idents.log
  echo $next_ident
}
#****************************
# end calculate archive ident
#****************************

#*********************************************
# create new zith9 dataset from Ml.xx dataset
#*********************************************
make_dataset() {
  ml_id=$1
  echo >> logs/idents.log
  echo "in make_dataset, master list dataset_id = "$ml_id >> logs/idents.log
  ml_dataset=$(mysql --login-path=$MLLOGIN -D $MLDB -e "SELECT name, url, doc_url, date_updated, preliminary_flag, dp.project_id, dp.date_posted, dp.in_progress_flag, dp.hide_flag FROM dataset JOIN dataset_project AS dp ON dataset.dataset_id=dp.dataset_id where dataset.dataset_id='$1';" | sed -n 2p)
  name=$( echo "$ml_dataset" | awk -F"\t" '{print $1}')
  url=$( echo "$ml_dataset" | awk -F"\t" '{print $2}')
  doc_url=$( echo "$ml_dataset" | awk -F"\t" '{print $3}')
  date_updated=$( echo "$ml_dataset" | awk -F"\t" '{print $4}')
  prelim=$( echo "$ml_dataset" | awk -F"\t" '{print $5}')
  project_name=$( echo "$ml_dataset" | awk -F"\t" '{print $6}')
  date_posted=$( echo "$ml_dataset" | awk -F"\t" '{print $7}')
  in_progress=$( echo "$ml_dataset" | awk -F"\t" '{print $8}')
  hide_flag=$( echo "$ml_dataset" | awk -F"\t" '{print $9}')
  add_note="This dataset links to supplemental information for the project."
  no_link_txt='This dataset has not yet been received.'
  hidden_txt='Since the project ended over 3 years ago, this dataset has been hidden.'
  ml_hidden_txt="This link in the Master List was hidden."

  if [[ -z "$project_name" ]];then
    echo "skip because no project for ML dataset: "$1 >> logs/make_dataset-errors.log
    return 1
  fi

#**********************************
# create appropriate archive ident
#**********************************
  new_archive_ident=$(calculate_archive_ident $project_name)
  echo "project is "$project_name", for dataset: "$ml_id >> logs/idents.log

#************************************************
# gather project metadata for dataset to inherit
#************************************************
  proj_meta=$(mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "SELECT begin_date, end_date, minimum_latitude, maximum_latitude, minimum_longitude, maximum_longitude, internal_contact_id from project where name='$project_name';" | sed -n 2p)
  proj_begin=$( echo "$proj_meta" | awk -F"\t" '{print $1}')
  proj_end=$( echo "$proj_meta" | awk -F"\t" '{print $2}')
  proj_minlat=$( echo "$proj_meta" | awk -F"\t" '{print $3}')
  proj_maxlat=$( echo "$proj_meta" | awk -F"\t" '{print $4}')
  proj_minlon=$( echo "$proj_meta" | awk -F"\t" '{print $5}')
  proj_maxlon=$( echo "$proj_meta" | awk -F"\t" '{print $6}')
  proj_internal_contact=$( echo "$proj_meta" | awk -F"\t" '{print $7}')

 # define quality                              enum('preliminary','final')
  if [[ "$prelim" == "1" ]];then
    prelim="preliminary"
  else
    prelim="final"
  fi

 # define progress                             enum('completed','historicalArchive','obsolete','onGoing','planned','required','underDevelopment')
  if [[ "$in_progress" == "1" ]];then
    in_progress="onGoing"
  else
    in_progress="planned"
  fi

  if [[ "$url" == "$doc_url" ]];then
    doc_url=""
  fi

  if [[ "$url" == "" ]] && [[ "$doc_url" == "" ]];then
    add_note=$no_link_txt
    in_progress="planned"
    if [[ "$proj_end" < "2016-01-01 01:01:01" ]];then
      vis_flag=0
      hide_flag=1
      add_note=$hidden_txt
  #   echo "dataset "$new_archive_ident" is from project older than 3 years, "$proj_end", so is hidden" >> logs/idents.log
    fi
  else
    prelim="final"
    in_progress="completed"
  fi

  # figure out visible flag, 0=hide, 1=visible
  if [[ "$hide_flag" == "1" ]];then
    vis_flag=0
    add_note+="$ml_hidden"
  #  add_note="$ml_hidden_txt $add_note"
  else
    vis_flag=1
  fi

  # echo "dataset "$new_archive_ident" has prelim="$prelim", progress="$in_progress >> logs/idents.log

  printf -v var "%q" "$name"

  #****************************
  # create new dataset in zith
  #****************************
  zith_did=$(mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "INSERT INTO dataset (archive_ident, title, archive_note, begin_date, end_date, minimum_latitude, maximum_latitude, minimum_longitude, maximum_longitude, internal_contact_id, visible, quality, progress, row_revise_contact_id)
    VALUES ('$new_archive_ident', '$var', '$add_note', '$proj_begin', '$proj_end', '$proj_minlat', '$proj_maxlat', '$proj_minlon', '$proj_maxlon', $proj_internal_contact, $vis_flag, '$prelim', '$in_progress', $2); SELECT LAST_INSERT_ID();" | sed -n 2p)

  if [[ $zith_did -eq 0 || "$zith_did" == "ERROR"* ]];then
    echo "Failed to create ML dataset: "$ml_id" in zith9 with archive_ident: "$new_archive_ident >> logs/make_dataset-errors.log
    echo "    zith_did = "$zith_did >> logs/make_dataset-errors.log
    return
  fi

  echo "created new dataset in zith with archive_ident = "$new_archive_ident  >> logs/idents.log

  #****************************
  # link new dataset to projects
  #****************************
  while read -r project
  do
    zith_pid=$(mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "SELECT id FROM project WHERE name='$project';" | sed -n 2p)
    mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "INSERT INTO dataset_project (dataset_id, project_id) VALUES ($zith_did, $zith_pid);"
  done< <(mysql --login-path=$MLLOGIN -D $MLDB -e "SELECT project_id FROM dataset_project WHERE dataset_id='$1';" | sed 1d)

  #************************
  # use date updated
  #************************
  if [[ -n "${date_updated}" && "$date_updated" != "NULL"* ]];then
    mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "UPDATE dataset SET row_revise_time='$date_updated' WHERE id=$zith_did;"
  fi

  #***********************
  # create new info xlink and link to new dataset if exists
  #************************
  url_result=""
  doc_result=""

  if [[ -n "${doc_url// }" &&  "$doc_url" != "NULL" ]];then
    mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "INSERT INTO xlink (href, row_revise_contact_id) VALUES ('$doc_url', $2); INSERT INTO dataset_xlink (dataset_id, xlink_id) VALUES ($zith_did, LAST_INSERT_ID());" 2>tmp
    doc_result=$(cat tmp)
  fi

  #***********************
  # create new download xlink and link to new dataset if exists
  #***********************
  if [[ -n "${url// }" &&  "$url" != "NULL" ]];then
    mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "INSERT INTO xlink (href, row_revise_contact_id) VALUES ('$url', $2); INSERT INTO dataset_xlink (dataset_id, xlink_id) VALUES ($zith_did, LAST_INSERT_ID());" 2>tmp
    url_result=$(cat tmp)
  fi

  if [[ $url_result == "ERROR"* ]];then
    if [[ $url_result == "ERROR 1062"* ]];then
      existing_xlink=$(mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "SELECT id FROM xlink where href='$url';" | sed -n 2p)
      if [[ -n "$existing_xlink" && "$existing_xlink" != "NULL" ]];then
        mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "INSERT INTO dataset_xlink (dataset_id, xlink_id) VALUES ($zith_did, $existing_xlink);"
      fi
    else
      echo "$1 url error "$url_result >> logs/make_dataset-errors.log
    fi
  fi

  if [[ $doc_result == "ERROR"* ]];then
    if [[ $doc_result == "ERROR 1062"* ]];then
      existing_xlink=$(mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "SELECT id FROM xlink where href='$doc_url';" | sed -n 2p)
      if [[ -n "$existing_xlink" && "$existing_xlink" != "NULL" ]];then
        mysql --login-path=$ZITHLOGIN -D $ZITHDB -e "INSERT INTO dataset_xlink (dataset_id, xlink_id) VALUES ($zith_did, $existing_xlink);"
      fi
    else
      echo "$1 url error "$doc_result >> logs/make_dataset-errors.log
    fi
  fi

  echo $1";"$new_archive_ident";"$zith_did >> logs/new_archive_idents.log
  echo $zith_did

}
#****************************
# end make dataset
#****************************

#**********
# main
#**********
while read -r output;
do
  make_dataset $output $CONTACT
done< <(mysql --login-path=$MLLOGIN -D $MLDB -e "SELECT dataset_id FROM dataset WHERE dataset_id LIKE 'ML%';" | sed 1d)

