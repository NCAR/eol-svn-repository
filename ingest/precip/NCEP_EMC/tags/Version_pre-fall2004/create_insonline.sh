#!/usr/bin/sh
# create_insonline.sh - creates the file insonline containing the 
#                       SQL commands necessary to add entries to the
#                       phys_dir_db:on_line_phys_dir (database:table)

# Author: Ronald A. Murdock

# turn debugging information on/off here
# debug=1 means that debugging info will appear
# debug=0 means that no debugging info will appear
#debug=1
debug=0

# turn on the writing of the SQL insert command into the insonline file here
# write_cmd=0 means that no writing will take place
# write_cmd=1 means that command will be written into insonline file
write_cmd=1


# initialize vars (default values)

# EDIT THE FOLLOWING LINE
# lfmt values are in phys_dir_db:logical_format:logical_fmt
# Or see the file /storm/codiac/codiac_db/logical_fmt.list
# 34 = Color Graphics Interchange Format (GIF)
# 36 = Tabular ASCII
# 10 = EBUFR
lfmt=36
if [ $debug -eq 1 ]
   then echo "logical format lfmt = $lfmt"
fi

# EDIT THE FOLLOWING LINE
# code values are in sdmc_order_db:media_code:media_code
# Or see the file /storm/codiac/codiac_db/media_code.list
# 10 = Magnetic Disk
# 12 = Magneto-Optical Disk (Jukebox)
code=12
if [ $debug -eq 1 ]
   then echo "media code = $code"
fi

# EDIT THE FOLLOWING LINE
# pfmt values are in data_dict_db:physical_format:phys_fmt
# Or see the file /storm/codiac/codiac_db/phys_fmt.list
# 1 = Binary
# 2 = ASCII
# 4 = Packed Binary (EBUFR)
pfmt=2
if [ $debug -eq 1 ]
   then echo "physical format = $pfmt"
fi

add="storm.joss.ucar.edu"
if [ $debug -eq 1 ]
   then 
      echo "hardware address is $add"
fi

# EDIT THE FOLLOWING LINE
# the dir_path is the directory path to the location of
# of the data files.
# (this is where the files will physically reside on CODIAC)
# dir_path=`pwd` (You can also use this if you understand why.)
#                (It requires the files already being in their 
#                 final place,
#                 and placing this script and file_insert.sh 
#                 in the same directory.)
dir_path="/archive/jb0/codiac/eop/katz_data/hrly_gage"
if [ $debug -eq 1 ]
   then 
      echo "directory path is $dir_path"
fi

hide=null
archive=today

# set to the correct storm_id for this dataset
# ***  EDIT THE FOLLOWING LINE  ***
id="21.004"
if [ $debug -eq 1 ]
   then 
      echo "storm_id is $id"
fi

# put in a very short title describing these files
# ***  EDIT THE FOLLOWING LINE  ***
desc="NCEP Hrly PCP"
if [ $debug -eq 1 ]
   then 
  echo "short title for these files will be \"$desc\""
fi


if [ $# = 0 ]
then

	# no file given,  exit with bad status
	exit 1
else
	mfile=$1
fi

# next 8 fields are obtained by reading the file by the range program
# and then extraction by the awk command

# echo "mfile is $mfile"
# infoline=`echo $mfile | awk '{ split( $0, arr, "_" ); print arr[2] }'`
# echo "infoline is $infoline"

if [ $debug -eq 1 ]
then
   echo "file name is \"$mfile\""
   echo " "
fi

# Change the following awk script to correctly parse out the
# date/time from the file name:
# ***  EDIT THE FOLLOWING LINES  ***
# substr starts counting bytes beginning with one, NOT zero
bdate=`echo $mfile | awk '{ print substr($0,16,8) }'`
bdate=$bdate
bhour="00"
bmin="00"
edate=`echo $mfile | awk '{ print substr($0,16,8) }'`
edate=$edate
ehour="23"
emin="59"
bid="null"
eid="null"


# next 6 lines echoing variables are just for 
# testing parsing of begin/end dates
#
# comment them out when parsing is working correctly
#

if [ $debug -eq 1 ]
then
 echo "begin date is $bdate"
 echo "begin hour is $bhour"
 echo "begin minute is $bmin"
 echo "end date is $edate"
 echo "end hour is $ehour"
 echo "end minute is $emin"
 echo " "
fi

# extract file size and convert to kilobyte format
lsline=`ls -l $mfile`
#echo lsline equals $lsline
amt=`echo $lsline | awk '{ print $4 }'`
amt=`expr $amt / 1024`
if [ $amt = 0 ]
then
# amt has to be at least 1
	amt=1
fi

if [ $debug -eq 1 ]
then
   echo "file size in K bytes is $amt"
fi

# finally build the empress sql command to insert a record into the
# on_line_phys_dir table and append the command to insonline file.
# turn off writing this SQL command (thus building or appending 
# to the insonline file) by setting the write_cmd variable to zero
# while testing, then turn it back on when ready to run, by setting
# the write_cmd variable to one (at top of this script)

if [ $write_cmd -eq 1 ]
then
   echo "insert into on_line_phys_dir values (" >> insonline
   echo "'"$id"', '"$bdate"', '"$bhour"','"$bmin"'," >> insonline
   echo "'"$edate"', '"$ehour"','"$emin"'," >> insonline
   echo "'"$desc"', "$bid", "$eid","$lfmt"," >> insonline
   echo "'"$amt"', "$code", "$pfmt",'"$add"'," >> insonline
   echo "'"$dir_path"','"$mfile"', "$hide", '"$archive"' );" >> insonline
fi

