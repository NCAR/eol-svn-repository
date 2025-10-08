#! /bin/sh
# file_insert.sh - inserts entries into the table on_line_phys_dir
# in the phys_dir_db database.

# This script is typically used to insert a number of files 
# (such as satellite GIF images) into
# phys_dir_db:on_line_phys_dir (database:table) where the files
# are named in such a way that the beginning and ending date/time
# can be parsed from the file name.

# You must be on storm to run this script since it accesses the
# Empress databases, and you must be logged on under a userid 
# such as stormdba or stormmnt that has permission to write into the 
# phys_dir_db:on_line_phys_dir (database:table).


# The script ./create_insonline.sh must also exist in the same directory
# as this script.

# Author: Ronald A. Murdock


# EDIT THE FOLLOWING LINE 
# (following command must list all file names to enter into phys_dir_db)
# (this example assumes that the files in this directory 
# that are going to be entered into the on_line_phys_dir table are all named
# something that ends in .gif)
list=`ls gage.hrly.prcp.*`

# The file insonline is used for input to the Empress utility empbatch.
# It will be built and appended to when the create_insonline.sh 
# script is executed.
# Remove insonline here to start with a new file.
# (Note that insonline is not removed at the end of this script, 
# although it could be.  It is sometimes useful to see the commands
# that were actually written into the file insonline for debugging
# purposes or historical purposes, so this script leaves the file intact.
rm -f insonline

# For each file name to be entered into phys_dir_db,
# run the script create_insonline.sh using that file name for an argument.
# Each iteration of the do loop will add some SQL commands to the 
# insonline file.
for i in $list
do
   echo "Processing file $i"
  ./create_insonline.sh $i
done

# after the insonline file is complete, run empbatch using insonline
# as the input argument.  The database will be phys_dir_db.
######/usr/empress68/bin/empbatch /storm/codiac/codiac_db/phys_dir_db <./insonline
