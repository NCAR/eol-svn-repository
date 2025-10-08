#-----------------------------------
# This file is used to export all the datasets under each platforms.
# The output file is named output_platform.txt.
# Author: Yuan Sui
# Created date: 10/18/2013
#-----------------------------------
import MySQLdb
import os
from sys import stdout
import getpass
import csv
from etc.login import login

#-----------------------------------
# tsplit method is used to split the string s into substring list by the delimiters in string sep.
#----------------------------------- 
def tsplit(s,sep):
	stack =[s]
	for char in sep:
		pieces = []
		for substr in stack:
			pieces.extend(substr.split(char))
		stack = pieces
	return stack
def platform_dataset(filename):
    #Get user name and password to access the databese
    try:
            os.remove(filename)
    except OSError:
            pass
    file=csv.writer(open(filename,'wb'))
    db = login('zith9')
    if db is None:
        return
    c = db.cursor()
    count = 0


    # Export project name, dataset archive_ident, dataseht title, dataset summary from zith9
    c.execute("Select dataset.archive_ident,dataset.title,platform.name from dataset join dataset_platform on dataset.id = dataset_platform.dataset_id right join platform on platform.id = dataset_platform.platform_id;")

    for row in c.fetchall():
        # Print out prompts 
        count=count+1
        stdout.write("\r%d datasets have been checked" % count)
        stdout.flush()
    
        if row[0] is None or row[1] is None:
    	   continue
        file.writerow([row[0],row[1],row[2]])

    stdout.write("\n")
    return
