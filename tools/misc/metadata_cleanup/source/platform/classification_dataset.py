#-----------------------------------
# This file is used to export all the datasets under each classification.
# The output file is named output_classification.csv.
# The txt file can be imported into Tree.
# Author: Yuan Sui
# Created date: 1/15/2014
#-----------------------------------
import MySQLdb
import os
from sys import stdout
import getpass
import csv
from etc.login import login


#-----------------------------------
# tsplit() method is used to split the string s into substring list by the delimiters in string sep.
# args - s: <string> string to be splited
# args - sep: <string> delimiters to split s
#----------------------------------- 
def tsplit(s,sep):
	stack =[s]
	for char in sep:
		pieces = []
		for substr in stack:
			pieces.extend(substr.split(char))
		stack = pieces
	return stack

#-----------------------------------
# import_file() is used to import csv file and transform it into a ditionary in python
# args - filename: <string> the imported file name
# return - dictionary
#-----------------------------------

def import_file(filename):
    dictionary = dict()
    try:
        platform_file = csv.reader(open(filename,'rU'))
        for row in platform_file:
            if row[0] != "":
                dictionary[row[0]]=row[2]
        return dictionary
    except IOError:
        print 'Can not find imported platform_dataset.csv file. please run platform_dataset.py firstly'
        return dictionary

def classification_dataset(filename1,filename2):
    #Get user name and password to access the databese
    try:
            os.remove(filename1)
    except OSError:
            pass
    file=csv.writer(open(filename1,'wb'))
    db = login('dmg_merged_ml')
    if db is None:
        return
    c = db.cursor()
    count = 0
    dictionary = import_file(filename2)
    platform = ""
    file.writerow(["archieve_ident","title","classification","platform"])


    # Export project name, dataset archive_ident, dataseht title, dataset summary from zith9
    c.execute("Select dataset.dataset_id,dataset.name,classification.name from dataset join dataset_classification on dataset.dataset_id = dataset_classification.dataset_id right join classification on classification.class_id = dataset_classification.class_id;")

    for row in c.fetchall():
        # Print out prompts 
        count=count+1
        stdout.write("\r%d datasets have been checked" % count)
        stdout.flush()
    
        if row[0] is None or row[1] is None:
    	   continue
        if row[0] in dictionary:
            platform = dictionary[row[0]]
        else:
            platform = ""
        file.writerow([row[0],row[1],row[2],platform])

    stdout.write("\n")
    return
