#-----------------------------------
# This file is used to export all the datasets under each classification.
# The output file is named classification_tree.txt.
# The txt file can be imported into Tree.
# Author: Yuan Sui
# Created date: 1/15/2014
#-----------------------------------
import MySQLdb
import os
from sys import stdout
import getpass
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
# classification_tree() is used to export acronyms 
# args - filename: <string> the file name of exported txt file
#----------------------------------- 
def classification_tree(filename):
    #Get user name and password to access the databese
    try:
            os.remove(filename)
    except OSError:
            pass
    file = open(filename,'wb')
    db = login('dmg_merged_ml')
    if db is None:
        return
    c = db.cursor()
    count = 0
    classification= ""


    # Export project name, dataset archive_ident, dataseht title, dataset summary from zith9
    c.execute("Select classification.name,dataset.dataset_id,dataset.name from dataset join dataset_classification on dataset.dataset_id = dataset_classification.dataset_id right join classification on classification.class_id = dataset_classification.class_id;")

    for row in c.fetchall():
        # Print out prompts 
        count=count+1
        stdout.write("\r%d datasets have been checked" % count)
        stdout.flush()
    
        if classification != row[0]:
    	   classification = row[0]
    	   file.write(row[0]+"\n")
        if row[1] is None or row[2] is None:
    	   continue
        file.write(" "+row[1]+" || "+row[2]+"\n")

    stdout.write("\n")
    return
