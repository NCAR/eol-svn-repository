#-----------------------------------
# File name: output_acronyms.py 
# This script will export all the acronyms in the Arctic dataset summary.
# The projects included are 'ACADIS', 'AMTS', 'ARC-MIP', 'ARCSS', 'ATLAS', 'BARROW', 'BASE', 'BeringSea', 'BOREAS', 'ITEX', 'PacMARS', 'SBI', 'SHEBA'
# The output file is named output_acronyms.csv.
# Author: Yuan Sui
# Created date: 10/18/2013
#-----------------------------------
import MySQLdb
import os
import csv
from sys import stdout
import sys

#sys.path.append(os.path.abspath("../etc/login.py"))
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
# output_acronyms() is used to export acronyms 
# args - filename: <string> the file name of exported csv file
#----------------------------------- 
def output_acronyms(filename):
	try:
        	os.remove(filename)
	except OSError:
        	pass
	#Get user name and password to access the databese
	file = csv.writer(open(filename,'wb'))
	db = login('zith9')
	if db is None:
		return
	c = db.cursor()
	delimiter = ';,.() '
	count = 0
	file.writerow(["project","archive_ident","title","Acronyms","Acronyms_number"])

	# Export project name, dataset archive_ident, dataseht title, dataset summary from zith9
	c.execute("select name,dataset.archive_ident,dataset.title,dataset.summary from project join dataset_project on project.id =dataset_project.project_id join dataset on  dataset.id=dataset_project.dataset_id WHERE project.name in ('ACADIS', 'AMTS', 'ARC-MIP', 'ARCSS', 'ATLAS', 'BARROW', 'BASE', 'BEST','BSIERP', 'BOREAS', 'ITEX', 'PacMARS', 'SBI', 'SHEBA')")

	for row in c.fetchall():
    	# Print out prompts 
    		count=count+1
    		stdout.write("\r%d datasets have been checked" % count)
    		stdout.flush()
    		w = ''
    		s = list()
    	#If summary is not empty, then split summary into words.
    		if row[3]!=None:
   				s = tsplit(row[3],delimiter)
   				acronyms_number = 0
		#For each substring list, if the word has two letters to be capital ones, export as acronyms.    	
				for item in s:
					item = str(item)
					if len(item)>1:
						for i in range(0,len(item)-1):
							if item[i]==item[i].upper() and item[i+1]==item[i+1].upper():
    								if item[i].isalpha() and item[i+1].isalpha():
										if item not in w:
											w = w+item+" || "
											acronyms_number=acronyms_number+1
					else:
						continue
				file.writerow([row[0],row[1],row[2],w,acronyms_number])
	stdout.write("\n")
	return
