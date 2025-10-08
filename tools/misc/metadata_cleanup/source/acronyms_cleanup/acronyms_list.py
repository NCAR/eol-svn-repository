#-----------------------------------
# This file is used to export all the acronyms and dataset ID in Arctic dataset summary, sorted by acronyms.
# The output file is named acronyms_list.csv.
# Please run it on Sferic, Ronning command: python acronym_list.py 
# Author: Yuan Sui
# Created date: 11/10/2013
#-----------------------------------
import MySQLdb
import os
import csv
from sys import stdout
import getpass
from etc.login import login_username



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
def acronyms_list(filename, username, password):
	try:
    	    os.remove(filename)
	except OSError:
    	    pass
	#Get user name and password to access the databese
	file = csv.writer(open(filename,'wb'))
	db = login_username('zith9',username,password);
	if db is None:
		return
	c = db.cursor()
	delimiter = ';,.()<> '
	count = 0
	acronyms_list = list()
	ident_list = list()
	file.writerow(["Acronyms","archive_ident"])

	# Export project name, dataset archive_ident, dataseht title, dataset summary from zith9
	c.execute("select dataset.archive_ident,dataset.summary from project left join dataset_project on project.id=dataset_project.project_id join dataset on dataset.id=dataset_project.dataset_id WHERE project.name in ('ACADIS', 'AMTS', 'ARC-MIP', 'ARCSS', 'ATLAS', 'BARROW', 'BASE', 'BEST','BSIERP', 'BOREAS', 'ITEX', 'PacMARS', 'SBI', 'SHEBA') ")

	for row in c.fetchall():
    		# Print out prompts 
    		count=count+1
    		stdout.write("\r%d datasets have been checked" % count)
    		stdout.flush()
    		s = list()
    		#If summary is not empty, then split summary into words.
    		if row[1]!=None:
   			s = tsplit(row[1],delimiter)
			#For each substring list, if the word has two letters to be capital ones, export as acronums.    	
			for item in s:
				item = str(item)
				if len(item)>1:
					for i in range(0,len(item)-1):
						if item[i]==item[i].upper() and item[i+1]==item[i+1].upper():
    							if item[i].isalpha() and item[i+1].isalpha():
									if item not in acronyms_list:
										acronyms_list.append(item)
										ident_list.append(row[0])
									else:
										flag = 0
										for ident in ident_list:
											if row[0] in ident:
												flag = 1
												break
										if flag!=1:
											word = ident_list[acronyms_list.index(item)]
											ident_list[acronyms_list.index(item)] = word + " || "+row[0]
				else:
					continue
	for i in range(0,len(acronyms_list)):
		file.writerow([acronyms_list[i],ident_list[i]])
	stdout.write("\n")
	return
