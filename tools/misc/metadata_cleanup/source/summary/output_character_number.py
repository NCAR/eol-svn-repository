#-----------------------------------
# File name: output_character_number.py 
# This script will export the list of datasets which have their dataset summary less than 140 characters.
# The projects included are 'ACADIS', 'AMTS', 'ARC-MIP', 'ARCSS', 'ATLAS', 'BARROW', 'BASE', 'BeringSea', 'BOREAS', 'ITEX', 'PacMARS', 'SBI', 'SHEBA'
# The output file is named output_character_number.csv.
# Author: Yuan Sui
# Created date: 10/18/2013
#-----------------------------------
import MySQLdb
import os
import csv
from sys import stdout
import getpass
from HTMLParser import HTMLParser
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
# MLStripper() and strip_tags() are used to get rid of HTML tags in the text.
# args - html: <string> text to be processed
#----------------------------------- 
class MLStripper(HTMLParser):
	def __init__(self):
		self.reset()
		self.fed = []
	def handle_data(self, d):
		self.fed.append(d)
	def get_data(self):
		return ''.join(self.fed)
def strip_tags(html):
	s = MLStripper()
	s.feed(html)
	return s.get_data()
#-----------------------------------
# output_character_number() is used to export the list of datasets which have their dataset summary less than 140 characters.
# args - filename: <string> the file name of exported csv file
#----------------------------------- 
def output_character_number(filename):
	try:
        	os.remove(filename)
	except OSError:
    	    pass
	file = csv.writer(open(filename,'wb'))
	db = login('zith9')
	if db is None:
		return
	c = db.cursor()
	projectlist = "('ACADIS', 'AMTS', 'ARC-MIP', 'ARCSS', 'ATLAS', 'BARROW', 'BASE', 'BEST','BSIERP', 'BOREAS', 'ITEX', 'PacMARS', 'SBI', 'SHEBA')"
	count = 0
	#number = c.execute("SELECT COUNT(*) FROM dataset")
	file.writerow(["project","archive_ident","title","SummaryCharacter"])
	c.execute("select name,dataset.archive_ident,dataset.title,dataset.summary from project left join dataset_project on project.id =dataset_project.project_id join dataset on dataset.id=dataset_project.dataset_id where project.name in %s" % projectlist)

	for row in c.fetchall():
		w = strip_tags(str(row[3]))
		if len(w)<140:		
			file.writerow([row[0],row[1],row[2],len(w)])
			count = count + 1
			stdout.write("\r%d short datasets have been found" % count)
       			stdout.flush()
	stdout.write("\n")
	return
