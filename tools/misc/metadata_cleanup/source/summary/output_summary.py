#-----------------------------------
# This python script will export all the Arctic dataset summary.
# The projects included are 'ACADIS', 'AMTS', 'ARC-MIP', 'ARCSS', 'ATLAS', 'BARROW', 'BASE', 'BeringSea', 'BOREAS', 'ITEX', 'PacMARS', 'SBI', 'SHEBA'
# The output file is named output_acronyms.csv.
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
# output_default() is used to export a list of summary for Artic datasets.
# args - filename: <string> the file name of exported csv file
#----------------------------------- 
def output_summary(filename):
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
	# Export project name, dataset archive_ident, dataseht title, dataset summary from zith9
	file.writerow(["project","archive_ident","title","Summary"])
	c.execute("select name,dataset.archive_ident,dataset.title,dataset.summary from project left join dataset_project on project.id =dataset_project.project_id join dataset on  dataset.id=dataset_project.dataset_id WHERE project.name in ('ACADIS', 'AMTS', 'ARC-MIP', 'ARCSS', 'ATLAS', 'BARROW', 'BASE', 'BEST','BSIERP', 'BOREAS', 'ITEX', 'PacMARS', 'SBI', 'SHEBA')")

	for row in c.fetchall():
		 # Print out prompts
		count=count+1
		stdout.write("\r%d datasets have been outputed" % count)
		stdout.flush()
		s = 0
		# Get rid of HTML tages
		w = strip_tags(str(row[3]))
		file.writerow([row[0],row[1],row[2],w])
	stdout.write("\n")
	return
