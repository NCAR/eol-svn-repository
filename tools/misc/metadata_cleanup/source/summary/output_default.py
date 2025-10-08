#-----------------------------------
# File name: output_default.py 
# This script will export a list of datasets 
# which have their data, latitude, longitude, spatical type, frequency ID or grant_code set as default values.
# The projects included are 'ACADIS', 'AMTS', 'ARC-MIP', 'ARCSS', 'ATLAS', 'BARROW', 'BASE', 'BeringSea', 'BOREAS', 'ITEX', 'PacMARS', 'SBI', 'SHEBA'
# The output file is named output_default.csv.
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
# output_default() is used to export a list of datasets which have default values for required fields.
# args - filename: <string> the file name of exported csv file
#----------------------------------- 
def output_default(filename):
	try:
    	    os.remove(filename)
	except OSError:
    	    pass
	file = csv.writer(open(filename,'wb'))
	db = login('zith9')
	if db is None:
		return
	c = db.cursor()
	c2 = db.cursor()
	defaultlist  = ["id", "archive_ident","title","summary","begin_date","end_date","minimum_latitude","minimum_longitude","maximum_latitude","maximum_longitude","spatial_type","frequency_id","grant_code"]
	projectlist = "('ACADIS', 'AMTS', 'ARC-MIP', 'ARCSS', 'ATLAS', 'BARROW', 'BASE', 'BEST','BSIERP', 'BOREAS', 'ITEX', 'PacMARS', 'SBI', 'SHEBA')"
	count = 0

	file.writerow(["project","archive_ident","title","columns","defalut_value"])
	c.execute("select column_name,column_default from information_schema.columns where table_name='dataset';")
	for row in c.fetchall():
    	    if row[0] in defaultlist:
			if row[1] == None:
				w = "IS NULL"
			else:
				w = "='%s'"%row[1]
			c2.execute("select name,dataset.archive_ident,dataset.title, dataset.%s from project left join dataset_project on project.id =dataset_project.project_id join dataset on  dataset.id=dataset_project.dataset_id where project.name in %s AND dataset.%s %s" % (row[0],projectlist,row[0],w))
			for row2 in c2.fetchall():
				count = count +1
				stdout.write("\r%d default value have been found" % count)
        			stdout.flush()
        			file.writerow([row2[0],row2[1],row2[2],row[0],row[1]])
	stdout.write("\n")
	return
