#****************************************************************************************************
# count.py
# 
# This script is used to count how many files have been updated after running html_insert_helper.sh
# Pre-request: ./html_insert_helper.sh > Output.txt
# Output.txt is under the same directory as count.py
#
# Syntax: python count.py
# 
#****************************************************************************************************
import os
import csv
import sys
import getpass
import HTMLParser
from urllib2 import HTTPError
import shutil

def import_file(filename):
	rownum = 0
	try:
   		open_file = open(filename,'rb')
   		return open_file
	except IOError:
   		print 'Can not find imported file {0}'.format(filename)
   		return sys.exit(0)

f = import_file("./Output.txt")
checked =0
success = 0
fail = 0
updated = 0
skipped = 0
for line in f:
	l = line.split(" ")
	if "html/htm files were checked" in line:
		checked = checked+int(l[0])
	elif "files were updated" in line:
		success = success+int(l[0])
	elif "files failed to update, see log files for more info" in line:
		fail = fail+int(l[0])
	elif "files have already had script inserted" in line:
		updated = updated + int(l[0])
	elif "was skipped" in line:
		skipped = skipped + 1
print "checked = "+str(checked)
print "success = "+str(success)
print "fail = "+str(fail)
print "updated = "+str(updated) 
print "skipped = "+str(skipped)
