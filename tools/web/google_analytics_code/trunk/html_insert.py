#****************************************************************************************************
# html_insert.py
# 
# This file is used to recursively insert google analytics code into .html or .htm file under 
# required directory. Each directory will generate a log file (*_logfile.csv) under /net/work/HTML_INSERT
# Need rilling_files.list under same directory. The script will skipped all the file listed in rilling_files.list
# 
# Syntax: python html_insert.py <subdirectory_name1> <subdirectory_name2> 
# 		or python html_insert.py all
# 
# 20 Jun 14, Yuan Sui(ys)
# 20 Jun 14, YS: Insert code without changing file owner.
# 20 Jun 14, YS: Add Comments
# 20 Jun 14, YS: Use shutil.copyfile
# 23 Jun 14, YS: Add comments, simplize writing process, write line by line
# 25 Jun 14, YS: Solved HTMLParseError, handle HTMLParseError
# 01 Jul 14, YS: Final updated
#****************************************************************************************************

import os
import csv
import sys
import getpass
import HTMLParser
from urllib2 import HTTPError
import shutil

#------------------------------------------------------
# MLStripper is used to analyse HTML page and get position of 
# <head>, </head>, <html> or </html> tag
# Parameters: string HTMLcode
# Return Value: 
# 	index: (-1,0) means none of <html>, </html>, <head> or </head> tag exists
#			(-2, 0) means analytics code has already been inserted 
#			others return value mean (line_number, offset) of corresponding tag
# 	tag: <html>, </html>, <head>, or </head>
#------------------------------------------------------
class MLStripper(HTMLParser.HTMLParser):
	
	def __init__(self):
		self.reset()
		self.fed = []
		self.index = (-1,0)
		self.tag = ""
		self.text = """(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
})(window,document,'script','//www.google-analytics.com/analytics.js','ga');

 ga('create', 'UA-2241808-1', 'ucar.edu');
 ga('send', 'pageview');"""
 	def handle_starttag(self, tag, attr):
 		#if only html exist
 		#get position of <html>
 		#tag = html
 		if(self.index[0]==-1 and tag=="html"):
			self.index = self.getpos()
			self.tag = "html"
		#if only head eixst
		#get position of <head>
		#tag = head
		if(self.index[0]>-2 and tag=="head"):
			self.tag = "head"
			self.index = self.getpos()
	def handle_endtag(self, tag):
		#if both <head></head> exist
		#get position of </head>
		#tag = /head
		if(self.index[0]>-2 and tag=="head" and self.tag=="head"):
			self.index = self.getpos()
			self.tag = "/head"
		#if only </head> exist
		#get position of </head>
		#tag = /head2
		elif(self.index[0]>-2 and tag=="head"):
			self.index = self.getpos()
			self.tag = "/head2"
	def handle_data(self, data):
		if(self.text in data):
			self.index = (-2,0)
	def return_index(self):
		return self.index
	def return_tag(self):
		return self.tag

#------------------------------------------------------
# import_file is used to open specific readable file
# It will catch IOError when the file cannot be open.
#
# Parameters: string filename
# Return Value: File open_file
#------------------------------------------------------
def import_file(filename):
   	open_file = open(filename,'rb')
   	return open_file


#------------------------------------------------------
# export_file is used to open specific writable file
# It will remove the original file if the file already existed 
#
# Parameters: string filename
# Return Value: File file
#------------------------------------------------------
def export_file(filename):
	try:
    		os.remove(filename)
	except OSError:
    		pass
	file = open(filename,'wb')
	return file

#------------------------------------------------------
# generate_filelist_helper will recursively generate a list of html file under specific directory
# - if the directory is not readable, the function will print error message and continue
# - if the directory is a html file, the function will append the file path in list
# - if the directory is a dir, the function will go into the dir recursively
# 
# Parameters: string directory, list filelist
# Return Value: 
#------------------------------------------------------
def generate_filelist_helper(directory,filelist,avoid_list):
	try:
		dirlist = os.listdir(directory)
	except OSError as err:
		print str(err)+", {0} will not be updated".format(directory)
		return
	for f in dirlist:
		filepath = os.path.join(directory,f)
		if os.path.isfile(filepath):
			if filepath.replace('/net/www/docs/','') in avoid_list:
				print "{0} was skipped".format(filepath)
				continue
			if filepath.lower().endswith('.html') or filepath.lower().endswith('.htm'):				
				if not os.path.islink(filepath):
					filelist.append(filepath)
		elif os.path.isdir(filepath):
			if not os.path.islink(filepath):
				generate_filelist_helper(filepath,filelist,avoid_list)
	return

#------------------------------------------------------
# generate_filelist will generate a list of html file under specific directory
# - if the directory is a html file, it returns True and append filelist
# - If the directory is other file, it returns False
# - if the directory is a dir, the function calls generate_filelist_helper() to go through
#   and return True if the filelist has any item, return False if the list is empty 
# 
# Parameters: string directory, list filelist
# Return Value: boolean status
#------------------------------------------------------
def generate_filelist(directory,filelist,avoid_list):
	if os.path.exists(directory):
		if os.path.isfile(directory):
			if directory.lower().endswith('.html') or directory.lower().endswith('.htm'):
				filelist.append(directory)
				return True
			else:
				return False
		elif os.path.isdir(directory):
			if not os.path.islink(directory):
				generate_filelist_helper(directory,filelist,avoid_list)
			if(len(filelist)>0):
				return True
			else:
				return False
	else:
		return False

#------------------------------------------------------
# generate_log_file will generate logfile for processed directory
# - The function generates a csv file accroding to error list
# - If the HTML file inserted successfully, the message is "Inserted successfully"
# - If the HTML file already been updated, the error message is "Updated already" 
# - If the HTML file is empty, the error message is "Failure, Empty file or other errors"
# - if the HTML file is broken, the message is "The <head></head> or <html></html> tag are not found, inserted at the top"
# - If the HTML file has other errors, the format will be "Failure, <error message>" 
# Parameters: list errorlist, string filename
# Return Value: 
#	int failnum: number of files that fail to update
#	int successnum: number of files updated successfully
#	int updatednum: number of files have already been updated in previous run
#------------------------------------------------------
def generate_log_file(errorlist, filename):
	try:
    		os.remove(filename)
	except OSError:
    		pass
    	file = csv.writer(open(filename,'wb'))
    	failnum = 0
    	successnum = 0
	updatednum = 0
	for row in errorlist:
		if "Failure" in row[1]:
			failnum = failnum +1
		elif "Updated already" in row[1]:
			updatednum = updatednum + 1
		else:
			successnum= successnum + 1
		file.writerow(row)	
	file.writerow(["{0} html/htm files were checked".format(successnum+failnum+updatednum),"{0} files were updated".format(successnum),"{0} files failed to update".format(failnum),"{0} files already been updated".format(updatednum)])
	return (failnum,successnum,updatednum)

def avoid_list(filename):
	f = import_file(filename)
	return_list = list()
	for line in f:
		return_list.append(line[2:len(line)].strip())
	return return_list
#------------------------------------------------------
# insert_text will insert Googlde analytics script into HTML pages at proper position
# - The function can handle 8 scenarios
# - If both <head> and </head> tag exist, insert_text will be inserted within <head></head>
# - If none of <head> or </head> exists, insert_text2 will be inserted at the top of file or under <html> tag
# - If <head> exists but </head> does not, insert_text3 will be inserted after <head>
# - if </head> exists but <head> does not, insert_text4 will be inserted before </head>
# - If the analytics code has already be inserted, file will be skipped and be logged
# - If the file is empty, script will not be inserted and the file will be logged
# - If the user has no permission to read/write the file, the file will be skipped and logged in logfile
# - If any other error causes file inserted unsuccessfully, it will be skipped and logged in logfile
#
# Parameters: list errorlist, string filename
# Return Value: 
#	
#------------------------------------------------------
def insert_text(filename, errorlist):
	#If both <head> and </head> tag exist, insert_text will be inserted within <head></head>
	insert_text = """
<script>
(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
})(window,document,'script','//www.google-analytics.com/analytics.js','ga');

 ga('create', 'UA-2241808-1', 'ucar.edu');
 ga('send', 'pageview');

</script>
"""
	#If none of <head> or </head> exists, insert_text2 will be inserted at the top of file or under <html> tag
	insert_text2 = """
<head>
<script>
(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
})(window,document,'script','//www.google-analytics.com/analytics.js','ga');

 ga('create', 'UA-2241808-1', 'ucar.edu');
 ga('send', 'pageview');

</script>
</head>
"""
	#If <head> exists but </head> does not, insert_text3 will be inserted after <head>
	insert_text3 = """
<script>
(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
})(window,document,'script','//www.google-analytics.com/analytics.js','ga');

 ga('create', 'UA-2241808-1', 'ucar.edu');
 ga('send', 'pageview');

</script>
</head>
"""

	#if </head> exists but <head> does not, insert_text4 will be inserted before </head>
	insert_text4 = """
<head>
<script>
(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
})(window,document,'script','//www.google-analytics.com/analytics.js','ga');

 ga('create', 'UA-2241808-1', 'ucar.edu');
 ga('send', 'pageview');

</script>
"""
	
	#1. Import file and read html code
	try:
		in_file = import_file(filename)
		html = in_file.read()
	except IOError as err:
		errorlist.append([filename,"Failure, "+str(err)])
		return
	in_file.close()
	
	linedex=0
	offset = 0
	tag = ""

	#1.1 Use HTML parser to parse the code, and fine line number and offset of correspoding tags
	try:
		s = MLStripper()
		s.feed(html.decode("unicode_escape").encode("utf-8"))
	except (HTMLParser.HTMLParseError, UnicodeDecodeError) as err:
		#1.1.1 Check if it is microsoft-word HTML page
		lines = html.split("\n")
		if "Microsoft" or "microsoft" in lines[0]:
			for i in range(len(lines)):
				if lines[i].startswith("<head>") :
					if "GoogleAnalyticsObject" in lines[i+2]:
						linedex= -2
						offset = 0
						tag = "none"
					else:	
						linedex=i+1
						offset = 0
						tag = "head2"
			if tag == "":
				errorlist.append([filename,"Failure:"+str(err)])
				in_file.close()
				return
		else:
			print filename
			errorlist.append([filename,"Failure:"+str(err)])
			in_file.close()
			return
	if tag=="":
		#1.2 get line number and offset of <head> or <html> tag
		linedex,offset = s.return_index()
		tag = s.return_tag()
	
	#1.3.1 If none of <head></head><html> or </html> exists, insert_text2 will be inserted at the top of file 
	if linedex==-1:
		linedex = 1
		tag = "none"
	#1.3.2 If the analytics code has already be inserted, file will be skipped and be logged
	elif linedex==-2:
		errorlist.append([filename,"Updated already"])
		in_file.close()
		return

	#2. Import file and prepare for write
	in_file = import_file(filename)
	
	#2.1 create temp file "filename~"
	try:
		ex_file = export_file(filename+"~")
	except IOError as err:
		errorlist.append([filename,"Failure, "+str(err)])
		in_file.close()
		return
	
	#2.2 insert google analytics code into html page
	lineno = 0
	flag = True
	for line in in_file:
		if lineno+1 == linedex:
			#2.2.1 If both <head> and </head> tag exist, insert_text will be inserted within <head></head>
			if(tag=="/head"):
				line = line[0:offset]+insert_text+line[offset:len(line)]
				errorlist.append([filename,"Inserted successfully"])
				flag = False
			#2.2.2 If </head> exists but <head> does not, insert_text4 will be inserted before </head>
			elif(tag=="/head2"):
				line = line[0:offset]+insert_text4+line[offset:len(line)]
				errorlist.append([filename,"Inserted successfully"])
				flag = False
			#2.2.3 If <head> exists but </head> does not, insert_text3 will be inserted after <head>
			elif(tag=="head"):
				line = line[0:offset+6]+insert_text3+line[offset+6:len(line)]
				errorlist.append([filename,"Inserted successfully"])
				flag = False
			#2.2.4 If none of <head> or </head> exists, insert_text2 will be under <html> tag
			elif(tag=="html"):
				line = line[0:offset+6]+insert_text2+line[offset+6:len(line)]
				errorlist.append([filename,"Inserted successfully"])
				flag = False
			#2.2.5 If none of <head></head><html> or </html> exists, insert_text2 will be inserted at the top of file 
			elif(tag=="none"):
				line = insert_text2+line[0:len(line)]
				errorlist.append([filename,"The <head></head> or <html></html> tag are not found, inserted at the top"])
				flag = False
			elif(tag=="head2"):
				line = line[0:offset+6]+insert_text+line[offset+6:len(line)]
				errorlist.append([filename,"Microsoft format. Inserted code at line {0}".format(str(lineno))])
				flag = False
		ex_file.write(line)
		lineno = lineno + 1
	#2.2.6 If the file is empty, script will not be inserted and the file will be logged
	if(flag):
		errorlist.append([filename,"Failure, Empty file or other errors"])
	ex_file.close()
	in_file.close()
	
	#2.3 Copy the content of temp file back to original file
	try:
		shutil.copyfile(filename+"~",filename)
	except IOError as err:
		errorlist.pop()
		errorlist.append([filename,"Failure, "+str(err)])
		os.remove(filename+"~")
		return
	
	#2.4 Remove temp file
	os.remove(filename+"~")

if __name__== "__main__":
	if len(sys.argv)<2:
		print "Please enter <all> or <file name> or <sub-folder name> as arguement"
	#If argument is <all>, html_insert.py will run the script on all the files under current directory
	elif str(sys.argv[1])=="all":
		avoid_list = avoid_list("./rilling_files.list")
		filelist = list()
		if not (generate_filelist("./",filelist,avoid_list)):
			print "There is no html/htm file under targeted directory"
			sys.exit(0)
		errorlist = list()
		n = 0
		for f in filelist:
			insert_text(f, errorlist)
			n = n + 1
		failnum,successnum,updatednum = generate_log_file(errorlist, "/net/work/HTML_INSERT/logfile.csv")
		print "\t{0} html/htm files were checked\n\t{1} files were updated\n\t{2} files failed to update, see log files for more info\n\t{3} files have already had script inserted".format(n,successnum,failnum,updatednum)
	#If argument is <directory_name>, html_insert.py will run the script on all the files under <directory_name>
	else:
		avoid_list = avoid_list("./rilling_files.list")
		for i in range(1,len(sys.argv)):
			filelist = list()
			if(generate_filelist(sys.argv[i],filelist,avoid_list)):
				errorlist = list()		
				n = 0
				for f in filelist:
					insert_text(f, errorlist)
					n = n + 1
			
				log_name = "/net/work/HTML_INSERT/"+sys.argv[i].replace("/","_") +"_logfile.csv"
				#log_name = sys.argv[i].replace("/","_") +"_logfile.csv"
				failnum,successnum,updatednum = generate_log_file(errorlist, log_name)
				print "In diretory/file <{0}>".format(sys.argv[i])
				print "\t{0} html/htm files were checked\n\t{1} files were updated\n\t{2} files failed to update, see log files for more info\n\t{3} files have already had script inserted".format(n,successnum,failnum,updatednum)

			else:
				print "Directroy/File <{0}> cannot be processed, it may be caused by:".format(sys.argv[i])
				print "\t1. Directory/file does not exists"
				print "\t2. There is no html/htm file under targeted directory"
				print "\t3. Targeted file is not html or htm format"
				continue
