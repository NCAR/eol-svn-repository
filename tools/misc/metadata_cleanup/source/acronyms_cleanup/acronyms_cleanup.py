#-----------------------------------
# This file is used to help clean up all the acronyms in the Arctic dataset summary.Wrote in Python
# Please run it on sferic server. Running command: python acronym_cleanup.py
# The output file is named spell_out_list.csv.
# Author: Yuan Sui
# Created date: 11/25/2013
#-----------------------------------
import MySQLdb
import os
import csv
import sys
#from sys import stdout
import getpass
from HTMLParser import HTMLParser
from acronyms_list import acronyms_list
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

def quote(text):
	text.replace("'","''")
	print text+'\n'
	return text
def import_file(filename):
	dictionary = dict()
	rownum = 0
	try:
   		spellout_file = csv.reader(open(filename,'rU'))
   		for row in spellout_file:
			if rownum!=0:
				if row[0] != "":
					l = list()
					l.append(row[1])
					s = row[2].split("||")
					for item in s:
						if item != "":
							l.append(item)
				dictionary[row[0]]=l
			rownum=rownum+1;
		print dictionary		
		return dictionary
	except IOError:
   		print 'Can not find imported spell_out file. Will generate a new spreadsheet'
   		return dictionary
	#if file not exit
	#else

def export_file(filename,dictionary):
	try:
    		os.remove(filename)
	except OSError:
    		pass
	file = csv.writer(open(filename,'wb'))
	file.writerow(["acronym","spell_out","variation"])
	for key in dictionary:
		w = ""
		if key != "":
			if dictionary[key] is not None:
				for item in dictionary[key]:
					if dictionary[key].index(item)!=0:
						w = w+item+"||"
				file.writerow([key,dictionary[key][0],w])

def sentences_in_summary(ident,cursor,acronym):
	sentences = list()
	cursor.execute("SELECT dataset.summary FROM dataset WHERE dataset.archive_ident='%s'" % ident)
	summary = strip_tags(str(cursor.fetchone()))
	summary = summary.lstrip("('")
	summary = summary.lstrip("',)")
	sentences = summary.split(". ")
	result = ""
	for sentence in sentences:
        	if acronym in sentence:
				print"************Archive_ident = %s********************" % ident
				print sentence
				print"*****************************************************"
				result = sentence
				break
	return result
def autofill(acronym, dictionary, sentence, item,cursor):
	spell_out = ""
	if acronym in dictionary:
		for variation in dictionary[acronym]:
			if variation in sentence:
				if dictionary[acronym].index(variation) == 0:
					#skip
					print("'%s' is correct, %s is skipped" % (acronym+": "+variation, item))
					return True
				else:
					#replace
					spellout_continue = raw_input("Do You Want to replace '%s' with '%s' [Y/N]:" %(variation, dictionary[acronym][0]))
					if spellout_continue == "Y" or spellout_continue == "y" :
						cursor.execute("UPDATE dataset SET dataset.summary = REPLACE(dataset.summary,'%s','%s') WHERE dataset.archive_ident='%s'" %(variation,dictionary[acronym][0],item))
						
						sentences_in_summary(item,cursor,acronym)
						return True
					else:
						continue
			else:
				continue
	return False
def manualfill(acronym, dictionary, sentence, item,cursor):
	if acronym in dictionary:
		acronym_spell_out = dictionary[acronym][0]
	else:
		spell_out = raw_input("Spell out of Acronym '%s': " % acronym)
		acronym_spell_out = "%s (%s)" %(spell_out,acronym)
	replace_part = raw_input("Replace Part: ")
	replace_part = quote(replace_part)
	acronym_spell_out = quote(acronym_spell_out)
	spellout_continue = raw_input("Do You Want to replace '%s' with '%s' [Y/N]:" %(replace_part, acronym_spell_out))
	if spellout_continue == "Y" or spellout_continue == "y":
		cursor.execute("""UPDATE dataset SET dataset.summary = REPLACE(dataset.summary,"%s","%s") WHERE dataset.archive_ident='%s'""" %(replace_part,acronym_spell_out,item))
		print(cursor.fetchone())
		if acronym not in dictionary:
			l = list()
			l.append(acronym_spell_out)
		else:
			l = dictionary[acronym]
		if replace_part not in l:
			l.append(replace_part)
		dictionary[acronym] = l
		return True
			
	else: 
		return False					

def acronyms_cleanup(filename):
	#Get user name and password to access the databese
	username = raw_input("Enter Username for zith9:")
	password = getpass.getpass("Enter Password for zith9:")
	acronyms_list("outputs/acronyms_list.csv",username,password)
	try:
		acronyms_file = csv.reader(open("outputs/acronyms_list.csv",'rU'))
	except IOError:
   		print 'acronyms_list.csv not exist. Please run acronyms_list.py first.'
   		sys.exit(0)
	
	db = login_username('zith9',username,password);
	if db is None:
		return
	c = db.cursor()

	dictionary = import_file(filename)

	rownum = 0
	for row in acronyms_file:
		if rownum!=0:
			s = row[1].split(" || ")
			for item in s:
				if item != "":
					while(1):
						print('\n')
						sentence = sentences_in_summary(item,c,row[0])
						if autofill(row[0], dictionary, sentence, item,c):
							db.commit()
							break
						process_continue = raw_input("Do You Want to Spell Out '%s' manually[Y/N]:" % row[0])
						if process_continue == "Y" or process_continue == "y":
							if manualfill(row[0], dictionary, sentence, item,c):
								db.commit()
								sentences_in_summary(item,c,row[0])
								process_continue = raw_input("Do You Want to Update Exprot File[Y/N]:")
								if process_continue=="Y" or process_continue=="y":
									export_file("spell_out_list.csv",dictionary)
								break
							else:

								process_continue = raw_input("Do You Want to enter again [Y/N]: ")
								if process_continue=="Y" or process_continue=="y":
									continue
								else:
									break
						elif process_continue == "N" or process_continue == "n":
							if row[0] not in dictionary:
								l = list()
								l.append("")
								dictionary[row[0]]=l
								export_file("spell_out_list.csv",dictionary)
							break
		
				
		rownum=rownum+1




