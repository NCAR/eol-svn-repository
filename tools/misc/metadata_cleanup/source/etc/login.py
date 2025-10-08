import getpass
import MySQLdb
import os
import socket
from sys import stdout
def login(database):
	domain = socket.getfqdn()
	if database is "dmg_merged_ml":
		if domain!="riesling.eol.ucar.edu": 
        		print "You are on the server '%s'\n'dmg_mergerd_ml' database is on 'riesling.eol.ucar.edu'\nPlease Login to riesling and run the script again"%domain
        		return None
		username = raw_input("Enter Username for Mysql %s:"%database)
		password = getpass.getpass("Enter Password for Mysql %s:"%database)
		try:
			db = MySQLdb.connect(user=username,passwd=password,db = database)
			return db
		except MySQLdb.OperationalError:
			print "Invalid username or password"
			return None
	elif database is "zith9":
		username = raw_input("Enter Username for Mysql %s:"%database)
		password = getpass.getpass("Enter Password for Mysql %s:"%database)
		try:
			db = MySQLdb.connect(host = "farskol",user=username,passwd=password,db = database)
			return db
		except MySQLdb.OperationalError:
			print "Invalid username or password"
			return None

def login_username(database,username,password):
	domain = socket.getfqdn()
	if database is "dmg_merged_ml":
		if domain!="riesling.eol.ucar.edu": 
        		print "You are on the server '%s'\n'dmg_mergerd_ml' database is on 'riesling.eol.ucar.edu'\nPlease Login to riesling and run the script again"%domain
        		return None
		try:
			db = MySQLdb.connect(user=username,passwd=password,db = database)
			return db
		except MySQLdb.OperationalError:
			print "Invalid username or password"
			return None
	elif database is "zith9":
		try:
			db = MySQLdb.connect(host = "farskol",user=username,passwd=password,db = database)
			return db
		except MySQLdb.OperationalError:
			print "Invalid username or password"
			return None