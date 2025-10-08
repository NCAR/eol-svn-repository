import getpass
import MySQLdb
import os
import socket
from sys import stdout

def login(database):
	domain = socket.getfqdn()
	db_host = raw_input("Enter Host for Database %s:"%database)
	username = raw_input("Enter Username for Mysql %s:"%database)
	password = getpass.getpass("Enter Password for Mysql %s:"%database)
	try:
		db = MySQLdb.connect(host=db_host,user=username,passwd=password,db=database)
		return db
	except MySQLdb.OperationalError:
		print "Invalid username or password"
		return None
	
