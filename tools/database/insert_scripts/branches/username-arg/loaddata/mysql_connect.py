#!/usr/bin/python
import mysql.connector
import sys

class mysqlConnect:

	# This is the connect function within the class. Given "column", "table" inputs, it will pull the selected information and write the list into the output file "write_name"
	def connectCODIAC(self, column, table, write_name):
		zith_cnx = mysql.connector.connect(user='zithupdate', password='change-999', host='emdac.eol.ucar.edu', database='zith9')
                cursor = zith_cnx.cursor()
		

		# Variables used for special WHERE cases. 
		where_pn = "WHERE active_editor=1 and person_name!='NULL'"
		where_pna = "WHERE person_name!='NULL'"
		where_ona = "WHERE organization_name!='NULL'"
		where_sn= "WHERE short_name!='NULL'"
	
		# Query database with inputs and special WHERE variables when needed.
		if column == "person_name" and table == "contact":
			if write_name == "codiac_contact_id_active":
				query = ("SELECT id, %s FROM %s %s" % (column, table, where_pn))
			elif write_name == "codiac_contact_id_all":
				query = ("SELECT id, IFNULL(%s, organization_name) %s FROM %s %s" % (column, column, table, where_ona))
		elif column == "short_name" and table == "format":
			query = ("SELECT id, %s FROM %s %s" % (column, table, where_sn))
		else:
			query = ("SELECT id, %s FROM %s" % (column, table)) # Default query
		
		cursor.execute(query)
		
		
		# Create a list record and fill with database information
		record = []
		for (id, column) in cursor:
			column = column.replace(":","")
			record.append((id, column))
		
		# Sort list by id number
		record.sort()

		f = open('config/comboBox/%s.yml' % write_name, 'w')

		f.write("%s:\n   1001: EOL Data Support\n " % write_name)

		for (id, column) in record:
	       		f.write("  %d: %s\n " %(id, column.encode("utf-8")))

		cursor.close()
		zith_cnx.close()


	def connectDTS(self, column, table, write_name):
		dts_cnx = mysql.connector.connect(user='dts-update', password='gu3uz3', host='emdac.eol.ucar.edu', database='dmg_dts')
		cursor = dts_cnx.cursor()


		where_ae = "WHERE active_editor=1"

		if column == "contact_short_name":
			if write_name == "dts_contact_id_active":
				query = ("SELECT contact_id, %s FROM %s %s" %(column, table, where_ae))
			elif write_name == "dts_contact_id_all":
				query = ("SELECT contact_id, %s FROM %s" %(column, table))

		elif column == "name":
			query = ("SELECT status_id, %s FROM %s" %(column, table))

		cursor.execute(query)
		
		record = []
                for (id, column) in cursor:
                        column = column.replace(":","")
                        record.append((id, column))

                # Sort list by id number
                record.sort()

		
		f = open('config/comboBox/%s.yml' % write_name, 'w')

		f.write("%s:\n " % write_name)

		for (id, column) in record:
			f.write("  %d: %s\n " % (id, column))
		

		cursor.close()
		dts_cnx.close()


