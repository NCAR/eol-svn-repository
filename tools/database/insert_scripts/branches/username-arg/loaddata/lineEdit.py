#!/usr/bin/env python

#===============================================================================#
# lineEdit.py                                                                   #
#                                                                               #
# lineEdit script is a sub module for load_data_proj.py script.                 #
#                                                                               #
# This script deals with adding, loading, saving and checking lineEdit fields   #
#                                                                               #
# add function takes the name from the config yaml file then uses it to create  #
# a label and also a lineEdit field.                                            #
#                                                                               #
# When loading a yaml file, it takes a lineEdit field's dictionary value and    #
# inserts it in the lineEdit widget.                                            #
#                                                                               #
# Save method extracts data from the respective lineEdit fields and stores it   #
# in a dataset                                                                  #
#                                                                               #
# Check method for lineEdit fields are (currently) separated into nine	        #
# different types.                                                              #
#  - check: only checks whether the field is empty or not and if <*> has been 	#
#		  replaced										                        #
#  - arcIdCheck: checks whether the input matches the required pattern          #
#  - str_arcIdCheck: checks for arcId and emptiness	                            #
#  - str_unitCheck: checks for integer then unit of time						#
#  - versionCheck: checks for version number									#
#  - returnVersion: returns the version number (will be used for quality)		#
#  - filelengthCheck: checks if the checkBox for filelength and enddate 		#
#					  have been checked											#
#  - longCheck: checks if long is -180 or 180									#
#  - latCheck: checks if lat is -90 or 90										#
#  - emptyCheck: checks if the field is empty									#
#																				#
# When adding a new lineEdit field, either use the nine of the given checks	    #
# for that field or depending on the field's requirements, create a new check   #
# then add the separate check in load_data_proj script.         	            #
# Instructions for adding a new check in load_data_proj script is found under   #
# loadDataModel class - errorMsgs function with additional instructions         #
#                                                                               #
#===============================================================================#
# created by Soo Park 2015                                                      #
# updated: 4/24/15                                                              #
# copyright: University Corporation for Atmospheric Research, 2015              #
#===============================================================================#

from PyQt4 import QtGui, QtCore
import re

class lineEditField(QtGui.QWidget):
    def __init__(self, name, layout, row, scroll):

        # parameters needed for all fields
        super(lineEditField, self).__init__()
        self.name = name
        self.row = row
        self.layout = layout
        self.scroll = scroll

    def add(self, field_toolTip):

        subLayout = QtGui.QVBoxLayout()

        # takes the name from dataset yaml and replaces all under scores with a space then
        # capitalizes all words for label uses.
        self.labelName = self.name.replace("_", " ").title()
	# name in the yaml is actually the project name, so change label in
	# GUI.
	self.labelName = self.labelName.replace("Name", "PROJECT")

        # a special case where lineEdit and checkBox are conjoined
        if "filelength" in self.name or "enddate_pattern" in self.name:
            self.checkBox = QtGui.QCheckBox(self.scroll)
            self.filelength = True
            self.checkBox.setText(self.labelName)
            subLayout.addWidget(self.checkBox)

            self.lineEdit = QtGui.QLineEdit()
            self.lineEdit.setToolTip(field_toolTip)
            subLayout.addWidget(self.lineEdit)
            self.layout.addLayout(subLayout, self.row, 0,1,4)

			# if checkBox is checked, disable the respective lineEdit field
            QtCore.QObject.connect(self.checkBox, QtCore.SIGNAL("toggled(bool)"), self.lineEdit.setDisabled)


        else:
            # label
            self.label = QtGui.QLabel(self.scroll)
            self.label.setText(self.labelName)
            subLayout.addWidget(self.label)

            # lineEdit
            self.lineEdit = QtGui.QLineEdit()
            self.lineEdit.setToolTip(field_toolTip)
            subLayout.addWidget(self.lineEdit)
            self.layout.addLayout(subLayout, self.row, 0,1,4)



    # load lineEdit field data
    def load(self, loadData):

        self.loadData = loadData
        self.lineEdit.clear()
        # take the respective lineEdit field data from yaml dataset and displays it in QLineEdit
        loadLineData = self.loadData['dataset'][self.row][self.name]
        if loadLineData:
            self.lineEdit.setText(loadLineData)
        else:
            self.lineEdit.setText("")

	# load for filelength/enddate pattern fields
    def line_checkLoad(self, loadData):

        self.loadData = loadData
        self.clear()
        self.checkBox.setChecked(False)

        loadLineData = self.loadData['dataset'][self.row][self.name]

        if not loadLineData:
            self.checkBox.setChecked(True)
            self.lineEdit.setText("")
        else:
            self.checkBox.setChecked(False)
            self.lineEdit.setText(str(self.loadData['dataset'][self.row][self.name]))

	# save lineEdit data
    def save(self, saveData):

        self.saveData = saveData
        # stores the data in lineEdit field in dataset
        lineEditData = str(self.lineEdit.text())

        if lineEditData:
            self.saveData['dataset'][self.row][self.name] = lineEditData
        elif not lineEditData:
            self.saveData['dataset'][self.row][self.name] = ""

	# clear the field in the GUI
    def clear(self):

        self.lineEdit.clear()
        # if filelength/enddate pattern, uncheck checkboxes as well
        if "filelength" in self.name or "enddate_pattern" in self.name:
            self.checkBox.setChecked(False)

	# check if <*> have all been replaced or the field is empty
    def check(self):

        pattern = re.compile("(<.*>)")
        patternCheck = pattern.search(str(self.lineEdit.text()))
        if (not self.lineEdit.text() or patternCheck):

            return self.labelName

    # checks for archive identifiers
    # if the dataset you want have an archive ident number that does not fit in these patterns
    # create a new variable with the pattern then repeat the check steps.
    def arcIdCheck(self):

        # if masking int to the value passes, return error since it needs to be a float
        pattern1 = re.compile("^([0-9][0-9][0-9]\.[0-9][0-9][0-9])$") # "999.999"
        pattern2 = re.compile("^[0-9][0-9][0-9]\.B[0-9][0-9]-[0-9][0-9][0-9]") # "999.B999"

        check1 = pattern1.match(str(self.lineEdit.text()))
        check2 = pattern2.match(str(self.lineEdit.text()))

        if not check1:
            if not check2:
                return self.labelName

    # takes both check and decimal check to check for both
    def str_arcIdCheck(self):

        if (self.arcIdCheck() == self.labelName or not str(self.lineEdit.displayText())):
            return self.labelName

    # checks for integer with units
    def str_unitCheck(self):
	# value must be an integer followed by a case-insensitive frequency of year, month, day,
	# hour, or minute (any of them can be plural)
	pattern = re.compile("^(\d+)\s+(years?|months?|days?|hours?|minutes?|seconds?)$")

        check = pattern.match(str(self.lineEdit.text()))

	if not check:
            return self.labelName

	# checks for the version number
    def versionCheck(self):
        prelimPattern = re.compile("^0.*")
        finalPattern = re.compile("^[1-9].*")

        prelimCheck = prelimPattern.match(str(self.lineEdit.text()))
        finalCheck = finalPattern.match(str(self.lineEdit.text()))

		# if it's prelim
        if prelimCheck:
            return False
        # if it's final
        if finalCheck:
            return True
        # if neither, error
        if not prelimCheck and not finalCheck:
            return self.labelName

	# return version number
    def returnVersion(self):

        return str(self.lineEdit.text())

	# check if the checkBox has been checked
    def filelengthCheck(self):

        if self.checkBox.isChecked():
            return self.labelName

	# check for long
    def longCheck(self):

        long = str(self.lineEdit.text())
        if self.lineEdit.text():
            if (long == "-180" or long == "180"):
                return self.labelName
        else:
            return self.labelName

	# check for lat
    def latCheck(self):

        lat = str(self.lineEdit.text())
        if self.lineEdit.text():
            if (lat == "-90" or lat == "90"):
                return self.labelName
        else:
            return self.labelName

	# check if empty
    def emptyCheck(self):

        if not self.lineEdit.text():
            return True
