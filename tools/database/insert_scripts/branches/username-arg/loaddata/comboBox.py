#!/usr/bin/env python

#===============================================================================#
# comboBox.py                                                                   #
#                                                                               #
# comboBox script is a sub module for load_data_proj script 	                #
#                                                                               #
# This script deals with adding, loading, saving and checking comboBox fields   #
# which is a dropdown list widget                                               #
#                                                                               #
# comboBoxes need separate config files that contain the dictionary data for the#
# respective field. Instructions on comboBox config data format can be found    #
# in platform_id.yml file in the config directory.                              #
#                                                                               #
#============================!!!!!IMPORTANT!!!!!================================#
# 										#
# The comboBox config file name and the field name in the yaml dataset must     #
# match EXACTLY. Or else the script will not work properly.                     #
# For example, if the name of the comboBox field is "Platform Id", the entry    #
# in the configYaml must be "platform_id: comboBox" and there must be a config  #
# file named "platform_id.yml" in the ../config/comboBox directory		#
# with the content: 							        #
#                                                                               #
# platform_id:                                                                  #
#      - 2: aircraft                                                            #
#        ...                                                                    #
#                                                                               #
# and the load_data_proj will take care of the rest :)                          #
#										#
#===============================================================================#
#                                                                               #
# Add method, different from other sub-modules, takes in a comboBoxConfigDir    #
# variable which contains the parent directory of where all the comboBox config #
# fiels are. Using the name that was used to call the instance, add method      #
# completes the comboBox config file directory and opens the respective file    #
# to populate the comboBox widget.                                              #
#                                                                               #
# Load method takes the key value from the comboBox field data then searches    #
# the config data for a matching value, then selects the repsective index in    #
# the comboBox field to display the matching comboBox value.                    #
#                                                                               #
# Save method takes the value that's selected from the comboBox widget and      #
# searched through the comboBox config data for the corresponding value, and    #
# saves the respective key value in the yaml dataset                            #
#                                                                               #
# Check method for comboBox fields are (currently) separated into three	        #
# different types.                               								#
#  - check: checks if an item has been selected									#
#  - pointCodiacCheck: checks if point of contact codiac field is "local"	#
#  - qualityCheck: check if quality and version number match			#
#                                                                               #
#===============================================================================#
# created by Soo Park 2015                                                      #
# updated: 4/24/15                                                              #
# copyright: University Corporation for Atmospheric Research, 2015              #
#===============================================================================#

from PyQt4 import QtGui, QtCore
import yaml, sys

class comboBoxField(QtGui.QWidget):
	
    def __init__(self, name, layout, row, scroll):

        # parameters needed for all fields
        super(comboBoxField, self).__init__()
        self.name = name
        self.row = row
        self.layout = layout
        self.scroll = scroll

    def add(self, comboBoxConfigDir, field_toolTip):
	
        # sublayout is needed for combining label and comboBox field
        subLayout = QtGui.QVBoxLayout()
        # the comboBox config file dir
        self.comboBoxConfigDir = comboBoxConfigDir
        # label
        if "codiac" in self.name:
            self.labelName = self.name.replace("_", " ").title()
            self.labelName = self.labelName.replace("Codiac", "CODIAC")
        elif "dts" in self.name:
            self.labelName = self.name.replace("_", " ").title()
            self.labelName = self.name.replace("Dts", "DTS")
	elif "iso" in self.name:
	    self.labelName = self.name.replace("_", " ").title()
            self.labelName = self.labelName.replace("Iso", "ISO")
        else:
            self.labelName = self.name.replace("_", " ").title()

        self.label = QtGui.QLabel(self.scroll)
        self.label.setText(self.labelName)
        subLayout.addWidget(self.label)

        #ComboBox
        self.comboBox = QtGui.QComboBox()
        self.comboBox.setToolTip(field_toolTip)
        self.comboBox.addItem(str(""))
        subLayout.addWidget(self.comboBox)


        #Checks to see if the contact filed block is the 4th or the 5th so that they can be hidden by default, so the user isn't bombarded with empty fields
        if "role_4" in self.name or "role_5" in self.name:
            self.PushButton = QtGui.QPushButton(self.scroll)
            self.PushButton.setText("Click to add Contact.")
            subLayout.addWidget(self.PushButton)

            #If button is not pressed, hide the respective comboBox field
            QtCore.QObject.connect(self.PushButton, QtCore.SIGNAL("toggled(bool)"),
            self.comboBox.setHidden)
            self.PushButton.setFixedWidth(200) #Make the button for the ISO contacts larger
            self.PushButton.setCheckable(True)
            self.PushButton.setChecked(True)
            self.PushButton.setStyleSheet("QPushButton { background-color: rgb(70, 70, 70) }" "QPushButton:checked { background-color: rgb(240, 240, 240) }")

        #Checks to see if the comboBox name represents Datacite type or CODIAC contact ID and if so, makes the button smaller (but still keeps the fields hidden by default
        elif "type_4" in self.name or "type_5" in self.name or "codiac_4" in self.name or "codiac_5" in self.name:
            self.PushButton = QtGui.QPushButton(self.scroll)
            self.PushButton.setText("+")
            subLayout.addWidget(self.PushButton)

            #If button is not pressed, hide the respective comboBox field
            QtCore.QObject.connect(self.PushButton, QtCore.SIGNAL("toggled(bool)"),
            self.comboBox.setHidden)
            self.PushButton.setFixedWidth(30) #Make the button smaller
            self.PushButton.setCheckable(True)
            self.PushButton.setChecked(True)
            self.PushButton.setStyleSheet("QPushButton { background-color: rgb(70, 70, 70) }" "QPushButton:checked { background-color: rgb(240, 240, 240) }")

	else:
	   pass

        # comboBox data
        comboBoxFile = open(self.comboBoxConfigDir, 'r')
        self.comboBoxData = yaml.load(comboBoxFile)
        for key1, value1 in self.comboBoxData.iteritems():
            for key2 in value1:
                self.comboBox.addItem((self.comboBoxData[key1][key2]))
                if self.name == "point_of_contact_id_codiac":
                    self.comboBox.setCurrentIndex(0)
                else:
                    # so when comboBox is added, the default is set to nothing
                    self.comboBox.setCurrentIndex(-1)

        # add sublayout to scroll area layout
        self.layout.addLayout(subLayout, self.row, 0,1,4)

    # load comboBox field data
    def load(self, loadData):
        self.loadData = loadData

        # comboBox index data from the yaml dataset
        target = self.loadData['dataset'][self.row][self.name]

        # if data exists
        if target:
            for key1, value1 in self.comboBoxData.iteritems():
                for key2 in value1:
                    # match current data to config data and extract matching value
                    if target == key2:
                        self.findIndex = self.comboBoxData[key1][key2]
                        index = self.comboBox.findText(self.findIndex)
                        self.comboBox.setCurrentIndex(index)
        # if data doesn't exist, set index to nothing
        elif not target:
            self.comboBox.setCurrentIndex(-1)

    # save QComboBox data
    def save(self, saveData):

        self.saveData = saveData
        # find current what the index is currently set to
        indexSelected = str(self.comboBox.currentText())
        for key1, value1 in self.comboBoxData.iteritems():
            for key2 in value1:
                # save the matching key value to yaml dataset
                if indexSelected == self.comboBoxData[key1][key2]:
                    self.saveData['dataset'][self.row][self.name] = key2
                elif not indexSelected:
                    self.saveData['dataset'][self.row][self.name] = ""

	# clear fields
    def clear(self):
        if self.name == "point_of_contact_id_codiac":
            self.comboBox.setCurrentIndex(0)
        else:
            self.comboBox.setCurrentIndex(-1)

    # checks whether the selection is blank
    def check(self):

        # if the platfrom selection is blank, send to error
        if (self.comboBox.currentIndex() == -1 or str(self.comboBox.currentText()) == "local"):
            return self.labelName

	# check for point of contact codiac
    def pointCodiacCheck(self):
        
        if not self.comboBox.currentText() == "local":
            return self.labelName

	# check for quality. version is read from versionCheck in lineEdit
    def qualityCheck(self, version):

        if version == True: # if final
            if not self.comboBox.currentText() == "final":
                return self.labelName
        elif version == False: # if prelim
            if not self.comboBox.currentText() == "preliminary":
                return self.labelName
        elif version == "Version Number": # if errored from the versionCheck in lineEdit
                return self.labelName

	# checks if empty
    def emptyCheck(self):

        if self.comboBox.currentIndex() == -1:
            return True
