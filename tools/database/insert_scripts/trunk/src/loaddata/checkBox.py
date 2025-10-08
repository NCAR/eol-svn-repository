#!/usr/bin/env python

# =========================================================================== #
# checkBox.py                                                                 #
#                                                                             #
# checkBox script is a submodule for load_data_prj script                     #
#                                                                             #
# This script deals with adding, loading, saving and checking checkBox fields #
#                                                                             #
# Add method takes the name from the yaml dataset then uses it to create a    #
# checkBox field. checkBox fields by default include a label therefore a      #
# separate label or a sublayout to combine the two are not needed.            #
#                                                                             #
# Load method takes the comboBox field's dictionary value and checks the box  #
# if the value is "1" and unchecks if "0"                                     #
#                                                                             #
# Save method checks whether the checkBox has been checked and stores a "1"   #
# if checked and a "0" if not checked.                                        #
#                                                                             #
# Because there is no way for the code to check whether the entered entry was #
# user selected or default, checking method for this field is not used and    #
# simply passed. (With the exception of filelength and enddate pattern which  #
# are checked in the lineEdit module and not the checkBox module)             #
#                                                                             #
# =========================================================================== #
# Created by Soo Park 2015                                                    #
# updated: 4/24/15                                                            #
# Ported to Python3 and PyQt5: 3/13/2021                                      #
# copyright: University Corporation for Atmospheric Research, 2015            #
# =========================================================================== #

from PyQt5.QtWidgets import QWidget, QCheckBox


class checkBoxField(QWidget):

    def __init__(self, name, layout, row, scroll):

        # parameters needed for all fields
        super(checkBoxField, self).__init__()
        self.name = name
        self.row = row
        self.layout = layout
        self.scroll = scroll

    def add(self, field_toolTip):

        # sublayout or a label isn't needed for a checkBox field
        self.labelName = self.name.replace("_", " ").title()
        self.checkBox = QCheckBox(self.scroll)
        self.checkBox.setToolTip(field_toolTip)
        self.checkBox.setText(self.labelName)
        self.layout.addWidget(self.checkBox, self.row, 0, 1, 4)

    # load checkBox field data
    def load(self, loadData):

        self.clear()
        self.loadData = loadData

        target = self.loadData['dataset'][self.row][self.name]
        if target == 1:
            self.checkBox.setChecked(True)
        else:  # so default is a "not"
            self.checkBox.setChecked(False)

    # save QCheckBox data
    def save(self, saveData):

        self.saveData = saveData
        # checking to see whether the box is checked or not and saving
        # different values depending
        if self.checkBox.isChecked():
            self.saveData['dataset'][self.row][self.name] = 1
        else:
            self.saveData['dataset'][self.row][self.name] = 0

    def clear(self):
        self.checkBox.setChecked(False)

    # checkBox fields cannot be checked for validity
    def check(self):
        pass

    # check if empty
    def emptyCheck(self):
        return True
