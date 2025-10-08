#!/usr/bin/env python

# =========================================================================== #
# plainTextEdit.py                                                            #
#                                                                             #
# plainTextEdit script is a submodule for load_data_proj script               #
#                                                                             #
# This script deals with adding, loading, saving and checking plainTextEdit   #
# fields which is for paragraph long text data                                #
#                                                                             #
# Add method takes the name from the yaml dataset then uses it to create a    #
# label and also a plainTextEdit field                                        #
#                                                                             #
# Load method takes the plainTextEdit field's dictionary value and inserts it #
# in the plainTextEdit widget                                                 #
#                                                                             #
# Save method extracts data from the plainEditText field then stores it in a  #
# dataset                                                                     #
#                                                                             #
# Check method for plainTextEdit checks for whether the field is empty or not #
#                                                                             #
# =========================================================================== #
# created by Soo Park 2015                                                    #
# updated: 4/24/15                                                            #
# Ported to Python3 and PyQt5: 3/13/2021                                      #
# copyright: University Corporation for Atmospheric Research, 2015            #
# =========================================================================== #

from PyQt5.QtWidgets import QWidget, QVBoxLayout, QLabel, QPlainTextEdit
import re


class plainTextEditField(QWidget):
    def __init__(self, name, layout, row, scroll):

        # parameters needed for all fields
        super(plainTextEditField, self).__init__()
        self.name = name
        self.row = row
        self.layout = layout
        self.scroll = scroll

    # adds a plainTextEdit field
    def add(self, field_toolTip):

        # sublayout is needed for combining label and plainTextEdit field
        # label
        subLayout = QVBoxLayout()
        subLayout.setSpacing(0)
        self.labelName = self.name.replace("_", " ").title()
        self.label = QLabel(self.scroll)
        self.label.setText(self.labelName)
        subLayout.addWidget(self.label)

        # plainTextEdit
        self.plainTextEdit = QPlainTextEdit()
        self.plainTextEdit.setToolTip(field_toolTip)
        subLayout.addWidget(self.plainTextEdit)
        self.layout.addLayout(subLayout, self.row, 0, 1, 4)

    # load plainTextEdit data
    def load(self, loadData):

        self.loadData = loadData
        # clear whatever was in the widget before
        self.clear()

        loadPlainData = self.loadData['dataset'][self.row][self.name]
        if loadPlainData:
            self.plainTextEdit.appendPlainText(
                    str(self.loadData['dataset'][self.row][self.name]))
        else:
            self.plainTextEdit.appendPlainText("")

    # save QPlainTextEdit data
    def save(self, saveData):

        self.saveData = saveData
        savePlainData = self.plainTextEdit.toPlainText()
        if savePlainData:
            self.saveData['dataset'][self.row][self.name] = \
                    str(self.plainTextEdit.toPlainText())
        else:
            self.saveData['dataset'][self.row][self.name] == ""

    def clear(self):
        self.plainTextEdit.clear()

    # check whether the plainTextEdit widget is empty or have replaced all <*>
    def check(self):
        pattern = re.compile("(<.*>)")
        patternCheck = pattern.search(str(self.plainTextEdit.toPlainText()))
        if (not str(self.plainTextEdit.toPlainText()) or patternCheck):
            return self.labelName

    # checks if empty
    def emptyCheck(self):
        if not self.plainTextEdit.toPlainText():
            return True
