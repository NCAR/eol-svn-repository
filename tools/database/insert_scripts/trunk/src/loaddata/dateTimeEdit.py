#!/usr/bin/env/ python

# =========================================================================== #
# dateTimeEdit.py                                                             #
#                                                                             #
# dateTimeEdit script is a sub module for load_data_proj script               #
#                                                                             #
# This script deals with adding, loading, saving and checking dateTimeEdit    #
# fields.                                                                     #
#                                                                             #
# Add method takes the name of the field from the yaml dataset then uses      #
# it to create a label and also a dateTimeEdit field.                         #
#                                                                             #
# Load method takes the dateTimeEdit field's dictionary value and inserts     #
# it as a QDateTime data to QDateTimeEdit widget                              #
#                                                                             #
# Save method takes the QDateTime data from QDateTimeEdit widget and          #
# stores it in the dataset as a string                                        #
#                                                                             #
# Check method checks whether the QDateTimeEdit widget's data has been        #
# changed from its default min/max QDateTime data.                            #
# This is subject to change if min/max data will be used as an actual         #
# date for datasets                                                           #
#                                                                             #
# =========================================================================== #
# created by Soo Park 2015                                                    #
# updated: 4/23/15                                                            #
# ported to Python3 and PyQt5: 3/13/2021                                      #
# copyright: University Corporation for Atmospheric Research, 2015            #
# =========================================================================== #

from PyQt5 import QtCore
from PyQt5.QtWidgets import QWidget, QVBoxLayout, QLabel, QDateTimeEdit


class dateTimeEditField(QWidget):
    def __init__(self, name, layout, row, scroll):
        super(dateTimeEditField, self).__init__()

        # parameters needed for all fields
        self.name = name
        self.layout = layout
        self.row = row
        self.scroll = scroll

        self.dateDict = {}

    # add QDateTimeEdit widgets
    def add(self, field_toolTip):

        # secondary layout is needed to combine label and widget
        # label
        subLayout = QVBoxLayout()
        subLayout.setSpacing(0)
        self.labelName = self.name.replace("_", " ").title()
        self.label = QLabel(self.scroll)
        self.label.setText(self.labelName)
        subLayout.addWidget(self.label)

        # dateTimeEdit
        self.dateTimeEdit = QDateTimeEdit()
        self.dateTimeEdit.setToolTip(field_toolTip)
        subLayout.addWidget(self.dateTimeEdit)
        self.layout.addLayout(subLayout, self.row, 0, 1, 4)

        # change the display format so it is for ex: 2014/11/14 11:14:02
        self.dateTimeEdit.setDisplayFormat("yyyy-MM-dd HH:mm:ss")

        # the very earliest date that is valid by QDateTime and set it as
        # min time
        self.minDateTime = QtCore.QDateTime(1000, 1, 1, 00, 00, 00)
        self.dateTimeEdit.setMinimumDateTime(self.minDateTime)
        dateTimeEditField.dateTimeEdit = self.dateTimeEdit

        # the very latest date that is valid by QDateTime and set it as
        # max time
        self.maxDateTime = QtCore.QDateTime(7999, 12, 31, 23, 59, 59)
        self.dateTimeEdit.setMaximumDateTime(self.maxDateTime)

        # for some reason maxDateTime is not being set correctly. Can
        # investigate further if crucial
        if self.name == "end_date":
            self.dateTimeEdit.setDateTime(self.maxDateTime)
        else:
            self.dateTimeEdit.setDateTime(self.minDateTime)

    # load dateTimeEdit data
    def load(self, loadData):

        self.loadData = loadData

        # convert string dateTimeEdit data to QDateTime data then insert it to
        # QDateTimeEdit widget
        target = self.loadData['dataset'][self.row][self.name]
        if not target:
            self.clear()
        else:
            self.dateTime = QtCore.QDateTime.fromString(
                    str(self.loadData['dataset'][self.row][self.name]),
                    "yyyy-MM-dd HH:mm:ss")
            self.dateTimeEdit.setDateTime(self.dateTime)

    # save QDateTimeEdit data
    def save(self, saveData):

        self.saveData = saveData
        dateTimeEditField.saveData = saveData

        # turn QDateTimeEdit data to dateTime.() data then string then save
        # to dataset
        self.dateTimeWidget = str(self.dateTimeEdit.dateTime().toString(
            "yyyy-MM-dd HH:mm:ss"))
        self.saveData['dataset'][self.row][self.name] = self.dateTimeWidget

    def clear(self):
        if self.name == "end_date":
            self.dateTimeEdit.setDateTime(self.maxDateTime)
        else:
            self.dateTimeEdit.setDateTime(self.minDateTime)

    # check QDateTimeEdit widget
    def check(self):

        # it's checking to see whether the date has been changed from the
        # default values of min/max dates
        # I remember datasets that used the "beginning/end of time" as their
        # actual dates...
        # if begin date is still min date, send to error
        if str(self.dateTimeEdit.dateTime().toString("yyyy-MM-dd HH:mm")) \
                == self.minDateTime.toString("yyyy-MM-dd HH:mm"):
            return self.labelName
        # if end date is still max date, send to error
        elif str(self.dateTimeEdit.dateTime().toString("yyyy-MM-dd HH:mm")) \
                == self.maxDateTime.toString("yyyy-MM-dd HH:mm"):
            return self.labelName

    # date range checking
    def dateTimeVar(self):

        if self.name == "begin_date":
            begin = self.dateTimeEdit.dateTime()
            return begin
        elif self.name == "end_date":
            end = self.dateTimeEdit.dateTime()
            return end

    # check if empty
    def emptyCheck(self):

        return True
