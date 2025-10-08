#!/usr/bin/env python

# =========================================================================== #
# comboBox.py                                                                 #
#                                                                             #
# comboBox script is a sub module for load_data_proj script                   #
#                                                                             #
# This script deals with adding, loading, saving and checking comboBox fields #
# which is a dropdown list widget                                             #
#                                                                             #
# comboBoxes need separate config files that contain the dictionary data for  #
# the respective field. Instructions on comboBox config data format can be    #
# found in platform_id.yml file in the config directory.                      #
#                                                                             #
# ===========================!!!!!IMPORTANT!!!!!============================= #
#                                                                             #
# The comboBox config file name and the field name in the yaml dataset must   #
# match EXACTLY. Or else the script will not work properly.                   #
# For example, if the name of the comboBox field is "Platform Id", the entry  #
# in the configYaml must be "platform_id: comboBox" and there must be a       #
# config file named "platform_id.yml" in the ../config/comboBox directory     #
# with the content:                                                           #
#                                                                             #
# platform_id:                                                                #
#      - 2: aircraft                                                          #
#        ...                                                                  #
#                                                                             #
# and the load_data_proj will take care of the rest :)                        #
#                                                                             #
# =========================================================================== #
#                                                                             #
# Add method, different from other sub-modules, takes in a comboBoxConfigDir  #
# variable which contains the parent directory of where all the comboBox      #
# config files are. Using the name that was used to call the instance, add    #
# method completes the comboBox config file directory and opens the           #
# respective file to populate the comboBox widget.                            #
#                                                                             #
# Load method takes the key value from the comboBox field data then searches  #
# the config data for a matching value, then selects the repsective index in  #
# the comboBox field to display the matching comboBox value.                  #
#                                                                             #
# Save method takes the value that's selected from the comboBox widget and    #
# searched through the comboBox config data for the corresponding value, and  #
# saves the respective key value in the yaml dataset                          #
#                                                                             #
# Check method for comboBox fields are (currently) separated into three       #
# different types.                                                            #
#  - check: checks if an item has been selected                               #
#  - pointCodiacCheck: checks if point of contact codiac field is "local"     #
#  - qualityCheck: check if quality and version number match                  #
#                                                                             #
# =========================================================================== #
# created by Soo Park 2015                                                    #
# updated: 4/24/15                                                            #
# Ported to Python3 and PyQt5: 3/13/2021                                      #
# copyright: University Corporation for Atmospheric Research, 2015            #
# =========================================================================== #

from PyQt5.QtWidgets import QWidget, QVBoxLayout, QLabel, QComboBox, \
    QPushButton, QCompleter, QMessageBox
from PyQt5.QtCore import Qt
import re
from zith9 import ZITH9tables


class comboBoxField(QWidget):

    def __init__(self, name, layout, row, scroll):

        # parameters needed for all fields
        super(comboBoxField, self).__init__()
        self.name = name
        self.row = row
        self.layout = layout
        self.scroll = scroll

        self.tables = ZITH9tables

    def add(self, comboBoxConfigDir, field_toolTip):

        # sublayout is needed for combining label and comboBox field
        subLayout = QVBoxLayout()
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

        self.label = QLabel(self.scroll)
        self.label.setText(self.labelName)
        subLayout.addWidget(self.label)

        # ComboBox
        self.comboBox = QComboBox()
        self.comboBox.setToolTip(field_toolTip)
        self.comboBox.addItem(str(""))
        # Implement autocompletion on typing. Typed chars are matched if they
        # exist anyplace in the string, case insenstive.
        self.comboBox.setEditable(True)  # must be editable for autocomplete
        self.comboBox.completer().setCompletionMode(
                                      QCompleter.UnfilteredPopupCompletion)
        self.comboBox.completer().setFilterMode(Qt.MatchContains)
        subLayout.addWidget(self.comboBox)

        # Checks to see if the contact filed block is the 4th or the 5th so
        # that they can be hidden by default, so the user isn't bombarded with
        # empty fields
        if "role_3" in self.name or "role_4" in self.name or \
                "role_5" in self.name:
            self.PushButton = QPushButton(self.scroll)
            self.PushButton.setText("Click to add Contact.")
            subLayout.addWidget(self.PushButton)

            # If button is not pressed, hide the respective comboBox field
            self.PushButton.toggled.connect(self.comboBox.setHidden)
            # Make button for ISO contacts larger
            self.PushButton.setFixedWidth(200)
            self.PushButton.setCheckable(True)
            self.PushButton.setChecked(True)
            self.PushButton.setStyleSheet("QPushButton { background-color: \
                rgb(70, 70, 70) }" "QPushButton:checked { background-color: \
                rgb(240, 240, 240) }")

        # Checks to see if the comboBox name represents Datacite type or
        # CODIAC contact ID and if so, makes the button smaller (but still
        # keeps the fields hidden by default
        elif "type_3" in self.name or "type_4" in self.name or \
                "type_5" in self.name or "codiac_3" in self.name or \
                "codiac_4" in self.name or "codiac_5" in self.name:
            self.PushButton = QPushButton(self.scroll)
            self.PushButton.setText("+")
            subLayout.addWidget(self.PushButton)

            # If button is not pressed, hide the respective comboBox field
            self.PushButton.toggled.connect(self.comboBox.setHidden)
            self.PushButton.setFixedWidth(30)  # Make the button smaller
            self.PushButton.setCheckable(True)
            self.PushButton.setChecked(True)
            self.PushButton.setStyleSheet("QPushButton { background-color: \
                rgb(70, 70, 70) }" "QPushButton:checked { background-color: \
                rgb(240, 240, 240) }")

        else:
            pass

        # Load comboBox data from zith9 dictionary
        keys = ['host', 'datacite_contributor_type', 'iso_citation_role',
                'quality', 'purpose', 'spatial_type', 'format',
                'category_id', 'frequency_id', 'platform_id', 'instrument_id',
                'note_type_id']
        for key in keys:
            if re.match(r".*"+key+".*", self.name):
                self.comboBoxData = self.readOptions(key)

        keys = ['ingest_status_id_dts', 'load_status_id_dts',
                'approve_status_id_dts']
        for key in keys:
            if re.match(r".*"+key+".*", self.name):
                self.comboBoxData = self.readOptions('dts_status_id')

        keys = ['ingest_contact_id_dts', 'load_contact_id_dts',
                'approve_contact_id_dts', 'source_contact_id_dts',
                'internal_contact_id_dts', 'author_id_dts']
        for key in keys:
            if re.match(r".*"+key+".*", self.name):
                self.comboBoxData = self.readOptions('dts_contact_id_active')

        keys = ['source_contact_id_dts', 'dts_contact_id_all']
        for key in keys:
            if re.match(r".*"+key+".*", self.name):
                self.comboBoxData = self.readOptions('dts_contact_id_all')

        keys = ['point_of_contact_id_codiac', 'contact_id_codiac',
                'codiac_contact_id_all']
        for key in keys:
            if re.match(r".*"+key+".*", self.name):
                self.comboBoxData = self.readOptions('codiac_contact_id_all')

        keys = ['internal_contact_id_codiac',
                'codiac_contact_id_active']
        for key in keys:
            if re.match(r".*"+key+".*", self.name):
                self.comboBoxData = self.readOptions('codiac_contact_id_active')

        # add sublayout to scroll area layout
        self.layout.addLayout(subLayout, self.row, 0, 1, 4)

    # load comboBox field data
    def load(self, loadData):
        self.loadData = loadData

        # comboBox index data from the yaml dataset
        target = self.loadData['dataset'][self.row][self.name]

        # if data exists in the YAML file (line is not empty)
        if target:
            # Translate descriptive name from YAML file to table name in DTS
            if self.name in ['ingest_status_id_dts', 'load_status_id_dts',
                             'approve_status_id_dts']:
                key1 = 'dts_status_id'
            elif self.name in ['ingest_contact_id_dts',
                               'load_contact_id_dts', 'approve_contact_id_dts',
                               'internal_contact_id_dts', 'author_id_dts']:
                key1 = 'dts_contact_id_active'
            elif self.name in ['source_contact_id_dts', 'dts_contact_id_all']:
                key1 = 'dts_contact_id_all'
            elif self.name in ['point_of_contact_id_codiac',
                               'contact_id_codiac', 'codiac_contact_id_all']:
                key1 = 'codiac_contact_id_all'
            elif self.name in ['internal_contact_id_codiac',
                               'codiac_contact_id_active']:
                key1 = 'codiac_contact_id_active'
            elif re.match(r'.*_2$', self.name):
                key1 = self.name.replace('_2', '')
                if key1 in ['contact_id_codiac']:
                    key1 = 'codiac_contact_id_all'
            elif re.match(r'.*_3$', self.name):
                key1 = self.name.replace('_3', '')
                if key1 in ['contact_id_codiac']:
                    key1 = 'codiac_contact_id_all'
            elif re.match(r'.*_4$', self.name):
                key1 = self.name.replace('_4', '')
                if key1 in ['contact_id_codiac']:
                    key1 = 'codiac_contact_id_all'
            elif re.match(r'.*_5$', self.name):
                key1 = self.name.replace('_5', '')
                if key1 in ['contact_id_codiac']:
                    key1 = 'codiac_contact_id_all'
            else:
                key1 = self.name


            # If the requested target value doesn't exists in the database,
            # remove target value and prompt user to fix it.
            try:
                self.findIndex = self.comboBoxData[key1][target]
                index = self.comboBox.findText(self.findIndex)
                self.comboBox.setCurrentIndex(index)
            except Exception as e:
                genericErrorMsg = "Value " + str(target) + " is not a valid " + \
                                  "value for field " + key1 + ". Value " + \
                                  "has been removed. Please fix in the " + \
                                  "loaded YAML file and reload the GUI.\n"
                ErrorBox = QMessageBox.question(
                           self, "Warning", genericErrorMsg +
                           "\n\n===================================\n\n",
                           QMessageBox.Ok)
                self.comboBox.setCurrentIndex(-1)

        # if data doesn't exist, set index to nothing
        elif not target:
            self.comboBox.setCurrentIndex(-1)

    # save QComboBox data
    def save(self, saveData):

        self.saveData = saveData
        # find current what the index is currently set to
        indexSelected = str(self.comboBox.currentText())
        for key1, value1 in self.comboBoxData.items():
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
        if (self.comboBox.currentIndex() == -1 or
                str(self.comboBox.currentText()) == "local"):
            return self.labelName

    # check for point of contact codiac
    def pointCodiacCheck(self):

        if not self.comboBox.currentText() == "local":
            return self.labelName

    # check for quality. version is read from versionCheck in lineEdit
    def qualityCheck(self, version):

        if version is True:  # if final
            if not self.comboBox.currentText() == "final":
                return self.labelName
        elif version is False:  # if prelim
            if not self.comboBox.currentText() == "preliminary":
                return self.labelName
        # if errored from the versionCheck in lineEdit
        elif version == "Version Number":
            return self.labelName

    # checks if empty
    def emptyCheck(self):

        if self.comboBox.currentIndex() == -1:
            return True

    # Read in attributes from a given database table and add to comboBox
    def readOptions(self, attribute):
        for key in self.tables[attribute]:
            self.comboBox.addItem((self.tables[attribute][key]))  # Add values
            if self.name == "point_of_contact_id_codiac":
                self.comboBox.setCurrentIndex(0)
            else:
                # so when comboBox is added, the default is set to nothing
                self.comboBox.setCurrentIndex(-1)

        return(self.tables)
