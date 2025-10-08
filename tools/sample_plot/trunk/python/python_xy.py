# Authorship: NCAR Earth Observing Laboratory Data Management & Services Group
# Contact: eol-archive@ucar.edu
#
# Licensing: The code associated with this document is provided freely and openly. 
# Users are hereby granted a license to access and use this code, unless otherwise 
# stated, subject to the terms and conditions of the GNU Affero General Public 
# License 3.0 (AGPL-3.0; https://www.gnu.org/licenses/agpl-3.0.en.html). This 
# documentation and associated code are provided "as is" and are not supported. 
# By using or downloading this code, the user agrees to the terms and conditions 
# set forth in this code and in the "Using Python to View OPeNDAP Files" document.
#
# Acknowledgment: This work was sponsored by the National Science Foundation. 
# This material is based upon work supported by the National Center for Atmospheric 
# Research, a major facility sponsored by the National Science Foundation and managed 
# by the University Corporation for Atmospheric Research. Any opinions, findings 
# and conclusions or recommendations expressed in this material do not necessarily 
# reflect the views of the National Science Foundation.
#
# To run this script, use the following command:
# 		python3 python_xy.py {OPeNDAP link}
# where {OPeNDAP link} is the OPeNDAP link to the desired dataset.
#
# NOTE: This script requires the following packages to be installed:
# 		python3
#		pydap
#		matplotlib
#		numpy
#
# Refer to the "Using Python to Plot OPeNDAP Files" document for more information on the script.
# For questions about Python scripts and other features, refer to the Python help page at
# https://www.python.org/about/help/
# 
# The python_xy.py script is used for visualization of the data 
# file. The script takes in an OPeNDAP file. The user puts in the OPeNDAP file link
# upon calling the script and can then choose variables to plot from the file.
# The script will plot the data in 2d plots for a given location.

# This script first reads in the OPeNDAP file through a URL.
# The script promts the user for the desired variables to plot, stations to plot,
# and graph labels. The script then finds all of the desired information for the 
# given location and plots the information. The 2D plots will be saved in the 
# directory the script was called from and are *.png files.

# The script uses the following hard-coded entities that may need to be changed:
# 	missing_value: This is a string containing the value used for missing data points
# 		e.g. missing_value = "-999.99"
#	var_index: This is the number of variables preceding the variables available to plot
#		against time. This includes any variables containing station information, time,
#		and location.
#	formatData: This is a string containing format information to convert strings to the 
#		python object datetime.
#	variable names: This script assumes that the variable names from the OPeNDAP file
#		are "date", "time", "network-name", "station-name"
#	Graph elements:
#		graph size: This is hard-coded into the figure declaration and declares the size of
#			the output graph.
#		yticks: The graphs are formatted so that the y-ticks are in steps of 3 from the
#			minimum value to the maximum value.
#		x-label: The graphs will be produced with the label "Date and Time" on the x-axis
#		title: The plot title is set to be the variable(s), date range, and location name
#		file name: Similarly to the plot title, the file name to be saved includes the date
#			range, station name, and variable names
# To change any of these values, simply search "HARD-CODED" in this script 
#
# July 2022: Changed getVars and getStations functions so they were not recursive
#            Modified the plotting to have a datetimes structure for each variable
#            Changed the title of the plot to have one line for each variable being plotted
#

# Import necessary packages: 
from pydap.client import open_url
import sys
from datetime import datetime
import matplotlib.pyplot as plt
import numpy as np


# Functions:

# getVars() function prompts the user for the desired variable(s) to plot, and collects the
# information for later use.

def getVars():
	error = False
# Prompt the user for desired variables and collect entries 
	while True:
		vars = input("\nWhich variables would you like to plot against time? To see a list of possible variables, enter 'list'. Please note that not entering the variable name as it appears within the file or OPeNDAP webform will result in errors.\n\n").split(',')

# Allow the user to view a list of all the possible variables to plot
		if(vars[0] == "list"): 
			print("\nThis file has the following variables available to plot against time: " + '\n\n')

# Print the variables excluding the date, time, and location variables
			print(list(dataset.keys())[var_index:]) 
			vars = input("\n\nEnter variable(s) to plot separated by commas:\n").split(',')

# Check the validity of the user-entered variables by cross-referencing the list of possible variables 
		for i in vars:
# Get rid of any excess white space
			i = i.strip()

			if i not in dataset.keys():
# Once invalid variable is found, print the invalid variable 
				print("Invalid variable entered: " + i + "\n")
				error = True
		if error:
	   		print("Please enter your variables again.\n")
	   		error = False
	   		continue
		else:
	   		break

# Return list of desired variables
	return vars


# getStations() function prompts the user for the location(s) (aka stations) at which
# to produce plots and stores that information for later use.

def getStations():
	error = False
# Compile a list of unique stations available to create plots at by combining the 
# network name and station name.
	networkId = toArr("network_name") # HARD-CODED
	stationId = toArr("platform_name") # HARD-CODED
	
# Create a list to store the occurence of all the stations in the data file
	stations = []

# Create a list of each unique stations to present to the user
	uniqStations = []

# Make a dictionary that will store the station name as the key, and a list of
# indices at which that station occurs.
	stationList = {} 

# Create a list to store user input
	usrInput = []

# Loop through lists of networks and stations to create a string combining each
# network station pair.
	for i in range(len(networkId)):
		stationStr = networkId[i] + '-' + stationId[i]
		stations.append(stationStr)
		
# Add the pair of network and station to the unique list if it is not already added.
		if not stationStr in uniqStations:
			uniqStations.append(stationStr)

# Prompt the user for the desired station(s) and collect their response.
	while True:
		usrInput = input("\nWhich stations would you like to create plots for? Enter station name(s) separated by commas. The stations are named with the following convention: \n\nnetwork_name-platform_name\n\nTo see a list of possible stations, enter 'list'. Please note that not entering the station name as it appears within the file will result in errors.\n\n").split(',')

# Allow the user to view a list of all the possible stations over which to plot
		if usrInput[0] == "list":
			print("\nThe following stations are available to create plots for: " + '\n\n')
			uniqStations.sort()	
			print(list(uniqStations))
			usrInput = input("\n\nEnter stations(s) to plot separated by commas:\n").split(',')

# Check the validity of the user's entries by crossing reference the list of unique stations
		for i in usrInput:
# Delete any excess white space
			i = i.strip()

# If an entry is not in the list of unique stations, print the invalid entry and start this
# section over. 
			if i not in uniqStations:
				print("Invalid station entered: " + i + "\n")
				error = True

# If the station is valid, create a key in the dictionary with the station name pointing to
# an empty list
			else:
				stationList[i] = []
		if error:
			print("Please try again:\n")
			error = False
			continue
		else:
			break
			

# Loop through each occurence of the desired station(s) and note the indices in order
# to retrieve the relevant data points later on.
	for i in range(len(stations)):
		if stations[i] in stationList:
			stationList[stations[i]].append(i)

# Return dictionary of desired stations with list of relevant indices. 
	return stationList 


# toArr(var) function takes the name of a variable as seen in the OPeNDAP file and uses
# the pydap client to retrieve an array of the values of that variable.
def toArr(var):
	commandStr = "enumerate(dataset."+var+".iterdata())"
	arr = []
	for i in eval(commandStr): arr.append(str(i[1]))
	return arr

# Main:

#HARD-CODED
missing = "-999.99"
var_index = 9
formatData = "%Y/%m/%d%H:%M:%S"

# Pull OPeNDAP file link from command line argument 
url = sys.argv[1]

# Import dataset using the pydap client and specify QCF for NCAR/EOL datasets 
dataset = open_url(url).QCF

# Collect desired variables and stations
vars = getVars()
relevantStations = getStations()

# Collect lists of times and dates from the dataset 
times = toArr("time") # HARD-CODED
dates = toArr("date") # HARD-CODED

# Create a dictionary with the desired variable name(s) as the key(s)
# and an empty list that will eventually hold the relevant variable values
varValues = dict.fromkeys(vars, [])

# Grab data values for every desired variable 
for i in range(len(vars)):
	varValues[vars[i]] = toArr(vars[i])

# Collect user input for the label on the y axis
# e.g. "Degrees (celsius)
y_label = input("\n\nWhat would you like the label to be on the y-axis?\n")

print("\nCreating plots...\n\n")

# Loop through each desired station to create plots
for station in relevantStations.keys():
# Create an empty dictionary to hold the python object datetime
	datetimes = {}
# Create an empty dictionary to eventually hold the relevant data points
	values = {}

# Loop through the list of indices at the desired station and use those indices
# to pull the data points from that station
	for i in relevantStations[station]:

# Loop through desired variables to collect values
		for var in varValues.keys():
# On first pass, add variable name as key to values and datetimes arrays
			if not var in values.keys():
				values[var]= []
				datetimes[var] = []
			
# Check that variable value isn't missing before adding it to the list of values
			if not varValues[var][i] == missing:
				values[var].append(float(varValues[var][i]))
# Add datetime objects to datetimes list at relevant indices
				datetimes[var].append(datetime.strptime(dates[i]+times[i], formatData))

# Begin plotting
# Create figure to hold graph
	fig = plt.figure(figsize=(20,10)) # HARD-CODED
	
# Create place holders to store the graph's minimum and maximum values
	max_value = 0
	min_value = 100

# Collect minimum and maximum values for each variable, then plot line
# of each variable
	for var in values.keys():
		if max(values[var]) > max_value: max_value = max(values[var])
		if min(values[var]) < min_value: min_value = min(values[var])	
		plt.plot(datetimes[var], values[var], marker = 'o', label= var)

# Set x label, y label, and plot title
	plt.xlabel("Date and Time")
	plt.ylabel(y_label)

# Convert variable names to include spaces instead of underscores
# Print a title line for each variable with the date/time range
# Each variable can have a different date/time range because of missing values
	title_str = ""
	for var in values.keys(): 
		vars_str = var.strip().replace("_", " ").title() 
		title_str += vars_str + ' from ' + datetimes[var][0].strftime("%Y/%m/%d %H:%M")  + ' to ' + datetimes[var][len(datetimes[var])-1].strftime("%Y/%m/%d %H:%M") + ' UTC for ' + station + "\n"
	title_str = title_str[0:len(title_str)-1] 
	plt.title(title_str)

# Add legend to plot
	plt.legend()

# Add y ticks and x ticks to plot in addition to formatting
	plt.yticks(np.arange(min_value, max_value, step=3))  # Set label locations. #HARDCODED
	plt.xticks(rotation=45)

# Save plot to current directory with name of station, variables, and start date
	fileStr = station + '_'
	for var in values.keys():
		fileStr += var.strip() + '_'
	fileStr += datetimes[var][0].strftime("%Y%m%d%H%M") + '.png'
	fig.savefig(fileStr)

# Close figure
	plt.close(fig)

# Print the name of the plot and where it is saved.
	print('Plot saved in the current directory with the name ' + fileStr)
