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
#	   python3 python_xyz.py {OPeNDAP link}
# where {OPeNDAP link} is the OPeNDAP link to the desired dataset.
#
# NOTE: This script requires the following packages to be installed:
#	   python3
#	   pydap
#	   matplotlib
#	   numpy
#
# Refer to the "Using Python to View OPeNDAP Files" document for more information on the script.
# For questions about Python scripts and other features, refer to the Python help page at
# https://www.python.org/about/help/
# 
# The python_xyz.py script is used for visualization of the data file. 
# The script takes in an OPeNDAP file. The user puts in the OPeNDAP file link
# upon calling the script and can then choose a variable to plot and a date/time
# from the file. The script will plot the data in 3d plots for a given location.

# This script first reads in the OPeNDAP file through a URL.
# The script promts the user for the desired variable to plot, date/time to plot,
# and graph labels. The script then finds all of the desired information for the 
# given time and plots the information over all locations in the file. 
# The 3D plots will be saved in the directory the script was called from and are *.png files.

# The script uses the following hard-coded entities that may need to be changed:
#   missing_value: This is a string containing the value used for missing data points
#	   e.g. missing_value = "-999.99"
#   var_index: This is the number of variables preceding the variables available to plot
#	   against time. This includes any variables containing station information, time,
#	   and location.
#   lines: This is a boolean value that determines whether the data points with a line
#          down to the xy-plane will be plotted
#   formatData: This is a string containing format information to convert strings to the 
#	   python object datetime.
#   project_name: The name of the dataset's project for the title of the plots
#   variable names: This script assumes that the variable names from the OPeNDAP file
#	   are "date", "time", "network-name", "station-name"
#   Graph elements:
#	   graph size: This is hard-coded into the figure declaration and declares the size of
#		   the output graph.
#	   title: The plot title is set to be the variable(s), date range, and location name
#	   file name: Similarly to the plot title, the file name to be saved includes the date
#		   range, station name, and variable names
# To change any of these values, simply search "HARD-CODED" in this script 
#
# August 2022: Changed getVars and getTimes functions so they were not recursive
#              Modified the plotting to verify there are 3 data points before
#                 adding the datetime to the list of available times to plot
#              Added user selection to plot either nominal time or actual time
#

# Import neccessary packages 
from pydap.client import open_url
import sys
from datetime import datetime
import matplotlib.pyplot as plt
import numpy as np
from matplotlib import cm
from mpl_toolkits.mplot3d import Axes3D

# Functions:

# getVar() function prompts the user for the desired variable to plot, and collects the
# information for later use.
def getVar():
# Prompt the user for desired variables and collect entries
	while True:
		var = input("\nWhich variable would you like to plot against space? To see a list of possible variables, enter 'list'. Please note that not entering the variable name as it appears within the file or OPeNDAP webform will result in errors.\n\n").strip()

# Allow the user to view a list of all the possible variables to plot
		if(var == "list"): 
			print("\nThis file has the following variables available to plot against time: " + '\n\n')

# Print the variables excluding the date, time, and location variables
			print(list(dataset.keys())[var_index:])
			var = input("\n\nEnter variable to plot:\n").strip()

# Check the validity of the user-entered variable by cross-referencing the list of possible variables
# Once invalid variable is found, print the invalid variable and start this section over
		if var not in dataset.keys():
			print("\nInvalid variable entered. Please enter your variable again.\n")
			continue
		else:
			break

# Return desired variable
	return var

# getTimes() function prompts the user for the date(s) and time(s) at which
# to produce plots and stores that information for later use.

def getTimes():
	error = False
# Allow the user to specify either nominal time or actual time for the plot
	while True:
		timeToUse = input("\nWould you like to use nominal time or actual time for the plots? Enter nominal or actual. \nPlease note that using nominal time will result in more data points at each time for the plot.\n\n")
		if timeToUse not in ["nominal", "actual"]:
			print("Invalid answer entered: " + timeToUse + "  Please try again.\n")
			error = True
		if error:
			error = False
			continue
		else:
			break

# Retrieve lists of dates and times from the dataset
	if timeToUse == "nominal":
		dates = toArr("date_nominal") # HARD-CODED
		times = toArr("time_nominal") # HARD-CODED
	else:
		dates = toArr("date") # HARD-CODED
		times = toArr("time") # HARD-CODED
	datetimes = []

# Create format string to use to convert dates and times to the python object datetime
	formatData = "%Y/%m/%d-%H:%M:%S"

# Create a list of unique datetimes to present to the user
	uniqDateTimes = []

# Create a dictionary that has the desired datetimes as the keys, and a list of indices
# correlating to that datetime as the values
	dateIndices = {}

# Loop through the dates and times to create a python datetime object	
	for i in range(len(dates)):
		dateTimeStr = dates[i] + "-" + times[i]
		curr = datetime.strptime(dateTimeStr, formatData)
		datetimes.append(curr)

# Add the datetime to the list of unique datetimes if it is not already added
# Only add the datetime to the list of unique datetimes if there are at least
# 3 data points.  There has to be at least 3 data points to produce a 3D plot.
	for i in range(len(datetimes)):
		if not datetimes[i] in uniqDateTimes:
			if datetimes.count(datetimes[i]) > 2:
				uniqDateTimes.append(datetimes[i])

# Prompt the user for the desired datetimes(s) and collect their response.
	error = False
	while True:
		usrInput = input("\nWhich time(s) would you like to create 3D plots for? Enter UTC datetimes separated by commas. Please enter the dates and times (UTC) with the following convention: \n\nYYYY/mm/dd-HH:MM  where YYYY = year, mm = month, dd = day, HH = hour, MM = minutes.\n\nTo see a list of possible times, enter 'list'. Please note that not entering the date and times as they appear within the file will result in errors.\n\n").split(',')
	
# Allow the user to view a list of all the possible datetimes over which to plot
		if usrInput[0] == "list":
			print("\nThe following date-times are available to create plots for: " + '\n\n')
			uniqDateTimes.sort()
			for i in uniqDateTimes: print(i.strftime("%Y/%m/%d-%H:%M"))
			usrInput = input("\n\nEnter date/time(s) to plot separated by commas:\n").split(',')

		for i in usrInput:
# Convert the user's input into the datetime object
			i = datetime.strptime(i.strip(), "%Y/%m/%d-%H:%M")

# Check the validity of the user's entries by crossing reference the list of unique datetimes

# If an entry is not in the list of unique stations, print the invalid entry and start this
# section over. 
			if i not in uniqDateTimes:
				print("\n\nInvalid time entered: " + i.strftime("%Y/%m/%d-%H:%M") + ".\n")
				error = True
# If the datetime is valid, create a key in the dictionary with the datetime pointing to
# an empty list
			else:
				dateIndices[i] = []
                        
		if error:
			print("Please try again:\n")
			error = False
			continue
		else:
			break
				
# Loop through each occurence of the desired datetime(s) and note the indices in order
# to retrieve the relevant data points later on.
	for i in range(len(datetimes)):
		if datetimes[i] in dateIndices.keys():
			dateIndices[datetimes[i]].append(i)

# Return dictionary of desired datetime(s) with list of relevant indices. 
	return dateIndices, timeToUse 


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
lines = True
formatData = "%Y/%m/%d-%H:%M:%S"
project_name = "GCIP/ESOP 95"

# Pull OPeNDAP file link from command line argument 
url = sys.argv[1]

# Import dataset using the pydap client and specify QCF for NCAR/EOL datasets 
dataset = open_url(url).QCF

# Collect desired variable
var = getVar()

# Grab data values for desired variable 
varValues = toArr(var)

# Collect desired datetime(s)
(relevantTimes, timeToUse) = getTimes()

# Collect lists of latitudes and longitudes from the dataset 
lats = toArr("latitude") # HARD-CODED
lons = toArr("longitude") # HARD-CODED

# Collect user input for the label on the z axis
# e.g. "Degrees (celsius)
z_label = input("\n\nWhat would you like the label to be on the z-axis?\n")

print("\nCreating plots...\n\n")

# Loop through each desired datetime to create plots
for datetime in relevantTimes.keys():
# Create an empty dictionary to hold values of latitude, longitude, and variable data values
	values = {}

# Create an empty list for each of latitude, longitude, and variable values
	values["lat"] = []
	values["lon"] = []
	values[var] = []

# Loop through the list of indices at the desired datetime and use those indices
# to pull the data points from that time	
	for i in relevantTimes[datetime]:

# Check that variable value isn't missing before adding it and corresponding
# latitude and longitude values to the list of values
		if not varValues[i] == missing:
			values[var].append(float(varValues[i]))
			values["lat"].append(float(lats[i]))
			values["lon"].append(float(lons[i]))

# Begin plotting
# Create figure to hold graph	
	fig = plt.figure(figsize=(20,15)) # HARD-CODED

# Create 3D projection
	ax = fig.gca(projection='3d')

# Rename lists as axises and convert to numpy array for plotting
	x = np.array(values["lat"])
	y = np.array(values["lon"])
	z = np.array(values[var])

# Create array of all zeros to plot stations on the xy-plane
	z0 = z * 0

# Find max and min value to create limits on z-axis
	maxZ = max(z) + 5
	minZ = min(z) - 5

# Plot triangulated surface
	pt3 = ax.plot_trisurf(x, y, z, linewidth=0.2, antialiased=True, cmap='jet') 

# If lines are wanted, plot points on triangulated surface, points on xy-axis
# and lines connecting them
	if lines:	
# Two scatter plots
		ax.scatter(x,y,z, marker='.', s=10, c="black", alpha=0.5)
		ax.scatter(x,y,z0, marker='.', s=10, c='black', alpha=0.5)

# Plot lines connecting points
		for i in range(0, len(z)):
			ax.plot([x[i],x[i]], [y[i],y[i]], [0,z[i]], linewidth=0.5, c='black')
	
# Set labels, colors, title, and view of graph
# HARD-CODED
	ax.set_zlim(0, maxZ)
	plt.xlabel('Latitude')
	ax.set_zlabel(z_label)
	plt.ylabel('Longitude')
	ax.view_init(elev=20, azim = 45)
	cbar=plt.colorbar(pt3)
	cbar.set_label(z_label)
	
# Replace underscores in the variable name with spaces
	varStr = var.replace('_', ' ').title()
	plt.title(project_name + ' ' + varStr + ' on '+ datetime.strftime("%Y-%m-%d %H:%M:%S") + ' UTC (' + timeToUse.capitalize() + ' Time)')
	
# Save plot to current directory with variable and date 
	fig.savefig(datetime.strftime("%Y%m%d%H%M")  + '_' + var + '.png')

# Close figure
	plt.close(fig)

# Print the name of the plot and where it is saved.
	print('Plot saved in the current directory with the name ' + datetime.strftime("%Y%m%d%H%M")  + '_' + var + '.png')

