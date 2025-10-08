#------------------------------------------------------------------------------------------------
# Disclaimer:  [UCAR/NCAR/EOL Disclaimer To Be Determined.]
# 
# Description:  This software, plot_surface2D.py, ingests columnar ASCII data or OPeNDAP-generated ASCII files and generates 2-D plot output files to a user specified directory. The user should beware that the input data must be of a specific form with input parameters separated by the iterator entered in the user input section.  This sample software plots temperature and dew point parameters in C on the same plot. Note that this software processes all files that are located in the input_file_directory, and will create plots over the entire time interval of the files given, separated by an optional, user-specified step size (ie 3 days). 
# 
# Inputs:  
#       * Columnar ASCII file including the following: Date Time  Temperature(C)  Dew_Point(C)
#	       ie -31.8729,-65.3513,0,"BOL30015",20180901002000,11.2,39,-2.2,0.3,90,NaN
#	 OR OPeNDAP- generated ASCII file of the form: Time, x, x, x, 
#						       Temp, x, x, x
#	       ie td_21m.td_21m, 24.6, 24.5, 24.6, 24.6, 24.6
#       * Input_file_directory name. Must be full path.
#	       ie ./input_directory/
#       * Output_file_directory name. Must be full path.
#	       ie ./output_directory/
#       * Type of file (OPeNDAP or ascii) flag set to empty string if false, true if true
#	       ie OPeNDAP = True
#       * Delimeter value in regular expression:
#	       ie delim = ',' or delim = '\s*'
#	* Labels or index values for each variable in the file. Note that index values start at 0 and input must be the exact label or index that appears the same in every data file.
#	       ie var1_label = 'Temperature'
#	       OR var1_label = 16
#	 FOR OPENDAP FILES = only enter the single label that appears at the beginning of the line
#	       ie var1_label = 't21' NOT 't21.t21'
#	 NOTE: Spaces within an entry in ASCII file (ie Station Name) will shift index values, so check data files for spaces and give errors #fix wording here
#		ie Silver Lake vs Silver_Lake
#	* Rows to skip if any (indexed at 0) #need examples here
#       * Null values used in data if not NaN - ie -999.99 #missing values NaN's are handled
#       * Unix timestamp flag - set to true if time in data files is in the form of a timestamp (examples)
#	       OR
#       * Date and time format - visit https://strftime.org/ to get more information (examples)
#	* Interval over which to plot values in days. Set interval = 0 if each plot should cover entire time frame. Otherwise, time frame will be divided into interval length in days, and each station will have plots for each interval.
#		ie interval = 3
#       
# Outputs:  2-D Plot files to outout directory with the name station_YYYYmmddHHMM_YYYYmmddHHMM.png with the YYYYmmddHHMM being the start and end date of the plot, respectively.
#	ie GUTH_199507150000_199507152300.png
# 
# Execute command: 
#	python3 plot_surface3D.py
# 
# Assumptions and Warnings: 
#	1. This software plots temperature and dew point on a single output plot for each station over the specified interval(s).
#	2. The time shown on the output plot files will be UTC and it is assumed that the time of input files is either UTC or a timestamp (see description in input section).
#	3. The incoming temperature and dew_point values are assumed to be in celsius
#	4. Plot and axes titles are hardcoded. Search for “title” and “label” to modify.
#	5. Legend text on the plot and plot colors are hardcoded
#	6. The software assumes a basic knowledge of python3
#	7. See the Disclaimer at the top of the file. -- make this number 1
#	8. Before running, check/ change every variable in the user input section. Search "USER INPUT SECTION"
#	9. Spaces within individual data entries will result in errors in the program. For example, if the station name is entered as Silver Lake, please change it to Silver_Lake or Silver-Lake.
#       10. Index values start at 0.
# 
# Author:  Summer Stafford for NCAR/EOL [Date Created – February 2020]
# 
# Version:  1.0 – Initial Release. 
#
#------------------------------------------------------------------------------------------------


#---------------------------------------------------
#	   IMPORT
#---------------------------------------------------

import sys
import csv
import matplotlib.pyplot as plt
import numpy as np
import collections
import os
import datetime
import re
import math
import pandas as pd
import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)

#---------------------------------------------------
#	USER INPUT SECTION   
#---------------------------------------------------

# All data files should be located here; no other files in this directory
# Plots will be saved here

# True if acquired through Opendap, false if ascii/csv data file not acquired through OPeNDAP (see assumptions in header about these files)
# Delimeter in input files (csv vs ascii)- regular expression phrase or character 
# Set the column labels OR index numbers (starting at 0) in the csv/ascii files 
# If data are all from the same station, or if there is no station variable, set station_label = -1
# If date and time are not separated in data, set time_label = -1
# If there is not a network name in the data, set network_label = -1
# Interval over which to separate the data, in days
# If entire time frame is wanted, set interval = 0

# If any rows should be skipped in ALL data files, enter here. If not, set skip_rows = []
# Enter the null value used in the data files if not NaN

# Flag for unix timestamp, ie number of seconds since January 1, 1970. If unix timestamp is used, leave date_format= '%Y/%m/%d' and time_format= '%H:%M:%S'

# Date and time formats of data entries- for more information, visit https://strftime.org/
# If date and time are one object in data entry, ie 19700101000000, enter format into date_format and set time_format = ''



#---------------------------------------------------
#	END OF USER INPUT SECTION
#---------------------------------------------------

def input_OPeNDAP():				# Input function for OPeNDAP- generated ASCII files
	input_files_total = 0
	data = {}
	print ('Processing files in input directory...')

	for filename in os.listdir(input_directory):
		data_prelim = {}
		print('Processing file: ' + filename)
		with open(input_directory + filename, mode='r') as csv_file:
			csv_reader = csv.reader(csv_file)
			next(csv_reader)	# Skip header row
			for row in csv_reader:
				if row:
					data_prelim[row[0]] = []
					for key in np.arange(1,len(row)):
						data_prelim[row[0]].append(row[key])
			
			#Sort the data by station and add data points to dictionary
			for i in np.arange(0,len(data_prelim[date_label])):
				if epoch: date_time = datetime.datetime.utcfromtimestamp(int(data_prelim[date_label][i]))
				elif time_label == -1: date_time = datetime.datetime.strptime(str(data_prelim[date_label][i]), date_format+time_format)
				else: date_time = datetime.datetime.strptime(str(data_prelim[date_label])+str(data_prelim[time_label][i]), date_format+time_format)
				var1 = data_prelim[var1_label+'.'+var1_label][i]
				var2 = data_prelim[var2_label+'.'+var2_label][i]

				# Check for null value and set equal to NaN
				if var1 == missing_value: var1 = np.nan
				if var2 == missing_value: var2 = np.nan

				data_point = [date_time, var1, var2]		
				
				# Set station label
				if (station_label == -1): station = project_name
				else: station = data_prelim[station_label][i]
				
				#--------------------------------			
				#Check for station in data list
				#-------------------------------
				if station in data.keys():
					data[station].append(data_point)
				else:
					data[station] = []
					data[station].append(data_point)
		input_files_total+=1

	print('Processed', input_files_total, 'files. Creating plots now.')
	create_plots(data)

	
def input_ascii(delim):
	input_files_total = 0
	df = pd.DataFrame()
	data = {}
	print ('Processing files in input directory...')

	for filename in os.listdir(input_directory):
		print('Processing file: ' + filename)
		df = pd.read_csv(input_directory + filename, sep = delim, header =0, engine='python', skiprows=skip_rows)
		for index, row in df.iterrows():
			if epoch: date_time = datetime.datetime.utcfromtimestamp(int(row[date_label])) 
			elif time_label == -1: date_time = datetime.datetime.strptime(str(row[date_label]), date_format+time_format)
			else: 
				date_time = datetime.datetime.strptime(str(row[date_label])+str(row[time_label]), date_format+time_format)
	
			var1 = row[var1_label]
			var2 = row[var2_label]
	
			# Check for null values and set to NaN
			if var1 == missing_value: var1 = np.nan
			if var2 == missing_value: var2 = np.nan

			data_point = [date_time, var1, var2]

			# Set station label			
			if (station_label == -1): station = project_name
			elif not (network_label == -1): station = row[network_label] + '-' + row[station_label]	
			else: station = row[station_label]

			#Check for station in data list
			if station in data.keys():
				data[station].append(data_point)
			else:
				data[station] = []
				data[station].append(data_point)
		input_files_total+=1
	
	print('Processed', input_files_total, 'files. Creating plots now.')
	create_plots(data) #put comment here!

def create_plots(data):
	output_files = 0

	# Sort the data from each station by date- explain
	for station in data.keys():
		sortedData = sorted(data[station], key=lambda x: x[0])
		begin = sortedData[0][0]
		end = sortedData[len(sortedData)-1][0]		

		if (interval == 0): 
			graphs_needed = 1 # explain why graphs_needed = 1
			duration = (end-begin).days	
			if (duration == 0): duration+=1
		else: 
			graphs_needed = math.ceil((end-begin).days/interval)
			duration = interval
		if graphs_needed == 0: graphs_needed+=1

			
		print("Producing graphs for " + station + " now. Graphs will be produced over " + str(graphs_needed) + " time period(s)")

		
		for j in range(1,graphs_needed+1):
			dates_var1 = []
			dates_var2 = []
			var1s = []
			var2s = []
			if (graphs_needed > 1):
				var1_total = []
				var2_total = []
				dates_1_total = []
				dates_2_total = []

			for point in sortedData:
				if point[0] in [begin + datetime.timedelta(minutes=x) for x in range((j-1)*duration*1440, j*duration*1440)]:
					var1 = float(point[1])
					var2 = float(point[2])
	
					if not (np.isnan(var1)):	# Only graph non null values
						var1s.append(var1)
						dates_var1.append(point[0])
						if (graphs_needed > 1):
							var1_total.append(var1)
							dates_1_total.append(point[0])
					if not (np.isnan(var2)):
						var2s.append(var2)
						dates_var2.append(point[0])
						if (graphs_needed > 1):
							var2_total.append(var2)
							dates_2_total.append(point[0])
				

		#--------------------------------------------------------------------
		#	  Graph the results for each station over desired interval
		#--------------------------------------------------------------------
	
			if var2s or var1s:				# Check that at least one array exists
				fig = plt.figure(figsize=(20,10)) #HARDCODED and explain
	
				max_var1 = 0
				max_var2 = 0
				min_var2 = 0
				min_var1 = 0

				#Check for empty arrays
				if var1s:
					plt.plot(dates_var1, var1s, label='Air Temperature')	# Plot temperature line
					max_var1 = max(var1s)
					min_var1 = min(var1s)
				if var2s:
					plt.plot(dates_var2, var2s, label= 'Dew Point Temperature')	# Plot dewpoint line
					max_var2 = max(var2s)
					min_var2 = min(var2s)

				# Set labels, legend, and title of graph	
				plt.xlabel('Date/Time')
				plt.ylabel('Degrees Celsius')
				if (station == project_name): plt.title('Temperature and Dew Point Temperature from ' + dates_var1[0].strftime("%Y/%m/%d %H:%M") + ' to ' + dates_var1[len(dates_var1)-1].strftime("%Y/%m/%d %H:%M") + ' UTC for ' + project_name)
				else: plt.title('Temperature and Dew Point Temperature from ' + dates_var1[0].strftime("%Y/%m/%d %H:%M")  + ' to ' + dates_var1[len(dates_var1)-1].strftime("%Y/%m/%d %H:%M") + ' UTC for ' + project_name + ' ' + station)
				plt.legend()
	
				# Find max and min temps to create y axis ticks and labels
				max_value = max(max_var1,max_var2)
				min_value = min(min_var1, min_var2)
				plt.yticks(np.arange(min_value, max_value, step=3))  # Set label locations. #HARDCODED
				plt.xticks(rotation=45)

				# Save plot to output directory and close figure
				fig.savefig(output_directory + station + '_'+ dates_var1[0].strftime("%Y%m%d%H%M")  + '_' + dates_var1[len(dates_var1)-1].strftime("%Y%m%d%H%M") + '.png')
				plt.close(fig)

				print('Plot saved in the plots directory with the name ' + station + '_' + dates_var1[0].strftime("%Y%m%d%H%M")  + '_' + dates_var1[len(dates_var1)-1].strftime("%Y%m%d%H%M") + '.png')
				output_files += 1

		if (graphs_needed > 1):
			fig = plt.figure(figsize=(20,10))
			max_var1 = 0
			max_var2 = 0
			min_var2 = 0
			min_var1 = 0
	
			plt.plot(dates_1_total, var1_total, label='Air Temperature')    # Plot temperature line
			max_var1 = max(var1_total)
			min_var1 = min(var1_total)
			plt.plot(dates_2_total, var2_total, label= 'Dew Point Temperature')   # Plot dewpoint line
			max_var2 = max(var2_total)
			min_var2 = min(var2_total)
	
			# Set labels, legend, and title of graph	
			plt.xlabel('Date/Time')
			plt.ylabel('Degrees Celsius')
			if (station == project_name): plt.title('Temperature and Dew Point Temperature from ' + project_name)
			else: plt.title('Temperature and Dew Point Temperature from ' + project_name + ' ' + station)
			plt.legend()
	
			# Find max and min temps to create y axis ticks and labels
			max_value = max(max_var1,max_var2)
			min_value = min(min_var1, min_var2)
			plt.yticks(np.arange(min_value, max_value, step=3))  # Set label locations.
			plt.xticks(rotation=45)
	
			# Save plot to output directory and close figure
			fig.savefig(output_directory + station + '_overall' + '.png')
			plt.close(fig)

			print('Plot saved in the plots directory with the name ' + station + '_overall' + '.png')
			output_files += 1	
	print('Created ' + str(output_files) + ' files to view in output directory.')
	print('Done')
# Check that input file argument is entered
if not (len(sys.argv) == 2):
	print("Incorrect number of inputs received. Please retry the python3 plot_surface2D.py command followed only by the full path to the input file.")
	exit()
input_file = sys.argv[1]
inputs = []
false_options = ['False', 'false', '0']
print ('Retrieving data in input file: ' + str(input_file))
file = open(input_file, 'r')
for line in file:
	inputs.append(line.rstrip())

# Check for correct number of inputs
if not (len(inputs) == 22):
	print("Incorrect number of inputs in input file. This program expects 22 inputs in input file, but there are currently " + str(len(inputs)) + " lines in input file. A complete list of inputs expected can be found in the user guide or in the header of this program. Please check inputs and try again.")
	exit()

# Initialize all variables
input_directory = inputs[0]
output_directory = inputs[1]
project_name = inputs[2]
if (inputs[3] in false_options): OPeNDAP = False
else: OPeNDAP = True
var1_name = inputs[4]
var1_label = inputs[5]
if (re.search('\d+', var1_label)): var1_label = int(var1_label)
var2_name = inputs[6]
var2_label = inputs[7]
if (re.search('\d+', var2_label)): var2_label = int(var2_label)
date_label = inputs[8]
if (re.search('\d+', date_label)): date_label = int(date_label)
time_label = inputs[9]
if (re.search('\d+', time_label)): time_label = int(time_label)
network_label = inputs[10]
if (re.search('\d+', network_label)): network_label = int(network_label)
station_label = inputs[11]
if (re.search('\d+', station_label)): station_label = int(station_label)
delim = inputs[12]
missing_value = inputs[13]
if(re.search('\d+', missing_value)): missing_value = float(missing_value)
skip_rows_str = inputs[14]
skip_rows = []
for i in (re.findall('\d+', skip_rows_str)):
	skip_rows.append(int(i))
if (inputs[15] in false_options): epoch = False
else: epoch = True
interval = int(inputs[16])
interval_units = inputs[17]
date_format = inputs[18]
time_format = inputs[19]
x_label = inputs[20]
y_label = inputs[21]

#print(input_directory+'\n'+output_directory+'\n'+project_name+'\n'+str(OPeNDAP)+'\n'+var1_name+'\n'+str(var1_label)+'\n'+var2_name+'\n'+str(var2_label)+'\n'+str(date_label)+'\n'+str(time_label)+'\n'+str(network_label)+'\n'+str(station_label)+'\n'+delim+'\n'+str(missing_value)+'\n'+str(skip_rows)+'\n'+str(epoch)+'\n'+str(interval)+'\n'+interval_units+'\n'+date_format+'\n'+time_format+'\n'+x_label+'\n'+y_label)

# Send data to appropriate input function
if (OPeNDAP):
	input_OPeNDAP()
else:
	input_ascii(delim)

