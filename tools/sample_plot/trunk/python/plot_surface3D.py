#------------------------------------------------------------------------------------------------
# Disclaimer:  [UCAR/NCAR/EOL Disclaimer To Be Determined.]
# 
# Description:  This software, plot_surface3D.py, ingests columnar ASCII data, QCF data, or OPeNDAP-generated ASCII files and generates 3-D plot output files to a user specified directory. The user should beware that the input data must be of a specific form with input parameters separated by the iterator entered in the user input section.  This sample software plots temperature and dew point parameters in C on sepearate 3-D plots for each unique date in data. Note that this software processes all files that are located in the input_file_directory, and will create plots for each date over all latitude and longitude values. If desired, this program can generate animations that will circle to show the entirety of the 3D plot. 
# 
# Inputs:  
#	* ASCII file or QCF file including the following: Date Time  Temperature(K)  Dew_Point(K) Latitude Longitude
# 		ie -31.8729,-65.3513,0,"BOL30015",20180901002000,11.2,39,-2.2,0.3,90,NaN
#	  OR OPeNDAP- generated ASCII file of the form: Time, x, x, x, 
#							Temp, x, x, x
#		ie td_21m.td_21m, 24.6, 24.5, 24.6, 24.6, 24.6
#	* Input_file_directory name. Must be full path.
#		ie ./input_directory/
#	* Output_file_directory name. Must be full path.
# 		ie ./output_directory/
#	* Type of file (opendap or ascii) flag 
#		ie opendap = True
#	* Delimeter value in regular expression:
#		ie delim = ',' or delim = '\s*'
#	* Labels or index values for each variable in the file. Note that index values start at 0 and input must be the exact label or index that appears in every data file.
#		ie temp_label = 'Temperature'
#		OR temp_label = 16
#	  FOR OPENDAP FILES = only enter the single label that appears at the beginning of the line
#		ie temp_label = 't21' NOT 't21.t21'
#	  NOTE: Spaces within an entry in ASCII file (ie Station Name) will shift index values, so check data files for spaces and give errors
#		ie Silver Lake vs Silver_Lake
#	* Rows to skip if any (indexed at 0)
#	* Null values used in data if not NaN - ie -999.99
#	* Unix timestamp flag - set to true if time in data files is in the form of a timestamp
#		OR
#	* Date and time format - visit https://strftime.org/ to get more information
#	* Project name for plot titles and file names (again, no spaces)
#	* Datetimes over which to produce animations, if desired.
#		
#	
# Outputs:  3-D Plot files to outout directory with the name projectName_temperature_YYYYmmddHHMMSS.png projectName_dewpoint_YYYYmmddHHMMSS.png with the YYYYmmddHHMMSS being the date and time of the plot.
#	ie GCIP-ESOP-95_temperature_19950717030000.png
#	
#	    3-D animations to output directory with the name projectName_temperatureAnimation_YYYYmmddHHMMSS.mp4, projectName_dewpointAnimation_YYYYmmddHHMMSS.mp4 with YYYYmmddHHMMSS being the date and time of the animation.
#	ie GCIP-ESOP-95_dewpointAnimation_199507150000.mp4
# 
# Execute command: 
#       python3 plot_surface3D.py
# 
# Assumptions and Warnings: 
#       1. This software plots every location on single output plot for each day.
#       2. The time of the output files will be UTC and it is assumed that the time of input files is either UTC or a timestamp.
#       3. The incoming temperature and dew_point values are assumed to be in celsius
#       4. Plot and axes titles are hardcoded. Search for “title” and “label” to modify.
#       5. Legend text and plot colors are hardcoded
#       6. The software assumes a basic knowledge of python3
#       7. See the Disclaimer at the top of the file.
#       8. Before running, check/ change every variable in the user input section. Search "USER INPUT SECTION"
#	9. Spaces within individual data entries will result in errors in the program. For example, if the station name is entered as Silver Lake, please change it to Silver_Lake or Silver-Lake.
#	10. Index values start at 0.
#	11. There must be at least 3 values of temperature or dewpoint temperature at each distinct date/time in order to produce plots
#	12. If animations are desired, in order to save the animated files to your device, either ffmpeg or imagemagick must be installed. 
# 
# Author:  Summer Stafford for NCAR/EOL [Date Created – February 2020]
# 
# Version:  1.0 – Initial Release.  
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
from matplotlib import cm
from matplotlib import animation
from mpl_toolkits.mplot3d import Axes3D
warnings.simplefilter(action='ignore', category=FutureWarning)

#---------------------------------------------------
#	USER INPUT SECTION   
#---------------------------------------------------

input_directory = './1.003/' 		# All data files should be located here; no other files in this directory
output_directory = './plot_1.003/test/'	# Plots will be saved here

project_name = 'GCIP-ESOP-95'		# This will be used in the titles of the plots and the file names- NO SPACES IN PROJECT NAME

opendap = False				# True if acquired through Opendap, false if ascii/csv data file not acquired through opendap (see assumptions in header about these files)
delim = '\s*'				# Delimeter in input files (csv vs ascii)- regular expression phrase or character

anim_date = [datetime.datetime(1995, 7, 15, 0, 0)]  #Enter dates here for animations. Multiple dates can be entered in the brackets separated by commas. If no animations are desired, set anim_date = []. WARNING: Animations take up to 5 minutes each to process.

# Set the column labels OR index numbers (starting at 0) in the csv/ascii files 
# If date and time are not separated in data, set time_label = -1
lat_label = 6 
long_label = 7
temp_label = 16
dwpt_label = 18
time_label = 1 
date_label = 0


skip_rows = [1,2]			# If any rows should be skipped in ALL data files, enter here. If not, set skip_rows = []
null_value = -999.99			# Enter the null value used in the data files if not NaN

epoch = False				# Flag for unix timestamp, ie number of seconds since January 1, 1970. If unix timestamp is used, leave date_format= '%Y/%m/%d' and time_format= '%H:%M:%S' 

# Date and time formats of data entries- for more information, visit https://strftime.org/
# If date and time are one object in data entry, ie 19700101000000, enter format into date_format and set time_format = ''
date_format =  '%Y/%m/%d'		
time_format =  '%H:%M:%S'

#---------------------------------------------------
#	END OF USER INPUT SECTION
#---------------------------------------------------

def input_opendap():			# Input function for OPeNDAP- generated ASCII files
	input_files_total = 0
	data = {}
	print ('Processing files in input directory...')

	for filename in os.listdir(input_directory):
		data_prelim = {}
		print('Processing file: ' + filename)
		with open(input_directory + filename, mode='r') as csv_file:
			csv_reader = csv.reader(csv_file)
			next(csv_reader)	#Skip header row
			for row in csv_reader:
				if row:
					data_prelim[row[0]] = []
					for key in np.arange(1,len(row)):
						data_prelim[row[0]].append(row[key])
			
			#Sort the data by date and add data points to dictionary
			for i in np.arange(0,len(data_prelim[date_label])):
				if epoch: date_time = datetime.datetime.utcfromtimestamp(int(data_prelim[date_label][i]))
				elif time_label == -1: date_time = datetime.datetime.strptime(str(data_prelim[date_label][i]), date_format+time_format)
				else: date_time = datetime.datetime.strptime(str(data_prelim[date_label][i]) + str(data_prelim[time_label][i]), date_format+time_format)
				lat = data_prelim[lat_label+'.'+lat_label][i]
				lon = data_prelim[long_label+'.'+long_label][i]
				temp = data_prelim[temp_label+'.'+temp_label][i]
				dwpt = data_prelim[dwpt_label+'.'+dwpt_label][i]
	
				#Check for null values and set to NaN
				if temp == null_value: temp = np.nan
				if dwpt == null_value: dwpt = np.nan
					
				data_point = [lat, lon, temp, dwpt]
				
				#Check for date in data list
				if date_time in data.keys():
					data[date_time].append(data_point)
				else:
					data[date_time] = []
					data[date_time].append(data_point)
		input_files_total+=1

	print('Processed', input_files_total, 'files. Creating 3D plots for temperature and dewpoint temperature now.')
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
			else: date_time = datetime.datetime.strptime(str(row[date_label])+str(row[time_label]), date_format+time_format)

			lat = row[lat_label]
			lon = row[long_label]
			temp =row[temp_label]
			dwpt = row[dwpt_label]

			# Check for null values and set to NaN
			if temp == null_value: temp = np.nan
			if dwpt == null_value: dwpt = np.nan

			data_point = [lat, lon, temp, dwpt]
		
			#Check for time in data list
			if date_time in data.keys():
				data[date_time].append(data_point)
			else:
				data[date_time] = []
				data[date_time].append(data_point)
		input_files_total+=1

	print('Processed', input_files_total, 'files. Creating 3D plots for dewpoint and temperature now.')
	create_plots(data)


def create_plots(data):
	output_files = 0
	anim_files = 0
	graphs_needed = len(data.keys())		# Create a graph for each unique date/time that includes all stations
	print("Graphs will be produced for a total of " + str(graphs_needed) + " dates")

	for date in data.keys():
		lats_t = []
		lons_t = []
		lats_d = []
		lons_d = []
		temps = []
		dewpoint = []
		for point in data[date]:
			temp = float(point[2])
			dp = float(point[3])
			if not np.isnan(temp):		# Only graph non null values
				temps.append(temp)
				lats_t.append(float(point[0]))
				lons_t.append(float(point[1]))
			if not np.isnan(dp):
				dewpoint.append(dp)
				lats_d.append(float(point[0]))
				lons_d.append(float(point[1]))
	#-------------------------------------------------------------------------------
	#	   Graph the results for the distinct date for temperature and dewpoint 
	#-------------------------------------------------------------------------------

		# Check that there are at least three values:
		if (len(temps) >= 3):
			# TEMPERATURE PLOT
			fig_temp = plt.figure(figsize=(20,15))
			ax = fig_temp.gca(projection='3d')
			x= np.array(lons_t)
			y = np.array(lats_t)
			z = np.array(temps)
		
			# Set up arrays in order to show dots for each station on xy axis
			z0 = z * 0 
			minZ = min(z) - 5
			maxZ = max(z) + 5
	
			def init():		
				pt3 = ax.plot_trisurf(x, y, z, linewidth=0.2, antialiased=True, cmap='jet')	# Plot triangulated data
				ax.scatter(x,y,z, marker='.', s=10, c="black", alpha=0.5)			# Plot points at each data point
				ax.scatter(x,y,z0, marker='.', s=10, c='black', alpha=0.5)			# Plot points for each station on xy axis
				for i in range(0, len(z)):			# Plot lines between xy axis up to data points
					ax.plot([x[i],x[i]], [y[i],y[i]], [0,z[i]], linewidth=0.5, c='black')
		
	
				# Set labels, colors, title, and view of graph
				ax.set_zlim(0, 40)
				plt.xlabel('Latitude')
				cbar=plt.colorbar(pt3)
				cbar.set_label("Degrees (celcius)")	
				plt.title('Temperature from '+ date.strftime("%Y-%m-%d %H:%M:%S") + ' UTC for ' + project_name)
				plt.ylabel('Longitude')
				return fig_temp, 
	
			def animate(i):
				ax.view_init(elev=10, azim=i)
				return fig_temp,


			# Save plot to output directory and close figure
			init()
			ax.view_init(elev=10, azim = 25)
			plt.savefig(output_directory + project_name + '_temperature_' + date.strftime("%Y%m%d%H%M%S")  + '.png')
			plt.close(fig_temp)

			print('Plot saved in the plots directory with the name ' + project_name + '_temperature_' + date.strftime("%Y%m%d%H%M")  + '.png')
			output_files += 1
				
			if (date in anim_date):
				
				# Animate
				anim = animation.FuncAnimation(fig_temp, animate, init_func=init, frames=360, interval=20, blit=True)
			
				# Save
				anim.save(output_directory + project_name + '_temperatureAnimation_' + date.strftime("%Y%m%d%H%M") + '.mp4', fps=30, extra_args=['-vcodec', 'libx264'])

				print('Animation saved in the plots directory with the name ' + project_name + '_temperatureAnimation_' + date.strftime("%Y%m%d%H%M") + '.mp4')
				anim_files += 1
			
				
		# Check that there are at least three values:
		if (len(dewpoint) >= 3):
			# DEWPOINT PLOT
			fig_dwpt = plt.figure(figsize=(15,10))
			ax = fig_dwpt.gca(projection='3d')
			x = np.array(lons_d)
			y = np.array(lats_d)
			z = np.array(dewpoint)
	
			# Set up arrays in order to show dots for each station on xy axis
			z0 = z * 0
			minZ = min(z) - 5
			maxZ = max(z) + 5
	
			def init():
				pt3 = ax.plot_trisurf(x, y, z, linewidth=0.2, antialiased=True, cmap='jet')     # Plot triangulated data
				ax.scatter(x,y,z, marker='.', s=10, c="black", alpha=0.5)		       # Plot points at each data point
				ax.scatter(x,y,z0, marker='.', s=10, c='black', alpha=0.5)		      # Plot points for each station on xy axis
				for i in range(0, len(z)):		      # Plot lines between xy axis up to data points
					ax.plot([x[i],x[i]], [y[i],y[i]], [0,z[i]], linewidth=0.5, c='black')


				# Set labels, colors, title, and view of graph
				ax.set_zlim(0, 40)
				plt.xlabel('Latitude')
				cbar=plt.colorbar(pt3)
				cbar.set_label("Degrees (celcius)")
				plt.title('Dewpoint Temperature from '+ date.strftime("%Y-%m-%d %H:%M:%S") + ' UTC for ' + project_name)
				plt.ylabel('Longitude')
				return fig_dwpt,

			def animate(i):
				ax.view_init(elev=10, azim=i)
				return fig_dwpt,
			
			init()
			ax.view_init(elev=10, azim = 25)
			plt.savefig(output_directory + project_name + '_dewpoint_' + date.strftime("%Y%m%d%H%M%S")  + '.png')
			plt.close(fig_dwpt)

			print('Plot saved in the plots directory with the name ' + project_name + '_dewpoint_' + date.strftime("%Y%m%d")  + '.png')	
			output_files += 1
			
			if (date in anim_date):

				# Animate
				anim = animation.FuncAnimation(fig_dwpt, animate, init_func=init, frames=360, interval=20, blit=True)

				# Save
				anim.save(output_directory + project_name + '_dewpointAnimation_' + date.strftime("%Y%m%d%H%M") + '.mp4', fps=30, extra_args=['-vcodec', 'libx264'])

				print('Animation saved in the plots directory with the name ' + project_name + '_dewpointAnimation_' + date.strftime("%Y%m%d%H%M") + '.mp4')
				anim_files += 1

	print('Created ' + str(output_files) + ' plot files and ' + anim_files + ' animations to view in output directory.')
	print('Done')

# Send data to appropriate input function
if (opendap):
	input_opendap()
else:
	input_ascii(delim)

