#!/bin/python3

#
# hpcn_download_data.py
#
# This python script is called by the perl script hpcn_archive_data.pl.  This script
# downloads the HPCN data from the Automated Weather Data Network (AWDN) web site.  
# This script uses python routines that came from the AWDN and are located in 
# the WebServices.py file.  This script first retrieves all stations instead of
# only active stations.  This is because we may miss some data if only active 
# stations are checked.  The script will download data for any station that is
# not currently closed down (None in closedown).  If the station is closed down, then it will retrieve
# data if the close down date is later than the first of the month that it is 
# retrieving data for.  For example, if the script is downloading data for 
# January 2023, then data will be downloaded for any station that is closed down
# where the close down date occurs after 1/1/2023.  A separate file is written 
# for each individual station using stationname_yyyymm.csv.  If a station name
# has spaces in the name, then the spaces are replaced with _.  If a station name
# has " - " in the name, that is replaced by "-". All parentheses are removed from
# the station names. If there are duplicate station
# names (this is true for Gypsum and Olathe, both in CO and KS), then the station
# id is added to the output file name.
#

import urllib3
import json
import argparse

from WebServices import WebServices

#
# Retrieve data from command line
#
parser = argparse.ArgumentParser()
parser.add_argument("year", help="year to be downloaded")
parser.add_argument("month", help="month to be downloaded")
parser.add_argument("last_day_in_month", help="last day in month to be downloaded")
parser.add_argument("download_dir", help="directory used for downloaded files")
parser.add_argument("station_file", help="station list file including directory")
args = parser.parse_args()

#
# Create the begin_date and end_date for retrieving the data from the AWDN web site.  In the end date
# include 23 for the hour.  If the 23 is not included, the routine will only 
# return data for the 00 hour for the last day in the month.
#
begin_year_month = args.year + args.month
begin_date = begin_year_month + "01"
end_date = begin_year_month + args.last_day_in_month + "23"
# Used to check against a station that is now closed down.  If the closedown date/time is after
# the first of the month being run, then download the data until the closedown date/time.
check_date = args.year + "-" + args.month + "-01 00:00:00+00:00"
# Used to check against a station that has opened recently.  If the startup date/time is before
# the last day of the month being run, then download the data from the startup date/time to the
# end of the month.
check_startup_date = args.year + "-" + args.month + "-" + args.last_day_in_month + " 00:00:00+00:00"
# Station list saved from the previous run. This is used to check if there are any new stations
# since the last run of the script.
station_list_previous = args.station_file

#
# Set up the python routines to be called for accessing the data from AWDN
#
ws = WebServices(False)

#
# Get the list of all stations for the scqc60 product which is hourly data
#
allStations = json.loads(ws.getList("scqc60"))
allStationsNew = []
#
# Read in the previous station list to compare to the current station list
#
f = open(station_list_previous, "r")
data = f.read()
f.close()
# Transfer the data read in, to a list.  Remove the last item in the list,
# since it will be an empty line because of the split on the newline.
allStationsPrevious = data.split("\n")
del allStationsPrevious[-1]
# Create a list of the current stations to match the previous station list.
for station in allStations:
   allStationsNew.append(station['stationid']+","+str(station['closedown'])+","+station['startup'])
# Find the differences in the two lists.  stationDifference will contain stations
# that are different in the new station list, from the previous station list and all
# possibly new stations.
stationDifference = list(set(allStationsNew) - set(allStationsPrevious))

for station in allStations:
#
# Download data for each active station.
# If "None" is closedown for a station, that means the station is active.  Verify 
# that the startup of the station is on/before the last day of the month that data
# is being downloaded for.
# If a station has a closedown date, only download data for that station if the 
# closedown date is after the first day of the month that data is being downloaded for.
#
   if (station["closedown"] == None and station["startup"] <= check_startup_date) or (station["closedown"] != None and station["closedown"] >= check_date) :
      stationData = ws.getData(station["stationid"], begin_date, end_date, "scqc60", "csv", "si", "UTC", "all", "all")
# Replace the spaces in the station name and remove parentheses
      outfile = station["name"].replace(" - ", "-")
      outfile = outfile.replace("(", "")
      outfile = outfile.replace(")","")
      outfile = outfile.replace(" ", "_")
# Since there are two stations named Gypsum and Olathe (KS and CO), add the
# station id to these files names, so the file names will be unique.
      if (station["name"] == "Gypsum") or (station["name"] == "Olathe") :
         outfile = outfile + "_" + station["stationid"]
      outfile = args.download_dir + "/" + outfile + "_" + begin_year_month + ".csv"
      f = open(outfile, "w")
      f.write(stationData)
      f.close()
#
# Print out the new station list to the existing station file
#
f = open(station_list_previous, "w")
for station in allStations:
   f.write(station['stationid']+","+str(station['closedown'])+","+station['startup']+"\n")
f.close()
#
# Print out the new stations to be returned to the perl script.
#
for station in stationDifference :
   print(station)
