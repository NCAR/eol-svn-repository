#!/usr/bin/python
# Note: This python script was written by Andrew Janiszeski of UIUC (janszsk2@illinois.edu). In order to use the script, first copy the text below into a new file with a .py extension and make the file executable.


# The purpose of this code is to print out a table of the data in a netCDF file to make it easier to read
# You'll need to use numpy, astropy, netCDF4, and pandas

import sys
import numpy as np
import pandas as pd
import netCDF4 as nc
import astropy

from astropy.table import Table
from netCDF4 import Dataset as NetCDFFile

# Read command line arguments
cmdargs = str(sys.argv)
infile = sys.argv[1]

# Create output file using input name. Replace the .nc with .dat

outfile = infile.replace('.nc', '.dat')

# Read in the file
nc = NetCDFFile(infile, 'r')

# Extract the data arrays

temp = nc.variables['TC'][:]
rh = nc.variables['RH'][:]
time = nc.variables['Time'][:]
hagl = nc.variables['HAGL'][:]
windspd = nc.variables['WINDSPD'][:]
winddrn = nc.variables['WINDDRN'][:]
press = nc.variables['PRESS'][:]

nc.close

# Define a new varible to organize and cluster all the data that was extracted together.

data=time,press,hagl,temp,rh,windspd,winddrn

# Using Table from astropy.table, a simple and neatly ordered table is made with the data extracted from the netCDF file.

outdata = Table(data, names=('Time','Pressure','Height AGL','Temperature','Relative Humidity','Wind Speed','Wind Direction'))

outdata.write(outfile, format='ascii')
