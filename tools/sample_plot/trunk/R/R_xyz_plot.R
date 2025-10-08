# Authorship: NCAR Earth Observing Laboratory Data Management & Services Group
# Contact: eol-archive@ucar.edu
# Created: 12 April 2022

# Licensing: The code associated with this document is provided freely and openly.
# Users are hereby granted a license to access and use this code, unless otherwise
# stated, subject to the terms and conditions of the GNU Affero General Public
# License 3.0 (AGPL-3.0; https://www.gnu.org/licenses/agpl-3.0.en.html). This
# documentation and associated code are provided "as is" and are not supported.
# By using or downloading this code, the user agrees to the terms and conditions
# set forth in this document.

# Acknowledgment: This work was sponsored by the National Science Foundation.
# This material is based upon work supported by the National Center for Atmospheric
# Research, a major facility sponsored by the National Science Foundation and managed
# by the University Corporation for Atmospheric Research. Any opinions, findings
# and conclusions or recommendations expressed in this material do not necessarily
# reflect the views of the National Science Foundation.

# This script shows how the data from a OPeNDAP file can be used to create a
# 3D plot to visualize the data. This script takes one time and creates a
# plot for the entire area of the file.

# This script reads in a OPeNDAP file either through a URL or as a saved 
# file. The user stores the desired parameters in variables. This script
# then takes a given time and stores it along with the entire area in the
# OPeNDAP file. The file then plots the parameters for the entire area in
# the given time.

# This script shows an example of taking an opendap url and plotting the
# temperature for a given location. Note that this example is a guide and
# may need to be changed.

# Notes and Warnings
# There are several instances in this code that contain hardcoded information. 
# The user must carefully review the code and update those sections appropriately.  
# For instance, see the hardcoded URL for the input file, intermediate file names, 
# plot variables, labels for plot axes, and output plot file name. 

library(ncdf4)
library(plot3D)
library(akima)

print("Begin R_xy_plot.R")

# Create a variable and store the URL of the file.

url <- "https://data.eol.ucar.edu/opendap/data/esop_95/hrly_sfc/ES95HRLY_950715.qcf"

# Open the file.

nc <- nc_open(url)

# Store the contents of the files into a text file.

{
    sink('metadata.txt')
    print(nc)
    sink()
}

# Store the location, temp, and other desired info into variables.

lat <- ncvar_get(nc, "QCF.latitude")
long <- ncvar_get(nc, "QCF.longitude")

dew_point <- ncvar_get(nc, "QCF.dew_point_temperature")
air_temp <- ncvar_get(nc, "QCF.air_temperature")

date <- ncvar_get(nc, "QCF.date_nominal")
time <- ncvar_get(nc, "QCF.time_nominal")

# Take the date and time and create the datetime.

datetime <- paste(date, time, sep=" ")

# Find the unique time and datetime.

unique_datetime <- unique(datetime)
unique_time <- unique(time)

# This example takes the first time in the file.

test_time <- 1

# Save the desired time into variable.

t <- unique_time[test_time]

# Create an array to store the line numbers where the desired time is located.

array_time <- rep(0, length(date))

# Find the indices of the desired time and store it in the array.

for (i in 1:length(date)){

    if (t == time[i]){

        array_time[i] <- t

    }

}

# Create a new array that does not have the 0s in 'array_time'.

final_array_time <- array_time[array_time !="0"]

# Create variables that go with the indices found eariler to store the info.

final_dew_temp <- rep(0, length(final_array_time))
final_temp <- rep(0, length(final_array_time))
final_latitude <- rep(0, length(final_array_time))
final_longitude <- rep(0, length(final_array_time))

# Find the info of desired variables for the given location at the given 
# time and store the info into the variables.

ind <- 1

for (i in 1:length(final_array_time)){

    if (t == time[i]){

        final_latitude[i] <- lat[i]
        final_longitude[i] <- long[i]
        final_dew_temp[i] <- dew_point[i]
        final_temp[i] <- air_temp[i]
        ind <- ind + 1

    }

}

# The file contains -999 values. Setting these values to NaN allows 
# the script to ignore plotting these values.

for (i in 1:length(final_dew_temp)){

    if(final_dew_temp[i] < -100){
    
        final_dew_temp[i] <- NaN

    }

}

for (i in 1:length(final_temp)){

    if(final_temp[i] < -100){

        final_temp[i] <- NaN

    }

}

# Create a data frame to plot.

data_grid <- data.frame(

    data_col = c(final_temp),
    axis1 = c(final_latitude),
    axis2 = c(final_longitude)
)

# Remove any lines that contain missing values in the data frame.

data_grid_omit <- na.omit(data_grid)

# Create a matrix from the data frame to plot.

mat = matrix(final_temp, nrow=length(final_latitude), 
 ncol=length(final_longitude))

# Use the interp function to interpolate the grid.

grid <- interp(data_grid_omit$axis1, data_grid_omit$axis2, 
data_grid_omit$data_col, duplicate="strip", nx = 100, ny = 100)

# Use the mesh to create the grid.

M <- mesh(grid$x, grid$y)

surf3D(M$x, M$y, grid$z, xlab="Latitude", ylab="Longitude", 
 zlab="Air Temp (Celsius)", 
 main=paste("GCIP/ESOP-95 Hourly Surface - 19950715", t, sep=""), 
 colvar = grid$z, colkey = TRUE, 
 box = TRUE, bty = "b", phi = 20, theta = 120)
 #, border = "black")

points3D(data_grid_omit$axis1, data_grid_omit$axis2, data_grid_omit$data_col, 
 data_grid_omit$data_col, add=TRUE, pch=20, cex=0.5, col = "black", size=4)

print("End R_xy_plot.R")

