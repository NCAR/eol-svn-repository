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

# This R script is used for 2D visualization of data for a single site extracted from
# a user specified netCDF file. This script shows how R code can be used 
# to generate these plots.

# The script first reads in the netCDF file either through a URL or through
# a downloaded file. The user stores the wanted information as variables
# and stores the desired location information. The script then finds all of
# the desired information for the given location and plots the information.

# The script shows an example of taking an opendap url and plotting the
# temperature for a given location. Note that this example is a guide and
# may need to be changed.

# Notes and Warnings
# There are several instances in this code that contain hardcoded information. 
# The user must carefully review the code and update those sections appropriately.  
# For instance, see the hardcoded URL for the input file, intermediate file names, 
# plot variables, labels for plot axes, and output plot file name. 

library(ncdf4)
library(ggplot2)
library(tidyr)

print("Begin R_xy_plot.R") 

# Store the URL for the files in a variable.

url <- "https://data.eol.ucar.edu/opendap/data/esop_95/hrly_sfc/ES95HRLY_950715.qcf"

# Open the file.

nc <- nc_open(url)

# Store the info in the file into a text file for the viewer to read.

{
    sink('metadata.txt')
    print(nc)
    sink()
}

# Save the date and time into variables.

date <- ncvar_get(nc, "QCF.date_nominal")
time <- ncvar_get(nc, "QCF.time_nominal")

# Take the date and time and create the datetime.

datetime <- paste(date, time, sep=" ")

# Only keep the unique datetimes.

unique_datetime <- unique(datetime)

# Load other variables to plot.

dew_point <- ncvar_get(nc, "QCF.dew_point_temperature")
air_temp <- ncvar_get(nc, "QCF.air_temperature")

network <- ncvar_get(nc, "QCF.network_name")
station <- ncvar_get(nc, "QCF.platform_name")

# Combine the network and station and create a location.

location <- paste(network, station)

unique_location <- unique(location)
unique_location <- sort(unique_location)

# Save the locations into a text file for the viewer to see.

{
    sink('location_list.txt')
    print(unique_location)
    sink()
}

# For this example, the first location is used for plotting.

example_location <- unique_location[1]

# Take the number of times and create an array of the same size to create an 
# array to store the indices of repeating locations.

time_size <- nrow(time)
inc_array <- rep(0, nrow(time))

# Find the indices where the location repeats. 

inc <- 1

for (i in 1:time_size){

    current_station <- location[i]
    
    if (current_station == example_location){

        inc_array[i] <- i
        inc <- inc + 1
    
    }

}

# Take the array of indices and remove the 0's.

final_array <- inc_array[inc_array != 0]

# Use the line commented below to see the indices of the location.

# print(final_array)

# Create arrays to store the temp of the given location.

final_dew_temp <- rep(0, length(final_array))
final_temp <- rep(0, length(final_array))

# For this file, the data contains -999 values. Here these 
# values are set to NaN so they will not be plotted.

for (k in 1:length(final_array)){

    if (air_temp[final_array[k]] <= -100){
        
        final_temp[k] <- NaN

    }

    if (air_temp[final_array[k]] > -100){

        final_temp[k] <- air_temp[final_array[k]]

    }

    if (dew_point[final_array[k]] <= -100){

        final_dew_temp[k] <- NaN

    }

    if (dew_point[final_array[k]] > -100){

        final_dew_temp[k] <- dew_point[final_array[k]]

    }

}

# Create the data frame for plotting.

df <- data.frame(

    df_datetime = unique_datetime,
    df_air_temp = final_temp,
    df_dew_temp = final_dew_temp

)

# Plot the data.

ggplot(data = df, aes(x = df_datetime, group = 1)) + 
geom_line(aes(y = df_air_temp, color = "darkred")) + 
geom_line(aes(y = df_dew_temp, color = "steelblue"))+ 
geom_point(aes(y = df_air_temp, color = "darkred")) + 
geom_point(aes(y = df_dew_temp, color = "steelblue")) +

# Adjust labels, title, legend, etc.

xlab(" ") +
ylab("Temperature (Celsius)") +
labs(title=paste("GCIP/ESOP-95 Hourly Surface: ", unique_location, sep=""), color = "Legend") +
scale_color_hue(labels = c("Air Temp", "Dew Point Temp")) +
theme(plot.title = element_text(hjust = 0.5)) +
theme(axis.text.x = element_text(angle = 90))

# Save the file as a .png.

ggsave("test.png")


print("End R_xy_plot.R")
















