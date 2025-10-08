# datafile is a Python module to find and read data from data files on 
# the local disk.
#
# The class findFiles locates files on the system and returns
# information about them.
#
# The class parseNetCDF reads data and metadata from netCDF files
# and returns that info.
#

# import some Python standard modules
from string import upper
import Nio
import datetime

##################################################################
########### Define the findFiles class ###########################
##################################################################
class findFiles:
        "a class to find files on the system"


        def find_file(self,path,filename):
                from os.path import exists
                file = path + filename
                if exists(file):
                        return "exists"
                else:
		    print "WARNING: File "+file+" does not exist!!!"
		    return "does not exist"

##################################################################
########### Define the parseNetCDF class #########################
##################################################################
class parseNetCDF:
        "a class to retrieve data and metadata from netCDF files"

	def get_var_list(self,ncfile):
        	"Get a list of all variables in a netCDF file"

        	varNames = ncfile.variables.keys()

        	return varNames

	def parseFlightTimes(self,flightDate,timeInterval):
	        """Parse RAF Flight dates into component parts

	        Given the FlightDate and TimeInterval from an RAF netCDF flight
	        data file, return the year,month,day,hour,min, and sec of the
	        beginning and end of the flight.

	        input: FlightDate and TimeInterval from header of netCDF file
	        output: two arrays containing year,month,day,hour,min,sec of
	        start of flight and end of flight

	        """

	        month = int(flightDate[0:2])
	        day = int(flightDate[3:5])
	        year = int(flightDate[6:10])
	        date = flightDate[6:10]+flightDate[0:2]+flightDate[3:5]
	        startHour = int(timeInterval[0:2])
	        startMin = int(timeInterval[3:5])
	        startSec = int(timeInterval[6:8])
	        endHour = int(timeInterval[9:11])
	        endMin = int(timeInterval[12:14])
	        endSec = int(timeInterval[15:17])
	        startTime = datetime.datetime(year,month,day,startHour,startMin,startSec)
	        endTime = datetime.datetime(year,month,day,endHour,endMin,endSec)
	        if endHour < startHour:
		        addDay = datetime.timedelta(days=1)
			endTime = endTime + addDay

		print startTime
		print endTime
	        return [startTime,endTime,date]

	
