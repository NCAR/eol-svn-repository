#!/bin/bash

: '

Title:
stmConverter

Author:
Christian Sibo, UCAR - EOL

Date:
08/11/2011

Execution:
Bash Shell

Description:


3rd Party Programs Required:


Parameters:


Operation:


Error Prevention:


Script Errors:


ffmpeg Errors:

'

function toUpper {
    echo $1 | tr '[a-z]' '[A-Z]'
}

function getCount {
    echo $1 | grep -on _ | wc -l
}

if [ soil_conversion.log ] ; then
    echo -e '\n\n\n-------------------------------' >> soil_conversion.log
    echo $(date) >> soil_conversion.log
    echo -e '\n\n' >> soil_conversion.log
else
    echo -e '-------------------------------' > soil_conversion.log
    echo $(date) >> soil_conversion.log
    echo -e '\n\n' >> soil_conversion.log
fi

#Main for loop to iterate through the directories under the current
javadir='/net/work/CEOP/version2/netCDF_conversions/jarfiles'
for dir in $(find ./ -type d) ; do
   len=${#dir}
   if [ $len -eq 2 -o $len -eq 3 ] ; then
      echo
   else
       echo -e 'Searching for files in' $dir ' ...\n' >> soil_conversion.log
        #secondary for loop to find each .stm in the current $dir
        for theFile in $dir/*.stm ; do
	    theFile2=${theFile/$dir/}
	    len2=${#theFile2}
	    if [ $len2 -eq 6 ] ; then
		echo
	    else
                 #check if $theFile is an original file that already has a $theFile_copy.stm in this directory
                 #ie. $theFile has already been converted
		if [ $(find $dir -wholename ${theFile%.stm}_copy.stm) ] ; then
		    echo NOT CONVERTING $theFile , Already Converted
		else
	             #set up variables for the next error check
	             #saves a len for $theFile's filename
		    original=${#theFile}
	             #saves a string for $theFile's filename with _copy replaced with nothing
		    bub=${theFile/_copy/}
		    bub2=${theFile/_nc.stm/}
	             #convertes the string to a len
		    altered=${#bub}
		    altered2=${#bub2}
	             #check if $theFile is a copy of an original file
		    if [ $original != $altered ] ; then
			echo NOT CONVERTING $theFile, Already Converted
		    else
		    if [ $original != $altered2 ] ; then
			echo NOT CONVERTING $theFile, Already Converted
		    else
			counter=0
			echo -e 'Converting ' $theFile ' ...\n' >> soil_conversion.log
			echo Changing -0.00s to 0.00s as ${theFile%.stm}_copy.stm  ...
	                #save the $theFile_copy.stm
			cp $theFile ${theFile%.stm}_copy.stm
	                #Convert the -0.00 to 0.00
			grep -rl '\-0.00' ${theFile%.stm}_copy.stm | xargs sed -i 's/\-0.00/ 0.00/g'
			echo Converting $theFile from ASCII to netCDF ...
	                #run the ascii to netcdf converter
			java -jar $javadir/CEOPstmiosp.jar -i $theFile -o $dir > $dir/soil.log
	                #find each new netcdf file made
			for newFile in $dir/*_stm.nc ; do
			    let "counter = $counter + 1"
                            #convert the new netcdf back to ascii
			     flag=0
			    stationName=${newFile%_stm.nc}
			    stationName=${stationName/$dir/}
			    stationName=${stationName/\//}


			    dirTemp=${dir/.\//}
			    rhpTemp=${dirTemp/\/*/}
			    rsiTemp=${dirTemp/$rhpTemp/}
			    rsiTemp=${rsiTemp/\//}
			    rhpCount=$(getCount $rhpTemp)
			    rsiCount=$(getCount $rsiTemp)
			    rhpLen=${#rhpTemp}
			    rsiLen=${#rsiTemp}
			    
			    realFile=${theFile/$dir/}
			    realFile=${realFile/\//}
			    rhp=${realFile:0:rhpLen}
			    rsi=${realFile/$rhp/}
			    rsi=${rsi#_}
			    rsi=${rsi%_*_*.stm}
                            rsiCount2=$(getCount $rsi)
			    if [ $rsiCount2 -ne $rsiCount ] ; then
				temp=${rsi%_*}
				misc=${rsi/$temp/}
				misc=${misc#_}
				rsi=${rsi%$misc}
				rsi=${rsi%_}
                            else
				flag=1
			    fi

			    stationName=${stationName/$rhp\_/}
			    stationName=${stationName/$rsi\_/}
                            if [ $flag -eq 1 ]; then
				flag=1
			    else
				stationName=${stationName/$misc\_/}
			    fi

			    echo Converting $stationName from netCDF back to ASCII ...
			    java -jar $javadir/CEOPcdf2stm.jar -i $newFile -o $dir > $dir/$stationName'_soil.log'
	
			done
			echo -e $counter 'stations converted for' $theFile '\n' >> soil_conversion.log
		      fi
       
		    fi
		fi
	    fi
	done
    fi
  
done   
echo Finished
echo -e 'Finished\n\n' >> soil_conversion.log