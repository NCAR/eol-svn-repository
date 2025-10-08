#!/bin/bash

: '

Title:
flxDiff

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

function getCount2 {
    more $1 | grep $2 | wc -w
}

function getLine {
    more $1 | grep $2 | wc -l
}

function getChar {
    more $1 | grep $2 | wc -c
}

if [ flux_diff.log ] ; then
    echo -e '\n\n\n-------------------------------' >> flux_diff.log
    echo $(date) >> flux_diff.log
    echo -e '\n\n' >> flux_diff.log
else
    echo -e '-------------------------------' > flux_diff.log
    echo $(date) >> flux_diff.log
    echo -e '\n\n' >> flux_diff.log
fi

if [ $(find . -name flux_errors.log | wc -l) -ne 0 ] ; then
    echo ''
else
    touch flux_errors.log
fi

#Main for loop to iterate through the directories under the current
javadir='/net/work/CEOP/version2/netCDF_conversions/jarfiles/'
for dir in $(find ./ -type d) ; do
   len=${#dir}
   if [ $len -eq 2 -o $len -eq 3 ] ; then
      echo
   else
       echo -e 'Searching for files in' $dir ' ...\n' >> flux_diff.log
        #secondary for loop to find each .flx in the current $dir
 
	   for theFile in $dir/*.flx ; do
	    theFile2=${theFile/$dir/}
	    len2=${#theFile2}
	    if [ $len2 -eq 6 ] ; then
		echo
	    else
                 #check if $theFile is an original file that already has a $theFile_copy.flx in this directory
                 #ie. $theFile has already been converted
	             #set up variables for the next error check
	             #saves a len for $theFile's filename
		    original=${#theFile}
	             #saves a string for $theFile's filename with _copy replaced with nothing
		    bub=${theFile/_copy/}
		    bub2=${theFile/_nc.flx/}
	             #convertes the string to a len
		    altered=${#bub}
		    altered2=${#bub2}
	             #check if $theFile is a copy of an original file
		    if [ $original != $altered ] ; then
			echo 
		    else
			if [ $original != $altered2 ] ; then
			    echo 
			else
			    counter=0
			    counter2=0
			    actAvg=0
			    theoAvg=0
			    error=0
			    for newFile in $dir/*_flx.nc ; do
				let  "counter = $counter + 1"
				 flag=0
			    stationName=${newFile%_flx.nc}
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
			    rsi=${rsi%_*_*.flx}
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
				flag=2
				stationName=${stationName/$misc\_/}
			    fi

                                #check if $theFile is an original file that already has a $theFile_copy.flx in this directory
                                #ie. $theFile has already been converted
				if [ $(find $dir -name $stationName\_flux.diff) ] ; then
				    echo NOT DIFFING $theFile , Already Diffed
				    echo -e 'NOT DIFFING' ${theFile/$dir/} ', Already Diffed\n' >> flux_diff.log
				else
				    let "counter2 = $counter2 + 1"
	  
		    
		                    #diff the converted-back file and the _copy of the orig
			    
				    stationFile=$rhp\_$rsi\_$stationName\_\*\_nc.flx
				    if [ $(find $dir -name $stationFile | wc -l) -ne 0 ]; then
					echo
				    else
					stationFile=$rhp\_$rsi\_$misc\_$stationName\_\*\_nc.flx
				    fi
				    echo DIFFING $stationFile ...
					#echo diffing ${theFile%.flx}_copy.flx and $dir/$stationFile searching for $stationName
					diff ${theFile%.flx}_copy.flx $dir/$stationFile | grep $stationName > $dir/$stationName'_flux.diff'

					stationDiff=$dir/$stationName'_flux.diff'

					#if [ $(stat -c%s $stationDiff) -ne 0 ]; then
					    #echo Trying to solve diff problems ...
					    #altLine=$(more $dir/flux.log | grep 'Altitude difference for '$stationName)
					    #altPrev=${altLine/'WARNING: Altitude difference for '*' - previous: '}
					    #altPrev=${altPrev/' current'*/}
					    #altCurr=${altLine/'WARNING: Altitude difference for '*' - previous: '*' current: '/}
					    #altCurr=${altCurr%'.'}
					    #cp ${theFile%.flx}_copy.flx ${theFile%.flx}_copy_changed.flx
					    #occurences=$(grep -c $altPrev ${theFile%.flx}_copy_changed.flx)
					    #locations=$(grep -on $altPrev ${theFile%.flx}_copy_changed.flx)
					    #i=0
					    #while [ $i -lt $occurences ]; do
						#tempString=${locations#':*'}
						
						#tempArray[$i]=${locations
						#let "i = $i + 1"
					    #done
					    
                                            #arg='s/\($stationName [^ ][^ ]* [^ ][^ ]* \)$altPrev/\1$altCurr/'
					    #arg=s/$altPrev/$altCurr/
					    #sed -i '$arg' ${theFile%.flx}_copy_changed.flx

					    #lines=$(grep -c $stationName ${theFile%.flx}_copy.flx)
					    #j=0
					    #touch ${theFile%.flx}_copy_changed.flx
					    #while [ $j -lt 3 ]; do
						#total=$(more ${theFile%.flx}_copy.flx | grep $stationName)
						#startPos=$(expr index "$total" $rhp)
						#let "startPos = $startPos - 34"
						#endPos=$startPos
						#let "endPos = $endPos + 295"
						#cutLine=${total:$startPos:295}
						#echo cutLine is $cutLine
						#let "j = $j + 1"
					    #done
					    #diff ${theFile%.flx}_copy_changed.flx $dir/$stationFile | grep $stationName > $dir/$stationName'_flux_changed.diff'
					    #stationDiff=$dir/$stationName'_flux_changed.diff'
					    #if [ $(getCount2 $stationDiff) -lt  $(getCount2 $dir/$stationName'_flux.diff') ]; then
						#copyFile=${theFile/$dir/}
						#copyFile=${copyFile%.flx}_copy_changed.flx
						#echo keeping ...
					    #else
						copyFile=${theFile/$dir/}
						copyFile=${copyFile%.flx}_copy.flx
						##rm $dir/$stationName'_flux_changed.diff'
						#rm ${theFile%.flx}_copy_changed.flx
						stationDiff=$dir/$stationName'_flux.diff'
					    #fi
					#fi

					echo -e 'DIFFING '$copyFile ' against ' $stationFile ' ...\n' >> flux_diff.log
					copySize=$(stat -c%s $dir/$copyFile)
					stationDiffSize=$(stat -c%s $stationDiff) 
					copyWordCount=$(getCount2 $dir/$copyFile $stationName)
					copyLineCount=$(getLine $dir/$copyFile $stationName)
					stationWordCount=$(getCount2 $dir/$stationFile $stationName)
					stationLineCount=$(getLine $dir/$stationFile $stationName)
					stationSize=$(stat -c%s $dir/$stationFile)
					
					if [ $copySize -gt 0 ]; then
					    let "actAvg = $actAvg + $stationSize"
					fi
					echo $copyFile "s original size = " $copySize >> flux_diff.log
					echo $stationName "s converted size = "$stationSize >> flux_diff.log
					echo $stationName'_flux.diffs size = ' $stationDiffSize >> flux_diff.log
					if [ $stationDiffSize -ne 0 ] ; then
					    echo -e '=========Diff size not 0==========\n' >> flux_diff.log
					    error=1
					fi
					if [ $stationWordCount -lt $copyWordCount ] ; then
					    echo -e '=========Stations word count is wrong=========\n' >> flux_diff.log
					    error=1
					else
					    echo -e 'Stations word count is okay\n' >> flux_diff.log
					fi
					if [ $stationLineCount -lt $copyLineCount ] ; then
					    echo -e '==========Stations line count is wrong=========\n' >> flux_diff.log
					    error=1
					else
					    echo -e 'Stations line count is okay\n' >> flux_diff.log
					fi

					if [ $error -eq 1 ]; then
					    echo 'Look at '$stationDiff '...\\n' >> flux_errors.log
					fi
					
					#echo $(diff ${theFile%.flx}_copy.flx $dir/$stationFile | grep $stationName) >> flux_diff.log

				fi
			    done
			            if [ $counter -eq 0 ] ; then
					counter=1
				    fi
				    if [ $counter2 -eq 0 ] ; then
					counter2=1
				    fi
			    	    let "actAvg = $actAvg / $counter2"
				    let "theoAvg = $copySize / $counter"
				    echo -e $counter 'stations were converted for' $theFile '\n' >> flux_diff.log
				    echo -e $counter2 'stations were diffed\n' >> flux_diff.log
				    echo -e 'Each converted back size should average ' $theoAvg '\n' >>flux_diff.log
				    echo -e 'Actual average is ' $actAvg '\n\n\n\n\n' >> flux_diff.log
			fi
		    fi
	    fi

	   done
     fi
done   
echo Finished
echo -e 'Finished\n\n' >> flux_diff.log
more flux_errors.log | xargs echo -e >> flux_diff.log
rm flux_errors.log