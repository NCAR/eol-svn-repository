!/usr/bin/env python

import Nio
import datafile
import genHTML
import string
import re
import dircache
import commands

#from datetime import timedelta, datetime
from datetime import datetime

class findRAF(datafile.findFiles):

    def FlightReport(self,FltNum,Proj):
	path="/net/www/docs/raf/Projects/"+Proj+"/Flight_Reports/"
	file = FltNum + '.html'
	state = self.find_file(path,file)
	print "Flight Report " + state + " for flight " + FltNum
	if state is "exists":
            ref = html.href('Flight_Reports/'+file,"Report")
	    return ref
	else:
	    return 'No Report'

    def MissionSummary(self,FltNum,Proj,platform):
        """

        input: Flight Number
	You can't match on date because the date of the summary is not always
	the same as the date of the flight. But every summary contains the flight
	number so match on that.
        """
	root="/net/web/catalog/html/"
        path=string.lower(Proj)+"/report/"+platform+"/"

	# Loop through all date subdirs and find all PDF files.
	# datelist contains the names of all the date directories in the
	# report/<platform> subdir.
	subdirlist = dircache.listdir(root+path)	

	print "Looking for Mission Summary"
	for i in range(len(subdirlist)):
	    subdir = subdirlist[i]
	    filelist = dircache.listdir(root+path+subdir)
	    #There may be more than one file in the subdir.  We only want the pdf
	    #files with names of the form
	    # "report.<platform>.<date>.flight_summary.pdf"
	    for file in filelist:
		print "Checking file "+subdir+" "+file
		if re.search("^report.*.flight_summary.pdf",file):
		    print "Found a file!"
		    #We found a pdf flight summary report.  Now check it to see
		    #which flight it is for.
	            flight = commands.getstatusoutput(\
			    "pdftotext "+root+path+"/"+subdir+"/"+file+" - \
			    | grep ' [RFT]F[0-9][0-9]'")
		    flight = flight[1]
		    flight = str(flight)
		    if FltNum in flight:
		        print "File is for flight " + FltNum
                        print "Mission Summary " + state + " for flight " + FltNum
			#print path + file
	    		ref = \
	    		    html.href('http://catalog.eol.ucar.edu/'+path+ \
			    "/"+subdir+"/"+file, "Mission Summary")
			subdirlist[i:i+1] = []
			return ref

	else:
	    return 'N/A'

    def QAreport(self,Proj,FltNum):
        
	path="/net/www/docs/raf/Projects/"+Proj+"/"
        file = "QA.html"
        state = self.find_file(path,file)
        print "QA Report " + state
	#print path + file
	if state is "exists":
	    ref = \
	    html.href('QA.html#'+FltNum, "QA Report")
	    return ref
	else:
	    return 'N/A'

    def DataDownloadCatalog(self,ProjNum,rate):
	"""

	input: 3-digit project number, string containing rate
	(either "lrt","hrt", or "movie")
	"""
	# Build the complete path and name of the file we are looking for.
	path = "/net/www/docs/raf/Catalog/"
	file = "taplog."+rate+"."+ProjNum+".html"

	# Check to see if the file exists.
	state = self.find_file(path,file)
	print rate + " data Download Catalog " + state + \
	    " for project " + ProjNum
	#print path + file

	reflist = []
	if state is "exists":
	    # Go through the file and find all lines containing a FltNum
	    # parse out the href string, and return it.

	    # Open the file for reading
            catalog = open(path+file, 'r')	# open for reading only
	    lines = catalog.readlines()	# returns a list containing all lines in the file
	    catalog.close()

	    for line in lines:
		#print line
		match = re.search(r'(<a href.*[FRT]F[0-9][0-9].*>).*</a>',line)
		if match:
	    		ref = match.group(1)
			print ref
			reflist.append(ref)

	return reflist

    def SearchCatalog(self,ProjNum,rate,FltNum):
	"""

	input: 3-digit project number, string containing rate
	("catalog") and Flight Number to search for.
	"""
	# Build the complete path and name of the file we are looking for.
	path = "/net/www/docs/raf/Catalog/"
	file = "taplog."+rate+"."+ProjNum+".html"

	# Check to see if the file exists.
	state = self.find_file(path,file)
	print rate + " data Download Catalog " + state + \
	    " for project " + ProjNum
	#print path + file

	reflist = []
	if state is "exists":
	    # Go through the file and find the line containing FltNum
	    # Just return success, i.e. "exists"

	    # Open the file for reading
            catalog = open(path+file, 'r')	# open for reading only
	    lines = catalog.readlines()	# returns a list containing all lines in the file
	    catalog.close()

	    for line in lines:
		#print line
		match = re.search(r'(<a href.*'+FltNum+'.*>).*</a>',line)
		if match:
		    status = "exists"
		    break


	return status

#f.write(myfn.__doc__)

print "*** Run me on shiraz b/c nio installed there! ***\n"
# Create instances of a couple of class objects
find = datafile.findFiles()
parsenc = datafile.parseNetCDF()
html = genHTML.RAFhtml()
findraf = findRAF()

# Define some project constants
# In the long run, maybe these should all be added to and read from
# /jnet/local/projects/[f.ProjectName]/C130_[f.Aircraft]/Production/proj.info
# f.Aircraft = N130AR for the C130
ProjNum = "114"	#Read this from somewhere - where?
Year = "2007"
PI = "Alan Bandy"
plat = "C-130Q Hercules"
platform = string.join(["NSF/NCAR",plat,"(N130AR)"]," ")
Proj = "PASE"
ProjLongName = "Pacific Atmospheric Sulfer Experiment"
platform_catalog = "ncar_c-130"
platform_image = "/raf/pics/ps_c130fixa.jpg"
Lpath = "/net/www/docs/raf/Projects/"+Proj+"/"
Limage = "pics/"+Proj+"_logo.png"
Rpath = "/net/www/docs"
Rimage = "/raf/pics/RAF_final_sm.jpg"

ProjectNum = string.join([Year,ProjNum],'-')

# Open the output file
desc = "Flight Summary Information Matrix"
colNames=["RAF Flight No.","Start Date","Flight Times (UTC)", \
	"Mission","Instrument Anomalies *","Get Data","Whole-Flight Plots"]
colSpan=[1,1,1,1,1,1,4]
outfile = open('TestFltMatrix.html', 'w')	# w = write
# outfile = open('TestFltMatrix.html', 'a')	# a = append


# Define some images and confirm that they exist
# Path is relative to /net/www/docs/raf/Projects/[PROJECT]/ since that is
# where the resulting HTML file will be saved.
state = find.find_file(Lpath,Limage)
print "Image "+ Lpath + Limage + " " + state

# These paths are relative to the raf web root: /net/www/docs
state = find.find_file(Rpath,Rimage)
print "Image "+ Rpath + Rimage + " " + state

state = find.find_file(Rpath,platform_image)
print "Image "+ Rpath+ platform_image + " " + state


# Write the NetCDF title to the output file.
outfile.write("<html>")
title = "\n<head>\n\t" + html.title(Proj,ProjectNum,plat,desc) \
	+ '<script type="text/javascript" src = "measurementDescriptions.js"></script>' + \
	'\n</head>\n'
outfile.write(title)

# Start the body of the document
outfile.write("<body>\n")

# Put the RAF project-specific banner at the top
banner = \
html.banner(Limage,Proj,ProjectNum,ProjLongName,PI,platform,Rimage)
outfile.write(banner)

# Next put the title that appears on the webpage.
header = \
html.h2center(string.join([html.href("/raf/Bulletins/bulletin3.html", \
	html.image(platform_image,"Go to RAF Bulletin No. 3","middle")),desc]))
outfile.write(header)

# Create Flight Summary Information Matrix (an html table)
outfile.write('<table border width="100%">\n')
# Write table header row to output file
row = html.header_row(colNames,span = colSpan,bgcolor="#0070b0")
outfile.write(row)

# Loop through available rates and get listing of all catalog flight listings
rates = ['lrt','hrt','movie']
for rate in rates:
    if rate is "lrt":
	mss_retrieval_lrt = findraf.DataDownloadCatalog(ProjNum,rate)
    if rate is "hrt":
	mss_retrieval_hrt = findraf.DataDownloadCatalog(ProjNum,rate)
    if rate is "movie":
	mss_retrieval_movie = findraf.DataDownloadCatalog(ProjNum,rate)

print "Loop through flights"
for line in mss_retrieval_lrt:
    # use the LRT file as the source of RAF flights. This assumes that all
    # flights will be in the LRT files, but that there may not be HRT, Movie, or Camera entries
    # for all flights. ***I should search for files in HRT, Movie, or Camera that are not in LRT
    # as a sanity check.***
    print line
    match = re.search(r'.*([FRT]F[0-9][0-9]).*',line)
    flightnum_from_taplog =  string.lower(match.group(1))


    # open an RAF flight level production NetCDF file for the current flight
    path = "/scr/raf2/Prod_Data/"+Proj+"/"
    ncfile=Proj+flightnum_from_taplog+".nc"
    state = find.find_file(path,ncfile)
    if state is "exists":
        f = Nio.open_file(path+ncfile,"r")
    else:
        print "NetCDF file for "+ncfile+" does not exist."
        next

    # Confirm project name in nc file is same as project name expected.
    print "Processing Project "+Proj
    if f.ProjectName == Proj:
        print "project match"
    else:
        print "project mismatch"
        quit


    # determine the flight number from the global attribute in the netCDF
    # file
    if f.FlightNumber == flightnum_from_taplog:
        FltNum =  string.upper(f.FlightNumber)
        print 'Processing flight '+FltNum
    else:
	print "Flight Number mismatch"
	quit

    # Create a row containing all information for a single flight
    # Column 1: RAF Flight No.
    # Check if flight report exists for this flight
    file = findraf.FlightReport(FltNum,Proj)
    cols = [html.b(FltNum)+'<hr>'+file]

    # Column 2: Start Date and Column 3: Flight Times (UTC)
    startDate = f.FlightDate
    cols.append(startDate)
 ***This still need work***

    flightTimes = f.TimeInterval
    print flightTimes[0:8]
    print flightTimes[9:17]
    [start,end,date] = parsenc.parseFlightTimes(startDate,flightTimes)

    starttime = datetime(*start)
    endtime = datetime(*end)

    print startDate + ' ' + flightTimes
    timedelta =  endtime - starttime
    print "Length of flight was " + str(timedelta) + " hh:mm:ss"

    # reformat time delta to hours.hundredths.
    hours =  '%(hours)0.2f' % {'hours': timedelta.seconds / 3600.}

    col3=flightTimes[0:8]+'<br>'+flightTimes[9:17]+'<hr>'+ hours + " hours"
	    
    cols.append(col3)

    # Column 4: Mission
    col4 = findraf.MissionSummary(FltNum,Proj,platform_catalog)
    cols.append(col4)

    # Column 5: Instrument Anomalies
    col5 = findraf.QAreport(Proj,FltNum)
    cols.append(col5)
    # This is not used.  It returns a list of all the variables in the netCDF file.
    # I thought it might be useful for finding variables in QA.html because you could search
    # the text of QA.html under a given flight for any of the variables in varlist, and if they
    # exist, then link them in the table. However, I am not sure if all the variables in QA.html 
    # will always be in the variable list.  Maybe a better way is to grep the QA flight paragraph
    # for capitalized terms? ***This still need work***
    # varlist = parsenc.get_var_list(f)
    # print varlist

    # Column 6: Get Data 
    # Low-Rate
    getdata = "netCDF<br>"+line+"Low-Rate</a>"

    # High-Rate
    print 'Searching for '+FltNum+' in hrt catalog'
    hline = ''
    for line in mss_retrieval_hrt:
        match = re.search(r'(^.*'+FltNum+'.*$)',line)
	if match:
	    hline =  match.group(1)
	    print hline
	    break
    if hline:
	getdata = string.join([getdata,"<br>"+hline+"High-Rate</a>"],' ')

    # Movie
    print 'Searching for '+FltNum+' in movie catalog'
    mline = ''
    for line in mss_retrieval_movie:
        match = re.search(r'(^.*'+FltNum+'.*$)',line)
	if match:
	    mline =  match.group(1)
	    print mline
	    state = findraf.SearchCatalog(ProjNum,"camera",FltNum)
	    if state is "exists":
	        mline = string.join([mline,"Movie&nbsp;(mpeg4)</a>", \
			'<br>'+html.href('http://'+\
			'www.eol.ucar.edu/raf/Catalog/taplog.camera.'+\
			ProjNum+".html#"+FltNum,'Camera&nbsp;(jpg.tar)')],' ')
		print mline
    if mline:
	getdata = string.join([getdata,"<hr>"+mline],' ')

    cols.append(getdata)

    # Column 7-10: Whole Flight Plots
    plottypes = ["TD","ALT","XY","XYos","XYZ","XYZos"]
    plotlabels = ["Temp/Dew Point","GPS Altitude","X-Y Track", \
	    "<hr>X-Y Track (on station)","X-Y-Z Track","<hr>X-Y-Z Track (on station)"]
    path = "/net/www/docs/raf/pics/"+ ProjNum + "/" + FltNum + "/"
    index = 0
    while index < len(plottypes):
        param = plottypes[index]
        file = param + ".png"
        state = find.find_file(path,file)
	print "Whole-Flight Plots " + state + " for project " + \
        ProjNum + " for param " + param
	if state is "exists":
	    line = html.href('../../pics/'+ProjNum+'/'+FltNum+'/'+file,\
		    plotlabels[index])
	    if re.search(r'os',line):
		cols[len(cols)-1] = string.join([cols[len(cols)-1],line],'')
	    else:
	        cols.append(line)
	index = index +1

    # Write the current row to the output file
    row = html.normal_row(cols,align="center")
    outfile.write(row)

    # Close the RAF flight level production netCDF file for the current flight.
    f.close()

# Finish out the HTML file
outfile.write('</table>')
outfile.write('<table celspacing="10">')
n1 = '*&nbsp;&nbsp;&nbsp;Note:'
n2 = 'The "Instrument Anomalies" column is set up to display '+ \
    "a measurement's description in your browser's status bar when the "+ \
    "cursor is dragged over a measurement's variable name in this " + \
    "column. (You may need to adjust your browser settings for this to " + \
    "work.) A left click will take you to a detailed description " + \
    " of the anomalies."
row = html.normal_row([n1,n2],valign="top")
outfile.write(row)
outfile.write('</table>')
outfile.write('<hr><font size="2"> Last update: '+\
	str(datetime.utcnow())+' GMT</font>')
outfile.write("\n</body>")
outfile.write("\n</html>")

# Close the Flight Matrix html file
outfile.close()

