#!/bin/perl -w
# This program will build a station list.  The format is 
#   Network ID, Station ID, lat, lon, elev, occ, first date, last date
# space seperated

print "Enter directory name including trailing /_ ";
chop($DIR = <STDIN>);
print "Beginning station list\n";
$OFILE = "stn_date_list.txt";

# Read through all the *.qcf files in this directory.
opendir(FILEDIR, $DIR) || die "Can't open $DIR: $!\n";
@files = readdir(FILEDIR);
@files = sort(@files);
closedir(FILEDIR);

$num_stns = 0;
@stations = \0;
@ftime = \0;
@ltime = \0;
@latlon = \0;

# Read through the all files
    foreach $file (@files)
    {
	if ($file =~ /[0-9]\.qcf$/)
	{
	    # Open the file for reading.
	    $filename = $DIR.$file; # concatenate directory w/ file name
	    if (!open(INFILE, $filename))
	    {
		print "Sorry, could not open $filename\n";
		next;
	    }

	    # Read through the file
	    # All files are sorted by time, so the first time we encounter 
	    # a station will be the earliest time, and the last time we 
	    # encounter that same station will be latest time possible.
	    print "Now reading $filename\n";
	    while ($line = <INFILE>)
	    {
		$sname = substr($line, 30, 26);	# get network/station id

		$found = 0;
		if ($num_stns > 0)
		{
		    $i = 0;
		    $found = 0;
		    foreach $stn (@stations)
		    {
			if ($sname =~ $stn)
			{
			    $found = 1;
			    $ltime[$i] = substr($line, 0, 14);
			}
			$i++;
		    }
		}
		if (!$found)	# save info for this station
		{
		    $stations[$num_stns] = $sname;
		    $ftime[$num_stns] = substr($line, 0, 14);
		    $ltime[$num_stns] = substr($line, 0, 14);
		    $latlon[$num_stns] = substr($line, 56, 22);
		    $occ[$num_stns] = substr($line, 80, 3);
		    $elev[$num_stns] = substr($line, 84, 7);
		    $num_stns++;
		}

	    }			# end while (read loop)
	    close(INFILE);
	}			# end if file match
    }				# end foreach file
# Now print what we have to a file.
$filename = $DIR.$OFILE;
open(LIST, ">$filename") || die "Can't open $filename: $!\n";
print "Writing file $filename\n";

$i = 0;
foreach $stn (@stations)
{
    print LIST "$stations[$i] ";
    print LIST "$latlon[$i] ";
    print LIST "$elev[$i] ";
    print LIST "$occ[$i] ";
    print LIST "$ftime[$i] ";
    print LIST "$ltime[$i]\n";
    $i++;
}
close (LIST);
