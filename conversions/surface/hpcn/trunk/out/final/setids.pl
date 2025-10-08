#!/bin/perl -w

#   setids.pl
#
#   Pulls data line by line from the High Plains *0qc output
#   files, and writes the station number for each station
#   as part of the station name on the line.
#
# ds 8/6/96

# Read through all the *.0qc files in this directory.
opendir(FILEDIR, ".") || die "Can't open the directory\n";
@files = readdir(FILEDIR);
@files = sort(@files);
closedir(FILEDIR);

# open(STNLIST, "../../exe/hplains.stns") || die "Can't open station list\n";
open(STNLIST, "hplains.stns") || die "Can't open station list\n";
@stationlist = <STNLIST>;
close (STNLIST);

$stncount = @stationlist;
# print "Stations count = $stncount, last index = $#stationlist\n";

@stationfields = ();

# Read through the files
foreach $infile (@files) {
    if ($infile =~ /\.0qc$/) {
        open(INFILE, $infile) || die "Couldn't open $infile\n";
        $outfile = $infile;
        $outfile =~ s/\.0qc$/\.pqc/;
        open(OUTFILE, ">$outfile") || die "Couldn't open $outfile\n";
        print "Now reading in the data from $infile \n";
        while ($input_line = <INFILE>) {
            $station = substr($input_line, 41, 15);
            $name = sprintf("%-15.15s", $station);
            $lat = substr($input_line, 56, 11); 
            $lon = substr($input_line, 67, 11);
            $lon *= -1.0;
            
            $i = 0;
            do {
                $tomatch = substr($stationlist[$i++], 12, 27);
                $nametomatch = sprintf("%-15.15s", $tomatch);
#               print "name=$name, to match=$nametomatch, index=$i\n";
            } while ($name ne $nametomatch && $i != $stncount);

            if ($i == $stncount && $name ne $nametomatch) {
                print "\tERROR: Couldn't match the $name station!!\n";
            } else {
#               print "\tMatched $name with $nametomatch\n";
            }
            
            $i--;
            
            if ($name eq $nametomatch) {
                $matchlat = substr($stationlist[$i], 45, 8);
                $matchlon = substr($stationlist[$i], 53, 9);
#               print "now checking lat/lon: $matchlat with $lat, $matchlon with $lon \n\n";

                if (($matchlat != $lat) || ($matchlon != $lon)) {
                    do {
                        $tomatch = substr($stationlist[++$i], 12, 27);
                        $nametomatch = sprintf("%-15.15s", $tomatch);
#                       print "A lat/lon mixmatch: name=$name, to match=$nametomatch, index=$i\n";
                    } while ($name ne $nametomatch && $i != $stncount);
                
                    if ($i == $stncount && $name ne $nametomatch) {
                        print "ERROR: Couldn't match the $name station!!\n";
                    } else {
#                       print "\tMatched $name with $nametomatch\n";
                    }
                        
                }
                @stationfields = split(" ", $stationlist[$i]);
                $toinsert = sprintf("HPLAINS    %03d_%-11.11s", $stationfields[1], $name);
            } else {
                print "ERROR: You should never get here!!\n";
            }

            die "ERROR: Nothing to insert!\n" if (!(defined($toinsert)));
            
            substr ($input_line, 30, 26) = $toinsert;
            print OUTFILE ("$input_line");
            undef ($toinsert);
        }
    }
    close (INFILE);
    close (OUTFILE);
}
