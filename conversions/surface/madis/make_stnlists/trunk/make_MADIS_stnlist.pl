#!/usr/bin/perl
use NetCDF;
use lib "/net/work/lib/perl/hpss";
use HPSS;

#------------------------------------------------------------------------------
# Change which variables to extract from the netCDF file. 
#------------------------------------------------------------------------------
our @fields = ("dataProvider","stationId", "latitude","longitude","elevation",
    "observationTime","precipAccum","temperature");

#------------------------------------------------------------------------------
# Change which variables to calcuate frequency for.
#------------------------------------------------------------------------------
our @freqObs = ("precipAccum","temperature");

#------------------------------------------------------------------------------
# Change where to download HPSS files to and where to write station lists.
#------------------------------------------------------------------------------
my $dnld_dir = "raw";
my $outdir = "out";


# Usage
if (! defined(@ARGV) || scalar(@ARGV) < 4 || ($ARGV[0]== '-d' && scalar(@ARGV) < 5)) {
    print "Usage: ./make_stnlist.pl [-d] year month day FEED\n";
    print "\t-d: download files from HPSS\n";
    print "\t    leave off if files are already downloaded\n";
    print "\tFEED is one of [CRN, HCN, MESONET1, NEPP, URBANET]\n\n";
    exit(1);
}

my $download;
# Parse command line arguments
if ($ARGV[0] == '-d') {
    # Download data
    $download = 1;
    shift @ARGV;
} else {
    # Data alreadu in dnld_dir. Don't download
    $download = 0;
}
my $year = $ARGV[0];
my $month = $ARGV[1];
my $day = $ARGV[2];
my $feed = $ARGV[3];

# Right now, only MESONET1 data is on HPSS
# Create archive data sets for the Climate Reference Network and the URBANET data
# sets.  The other two are still highly localized and not really useful
# enough at this stage to archive.
if (!(my @found = grep { $_ eq $feed } ("MESONET1","CRN","URBANET"))) {
    print "\tOnly MESONET1, CRN, and URBANET feeds are on HPSS\n";
    print "\tFor other feeds, copy files from /export/ldm/data/madis to\n";
    print "\t$dnld_dir/$feed and run without -d\n";
    exit(1);
}

# Change where to download HPSS files from
my $HPSSdir = "/EOL/operational/surface/MADIS/$feed";

#Combine dates to get date of filename on HPSS
my $filedate = sprintf("%04d%02d%02d",$year,$month,$day);
   
# Download requested files from HPSS
if ($download) { 
    &get_files_from_HPSS($filedate,$HPSSdir,"$dnld_dir/$feed");
}

# Get station info for the year, month, day requested
&get_station_info($filedate,"$dnld_dir/$feed","$outdir/$feed");


### END MAIN ###

#------------------------------------------------------------------------------
# Get station info for the year, month, day requested
#------------------------------------------------------------------------------
sub get_station_info {
    my $filedate = shift;
    my $dnld_dir = shift;
    my $out_dir = shift;

    # Get a listing of files to process
    # (Do this here instead of when download files so that downloading can be
    # skipped if already done.)
    opendir (OUT,$dnld_dir) or die "Can't open $dnld_dir:$!\n";
    my @files = grep{/$filedate/} readdir(OUT);

    my %freqs = ();
    # Confirm that the filedate requested exists in the downloaded files
    if (grep {$_ =~ /$filedate/} @files) {
        foreach my $infile (sort @files) {
            my $outFilename = "./".$out_dir."/".$infile.".out";
            $infile = "$dnld_dir/$infile";
            print "Getting station information from $infile\n";

	    # Open the netCDF file for reading 
	    # Get the dims, attributes, and values from the header.
            my $ncid = NetCDF::open($infile, NetCDF::NOWRITE);
	    my $ndims, my $nvars, my $natts, my $recdim, my $dimname, my $nrec;
            NetCDF::inquire($ncid,$ndims,$nvars,$natts,$recdim);
            NetCDF::diminq($ncid,$recdim,$dimname,$nrec);

            foreach my $obsName (@fields) {
	        # Determine the index of the variable we want to get.
                my $varcount = NetCDF::varid($ncid,$obsName);

	        # Determine the type and number of dimensions of the variable.
		my $varname,my $type,my $dims, my @dimids, my $atts;
                NetCDF::varinq($ncid,$varcount,$varname,$type,$dims,\@dimids,$atts);

	        # Initialize array to hold new data.
	        @{$varname}=();

	        # Download based on variable type and dimensions
                if ($type == 2 && $dims == 2) { #2-D char array
	            for (my $recs=0;$recs<$nrec;$recs++) {
			my $dimsize;
	                NetCDF::diminq($ncid,$dimids[1],$dimname,$dimsize);
                        my @start = ($recs,0);
                        my @count = (1,$dimsize);
		        my @values = ();
                        NetCDF::varget($ncid,$varcount,\@start,\@count,\@values);
	                foreach my $value (@values) {
		            if ($value != 0) { #Not a null char
	                        $$varname[$recs] .= chr($value);
	                    }
	                }
	            }
                    print "number of $obsName = ".scalar @{$varname}."\n";
                    #Put into an array named after this variable
                    @{$obsName} = @{$varname};
                } elsif (($type == 5 && $dims == 1) #1-D Float
		        || ($type == 6 && $dims == 1)) {#1-D Double
                    my @start = (0);
                    my @count = ($nrec);
		    my @values = ();
                    NetCDF::varget($ncid,$varcount,\@start,\@count,\@values);
                    print "number of $obsName = ".scalar @values."\n";
                    #Put into an array named after this variable
                    @{$obsName} = @values;
                } else {
	            print "Code not set up to download $dims-dimensional variables of type $type: $varname\n";
	            exit(1);
	        }
            }

            #-------------------------------------------------------------------
            # Get our station date from the filename as a sanity check
            #-------------------------------------------------------------------

            $_ = $infile;
            (my $yr, my $mon, my $day, my $hour, my $min) = 
	        /(\d{4})(\d{2})(\d{2})_(\d{2})(\d{2})/;
            print "yr = $yr, mon = $mon, day = $day, time = $hour:$min\n";

            my $date = "$yr/$mon/$day";
            print "Will write data for $date\n\n";

            #-------------------------------------------------------------------
            # Create station record with just the fields we requested by putting
            # the values together by location in the array and write to the 
	    # output file. The 0'th value is the variable name, so index of obs 
	    # starts at 1.
            #-------------------------------------------------------------------
   
            # Write records to hash so we can eliminate duplicates	
	    my %records = ();

            my $numObs = scalar @{$fields[0]}."\n";
	    my $i;
            for ($i=1; $i < $numObs; $i++) {
                #------------------------------------------------------------ 
    	        # Get ready to start next record.
                #------------------------------------------------------------ 
    	        my $record="";
    
                foreach my $obsName (@fields) {
		    if ($obsName eq "dataProvider" ||
		        $obsName eq "stationId" ||
			$obsName eq "latitude" ||
			$obsName eq "longitude" || 
			$obsName eq "elevation") {
		      $record .= sprintf("%s ", ${$obsName}[$i]);
		    }
                }
                #------------------------------------------------------------ 
                # Finish the line with a carriage return
                #------------------------------------------------------------ 
                $record .= "\n";
    
                #------------------------------------------------------------ 
    	        # Write record to hash if it isn't already there.
                #------------------------------------------------------------ 
    	        if (!(-e $records{$record})) {
    		    $records{$record} = 1;
		}

                #------------------------------------------------------------ 
		# Write observation time (s) to hash if it isn't already there.
		# If it is there, determine a frequency and add to reporting
		# frequency histogram for this station.
		# Do this for each requested observation.
                #------------------------------------------------------------ 
		foreach my $obs (@freqObs) {
		    if ((exists (${$obs}[$i]))
			&& (abs(${$obs}[$i] + 9999.0) > 1)) {
    	                if (!(exists $freqs{$record}{"time"}{$obs})) {
		            $freqs{$record}{"time"}{$obs}=${"observationTime"}[$i];
    	                } else {
		             my $freq =
			     ${"observationTime"}[$i]-$freqs{$record}{"time"}{$obs};
		            $freqs{$record}{"time"}{$obs}=${"observationTime"}[$i];
    	                    if (!(exists $freqs{$record}{$freq}{$obs})) {
			        $freqs{$record}{$freq}{$obs} = 1;
		            } else {
		                $freqs{$record}{$freq}{$obs} += 1;
		            }
		        }
	            }
	        }
    
            }
    
            #-------------------------------------------------------------------
            # If there is an output file with this name already, then we have 
	    # more times to add to it, so open with an append. Otherwise, open 
	    # a new file for output.
            #-------------------------------------------------------------------
            if (-e "$outFilename") {
                open (OUTFILE, ">>$outFilename") 
    	        || die "Can't open $outFilename for output";
            } else {
                open (OUTFILE, ">$outFilename")  
    	        || die "Can't open $outFilename for output";
            } 
    
    
    	    # Write hash to output file
    	    foreach my $record (sort keys %records) {
    	        print OUTFILE $record;
            }
        
        
            if ($i != $numObs) {
                printf ("*** Had %d number of observations in $infile, ");
    	    printf ("but was expecting %d!\n", $i, $numObs);
            }
                    
            close (OUTFILE);
        }

	# Write out the frequency hash.
	foreach my $obs (@freqObs) {
            my $freqFilename = "./".$out_dir."/"."$filedate.$obs.freq";
            open (OUTFILE, ">$freqFilename")  
   	        || die "Can't open $freqFilename.$obs.freq for output";
    
    
    	    # Write hash to output file
    	    foreach my $record (sort keys %freqs) {
    	        print OUTFILE $record;
    	        foreach my $freq (sort keys %{$freqs{$record}}) {
		    if ($freq ne "time") {
			if (defined $freqs{$record}{$freq}{$obs}) {
	                    print OUTFILE "$freq $freqs{$record}{$freq}{$obs}\n";
		        }
	            }
                }
    	        print OUTFILE "\n";
	    }
	}


    } else {
        # Notify user if file doesn't exist
        print "Requested file: $filedate.tar doesn't exist in $dnld_dir subdir\n";
        exit(1);
    }
}
#------------------------------------------------------------------------------
# Download requested files from HPSS
#------------------------------------------------------------------------------
sub get_files_from_HPSS {
    my $filedate = shift;
    my $HPSSdir = shift;
    my $outdir = shift;

    #HPSS directory to read files from
    my $HPSSdirectory = "$HPSSdir/".substr($year,0,4);

    # List the contents of the directory
    my @files = HPSS::ls($HPSSdirectory,"-1");

    # First line of listing is login info so remove that.
    my $HPSSlogin = shift(@files);
    
    if (grep {/$filedate.tar$/} @files) {
	# Download file if it exists
	my $dnldfile =  "$HPSSdirectory/$filedate.tar";
	my $outfile = "$outdir/$filedate.tar";
	print "FILE:".$dnldfile."\n";
	print "OUTFILE:".$outfile."\n";
    	HPSS::get(\$dnldfile,\$outfile);

    	print "untar and unzip $outfile\n";
	system("cd $outdir; tar -xvf $filedate.tar; rm $filedate.tar;");
        system("cd $outdir; gunzip $filedate*; rm $outdir/*gz; cd ..;");
    } else {
        # Notify user if file doesn't exist
        print "Requested file: $HPSSdirectory/$filedate.tar doesn't exist on HPSS\n";
        exit(1);
    }
}
