#!/usr/bin/perl

use strict 'vars';
use POSIX; # Perl interface to IEEE Std 1003.1
################################################################################
# Convert columnar ascii data with time in the first column and 0 or more
# lines of header to NASA Ames format ala
#
# http://badc.nerc.ac.uk/help/formats/NASA-Ames/na-for-dummies-1D.html
#
# so that asc2cdf can convert it to netCDF and the metadata will be
# preserved. Note that this code requires a config file detailing the
# structure of the header in the input file, the format of the time in the
# first column, and any metadata missing from the header.
################################################################################

#my $timetype = "offsetFromMidnight"; # Mission Time
my $timetype = "offsetFromSTART";
&main();

############
# main loop
############

sub main {
    # Usage - call txt2ames with a metadata config file
    if (@ARGV != 1) {
        print "Usage: txt2ames file.config\n";
        exit(1);
    }
    print "THIS ROUTINE ASSUMES TIME IS IN SECS SINCE START OF FILE!!!!!\n";

    # Define the metadata hash to contain all the metadata we need to process
    # columnar ascii input and generate NASA Ames formatted output.
    my %metadata = ();
    my %meta = ();
    my %charstart = ();
    my %charlen = ();
    &hashdef(\%metadata, \%meta, \%charstart, \%charlen);

    # List all the files to be processed into an array using Perl file glob fn.
    my @files = glob($metadata{filepattern});
    if (@files == ()) {
        print "There are no $metadata{filepattern} files in the processing ".
            "dir. Exiting\n";
        exit(1);
    }

    #Loop through the files and process each individually
    foreach my $file (@files) {

        print "Processing $file\n";

        # Parse metadata from the filename - THIS IS NOT GENERIC ENOUGH!
        my $type;
        my $timestep;
        my $flight;
        if ($file =~ /^$metadata{filemetadata}$/) {
	    $type = $2;
	    $timestep = $3;
	    $flight = $1;
	    #print $timestep." ".$flight."\n";
	    $metadata{timeStep} = $timestep."\n";
        }

        # Open the input file we are working on.
        open (FILE, $file);

        # Read the metadata from the file header
        my $first_dataline = 
            &read_ascii_header(\%metadata, \%meta, \%charstart, \%charlen, FILE);

        # Do a few sanity checks
        # Confirm that filename and name given in metadata match
        if ($metadata{filename} != "") {
	    if ($file != $metadata{filename}) {
	        print "WARNING: Filename and filename listed in header do".
                    " not match: $file, $metadata{filename}\n";
	        exit(1);
            }
        }

        # Open the output file
        my $outfile =
            $type."_RF".$flight."_".$metadata{year}.$metadata{month}.
	    $metadata{day}."_".$metadata{hour}.$metadata{min}.
	    $metadata{sec}."_".$timestep;

        open (OUTFILE, ">$outfile.ames") or die "Can't open $outfile.ames:$!\n";

        # Write the metadata header to the AMES file
        &write_ames_header(\%metadata, OUTFILE);

        # Read and write the data
        &write_data(\%metadata, FILE, OUTFILE, $first_dataline);

        # Close the input file
        close(FILE);
        close(OUTFILE);

        # Create a global attributes file for inclusion in the netCDF from the
        # comments array.
        open (ATTFILE, ">$file.globalatts");
        for (my $i = 0; $i < @{$metadata{comment}}; $i++) {
	    my @values = split / = /,${$metadata{comment}}[$i];
	    $values[0] =~ s/ /_/g;
            ${$metadata{comment}}[$i] = join('=',@values);
            print ATTFILE "${$metadata{comment}}[$i]";
        }
        close(ATTFILE);

        # Convert the ames file to netCDF
        chomp $metadata{timeStep};
        print "asc2cdf -r $metadata{timeStep} -g $file.globalatts ".
            "-a $outfile.ames $outfile.nc\n";
        system("asc2cdf -r $metadata{timeStep} -g $file.globalatts ".
	    "-a $outfile.ames $outfile.nc");

        print "\n\n";

        # Define the metadata hash to contain all the metadata we need to process
        # columnar ascii input and generate NASA Ames formatted output.
        %metadata = ();
        &hashdef(\%metadata, \%meta, \%charstart, \%charlen);
    }
}

sub read_ascii_header {
    # This subroutine parses the header, if extant, in a data file and
    # stores the parsed metadata to the %metadata hash. It also checks to
    # see if those values were already populated from the config file and
    # warns the user of any conflict.
    my ($metadata, $meta, $charstart, $charlen, $fhandle) = @_;
    my $header;

    # Read the header until the first data line is encountered, populating
    # metadata hash as you proceed.
    while ($header = <$fhandle>) {
	my $header_processed = 0;
        # This pattern is designed to match data lines but NOT header lines. 
	if ($header !~ /^[ \.,0-9-\re\t]+$/) {
	    $header =~ s/\r//;
	    if ($header =~ /^$/) {
	        # Found a blank line in header, skip it.
	        print "Found a blank line in header\n";
	        next;
	    }
	    print $header;
	    #chomp $header;
	    foreach my $key (sort keys %$meta) {
		# The expected patterns for this header are defined in the
		# config file in the hash %meta - one pattern per AMES
		# format header element. Loop through the keys and check if
		# the current header line matches. Handle single elements
		# and array elements in two separate blocks.

		# Handle Array elements in config hash, e.g. the AMES format 
		# can have many lines of comment, so there are stored in 
		# an array with the key "comment"
		if (ref($meta->{$key}) eq 'ARRAY') {
		    # Loop through the patterns in this array
	            foreach my $item ( @{$meta->{$key}} ) {
			# If the header matches a %meta array element
	                if ($header =~ /^($item)/) {
			    # Parse the metadata from the header
		            my $val = (split $1,$header)[1];
			    # Save it to the metadata array
			    if ($key == 'comment') {
				# For comments, save the entire line
			        push @{$metadata->{$key}}, $header;
			    } else {
				# Else just save the metadata portion
			        push @{$metadata->{$key}}, $val;
			    }
			    #for (my $i = 0; $i < @{$metadata->{$key}}; $i++) {
			    #    print "$key *** ${$metadata->{$key}}[$i]\n";
			    #}
			    $header_processed = 1;
			}
	            }
		# Handle string elements:
		} else {
		    # If the header contains the string pattern for this
		    # key, save the header info to the metadata hash for
		    # printing to the AMES header later.
	            if ($header =~ /^($meta->{$key})/) {
			my $pattern = $1;
			if ($key =~ 'varNames' || $key =~ 'units') {
	                    $header =~ s/\n//;
			    my @vars = (split ' ',$header);
			    shift @vars; # Remove the first column (time)
			    push @{$metadata->{$key}}, @vars;
			    $metadata->{numVars} = @{$metadata->{$key}};
			    print "numVars $metadata->{numVars}\n";
			    for (my $i = 0; $i < @{$metadata->{$key}}; $i++) {
				${$metadata->{$key}}[$i] =~ s/^ *//;
				${$metadata->{$key}}[$i] =~ s/ *$//;
				print "$key *** ${$metadata->{$key}}[$i]\n";
			    }
			} elsif ($key =~ 'dates') {
		            my $startdate = (split $pattern,$header)[1];
			    # Now parse the date components from the date
			    $metadata->{year} =
			    substr($startdate,$charstart->{yr},$charlen->{yr});
			    $metadata->{month} =
			    substr($startdate,$charstart->{mo},$charlen->{mo});
			    $metadata->{day} =
			    substr($startdate,$charstart->{dy},$charlen->{dy});
			    $metadata->{hour} =
			    substr($startdate,$charstart->{hr},$charlen->{hr});
			    $metadata->{min} =
			    substr($startdate,$charstart->{mn},$charlen->{mn});
			    $metadata->{sec} =
			    substr($startdate,$charstart->{sc},$charlen->{sc});
			    $metadata->{subsec} =
			   substr($startdate,$charstart->{subs},$charlen->{subs});
			    $metadata->{sec_since_midnight} = $metadata->{sec}
			    + $metadata->{subsec}/100.
			    + $metadata->{min}*60 + $metadata->{hour}*3600;


			    #if ($metadata->{subsec} != 0) {
			    #	print "non-zero subsec: $metadata->{subsec}!!! Update code\n"; 
			    #	exit(1);
			    #}
			    
			    # Find the processing date (today)
			    my $now = strftime "%Y %m %d",localtime();

			    # Save the dates to the dates key
			    $metadata->{$key} = "$metadata->{year} ".
			        "$metadata->{month} $metadata->{day} $now\n";
			    $metadata->{timeVar} = 
			        "Time (seconds since ".$metadata->{year}."-".
			        $metadata->{month}."-".$metadata->{day}." ".
				"00:00:00 +00:00)\n";
				#$metadata->{hour}.":".$metadata->{min}.":".
				#$metadata->{sec}.
				#print "$key *** $metadata->{$key}";

			} else {
		            my $val = (split $pattern,$header)[1];
		            $metadata->{$key} = $val;
			    #print "$key *** $metadata->{$key}";
			}
			$header_processed = 1;
		    }
		}
	    }

	    # Warn user of unexpected lines in header
	    if ($header_processed == 0) {
		print "Unexpected line in header: $header\n";
		exit(1);
	    }
	} elsif ($header =~ /^ *\r$/) {
	    # Found a blank line in header, skip it.
	    #print "Found a blank line in header\n";
	    next;
	} else {
	    # Done reading header. Now header contains first data line.
	    last;
	}
    }

    # Now header contains first data line, so return that to main program.
    return $header;
}

sub write_ames_header {
    # This subroutine writes the AMES header from the metadata hash info.
    # It also check for each values existence before writing.
    my @header_lines = ();

    my ($metadata, $fhandle) = @_;

    my @metavars = ("PI","institute","instrument","project");
    for (my $i = 0; $i < @metavars; $i++) {
	if ($metadata->{$metavars[$i]} =~ //) {
	    print "ERROR: NO $metavars[$i] GIVEN IN FILE!!\n"; exit(1);
	} else {push @header_lines, "$metadata->{$metavars[$i]}";}
    }

    # "1 1"
    push @header_lines, "1 1\n"; 

    foreach my $metavar ("dates","timeStep","timeVar") {
        if ($metadata->{$metavar} =~ //) {
	    print "ERROR: NO $metavar GIVEN IN FILE!!\n"; exit(1);
        } else {push @header_lines, "$metadata->{$metavar}";}
    }

    # numVars
    foreach my $metavar ("numVars") {
        if ($metadata->{$metavar} =~ //) {
            print "ERROR: NO $metavar GIVEN IN FILE!!\n"; exit(1);
        } else {push @header_lines, "$metadata->{$metavar}\n";}
    }

    # scaling factor
    foreach my $metavar ("scaleFactor","missingVal") {
        if ($metadata->{$metavar} =~ //) {
	    print "ERROR: NO $metavar GIVEN IN FILE!!\n"; exit(1);
        } else {
	    my $line = "";
            for (my $i=0; $i < $metadata->{numVars}; $i++) {
	        $line .= "$metadata->{$metavar} ";
            }
	    $line .= "\n";
            push @header_lines, $line;
        }
    }

    # longnames and units
    foreach my $key (@{$metadata->{varNames}}) {
	print "$key ${$metadata->{varlist}}{$key}[2]\n";
        push @{$metadata->{longNames}}, ${$metadata->{varlist}}{$key}[2];
        if ($metadata->{units}[0] =~ /^$/) {
            if (${$metadata->{varlist}}{$key}[1] =~ /^.*\((.*)\).*$/) {
	      my $unitval = $1;
	      print "$key $unitval\n";
              push @{$metadata->{units}}, $unitval;
	    }
	}
    }
    if ($metadata->{longNames} =~ //) {
	print "ERROR: NO longNames GIVEN IN FILE!!\n"; exit(1);
    } elsif ($metadata->{units}[0] =~ /^$/) {
	print "ERROR: NO units GIVEN IN FILE!!\n"; exit(1);
    } else {
        for (my $i=0; $i < $metadata->{numVars}; $i++) {
	    push @header_lines, "$metadata->{longNames}[$i] ($metadata->{units}[$i])\n";
        }
    }

    #
    push @header_lines, "0\n";

    # comments
    my $len =  @{$metadata->{comment}};
    if ($metadata->{commentLen} == "") {
        push @header_lines, "$len\n";
    } elsif ($metadata->{commentLen} != $len) {
	print "ERROR: mismatched commentLen GIVEN IN FILE!!\n"; 
	print "$metadata->{commentLen} $len\n";
	exit(1);
    } else {
        push @header_lines, "$len\n";
    }
    for (my $i=0; $i < @{$metadata->{comment}}; $i++) {
	push @header_lines, $metadata->{comment}[$i];
    }

    # column headings
    my $line = "";
    $line .= "Time";
    if ($metadata->{varNames} =~ //) {
	print "ERROR: NO varNames GIVEN IN FILE!!\n"; exit(1);
    } else {
        for (my $i=0; $i < $metadata->{numVars}; $i++) {
	    #print $fhandle " $metadata->{varNames}[$i]"; #User names
            $line .= " ${$metadata->{varlist}}{$metadata->{varNames}[$i]}[0]";
        }
        $line .= "\n";
    }
    push @header_lines, $line;

    #Specifies the number of lines of header and the distinct format under AMES.
    my $header_lines = @header_lines+1;
    unshift @header_lines, "$header_lines 1001\n"; 
    print $fhandle @header_lines;

}

sub write_data {
    ### THIS ROUTINE ASSUMES TIME IS IN SECS SINCE START OF FILE!!!!!

    # Save time of first rec to subtract from all others
    # Time in these files is a mishmash. Start time is UTC of
    # first rec. MissionTime is time since start of mission which 
    # we don't know. So add record number, not MissionTime, to 
    # start UTC to get record UTC.

    my ($metadata, $ihandle, $fhandle, $line) = @_;
    my $firstTime = -999;
    
    do {
	# KLUDGE for 1000hz PVM data
	if ($line =~ /^ *-0.001/) {$line = <$ihandle>;}

	$line =~ s/\r//;
	chomp $line;
	(my @params) = split(/\s+/, $line);

        # Grab the MissionTime
        # remove the MissionTime from the remaining list
	my $date = shift @params; 

	if ($firstTime == -999) {$firstTime = $date;}

	# If offset from start rather than MissionTime, add the offset from
	# starttime. Default is offsetFromMidnight
        if ($timetype == "offsetFromSTART") {
	    $date = $date+$metadata->{sec_since_midnight}-$firstTime;
        }

	# replace missing values in the remaining params
	foreach my $param (@params) { 
	    if ($param =~ /^$/) {$param = $metadata->{missingVal};}
	}
	print $fhandle $date." ".join(' ',@params)."\n"; 

    } while ($line = <$ihandle>);
}

sub hashdef() {
##### This is a kludge. Change to READ from the config file later!!! #####
    my $metadata = shift;
    my $meta = shift;
    my $charstart = shift;
    my $charlen = shift;

    ### BE SURE TO PUT \n AT END OF LINES YOU HARDCODE ###
    # Read the metadata config file
    $metadata->{filepattern} = "TO[0-9][0-9]_MRLA3_10*Hz.txt";
    $metadata->{filemetadata} = 'TO([0-9][0-9])_(MRLA3)_(10*)Hz.txt';
    # AMES format requirements
    # PI Name (last, first)
    $metadata->{PI} = "";
    # PI institute
    $metadata->{institute}= "NCAR/RAF\n";
    # Instrument name
    $metadata->{instrument} = "";
    # Project name
    $metadata->{project} = "Physics Of Stratocumulus Tops (POST)\n";
    # Date of observations Date file produced (YYYY MM DD YYYY MM DD);
    $metadata->{dates} = "";
    # Size of intervals in time (use 0.0 if non-uniform)
    $metadata->{timeStep} = "";
    # Name for time variable with units 
    # i.e. "Time (seconds since yyyy-mm-dd hh:mm:ss +00:00)"
    $metadata->{timeVar} = "";
    #Number of variables for each time point (d)
    $metadata->{numVars} = "";
    #Scaling factors for these variables (1.0)
    $metadata->{scaleFactor} = "1.0";
    #Missing values for these variables (-32767)
    $metadata->{missingVal} = "-32767";
    #Name of variables i.e. "wind speed (m/s)"
    $metadata->{varNames} = [];
    $metadata->{longNames} = [];
    $metadata->{units} = [];
    #Number of lines of comments to be used
    $metadata->{commentLen} = "";
    #Comment line 1-N
    $metadata->{comment} = [];
    
    # Hash to convert given var names to to the RAF abbreviation as given in
    # the "FINAL POST Twin Otter Output Parameter List (10/25/2008)"
    $metadata->{varlist} = {
        'MR' => ['MRLA3','mr (g/kg)','Fast Lyman-Alpha UV hygrometer'],
    };
    # If filename exists in header, store it here so we can confirm it matches
    # name of file.
    $metadata->{filename} = "";
    # End metadata config file
    
    $meta->{PI} = "DATA CONTACT = ";
    $meta->{instrument} = "INSTRUMENT = ";
    $meta->{filename} = "FILE NAME = ";
    $meta->{comment} = ["LOCATION = ","PLATFORM = ", "DATA VERSION = ", "REMARKS = "];
    $meta->{dates} = "DATA COVERAGE = START:";
    $meta->{varNames} = "MissionTime";
    $meta->{units} = "     ";
    
    $charstart->{yr} = 0;
    $charstart->{mo} = 4;
    $charstart->{dy} = 6;
    $charstart->{hr} = 8;
    $charstart->{mn} = 10;
    $charstart->{sc} = 12;
    $charstart->{subs} = 15;
    
    $charlen->{yr} = 4;
    $charlen->{mo} = 2;
    $charlen->{dy} = 2;
    $charlen->{hr} = 2;
    $charlen->{mn} = 2;
    $charlen->{sc} = 2;
    $charlen->{subs} = 3;

}
