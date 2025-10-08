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
#
# Initial version to process POST PVM data. JAG
#	Config file not implemented
#
# Updated to process POST MRLA3 data.  JAG
#	Added subsec handling and timetype differentiation.
#	Config file still not implemented
# 
# Updated and generalized for cabin, CIP, etc. Aug, 2009 JAG
#	Implemented config file to handle ASCII file differences.
################################################################################
my $debug = 0;

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
    my $config_file = $ARGV[0];

    # Define the metadata hash to contain all the metadata we need to process
    # columnar ascii input and generate NASA Ames formatted output.
    my %metadata = ();
    my %meta = ();
    my %charlen = ();
    &hashdef(\%metadata, \%meta, \%charlen, $config_file);

    # List all the files to be processed into an array using Perl file glob fn.
    my @files = get_files_to_process($metadata{filepattern});

    #Loop through the files and process each individually
    foreach my $file (@files) {

        print "Processing $file\n";

        # Parse metadata from the filename
	(my $type, my $timestep, my $flight) = parse_filename($file,\%metadata);

        # Open the input file we are working on.
        open (FILE, $file);

        # Read the metadata from the file header
        my $first_dataline = 
            &read_ascii_header(\%metadata, \%meta, \%charlen, FILE);

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
            $type."_RF".$flight."_".$metadata{year}.
	    $metadata{month}.$metadata{day}."_".$metadata{hour}.$metadata{min}.
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
	print "*"x80;
        print "asc2cdf -r $metadata{timeStep} -g $file.globalatts ".
            "-a $outfile.ames $outfile.nc\n";
	print "*"x80;
        system("asc2cdf -r $metadata{timeStep} -g $file.globalatts ".
	    "-a $outfile.ames $outfile.nc");

        print "\n\n";

        # Define the metadata hash to contain all the metadata we need to process
        # columnar ascii input and generate NASA Ames formatted output.
	%metadata = ();
	&hashdef(\%metadata, \%meta, \%charlen, $config_file);
    }
}

sub read_ascii_header {
    # This subroutine parses the header, if extant, in a data file and
    # stores the parsed metadata to the %metadata hash. It also checks to
    # see if those values were already populated from the config file and
    # warns the user of any conflict.
    my ($metadata, $meta, $charlen, $fhandle) = @_;
    my $header;

    # Read the header until the first data line is encountered, populating
    # metadata hash as you proceed.
    while ($header = <$fhandle>) {
	my $header_processed = 0;
        # This pattern is designed to match data lines but NOT header lines. 
	if ($header !~ /^[ \.,0-9-\re\t]+$/ || $header =~ /^\t+\r$/) {
	    $header =~ s/\r//;
	    if ($header =~ /^\t*$/) {
	        # Found a blank line in header, skip it.
	        if ($debug) {print "Found a blank line in header\n";}
	        next;
	    }
	    print $header;
	    $header =~ s/\t*$//;
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
			    if ($debug) {
			      for (my $i = 0; $i < @{$metadata->{$key}}; $i++) {
			        print "$key *** ${$metadata->{$key}}[$i]\n";
			      }
			    }
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
			if ($debug) {
			    print "$pattern Key: $key \n";
			}
			# Save the variable names and units from the header
			# into the metadata hash.
			if ($key =~ 'varNames' || $key =~ 'units') {
	                    $header =~ s/\n//;
			    if ($debug) {
			       print "Splitting on $metadata->{dataSplitChar}:";
			       print $header."\n";
			    }
			    my @vars=
			        (split("$metadata->{dataSplitChar}",$header));
			    if ($debug) {
				print "test\n";
				foreach my $var (@vars) {print "$var\n";}
			    }
			    shift @vars; # Remove the first column (time)
			    push @{$metadata->{$key}}, @vars;
			    $metadata->{numVars} = @{$metadata->{$key}};
			    print "numVars $metadata->{numVars}\n";
			    # Remove leading and trailing spaces.
			    for (my $i = 0; $i < @{$metadata->{$key}}; $i++) {
				${$metadata->{$key}}[$i] =~ s/^ *//;
				${$metadata->{$key}}[$i] =~ s/ *$//;
				if ($debug) {
				    print "$key *** ${$metadata->{$key}}[$i]\n";
				}
			    }
			} elsif ($key =~ 'dates') {
			    # Parse dates from inside header ("DATA COVERAGE
			    # = START:" line)
		            my $startdate = (split $pattern,$header)[1];
			    # Now parse the date components from the date
			    my $century;
			    if ($charlen->{yr} == 2) {$century = "20";}
			        else {$century = "";}
			    my %charstart=();
			    $charstart{yr} = 0;
                            $charstart{mo} = $charstart{yr} + $charlen->{yr};
                            $charstart{dy} = $charstart{mo} + $charlen->{mo};
                            $charstart{hr} = $charstart{dy} + $charlen->{dy};
                            $charstart{mn} = $charstart{hr} + $charlen->{hr};
                            $charstart{sc} = $charstart{mn} + $charlen->{mn};
                            if ($charlen->{subs}) {
                                $charstart{subs} = $charstart{sc}+$charlen->{sc};
                            } else {
                                $charstart{subs} = undef;
                            }
    
			    $metadata->{year} = $century.
			    substr($startdate,$charstart{yr},$charlen->{yr});
			    $metadata->{month} =
			    substr($startdate,$charstart{mo},$charlen->{mo});
			    $metadata->{day} =
			    substr($startdate,$charstart{dy},$charlen->{dy});
			    $metadata->{hour} =
			    substr($startdate,$charstart{hr},$charlen->{hr});
			    $metadata->{min} =
			    substr($startdate,$charstart{mn},$charlen->{mn});
			    $metadata->{sec} =
			    substr($startdate,$charstart{sc},$charlen->{sc});
			    $metadata->{subsec} =
			   substr($startdate,$charstart{subs},$charlen->{subs});
			    $metadata->{sec_since_midnight} = $metadata->{sec}
			    + $metadata->{subsec}/100.
			    + $metadata->{min}*60 + $metadata->{hour}*3600;

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
			    if ($debug) {print "$key *** $metadata->{$key}";}

			} else {
		            my $val = (split $pattern,$header)[1];
		            $metadata->{$key} = $val;
			    if ($debug) {print "$key *** $metadata->{$key}";}
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
	chomp $metadata->{$metavars[$i]};
	if ($debug) {print "$i $metavars[$i] $metadata->{$metavars[$i]} \n";}
	if ($metadata->{$metavars[$i]} =~ /^$/) {
	    print "ERROR: NO $metavars[$i] GIVEN IN FILE!!\n"; exit(1);
	} else {push @header_lines, "$metadata->{$metavars[$i]}\n";}
    }

    # "1 1"
    push @header_lines, "1 1\n"; 

    foreach my $metavar ("dates","timeStep","timeVar") {
	if ($debug) {print "$metavar $metadata->{$metavar} \n";}
        if ($metadata->{$metavar} =~ /^$/) {
	    print "ERROR: NO $metavar GIVEN IN FILE!!\n"; exit(1);
        } else {push @header_lines, "$metadata->{$metavar}";}
    }

    # numVars
    foreach my $metavar ("numVars") {
	if ($debug) {print " $metavar $metadata->{$metavar} \n";}
        if ($metadata->{$metavar} =~ /^$/) {
            print "ERROR: NO $metavar GIVEN IN FILE!!\n"; exit(1);
        } else {push @header_lines, "$metadata->{$metavar}\n";}
    }

    # scaling factor
    foreach my $metavar ("scaleFactor","missingVal") {
        if ($metadata->{$metavar} =~ /^$/) {
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
    my $i = 0; # Index of this variable
    foreach my $key (@{$metadata->{varNames}}) {
	if ($debug) {print "$key longname ${$metadata->{varlist}}{$key}[2]\n";}
        push @{$metadata->{longNames}}, ${$metadata->{varlist}}{$key}[2];
	# Get the units for this varName from the varlist hash.
        my $unitval;
        if (${$metadata->{varlist}}{$key}[1] =~ /^.*\((.*)\).*$/) {
	    $unitval = $1;
	}
	# If the units are not given in the input file, get the units from
	# the varlist hash.
        if ($metadata->{units}[$i] =~ /^$/) {
	    print "$key $unitval\n";
            push @{$metadata->{units}}, $unitval;
	} else {
	# If the units ARE given in the input file, confirm that they match
	# the hash.
	    if ($metadata->{units}[$i] !~ /$unitval/i) {
	      print "WARNING: Units in data file, $metadata->{units}[$i] do ".
	      "not match units in config file $unitval for var $key\n";
	    }
	}
	$i++;
    }
    if ($metadata->{longNames} =~ /^$/) {
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
    if ($metadata->{varNames} =~ /^$/) {
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

	# Remove end of line chars and split line into params.
	$line =~ s/\r//;
	chomp $line;
	(my @params) = split(/\s+/, $line);

	# Grab the time (column 1) and remove the time from the remaining list
        # Default is to assume time is the MissionTime.
	my $date = shift @params; 
	if ($debug) {print "Processing record time $date\n";}

	# If YYMMDDHHMMSS (cabin files) then calc offsetFromMidnight
	if ($metadata->{timetype} =~ /^YYMMDDHHMMSS/) {
	    $date = 
	      substr($date,6,2)*3600+substr($date,8,2)*60+substr($date,10,2);
	    if ($debug) {print "Record time is equivalent to $date\n";}
	}

	if ($firstTime == -999) {$firstTime = $date;}

	# Other possible time formats (set in metadata config file)

	# If offset from start rather than MissionTime, add the offset from
	# starttime. Default is offsetFromMidnight
        if ($metadata->{timetype} == "offsetFromSTART") {
	    $date = $date+$metadata->{sec_since_midnight}-$firstTime;
        }


	# replace missing values in the remaining params
	foreach my $param (@params) { 
	    if ($param =~ /$metadata->{srcMissing}/) 
	        {$param = $metadata->{missingVal};}
	}
	print $fhandle $date." ".join(' ',@params)."\n"; 

    } while ($line = <$ihandle>);
}
sub get_files_to_process() {
    my $filepattern = shift;
    my @files;
    if (defined $filepattern) {
        @files = glob($filepattern);
        if (@files == ()) {
            print "There are no $filepattern files in the processing ".
                "dir. Exiting\n";
            exit(1);
        }
    } else {
	print "Please define filepattern in config file\n"; 
	exit(1);
    }
    return(@files);
}
sub parse_filename () {
    my $file = shift;
    my $metadata = shift;
    my $type;
    my $timestep;
    my $flight;
    if (defined $metadata->{filemetadata}) {
        if ($file =~ /^$metadata->{filemetadata}$/) {
	    $type = ${$metadata->{type}};
	    $timestep = ${$metadata->{timestep}};
	    $flight = ${$metadata->{flight}};
	    if ($debug) {
	        print $timestep." RF".$flight."\n";
	    }
	    $metadata->{timeStep} = $timestep."\n";
        } else {
	    print "ERROR: file does not match filemetadata pattern ".
	  	    " in config file\n";
	    print $metadata->{filemetadata}."\n";
	    exit(1);
	}
    } else {
        print "Please define filemetadata in config file\n";
        exit(1);
    }
    return($type,$timestep,$flight);
}

sub hashdef() {
##### This is a kludge. Change to READ from the config file later!!! #####
    my $metadata = shift;
    my $meta = shift;
    my $charlen = shift;
    my $config_file = shift;

    ### BE SURE TO PUT \n AT END OF LINES YOU HARDCODE ###

    # Read the metadata config file
    open (CFILE, $config_file) 
	or die "Can't open config file $config_file: $!\n";

    while (<CFILE>) {
	if (/^#/) {next;} # Skip comment lines (they begin with #)
	if (/^$/) {next;} # Skip blank lines

	 (my $key, my $value) = split(/[\t\n]/, $_);
	 $metadata->{$key} = $value;
    }
    if ($debug) {
	foreach my $key (keys %{$metadata}) {print "$key $metadata->{$key}\n";}
    }

    # Validate input

    #"offsetFromMidnight" = Mission Time (PVM data)
    #"offsetFromSTART" = Offset from starttime (MRLA3 data)
    #"YYMMDDHHMMSS" =  UTC Data/Time (cabin data)
    my @valid_timetypes =
    ("offsetFromMidnight","offsetFromSTART","YYMMDDHHMMSS");
    if ($metadata->{timetype} !~ /^[@valid_timetypes]/) {
	print "ERROR: timetype $metadata->{timetype} not a valid type:\n\t[@valid_timetypes]\n";
    }

    # Char on which to split data rows.
    if ($metadata->{dataSplitChar} =~ //) {
	print "ERROR: dataSplitChar not found.\n";
    }

    # Static (non-changing) AMES format requirements
    # Project name
    $metadata->{project} = "Physics Of Stratocumulus Tops (POST)\n";
    #Scaling factors for these variables (1.0)
    $metadata->{scaleFactor} = "1.0";
    #Missing values for these variables (-32767)
    $metadata->{missingVal} = "-32767";
    #Name of variables i.e. "wind speed (m/s)"
    $meta->{varNames} = $metadata->{varNames};
    $metadata->{varNames} = [];
    $meta->{units} = $metadata->{units};
    $metadata->{units} = [];
    $metadata->{longNames} = [];
    #Number of lines of comments to be used
    $metadata->{commentLen} = "";
    #Comment line 1-N
    $metadata->{comment} = [];
    
    # If filename exists in header, store it here so we can confirm it matches
    # name of file.
    $metadata->{filename} = "";
    # End metadata config file
    
    $meta->{PI} = "(PI\/)?DATA CONTACT = ";
    $meta->{instrument} = "INSTRUMENT = ";
    $meta->{filename} = "FILE NAME = ";
    $meta->{comment} = ["LOCATION = ","PLATFORM = ", "DATA VERSION = ", "REMARKS = "];
    $meta->{dates} = "DATA COVERAGE = START:";


    if (defined($metadata->{headerTimeFormat})) {
	if ($metadata->{headerTimeFormat} =~ /^YYYYMMDDHHMMSS/) {
            $charlen->{yr} = 4;
	} elsif ($metadata->{headerTimeFormat} =~ /^YYMMDDHHMMSS/) {
            $charlen->{yr} = 2;
        } else {
	    print "headerTimeFormat $metadata->{headerTimeFormat} not".
	    " recognized by code\n";
	    exit{1};
	}
	if ($metadata->{headerTimeFormat} =~ /\.SS/) {
            $charlen->{subs} = 3;
        } else {
            $charlen->{subs} = undef;	# sub-seconds
	}
    } else {
	print "Please define headerTimeFormat in config file\n"; 
	exit(1);
    }
    $charlen->{mo} = 2;
    $charlen->{dy} = 2;
    $charlen->{hr} = 2;
    $charlen->{mn} = 2;
    $charlen->{sc} = 2;

    close(CFILE);


# Hash to convert given var names to the RAF abbreviation as given in
# the "FINAL POST Twin Otter Output Parameter List (10/25/2008)"
if ($metadata->{filepattern} =~ /CABIN_II/) {
  $metadata->{varlist} = {
    'GLAT' => ['GLAT','Lat (deg)','GPS latitude (CIRPAS)'], 
    'GLON' => ['GLON','Long (deg)','GPS longitude (CIRPAS)'], 
    'GALT' => ['GALT','Nov Atel (m)','GPS altitude (CIRPAS)'], 
    'GWIE' => ['GWIE','East Vel (m/s)','GPS East aircraft velocity (CIRPAS)'], 
    'GWIN' => ['GWIN','North Vel (m/s)','GPS North aircraft velocity (CIRPAS)'], 
    'GWIU' => ['GWIU','Up Vel (m/s)','GPS up aircraft velicity (CIRPAS)'],
    'ROLL' => ['ROLL','Roll (deg)','roll of aircraft'], 
    'PITCH' => ['PITCH','Pitch (deg)','pitch of aircraft'], 
    'THDG' => ['THDG','Heading (deg)','aircraft heading'], 
    'AT' => ['AT','T amb (C)','static ambient temperature'], 
    'DT' => ['DT','Td amb (C)','ambient dew-point temeprature'], 
    'RHUM' => ['RHUM','RH amb (%)','ambient relative humidity'], 
    'PS' => ['PS','Ps (mb)','static atmospheric pressure'], 
    'WSC' => ['WSC','Wind Speed (m/s)','horizontal wind speed'], 
    'WDC' => ['WDC','Wind Dir (deg)','wind direction'], 
    'WVC' => ['WVC','Vert. Wind (m/s)','vertical wind velocity'], 
    'SST' => ['SST','SST (C)','sea-surface temperature'], 
    'PALT' => ['PALT','P Alt (m)','pressure altitude'], 
    'RADALT' => ['RADALT','Rad Alt (m)','radar altitude'], 
    'TAS' => ['TAS','TAS (m/s)','true air speed'], 
    'THETA' => ['THETA','Theta (K)','potential temperature'], 
    'THETAE' => ['THETAE','Thetae (K)','equivalent potential temperature'], 
    'MR' => ['MRLA1','MR-H2O (g/Kg)','mixing ratio (from dew point, CIRPAS)'], 
    'SPHUM' => ['SPHUM','SP Hum (g/Kg)','specific humidity (from dew point)'], 
    'Tvirt' => ['Tvirt','Tvirt ()','Virtual Temperature'], 
    'Thetav'=> ['Thetav','Thetav ()','Virtual Potential Temperature'], 
    'LWC-wire' => ['LWC1','LWC-wire (g/m^3)','liquid water content measured by CAPS wire','(CIRPAS)'], 
    'CONC_CASFWD' => ['CONC_CAS','CASFWD (#/cc)','CAS concentration, 1 - 50 um diameter'],
    'VOL_CASFWD' => ['VOL_CAS','CASFWD (Vol/cc)','CAS volume'], 
    'SYNCH' => ['SYNCH','synch (volts)','1/2-hz GPS synch signal'], 
  };


} elsif ($metadata->{filepattern} =~ /PVM/) {
  $metadata->{varlist} = {
    'PVM LWC' => ['LWC3','LWC (volts)','liquid water content (GSI)'], 
    'PVM PSA' => ['PSA','PSA (volts)','particle surface area'], 
    'Re' => ['Re','Re (micron)','effective radius'], 
    'SYNCH' => ['SYNCH','synch (volts)','1/2-hz GPS synch signal'],
  };

} elsif ($metadata->{filepattern} =~ /MRLA3/) {
  $metadata->{varlist} = {
    'MR' => ['MRLA3','mr (g/kg)','Fast Lyman-Alpha UV hygrometer'],};
} else {
    print "Please define varlist for $metadata->{type} in hashdef subroutine.\n";
    exit(1);
}

}
