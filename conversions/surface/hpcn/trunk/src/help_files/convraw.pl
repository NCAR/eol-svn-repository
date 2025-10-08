#!/bin/perl
#    convraw.pl
#
#    Pulls data line by line from the High Plains hourly raw data,
#      and puts it into a standard format for processing.
#
#    An input file must be specified on the command line -
#
#      input file: a single line containing the field numbers of the
#        raw data, separated by commas, in the order which they
#        will be put into our 8 field format.  (start at 0, which will
#        be the date and time put into a single field by the perl script)
#
#      example:   0,1,2,3,5,7,4,9
#                 (field #7 in the raw data is put in field #5 position in the converted data)
#
#    Use the -L switch between the program and filename for processing of Langley solar values
#      to rad flux Kcal m**2 values.
#      example: convraw.pl -L group17.inp
#
# ds 3/4/96
# rev. 5/15/96

if ($ARGV[0] eq "-L") {						# check for processing of langley readings
	$langley = 1;
	shift (@ARGV);
}
if (!-e $ARGV[0]) {                               		# check for input file with format conversion data on command line
	die "\tNeed to specify a valid input file!\n";  
}
print "\nRunning data pre-processing program for HPCN files with $ARGV[0] input file...\n";
if ($langley) {
	print "\t (using x10 conversion of Langley values to rad flux values)\n";
}
@changes = <>;
@list = split(/,/,$changes[0]);                                 # put field number list into form PERL can deal with later

$dir = "./";
opendir(FILEDIR, $dir) || die "Can't open $dir: $!\n";          # read filenames in this directory
@files = readdir(FILEDIR);
closedir(FILEDIR);

foreach $file (@files) {					# find filename matches to *.ucr
	if ($file =~ /\.ucr$/) {					# open the file for reading
		@header = ();					# make sure header is blank
		$infilename = $dir.$file;			# concatenate directory with file name
		$outfilename = $infilename;
		$outfilename =~ s/\.ucr$/\.new/;		# write to file with .new name
		if (!open(INFILE, $infilename)) {
			print "Could not open $infilename\n";
			next;
		}
		print "\nNow reading in the data from $infilename ";
		$input_line = <INFILE>) until $input_line =~ /\w/;	 # skip over any blank lines
		$header[0] = $input_line;				 # save first line of text 
		$input_line = <INFILE>;					
		$orig_header2 = $input_line;
		$station_id = (split(" ", $input_line))[0];		 # id in first field of next line
		print "\tStation ID = $station_id\n";  

		# form our second line of the header from the station id and set field names
		$header2 = sprintf("     %-12s AIR TEMP  REL HUM   SOIL TMP  WIND SP   WIND DIR  RAD FLUX   PRECIP\n", $station_id);
		$header3 =  "    Date/Time        F         %         F       MI/HR     DEGREES  Kcal m-2   INCHES\n";
		push(@header, $header2);
		push(@header, $header3);

		open (OUTFILE, ">$outfilename") || die "Can't open $outfilename!\n";
		print "Writing out data to file:    $outfilename\n";
		print OUTFILE @header;

		# read past header lines to first data line, beginning with 1 or 2 spaces, and 1 or 2 numerals
		$input_line = <INFILE> until $input_line =~ /^ {1,2}\d{1,2} /;
		$j = 0;
		$print_line = 0;

		while ($input_line) {					# fix up and write out the lines of data
			$date_time = substr($input_line, 0, 16);	# put date/time into single variable
			@fields = split(" ", $input_line);		# split line into an array of fields 

			foreach $i (1..4) {				# remove first four fields from our array
				shift(@fields);				# to make space for our own date/time field		
			}
			unshift(@fields, $date_time); 			# put our date/time variable in as first field

			@fields[0,1,2,3,4,5,6,7] = @fields[@list];	# place our fields
			if (($extrafields = $#fields - 7) > 0) {
				foreach $i (1..$extrafields) {	
					pop(@fields);			# remove unused fields on right
				}
			}
			undef @flag;					# start out anew
			foreach $i(0..$#fields) {			# scan each field
				$flag[$i] = chop($fields[$i]) if ($fields[$i] =~ /\D$/);        # if field ends with a character, chop off and save
					if ($flag[$i] =~ /[^eE]/) {				# if flag is any character except e or E
						$print_line = 1;
						if ($j == 0) {					# put the old header at top of our flags file
							open(MESSAGEFILE, ">>flags.msg") || die "Can't open flags.msg!\n";
							print MESSAGEFILE ("\n___________________________________________________________________________________");
							print MESSAGEFILE ("\nFlags other than e or E in file $infilename:\n");
							print "\n  Flags other than e or E are on these lines:\n";
							print MESSAGEFILE $orig_header2;
							$j++;
						}
					}
			}
			if ($print_line == 1)  {
				print MESSAGEFILE ("Line #$.\n $input_line");
				print ("\tLine: $.\n");				# and show it on the screen
			}
			if ($langley) {
				$fields[6] = $fields[6] * 10;			# convert for Langley solar rad amounts
			}
			write OUTFILE;
			$print_line = 0;
			$input_line = <INFILE>;				# read in next line of data
		}	
		close(INFILE);
		close(OUTFILE);
	}
}	
close(MESSAGEFILE);

format OUTFILE =
@<<<<<<<<<<<<<<< @>>>>>>>@ @>>>>>>>@ @>>>>>>>@ @>>>>>>>@ @>>>>>>>@ @###.###@ @>>>>>>>@
$fields[0],$fields[1],$flag[1],$fields[2],$flag[2],$fields[3],$flag[3],$fields[4],$flag[4],$fields[5],$flag[5],$fields[6],$flag[6],$fields[7],$flag[7]
.
