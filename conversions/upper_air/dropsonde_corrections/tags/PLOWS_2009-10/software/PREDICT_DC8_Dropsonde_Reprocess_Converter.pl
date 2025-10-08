#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The Dropsonde_Reprocess_Converter.pl script reads in corrected EOL
# dropsonde data and the original ESC converted version of the data and
# replaces the original dewpt and RH values with the corrected values. 
# It then writes out a corrected ESC file.</p>
#
# @author Linda Echo-Hawk
# @version 8 December 2016 for reprocessing of EOL dropsonde data
#          - The readDataFiles function reads in each of the corrected 
#            EOL files from the eol_data data directory and finds the 
#            appropriate ESC file in the esc_data directory. It then 
#            calls ParseSoundingFiles for each pair of files.
#          - BEWARE: Because there are differences in the seconds portion
#            of the file names between the ESC and EOL files, comparisons
#            for file matching do not check seconds. Some hard-coded
#            checks had to be added for several files that vary only by
#            seconds to assure that the correct files are matched up.
#          - Added a warning in the code if the ESC file was not 
#            1 line longer than the ESC file and therefore not a 
#            good match.
#          - Each header line in the original ESC file is printed to
#            the new file "as is" with the exception of the Post
#            Processing Comments line, which has "TDDryBiasCorrApplied" 
#            appended to the end of the line.
#          - The RH and dewpt values from the corrected EOL file are
#            used to replace the existing values in the original ESC 
#            file. Then this line is formatted and printed to the 
#            new file.
#          - The new ESC file is written to the /output directory.
#
# use  Dropsonde_Reprocess_Converter.pl >&! results.txt
#
##Module------------------------------------------------------------------------
package Dropsonde_Reprocess_Converter;
use strict;

if (-e "/net/work") {
    use lib "/net/work/lib/perl/Utilities";
    use lib "/net/work/lib/perl/UpperAir";
    use lib "/net/work/lib/perl/Station";
} else {
    use lib "/work/lib/perl/Utilities";
    use lib "/work/lib/perl/UpperAir";
    use lib "/work/lib/perl/Station";
} 

use ElevatedStationMap;
use Station;
use ClassConstants qw(:DEFAULT);
use ClassHeader;
use ClassRecord;
use Data::Dumper;

my $debug = 0;
my $debugHeader = 0;
my $WARN;
 printf "\nDropsonde_Reprocess_Converter.pl began on ";print scalar localtime;printf "\n";  
&main();
 printf "\nDropsonde_Reprocess_Converter.pl ended on ";print scalar localtime;printf "\n";

##------------------------------------------------------------------------------
# @signature void main()
# <p>Reprocess the EOL Dropsonde data by replacing the
# ESC format RH and dewpt data with corrected values.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = Dropsonde_Reprocess_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature Dropsonde_Reprocess_Converter new()
# <p>Create a new instance of a Dropsonde_Reprocess_Converter.</p>
#
# @output $self A new Dropsonde_Reprocess_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    $self->{"OUTPUT_DIR"} = "../output";
    # the eol dir is the eol data to be reprocessed
    $self->{"EOL_DIR"} = "../eol_data";
    # the esc dir is the originally processed *.cls data
	$self->{"ESC_DIR"} = "../esc_data";
                                
    $self->{"WARN_LOG"} = $self->{"OUTPUT_DIR"}."/warning.log";

    return $self;
}


##-------------------------------------------------------------------------
# @signature String cleanForFileName(String text)
# <p>Remove/translate characters in a String so it can be used in a file name.</p>
#
# @input $text The String to be cleaned.
# @output $text The cleaned up String.
##-------------------------------------------------------------------------
sub cleanForFileName {
    my ($self,$text) = @_;

    # Convert spaces to underscores
    $text =~ s/\s+/_/g;

    # Remove all hyphens
    $text =~ s/\-//g;

    return $text;
}

##------------------------------------------------------------------------------
# @signature void convert()
# <p>Convert the raw data to the ESC format.</p>
##------------------------------------------------------------------------------
sub convert {
    my ($self) = @_;

    mkdir($self->{"OUTPUT_DIR"}) unless (-e $self->{"OUTPUT_DIR"});

    $self->readDataFiles($WARN);

    # close($WARN);
}


##------------------------------------------------------------------------------
# @signature void parseSoundingFiles(FileHandle WARN, String eol_file, 
#                  String esc_file)
# <p>Parse the EOL and ESC files and make corrections to dewpoint and RH.</p>
#
# @input $WARN The file handle where warnings are to be stored.
# @input $eol_file The name of the corrected EOL file to be parsed.
# @input $esc_file The name of the original ESC file to be parsed.
##------------------------------------------------------------------------------
sub parseSoundingFiles
{
    my ($self,$WARN,$eol_file,$esc_file) = @_;

    printf("\tProcessing files: ");
	printf("EOL: %s  ESC: %s\n",$eol_file, $esc_file);
    
    
	open(my $ESCFILE,sprintf("%s/%s",$self->{"ESC_DIR"},$esc_file)) or die("Can't read $esc_file\n");
	my @esc_lines = <$ESCFILE>;
	# close ($ESCFILE);

	my $number_lines_in_esc_file = $#esc_lines+1; # 0 to n-1 correction
	print "\t$number_lines_in_esc_file Lines in ESC File\n";


    open(my $EOLFILE, sprintf("%s/%s",$self->{"EOL_DIR"},$eol_file)) or die("Can't read $eol_file\n");
	my @eol_lines = <$EOLFILE>;
	my $number_lines_in_eol_file = @eol_lines;
	print "\t$number_lines_in_eol_file Lines in EOL File\n";
	
	if ($number_lines_in_esc_file != $number_lines_in_eol_file + 1)
	{
		print "WARNING - This may be the wrong file!\n";
	}
	
	my @rev_eol_lines = reverse(@eol_lines);

    # ----------------------------------------------------
    # Create the output file name and open the output file
    # ----------------------------------------------------
    my $outfile;
    $outfile = sprintf("%s.corr", $esc_file);
    printf("\tOutput file name is %s\n", $outfile);


    open(my $OUT,">".$self->{"OUTPUT_DIR"}."/".$outfile)
        or die("Can't open output file for $esc_file\n");
    # ----------------------------------------------------
    # Output file open 
    # ----------------------------------------------------
	
    my $index = 0;
	# while there are elements in the @rev_eol_lines array
	my $n = 0;
	my $eol_line;
	# do this for each line in the ESC file
	while ($rev_eol_lines[$n])
	{
        # print out the header lines from the ESC file through index = 14
		if ($index < 8)
		{   
			print ($OUT $esc_lines[$index]);
		}
		elsif ($index == 8)
		{
			my $line = $esc_lines[$index];
			chomp $line;
			print ($OUT "$line; TDDryBiasCorrApplied\n");

		}
		elsif (($index > 8) && ($index < 14))
		{
			print ($OUT $esc_lines[$index]);

		}
        # combine to make index less than or equal to 14
	    elsif ($index == 14)
		{
				# need to print out the dashed lines
				# separating header from data
				print ($OUT $esc_lines[$index]);
		}
		else
		{
    	
			# Read the line from the EOL file to 
			# get the replacement dewpt and RH values
			$eol_line = $rev_eol_lines[$n];
			my (@eol_text) = split(' ',$eol_line);
			if ($eol_text[0] =~ /-------/)
			{
				print "\tFound last eol line\n";
				last;
			}
			my $new_dewpt = sprintf"%5.1f", $eol_text[6];
			if ($new_dewpt == -999.0)
			{
				$new_dewpt = 999.0;
			}
			my $new_RH = sprintf "%5.1f", $eol_text[7];
			my $new_RH_flag;
			if ($new_RH == -999.0)
			{
				$new_RH = 999.0;
				$new_RH_flag = 9.0;
			}

    	    # Insert the new dewpt and RH values
			# into the correct line in the ESC file
			my ($new_esc_line) = $esc_lines[$index];
			# print "NEW ESC LINE $new_esc_line\n";
			if ($new_esc_line)
			{
				chomp($new_esc_line);
				my (@esc_values) = split(' ',$new_esc_line);
				$esc_values[3] = $new_dewpt;
				$esc_values[4] = $new_RH;
				if ($esc_values[4] == 999.0)
				{
					$esc_values[17] = $new_RH_flag;
				}

	    	    # prepare the new ESC line for printing
    	 		$new_esc_line = sprintf "%6.1f %6.1f %5.1f %5.1f %5.1f %6.1f %6.1f %5.1f %5.1f %5.1f %8.3f %7.3f %5.1f %5.1f %7.1f %4.1f %4.1f %4.1f %4.1f %4.1f %4.1f\n",
					$esc_values[0], $esc_values[1], $esc_values[2], $esc_values[3], 
					$esc_values[4], $esc_values[5], $esc_values[6],	$esc_values[7], 
					$esc_values[8], $esc_values[9], $esc_values[10], $esc_values[11], 
					$esc_values[12], $esc_values[13], $esc_values[14], $esc_values[15], 
					$esc_values[16], $esc_values[17], $esc_values[18], $esc_values[19], 
					$esc_values[20];
    
	   	    	print ($OUT "$new_esc_line");
				$n++; # move to the next line in the @rev_eol_lines array
			}
		}

		$index++;
		# print "INDEX $index\n";
	}

close $EOLFILE;
}


##------------------------------------------------------------------------------
# @signature void readDataFiles()
# <p>Read in each of the corrected EOL files from the eol_data data directory
# and finds the appropriate ESC file in the esc_data directory. Then call
# ParseSoundingFiles for each pair of files.</p>
##------------------------------------------------------------------------------
sub readDataFiles {
    my ($self,$WARN) = @_;

    opendir(my $EOL,$self->{"EOL_DIR"}) or die("Can't read eol directory ".$self->{"EOL_DIR"});
	# example EOL file name: D20140720_131753_P.QC.eol
    my @eol_files = grep(/^D\d{8}_\d{6}.+\.eol$/i,sort(readdir($EOL)));
	my $number_eol_files = $#eol_files+1; # 0 to n-1 correction
	print "\tFound $number_eol_files EOL files\n";
	
	# print "\n\tRead ESC_DIR\n";
	opendir(my $ESC,$self->{"ESC_DIR"}) or die("Can't read ESC directory ".$self->{"ESC_DIR"});
    my @esc_files = grep(/\.cls.qc$/,readdir($ESC));
	
	my $matching_esc_file;
    foreach my $eol_file (@eol_files) {
		# example EOL file: D20140720_131753_P.QC.eol
		# example ESC file: NCAR_GV_N677F_20140720131752.cls.qc
        my $eol_date;
		my $eol_time;
        # if ($eol_file =~ /(\d{8})_(\d{4})(\d{2})/)
        if ($eol_file =~ /(\d{8})_(\d{4})/)
		{
			$eol_date = $1;
			$eol_time = $2;
		}

		foreach my $esc_file (@esc_files)
		{
			if ($esc_file =~ /(\d{8})(\d{4})(\d{2})/)
			{
				my ($esc_date, $esc_time) = ($1,$2);
				# print "\tESC: $esc_date $esc_time   EOL: $eol_date $eol_time\n";
				if (($esc_date == $eol_date) && ($esc_time == $eol_time))
				{
					if (($eol_file =~ /^D20100901_201629/) && ($esc_file !~ /NASA_DC8_N817NA_20100901201629/))
					{
						next;
					}
					if (($eol_file =~ /^D20100901_222406/) && ($esc_file !~ /NASA_DC8_N817NA_20100901222406/))
					{
						next;
					}
					if (($eol_file =~ /^D20100902_183717/) && ($esc_file !~ /NASA_DC8_N817NA_20100902183717/))
					{
						next;
					}
					if (($eol_file =~ /^D20100902_195539/) && ($esc_file !~ /NASA_DC8_N817NA_20100902195539/))
					{
						next;
					}
					if (($eol_file =~ /^D20100901_214617/) && ($esc_file !~ /NASA_DC8_N817NA_20100901214617/))
					{
						next;
					}
					if (($eol_file =~ /^D20100830_221024/) && ($esc_file !~ /NASA_DC8_N817NA_20100830221024/))
					{
						next;
					}
				    
					$matching_esc_file = $esc_file;

			    	print "\nESC_FILE: $matching_esc_file  ";
					print "MATCHES EOL_FILE: $eol_file\n";

			        $self->parseSoundingFiles($WARN,$eol_file,$matching_esc_file);
				    last;
				}
			    # print "\tESC_FILE: $matching_esc_file\n";
				# print "\tEOL_FILE: $eol_file\n";
			}
		}

    }

	rewinddir($ESC);
	closedir($ESC);
    
	rewinddir($EOL);
	closedir($EOL);

}

##------------------------------------------------------------------------------
# @signature String trim(String line)
# <p>Remove all leading and trailing whitespace from the specified String.</p>
#
# @input $line The String to be trimmed.
# @output $line The trimmed String.
##------------------------------------------------------------------------------
sub trim {
    my ($line) = @_;
    return $line if (!defined($line));
    $line =~ s/^\s+//;
    $line =~ s/\s+$//;
    return $line;
}
