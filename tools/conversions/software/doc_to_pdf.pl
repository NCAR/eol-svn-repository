#! /usr/bin/perl -w
#****************************************************
# doc_to_pdf.pl - A script that will convert the DOC files
# found in CODIAC to PDF files and stay in the same directory
#
# @author Alley Robinson
# @date 19 July 2018
#
# INPUT:
# 	docFilesList.txt - a file that contains all of the .doc files
#
#
# OUTPUT:
# 	conversionLog.log - a log file that will check the status of the program
# 	as it it running
# 	*.pdf - the PDF files that are in the same location as the original DOC files
#
#****************************************************
use strict;
use warnings;

printf "Welcome! doc_to_pdf.pl  began on ";print scalar localtime;printf "\n";

# Declaring all Global Variables:
my $debug = 0;
convert();

printf "See you soon! doc_to_pdf.pl ended on ";print scalar localtime;printf "\n";

#************************************************************
# @signature sub convert()
# This function will read the file
# that has been provided by CODIAC
# Database engineers that contains the
# .doc/.docx files that need to be converted
#
# Remember! There is not supposed to be .doc/.docx in CODIAC!
#************************************************************
sub convert
{
	# Declare variables and see if they exist:
	my $inputFile = $ARGV[0];
	my $index     = 0;

	unless (-e $inputFile)
	{
		print "WARNING: $inputFile does not exist! Please move the file to this directory! \n";
	}

	print "SUCCESSFULLY RETRIEVED FIlE: $inputFile \n"; 


	#Now to read the file and do the conversion:
	open (my $input_FH, '<', $inputFile) or die "Can't open $inputFile!\n";
  

	print "PROCESSING: $inputFile \n";

	# Begin processing:
	my @lines = <$input_FH>;
	foreach my $line (@lines)
	{

		#******************************************************************
		# Sample of the text file:
		#
		# Column 1: Dataset Number	10.4
		# Column 2: File Name		usgs_prelim_streamflow.doc
		# Column 3: Purpose		doc
		# Column 4: Directory		/net/archive/data/web/esop_95/docs
		#******************************************************************
		#TODO: Separate the file by , and then trim the spaces off like in sounding processing
		# need to start on index = 3
		if ($index >= 3)
		{
			my @data = split(",", $line);		
	
			$data[0] = trim($data[0]);
			$data[1] = trim($data[1]);
			$data[2] = trim($data[2]);
			$data[3] = trim($data[3]);
		
			if ($debug) { print "This is what data looks like: $data[0] , $data[1] , $data[2] , $data[3]  \n\n"; }		
			
			# Copy the .doc file before converting
			my $filepathDoc = $data[3] . "/" . $data[1];

			if ($debug) { print "Doc File: $filepathDoc \n\n"; }

			system("cp $filepathDoc $data[3]/$data[1].orig");

			# Now convert the file from .doc to .pdf (not the .orig) 
			system("libreoffice --headless --convert-to pdf $filepathDoc --outdir $data[3]");
		
			# Cleanup the copy of the file
			system("rm -r $data[3]/$data[1].orig");	
			if ($debug) { print "Successfully converted $filepathDoc /n/n";}

		} #end if index >= 3
	
		$index++;

	} #end foreach my $line 

	print "END PROCESSING \n";
	close $input_FH;

} #end sub convert()


#------------------------------------------------------------------------------
# @signature String trim(String line)
# <p>Remove all leading and trailing whitespace from the specified String.</p>
#
# @input $line The String to be trimmed.
# @output $line The trimmed String.
#------------------------------------------------------------------------------
sub trim 
{
    my ($line) = @_;
    return $line if (!defined($line));
    $line =~ s/^\s+//;
    $line =~ s/\s+$//;
    return $line;
}
