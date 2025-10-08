#!/usr/bin/perl -w

##Module-------------------------------------------------------------------------- 
# @author Linda Echo-Hawk
# @version Created for DYNAMO 2011 Indonesian Ranai soundings which had two
#          input files, one with data and one with header info.  Normally these
#          would be read into a hash, but not every COR file had a REF file and
#          this made it difficult, so I decided to just combine the files and
#          read them in once.
#
# Usage:    ReadTwoWriteOne.pl <input1 file> <input2 file> <combined output>
#
# Example:  ReadTwoWriteOne.pl *.cor *.ref *.combined
#
##Module--------------------------------------------------------------------------
use strict;
use warnings; 

if (-e $ARGV[1])
{
    open (IN_COR, "$ARGV[0]") || die "Can't open for reading\n";
    open (IN_REF, "$ARGV[1]") || die "Can't open for reading\n";
    open (OUT, ">$ARGV[2]") || die "Can't open file for writing\n";

    my $datafile = $ARGV[0];
    my $headerfile = $ARGV[1];
    print "Combining Files   $datafile  $headerfile\n";        

    while (<IN_REF>)
    {  
        my $line_ref = "$_";
	    # chomp($line_ref);
        if (($line_ref =~ /Identification Sonde/i) || 
	        ($line_ref =~ /Version logiciel/i))
	    {
	        # print OUT "$line_ref\n";
		    print OUT "$line_ref";
	    }
    }
    close IN_REF;

    while (<IN_COR>)
    {
        my $line_cor = "$_";
	    # chomp($line_cor);
	    # print OUT "$line_cor\n";
	    print OUT "$line_cor";
    }
    close IN_COR;
}
else
{
	print "$ARGV[1] is missing\n";
    open (IN_COR, "$ARGV[0]") || die "Can't open for reading\n";
    open (OUT, ">$ARGV[2]") || die "Can't open file for writing\n";
    # my $datafile = $ARGV[0];
    print "Adding missing header lines to  $ARGV[0]\n";        

    print OUT "Version logiciel station=No Information\n";
    print OUT "Identification Sonde=No Information\n";

    while (<IN_COR>)
    {
        my $line_cor = "$_";
	    # chomp($line_cor);
	    # print OUT "$line_cor\n";
	    print OUT "$line_cor";
    }
    close IN_COR;
}

