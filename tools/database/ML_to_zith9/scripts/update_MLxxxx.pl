#!/usr/bin/perl -w

#***********************************************************************
# update_MLxxxx.pl
#
# Read list of Master List datasets with dataset_ids that match ML.xxxx
#  and write out a file with SQL statements to change Master List MLxxxx
#  dataset_ids to archive_ident dataset ids.
#***********************************************************************

my $infile = "logs/new_archive_idents.log";
my $outfile = "update_MLxxxx_to_archive_ident.sql";
my $logfile = "logs/MLxxxx_to_archive_ident.log";

# Open the files for reading.
open(INFILE, $infile) || die "could not open $infile";
open(OUTFILE, ">$outfile") || die "Can't open $outfile: $!\n";
open(LOGFILE, ">$logfile") || die "Can't open $logfile: $!\n";

#***********************************************************************
# Read line
#***********************************************************************

print "Now reading $infile\n";
print OUTFILE "begin;\n";
while ($this_line = <INFILE>) {
    #print LOGFILE "\n$this_line";
    chomp($this_line);
    ($MLxxxx, $archive_ident, $zith_did) = split(/;/, $this_line);   
   	print OUTFILE "update dataset set dataset_id=\"$archive_ident\" where dataset_id=\"$MLxxxx\";\n";
}      

close(INFILE);
close(OUTFILE);
close(LOGFILE);
