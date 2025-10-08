#!/bin/perl -w
#*******************************************************************************
#  Author:   Janine Goldstein
#     Date:  2-25-99
#            This code checks qcf files for duplicate records that do not appear
#            side by side in the files.
# Modified:  Janine Goldstein
#     Date:  3-5-99
#            Parse qcf records by column number, not spaces.
# Modified:  Janine Goldstein
#     Date:  3-8-99
#            Add duplicate records to hash so that in turn they may be checked 
#            for their own dupes.
# Modified:  Janine Goldstein
#     Date:  3-9-99
#            Add flag to suppress reporting of colocated station.
#*******************************************************************************
#
# program initialization section

# check command line arguments
if (@ARGV < 1) {
   printf STDERR "Usage: $0 [-c] QCF_Files\n\n";
   printf STDERR "Use the -c flag to suppress reporting of colocated ";
   printf STDERR "records\n\n";
   exit;
}

if ($ARGV[0] =~ /-c/) {
   shift(@ARGV); $SUPPRESS = 1;
}

# cycle through files to be tested
foreach $arg (@ARGV)
{
  open INPUT, "<$arg";
  printf STDERR "Processing file $arg\n";
  $savetime = '';
  while (<INPUT>) {    # do it till it's done

    # read a data record
    &read_rec;

    # check for new time
    if ($savetime ne $curtime) {
      # new time so clear hashes and save current record
      %id_table = ();
      %net_location_table = ();
      %location_table = ();

      $id_table{$network}{$stationId} = $_;
      $net_location_table{$network}{$lat}{$lon} = $_;
      $location_table{$lat}{$lon} = $_;
      $savetime = $curtime;
    } else {
      # put record in hash; if record already exists, print duplicate record
      # warning

      if (exists $id_table{$network}{$stationId}) {
        printf STDERR "WARNING: duplicate stations within network ->\n";
        printf STDERR " $id_table{$network}{$stationId} $_\n";
        $net_location_table{$network}{$lat}{$lon} = $_;
        $location_table{$lat}{$lon} = $_;
      } elsif (exists $net_location_table{$network}{$lat}{$lon}) {
        if (!$SUPPRESS) {
          printf STDERR "WARNING: colocated stations within network ->\n";
          printf STDERR " $net_location_table{$network}{$lat}{$lon} $_\n";
        }
        $id_table{$network}{$stationId} = $_;
        $location_table{$lat}{$lon} = $_;
      } elsif (exists $location_table{$lat}{$lon}) {
        if (!$SUPPRESS) {
          printf STDERR "WARNING: colocated stations between networks ->\n";
          printf STDERR " $location_table{$lat}{$lon} $_\n";
        }
        $id_table{$network}{$stationId} = $_;
        $net_location_table{$network}{$lat}{$lon} = $_;
      } else {
        $id_table{$network}{$stationId} = $_;
        $net_location_table{$network}{$lat}{$lon} = $_;
        $location_table{$lat}{$lon} = $_;
      }

    }

  }
  close(INPUT);
}  # end foreach

# end of main program, that's all folks!
exit(0);

#*******************************************************************************
#********************************* subroutines *********************************
#*******************************************************************************
sub read_rec {

    # split $_ into fields

    ($nomdate, $nomtime, undef, undef, $network, $stationId, $latitude,
     $longitude) = unpack("a11a6a11a6a11a16a11a12a",$_);

    $curtime = $nomdate.$nomtime;
    $lat = int($latitude*100+.5);
    $lon = int($longitude*100+.5);

#    printf STDERR "$nomdate, $nomtime, $network, $stationId, $lat, $lon\n";

}
#*******************************************************************************
