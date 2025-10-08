#!/usr/bin/perl
#
##Module------------------------------------------------------------------------
# <title>cosmic2raob.pl</title>
#
# <p>The purpose of this code is to extract data from COSMIC NetCDF files
# and to produce columnar ascii of the data.
#
# @use    cosmic2raob.pl <file.nc>
#
##Module------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Note that a backslash before a variable indicates a reference to that
# variable.
#-------------------------------------------------------------------------------
# Include NetCDF module in this code. Note that the NetCDF module is NOT
# installed on every EOL machine. 'use' is a compile time directive. It is
# shorthand for the following:
#BEGIN {
#       require YourModule;
#       YourModule->import(LIST);
#      } 
# By wrapping the require and import in an eval, we can let the user know
# where to run the code and exit gracefully.
#
#use NetCDF;	# does NOT die gracefully when run on a machine where
		# NetCDF.pm is not installed. So...
BEGIN {
  eval {
    require NetCDF;
    NetCDF->import();
  };

  if ($@) {
    print "\nERROR: This code requires the NetCDF.pm module which is\n".
   	  "apparently not installed on the machine on which you are \n".
	  "attempting to run the code. Please run this code on merlot\n".
	  "or tsunami.\n\n"; 
    exit(1);
  }
}

# Include Joel's unit conversion module
use lib "/net/work/lib/perl/Utilities";
use DpgConversions;
use DpgCalculations;
use NCutils;

# PERL standard module which allows the code to refer to each component of the 
# time by name, i.e. ->year
use Time::gmtime;

# Routines ParseDate and UnixDate are contained within this CPAN module
use Date::Manip;

use strict 'vars';

#*********************************************************************

# Call the code
&main(@ARGV);

#*********************************************************************

# There are a ton of print statements in this code for debugging and 
# informational purposes.  Turn them on or off and see what you get (-:
sub DEBUG       {return 0; }
sub DEBUGoutput {return 1;}		# debug info for parse_data_rec subroutine
sub DEBUGgetV {return 0;}               # debug info for netCDF subroutine

# Output file extension
sub getOutfileExtension { return ".txt"; } 

# I like to define variables FALSE and TRUE and use them rather than one
# and zero in by comparison statements.  I think it is clearer.
sub FALSE       {return 0; }
sub TRUE        {return 1; }

#*********************************************************************
# Below this line is the meat of the code
#*********************************************************************
sub main {

    # usage
    if ($#ARGV < 0)
        {
        print "Usage: cosmic2raob.pl *.nc\n";
        exit 1;
        }


    # loop over input files
    while ($ARGV = shift)
    {
        print "Processing file $ARGV\n";

        # Separate directory from filename, if any
	# Save filename to $file.
        use File::Basename;
        (my $base, my $dir, my $inext) = fileparse($ARGV,'\..*');
	my $file = "$base$inext";

	print "Processing file $file\n";

	my $ncid;
	if (($ncid = NetCDF::open($ARGV,0)) == -1) {
	    die "Can't open $ARGV:$!\n";
	}

	# Parse the GNSS ID from the filename
	my @parts = split '\.',$file;
	(my $gnssid,my $junk) = split("_",$parts[5]);

        # Open the output file which the data will be written to
        my $ext = &getOutfileExtension;
        my %global_atts = get_global_atts($ncid);

        my $outfile = sprintf "%s%04d%02d%02d%02d%s", $gnssid, $global_atts{"year"},
        $global_atts{"month"},$global_atts{"day"},$global_atts{"hour"}, $ext;

        open (OUTFILE,">$outfile") 
            or die "Can't open output file $outfile:$!\n";

	# Write filename as header to output file.
	print OUTFILE "\"<HTML>\n<H2>$file</H2>\n<PRE>\n";

        # if input file is gzipped, unzip it
        my $zipflag;
        if ($ARGV =~ /.gz/) 
            {
            system "gunzip $ARGV";
            $ARGV =~ s/.gz//;
            $zipflag = &TRUE;
            }
        else
            {
            $zipflag = &FALSE;
            }



        # Read all the data from the entire input netCDF file, outputting
        # ascii as we go. (output is included within this input routine)
        (my $recDimName, my $var, my $recdimsize) = readNetCDFheader($ARGV);


        getData($ncid,$recDimName, $var, $recdimsize);


        my $obstime = sprintf "%02d%02d%02d/%02d%02d", substr($global_atts{"year"},2,2),
            $global_atts{"month"},$global_atts{"day"},$global_atts{"hour"},
	    $global_atts{"minute"};

        print OUTFILE "</PRE><H3>Station information and sounding indices</H3><PRE>\n";
        printf OUTFILE "%44s %s\n","Station identifier:",$gnssid;
        printf OUTFILE "%44s %s\n","Station number:","99999";
        printf OUTFILE "%44s %s\n","Observation time:",$obstime;
        printf OUTFILE "%44s %5.2f\n","Station latitude:",$global_atts{"lat"};
        printf OUTFILE "%44s %5.2f\n","Station longitude:",$global_atts{"lon"};;
        printf OUTFILE "%44s %s\n","Station elevation:","0.0";
	print OUTFILE "</PRE>";

	if (NetCDF::close($ncid) == -1) {
            die "Can't close $ARGV:$!\n";
        }

        # if the file was gzipped when we found it, re-zip it.
        if ($zipflag) {system "gzip $ARGV";}

        close(OUTFILE);

    } # End loop over input files

}
#*********************************************************************
sub parse_data_rec{
    my $recdimLen = shift;
    my $ARGV = shift;
    my $var = shift;

    my %output = ();
    foreach (my $recnum=0; $recnum < $recdimLen; $recnum++) {

    if (&DEBUGoutput) {print "\n\n";}
    if (&DEBUGoutput) {print "Processing record $recnum\n\n";}
 
    # $recnum is the index of all the data for a single time.  

    # Process this record and the next one.
    for (my $i = $recnum; $i < $recdimLen && $i <= $recnum+1; $i++) {

      $output{pressure}[$i] = getVar("Pres",$var,$i); #hPa = mb
      $output{temp}[$i] = getVar("Temp",$var,$i); #C
      $output{MSLalt}[$i] = convertLength(getVar("MSL_alt",$var,$i),'km','m');# km -> m
      $output{vapor_pressure}[$i] = getVar("Vp",$var,$i); #mb
      if ($output{temp}[$i] == -999) {
	  $output{sat_vapor_pressure}[$i] = -999.0;
      } else {
          $output{sat_vapor_pressure}[$i] = calculateVaporPressure($output{temp}[$i]); #mb
      }
      if (&DEBUGoutput) {print "sat_vapor_pressure $i $output{sat_vapor_pressure}[$i]\n";}

      if ($output{sat_vapor_pressure}[$i] == -999) {
	  $output{rh}[$i] = -999.0;
      } else {
          $output{rh}[$i] = (100.0 * $output{vapor_pressure}[$i] / $output{sat_vapor_pressure}[$i]); # %
      }
      if (&DEBUGoutput) {print "rh $i $output{rh}[$i]\n";}

      if ($output{rh}[$i] > 0) {
        $output{dewpt}[$i] = calculateDewPoint($output{temp}[$i],$output{rh}[$i]); # deg c
      } else {
	$output{dewpt}[$i] = -999.0;
	print "Calculated RH <= 0 at record ".sprintf($i).". Dewpt set to missing.\n";
      }
      if (&DEBUGoutput) {print "dewpt $i $output{dewpt}[$i]\n";}

      if ($output{pressure}[$i] > 0) {
        $output{Tvirt}[$i] = 
          $output{temp}[$i] / (1.0 - ($output{rh}[$i] * $output{sat_vapor_pressure}[$i])/(100.0 *
	  $output{pressure}[$i])); # C
      } else {
        $output{Tvirt}[$i] = -999.0;
	print "Pressure <= 0 at record ".sprintf($i).". Tvirt set to missing.\n";
      }
      if (&DEBUGoutput) {print "Tvirt $i $output{Tvirt}[$i]\n";}
    } # End for $i loop

    # now calc Geopotential Ht to replace geometric Ht.
    if (&DEBUGoutput) {
	print "Calculating Geopotential Ht for record ".$recnum."\n";
    }
    my $RD = 287.05307;
    my $GAMMA45 = 9.80665;
    
    my $GeoHt;
    # For adding to lower edge
    # my $step = 1;
    #if ($recnum+1 < $recdimLen && 

    # For adding to upper edge
    my $step = -1;
    if ($recnum >= 1 && 
	$output{Tvirt}[$recnum] != -999 && $output{Tvirt}[$recnum+$step] != -999 &&
        $output{MSLalt}[$recnum] != -999 && $output{pressure}[$recnum] != -999 &&
        $output{pressure}[$recnum+$step] != -999 ) {
        $GeoHt = $output{MSLalt}[$recnum];
        my $TVavg = ($output{Tvirt}[$recnum]+$output{Tvirt}[$recnum+$step])/2.0; # C
        my $layer_depth = ($RD/$GAMMA45) * $TVavg *
            log($output{pressure}[$recnum]/$output{pressure}[$recnum+$step]);
        $GeoHt = $GeoHt + $layer_depth;
    } elsif ($recnum == 0 && $output{MSLalt}[$recnum] != -999) {
	$GeoHt = $output{MSLalt}[$recnum];
    } else {
	$GeoHt = -999.0;
    }
    if (&DEBUGoutput) { print "GeoHt $GeoHt\n"; }

    # print new record to output file.
    my $outputRecord = sprintf " %6.1f %6d %6.1f %6.1f %6d -999.0 -999.0 -999.0 -999.0 -999.0 -999.0\n",
            $output{pressure}[$recnum],		# hPa
	    #$output{MSLalt}[$recnum], 		# m
	    $GeoHt, 				# m
            $output{temp}[$recnum],		# C
            $output{dewpt}[$recnum],		# deg C
	    $output{rh}[$recnum]; 		# %

    print OUTFILE $outputRecord;
    }
}

#-----------------------------------------------------------------------------#
#----------------------------- NetCDF Subroutines-----------------------------#
#-----------------------------------------------------------------------------#
#
# These subroutines process files in NetCDF format.  They refer to dimensions,
# variable and attributes as shown in the following section of an ncdump on a 
# NetCDF file: 
#
# netcdf 20020513_0000 {
#dimensions:
#        maxProviderIdLen = 12 ;
#        maxStaIdLen = 6 ;
#        recNum = UNLIMITED ; // (4726 currently)
#variables:
#        char providerId(recNum, maxProviderIdLen) ;
#                providerId:long_name = "Data Provider station Id" ;
#                providerId:reference = "station table" ;
#        char stationId(recNum, maxStaIdLen) ;
#                stationId:long_name = "alphanumeric station Id" ;
#                stationId:reference = "station table" ;
#        float latitude(recNum) ;
#                latitude:long_name = "latitude" ;
#                latitude:units = "degree_north" ;
#                latitude:_FillValue = 3.402823e+38f ;
#                latitude:missing_value = -9999.f ;
#                latitude:reference = "station table" ;
#        double observationTime(recNum) ;
#                observationTime:long_name = "time of observation" ;
#                observationTime:units = "seconds since 1-1-1970" ;
#                observationTime:_FillValue = 3.40282346e+38 ;
#                observationTime:missing_value = -9999. ;
#
# In this dump snippet, the dimensions are maxProviderIdLen, maxStaIdLen, and
# recNum.  recNum is the record dimension, and there are currently 4726 records
# in this file (they are not shown here for brevity).  There are 4 variables
# shown: providerId, stationId, latitude, and observationTime.  Provider ID
# is a two dimensional variable.  One dimension is the record number, and the
# other is the length of the string containing the provider ID.  Attributes are
# named variable:attribute, i.e. latitude:units.  Latitude has 5 attributes,
# observation time has 4, and stationID has two.
#
# Not all of the info in the NetCDF file is used by this code.
#
#---------------------------------------------------------------------
# @signature void getData()
# <p>Read in all the data for each variable that occurs in out QCF output
# i.e. time, station info, temperature, dewpoint, etc.
# Ignore other variables
#
# @output  %var{$variable}{values} Adds the array values to the hash for
#       each variable
#---------------------------------------------------------------------
sub getData {
    my $ncid = shift;
    my $recDimName = shift;
    my $var = shift;
    my $recdimLen = shift;	# nomber of records in the record dimension

    my $variable;

    foreach $variable (&getFields) {
        #--------------------------------------------------------------------- 
        # Make sure that the variable the user has requested in getFields 
        # actually exists in the data.
        #--------------------------------------------------------------------- 
	if (!defined($var->{$variable})) {
            print "WARNING: Unknown variable $variable requested by user";
            print " in code at getFields declaration.\n";
            exit(1);
        }
    }

    print OUTFILE "------------------------------------------------------------------------------\n";
    #print OUTFILE "   PRES   HGHT   TEMP   DWPT    RELH   MIXR   DRCT   SKNT   THTA   THTE   THTV\n";
    print OUTFILE "   PRES   GHGT   TEMP   DWPT    RELH   MIXR   DRCT   SKNT   THTA   THTE   THTV\n";
    print OUTFILE "    hPa     m      C      C       %    g/kg    deg   knot     K      K      K\n";
    print OUTFILE "------------------------------------------------------------------------------\n";
   

    # Loop over each record (height) in the netCDF file, read in the data for
    # that record
    foreach (my $record=0; $record < $recdimLen; $record++) {


        if (&DEBUG) {print "Reading in data for record $record\n";}

        # Loop through each parameter we want to extract from the raw data
        # as given in the descriptor file
        # Assign parameters to a raw record structure.
        foreach $variable (&getFields) {
            if (&DEBUG) {print "Reading in data for variable $variable\n";}

            my @values = ();
            my $dimLen;

            #-------------------------------------------------------------------
            # If the variable has only one dimension, 
            # then read it in. Note that varget saves the data to
            # the first index of @values.  It appears to save the data point as
            # a float, so information on significant digits is lost, and we get
            # numbers like 38.4943313598633
            #-------------------------------------------------------------------
            if ($var->{$variable}{ndims} == 1 ) {
                my @coords = ($record);
                my @counts = (1);

                if (NetCDF::varget($ncid,$var->{$variable}{varid},\@coords,
                      \@counts, \@values) == -1) {
                die "Can't get data for variable $variable:$!\n";
                }

                $var->{$variable}{values}[$record] = $values[0];
            }
            
            #-------------------------------------------------------------------
            # Otherwise, warn the user that we don't know how to read in the
            # variable.
            #-------------------------------------------------------------------
            else {
                print "ERROR: Don't know how to read in variable: $variable(";
                foreach my $dim ( 0 .. $var->{$variable}{ndims}-1) {
                    my $dimname = $var->{$variable}{dimname}[$dim];
                  print "$var->{$variable}{dimname}[$dim]=$var->{$variable}{$dimname} ";
                }
                print ")\n";
            }


        } # End foreach $variable

    } # End foreach $record

    # Now we have all the variables for all the records, so loop through again, do calculation (some
    # of which are dependent on multiple records) and print the record.
    &parse_data_rec($recdimLen,$ARGV,$var);

}
#---------------------------------------------------------------------
# Get date and location from global atts in input netCDF file.
#---------------------------------------------------------------------
sub get_global_atts {
    my $ncid = shift;
    my $atttype;
    my %global_atts= ();
    # Get observation time from global atts ($varid = -1)
    foreach my $attname ("lat","lon","year","month","day","hour","minute","second") {
        if (NetCDF::attinq($ncid,-1,$attname,$atttype,my $attlen) == -1) {
                die "Can't inquire of attribute type of $ARGV:$!\n";
        }
        my @value;
        if (NetCDF::attget($ncid,-1,$attname,\@value) == -1) {
                die "Can't inquire of value of attribute of $ARGV:$!\n";
        }
	# Assume all atts have numeric values (int,short,float,double). Char and
	# byte won't work this way. Need to pack chars and store byte as ptr to
	# array.
	$global_atts{$attname} = $value[0];
    }

    return(%global_atts);
}
#---------------------------------------------------------------------
# Data fields of interest..
#---------------------------------------------------------------------
sub getFields {
    my @names = (MSL_alt,Temp,Vp,Pres);
    return(@names);
}

sub getVar {
    my $varname = shift;
    my $var = shift;
    my $index = shift;

    my $value = $$var{$varname}{values}[$index];
    if (&DEBUGoutput) {print "$varname $index $value\n";}
    return $value;
}
