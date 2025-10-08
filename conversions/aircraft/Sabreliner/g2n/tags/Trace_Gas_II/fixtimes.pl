#!/usr/bin/perl -w
################################################
# For the Trace_GAS_II project, the data are OK. 
# The time is just off by factor of 10, so a 1 
# hr flight is recorded as a 6 minute flight. The
# times are wrong in the GENPRO files, so the
# converted files are also wrong.
#
# This script was written to read in the converted
# netCDF files, fix the times, and re-write out
# the data to a new netCDF file. It also adds a 
# Time dimension to be consistent with current 
# RAF NetCDF file standards.
#
# Written by Janine Aquino May 11, 2011
################################################
use NetCDF;

# Usage: fixtimes.pl <file.nc>
$ARGV = $ARGV[0];

# Open the original netCDF file, and determine what
# dimensions, variables, and attributes are in the file.
# Write that info to a hash called "var".
$ncid = NetCDF::open($ARGV,0);
($recDimName,$nvars,$recdimLen,$ndims,$natts) = &getFileStats($ncid,$ARGV);
my %var = &getVariableDescriptions($ncid,$nvars);
my $var = \%var;

# Open the output file that will hold the fixed netCDF data.
$ncidOut = NetCDF::create("$ARGV.2",0);

# Loop through the dimensions in the original file,
# and write each one to the output file.
# Convert the time from 10hz to 1hz.
my $id = {};
my $varid = {};
my $filesize;
foreach (my $dim=0; $dim <$ndims; $dim++) {
    NetCDF::diminq($ncid,$dim,$name,$size);
    #print "$name $size\n";
    if ($name eq "Time") {
	$size = $size*10;
	$filesize = $size;
        $id->{$name} = NetCDF::dimdef($ncidOut, $name, $size);
    }
}

# Write the time variables to the output file.
$varid->{"Time"} =
	NetCDF::vardef($ncidOut, "Time", NetCDF::FLOAT, [$id->{"Time"}]);
$varid->{"base_time"} =
	NetCDF::vardef($ncidOut, "base_time", 4 , \[]);
$varid->{"time_offset"} =
	NetCDF::vardef($ncidOut, "time_offset", NetCDF::FLOAT, [$id->{"Time"}]);

# Read through the original file and write all the remaining variables to
# the output file.
foreach $variable (sort keys %var) {
   if ($variable ne "base_time" && $variable ne "time_offset") {
       #print "$variable\n";
     $varid->{$variable} = NetCDF::vardef($ncidOut, $variable, NetCDF::FLOAT, [$id->{"Time"}]);
   }
   #print "$variable $varid->{$variable}\n";

   # For each variable, get the attributes from the original file and write
   # them to the output file for that variable.
   my @attval;
   my $name;
   foreach my $attnum ( 0 .. $var->{$variable}{natts} -1) {
       NetCDF::attname($ncid, $var->{$variable}{varid}, $attnum, $name);
       NetCDF::attinq($ncid,$var->{$variable}{varid},$name,$atttype,$attlen);
       NetCDF::attget($ncid, $var->{$variable}{varid}, $name, \@attval);
       #print "$name $atttype $attlen\n";
       if ($name ne "OutputRate") {
           NetCDF::attput($ncidOut,$varid->{$variable},$name, $atttype, \@attval);
       }
   }
}
# Now copy the global vars from the original to output file.
foreach my $attnum ( 0 .. $natts-1) {
   NetCDF::attname($ncid, NetCDF::GLOBAL, $attnum, $name);
   NetCDF::attinq($ncid,NetCDF::GLOBAL,$name,$atttype,$attlen);
   NetCDF::attget($ncid, NetCDF::GLOBAL, $name, \@attval);
   #print "$name $atttype $attlen\n";
   NetCDF::attput($ncidOut,NetCDF::GLOBAL,$name, $atttype, \@attval);
}

# That's the end of the netCDF header info, so closedef the output file.
NetCDF::endef($ncidOut);

# Begin writing the data...

# Create the data for the Time dimension and write it to the output file.
@start=(0);
@count=($filesize);
@values=(0 .. $filesize-1);
NetCDF::varput($ncidOut,$varid->{"Time"},\@start,\@count,\@values);

# Copy the base_time
@start=(0);
@count = (1);
@values = (); 
NetCDF::varget($ncid,$var->{"base_time"}{varid},\@start,\@count,\@values);
#print "@values\n";
NetCDF::varput($ncidOut,$varid->{"base_time"},\@start,\@count,\@values);

# Copy the time_offset
NetCDF::varget($ncid,$var->{"time_offset"}{varid},\@start,\@count,\@values);
@start=(0);
@count=($filesize);
@values=($values[0] .. $values[0]+$filesize-1);
NetCDF::varput($ncidOut,$varid->{"time_offset"},\@start,\@count,\@values);

# Copy the other variables
foreach $variable (sort keys %var) {
   if ($variable ne "base_time" && $variable ne "time_offset") {
   #print "$variable\n";
   #print $varid->{$variable}."\n";

   # Read each data chunk in one record at a time
   foreach (my $record=0; $record < $recdimLen; $record++) {
      #print $var->{$variable}{ndims}."\n";

      # Data must be treated differently depending on the number of
      # dimensions.
      if ($var->{$variable}{ndims} == 2) {
	  # Read in data that has (Time, sps10)
          @start=($record,0);
          $dim2Len = $var->{$variable}{$var->{$variable}{dimname}[1]};
          @count = (1,$dim2Len);
          @values = (); 
          NetCDF::varget($ncid,$var->{$variable}{varid},\@start,\@count,\@values);
	  # Write it out as 1-D data - Time
	  #print "@values\n";
          @start=($record*$dim2Len);
          @count = ($dim2Len);
	  #print "@start @count\n";
	  #print $variable."\n";
	  #print $varid->{$variable}."\n";
	  NetCDF::varput($ncidOut,$varid->{$variable},\@start,\@count,\@values);
      } elsif ($var->{$variable}{ndims} == 1) {
	  # Read in data that just has dimension Time
          @start=($record);
          @count = (1);
          @values = (); 
          NetCDF::varget($ncid,$var->{$variable}{varid},\@start,\@count,\@values);
	  # Write that data as every 10th value and pad the data in the
	  # middle with the missing value read from the variable
	  # attributes.
          @start=($record*10);
          @count = (10);
	  for (my $i=1;$i<=9;$i++) {
	      push(@values,sprintf("%6.1f",$var->{$variable}{missing_value}[0]));
          }
	  #print "@values\n";
          NetCDF::varput($ncidOut,$varid->{$variable},\@start,\@count,\@values);
      } elsif ($var->{$variable}{ndims} == 0) {
	  # If the data has no dimensions, then it is just a single value,
	  # so copy it directly.
          @start=(0);
          @count = (1);
          @values = (); 
          NetCDF::varget($ncid,$var->{$variable}{varid},\@start,\@count,\@values);
          NetCDF::varput($ncidOut,$varid->{$variable},\@start,\@count,\@values);
      } else {
	  # Catch anything I missed.
	  print "ERROR: $variable $var->{$variable}{ndims} \n";
	  exit(1);
      }
   }
   }
}

# All done.
NetCDF::close($ncidOut);
#---------------------------------------------------------------------
# The subroutines below this line were copied from the nc2qcf.pl code I
# wrote a few years ago.
#---------------------------------------------------------------------
sub getFileStats {
    my $ncid = shift;
    my $input_file = shift;

    my $ndims;		# The number of dimensions defined for this NetCDF
                        # input file. 
    my $nvars;		# The number of variables defined for this file.
    my $natts;		# The number of global attributes defined for this file.
    my $recdim;		# A pointer to which dimension is the record dimension.
                        # The record dimension is the dimension that contains 
                        # an integer giving the number of records in the file. 
                        # It is defined dynamically when the file is 
                        # written and can grow or shrink as necessary.
    my $recDimName;	# The name of the record dimension.  
    my $dimsize;	# The number saved in the record dimension = the number
                        # of records in the file.

    # NetCDF::inquire()
    # Inquire of a NetCDF file how many dimensions, variable, and global
    # attributes it has, and which dimension is it's record dimension.
    # 
    # @input - the NetCDF id of the file
    #
    # @output - the number of dimensions, variables, and global attributes
    #           in the file, and the location of the file's record dimension.

    if (NetCDF::inquire($ncid,$ndims,$nvars,$natts,$recdim) == -1) {
        die "Can't inquire of $input_file:$!\n";
    }

    # If there is no record dimension, make the first dimension the record
    # dimension.
    if ($recdim == -1) {$recdim = 0;}

    # NetCDF::diminq
    # Inquire of a NetCDF dimension, it's name and size, given a pointer to it.
    #
    # @input - the NetCDF id of the file
    # @input - the location of the file's record dimension
    #
    # @output - the name of the file's record dimension
    # @output - the size of the file's record dimension

    if (NetCDF::diminq($ncid,$recdim,$recDimName,$dimsize) == -1) {
        die "Can't inquire record dimension of $input_file:$!\n";
    }

        print "The id assigned to $input_file is $ncid\n";
        print "The total number of dimensions in $input_file is $ndims\n";
        print "The total number of variables in $input_file is $nvars\n";
        print "The total number of attributes in $input_file is $natts\n";
        print "The name of the record dimension is $recDimName\n";
        print "The size of the record dimension is $dimsize\n";

    return($recDimName,$nvars,$dimsize,$ndims,$natts);
}

#---------------------------------------------------------------------
# @signature void getVariableDescriptions()
# <p>Read in all the information about the variables in this NetCDF file,
# i.e. variable name, type, dimensions, attributes. Each variable has an
# associated table of information specifying attributes of that variable,
# i.e., the ID of the variable (uniquely specifies this variable - like a 
# pointer to that variable), the data type of the data stored in the variable
# (char, float, etc), the number of dimensions of this variable (does it 
# contain a scalar, an array, a matrix), the name and size of each dimension,
# and how many other attributes there are, and what they are, i.e. a long
# name for the variable, the units the data in the variable are in, how missing
# is defined, etc.
#
# @input  $ncid - the file id of the input file.
# @input  $nvars
# @input  $ARGV - the name of the input file.  Used for error reporting only.
#
# @output  %{$name} Returns a hash for each variable name that contains:
# <ul>
#    <li> $name = latitude,
#    <li> $var{latitude}{varid} = 22;
#    <li> $var{latitude}{datatype} = float,
#    <li> $var{latitude}{ndims} = 1 ( ${latitude}{recNum}=8446 )
#    <li> $var{latitude}{dimname}[0] = recNum;
#    <li> $var{latitude}{natts} = 5
#    <li> $var{latitude}{long_name} = "latitude"
#    <li> $var{latitude}{units} = "degree_north"
#    <li> $var{latitude}{_FillValue} = 3.40282346638529e+38
#    <li> $var{latitude}{missing_value} = -9999
#    <li> $var{latitude}{reference} = "station table"
# </ul>
#
#---------------------------------------------------------------------
sub getVariableDescriptions {
    my $ncid = shift;
    my $nvars = shift;

    # The NetCDF files encode the data types as a char. Use this hash to
    # unencode these types.
    my %data_types = (1,'byte',2,'char',3,'short',4,'int',5,'float',6,'double');

    # Create a hash for 
    # this variable to store all the information about the variable.
    my %var = ();

    # Loop through all the variables in the NetCDF input file.
    # Supposedly foreach $i ($X..$Y) is faster than 
    # for ($i=$x;$i<=$y;$i++)
    #for (my $varid = 0;$varid <$nvars; $varid++)
    foreach my $varid (0 ..$nvars-1) {
        my @dimids;
        
        # Given the NetCDF file ID and the variable ID, find out the variable
        # name, the data type the data is stored as (float, etc), the number
        # of dimensions of this variable, an array of pointers to the dimensions
        # and the number of variable attributes assigned to this variable.
        NetCDF::varinq($ncid,$varid, my $name, my $datatype,my $ndims,\@dimids,
                        my $natts);

        # Now that $name contains the name of the variable, assign all the
        # information about the variable to the hash $var{$name}
        $var{$name}{varid} = $varid;
        $var{$name}{datatype} = $data_types{$datatype};
        $var{$name}{ndims} = $ndims;
        $var{$name}{dimids} = \@dimids;
        $var{$name}{natts} = $natts;

        print "variable # $varid:\n\tname = $name,";
        print "\n\tdata type = $var{$name}{datatype},\n\t";
        print "number of dimensions = $var{$name}{ndims} ( ";

        # Loop through each of the dimensions of the variable and determine
        # the dimension name and size.
        # Supposedly foreach $i ($X..$Y) is faster than 
        # for ($i=$x;$i<=$y;$i++)
        #for (my $dim = 0;$dim <$var{$name}{ndims}; $dim++) 
        foreach my $dim (0 .. $var{$name}{ndims}-1) {
            my $dimname; my $dimsize;
            if (NetCDF::diminq($ncid,$dimids[$dim],$dimname,$dimsize) == -1)
                {die "Can't inquire dimension of dimension # $dim:$!\n";}
            $var{$name}{dimname}[$dim] = $dimname;
            $var{$name}{$dimname} = $dimsize;
            print "$dimname=$var{$name}{$dimname}";
        }

        # Assign the information on the number of attributes to the hash.
        $var{$name}{natts} = $natts;
        print ")\n\tNumber of attributes = $var{$name}{natts}\n";

        # Loop through each of the attributes assigned to this variable and
        # determine the attribute name, type, length, and value.
        # Supposedly foreach $i ($X..$Y) is faster than 
        # for ($i=$x;$i<=$y;$i++)
        #for (my $attnum = 0;$attnum <$var{$name}{natts}; $attnum++) 
        foreach my $attnum ( 0 .. $var{$name}{natts} -1) {

            # determine attribute name
            my $attname;
            if (NetCDF::attname($ncid,$varid,$attnum,$attname) == -1) {
                die "Can't inquire of attribute name of $ARGV:$!\n";
            }

            # determine attribute data type and length
            my ($atttype, $attlen);
            if (NetCDF::attinq($ncid,$varid,$attname,$atttype,$attlen) == -1) {
                die "Can't inquire of attribute type of $ARGV:$!\n";
            }
            print "\t$attname length = $attlen\n";
            $var{$name}{$attname}{attlen} = $attlen;

            # Convert the attribute type from a number to a descriptive string.
            $var{$name}{atttype} = $data_types{$atttype};

            # determine the attribute value.  The value is read in as an array
            # of numbers.  If the attribute contains a string, the numbers 
            # represent chars and we need to pack the chars together to get the
            # string.  If the attribute contains a number, then it should be the
            # first value in the array, unless the attribute is a 
            # comma-separated list of numbers, in which case they populate the
            # array.
            my @value;
            if (NetCDF::attget($ncid,$varid,$attname,\@value) == -1) {
                die "Can't inquire of value of attribute of $ARGV:$!\n";
            }

            if ($var{$name}{atttype} eq "char") {
                my $str = pack("C*",@value);
                $var{$name}{$attname} = $str;
                print "\t$attname = \"$var{$name}{$attname}\"\n"
            } elsif ($var{$name}{atttype} eq "byte") {
                #store as ptr to an array
                $var{$name}{$attname} = \@value;
                foreach my $val ($var{$name}{$attname}) 
                    {$val = unpack('C',$val);}
            } elsif ($var{$name}{atttype} eq "int" ||
                     $var{$name}{atttype} eq "short" ||
                     $var{$name}{atttype} eq "float" ||
                     $var{$name}{atttype} eq "double") {
                #store as ptr to an array
                $var{$name}{$attname} = \@value;
            } else {
                print "WARNING: Unknown attribute type $var{$name}{atttype}\n";
                exit(1);
            }
            print "\t$attname = ";
            foreach my $i (@{$var{$name}{$attname}}) {print "$i ";}
            print "\n";
        }
    }
    return(%var);
}

