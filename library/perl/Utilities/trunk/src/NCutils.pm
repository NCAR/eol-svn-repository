#!/usr/bin/perl -w
#
##Module------------------------------------------------------------------------
# <p>The purpose of the NCutils module is to extract data from NetCDF files
# so they can be used to produce ascii and other forms of the data.
#
# @use    use lib "/net/work/lib/perl/Utilities";
# @use    use NCutils;
# <p>The functions can be called like any other function.</p>
#
# @author Janine Goldstein
# @version 1.0
##Module------------------------------------------------------------------------

package NCutils;
use Exporter;
our @ISA = ("Exporter");
our @EXPORT = qw(readNetCDFheader);
$| = 1;

#-------------------------------------------------------------------------------
# Note that a backslash before a variable indicates a reference to that
# variable.
#-------------------------------------------------------------------------------
# For more information on the PERL NetCDF package, see the NetCDF User's Guide 
# for C at http://www.unidata.ucar.edu/packages/netcdf/guidec/guidec-3.html
# There is no perl guide to NetCDF - C is the closest you will get.  The syntax
# is different between PERL and C.  I worked out the syntax used in this code
# by trial and error.  The differences are consistent, so comparison of this 
# code to the manual should allow you to easily discern the pattern and change
# the syntax of other C NetCDF routines on the fly.
#-------------------------------------------------------------------------------

# Include NetCDF module in this code
use NetCDF;

use strict 'vars';

#********************************************************************************
# There are a ton of print statements in this code for debugging and 
# informational purposes.  Turn them on or off and see what you get (-:
sub DEBUG       {return 0; }
sub DEBUGFileStats {return 0;}		# debug info for netCDF subroutine
sub DEBUGgetV {return 0;}               # debug info for netCDF subroutine

#********************************************************************************

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
#--------------------------------------------------------------------------
# @signature int readNetCDFheader($ARGV)
# <p>Read data from a NetCDF file given as input to this
# subroutine via the variable $ARGV.
#
# @input  $ARGV - the name of the input file.
#--------------------------------------------------------------------------
sub readNetCDFheader {
    my $ARGV = shift;
        #-----------------------------------------------------------------------
        # Open the input NetCDF file for reading
        #
        # @input $ARGV  - the name of the input file.
        # @input   0    - A zero value specifies that the datafile should 
        #                 be opened with read-only access.
        # @output $ncid - the NetCDF ID used to identify the file. It functions
        #                 like a file pointer, though the syntax is different.
        #-----------------------------------------------------------------------
        my $ncid;
        if (($ncid = NetCDF::open($ARGV,0)) == -1) {
            die "Can't open $ARGV:$!\n";
        }

        #-----------------------------------------------------------------------
        # Find out the name of the record dimension ($recDimName) and how many 
        # variables ($nvars) are in the input file. The other values read in are
        # not essential to this code, so identify them in case future mods need 
        # them, then ignore them.
        #-----------------------------------------------------------------------
        my $recDimName;
        my $nvars;
        my $recdimsize; 	# The number of records in the file.
        ($recDimName,$nvars,$recdimsize) = &getFileStats($ncid,$ARGV);

        #-----------------------------------------------------------------------
        # If the number of records in the file is zero, then there is no data in
        # this file.  Warn the user and get the next file.
        #-----------------------------------------------------------------------
        if ($recdimsize == 0) {
            print "WARNING: File $ARGV contains no records\n";
            return($recdimsize,"0");
        }

        #-----------------------------------------------------------------------
        # Read in all the information about the variables in this NetCDF file,
        # i.e. variable name, type, dimensions, attributes.
        #-----------------------------------------------------------------------
        my %var = &getVariableDescriptions($ncid,$nvars,$ARGV);
        my $var = \%var;

        #-------------------------------------
        # All done.  Close the input file
        #-------------------------------------

        if (NetCDF::close($ncid) == -1) {
            die "Can't close $ARGV:$!\n";
        }

        return($recDimName, $var, $recdimsize);
}
#-------------------------------------------------------------------------------
# @signature void getFileStats($ARGV)
# <p>Find out the name of the record dimension and how many variables ($nvars)
# are in the input file. The other values read in are not essential to this
# code, so identify them in case future mods need them, then ignore them.
#
# @input  $ncid - the NetCDF ID of the input file from the previous call to 
#                 nc_open
# @input  $ARGV - the name of the input file.  Used for error reporting only.
#
# @output $recDimName - the name of the record dimension.
# @output $nvars - the number of variables in the input file.
# @output $dimsize - the number of records in the file
#-------------------------------------------------------------------------------
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

    if (&DEBUGFileStats) {
        print "The id assigned to $input_file is $ncid\n";
        print "The total number of dimensions in $input_file is $ndims\n";
        print "The total number of variables in $input_file is $nvars\n";
        print "The total number of attributes in $input_file is $natts\n";
        print "The name of the record dimension is $recDimName\n";
        print "The size of the record dimension is $dimsize\n";
    }

    return($recDimName,$nvars,$dimsize);
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
#    <li> $name (i.e. latitude)
#    <li> $var{$name}{varid} = 22;
#    <li> $var{$name}{datatype} = float,
#    <li> $var{$name}{ndims} = 1 ( ${$name}{recNum}=8446 )
#    <li> $var{$name}{dimname}[0] = recNum;
#    <li> $var{$name}{natts} = 5
#    <li> and for each att, $var{$name}{$attname} = value
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

        if (&DEBUGgetV) {print "variable # $varid:\n\tname = $name,";}
        if (&DEBUGgetV) {print "\n\tdata type = $var{$name}{datatype},\n\t";}
        if (&DEBUGgetV) {print "number of dimensions = $var{$name}{ndims} ( ";}

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
            if (&DEBUGgetV) {print "$dimname=$var{$name}{$dimname} ";}
        }

        # Assign the information on the number of attributes to the hash.
        $var{$name}{natts} = $natts;
        if (&DEBUGgetV) 
                    {print ")\n\tNumber of attributes = $var{$name}{natts}\n";}

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
            if (&DEBUGgetV) {print "\t$attname length = $attlen\n";}
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
                if (&DEBUGgetV) 
                    {print "\t$attname = \"$var{$name}{$attname}\"\n";}
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
            if (&DEBUGgetV) {print "\t$attname = ";
                foreach my $i (@{$var{$name}{$attname}}) {print "$i ";}
                print "\n";}
        }
    }
    return(%var);
}

1; # Needed to return from a script call
