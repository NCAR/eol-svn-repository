#!/usr/bin/perl
################################################################################
# Version 7.7.1
################################################################################
# plot_converted_data.pl: Uses gnuplot and convert to plot specified parameters
# and stations from data in the four CEOP formats.  Uses the gnuplot_template to
# make calls to gnuplot.  QCF data can also be plotted with this script.
#
# This program can plot a single parameter at a single height on each plot in
# black and white (default), a single parameter at a single height on each plot
# where color is used to denote the flag associated with each value (-c option),
# or a single parameter at all available heights where each height is a randomly
# assigned color (-m option).
#
# Dan Sullivan
# October, 2003
# Based on the plotit.pl script written for qcf data.  
#
# Updated 1/12/2004 by Janine Goldstein ( Version 2 )
# so that the AWK command is called once for each station/height combination
# rather than once for each station/ht/parameter combination.  This increased
# processing speed significantly.
#
# Updated 1/13/2004 by Janine Goldstein ( Version 3 )
# Added functionality to reuse plotdata files if they already exist rather than
# regenerating them.
#
# Updated 1/14/2004 by Janine Goldstein ( Version 4 )
# To plot multiple heights/depths of a single parameter for the same station on
# the same plot.  Invoked with the -m flag.
#
# Updated 1/21/2004 by Janine Goldstein ( Version 5.1 )
# CSEID and begin and end dates were constants in gnuplot_template so it had to
# be modified each time the code was run.  Now they are in this script and
# passed to gnuplot_template. CSEID is picked from data, and begin and end date
# are set at the top of this code as constants.  Next version will make them
# command line options.
# 	Modified code to make black and white plots for single ht plots, but
# to make each height a different color on multi height plots.
#
# Updated 1/22/2004 by Janine Goldstein ( Version 5.2 )
# Modified to make begin and end date command line options.
#
# Updated 1/22/2004 by Janine Goldstein ( Version 5.3 )
# Modified to create output dir if it does not exist.
#
# Updated 1/26/2004 by Janine Goldstein ( Version 5.4 )
# Modified to optionally make color plots for single ht plots via the flag
# -c.  Colors are assigned based on the flag associated with each value. If the
# only flag type is 'M', then the parameter will not be plotted. Type 
# "plot_converted_data.pl" for details. Also modified to automatically create
# subdirs for station, multi-height, and color.
#
# Updated 1/28/2004 by Janine Goldstein ( Version 5.5 )
# Made text of some messages clearer.  Fixed bug where color and multiHt subdirs
# were being created even if these plots were not being generated. Added that if
# the only flag type is 'N' or 'I' and the format is qcf, then the parameter 
# will not be plotted.  Added check for modification of input file that is more
# recent than formation of .plotdata file.  If this is the case, then regenerate
# .plotdata file. Fixed other minor bugs involving code dying gracefully when
# user performs unsupported actions.
#
# Updated 2/3/2004 by Janine Goldstein ( Version 6.0 )
# Project was hard-coded into gnuplot_template.  Modified to be a required input
# parameter.  Bug in code to determine if .plotdata needs to be reformed because
# input file that is more recent than formation of .plotdata file.  Fixed.
# Changed to run with "new" version of gnuplot, "gnuplot.new" which supports 
# line widths.  Hardcoded in a linewidth of 2.5 x the default for data flagged
# unchecked and good.  Added static yrange for BAMEX.  Other projects may be
# added by creating a yrange hash and adding it to this code.  BAMEX will
# always plot with static yranges.  All other projects will plot with dynamic
# ranges.
#
# Updated 3/5/2004 by Janine Goldstein (Version 6.1 )
# Code was erroneously comparing modification times between .plotdata files, 
# which led to the code trying to use one plotdata file to generate another.
# Fixed this by only including non-plotdata files in $data_files string.
# USAGE:
#
# Updated 6/28/2004 by Janine Goldstein ( Version 6.2 )
# 1) Filepointer INFILE was not being closed.  Added close statement.
# 2) Added removal of .plotdata file when done using it if the -r flag is set.
# The .plotdata file will be kept by default so it can be used for color and
# multiht plots. 3) The awk syntax to parse
# a given station/height out of the raw data was *very* slow.  Replaced with
# a grep on a pattern match making that section of code 5x as fast. Then decided
# to change from a grep to a while loop so that blank gusts can be replaced
# with an 'N' and columns after the gust column will be determined consistently
# by gnuplot.
# 4) Changed while (!eof(IN) ) { $line = <IN>; to while (<IN>) and that section
# of code is 4x as fast.  Also changed one other occurances of this syntax at
# PARAM. 5) Fixed height count in output filenames to only add
# height designation when file has multiple heights. 6) If files are gzipped, 
# code will unzip them.  Note that it goes through the files twice, so this 
# adds quite a bit of time to the processing (but saves space as only one file
# is unzipped at a time).  To save time, unzip them in advance. 7) Changed
# file naming so that files are named with actual height rather than a height
# count (_h01, etc). 8) Added a space after search for station ID so that
# APR and APRX4 won't both show up in the APR .plotdata file, and consequently
# be plotted on top of each other. 9) Removed option to plot elevation (Elev)
# from qcf format.  It shouldn't change, so it makes a boring plot, and we don't
# plot lat/lon, nor do we plot lat/lon/elev for any other format. 10) Fixed a
# bug in color plotting that tried to use the value after Ceiling_Ht (Ceiling
# Flag indicator) as the QC flag, when the QC flag is really the next value 
# after that.
#
# Updated 6/28/2004 by Janine Goldstein ( Version 6.3 )
# Sometimes we include a : in the network name, i.e. for MADIS data we prepend
# ms: to the network (ms:APRSWXN).  This : messes up the sed command, so we have
# escaped it with a \.
#
# Updated 7/14/2004 by Janine Goldstein ( Version 6.4 )
# 1) Code is intermittently and unpredictably creating magic files
# when it tries to run the convert command.  Added code to try to pin down this
# problem. 2) Also added ability to plot data files containing more than one 
# network.  Code will loop over networks the way it loops over stations.
# Added -n <networks> option to command line.  Added network to output dir tree
# and filenames. 3) Changed color scheme to be easier to see.  4) Added key to 
# right of each plot. 5) Fixed bug where -r command was removing .plotdata files
# before multi ht plots are created.
#
# Updated 8/3/2004 by Janine Goldstein ( Version 6.5 )
# 1) Code combines CSEid and refSiteId into a single string using a separator
# char so they can be processed like a network, then splits them to create
# subdirs.  When the separator char occured in the Id, this created errors.
# Code has been fixed to check for separator char in Id, and die if it exists.
# Separator char is hardcoded just below this comment section and can be changed
# if necessary. 2) Made key to right of each plot optional via a command line 
# option (-l). 3) Made missing value a fn of param since present weather has a 
# missing value of -999 rather than -999.99 for everything else.
#
# Updated 1/26/2005 by Janine Goldstein ( Version 6.6 )
# 1) Bug fix:  In last update, missed a spot where separatorChar was hardcoded.
# Replaced with ref to var separatorChar.
#
# Updated 2/4/2005 by Janine Goldstein ( Version 6.7 )
# 1) Bug fix:  Found final spot where separatorChar was hardcoded (multi loop).
# Replaced with ref to var separatorChar.
#
# Updated 2/14/2005 by Janine Goldstein ( Version 6.8 )
# 1) Bug fix:  $mtime was never being set.  Fixed.
#
# Updated 7/20/2005 by Janine Goldstein ( Version 7.0 )
# Added ability to plot JOSS precipitation formats, i.e. 15min pcp, hrly pcp,
# and dly pcp.  15 and hrly are considered pqc data - code will choose between 
# them automatically. daily is dqc data.
# In order to facilitate this change, code now differentiates between Network 
# and CSEid style data, rather than qcf and not qcf.  PQC exception in 
# create_plotdata contains the meat of the changes necessary to handle 
# precipitation data.
# NOTE: This code parses the input data on whitespaces, so networks or stations 
# that contain a space will NOT plot correctly.  The notable case of this is 
# "Rec rainga" data.  The generic solution to this will require a fundamental 
# change in how the fields are parsed.  For now, user will have to change Rec 
# rainga to Rec:rainga in the data to be plotted.
#
# Updated 9/14/05 by Janine Goldstein ( Version 7.2 )
# Changed perl path from /bin/perl to /usr/bin/perl.
#
# Updated 10/18/2005 by Janine Goldstein ( Version 7.3 )
# Sometimes we include a / in the network name, i.e. NCEP/EMC This / messes up 
# the sed command, so we have replaced it with a _ .  I accomplished this by
# moving handling of the : in MADIS data to a subroutine called 
# &escape_chars, and adding handling of / to that subroutine.
#
# Updated 7/4/2006 by Janine Goldstein ( Version 7.4 )
# Minor update to move colored plotting for 'M' flag out of Network only loop
# so 'M' data with a non-missing value will be plotted for CEOP data as
# well as QCF data.
#
# Updated 5/23/2007 by Janine Goldstein ( Version 7.5 )
# Fixed bug in propagation of missing value (-999 or -999.99). Added
# plotting for multiple stations/single parameter on a single plot using -S
# option.
#
# Updated 5/24/2007 by Janine Goldstein ( Version 7.6 )
# Wrapped temporary version that handles ms: into the main version. This
# version now handles both colons (i.e. ms:APRSWXN) and slashes 
# (i.e. NCEP/EMC) in the same code. It should be invisble to the user.
#
# Updated 5/24/2007 by Janine Goldstein ( Version 7.7 )
# Changed the separatorChar from a '-' or '_' to a ctrl char (Hex 31).
# The hope is that this will eliminate having to change between dash and
# underscore if one or the other char appears legitimately in the network
# or station name.
#
# Updated 5/29/2007 by Janine Goldstein ( Version 7.7.1)
# Fixed typo.  When making recent changes I accidentally changed a system
# call from "system" to "sys" (line 1268). Fixed.
#
# By executing the script with no arguments, i.e.
#
#   ./plot_converted_data.pl
#
# you will be given an extensive usage statement. In brief, usage is:
#
# plot_converted_data.pl <format> -P <project> -t <starttime> <endtime> 
#   -d <data files> -p <parameters> -h <heights> -s <stations> -n <networks> 
#   -f <params file> -o <out dir> -k -m -v -l
#
#   format must be specified
#   -d must be specified
#   -p can be ALL, if not specified ALL
#   -s can be ALL, if not specified ALL
#   -h can be ALL, if not specified ALL
#   -n can be ALL, if not specified ALL
#
# -or-
#
# plot_converted_data.pl <format> -help 
#   lists the possible parameters
################################################################################
use File::Basename;

#-------------------------------------------------------------------------------
# CSEIds and reference site ids are combined into a single string using the 
# separatorChar and returned as CSEids.  Define that char here so it is only 
# defined in one place.
#-------------------------------------------------------------------------------
my $separatorChar = "\x31";

#*******************************************************************************
# Construct the needed hash tables
# Two hash tables are needed for each format, one that defines the column number
# for each parameter (with the first column being 1, as opposed to 0) and one 
# that defines the parameter units.  Depending on the format specfied the, 
# %pcolumns and %punits hash tables are assigned to a set of these hashes below.
#*******************************************************************************

#-------------------------------------------------------------------------------
# QCF data
#-------------------------------------------------------------------------------
my %qcf_pcolumns = ( STN_Press => 11, SL_Press => 13,  
             Comp_SL_Press => 15, Air_Temp => 17, Dew_Pt => 19, Wind_Spd => 21, 
             Wind_Dir => 23, Precip => 25, Squall_Gust => 28, Present_Wx => 30, 
             Visibility => 32, Ceiling_Ht1 => 34, Ceiling_Ht2 => 39, 
             Ceiling_Ht3 => 44 );

my %qcf_punits = ( STN_Press => "mb", SL_Press => "mb", 
             Comp_SL_Press => "mb", Air_Temp => "Celsius", Dew_Pt => "Celsius", 
             Wind_Spd => "m/s", Wind_Dir => "deg", Precip => "mm", 
             SG_Flag => "char",  Squall => "m/s", Present_Wx => "code_val",
             Visibility => "m", Ceiling_Ht1 => "Hundreds of ft", 
             Ceiling_Ht2 => "Hundreds of ft", Ceiling_Ht3 => "Hundreds of ft" );

#-------------------------------------------------------------------------------
# PQCF data:
# WARNING: If you try to plot anything other than JOSS PQCF 15min, hrly, or
# daily precip formats, unknown results may occur.
#
# See "PQC exception:" in the subroutine create_plotdata for an explanation
# of why the column # is set to 8.
#-------------------------------------------------------------------------------
my %pqc_pcolumns = (precip => 8);
my %pqc_punits = ( precip => "mm" );

#-------------------------------------------------------------------------------
# CEOP sfc data
#-------------------------------------------------------------------------------
my %sfc_pcolumns = ( stn_press => 11, air_temp => 13, dew_pt => 15, 
          rel_humid => 17, specific_humid => 19, wind_spd => 21, wind_dir => 23,
          u_wind_comp => 25, v_wind_comp => 27, precip => 29, snow_depth => 31, 
          short_in => 33, short_out => 35, long_in => 37, long_out => 39,
          net_rad => 41, skin_temp => 43, par_in => 45, par_out => 47 );	

my %sfc_punits = ( stn_press => "hPa", air_temp => "C", dew_pt => "C", 
          rel_humid => "%", specific_humid => "g/kg", wind_spd => "m/s", 
          wind_dir => "degrees", u_wind_comp => "m/s", v_wind_comp => "m/s", 
          precip => "mm", snow_depth => "cm", short_in => "W/m^2", 
          short_out => "W/m^2", long_in => "W/m^2", long_out => "W/m^2",
          net_rad => "W/m^2", skin_temp => "C", par_in => "umol/m^2/s", 
          par_out => "umol/m^2/s" );	

#-------------------------------------------------------------------------------
# CEOP flx data
#-------------------------------------------------------------------------------
my %flx_pcolumns = ( sens_heat_flux => 12, lat_heat_flux => 14, CO2_flux => 16, 
          soil_heat_flux => 18 );
my %flx_punits = ( sens_heat_flux => "W/m^2", lat_heat_flux => "W/m^2", 
          CO2_flux => "umol/m^2/s", soil_heat_flux => "W/m^2" );

#-------------------------------------------------------------------------------
# CEOP stm data
#-------------------------------------------------------------------------------
my %stm_pcolumns = ( soil_temp => 12, soil_moist => 14 );
my %stm_punits = ( soil_temp => "C", soil_moist => "%" );

#-------------------------------------------------------------------------------
# CEOP twr data
#-------------------------------------------------------------------------------
my %twr_pcolumns = ( stn_press => 12, air_temp => 14, dew_pt => 16, 
          rel_humid => 18, specific_humid => 20, wind_spd => 22, wind_dir => 24,
          u_wind_comp => 26, v_wind_comp => 28 );
my %twr_punits = ( stn_press => "hPa", air_temp => "C", dew_pt => "C", 
          rel_humid => "%", specific_humid => "g/kg", wind_spd => "m/s", 
          wind_dir => "degrees", u_wind_comp => "m/s", v_wind_comp => "m/s" );
 
#*******************************************************************************
# These hash table define static Y-ranges for parameters in specific field
# projects.  Projects and parameters that are not defined here have dynamically
# determined Y-ranges.
#*******************************************************************************
my %yrange = ();

%{$yrange{"BAMEX"}} = (SL_Press => [980,1050], Comp_SL_Press => [980,1050],
          Air_Temp => [0,40], Dew_Pt => [-10,30], Wind_Spd => [0,30], 
          Wind_Dir => [0,360]);

#-------------------------------------------------------------------------------
# A hash that maps each format to the column hash
#-------------------------------------------------------------------------------
my %fmts_pcolumns = ( "qcf" => \%qcf_pcolumns,
	                    "pqc" => \%pqc_pcolumns,
	                    "dqc" => \%pqc_pcolumns,
	                    "sfc" => \%sfc_pcolumns,
	                    "flx" => \%flx_pcolumns, 
	                    "stm" => \%stm_pcolumns,
	                    "twr" => \%twr_pcolumns 
                    );

#-------------------------------------------------------------------------------
# A hash that maps each format to the unit hash
#-------------------------------------------------------------------------------
my %fmts_punits = ( "qcf" => \%qcf_punits,
                    "pqc" => \%pqc_punits, 
                    "dqc" => \%pqc_punits, 
                    "sfc" => \%sfc_punits, 
                    "flx" => \%flx_punits, 
                    "stm" => \%stm_punits,
                    "twr" => \%twr_punits,
                  );

#-------------------------------------------------------------------------------
# The height column of each format
#-------------------------------------------------------------------------------
my %fmts_height_col = ( "qcf" => undef(),
			"pqc" => undef(),
			"dqc" => undef(),
			"sfc" => undef(),
			"flx" => 11,
			"stm" => 11,
			"twr" => 11
			);
#-------------------------------------------------------------------------------
# The character number in the row where the height begins for each format
#-------------------------------------------------------------------------------
my %fmts_height_charcol = ( "qcf" => undef(),
			    "pqc" => undef(),
			    "dqc" => undef(),
			    "sfc" => undef(),
			    "flx" => 108,
			    "stm" => 108,
			    "twr" => 108
			  );

#-------------------------------------------------------------------------------
# The station column of each format
#-------------------------------------------------------------------------------
my %fmts_stn_col = ( "qcf" => 6,
			 "pqc" => 4,
			 "dqc" => 3,
			 "sfc" => 7,
			 "flx" => 7,
			 "stm" => 7,
			 "twr" => 7,
			);
#-------------------------------------------------------------------------------
# The station character number of each format
#-------------------------------------------------------------------------------
my %fmts_stn_charcol = ( "qcf" => 45,
			 "pqc" => 31,
			 "dqc" => 19,
			 "sfc" => 61,
			 "flx" => 61,
			 "stm" => 61,
			 "twr" => 61,
			);
#-------------------------------------------------------------------------------
# netType allows us to create a single command to work with all data types.
# A netType of Network means that the network name is given as a single word
# in the column previous to the station id. A netType pf "CSEid" means that
# the network is defined by the two columns previous to the station id, the
# way that CEOP data has CSI and reference site components to the network name.
# Also there are 10 possible flag values for Network data (i.e. qcf, pqc, dqc) 
# and 7 for CEOP data that are hardcoded within the code.
#-------------------------------------------------------------------------------
my %hash_netType = ( "qcf" => "Network",
            "pqc" => "Network",
            "dqc" => "Network",
            "sfc" => "CSEid",
            "flx" => "CSEid",
            "stm" => "CSEid",
            "twr" => "CSEid",
           );

#-------------------------------------------------------------------------------
# This is the end of what you should have to change if you are adding a new 
# format.  There are some hardcoded cases within the code that may need to be
# changed depending on the particulars of the new format (e.g. flag values 
# section and the gust column exception for qcf data).  Searching for the string
# "exception" should lead you to most of these. This code assumes that 
# the network id/ reference site id is in the column right before the station id
# for all formats.
#-------------------------------------------------------------------------------

#*******************************************************************************
# Print the usage if no arguments given					
#*******************************************************************************
if( @ARGV == 0 ) 
{
	printUsage();
	exit;
}
print (STDERR "Script began at: ".scalar(localtime)."\n");

#*******************************************************************************
# Read in all the command line arguments
#*******************************************************************************

#-------------------------------------------------------------------------------
# Get the format
#-------------------------------------------------------------------------------
my $format = shift( @ARGV );

#-------------------------------------------------------------------------------
# Check for validity of format
#-------------------------------------------------------------------------------
if( !defined( $fmts_pcolumns{$format} ) )
{
	print( STDERR "Format does not exist!  Possible formats are: \n" );
	foreach my $fmt( sort keys %fmts_pcolumns )
	{
		print( STDERR "  " . $fmt . "\n" );
	}
	exit 0;
}

#-------------------------------------------------------------------------------
# Get the columns, units, station_col number, and height_col number for
# this format.
#-------------------------------------------------------------------------------
my %pcolumns = %{$fmts_pcolumns{$format}};
my %punits = %{$fmts_punits{$format}};
my $stn_col = $fmts_stn_col{$format};
my $height_col = $fmts_height_col{$format};
my $stn_charcol = $fmts_stn_charcol{$format};
my $height_charcol = $fmts_height_charcol{$format};

#-------------------------------------------------------------------------------
# Print the available parameters for this format if -help option used
#-------------------------------------------------------------------------------
if( $ARGV[0] eq "-help" )
{
	showPossibleParams( $format, \%pcolumns, \%punits );
	exit 0;	
}


#-------------------------------------------------------------------------------
# Get the arguments from the command line.
#-------------------------------------------------------------------------------
(my $para_ref, my $files_ref, my $cseids_ref, my $stations_ref, my $heights_ref,
 my $param_file, my $out_dir, my $keep, my $removeplotdata, my $verbose, 
 my $multi, my $color, my $project, my $begindate, my $enddate, my $legend,
 my $multistn) 
      = getArguments();

my @params = @$para_ref;
my @files = @$files_ref;
my @stations = @$stations_ref;
my @heights = @$heights_ref;
my @cseids = @$cseids_ref;

#-------------------------------------------------------------------------------
# If project not defined on the command line, warn user and exit.
#-------------------------------------------------------------------------------
if ($project eq "")
       {
       print( STDERR "ERROR: Project must be specified on the command line." .
                      "Use the -P flag\n");
       exit 0;
       }

#-------------------------------------------------------------------------------
# If no begin and end times are specified on the command line, or the format of
# the times is wrong, warn user and exit.
#-------------------------------------------------------------------------------
if ($begindate eq "" || $enddate eq "") 
       {
       print( STDERR "ERROR: Dates must be specified on the command line." .
                      "Use the -t flag\n");
       exit 0;
       }
if ($begindate !~ /[0-9][0-9][0-9][0-9]\/[0-1][0-9]\/[0-3][0-9]/ ||
    $enddate !~ /[0-9][0-9][0-9][0-9]\/[0-1][0-9]\/[0-3][0-9]/)
       {
       print( STDERR "ERROR: Dates must be of format YYYY/MM/DD." .
                     " Values entered were $begindate and $enddate");
       exit 0;
       }

#-------------------------------------------------------------------------------
# If $out_dir does not exist, create it. If outdir is more than one dir below
# the current, this will create a tree below the current dir if necessary.
#-------------------------------------------------------------------------------
if ($out_dir ne "." && !(-e $out_dir)) 
       {
       my @dirs = split ( "/", $out_dir );
       my $dirpath = ".";
       foreach my $newdir (@dirs)
              {
              $dirpath = $dirpath."/".$newdir;
              if (!(-e $dirpath)) {system ("mkdir $dirpath");}
              }
       }

#-------------------------------------------------------------------------------
# If no data files are specified to extract data from, extract data from all
# plottable data files in the $out_dir subdir with format $format.
#-------------------------------------------------------------------------------
if( @files == 0 )
	{
        opendir (DIR, "$out_dir") or die "Can\'t open dir $out_dir:$!\n";

        #-----------------------------------------------------------------------
        # If all stations are being processed, find all files in the outdir that
        # are of the format specified on the command line and are plottable data
        # files (end in .plotdata)
        #-----------------------------------------------------------------------
        if( $stations[0] =~ /ALL/)
                {
                @files = grep /${format}.plotdata$/, readdir DIR;
                }
        #-----------------------------------------------------------------------
        # Otherwise just find files in the outdir that are of the format 
        # specified AND ARE FOR THE STATIONS SPECIFIED ON THE COMMAND LINE.
        #-----------------------------------------------------------------------
        else
                {
                @dirfiles = readdir DIR;
                foreach my $station (@stations)
                        {
                        @newfiles = grep /$station.*${format}.plotdata$/, 
                                    @dirfiles;
                        push @files, @newfiles;
                        }
                }
        closedir (DIR);

        #-----------------------------------------------------------------------
        # Append the path to the plottable data files names.
        #-----------------------------------------------------------------------
        foreach my $file (@files)
                {
	        $file = $out_dir . "/" . $file; 
                }
        }

#-------------------------------------------------------------------------------
# If we didn't find any files in $out_dir with format $format, warn user.
#-------------------------------------------------------------------------------
if ( @files == 0 )
        {
        print( STDERR "There are no files of format $format in $out_dir.\n".
              "        Please specify an input file on the command line via\n".
              "        the -d option.\n");
        exit(1);
        }

#-------------------------------------------------------------------------------
# Determine the parameters the user wants to plot.  If they were specified on
# the command line, the @params already contains them ( from call to 
# getArguments ).  If they were not specified on the command line, then @params
# contains the default word "ALL".  Replace this word with the actual list of
# parameters from the parameter hash,....
#-------------------------------------------------------------------------------
@params = keys( %pcolumns ) if( $params[0] eq "ALL" );

#-------------------------------------------------------------------------------
# or read in the parameters from a file if the -f option was used
#-------------------------------------------------------------------------------
if( defined( $param_file) )
{
	my $size = @params;

	open( PARAM, "<$param_file" ) or die
                "Unable to open specified parameter file: $param_file\n";

	while( $p = <PARAM> )
	{
		chop( $p );
		$params[$size++] = $p; 
	}	
        close( PARAM );
}

my %units = ();
my %column = ();

#-------------------------------------------------------------------------------
# Get the column for each parameter in the list of paramaters to plot.
#-------------------------------------------------------------------------------
foreach my $param (@params) 
{
	my $c = $pcolumns{$param};

	if( defined( $c ) )
	{ 
   	$column{$param} = $c;
		$units{$param} = $punits{$param}; 
	}
	else
	{ 
		print( STDERR "Parameter $param does not exist!!".
                              "  Ignoring this given parameter.\n" );
	}
}

#-------------------------------------------------------------------------------
# Get a list of stations, heights, CSEids & reference site ids or networks,
# contained in the input data files.  
#-------------------------------------------------------------------------------
(my $all_stns_ref, my $all_heights_ref, my $all_CSEids_ref) = 
        getStationsAndHeightsAndCSEids( \@files, $stn_col, $height_col, 
        $separatorChar);

my %all_stns = %$all_stns_ref;
my @all_heights = @$all_heights_ref;
#my @all_CSEids = @$all_CSEids_ref;

#-------------------------------------------------------------------------------
# If user specified all heights should be plotted (or did not
# specify anything), assign list of heights found in data to heights array.
#-------------------------------------------------------------------------------
@heights = @all_heights if( $heights[0] eq "ALL" );
$heights[0] = undef() if( @heights == 0 || !defined( $height_col ) );

#-------------------------------------------------------------------------------
# netType allows us to create a single command to work with all data types.
#-------------------------------------------------------------------------------
my $netType = $hash_netType{$format};

#-------------------------------------------------------------------------------
# If the user specified network/cseids on the command line then compare them to
# nets/cseids/refs found in the data and create an array that contains only
# the nets/cseids we want to plot.
#-------------------------------------------------------------------------------

if ($cseids[0] ne "ALL") {
        my $key;
        for (my $i=0; $i < scalar(@cseids); $i++) {
                # Values in @cseids are just the cseid, but keys in
                # all_stns are cseid_refid.  So have to do funky stuff
                # to match them.
                foreach $key (keys %all_stns) {
                        if ($key == $cseids[$i]) {last;}
                }
                if ($key != $cseids[$i]) {
                        print ( STDERR "WARNING: $netType $cseids[$i] not ".
                        "found in raw data. It will not be plotted.\n");
                        splice @cseids, $i ,1;
                }
        }
} else {
        # User wants to plot all networks/cseids that are in the raw data
        @cseids = sort keys %all_stns
}

#-------------------------------------------------------------------------------
# Now we only have networks/cseids that we want to plot in the cseid
# array.  If the user set stations to plot on the command line, they are now
# in @stations.  But we don't know which network goes with each station.  
# Determine that now.
#-------------------------------------------------------------------------------
print( STDERR "$netType to plot: \n" );
foreach my $cseid (@cseids) {
        print STDERR "User wants to plot $netType $cseid\n" if ($verbose);

        # If we have data for the cseid/network in question, find the stations.
        if (exists $all_stns{$cseid}) {
                print STDERR "Found a match: $cseid\n" 
                        if ($verbose);
                if ($stations[0] eq "ALL") {
                        # User wants to plot all stations that are in the
                        # raw data for the chosen networks
                        foreach my $stn_in_data (keys %{$all_stns{$cseid}}){
                                print STDERR "User wants to plot station ".
                                        "$stn_in_data\n" if ($verbose);
                                $all_stns{$cseid}{$stn_in_data} = "p";
                        }
                } else {
                        # Just print the stations listed on the command line.
                        foreach my $stn (@stations) {
                                print STDERR "User wants to plot station $stn\n"
                                        if ($verbose);
                                foreach my $stn_in_data
                                  (keys %{$all_stns{$cseid}}){
                                        if ($stn eq $stn_in_data) {
                                                print STDERR "Found $stn in ".
                                                "data for $netType $cseid\n"
                                                        if ($verbose);
                                                $all_stns{$cseid}{$stn}="p";
                                        }
                                }
                        }
                }
        } else {
                print STDERR "$netType $cseid not found in raw data\n";
                $cseid = undef();
                next;
        }
#-------------------------------------------------------------------------------
# At this point, in the array @cseids, we have a list of the networks/cseids
# we want to plot, and in the hash %all_stns, each net/cseid and station combo
# that we want to plot is assigned the value "p". Note that if user just asked
# for a single station, and did not specify the network (so network is ALL) at
# this point we have all the networks in the file and the single station with
# it's network set to "p".  We still need to remove the rest of the nets from
# @cseids so we don't try to plot them.
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Show the user what is being plotted
# and create subdirs by station within $out_dir
#-------------------------------------------------------------------------------

        # Delete entries in hash that are not marked "p".
        foreach my $stn (sort keys %{$all_stns{$cseid}})
        {
                if ($all_stns{$cseid}{$stn} ne "p")
                {
                        delete $all_stns{$cseid}{$stn};
                }
        }

        # See if there are any stations with value "p" left for this network
        my $dirpath = $out_dir;
        my @stns = sort keys %{$all_stns{$cseid}};
        if (scalar(@stns) != 0)
        {
                # Let the user know which networks we are plotting
        	print( STDERR "  ", $cseid, "\n" );
                print( STDERR "\tStations to plot in $netType $cseid: \n" );

                # Create subdirs for each network we want to plot
                # For data with a CSEid and RefSite, the CSEid and RefSite have
                # been combined via a separator char.  Split them here.
		if (!$multistn) {
                my @dirs;
                $cseid_escaped = &escape_chars($cseid);
                if ($netType eq "CSEid") 
                        {@dirs = split ($separatorChar, $cseid_escaped );}
                else    {@dirs = $cseid_escaped;}
                foreach my $newdir (@dirs)
                        {
                        $dirpathnet = $dirpath."/".$newdir;
                        if (!(-e $dirpathnet)) {system ("mkdir $dirpathnet");}
                        }
		}
        } else {
                print STDERR 
                        "There are no stations to plot for $netType $cseid\n";
                $cseid = undef();
                next;
        }

        foreach my $stn (@stns)
        {
                # Let the user know which stations we are plotting.
                if ($all_stns{$cseid}{$stn} ne "p") {next;}

        	print( STDERR "\t  ", $stn, "\n" );
                if (!$multistn && !(-e $dirpathnet."/".$stn))
                        {
                        system ("mkdir $dirpathnet/$stn");
                        }
                if ($color && !(-e $dirpathnet."/".$stn."/color"))
                        {
                        system ("mkdir $dirpathnet/$stn/color");
                        }
                if ($multi && !(-e $dirpathnet."/".$stn."/multiHt"))
                        {
                        system ("mkdir $dirpathnet/$stn/multiHt");
                        }
                if ($multistn && !(-e $dirpath."/multiStn"))
                        {
                        system ("mkdir $dirpath/multiStn");
                        }
        }
} # End loop of cseids/nets.

#-------------------------------------------------------------------------------
# If there are no cseid's that have stations attached, warn user and exit
# In this case, array @cseids should have only undef elements, which perl
# is supposed to collapse down to a single undef value according to O'Reilly
# "Programming Perl 2nd edition".  Doesn't seem to work though, so have to 
# do it myself.
#-------------------------------------------------------------------------------
while (scalar(@cseids) > 1 && $cseids[0] eq undef()) {shift @cseids; next;}
if (scalar(@cseids) == 1 && $cseids[0] eq undef()) {
        print STDERR "No data to plot - exiting\n\n\n";
        exit(1);
}

print( STDERR "Heights to plot: \n" );
if (!defined($heights[0]))
        { print( STDERR "This is a single height input file.\n" ); }
foreach my $ht (@heights)
{
	print( STDERR "  ", $ht, "\n" );
}
if ($multi)
{ 
    if (defined($heights[0]))
    {
        print( STDERR "All heights for a single parameter will be plotted on\n",
               "       a single graph.\n");
    } else  {
        print( STDERR "Multi is not a valid option.\n"); exit(1);
    }
}
if ($multistn)
{
    if($multi)
    {
	print( STDERR "Specifying both multiHt and MultiStn is invalid.\n"); 
	exit(1);
    } else {
	print( STDERR "All stns for a single parameter will be plotted on\n",
               "	a single graph.\n");
    }
}

print( STDERR "Parameters to plot: \n" );
foreach my $prm (@params)
{
	print( STDERR "  ", $prm, "\n" );
}

if (exists $yrange{$project})
{
        print( STDERR "Static plot limits being used:");
        foreach my $parm (keys %{$yrange{$project}}) 
        {
                print( STDERR "\n  $parm: ");
                foreach my $val (@{${$yrange{$project}}{$parm}}) 
                        {print( STDERR "$val ");}
        }
        print( STDERR "\n");
}
else 
{
        print( STDERR "No yrange info for project $project\n");
        print( STDERR "Dynamic plot ranges being used.\n");
}

if ($color) 
{
        print( STDERR "Color will be used to denote flags associated with\n",
               "       each value on the plot.\n");
}
	
my $data_files = "";

#*******************************************************************************
# Now create the plots
#*******************************************************************************

#-------------------------------------------------------------------------------
# Determine when input file was last modified.  We will use this later to 
# determine if the plottable data file is up to date with the input data file.
#-------------------------------------------------------------------------------
my $mtime = ""; 

#-------------------------------------------------------------------------------
# Append all raw input data files to a single string containing the list of
# files. Do not include *plotdata files previously generated by this code.
#-------------------------------------------------------------------------------
foreach my $file (@files)
{
        if ($file !~ m/plotdata/) {
	    $data_files = $data_files . " " . $file; 
            # Days since last modification of most recently 
            # modified data file.
            if ($mtime eq "") {$mtime = (-M $file);}
            if ($mtime ne "" && $mtime > (-M $file)) {$mtime = (-M $file);}
	    print STDERR "Plotting from data file $file last modified $mtime",
		" days ago.\n";
        }
}

my @infiles = undef();
my %missing = ();
my %plot_string = ();	# String that contains the gnuplot plot command for
			# multi-ht  or multi-stn plots.
#-------------------------------------------------------------------------------
# Assume gnuplot_template is in the same directory as this code.  If this dir is
# remote, we need to append the path to gnuplot_template for use by gnuplot.
#-------------------------------------------------------------------------------
($name, $path, $suffix) = fileparse($0);
my $template = $path."gnuplot_template";

#-------------------------------------------------------------------------------
# Loop through networks/CSEid_refSiteID
#-------------------------------------------------------------------------------
foreach my $cseid (@cseids)
{
        if (!$multistn) {
        print(STDERR "Making plots for $netType $cseid for project $project\n");
        }

        # cseid cannot have a : in it or it messes up the sed command
        # It is used in the plot title and the filenames, so escape the :.
        # This was added to deal with MADIS data that has network names like
        # ms:APRSWXN. JAG 6/28/04
        # Escape a "/" in the cseid to allow networks like "NCEP/EMC" 
        # JAG 10/17/05
        my $cseid_escaped = &escape_chars($cseid);

        #-----------------------------------------------------------------------
        # Loop through stations
        #-----------------------------------------------------------------------
        foreach my $station (keys %{$all_stns{$cseid}})
        {
                if ($all_stns{$cseid}{$station} ne "p") {next;}
                if ($multi) 
			{%plot_string = (); @infiles = undef(); %missing = ();}
                #---------------------------------------------------------------
                # Loop through heights
                #---------------------------------------------------------------
	        foreach my $height (sort @heights )
	        {
                        #-------------------------------------------------------
                        # Create filenames for the plotdata file.
                        #-------------------------------------------------------
        		my $filename = "${out_dir}/${cseid_escaped}_${station}";
                        if (defined ($height)) 
                            {$filename .= "_h".$height;}
        		my $infile = $filename . ".${format}.plotdata";

                        #-------------------------------------------------------
                        # Create a pattern match string containing only the 
                        # station and height we are trying to plot.
                        #-------------------------------------------------------
                        my $stnstr;
        		if( defined( $height ) )
        		{
                                # The number of chars between the station and
                                # the height is just the location of the height 
                                # minus the sum of the location of the station 
                                # and the length of the station (including a 
                                # single space after it).
                                my $coldiff = $height_charcol - $stn_charcol -
                                      length($station) -1;
                                # The space after $station is so that station
                                # APR doesn't match APRX4 as well.
			        $stnstr = 
                             "\^\.{$stn_charcol}$station \.{$coldiff} *$height";
		        }	
        		else	
        		{
                                # The space after $station is so that station
                                # APR doesn't match APRX4 as well.
        			$stnstr = "^.{$stn_charcol}$station ";
        		}	

                        #-------------------------------------------------------
                        # If plottable data file does not exist, or it exists
                        # but has zero size, reform it.
                        #-------------------------------------------------------
                        if (!(-e $infile) || (-z $infile))
                        {
                                if ( $verbose && -z $infile)
                                {
                                print( STDERR 
                                      "$infile was of zero size - reforming\n");
                                }

                                &create_plotdata(
                                         $infile, $data_files, $stnstr,$format);
                        }
                        #-------------------------------------------------------
                # If original input data modified more recently than plottable
                # data file, reform plottable data file to be sure it has most
                # recent data. BUT don't compare an input file that IS a 
                # plottable data file (because user did not supply an input data
                # file) to the plottable data file we are currently using. We
                # only want to regerate from a real raw data file.
                        #-------------------------------------------------------
                        elsif ($mtime ne "" && $mtime < (-M $infile))
                        {
                        my $time = (-M $infile);
                        print( STDERR "WARNING: Input data files $data_files ");
                        print( STDERR "modified more".
                              " recently than plottable data file $infile. ".
                              " $mtime < $time . ".
                              " Recreating $infile.\n");

                        &create_plotdata($infile, $data_files, $stnstr,$format);
                        }
                        #-------------------------------------------------------
                        # ...otherwise go ahead and use the old plottable data
                        # file because it contains the most recent data.
                        #-------------------------------------------------------
                        else
                        {
			my $time = (-M $infile);
                        print( STDERR "Plottable data file $infile",
                              " already exists. Last modified $time days ago.",
			      " It will not be recreated.\n");
                        }

                        #-------------------------------------------------------
                        # if the .plotdata file is empty, then there is not data
                        # for this station/height combination.  Warn user and 
                        # continue to next station/height.
                        #-------------------------------------------------------
                        if (-z $infile) 
                        {
                            print STDERR "Input file $infile has zero size.".
                                 " Proceeding".
                                 " to next station/height combination.\n";
                            next;
                        }
                        #-------------------------------------------------------
                        # Loop through parameters
                        #-------------------------------------------------------
        		foreach my $param (keys %units) 
        		{
                                #-----------------------------------------------
                                # Create filenames for the gnuplot script, ps 
                                # file, and gif file.
                                #-----------------------------------------------
                                my $dir = $cseid_escaped;
				#For sfc, flx, stm, and twr data, the cseid is
				# a combination of the cseid and ref site, 
				# combined with the separator char.  Create
				# correct subdir path here by changing separator
				# char to /.
                                if ($netType eq "CSEid") 
					{$dir =~ s/$separatorChar/\//;}
		        	my $file_name = "${out_dir}/${dir}/${station}";
                                if ($color) {$file_name .= "/color";}
                                $file_name .= "/${cseid_escaped}_${station}_${param}";
                                if (defined ($height)) 
                                                   {$file_name .= "_h".$height;}
	        		my $gnu_in = $file_name . ".gnuplot";
	        		my $ps_file= $file_name . ".ps";
	        		my $gif_file= $file_name . ".gif";

                                #-----------------------------------------------
                                # Height string for title of plot
                                #-----------------------------------------------
		        	my $theight = "";
		        	$theight = "Height $height" 
                                                       if( defined( $height ) );

                                #-----------------------------------------------
                                # Missing value for this parameter
                                #-----------------------------------------------
                                if ($param =~ /Present_Wx/) 
                                     {$missing = "-999";} 
                                else 
                                     {$missing = "-999.99";}
                                #-----------------------------------------------
                                # Y-range to plot this parameter over.
                                #-----------------------------------------------
                                $Yrange = "";
                                my $Yrange;
                                if (exists ${$yrange{$project}}{$param})
                                     {
                                     my $ymin = ${$yrange{$project}}{$param}[0];
                                     my $ymax = ${$yrange{$project}}{$param}[1];
                                     $Yrange = "set yrange [$ymin\\\:$ymax]";
                                  print( STDERR "$Yrange for $project $param\n")
                                     if ( $verbose );
                                     }
	
                                #-----------------------------------------------
                                # Create gnuplot input file
                                #-----------------------------------------------
		        	printf( STDERR 
                                   "Creating gnuplot input file: $gnu_in...\n" )
                                    if( $verbose );

                                #-----------------------------------------------
                                # Check for the existence of each flag in the 
                                # input file.  
                                #-----------------------------------------------
        		        open( INFILE, "<$infile" ) or die
                                        "Unable to open data file $infile.\n";
                                my %found = ();

                                # Loop through each line in the input file.
                                while (<INFILE>) {

                                        # There are 10 possible flags in a 
					# network file, and seven possible flags
					# for a CSEid file, so once all ten (or 
                                        # 7) are found go ahead and stop.
                                	if (($netType eq "Network"  &&
                                                          keys(%found) == 10) ||
                                	    ($netType eq "CSEid"  &&
                                                          keys(%found) == 7))
                                                {last;}

                                        # Pull the flag out of the line so it
                                        # can be compared to possible flag types
	                                my @arr = split( /\s+/, $_ );

                                        # $arr indices start at zero, so 
                                        # $arr[$column{$param}] is the flag
                                        # associated with param. For Ceiling 
                                        # heights, the flag is the second value
                                        # after the param, not the first.
                                        if ($param =~ /Ceiling_Ht/) {
                                            $_ = $arr[$column{$param}+1];
                                        } else {
                                            $_ = $arr[$column{$param}];
                                        }
                                        chomp; if (/\r$/) {chop;}
                                        if (/G($)/) {$found{G} = 1;}
                                        elsif (/U($)/) {$found{U} = 1;}
                                        elsif (/B($)/) {$found{B} = 1;}
                                        elsif (/C($)/) {$found{C} = 1;}
                                        elsif (/I($)/) {$found{I} = 1;}
                                        elsif (/M($)/) {$found{M} = 1;}
                                        elsif (/D($)/) {$found{D} = 1;}
                                        elsif (/T($)/ && $netType eq "Network")
                                                       {$found{T} = 1;}
                                        elsif (/E($)/ && $netType eq "Network") 
                                                       {$found{E} = 1;}
                                        elsif (/N($)/ && $netType eq "Network") 
                                                       {$found{N} = 1;}
                                        elsif (/X($)/ && $netType eq "Network") 
                                                       {$found{X} = 1;}
                                        else {print STDERR 
                                                  "Unknown flag $_. Exiting\n"; 
                                              exit;}
                                }
                                close ( INFILE );

                                if( $verbose ) 
                                {
                                       print( STDERR 
                                          "Flag found for parameter $param:\n");
                                        foreach my $flagVal (keys %found) 
                                        {
                                        print( STDERR "  $flagVal\n");
                                        }
                                }

                                #-----------------------------------------------
                                # if the only found one flag type for this 
                                # param, and it was 'M' (or 'N' or 'I' for 
				# Network data, then the data are all missing, 
				# so don't generate a plot.
                                #-----------------------------------------------
                                if (keys(%found) == 1)
                                {
                                my @key = keys %found;
                                if (($key[0] eq 'M') || (($netType eq "Network")
				 && ($key[0] eq 'N' || $key[0] eq 'I')))
                                {
			        printf( STDERR "Found only missing data for ".
                                "param $param. $gif_file not created.\n");
                                next;
                                }
                                }
                                #-----------------------------------------------
			# Plots can be made with either a single height on each
                        # plot, or all heights for a given parameter on the same
                        # plot. Multi height plots use a different color for 
                        # each height.  Colors are determined by the gnuplot 
                        # script and may vary plot to plot, i.e. there is no
                        # set color for a given height.  Single height plots use
                        # color to indicate the flag associated with each data
                        # point.  Type "plot_converted_data.pl" for more 
                        # information.
                                #-----------------------------------------------

                                #-----------------------------------------------
                                # $cseid can contain a :.  Escape it for use in
                                # the gnuplot script.
                                #-----------------------------------------------
                                #JAG $infile_escaped = &escape_chars($infile);

                                #-----------------------------------------------
                                # For multi-height plots, generate the plot
                                # command one height at a time.
                                #-----------------------------------------------
                                if ($multi)
                                {
                                my $plotline = 
                                     " \\\\\\\"${infile}\\\\\\\" ".
                                     "using 1\\\:$column{$param} ".
                                     "title \\\\\\\"$height\\\\\\\" with lines";
                        	$plot_string{$param}{$height} = $plotline;
                        	$missing{$param} = $missing;
                                }
                                #-----------------------------------------------
                                # For multi-station plots, generate the plot
                                # command one station at a time.
                                #-----------------------------------------------
				if ($multistn)
				{
				my $plotline = 
				    " \\\\\\\"${infile}\\\\\\\" ".
				    "using 1\\\:$column{$param} ".
				    "title \\\\\\\"$station\\\\\\\" with lines";
                                print "Adding plotting for station $station".
					" for param $param\n";
				$plot_string{$param}{$station} = $plotline;
                        	$missing{$param} = $missing;
				}

                                #-----------------------------------------------
                                # For single height plots, generate the plot
                                # command one data flag at a time.
                                #
                                # First do color plots.
                                #-----------------------------------------------
                                if ($color) 
                                {
                                        #---------------------------------------
                                        # For Ceiling heights, the flag is the 
                                        # second value after the param, not the 
                                        # first. For all other params, it is the
                                        # first.
                                        #---------------------------------------
                                        my $flag = $column{$param} + 1;
                                        if ($param =~ /Ceiling_Ht/) {$flag +=1;}

                                        #---------------------------------------
                                        # Generate plot commands for flags that
                                        # exist.
                                        #---------------------------------------
                                        my $plotline = "";
                                        if ($found{U}) {$plotline .= 
"\\\\\\\"< tr 'U' '1' < ${infile}\\\\\\\" using 1\\\:(\\\$$column{$param}*(\\\$$flag == 1 )) title \\\\\\\"unchecked\\\\\\\" with linespoints 9 7,";}	# grey
                                        if ($found{G}) {$plotline .= 
"\\\\\\\"< tr 'G' '1' < ${infile} \\\\\\\" using 1\\\:(\\\$$column{$param}*(\\\$$flag == 1 )) title \\\\\\\"good\\\\\\\" with linespoints 2 7,";} # green
                                        if ($found{D}) {$plotline .= 
"\\\\\\\"< tr 'D' '1' < ${infile} \\\\\\\" using 1\\\:(\\\$$column{$param}*(\\\$$flag == 1 )) title \\\\\\\"questionable\\\\\\\" with linespoints 6 7,";} # yellow
                                        if ($found{B}) {$plotline .= 
"\\\\\\\"< tr 'B' '1' < ${infile} \\\\\\\" using 1\\\:(\\\$$column{$param}*(\\\$$flag == 1 )) title \\\\\\\"bad\\\\\\\" with linespoints 1 7,";} # red
                                        if ($found{C}) {$plotline .= 
"\\\\\\\"< tr 'C' '1' < ${infile} \\\\\\\" using 1\\\:(\\\$$column{$param}*(\\\$$flag == 1 )) title \\\\\\\"Precip Large or Negative\\\\\\\" with linespoints 4 7,";} # purple
                                        if ($found{M}) {$plotline .= 
"\\\\\\\"< tr 'M' '1' < ${infile} \\\\\\\" using 1\\\:(\\\$$column{$param}*(\\\$$flag == 1 )) title \\\\\\\"missing\\\\\\\" with points 3 7,";} # blue
                                        if (($netType eq "CSEid") && $found{I}) {$plotline .= 
"\\\\\\\"< tr 'I' '1' < ${infile} \\\\\\\" using 1\\\:(\\\$$column{$param}*(\\\$$flag == 1 )) title \\\\\\\"interpolated\\\\\\\" with linespoints 7 7,";} # black
                                        if ($netType eq "Network") {
                                            if ($found{E}) {$plotline .= 
"\\\\\\\"< tr 'E' '1' < ${infile} \\\\\\\" using 1\\\:(\\\$$column{$param}*(\\\$$flag == 1 )) title \\\\\\\"estimated\\\\\\\" with points 7 7,";} # black
                                            if ($found{N}) {$plotline .= 
"\\\\\\\"< tr 'N' '1' < ${infile} \\\\\\\" using 1\\\:(\\\$$column{$param}*(\\\$$flag == 1 )) title \\\\\\\"not available\\\\\\\" with points 3 7,";} # blue
                                            if ($found{I}) {$plotline .= 
"\\\\\\\"< tr 'I' '1' < ${infile} \\\\\\\" using 1\\\:(\\\$$column{$param}*(\\\$$flag == 1 )) title \\\\\\\"incomputable\\\\\\\" with points 3 7,";} # blue
                                            if ($found{T}) {$plotline .= 
"\\\\\\\"< tr 'T' '1' < ${infile} \\\\\\\" using 1\\\:(\\\$$column{$param}*(\\\$$flag == 1 )) title \\\\\\\"trace\\\\\\\" with points 5 7,";} # cyan
                                            if ($found{X}) {$plotline .= 
"\\\\\\\"< tr 'X' '1' < ${infile} \\\\\\\" using 1\\\:(\\\$$column{$param}*(\\\$$flag == 1 )) title \\\\\\\"glitch\\\\\\\" with points 8 12,";} # orange
                                        }

                                        my $plotspec = "plot $plotline";
                                        $plotspec =~ s/,$//;
                                        my $color= "color"; 
			my $ps_file_esc = &escape_chars_colon($ps_file);
    			my $plotspec_esc = &escape_chars_colon($plotspec);
			my $cseid_escaped = &escape_chars_colon($cseid_escaped);

			                system( "sed \"s:XPROJECTX:$project:g; s:XPARAMETERX:$param:g; s:XSTATION:$station:g; s:XHEIGHTX:$theight:g; s:XUNITSX:$units{$param}:g; s:XOUTFILEX:$ps_file_esc:g; s:XPLOTSPECX:$plotspec_esc:g; s:XCOLORX:$color:g; s:XCSEIDX:$cseid_escaped:g; s:XYRANGEX:$Yrange:g; s:XBEGINDATEX:$begindate:g; s:XENDDATEX:$enddate:g; s:XKEYX:$legend:g; s:XMISSINGX:$missing:g; \" $template > \"$gnu_in\"" );

                                	createPlots( $station, $param, $gnu_in,
                                            $ps_file, $gif_file, $verbose, 
					    $keep);
                                }
                                #-----------------------------------------------
                                # Create traditional single-height black and 
                                # white plots.
                                #-----------------------------------------------
                                if (!$multi && !$color && !$multistn)
                                {
                                        my $plotline = " ".
                                            "\\\\\\\"${infile}\\\\\\\"".
                                            " using 1\\\:$column{$param}".
                                            " notitle with lines";
                                        my $plotspec = "plot $plotline";
                                        my $color= "monochrome";
print "*****\n";
			my $ps_file_esc = &escape_chars_colon($ps_file);
			my $plotspec_esc = &escape_chars_colon($plotspec);
			my $cseid_escaped = &escape_chars_colon($cseid_escaped);

                                        system( "sed \"s:XPROJECTX:$project:g; s:XPARAMETERX:$param:g; s:XSTATION:$station:g; s:XHEIGHTX:$theight:g; s:XUNITSX:$units{$param}:g; s:XOUTFILEX:$ps_file_esc:g; s:XPLOTSPECX:$plotspec_esc:g; s:XCOLORX:$color:g; s:XCSEIDX:$cseid_escaped:g; s:XYRANGEX:$Yrange:g; s:XBEGINDATEX:$begindate:g; s:XENDDATEX:$enddate:g; s:XKEYX:$legend:g; s:XMISSINGX:$missing:g; \" $template > \"$gnu_in\"" );

                                        createPlots( $station, $param, $gnu_in,
                                                $ps_file, $gif_file, $verbose,
                                                $keep);
print "*****\n";
                                }

		        } #foreach $param

                        # Remove the plotdata file if the -r flag is set. But
                        # can't remove them now if we need them for multiHt
                        # plots.
                        if( $removeplotdata ) {
                            if ($multi || $multistn) {push (@infiles, $infile);}
                            else { unlink($infile);}
                        }

        	} #foreach $height

                #---------------------------------------------------------------
                # Now that we have generated all the components of the plot 
                # command for multi-height plots, create the combined plot 
                # command and run gnuplot to plot the data.
                #---------------------------------------------------------------
                if ($multi) 
                {
        		foreach my $param (sort keys %plot_string) 
                        {
                                #-----------------------------------------------
                                # Create filenames for the gnuplot script, ps 
                                # file and gif file.
                                #-----------------------------------------------
                                my $dir = $cseid_escaped;
                                if ($netType eq "CSEid") 
					{$dir =~ s/$separatorChar/\//;}
        		        my $file_name = sprintf( 
                                    "${out_dir}/${dir}/${station}/multiHt/".
                                    "${cseid_escaped}_${station}_${param}_multiHt");
                		my $gnu_in = $file_name . ".gnuplot";
                		my $ps_file= $file_name . ".ps";
                		my $gif_file= $file_name . ".gif";

                                #-----------------------------------------------
                                # Create the complete plot command to generate 
                                # multi-height plots from the components stored
                                # in the hash.
                                #-----------------------------------------------
                                my $plotspec = "plot";
		                foreach my $height 
                                (sort {$a <=> $b;} keys %{$plot_string{$param}})
                                # Sort the heights so that they are in numerical
                                # order in the plot key.
                                {
                                    $plotspec = 
                                    $plotspec.$plot_string{$param}{$height}.",";
                                }
                                $plotspec =~ s/,$/\;/;
                                my $color = "color";
		my $ps_file_esc = &escape_chars_colon($ps_file);
		my $plotspec_esc = &escape_chars_colon($plotspec);
		my $cseid_escaped = &escape_chars_colon($cseid_escaped);
				#restore missing val
				$missing = $missing{$param};

                                system( "sed  \"s:XPROJECTX:$project:g; s:XPARAMETERX:$param:g; s:XSTATION:$station:g; s:XHEIGHTX:All Heights:g; s:XUNITSX:$units{$param}:g; s:XOUTFILEX:$ps_file_esc:g; s:XPLOTSPECX:$plotspec_esc:g; s:XCOLORX:$color:g; s:XCSEIDX:$cseid_escaped:g; s:XYRANGEX:$Yrange:g; s:XBEGINDATEX:$begindate:g; s:XENDDATEX:$enddate:g; s:XKEYX:$legend:g; s:XMISSINGX:$missing:g; \" $template > \"$gnu_in\"" );

                                createPlots( $station, $param, $gnu_in, 
                                     $ps_file, $gif_file, $verbose, $keep);
                        }
                        # Now done plotting, so remove .plotdata files
                        foreach my $infile (@infiles) {unlink $infile;}
                }
        } #foreach $station
} #foreach $cseid
#------------------------------------------------------------------------------
# Now that we have generated all the components of the plot command for 
# multi-station plots (which can be cross-network), create the combined plot 
# command and run gnuplot to plot the data.
#------------------------------------------------------------------------------
        if ($multistn) 
        {
		foreach my $param (sort keys %plot_string) 
                {
	        print "Plotting param $param\n\n";
                        #-----------------------------------------------
                        # Create filenames for the gnuplot script, ps 
                        # file and gif file.
                        #-----------------------------------------------
       		        my $file_name = sprintf( 
                            "${out_dir}/multiStn/${param}_multiStn");
              		my $gnu_in = $file_name . ".gnuplot";
               		my $ps_file= $file_name . ".ps";
               		my $gif_file= $file_name . ".gif";

                        #-----------------------------------------------
                        # Create the complete plot command to generate 
                        # multi-height plots from the components stored
                        # in the hash.
                        #-----------------------------------------------
                        my $plotspec = "plot";
	                foreach my $station 
                            (sort {$a <=> $b;} keys %{$plot_string{$param}})
                            # Sort the stations so that they are in alphabetical
                            # order in the plot key.
                        {
			    print "Plotting station $station\n";
                            $plotspec = 
                            $plotspec.$plot_string{$param}{$station}.",";
                        }
                        $plotspec =~ s/,$/\;/;
                        my $color = "color";
		        my $ps_file_esc = &escape_chars_colon($ps_file);
	        	my $plotspec_esc = &escape_chars_colon($plotspec);
			my $cseid_escaped = &escape_chars_colon($cseid_escaped);
			#restore missing val
			$missing = $missing{$param};

                        system( "sed  \"s:XPROJECTX:$project:g; s:XPARAMETERX:$param:g; s:XSTATION:$station:g; s:XHEIGHTX:All Heights:g; s:XUNITSX:$units{$param}:g; s:XOUTFILEX:$ps_file_esc:g; s:XPLOTSPECX:$plotspec_esc:g; s:XCOLORX:$color:g; s:XCSEIDX:$cseid_escaped:g; s:XYRANGEX:$Yrange:g; s:XBEGINDATEX:$begindate:g; s:XENDDATEX:$enddate:g; s:XKEYX:$legend:g; s:XMISSINGX:$missing:g; \" $template > \"$gnu_in\"" );

                        createPlots( $station, $param, $gnu_in, 
                             $ps_file, $gif_file, $verbose, $keep);
                }
                # Now done plotting, so remove .plotdata files
                foreach my $infile (@infiles) {unlink $infile;}
        } #if $multistn

print(STDERR "Script ended at: ".scalar(localtime)."\n\n\n");

exit 0;

################################################################################
# Subroutines
################################################################################

#*******************************************************************************
# The usage statement also serves as complete documentation for the code.

sub printUsage
{
    print( STDERR "\nUsage:\n\n" );
    print( STDERR "  $0 format -P <project> -t <begintime> <endtime> ".
                 " -h <heights> -p <parameters> -f <params file> -n <networks>".
                  " -d <0qc files> -s <stations> -o <out dir> -k -r -m -S -c ".
		  " -l -v\n\n"); 
    print( STDERR "REQUIRED:\n" );
    print( STDERR "  format - the format of the data to plot: qcf, pqc, dqc, " .
                  "sfc, flx, stm, twr\n\n" );
    print( STDERR "  -t Begin and end times to use as the plot range for time.".
                  " Times \n     should be of the format yyyy/mm/dd, and both ".
                  "a beginning and ending time\n     must be specified.\n" );
    print( STDERR "  -P Project.\n\n" );
    print( STDERR "OPTIONS:\n\n" );
    print( STDERR "  -help displays the possible parameters for the specified ".
                  "format. \n      This option must be placed immediatly after".
                  " the format.  \n\n" );
    print( STDERR "NOTE: the following options can be given in any order!\n\n");
    print( STDERR "  -p  List of paramaters to plot. Each image will contain\n",
         "      a single parameter. Use the -help option above to view \n",
         "      possible params. If value 'ALL' is given the script will\n",
         "      attempt to plot all of the parameters, if a parameter\n",
         "      contains all -999.99 values gnuplot will fail and the .ps\n",
         "      file will be deleted. Default is 'ALL'\n" );
    print( STDERR "  -f  File containing a list of parameters to plot.  This\n",
         "      can be used instead of or in addition to the -p option.\n",  
         "      Each line of the file should contain a single parameter.\n" );
    print( STDERR "  -s  The stations to plot. Default is 'ALL'\n" );  
    print( STDERR "  -n  The networks to plot. Default is 'ALL'\n" );  
    print( STDERR "  -h  The heights to plot. Works with CEOP formatted data\n",
         "      Default is 'ALL'\n" );  
    print( STDERR "  -d  The data files to extract data from.\n", 
         "      A pattern match is performed on each file to create a single \n", 
         "      file for each station and height. The files are placed in the\n",
         "      output directory with the name <station>_<height>.<format>.plotdata\n",
         "      If no data files are specified to extract data from, the \n",
         "      script looks for files matching <station>*<format>.plotdata\n",
         "      in the output directory. If no stations are specified the\n",
         "      script looks for files matching *<format>.plotdata in the\n",
         "      output directory. Therefore it is possible to run the script\n",
         "      with the -d option the first time and omit it each\n",
         "      subsequent time for the same set of stations.\n" );
    print( STDERR "  -o  The output directory to place all created files.\n",
         "      This includes the gnuplot input files, the .ps files\n",
         "      generated by gnuplot and the final .gif images. Be sure that\n",
         "      the specified dir exists or the scripts will fail. Defaults \n",
         "      to the current directory.\n" );
    print( STDERR "  -k  If this option is included the gnuplot input files\n",
         "      and the .ps files will NOT be removed, otherwise they will\n",
         "      be. The .plotdata files created for each station will not be\n",
         "      removed. \n" );
    print( STDERR "  -r  Remove the .plotdata file.\n");
    print( STDERR "  -m  If this option is included, the script will plot \n",
         "      multiple heights/depths of a single parameter for the same\n",
         "      station on the same plot. If it is omitted, each parameter/\n",
         "      height combination will be on it's own plot. Works with CEOP\n",
         "      formatted data.\n");
    print( STDERR "  -S  If this option is included, the script will plot \n",
         "      a single parameter for multiple stations on the same plot.\n",
         "      If it is omitted, each parameter/station combination will \n",
         "      be on it's own plot.\n");
    print( STDERR "  -c  If this option is included, the script will plot \n",
         "      single height plots with a different color to indicate the \n",
         "      flag associated with each data point using the following \n",
         "      color scheme:\n",
         "\n",
         "\t color  		CEOP Data flag		QCF flag\n",
         "\t -----  		--------------		--------\n",
         "\t red (3)		B (Bad)			B (Bad)\n",
         "\t yellow (6)		D (questionable		D (questionable\n",
         "\t 			   /dubious)		   /dubious)\n",
         "\t green (1)		G (good)		G (good)\n",
         "\t grey (9)		U (unchecked)		U (unchecked)\n",
         "\t pink (4)		C **			C **\n",
         "\t black (7)		I (Interpolated		E (Estimated)\n",
         "\t 			   /estimated)\n",
         "\t blue (2)		M (missing/could 	M/N/I (Missing/\n",
         "\t 			  not be computed)	  Not available/&&\n",
         "\t cyan (5)					T (trace precip)\n",
         "\t orange (8) 				X (Glitch)\n",
         "\n",
         "      (** = Reported values exceeds output format field size\n",
         "      or was negative precip,\n",
         "      && = Insufficient input to determine a computed value)\n");
    print( STDERR "  -l If this option is included, then a legend will be\n",
         "      printed to the right of the plot.   \n\n"); 
    print( STDERR "  -v  Verbose mode, print status messages as the plots\n",
         "      are created.   \n\n");
    print( STDERR "NOTE: STATIC YRANGE values have been hardcoded for BAMEX\n",
         "      All other projects have dynamic yranges.  A future upgrade\n",
         "      should make this user changeable without having to modify the\n",
         "      code.\n");
    print( STDERR "\nEXAMPLES:\n" );
    print( STDERR "  $0 qcf -t yyyy/mm/dd yyyy/mm/dd -P project -d data/*.0qcf -o out_dir\n" );
    print( STDERR "  $0 qcf -t yyyy/mm/dd yyyy/mm/dd -P project -d data/*.0qcf -o out_dir -c -l\n" );
    print( STDERR "  $0 twr -t yyyy/mm/dd yyyy/mm/dd -P project -p ALL -s Station1 Station2 -o out_dir\n" );
    print( STDERR "  $0 sfc -t yyyy/mm/dd yyyy/mm/dd -P project -f params.in -s Station1 -o ../Station1 -d data/*.0qcf\n" );
    print( STDERR "  $0 sfc -t yyyy/mm/dd yyyy/mm/dd -P project -o out_dir -s Station1 -m\n" );
    print( STDERR "  $0 sfc -help\n" );
    print( STDERR "  cat data.gz | gunzip > tempfile; $0 sfc -t yyyy/mm/dd yyyy/mm/dd -P project -o out_dir -d tempfile\n");

    print( STDERR "\nWARNING:\n  Code will die of network or stn contains a space, i.e. ",
	 "\"Rec rainga\".\n" );
    exit;
}
#*******************************************************************************
# Read in the command line arguments and assign to variables.

sub getArguments
{
	my @params; 
	my @files == 0;
	my @cseids;
	my @stations;
	my @heights;
	my $in_file;
	my $dir = ".";
        my $project = "";
	my $mode = "none";
	my $keep;
        my $removeplotdata = 0;
	my $verbose;
        my $multi = 0;
        my $multistn = 0;
        my $color = 0;
        my $begindate = "";
        my $enddate = "";
        my $legend = "nokey";

	foreach $arg (@ARGV)
	{
		if( $arg eq "-k" )
		{ 
			$keep = 1; 
			$mode= "none"; 
		}
		elsif( $arg eq "-r" )
		{ 
			$removeplotdata = 1; 
			$mode = "none";
		}
		elsif( $arg eq "-v" )
		{ 
			$verbose = 1; 
			$mode = "none";
		}
		elsif( $arg eq "-m" )
		{ 
			$multi = 1; 
			$mode = "none";
		}
		elsif( $arg eq "-S" )
		{ 
			$multistn = 1; 
			$mode = "none";
		}
		elsif( $arg eq "-c" )
		{ 
			$color = 1; 
			$mode = "none";
		}
		elsif( $arg eq "-l" )
		{ 
			$legend = "key outside"; 
			$mode = "none";
		}
		elsif( $arg eq "-p" || $arg eq "-d" || $arg eq "-f" || 
                       $arg eq "-s" || $arg eq "-o" || $arg eq "-h" ||
                       $arg eq "-t" || $arg eq "-P" || $arg eq "-n")
		{ $mode = $arg; }
		else
		{
			if( $mode eq "-t" )
			{
				$begindate = $arg;
                                $mode = "-t2";
			}
			elsif( $mode eq "-t2" )
			{
                                $enddate = $arg;
				$mode = "none";
			}
			elsif( $mode eq "-P" )
			{
				$project = $arg;
				$mode = "none";
			}
			elsif( $mode eq "-f" )
			{
				$in_file = $arg;
				$mode = "none";
			}
			elsif( $mode eq "-o" )
			{
				$dir = $arg;
				$mode = "none";
			}
			elsif( $mode eq "-p" )
			{
				if( $arg eq "ALL" )
				{
					foreach my $par ( sort keys %pcolumns )
					{
						$params[scalar(@params)] = $par;
					}
				}
				else
				{
					$params[scalar(@params)] = $arg;
				}
			}
			elsif( $mode eq "-n" )
			{
				$cseids[scalar(@cseids)] = $arg;
			}
			elsif( $mode eq "-s" )
			{
				$stations[scalar(@stations)] = $arg;
			}
			elsif( $mode eq "-d" )
			{
				$files[scalar(@files)] = $arg;
			}
			elsif( $mode eq "-h" )
			{
				$heights[scalar(@heights)] = $arg;
			}
			else
			{
				print( STDERR "Parameter $mode is invalid\n" );
				printUsage();
			}
		}	
	}

	$cseids[0] = "ALL" if( @cseids == 0 );
	$stations[0] = "ALL" if( @stations == 0 );
	$params[0] = "ALL" if( @params == 0 );
	$heights[0] = "ALL" if( @heights == 0 );

	return(\@params, \@files, \@cseids, \@stations, \@heights, $in_file, 
               $dir, $keep, $removeplotdata, $verbose, $multi, $color, $project,
               $begindate, $enddate, $legend, $multistn);
}
#*******************************************************************************
# Show user the parameters available for plotting within the user-specified
# data format.

sub showPossibleParams
{
	my $format = shift;
	my $cols = shift;
	my $units = shift;

	print( STDERR "Possible Paramaters for $format: ", "\n" );
	printf( STDERR "  %-15s %-20s %-10s\n",  "param", "units", "column" );	
	#print( STDERR "  ---------- ---------- ----------\n" );
	foreach my $param (sort keys %$cols)
	{
		printf( STDERR "  %-15s %-20s %-10s\n", $param, $units->{$param},
                        $cols->{$param} );
	}
}
#*******************************************************************************
# Determine all Stations, heights, and CSEids available in the input data.

sub getStationsAndHeightsAndCSEids
{
	my $files = shift;
	my $scol = shift;
	my $hcol = shift;
        my $separatorChar = shift;
	my %stns;
	my %heights;
        my %CSEids;

	$scol-- if( defined( $scol ) );
	$hcol-- if( defined( $hcol ) );

	foreach my $file (@$files)
	{
                # if input file is gzipped, unzip it
                my $zipflag;
                if ($file =~ /.gz/)
                    {
                    system "gunzip $file";
                    $file =~ s/.gz//;
                    $zipflag = 1;
                    }
                else
                    {
                    $zipflag = 0;
                    }

		open( IN, "<$file" ) or die( 
                        "Unable to open data file $file: $!\n");

                # This was a while (!eof (IN)) {$line = <IN>; ...}
                # but that syntax was very slow.  while(<IN>) is much faster.
		while( <IN> )
		{
			my @arr = split( /\s+/, $_ );
			$heights{$arr[$hcol]} = 1 if( defined( $hcol ) && 
                                             !exists( $heights{$arr[$hcol]} ) );
                        # Assume that the network/reference site id is in the
                        # column just preceeding the station ID. For qcf/pqc/dqc
			# data, there are only two identifiers: network
                        # and station.  For sfc, flx, twr, and stm there are 
                        # three: CSEid, refSiteId, and station.  CSEid and
                        # network are in the 5th column (4th array subscript)
                        # for all formats.  So if $arr[4] eq $arr[$scol-1] then
                        # we have qcf data. If not, then cat the two 
			# together and used the combined CSEid_refSiteId.
			# For 15min and hrly pqc, $arr[2] eq $arr[$scol-1]
			# For daily dqc, $arr[1] eq $arr[$scol-1]

                        # Note that the character we use to cat the two together
                        # must not occur in the CSEid or refSiteId, since we 
                        # will later split on this char to create subdirs.
                        if( defined ( $scol ))
                        {
                            my $id;
                            if ($arr[$scol-1] eq $arr[4] ||
				$arr[$scol-1] eq $arr[2] ||
				$arr[$scol-1] eq $arr[1])
                      	 	{ $id = $arr[$scol-1]; }
                            else 
                                { 
                                if ($arr[4] !~ /$separatorChar/ &&
                                    $arr[$scol-1] !~ /$separatorChar/)
                                    {
                                    $id = $arr[4].$separatorChar.$arr[$scol-1];
                                    }
                                else
                                    {
                                    print "WARNING: Separator char $separatorChar occurs in data!!! It is hardcoded just below the comment section at the top of this code.  User must modify it to something that does not occur in the Id's!!!\n"; 
                                    exit(1);
                                    }
                                }

                            $CSEids{$id} = 1 if(!exists( $CSEids{$id} ) );
			    $stns{$id}{$arr[$scol]} = 1 
                                if(!exists( $stns{$id}{$arr[$scol]} ) );
                        }
		}

                close ( IN );

               # if the file was gzipped when we found it, re-zip it.
               if ($zipflag) {system "gzip $file"; $file =~ s/$/.gz/;}
	}

	my @aheights = sort keys %heights;
        my @aCSEids = sort keys %CSEids;

	return( \%stns, \@aheights, \@aCSEids );
}
#*******************************************************************************
# Run gnuplot and convert to create the plots.

sub createPlots
{
        my $station = shift;
        my $param = shift;
        my $gnu_in = shift;
        my $ps_file = shift;
        my $gif_file = shift;
        my $verbose = shift;
        my $keep = shift;

        #-------------------------------------------------------
        # Run gnuplot to create .ps file
        #-------------------------------------------------------
        if (-e $ps_file) 
                {
                print( STDERR 
                       "WARNING: $ps_file already exists.  Overwriting...\n");
                }
	print( STDERR "Creating .ps file with gnuplot: $ps_file...\n" )
                if( $verbose );
	my $result = system( "gnuplot.new $gnu_in" );

        #-------------------------------------------------------
        # If .ps file contains data, convert to gif.
        #-------------------------------------------------------
	if( $result != 0 )
		{
		print( STDERR "Unable to create file for: " .
                        "$station $param, deleting $ps_file\n");
		unlink( $ps_file );
		unlink( $gnu_in ) unless( $keep );
		}
		else
		{
                if (-e $gif_file) 
                        {
                        print( STDERR 
                         "WARNING: $gif_file already exists. Overwriting...\n");
                        }

                # I've been getting magic files occasionally, and a plot is not
                # created for unknown reasons.  Add code here to try to catch 
                # the problem.
                # In version 6.3 of this code, error occured intermittently and
                # unpredictably and looked like:
                #    ./magicBAA7raG.z.Z: No such file or directory
                #    ./magicCAA8raG.z.Z: No such file or directory
                #    ./magicEAA.raG.z.Z: No such file or directory
                #    convert: Unable to open file (./magicFAA_raG.z) [No such file or directory].
                #    convert: Missing an image file name.
                #    plots/TXAU/TXAU_Dew_Pt.gif has been created
                # and TXAY_Dew_Pt.gif is NOT actually created, in spite of 
                # message. So...

                # Make sure we have a ps_file before we call convert.
                if (!(-e $ps_file))
                        {
                        print( STDERR
                         "ERROR: What happened to the .ps file: $ps_file?");
                        }

                # Make sure we have assigned a string to $gif_file
                if (!defined($gif_file))
                        {
                        print( STDERR "ERROR: What happened to the name".
                            " of the .gif file: $gif_file?");
                        }

		print( STDERR "Converting to .gif and rotating".
                        " 90 degrees: $gif_file...\n" ) 
                        if( $verbose );	

                # Capture the return value from the system call and make sure
                # it's O.K.
		my $ret_val = system("convert -rotate 90 $ps_file $gif_file");

		if ($ret_val == 0) 
                        {
                        print( STDERR "$gif_file has been created\n\n");
                        # Only delete the .ps and .gnuplot files on a clean
                        # exit.  If there is a problem, keep them around for
                        # debugging purposes.
		        if( !$keep )
		        	{
		        	unlink( $ps_file );
		        	unlink( $gnu_in );
		        	}
                        } 
                else
                        {
                        print( STDERR "convert command returned $ret_val. " .
                        "Unable to create gif image for: $station $param\n");
                        }
                # End of extra code to try to catch bug that is creating magic
                # files.

		}
}
#*******************************************************************************
# Create the plottable data file *.plotdata
sub create_plotdata {
    my $infile = shift;
    my $data_files = shift;
    my $stnstr = shift;
    my $format = shift;

    # Pull only the data for the current station/height from
    # the raw data files.  Save it to the .plotdata file.
    # This was done with an awk command in the original 
    # version of the code but it was *very* slow.  Grep is much faster.
    # -h causes the filename NOT to be prepended to the line
    # -E causes grep to match using full regular expressions
    #system("/usr/xpg4/bin/grep -h -E '$stnstr' $data_files > $infile");

    # Open the .plotdata files we are going to write to.
    open(INFILE,">".$infile) or die "Can't open $infile:$!\n";

    # Loop through each of the raw input data files
    my @datafiles = split(' ',$data_files);
    foreach my $file (@datafiles) {

        # if input file is gzipped, unzip it and set a flag
        # so we know to rezip it when we are done.
        my $zipflag;
        if ($file =~ /.gz/)
            {
            system "gunzip $file";
            $file =~ s/.gz//;
            $zipflag = 1;
            }
        else { $zipflag = 0; }

        # Loop through the data file looking for lines that
        # are for this station and height.
        open (DATAFILE,$file) or die "Can't open $file:$!\n";
        while(<DATAFILE>) {
            if (/$stnstr/) {
		#---------------------------------------------------------------
		# QCF exception:
		# If this is QCF data and the gust is blank, then set it to N 
		# so that plotting columns will not be thrown off.
		#---------------------------------------------------------------
		if ($format eq "qcf" && substr($_,176,1) eq ' ') {
		$_ = substr($_,0,176)."N".substr $_, 177, length($_)-177;
		}

		#---------------------------------------------------------------
		# PQC exception:
		# Precip has all values for a day on one line.  We need to
		# flip this to one obs per line per time stamp.
		# Input looks like:
		# For 15min and hrly:
		# 2004/08/12 00:00:00 LDMSFCMETR DUG               31.47000  -109.60000   0    0.00 0 G ...
		# For Daily:
		# 2003/05 COOP       16615709          30.00000   -90.13333   0    0.00 0 E 13 ...
		# The output to the plotdata file for all three time types looks
		# like:
		# 2003/05/01 13:00:00 COOP       16615709          30.00000   -90.13333   0    0.00 E
		#---------------------------------------------------------------
		if ($format eq "pqc" || $format eq "dqc") {
		    my $metadata_len;	# Length of beginning of line until
					# beginning of first data value.
		    my $val_len;	# Length of parameter value group (inc.
					# flags)
		    if ($format eq "pqc") {$metadata_len = 74; $val_len = 11;}
		    if ($format eq "dqc") {$metadata_len = 62; $val_len = 14;}

		    # Grab the metadata from the line.
		    my $metadata = substr($_,0,$metadata_len);

		    # Location of beginning of the next parameter value group
		    my $val_locn = $metadata_len;

		    # Contents of next parameter value group (inc flags)
		    my $pcpval;

		    # To deal with all data frequencies, count bins of data
		    # and relate that to time later.
		    my $timebin = 1; my $hr;

		    # Parse the next parameter group from the line until we
		    # run out of parameter groups.
		    while (length($pcpval =  substr($_,$val_locn,$val_len))
			==$val_len){

			# 15 min precip and hourly precip
			if ($format eq "pqc") {
			    # Remove the code_flag from the precip group because
			    # we don't need it for plotting and code does not
			    # expect it (since CEOP, sfc don't have it).
			    $pcpval = sprintf(substr($pcpval,0,8).
				substr($pcpval,10,1));

			    # In pcp data, the time reflects the first
			    # bin given on the line.
			    # modify the time in the metadata string to
			    # reflect the bin we are processing.

			    # hrly precip
			    if (length($_) == 362) { 
			        $metadata = sprintf(substr($metadata,0,11).
			        "%02d".substr($metadata,13,61),$timebin-1);

			    # 15 min pcp
                            } else {	
				$hr = int (($timebin-1)/4);
				my $min = (($timebin-1)-($hr*4))*15;
			        $metadata = sprintf(substr($metadata,0,11).
			        "%02d:%02d".substr($metadata,16,61),$hr,$min);
			    }

			    # Print the now-plottable line to the .plotdata file
			    print INFILE $metadata.$pcpval."\n";
			} 

			# Daily precip
			if ($format eq "dqc") {
			    
			    # In pcp data, the time reflects the first
			    # bin given on the line.
			    # modify the time in the metadata string to
			    # reflect the bin we are processing and the hour
			    # that the daily data was collected.
			    $hr = substr($pcpval,12,2);
			    $newmetadata = sprintf(substr($metadata,0,7).
			    "/%02d %02d:00:00".substr($metadata,7,55),$timebin,
			    $hr);

			    # Remove the code_flag from the precip group because
			    # we don't need it for plotting and code does not
			    # expect it (since CEOP, sfc don't have it).
			    $pcpval = sprintf(substr($pcpval,0,8).
				substr($pcpval,10,1));

			    # Print the now-plottable line to the .plotdata file
			    print INFILE $newmetadata.$pcpval."\n";
			} 

			# Move to the next parameter value group on the line
			# and increment timebin to show we are working with
			# the next time bin.
			$val_locn += $val_len + 1; $timebin++;
		    }
		} else {
		    # Formats other than precip are good as they are, so go
		    # ahead and print the data line to the .plotdata file.
		    print INFILE;
		}
	    }
        }
        close(DATAFILE);

        # if file was gzipped when we found it, re-zip it.
        if ($zipflag) {system "gzip $file"; $file =~ s/$/.gz/;}

    }
    close (INFILE);
}
#*******************************************************************************
sub escape_chars_colon {
    my $id = shift;
    $id_escaped = $id;

    # Sometimes we include a : in the network name, i.e. for MADIS data we 
    # prepend ms: to the network (ms:APRSWXN).  This : messes up the sed 
    # command, so we have escaped it with a \.
    $id_escaped =~ s/:/\\:/g;

    return ($id_escaped);
}
#*******************************************************************************
sub escape_chars {
    my $id = shift;
    $id_escaped = $id;

    # Sometimes there is a / in the network name, i.e. NCEP/EMC .
    # This / messes up the sed command, so we have replaced it with an _ .
    # Note that we do not want to replace a ./ at the beginning of the
    # filename with ._ so we check for that here.
    if ($id_escaped !~ /^\.\//) { $id_escaped =~ s/\//_/g; }

    return ($id_escaped);
}
