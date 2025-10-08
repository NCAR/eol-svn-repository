#!/bin/perl
################################################################################
# Version 5.5
################################################################################
# plot_ceop.pl: Uses gnuplot and convert to plot specified parameters and 
# stations from data in the four CEOP formats.  Uses the ceop_template to make 
# calls to gnuplot.  This can be modified as needed.  QCF data can also be 
# plotted with this script.
#
# WARNING: You will need to modify the template to set desired title, xrange,
# formats, missing value, etc.
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
# CSEID and begin and end dates were constants in ceop_template so it had to
# be modified each time the code was run.  Now they are in this script and
# passed to ceop_template. CSEID is picked from data, and begin and end date
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
# "plot_ceop.pl" for details. Also modified to automatically create subdirs for
# station, multi-height, and color.
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
# By executing the script with no arguments you will be given an extensive usage
# statement.
#
# usage:
# plot_ceop.pl <format> -d <data files> -p <parameters> -h <heights> -s <stations>
#   format must be specified
#   -d must be specified
#   -p can be ALL, if not specified ALL
#   -s can be ALL, if not specified ALL
#   -h can be ALL, if not specified ALL
#  or
#  plot_ceop.pl <format> -help 
#   lists the possible parameters
################################################################################
use File::Basename;

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
my %qcf_pcolumns = (  Elev => 10, STN_Press => 11, SL_Press => 13,  
             Comp_SL_Press => 15, Air_Temp => 17, Dew_Pt => 19, Wind_Spd => 21, 
             Wind_Dir => 23, Precip => 25, Squall_Gust => 28, Present_Wx => 30, 
             Visibility => 32, Ceiling_Ht1 => 34, Ceiling_Ht2 => 39, 
             Ceiling_Ht3 => 44 );

my %qcf_punits = ( Elev => "meters", STN_Press => "mb", SL_Press => "mb", 
             Comp_SL_Press => "mb", Air_Temp => "Celsius", Dew_Pt => "Celsius", 
             Wind_Spd => "m/s", Wind_Dir => "deg", Precip => "mm", 
             SG_Flag => "char",  Squall => "m/s", Present_Wx => "code_val",
             Visibility => "m", Ceiling_Ht1 => "Hundreds of ft", 
             Ceiling_Ht2 => "Hundreds of ft", Ceiling_Ht3 => "Hundreds of ft" );

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
 

#-------------------------------------------------------------------------------
# A hash that maps each format to the column hash
#-------------------------------------------------------------------------------
my %fmts_pcolumns = ( "qcf" => \%qcf_pcolumns,
	                    "sfc" => \%sfc_pcolumns,
	                    "flx" => \%flx_pcolumns, 
	                    "stm" => \%stm_pcolumns,
	                    "twr" => \%twr_pcolumns 
                    );

#-------------------------------------------------------------------------------
# A hash that maps each format to the unit hash
#-------------------------------------------------------------------------------
my %fmts_punits = ( "qcf" => \%qcf_punits,
                    "sfc" => \%sfc_punits, 
                    "flx" => \%flx_punits, 
                    "stm" => \%stm_punits,
                    "twr" => \%twr_punits,
                  );

#-------------------------------------------------------------------------------
# The height column of each format
#-------------------------------------------------------------------------------
my %fmts_height_col = ( "qcf" => undef(),
			"sfc" => undef(),
			"flx" => 11,
			"stm" => 11,
			"twr" => 11
			);

#-------------------------------------------------------------------------------
# The station column of each format
#-------------------------------------------------------------------------------
my %fmts_stn_col = ( "qcf" => 6,
			 "sfc" => 7,
			 "flx" => 7,
			 "stm" => 7,
			 "twr" => 7,
			);

#*******************************************************************************
# Print the usage if no arguments given					
#*******************************************************************************
if( @ARGV == 0 ) 
{
	printUsage();
	exit;
}

#*******************************************************************************
# Read in all the command line arguments
#*******************************************************************************

#-------------------------------------------------------------------------------
# Get the format
#-------------------------------------------------------------------------------
my $format = shift( @ARGV );

#-------------------------------------------------------------------------------
# Check for existence
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
# Get the columns, units, station_col number, and height_col number
#-------------------------------------------------------------------------------
my %pcolumns = %{$fmts_pcolumns{$format}};
my %punits = %{$fmts_punits{$format}};
my $stn_col = $fmts_stn_col{$format};
my $height_col = $fmts_height_col{$format};

#-------------------------------------------------------------------------------
# Print the available parameters if -help option used
#-------------------------------------------------------------------------------
if( $ARGV[0] eq "-help" )
{
	showPossibleParams( $format, \%pcolumns, \%punits );
	exit 0;	
}


#-------------------------------------------------------------------------------
# Get the arguments
#-------------------------------------------------------------------------------
(my $para_ref, my $files_ref, my $stations_ref, my $heights_ref, my $param_file,
 my $out_dir, my $keep, my $verbose, my $multi, my $color, my $begindate, 
 my $enddate) 
      = getArguments();

my @params = @$para_ref;
my @files = @$files_ref;
my @stations = @$stations_ref;
my @heights = @$heights_ref;

#-------------------------------------------------------------------------------
# If no begin and end times are specified, warn user and exit.
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
# If $out_dir does not exist, create it.
#-------------------------------------------------------------------------------if ($out_dir ne "." && !(-e $out_dir)) 
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
# files in the $out_dir subdir with format $format.
#-------------------------------------------------------------------------------
if( @files == 0 )
	{
        opendir (DIR, "$out_dir") or die "Can\'t open dir $out_dir:$!\n";

        if( $stations[0] =~ /ALL/)
                {
                @files = grep /${format}.plotdata$/, readdir DIR;
                }
        else
                {
                @dirfiles = readdir DIR;
                foreach my $station (@stations)
                        {
                        @newfiles = grep /$station\_.*${format}.plotdata$/, 
                                    @dirfiles;
                        push @files, @newfiles;
                        }
                }
        closedir (DIR);

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
        print( STDERR "There are not files of format $format in $out_dir.\n".
              "        Please specify an input file on the command line via\n".
              "        the -d option.\n");
        exit(1);
        }

@params = keys( %pcolumns ) if( $params[0] eq "ALL" );

#-------------------------------------------------------------------------------
# Read in the parameters from a file if the -f option was used
#-------------------------------------------------------------------------------
if( defined( $param_file) )
{
	my $size = @params;

	open( PARAM, "<$param_file" ) or die
                "Unable to open specified parameter file: $param_file\n";

	while( !eof( PARAM ) )
	{
		my $p = <PARAM>;
		chop( $p );
		$params[$size++] = $p; 
	}	
        close( PARAM );
}

my %units = ();
my %column = ();

#-------------------------------------------------------------------------------
# Get the list of paramaters to plot
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
# Get the stations and heights from the data files
#-------------------------------------------------------------------------------
(my $all_stns_ref, my $all_heights_ref, my $all_CSEids_ref) = 
                getStationsAndHeightsAndCSEids( \@files, $stn_col, $height_col);

my @all_stns = @$all_stns_ref;
my @all_heights = @$all_heights_ref;
my @all_CSEids = @$all_CSEids_ref;

@stations = @all_stns if( $stations[0] eq "ALL" );
@heights = @all_heights if( $heights[0] eq "ALL" );

my $netType;
if ($format eq "qcf") {$netType = "Network";} else {$netType = "CSEid";}

if (scalar(@all_CSEids) != 1) {
        print( STDERR 
               "More than one $netType contained within raw data:\n");
        foreach my $id (@all_CSEids) {print STDERR "\t$id\n";}
        print( STDERR "Code currently only works on one $netType at\n".
                      "a time.  Try: cat <file> | grep <CSEid> > tempfile\n".
                      "$0 ... -d tempfile");
        exit(1);
} else {
        $cseid = $all_CSEids[0];
        print( STDERR "Making plots for $netType $cseid\n");
}


#-------------------------------------------------------------------------------
# Show the user what is being plotted
# and create subdirs by station with $out_dir
#-------------------------------------------------------------------------------
print( STDERR "Stations to plot: \n" );
foreach my $stn (@stations)
{
	print( STDERR "  ", $stn, "\n" );
        if (!(-e $out_dir."/".$stn))
                {
                system ("mkdir $out_dir/$stn");
                }
        if ($color && !(-e $out_dir."/".$stn."/color"))
                {
                system ("mkdir $out_dir/$stn/color");
                }
        if ($multi && !(-e $out_dir."/".$stn."/multiHt"))
                {
                system ("mkdir $out_dir/$stn/multiHt");
                }
}
print( STDERR "Heights to plot: \n" );
foreach my $ht (@heights)
{
	print( STDERR "  ", $ht, "\n" );
}
print( STDERR "Parameters to plot: \n" );
foreach my $prm (@params)
{
	print( STDERR "  ", $prm, "\n" );
}
if ($multi) 
{
        print( STDERR "All heights for a single parameter will be plotted on\n",
               "       a single graph.\n");
}
if ($color) 
{
        print( STDERR "Color will be used to denote flags associated with\n",
               "       each value on the plot.\n");
}
	
$heights[0] = undef() if( @heights == 0 || !defined( $height_col ) );

my $data_files = "";

#*******************************************************************************
# Now create the plots
#*******************************************************************************

#-------------------------------------------------------------------------------
# Append all the data files to plot
#-------------------------------------------------------------------------------
my $mtime; # Days since last modification of most recently modified data file.
foreach my $file (@files)
{
	$data_files = $data_files . " " . $file; 
        if ($mtime > (-M $file)) {$mtime = (-M $file);}
}

my $hcount = 0;
my %plot_string;
#-------------------------------------------------------------------------------
# Assume ceop_template is in the same directory as this code.  If this dir is 
# remote, we need to append the path to ceop_template for use by gnuplot.
#-------------------------------------------------------------------------------
($name, $path, $suffix) = fileparse($0);
my $template = $path."ceop_template";

#-------------------------------------------------------------------------------
# Loop through stations
#-------------------------------------------------------------------------------
foreach my $station (@stations)
{
        if ($multi) {%plot_string = ();}
        #-----------------------------------------------------------------------
        # Loop through heights
        #-----------------------------------------------------------------------
	foreach my $height( @heights )
	{
		my $filename = sprintf( 
                   "${out_dir}/${station}_h%2.2d", $hcount );
		my $awk_file = $filename . ".awk";
		my $infile = $filename . ".${format}.plotdata";

                #---------------------------------------------------------------
                # Create a temporary files containing only the station
                # and height we are trying to plot.
                #---------------------------------------------------------------
		print( STDERR
                     "Creating the temporary awk program: $awk_file...\n"
                     ) if( $verbose );
		open( AWK, ">$awk_file" );
		if( defined( $height ) )
		{
			print( AWK "{ if( \$$stn_col==\"$station\" && \$$height_col==$height) printf( \"%s\\n\", \$0 ) }" );
		}	
		else	
		{
			print( AWK "{ if( \$$stn_col==\"$station\" ) printf( \"%s\\n\", \$0 ) }" );
		}	
		close( AWK );

                if (!(-e $infile))
                {
                        system ("awk -f $awk_file $data_files > $infile");
                }
                elsif ($mtime < (-M $infile))
                {
                        print( STDERR "WARNING: plottable data file $infile",
                              " modified more recently than input data. ",
                              " Recreating $infile.\n");
                        system ("awk -f $awk_file $data_files > $infile");
                }
                else
                {
                        print( STDERR "WARNING: plottable data file $infile",
                              " already exists. It will not be recreated.\n");
                }

		if( !$keep )
		{
			unlink( $awk_file );
                }

                #---------------------------------------------------------------
                # Loop through parameters
                #---------------------------------------------------------------
		foreach my $param (keys %units) 
		{
			my $file_name = "${out_dir}/${station}";
                        if ($color) {$file_name .= "/color";}
                        $file_name .= sprintf("/${station}_${param}_h%2.2d",
                                $hcount );
			my $gnu_in = $file_name . ".gnuplot";
			my $ps_file= $file_name . ".ps";
			my $gif_file= $file_name . ".gif";

                        #-------------------------------------------------------
                        # Height string for title of plot
                        #-------------------------------------------------------
			my $theight = "";
			$theight = "Height $height" if( defined( $height ) );
	
                        #-------------------------------------------------------
                        # Create gnuplot input file
                        #-------------------------------------------------------
			printf( STDERR 
                                   "Creating gnuplot input file: $gnu_in...\n" )
                            if( $verbose );

                        #-------------------------------------------------------
			# Plots can be made with either a single height on each
                        # plot, or all heights for a given parameter on the same
                        # plot. Multi height plots use a different color for 
                        # each height.  Colors are determined by the gnuplot 
                        # script and may vary plot to plot, i.e. there is no
                        # set color for a given height.  Single height plots use
                        # color to indicate the flag associated with each data
                        # point.  Type "plot_ceop.pl" for more 
                        # information.
                        #-------------------------------------------------------

                        #-------------------------------------------------------
                        # For multi-height plots, generate the plot command one
                        # height at a time.
                        #-------------------------------------------------------
                        if ($multi)
                        {
                                my $plotline = " \\\\\\\"${infile}\\\\\\\" using 1\\\:$column{$param} with lines";
                        	$plot_string{$param}{$height} = $plotline;
                        }
                        #-------------------------------------------------------
                        # For single height plots, generate the plot command one
                        # data flag at a time.
                        #-------------------------------------------------------
                        if ($color) 
                        {
                                my $flag = $column{$param} + 1;
                                my $plotline = "";
                                #-----------------------------------------------
                                # Check for existence of each flag in the input
                                # file.
                                #-----------------------------------------------
		                open( INFILE, "<$infile" ) or die
                                      "Unable to open data file $infile.\n";
                                my %found = ();
                                while (!eof( INFILE ) && ($_ = <INFILE>) && 
                                   (($format eq "qcf" && keys(%found) != 10 ) ||
                                    ($format ne "qcf" && keys(%found) != 7) )) {
                                      # Pull the flag out of the line so it can
                                      # be compared to possibel flag types.
			              my @arr = split( /\s+/, $_ );
                                      $_ = $arr[$column{$param}];
                                      chomp; if (/\r$/) {chop;}
                                      if (/G($)/) {$found{G} = 1;}
                                      if (/U($)/) {$found{U} = 1;}
                                      if (/B($)/) {$found{B} = 1;}
                                      if (/C($)/) {$found{C} = 1;}
                                      if (/I($)/) {$found{I} = 1;}
                                      if (/M($)/) {$found{M} = 1;}
                                      if (/D($)/) {$found{D} = 1;}
                                      if ($format eq "qcf") {
                                            if (/ T($)/) {$found{T} = 1;}
                                            if (/ E($)/) {$found{E} = 1;}
                                            if (/ N($)/) {$found{N} = 1;}
                                      }
                                }
                                #-----------------------------------------------
                                # if the only flag found was 'M', then the data 
                                # were all missing, so don't generate a plot.
                                #-----------------------------------------------
                                if (keys(%found) == 1)
                                      {
                                      my @key = keys %found;
                                      if (($key[0] eq 'M') ||
                                          (($format eq "qcf")&&
                                          (($key[0] eq 'N')|| ($key[0] eq 'I')))
                                         )
                                              {
			                      printf( STDERR 
                                           "Found only missing data for param".
                                           " $param. $gif_file not created.\n");
                                              next;
                                              }
                                      }
                                #-----------------------------------------------
                                # Generate plot commands for flags that exist.
                                #-----------------------------------------------
                                if ($found{U}) {$plotline .= 
"\\\\\\\"< tr 'U' '1' < ${infile}\\\\\\\" using 1\\\:(\\\$$column{$param}*(\\\$$flag == 1 )) with lines 2,";}
                                if ($found{G}) {$plotline .= 
"\\\\\\\"< tr 'G' '1' < ${infile} \\\\\\\" using 1\\\:(\\\$$column{$param}*(\\\$$flag == 1 )) with lines 1,";}
                                if ($found{D}) {$plotline .= 
"\\\\\\\"< tr 'D' '1' < ${infile} \\\\\\\" using 1\\\:(\\\$$column{$param}*(\\\$$flag == 1 )) with points 8 3,";}
                                if ($found{B}) {$plotline .= 
"\\\\\\\"< tr 'B' '1' < ${infile} \\\\\\\" using 1\\\:(\\\$$column{$param}*(\\\$$flag == 1 )) with points 3 3,";}
                                if ($found{C}) {$plotline .= 
"\\\\\\\"< tr 'C' '1' < ${infile} \\\\\\\" using 1\\\:(\\\$$column{$param}*(\\\$$flag == 1 )) with points 4 3,";}
                                if ((($format eq "qcf") && $found{E}) || 
                                    (($format ne "qcf") && $found{I})) {
                                      $plotline .= 
"\\\\\\\"< tr 'I' '1' < ${infile} \\\\\\\" using 1\\\:(\\\$$column{$param}*(\\\$$flag == 1 )) with points 5 3,";}
                                if ((($format eq "qcf") && 
                                     ($found{N} || $found{I}))
                                     || $found{M}) {
                                      $plotline .= 
"\\\\\\\"< tr 'M' '1' < ${infile} \\\\\\\" using 1\\\:(\\\$$column{$param}*(\\\$$flag == 1 )) with points 7 3,";}

                                my $plotspec = "plot $plotline";
                                $plotspec =~ s/,$//;
                                my $color= "color"; 

			        system( "sed  \"s:XPARAMETERX:$param:g; s:XSTATION:$station:g; s:XHEIGHTX:$theight:g; s:XUNITSX:$units{$param}:g; s:XOUTFILEX:$ps_file:g; s:XPLOTSPECX:$plotspec:g; s:XCOLORX:$color:g; s:XCSEIDX:$cseid:g; s:XBEGINDATEX:$begindate:g; s:XENDDATEX:$enddate:g; \" $template > \"$gnu_in\"" );


                        	createPlots( $station, $param, 
					$gnu_in, $ps_file, $gif_file, $verbose, 
					$keep);
                        }
                        if (!$multi && !$color)
                        {
                                my $plotline = " \\\\\\\"${infile}\\\\\\\" using 1\\\:$column{$param} with lines";
                                my $plotspec = "plot $plotline";
                                my $color= "monochrome";

                                system( "sed  \"s:XPARAMETERX:$param:g; s:XSTATION:$station:g; s:XHEIGHTX:$theight:g; s:XUNITSX:$units{$param}:g; s:XOUTFILEX:$ps_file:g; s:XPLOTSPECX:$plotspec:g; s:XCOLORX:$color:g; s:XCSEIDX:$cseid:g; s:XBEGINDATEX:$begindate:g; s:XENDDATEX:$enddate:g; \" $template > \"$gnu_in\"" );

                                createPlots( $station, $param,
                                        $gnu_in, $ps_file, $gif_file, $verbose,
                                        $keep);
                        }

		} #foreach $param

		$hcount++;
	} #foreach $height

        if ($multi) 
        {

		foreach my $param (keys %plot_string) 
                {
		        my $file_name = sprintf( 
                                "${out_dir}/${station}/multiHt/${station}_${param}_multiHt");
        		my $gnu_in = $file_name . ".gnuplot";
        		my $ps_file= $file_name . ".ps";
        		my $gif_file= $file_name . ".gif";
                        my $plotspec = "plot";
		        foreach my $height (keys %{$plot_string{$param}}) 
                        {
                                $plotspec = 
                                    $plotspec.$plot_string{$param}{$height}.",";
                        }
                        $plotspec =~ s/,$/\;/;
                        my $color = "color";

                        system( "sed  \"s:XPARAMETERX:$param:g; s:XSTATION:$station:g; s:XHEIGHTX:All Heights:g; s:XUNITSX:$units{$param}:g; s:XOUTFILEX:$ps_file:g; s:XPLOTSPECX:$plotspec:g; s:XCOLORX:$color:g; s:XCSEIDX:$cseid:g; s:XBEGINDATEX:$begindate:g; s:XENDDATEX:$enddate:g;\" $template > \"$gnu_in\"" );

                        createPlots( $station, $param, $gnu_in, 
                                     $ps_file, $gif_file, $verbose, $keep);
                }
        }
} #foreach $station

exit 0;

################################################################################
# Subroutines
################################################################################

#*******************************************************************************
sub printUsage
{
    print( STDERR "Usage:\n" );
    print( STDERR "  $0 format -h -p <parameters> -f <params file> -t <times>".
                  " -d <0qc files> -s <stations> -o <out dir> -k -m -v\n\n" ); 

    print( STDERR "  format - the format of the data to plot: qcf, sfc, flx, " .
                  "stm, twr\n\n" );
    print( STDERR "  -t Begin and end times to use as the plot range for time.".
                  " Times should be of the format yyyy/mm/dd, and both a " .
                  " beginning and ending time must be specified." );
    print( STDERR "OPTIONS:\n" );
    print( STDERR "  -help displays the possible parameters for the specified ".
                  "format. \n      This option must be placed immediatly after".
                  " the format.  \n" );
    print( STDERR "  NOTE: the following options can be given in any order!\n");
    print( STDERR "  -p  List of paramaters to plot. Each image will contain\n",
         "      a single parameter. Use the -h option above to view possible\n",
         "      params. If value 'ALL' is given the script will attempt to\n",
         "      plot all of the parameters, if a parameter contains all \n",
         "      -999.99 values gnuplot will fail and the .ps file will be \n",
         "      deleted. Default is 'ALL'\n" );
    print( STDERR "  -f  File containing a list of parameters to plot.  This\n",
         "      can be used instead of or in addition to the -p option.\n",  
         "      Each line of the file should contain a single parameter.\n" );
    print( STDERR "  -s  The stations to plot. Default is 'ALL'\n" );  
    print( STDERR "  -h  The heights to plot. Default is 'ALL'\n" );  
    print( STDERR "  -d  The data files in qcf format to extract data from.\n", 
         "      A simple awk is performed on each file to create a single \n", 
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
         "      be. The .dat files created for each station will never be\n",
         "      removed \n" );
    print( STDERR "  -m  If this option is included, the script will plot \n",
         "      multiple heights/depths of a single parameter for the same\n",
         "      station on the same plot. If it is omitted, each parameter/\n",
         "      combination will be on it's own plot.\n");
    print( STDERR "  -c  If this option is included, the script will plot \n",
         "      single height plots with a different color to indicate the \n",
         "      flag associated with each data point using the following \n",
         "      color scheme:\n",
         "\n",
         "\t color  		CEOP Data flag		QCF flag\n",
         "\t -----  		--------------		--------\n",
         "\t red (3)		B (Bad)			B (Bad)\n",
         "\t orange (8)		D (questionable		D (questionable\n",
         "\t 			   /dubious)		   /dubious)\n",
         "\t green (1)		G (good)		G (good)\n",
         "\t blue (2)		U (unchecked)		U (unchecked)\n",
         "\t pink (4)		C **			C **\n",
         "\t lt blue (5)\t	I (Interpolated		E (Estimated)\n",
         "\t 			   /estimated)\n",
         "\t black (7)		M (missing/could 	M/N/I (Missing/\n",
         "\t 			  not be computed)	  Not available/&&\n",
         "\t yellow (6)					T (trace precip)\n",
         "\t color not assigned				X (Glitch)\n",
         "\n",
         "      (** = Reported values exceeds output format field size\n",
         "      or was negative precip,\n",
         "      && = Insufficient input to determine a computed value)\n");
    print( STDERR "  -v  Verbose mode, print status messages as the plots\n",
         "      are created.   \n");
    print( STDERR "\nEXAMPLES:\n" );
    print( STDERR "  $0 qcf -t 2002/10/01 2003/03/31 -d data/*.0qcf -o out_dir\n" );
    print( STDERR "  $0 qcf -t 2002/10/01 2003/03/31 -d data/*.0qcf -o out_dir -c\n" );
    print( STDERR "  $0 twr -t 2002/10/01 2003/03/31 -p ALL -s Station1 Station2 -o out_dir\n" );
    print( STDERR "  $0 sfc -t 2002/10/01 2003/03/31 -f params.in -s Station1 -o ../Station1 -d data/*.0qcf\n" );
    print( STDERR "  $0 sfc -t 2002/10/01 2003/03/31 -o out_dir -s Station1 -m\n" );
    print( STDERR "  $0 sfc -help\n" );
    print( STDERR "  cat data.gz | gunzip > tempfile; $0 sfc -t 2002/10/01 2003/03/31 -o out_dir -d tempfile\n");

    exit;
}
#*******************************************************************************


sub getArguments
{
	my @params; 
	my @files;
	my @stations;
	my @heights;
	my $in_file;
	my $dir = ".";
	my $mode = "none";
	my $keep;
	my $verbose;
        my $multi = 0;
        my $color = 0;
        my $begindate = "";
        my $enddate = "";

	foreach $arg (@ARGV)
	{
		if( $arg eq "-k" )
		{ 
			$keep = 1; 
			$mode= "none"; 
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
		elsif( $arg eq "-c" )
		{ 
			$color = 1; 
			$mode = "none";
		}
		elsif( $arg eq "-p" || $arg eq "-d" || $arg eq "-f" || 
                       $arg eq "-s" || $arg eq "-o" || $arg eq "-h" ||
                       $arg eq "-t" )
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

	$stations[0] = "ALL" if( @stations == 0 );
	$params[0] = "ALL" if( @params == 0 );
	$heights[0] = "ALL" if( @heights == 0 );

	return(\@params, \@files, \@stations, \@heights, $in_file, $dir, $keep, 
               $verbose, $multi, $color, $begindate, $enddate);
}
#*******************************************************************************

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

sub getStationsAndHeightsAndCSEids
{
	my $files = shift;
	my $scol = shift;
	my $hcol = shift;
	my %stns;
	my %heights;
        my %CSEids;

	$scol-- if( defined( $scol ) );
	$hcol-- if( defined( $hcol ) );

	foreach my $file (@$files)
	{
		open( IN, "<$file" ) or die( 
                        "Unable to open data file $file: $!\n");

		while( !eof( IN ) )
		{
			my $line = <IN>;
			my @arr = split( /\s+/, $line );

			$stns{$arr[$scol]} = 1 if( defined( $scol ) && 
                                                !exists( $stns{$arr[$scol]} ) );
			$heights{$arr[$hcol]} = 1 if( defined( $hcol ) && 
                                             !exists( $heights{$arr[$hcol]} ) );
			$CSEids{$arr[4]} = 1 if( !exists( $CSEids{$arr[4]} ) );
		}

                close ( IN );
	}

	my @astns = sort keys %stns;
	my @aheights = sort keys %heights;
        my @aCSEids = sort keys %CSEids;

	return( \@astns, \@aheights, \@aCSEids );
}
#*******************************************************************************
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
	my $result = system( "gnuplot $gnu_in" );

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
                if (-e $ps_file) 
                        {
                        print( STDERR 
                         "WARNING: $gif_file already exists. Overwriting...\n");
                        }
		print( STDERR "Converting to .gif and rotating".
                        " 90 degrees: $gif_file...\n" ) 
                        if( $verbose );	
		system("convert -rotate 90 $ps_file $gif_file");

		print( STDERR "$gif_file has been created\n\n");

		if( !$keep )
			{
			unlink( $ps_file );
			unlink( $gnu_in );
			}
		}
}
