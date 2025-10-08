#!/bin/perl 

################################################################################
# Version 2
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
# Updated 1/12/2004 by Janine Goldstein
# so that the AWK command is called once for each station/height combination 
# rather than once for each station/ht/parameter combination.  This increased 
# processing speed significantly.
#
# By executing the script with no arguments you will be given an extensive usage
#  statement.
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
	print( "Format does not exist!  Possible formats are: \n" );
	foreach my $fmt( sort keys %fmts_pcolumns )
	{
		print( "  " . $fmt . "\n" );
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
 my $out_dir, my $keep, my $verbose) 
      = getArguments();

my @params = @$para_ref;
my @files = @$files_ref;
my @stations = @$stations_ref;
my @heights = @$heights_ref;

@params = keys( %pcolumns ) if( $params[0] eq "ALL" );

#-------------------------------------------------------------------------------
# Read in the parameters from a file if the -f option was used
#-------------------------------------------------------------------------------
if( defined( $param_file) )
{
	my $size = @params;

	open( PARAM, "<$param_file" ) or die
                ( "Unable to open specified parameter file: $param_file\n" );

	while( !eof( PARAM ) )
	{
		my $p = <PARAM>;
		chop( $p );
		$params[$size++] = $p; 
	}	

        close ( PARAM );
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
(my $all_stns_ref, my $all_heights_ref) = 
                getStationsAndHeights( \@files, $stn_col, $height_col );

my @all_stns = @$all_stns_ref;
my @all_heights = @$all_heights_ref;

@stations = @all_stns if( $stations[0] eq "ALL" );
@heights = @all_heights if( $heights[0] eq "ALL" );

#-------------------------------------------------------------------------------
# Show the user what is being plotted
#-------------------------------------------------------------------------------
print( "Stations to plot: \n" );
foreach my $stn (@stations)
{
	print( "  ", $stn, "\n" );
}
print( "Heights to plot: \n" );
foreach my $ht (@heights)
{
	print( "  ", $ht, "\n" );
}
print( "Parameters to plot: \n" );
foreach my $prm (@params)
{
	print( "  ", $prm, "\n" );
}
	
$heights[0] = undef() if( @heights == 0 || !defined( $height_col ) );

my $data_files = "";

#*******************************************************************************
# Now create the plots
#*******************************************************************************

#-------------------------------------------------------------------------------
# Append all the data files to plot
#-------------------------------------------------------------------------------
foreach my $file (@files)
{
	$data_files = $data_files . " " . $file; 
}

my $hcount = 0;
#-------------------------------------------------------------------------------
# Loop through stations
#-------------------------------------------------------------------------------
foreach my $station (@stations)
{
        #-----------------------------------------------------------------------
        # Loop through heights
        #-----------------------------------------------------------------------
	foreach my $height( @heights )
	{
		my $filename = sprintf( 
                   "${out_dir}/${station}_h%2.2d", $hcount );
		my $awk_file = $filename . ".awk";
		my $infile = $filename . ".plotdata";

                #---------------------------------------------------------------
                # Create a temporary files containing only the station
                # and height we are trying to plot.
                #---------------------------------------------------------------
		print(
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

                system ("awk -f $awk_file $data_files > $infile");

                #---------------------------------------------------------------
                # Loop through parameters
                #---------------------------------------------------------------
		foreach my $param (keys %units) 
		{
			my $file_name = sprintf( 
                            "${out_dir}/${station}_${param}_h%2.2d", $hcount );
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
			printf( "Creating gnuplot input file: $gnu_in...\n" )
                            if( $verbose );
			system( "sed  \"s:XPARAMETERX:$param:g; s:XSTATION:$station:g; s:XHEIGHTX:$theight:g; s:XUNITSX:$units{$param}:g; s:XOUTFILEX:$ps_file:g; s:XINFILEX:$infile:g; s:XCOLUMNX:$column{$param}:g;\" ceop_template > \"$gnu_in\"" );

                        #-------------------------------------------------------
                        # Run gnuplot to create .ps file
                        #-------------------------------------------------------
			print( "Creating .ps file with gnuplot: $ps_file...\n" )
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
				unlink( $awk_file ) unless( $keep );
			}
			else
			{
				print( "Converting to .gif and rotating 90 " .
                                       "degrees: $gif_file...\n" ) 
                                       if( $verbose );	
				system("convert -rotate 90 $ps_file $gif_file");

				print( "$gif_file has been created\n\n" );

				if( !$keep )
				{
					unlink( $awk_file );
					unlink( $ps_file );
					unlink( $gnu_in );
				}
			}
		} #foreach $param

		$hcount++;
	} #foreach $height
} #foreach $station

exit 0;

################################################################################
# Subroutines
################################################################################

#*******************************************************************************
sub printUsage
{
    print( STDERR "Usage:\n" );
    print( STDERR "  $0 format -h -p <parameters> -f <params file>".
                  " -d <0qc files> -s <stations> -o <out dir> -k\n\n" ); 

    print( STDERR "  format - the format of the data to plot: qcf, sfc, flx, " .
                  "stm, twr\n\n" );

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
         "      file for each station and height.  The files are placed in \n",
         "      the output directory with the name <station>_<height>.\n",
         "      plotdata. \n" );
#         "      If no stations\n",
#         "      are specified the script looks for a <station>_<height>.plotdata \n",
#         "      file in the current directory for each station specified.\n",
#         "      Therefore it is possible to run the script with the -d\n",
#         "      option the first time and omit it each subsequent time for\n",
#         "      the same set of stations. \n" );
    print( STDERR "  -o  The output directory to place all created files.\n",
         "      This includes the gnuplot input files, the .ps files\n",
         "      generated by gnuplot and the final .gif images. Defaults \n",
         "      to the current directory.\n" );
    print( STDERR "  -k  If this option is included the gnuplot input files\n",
         "      and the .ps files will NOT be removed, otherwise they will\n",
         "      be. The .dat files created for each station will never be\n",
         "      removed \n" );
    print( STDERR "  -v  Verbose mode, print status messages as the plots\n",
         "      are created.   \n");
    print( STDERR "\nWARNING: You will need to modify ceop_template to set\n",
         "      the desired title, xrange, formats, missing value, etc.\n");

    print( STDERR "\nEXAMPLES:\n" );
    print( STDERR "   $0 qcf -d data/*.0qcf -o out_dir\n" );
    print( STDERR "   $0 twr -p ALL -s Station1 Station2 -o out_dir\n" );
    print( STDERR "   $0 sfc -f params.in -s Station1 -o ../Station1 -d data/*.0qcf\n" );
    print( STDERR "   $0 sfc -help\n" );

    exit;
}
#*******************************************************************************


sub getArguments()
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
		elsif( $arg eq "-p" || $arg eq "-d" || $arg eq "-f" || 
                       $arg eq "-s" || $arg eq "-o" || $arg eq "-h" )
		{ $mode = $arg; }
		else
		{
			if( $mode eq "-f" )
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
				print( "Parameter $mode is invalid\n" );
				printUsage();
			}
		}	
	}

	$stations[0] = "ALL" if( @stations == 0 );
	$params[0] = "ALL" if( @params == 0 );
	$heights[0] = "ALL" if( @heights == 0 );

	if( @files == 0 )
	{ die( "The data files must be specified (-d parameter).\n" ); }
	return(\@params, \@files, \@stations, \@heights, $in_file, $dir, $keep, 
               $verbose);
}
#*******************************************************************************

sub showPossibleParams
{
	my $format = shift;
	my $cols = shift;
	my $units = shift;

	print( "Possible Paramaters for $format: ", "\n" );
	printf( "  %-15s %-20s %-10s\n",  "param", "units", "column" );	
	#print( "  ---------- ---------- ----------\n" );
	foreach my $param (sort keys %$cols)
	{
		printf( "  %-15s %-20s %-10s\n", $param, $units->{$param},
                        $cols->{$param} );
	}
}
#*******************************************************************************

sub getStationsAndHeights
{
	my $files = shift;
	my $scol = shift;
	my $hcol = shift;
	my %stns;
	my %heights;

	$scol-- if( defined( $scol ) );
	$hcol-- if( defined( $hcol ) );

	foreach my $file (@$files)
	{
		open( IN, "<$file" ) || die
                    ( "Unable to open data file $file.\n" );

		while( !eof( IN ) )
		{
			my $line = <IN>;
			my @arr = split( /\s+/, $line );

			$stns{$arr[$scol]} = 1 if( defined( $scol ) && 
                                                !exists( $stns{$arr[$scol]} ) );
			$heights{$arr[$hcol]} = 1 if( defined( $hcol ) && 
                                             !exists( $heights{$arr[$hcol]} ) );
		}

                close ( IN );
	}

	my @astns = sort keys %stns;
	my @aheights = sort keys %heights;

	return( \@astns, \@aheights );
}
#*******************************************************************************
