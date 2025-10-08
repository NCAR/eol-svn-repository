#!/usr/bin/perl

use strict;
use Time::Local;
use Time::localtime;
use DirHandle;
use File::Basename;
use Data::Parameter;
use Data::DataPoint;
use Data::File;
use Data::TWRFile;
use Data::FileList;
use DQR::Flag;
use DQR::FlagList;

require "conversion.constants.pl";
require "bin/calc_UV_winds_NEW.pl";
require "bin/calc_dewpoint_NEW.pl";
require "bin/calc_specific_humidity_NEW.pl";

my $DEBUG = 1;
my $PRINT_HEADER = 1;
my $MISSING = -999.99;

#------------------------------------------
# first, fetch the data directories
#------------------------------------------
my %data_dirs = %{&data_dirs()};

#------------------------------------------
# fetch the project begin & end date(s)
#------------------------------------------
my %project_dates = %{&project_dates()};
my $date_begin = $project_dates{'begin'};
my $date_end = $project_dates{'end'};

# the output filename
my $output_fname = &output_fname();

# CSE_id
my $CSE_id = &cse_id();
# site id
my $site_id = &site_id();
# station id
my $station = &station();

#------------------------------------------
# convert the project begin/end times to seconds since 1/1/1970
#------------------------------------------
my $date_begin_epoch = convert_to_epoch("$date_begin 00:00:00");
my $date_end_epoch = convert_to_epoch("$date_end 23:59:59");
# make sure that the project dates make sense
die  "ERROR: $date_begin is greater than $date_end" if ( $date_begin_epoch > $date_end_epoch );

#------------------------------------------
# the list of all files in the data directories
#------------------------------------------
my @list_of_files = fetch_all_files( \%data_dirs );

#------------------------------------------
# generate a list of files that fall
# within the begin/end dates where file_list
# is an Data::FileList object
#------------------------------------------
my $file_list = &fetch_file_list( \@list_of_files, $date_begin_epoch, $date_end_epoch );

#------------------------------------------
# the input/output time intervals
my $output_time_interval = &time_interval()->{'output'};
my $input_time_interval = &time_interval()->{'input'};
#------------------------------------------

#------------------------------------------
# fetch a list of DQR::Flag objects...returns
# a DQR::FlagList object
#------------------------------------------
# a list of DQR filenames 
my $dqr_fname_ref = &dqr_fname();
# read each file and put a FlagList Object into a hash
my ($fname, %dqr_flag_hash, $key);
foreach $key(keys %$dqr_fname_ref) {
  # the name of the DQR file to process
  # where $key is the type (gndrad, mettwr2h, mettwr4h..etc)
  $fname = $dqr_fname_ref->{$key};
  # return a hash containing a FlagList object
  $dqr_flag_hash{$key} = &read_dqr($fname, $input_time_interval, $output_time_interval, $date_begin, $date_end);
} # end foreach

#------------------------------------------
# make sure there are files to process!!
#------------------------------------------
my $num_files = $file_list->num_files();
die "no files to process between $date_begin & $date_end" if ( $num_files <= 0 );

#------------------------------------------
# a list of files by date where the
# key is the date and the value is
# a reference to an array of file objects 
#------------------------------------------
# first, get a list of dates
my $list_of_dates = $file_list->list_of_dates();
# now, create the hash
my ($date, $files_by_date, $i);
foreach $date (@$list_of_dates) {
  $files_by_date->{$date} = $file_list->files_by_date($date);
} # end foreach

#------------------------------------------
# loop through the list and read each file
#------------------------------------------
# a reference to a list of parameter names for
# each network
my $param_ref = &params();

my ($i, $date, $fname, $file, $arr_ref, $data_ref, $network, $key);

# a reference to a list of categories so
# we know the order that they were entered
# in the param_ref hash...not the most graceful
my $category_ref = &category();

# a reference to a list of header names & formats
my $header_ref = &header();

foreach $date (@$list_of_dates) {

  print "date: $date\n" if ($DEBUG);

  # a reference to an array of file objects for a given date
  $arr_ref = $files_by_date->{$date};

  my $filename_pattern = &filename_pattern();

  # read/print the files for this date
  for ( $i=0; $i<=$#$arr_ref; $i++) {

    $file = $arr_ref->[$i]; 	# the file object

    $network = $file->fetch_network($filename_pattern); # file network 

    $fname = basename($arr_ref->[$i]->name());	# only the filename

    print "\tprocessing $fname\n" if ($DEBUG);

    # read the file 
    #print "\treading $fname..\n";
    my $num_data_points = &num_data_points();

    print "reading ".$file->name()."\n";
    $file->read( $input_time_interval, $param_ref->{$network}, $MISSING, $num_data_points );

    # apply any corrections to the data or flag
    #$file = &apply_corrections($file, \&convert_to_epoch, $MISSING);

    $file->write_header($output_fname, $header_ref ) if $PRINT_HEADER;
    $PRINT_HEADER = 0;

    # print the file data to the output file
    $file->write_data( $output_fname, $param_ref->{$network}, 
                       $input_time_interval, $output_time_interval, 
                       $num_data_points, $CSE_id, $site_id, $station, $MISSING, 
                       \&calc_dewpoint, \&calc_specific_humidity, \&calc_UV_winds, 
                       $dqr_flag_hash{$network}, \&apply_corrections);

    $arr_ref->[$i]->delete();

  } # end for
}
exit();
#*******************************
sub fetch_file_list { 

   # the list of files to process
   my $list_of_files_ref = shift;
   my @list_of_files = @$list_of_files_ref;

   # the project begin & end times as
   # specified in the config file 
   my $date_begin = shift;
   my $date_end = shift;

   # the FileList object
   my $file_list = Data::FileList->new();

   my ($timestamp, $file, $fname, $date, $time);
   my $filename_pattern = filename_pattern();

   foreach $file (@list_of_files) {
      # first, get the date and time
      $fname = basename($file);
      $fname =~ /$filename_pattern/;
      
      $date = "$3/$4/$5";
      $time = "$6:$7:00";

      # now, convert the date/time to seconds since 1/1/1970
      $timestamp = convert_to_epoch("$date $time");

      # only add the files within the specified range
      if ( $timestamp >= $date_begin && $timestamp <= $date_end ) {
        #$file_list->add_file( Data::File->new($file, $timestamp) );
        $file_list->add_file( Data::TWRFile->new($file, $timestamp) );
      } # endif
   } # end foreach

   return $file_list;

}
#------------------------------------------
# get a list of files to process (only those
# with a .dat extension)
#------------------------------------------
sub fetch_all_files {

   # fetch a list of all files to process
   my $dirs = shift;
   my @list_of_files;
   my ($network, $dir);

   foreach $network(sort(keys %$dirs))  {

      # the directory to process
      $dir = "$dirs->{$network}/";
      opendir(FILE_DIR, $dir) || die "cannot open $dir";

      # only fetch the .dat files for this network
      my @this_dir = sort( grep(/\.dat$/, readdir(FILE_DIR)));

      # prepend the directory to the filename
      @this_dir = map($dir.$_, @this_dir);

      # now, add the files in this directory
      # to the master list
      push (@list_of_files, @this_dir);

      closedir(FILE_DIR);

   } # end foreach

   return @list_of_files;

}
#------------------------------------------
# convert a date/time (YYYY/MM/DD hh:mm:ss) to
# seconds since 1/1/1970
#------------------------------------------
sub convert_to_epoch {

  my $date_time = shift;        # YYYY/MM/DD hh:mm:ss

  $date_time =~ /(\d{4})\/(\d{2})\/(\d{2})(\s)(\d{2})\:(\d{2})\:(\d{2})/;
  my $year = int($1);
  $year -= 1900;
  my $month = int($2);
  $month -= 1;
  my $day = int($3);
  my $hour = int($5);
  my $min = int($6);
  my $sec = int($7);

  return timegm($sec, $min, $hour, $day, $month, $year);

}
#------------------------------------------
# return a date/time string (YYYYMMDDhhmm)
# from a unix timestamp
#------------------------------------------
sub fetch_date_time {

  # return the date/time for this timestamp
  # YYYYMMDDhhmm
  my $timestamp = shift;
  my ($sec, $min, $hour, $day, $month, $year) = gmtime($timestamp);
  $month++;
  $year += 1900;

  return "$year$month$day$hour$min";

}
#------------------------------------------
# read the dqr file(s) and return a
# DQR::FileList object
#------------------------------------------
sub read_dqr {

  my $dqr_fname = shift;
  print "reading $dqr_fname...\n";
  my $input_time_interval = shift;
  my $output_time_interval = shift;
  my $input_time_in_seconds = $input_time_interval * 60;
  my $output_time_in_seconds = $output_time_interval * 60;
  # begin and end date (YYYY/MM/DD) 
  my $d1 = shift;
  my $d2 = shift;

  my ($station, $parameter, $flag);
  my ($date_begin, $date_end, $time_begin, $time_end);
  my ($flag_obj);

  # an empty FlagList
  my $flag_list = DQR::FlagList->new();

  open(DQR, "$dqr_fname") || die "cannot open $dqr_fname";

  while ( <DQR> ) {
    chop;
    if ( !/^Station/ && !/^#/ ) {
      ($station, $parameter, $flag, $date_begin,
       $time_begin, $date_end, $time_end) = 
       split(/\s+/, $_);
       if ( &field_is_valid($parameter) ) {
         $flag_obj = DQR::Flag->new($station, $parameter, $flag,
                                    $date_begin, $time_begin, 
                                    $date_end, $time_end);
	 $flag_list->add_flag($flag_obj);
       } # endif
       # create
    } # endif
 
  } # end while
  close(DQR);

  # order the the flag list by flag value precedence
  # this is because there are overlaps in the dqr flags
  # and we want to make sure that we choose the correct flag
  $flag_list->order_by_value();

  #return $flag_list;

  # now, create a hash where each key is the timestamp
  # and each value is it's corresponding flag..we are
  # doing this to avoid looking through the flag_list
  # for every single data point (which is *very*
  # time consuming!!)
  my $ts1 =  convert_to_epoch("$d1 00:00:00");
  my $ts2 = convert_to_epoch("$d2 23:59:59");
  my $flag_hash = $flag_list->to_hash($output_time_in_seconds, $input_time_in_seconds,$ts1, $ts2);

  return $flag_hash;

}
#------------------------------------------
# return a boolean indicating if the given
# field name is valid
#------------------------------------------
sub field_is_valid {

  my $field_to_find = shift;
  my $param_ref = &params();
  my ($network, $parameter_name, %list);

  foreach $network (keys %$param_ref) {
    foreach $parameter_name (keys %{$param_ref->{$network}}) {
      return 1 if ($parameter_name eq $field_to_find );
    } # end foreach
  } # end foreach

  # can't find the field so return false
  return 0;
}
