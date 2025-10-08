#!/usr/bin/perl -w

use Time::Local;
#************************
# convert a date/time (YYYY/MM/DD hh:mm:ss) to
# seconds since 1/1/1970
sub convert_to_epoch {

  my $date = shift;	# YYYY/MM/DD
  my $hour = shift;
  my $min = shift;
  my $sec = 0;

  $date =~ /(\d+)\/(\d+)\/(\d+)/;
  my $year = $1;
  $year -= 1900;
  my $month = $2;
  $month -= 1;
  my $day = $3;

  return timegm($sec, $min, $hour, $day, $month, $year);

}
#***********************
sub apply_precip_corrections {

  my $value = shift;
  my $flag = shift;
  my $max_value = shift;

  return "D" if ( $value > $max_value ); 

  return $flag;

}
#***********************
sub apply_value_limit {

  my $id = shift;
  my $value = shift;
  my $flag = shift;
  my $category = shift;
  my $date = shift;
  my $hour = shift;
  my $min = shift;

  my $actual_epoch_time = &convert_to_epoch($date, $hour, $min);

  my $flag_precedence = {"M"=>11, "N"=>10, "C"=>9, "I"=>8, "X"=>7, "B"=>6, "E"=>5, "D"=>4, "U"=>3, "G"=>2, "T"=>1};
  my $bad = "B";
  my $dubious = "D";

  # apply the corrections based on flag priority
  $existing_flag_precedence = $flag_precedence->{$flag};
  if ( $existing_flag_precedence > $flag_precedence->{"B"} ) {
    return $flag;
  } # endif


  if ( $id eq "C2") {

    if ( $category eq "long_in" ) {
      # flag long_in and net_rad as "D" for any long_in value > 420
      my $value_to_check = 420.00;
      if (&check_flag($actual_epoch_time, "2008/08/01 00:00:00", "2008/08/31 23:59:00")) {
        return $dubious if ( $value > $value_to_check );
      } # endif
    } # endif 


  } # endif

  return $flag;

}
#***********************
sub get_post_analysis_flag {

  # this is where all the custom flags go
  my $id = shift;
  my $category = shift;
  my $flag = shift;
  my $date = shift;
  my $hour = shift;
  my $min = shift;
  my ($epoch_begin, $epoch_end);	# the target epoch times

  my $actual_epoch_time = &convert_to_epoch($date, $hour, $min);
  #print "finding flag for $category $date $hour:$min\n";

  my $flag_precedence = {"M"=>11, "N"=>10, "C"=>9, "I"=>8, "X"=>7, "B"=>6, "E"=>5, "D"=>4, "U"=>3, "G"=>2, "T"=>1};

  # if the original flag is set to missing,
  # then leave it alone!
  return $flag if ( $flag eq "M" );

  my $dubious = "D";
  my $bad = "B";
  if ($id eq "C1") {

    if ( $category eq "net_rad" || $category eq "long_in" ) {

      if (&check_flag($actual_epoch_time, "2005/05/19 09:00:00", "2005/05/19 09:00:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$dubious});
        return $dubious;
      } elsif (&check_flag($actual_epoch_time, "2005/05/19 10:00:00", "2005/05/19 11:00:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$dubious});
        return $dubious;
      } elsif (&check_flag($actual_epoch_time, "2005/05/20 09:00:00", "2005/05/20 15:30:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$dubious});
        return $dubious;
      } elsif (&check_flag($actual_epoch_time, "2005/05/22 04:00:00", "2005/05/22 04:00:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$dubious});
        return $dubious;
      } elsif (&check_flag($actual_epoch_time, "2005/05/22 07:30:00", "2005/05/22 10:00:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$dubious});
        return $dubious;
      } elsif (&check_flag($actual_epoch_time, "2005/05/22 19:30:00", "2005/05/22 19:30:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$dubious});
        return $dubious;
      } elsif (&check_flag($actual_epoch_time, "2005/05/23 03:30:00", "2005/05/23 05:30:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$dubious});
        return $dubious;
      } elsif (&check_flag($actual_epoch_time, "2005/05/28 16:00:00", "2005/05/28 16:30:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$dubious});
        return $dubious;
      } elsif (&check_flag($actual_epoch_time, "2005/05/31 16:00:00", "2005/05/31 19:00:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$dubious});
        return $dubious;
      } elsif (&check_flag($actual_epoch_time, "2005/09/13 22:00:00", "2005/09/14 02:00:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$dubious});
        return $dubious;
      } elsif (&check_flag($actual_epoch_time, "2006/02/15 13:30:00", "2006/02/15 13:30:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$dubious});
        return $dubious;
      } elsif (&check_flag($actual_epoch_time, "2006/02/15 14:30:00", "2006/02/15 16:30:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$dubious});
        return $dubious;
      } elsif (&check_flag($actual_epoch_time, "2007/09/30 20:30:00", "2007/10/03 01:00:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$bad});
        return $bad;
      } else {
        return $flag;
      } # endif

    }  # endif 

    if ( $category eq "net_rad" || $category eq "short_out" ) {
      if (&check_flag($actual_epoch_time, "2005/08/26 19:00:00", "2005/08/27 00:30:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$bad});
        return $bad;
      } # endif
    } # endif

    if ( $category eq "net_rad" || $category eq "short_in" ) {
      if (&check_flag($actual_epoch_time, "2007/09/30 20:00:00", "2007/10/03 01:30:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$bad});
        return $bad;
      } # endif
    } # endif

    if ( $category eq "skintemp" ) {
      if (&check_flag($actual_epoch_time, "2007/09/08 04:00:00", "2007/09/08 04:00:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$bad});
        return $bad;
      } 
      if (&check_flag($actual_epoch_time, "2007/09/08 21:00:00", "2007/09/08 21:00:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$bad});
        return $bad;
      } # endif
    } # endif

    if ( $category eq "snow" ) {
      if (&check_flag($actual_epoch_time, "2005/01/01 00:00:00", "2007/12/31 23:59:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$bad});
        return $bad;
      } # endif
    } # endif

    if ( $category eq "wind_spd" || $category eq "wind_dir" || 
         $category eq "U_wind" || $category eq "V_wind" ) {
      if (&check_flag($actual_epoch_time, "2005/01/01 00:00:00", "2005/04/26 19:00:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$bad});
        return $bad;
      } # endif
    } # endif

    # The following precip corrections are from the DQR files and represent the flag for the
    # accumulation..ie: if 1 precip is bad, then the entire accumulation must be flagged as bad
    # Here..we set the date/time to the next output data time period.  For example: if the DQR
    # reports a bad value at 2005/09/05 22:21, then we set the flag value for 22:30 since 22:21
    # is part of the accumulated value for 22:30.
    if ( $category eq "precip" ) {
      if (&check_flag($actual_epoch_time, "2005/08/22 18:00:00", "2005/08/27 02:00:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$dubious});
        return $dubious;
      } elsif (&check_flag($actual_epoch_time, "2005/09/05 22:30:00", "2005/09/06 19:30:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$dubious});
        return $dubious;
      } elsif (&check_flag($actual_epoch_time, "2006/06/13 21:45:00", "2006/06/14 00:00:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$bad});
        return $bad;
      } elsif (&check_flag($actual_epoch_time, "2007/04/09 23:30:00", "2007/10/10 00:30:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$dubious});
        return $dubious;
      } elsif (&check_flag($actual_epoch_time, "2008/10/10 12:30:00", "2008/10/10 18:00:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$dubious});
        return $dubious;
      } elsif (&check_flag($actual_epoch_time, "2008/10/11 07:00:00", "2008/10/11 11:00:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$dubious});
        return $dubious;
      } elsif (&check_flag($actual_epoch_time, "2008/11/07 07:00:00", "2008/11/08 19:30:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$dubious});
        return $dubious;
      } elsif (&check_flag($actual_epoch_time, "2008/11/22 14:00:00", "2008/11/24 03:30:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$dubious});
        return $dubious;
      } elsif (&check_flag($actual_epoch_time, "2008/11/25 19:00:00", "2008/11/27 03:00:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$dubious});
        return $dubious;
      } elsif (&check_flag($actual_epoch_time, "2008/02/13 15:00:00", "2009/02/18 18:00:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$dubious});
        return $dubious;
      } # endif

    } # endif

    # per Scot, 2010/02/24
    if ( $category eq "stn_pres" || $category eq "temp_air" || $category eq "dew_pt" || $category eq "rel_hum" ) { 
      if (&check_flag($actual_epoch_time, "2008/12/18 21:00:00", "2008/12/18 21:00:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$bad});
        return $bad;
      } # endif
    } # endif
    if ( $category eq "wind_spd" || $category eq "wind_dir" || $category eq "U_wind" || $category eq "V_wind" ) {
      if (&check_flag($actual_epoch_time, "2008/12/18 20:30:00", "2008/12/18 21:00:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$bad});
        return $bad;
      } # endif
    } # endif
    if ( $category eq "wind_spd" || $category eq "wind_dir" || $category eq "U_wind" || $category eq "V_wind" || 
         $category eq "skintemp" ) {
      if (&check_flag($actual_epoch_time, "2008/08/01 09:30:00", "2008/08/01 10:00:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$bad});
        return $bad;
      } # endif
    } # endif
   

  } elsif ($id eq "C2") {

    if ( $category eq "net_rad" || $category eq "long_in" ) {
      if (&check_flag($actual_epoch_time, "2005/03/09 02:30:00", "2005/03/09 05:00:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$bad});
        return $bad;
      } # endif
      if (&check_flag($actual_epoch_time, "2007/09/01 02:30:00", "2007/09/01 02:30:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$bad});
        return $bad;
      } # endif
    } # endif

    if ( $category eq "snow" ) {
      if (&check_flag($actual_epoch_time, "2005/01/01 00:00:00", "2007/12/31 23:59:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$dubious});
        return $dubious;
      } # endif
    } # endif

    if ( $category eq "skintemp" ) {
      if (&check_flag($actual_epoch_time, "2005/01/07 18:30:00", "2005/01/07 18:30:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$bad});
        return $bad;
      } # endif
      if (&check_flag($actual_epoch_time, "2005/04/06 21:00:00", "2005/04/06 22:30:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$bad});
        return $bad;
      } # endif
      if (&check_flag($actual_epoch_time, "2005/11/27 23:00:00", "2005/11/28 00:30:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$bad});
        return $bad;
      } # endif
    } # endif

    if ( $category eq "stn_pres" ) {
      if (&check_flag($actual_epoch_time, "2005/11/24 20:00:00", "2005/11/24 20:00:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$bad});
        return $bad;
      } # endif
    } # endif

    # The following precip corrections are from the DQR files and represent the flag for the
    # accumulation..ie: if 1 precip is bad, then the entire accumulation must be flagged as bad
    # Here..we set the date/time to the next output data time period.  For example: if the DQR
    # reports a bad value at 2005/09/05 22:21, then we set the flag value for 22:30 since 22:21
    # is part of the accumulated value for 22:30.
    if ( $category eq "precip" ) {
       if ( &check_flag($actual_epoch_time, "2006/02/21 11:30:00", "2006/02/22 00:00:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$bad});
         return $bad;
       } elsif ( &check_flag( $actual_epoch_time, "2006/08/21 22:30:00", "2006/08/21 23:30:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$bad});
         return $bad;
       } elsif ( &check_flag( $actual_epoch_time, "2006/08/22 03:00:00", "2006/08/22 03:30:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$bad});
         return $bad;
       } elsif ( &check_flag( $actual_epoch_time, "2006/08/23 22:30:00", "2006/08/23 22:30:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$bad});
         return $bad;
       } elsif ( &check_flag( $actual_epoch_time, "2006/08/24 16:30:00", "2006/08/24 16:30:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$bad});
         return $bad;
       } elsif ( &check_flag( $actual_epoch_time, "2006/08/25 03:00:00", "2006/08/25 03:30:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$bad});
         return $bad;
       } elsif ( &check_flag( $actual_epoch_time, "2006/09/01 02:00:00", "2006/09/01 04:00:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$bad});
         return $bad;
       } # endif
    } # endif

    # per Scot, 2010/02/24
    if ( $category eq "wind_spd" || $category eq "wind_dir" || $category eq "U_wind" || $category eq "V_wind") {
      if (&check_flag($actual_epoch_time, "2008/12/16 01:30:00", "2008/12/16 12:00:00")) {
        return $flag if ( $flag_precedence->{$flag} > $flag_precedence->{$bad});
        return $bad;
      } # endif
    } # endif
  } # endif

  return $flag;

}
# return the specified flag if the
# value falls in the specified range
sub check_flag {

  my $actual_epoch_time = shift;	# seconds since 1/1/1970
  my $date_time_begin = shift;		# YYYY/MM/DD hh:mm:ss
  my $date_time_end = shift;		# YYYY/MM/DD hh:mm:ss

  # split out the date/time stings
  my ($date_begin, $time_begin) = split(/\s+/, $date_time_begin);
  
  # now, get the begin hour,min,sec from the time string
  $time_begin =~ /(\d{2}):(\d{2}):(\d{2})/;
  my $hour_begin = $1;
  my $min_begin = $2;
  my $sec_begin = $3;

  # get the begin epoch time
  my $epoch_begin = &convert_to_epoch($date_begin, $hour_begin, $min_begin);

  # split out the date/time stings
  my ($date_end, $time_end) = split(/\s+/, $date_time_end);
  
  # now, get the end hour,min,sec from the time string
  $time_end =~ /(\d{2}):(\d{2}):(\d{2})/;
  my $hour_end = $1;
  my $min_end = $2;
  my $sec_end = $3;

  # get the end epoch time
  my $epoch_end = &convert_to_epoch($date_end, $hour_end, $min_end);

  if ( ($epoch_end < $epoch_begin) || ($epoch_begin > $epoch_end) ) {
    print "WARNING: begin and end time corrections do not make sense where begin = $date_begin $hour_begin:$min_begin "
          ."and end = $date_end $hour_end:$min_end\n";
    return $existing_flag;
  } # endif

  return 1 if ( $actual_epoch_time >= $epoch_begin && $actual_epoch_time <= $epoch_end);

  return 0;

}
sub fix_lon {

  my $lon = shift;
  # make sure that lon is a number
  $lon += 0;
  my $date = shift;
  my $hour = shift;
  my $min = shift;
  my $station = shift;

  my $epoch_time_begin = &convert_to_epoch("2005/01/01", "00", "00");
  my $epoch_time_end = &convert_to_epoch("2009/12/31", "23", "59");
  my $epoch_time = &convert_to_epoch($date, $hour, $min);

  if ( $epoch_time >= $epoch_time_begin && $epoch_time <= $epoch_time_end ) {
    if ( $station eq "C2" && $lon == -157.40700) {
      return -157.40660;
    } # endif
  } # endif

  return $lon;

}
1;
