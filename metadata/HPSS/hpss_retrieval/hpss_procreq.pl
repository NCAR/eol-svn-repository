#!/usr/bin/perl

use File::Basename;

# make sure to include the necessary libs in the
# lib path
#$INC[$#INC+1]="/h/eol/mssuser/wwwmss/lib";
#push(@INC, "/h/eol/snorman/hpss/lib");
#push(@INC, "/net/www/cgi-bin/hpss_retrieval/lib");
push(@INC, "/net/www/cgi-bin/mss_retrieval/lib");

# set the path so this script can be run as a cron job
#$ENV{'PATH'} = "/h/eol/mssuser/bin:/h/eol/mssuser/xbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/usr/bin/X11:/usr/openwin/bin:/opt/SUNWspro/bin:/usr/ccs/bin:/usr/ucb:.";
$ENV{'PATH'} = "/opt/local/hpss/bin:/usr/local/bin:/usr/kerberos/bin:/bin:/usr/bin:/usr/X11R6/bin:/opt/local/bin:/net/local_lnx/bin";

require 'constants.pl'; # the constants
require 'procreq.lib';  # the subroutines used in this script

# script to process the hpss data request
# this script is run as a cron job that wakes
# up every minute, checks the queue for a 
# request and processes the request
# crontab file: * * * * * ~mssuser/hpss/procreq.pl

# make sure the program is not already running
($PROG = $0) =~ s%.*/%%;

if ( $#ARGV < 0 ) {
  if ( is_running($PROG) ) {
    print STDERR "$PROG: Instance already running..exiting..\n";
    exit();
  } # endif
} # endif

# the constants
my %c = &get_constants();
my $queue_path = $c{'queue_path'};
my $error_path = $c{'error_path'};
my $finished_path = $c{'finished_path'};
my $local_dest = $c{'dest_ftp'};
my $mail_cmd = $c{'mail_cmd'};
my $ftp_url = $c{'ftp_url'};
my $total_size=0; # the size of the hpss files
my $size = 0;

#print "checking $queue_path for a request...\n\n";

opendir(QUEUE,"$queue_path") || die "cannot open $queue_path";
my @list_of_requests = grep(/^\w+\.\d+$/, readdir(QUEUE));
my $fname;

foreach $request (@list_of_requests) {
  print "processing $request..\n";
  my $full_path = "$queue_path/$request";
  my $age = -M $full_path; 
  # don't process if age of request is less that a minute
  next if ( $age < .0007 );
  my %request = &parse_request($full_path);

  # move the request to the finished directory
  $cmd = "mv $full_path $finished_path";
  system("$cmd");

  my @hpss_fnames = &get_fnames($request{'file'});
  #my $new_dir = "$local_dest/$request";
  #if ( !-e $new_dir ) {
  #  mkdir($new_dir) || die "cannot open $new_dir: $!";
  #}

  # first, create a new directory
  #my $dest_dir = "$constants{'dest_ftp'}/$request{'name'}.$$";
  my $dest_dir = "$constants{'dest_ftp'}/$request";
  if ( !-e $dest_dir ) {
    if( !(system("mkdir -m 777 -p $dest_dir") >> 8) == 0 ) {
      print "cannot create $dest_dir: $!\n";
      next;
    } # endif
  } # endif

  my $total_size = 0;
  foreach $fname (@hpss_fnames) {
    # fetch the file from the hpss
    my $fname_only = basename($fname);
    my $local_fname = "$dest_dir/$fname_only";
    my ($status, $size) = &hpss_copy( $fname, $local_fname );
    $total_size += $size;

    # move the request to error queue if the transfer was unsuccessful
    $status = "successful";
    if ( $status =~ /unsuccessful/ ) {
       $cmd = "mv $finished_path/$request $error_path";
       # send an email to data req so we know there's a problem!
       $msg = &error_email("$finished_path/$request");
       $mail = "To:$c{'data_email'}\n".
               "From:Mssuser\n".
 	      "Reply-To:$request{'email'}\n".
 	      "Subject:MSS Data Transfer\n\n".
 	      &error_email("$finished_path/$request", "$error_path/$request");
       # now, send an email so we know there is a problem!!
       open(MAIL, "$mail_cmd") || die "cannot open mail command!";
       print MAIL "$mail";
       close(MAIL);
       system($cmd);
       exit();
    } # endif $status

   # compress if necessary
   if ($request{'compression'} =~ /gzip/i) {
     if (substr($fname_only, -3) ne ".gz") {
         $cmd = "nice -19 gzip -f $dest_dir/$fname_only";
         system($cmd);
         $local_fname.=".gz";
       }
   } # endif

   # set the copy status for this local file
   #$copy_status{$local_fname} = $status;
   if ( $status =~ /\bsuccess/i ) {
      @tmp = split(/\//, $dest_dir);
      #$key = "$ftp_url/$local_fname";
      $key = "$ftp_url/$tmp[$#tmp]/$fname_only";
      $copy_status{$key} = $status;
   } else {
      #$copy_status{$mss_fname} = $status;
      $copy_status{$fname} = $status;
   }

  } # end foreach $fname

  # append the file size to the request file
  # for book keeping purposes
  $request_fname = "$finished_path/$request";
  $cmd = "mv $request_fname $request_fname.tmp";
  system($cmd);
  open(REQUEST, "$request_fname.tmp") || die "cannot open $request_fname.tmp";
  open(OUT, ">$request_fname") || die "cannot open $request_fname";
  while (<REQUEST>) {
     chop;
     s/$/; $total_size/g;
     print OUT "$_\n";
  }
  system("rm $request_fname.tmp");
  close(OUT);
  close(REQUEST);

  # generate the appropriate email based on the requested media
  if ( $request{'media'} =~ /ftp/i ) {
     $mail = "To:$request{'email'}\n".
             "From:Mssuser\n".
  	   "Subject:MSS Data Transfer\n\n".&user_email(%copy_status);
  } else {
     $mail = "To:$c{'data_email'}\n".
             "From:Mssuser\n".
  	   "Reply-To:$request{'email'}\n".
  	   "Subject:MSS Data Transfer\n\n".
  	   &datareq_email(\%request, \%copy_status);
  
  } # endif
  
  # now, send an email with the necessary information
  open(MAIL, "$mail_cmd") || die "cannot open mail command!";
  print MAIL "$mail";
  close(MAIL);
} # end foreach $request

close(QUEUE);

exit();
