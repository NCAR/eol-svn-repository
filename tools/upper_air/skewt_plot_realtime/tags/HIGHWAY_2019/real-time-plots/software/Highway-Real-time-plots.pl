#! /usr/bin/perl -w

##Module-----------------------------------------------------------------------
## <p>The Highway-Real-time-plots.pl script is a cron script that does the following
## tasks:</p>
## <ol>
##   <li>Create log file for output. 
##   <li>If DO_NOT_DELETE_running file exists in $data_proc_dir directory, then exit.</li>
##   <li>Save a long listing of the files in the ftp directory for use later.</li>
##   <li>Copy GTS-BUFR files from the incoming ftp area to the raw_gts_bufr_data directory
##          if newer than the DO_NOT_DELETE_last_run_time file.</li>
##   <li>Append a listing of the raw files and the snapshot file to the cross reference file.<li>
##   <li>Run conversion programs on GTS-BUFR files.</li> 
##   <li>Run script to convert .cls files to .cls.sharppy format.</li> 
##   <li>Run the python script to create skew-t plots from the .cls.sharppy format.</li> 
##   <li>Tar, gzip all processed and raw files to the snapshot directory.</li> 
##   <li>Remove all files from directories that were included in the tar, gzip.</li> 
##   <li>Upload skew-t plots to the field catalog directory.</li> 
##   <li>Move $running_file to $last_run_time file.<\li>
## </ol>
##
## @author Janet Scannell 
## @version 1.0 July 2019
##
###Module-----------------------------------------------------------------------
#
use strict;
use lib "/net/work/lib/perl/mail";
use DateTime;
use MAIL;
use File::Find;

my $ftp_dir = "/net/work/Projects/HIGHWAY/data_ingest/upperair/radiosonde/GTS/HKNC/BUFR";
my $ftp_dir2 = "/net/work/Projects/HIGHWAY/data_ingest/upperair/radiosonde/GTS/HKLO/BUFR";
my $project_dir = "/net/work/Projects/HIGHWAY/real-time-plots";
my $data_proc_dir = "$project_dir/data_processing";
my $software_dir = "$project_dir/software";
my $processed_dir = "$data_proc_dir/snapshots";
my $log_dir = "$data_proc_dir/logs";
my $raw_gts_bufr_dir = "$data_proc_dir/raw_gts_bufr_data";
my $output_ascii_dir = "$data_proc_dir/output_ascii_data";
my $output_preproc_dir = "$data_proc_dir/output_preproc_data";
my $output_esc_dir = "$data_proc_dir/output_esc";
my $output_plots = "$data_proc_dir/output_plots";
my $catalog_dir = "/net/iftp2/pub/incoming/catalog/highway";
my $running_file = "$data_proc_dir/DO_NOT_DELETE_running";
my $last_run_time = "$data_proc_dir/DO_NOT_DELETE_last_run_time";
my $to_ascii_pgm = "$software_dir/preprocess_GTS_BUFR.pl";
my $to_esc_pgm = "$software_dir/GTSBUFR_Radiosonde_Converter_RealTime.pl";
my $to_sharppy_pgm = "$software_dir/convertESCtosharppy.pl";
my $to_skewt_pgm = "$software_dir/plot_sounding.py";
my $datafiles_listing_short = "raw-data-cross-reference.txt";

my @monitors = ('anstett@ucar.edu', 'cully@ucar.edu', 'loehrer@ucar.edu', 'gstoss@ucar.edu');

# Get current time in UTC.
my $dt = DateTime->now;
my $current_time = sprintf("%04d%02d%02d%02d%02d", $dt->year, $dt->month, $dt->day, $dt->hour, $dt->minute);

# Create log and error files
my $log_file = "$log_dir/highway-rt-log-" . $current_time . ".log";
my $err_file = "$log_dir/highway-rt-err-" . $current_time . ".err";
my $ftp_listing = "$log_dir/highway-rt-ftp-listing-" . $current_time . ".txt";
my $datafiles_listing = "$processed_dir/$datafiles_listing_short";
my $snapshot_file_short = "data_files_$current_time.tar.gz";
my $snapshot_file = "$processed_dir/$snapshot_file_short";
if (! open (LOGFILE, ">$log_file")) {
   sendMail(sprintf("Cannot create $log_file.\n"));
   exit(1);
}
print LOGFILE "Current time (UTC): $current_time\n";

# Does $running_file exist?
# If so, then a previous process is running, so end this process.
if (-e $running_file) {
   printf LOGFILE "$running_file exists.  A previous process is already running.\n";
   close(LOGFILE);
   exit(1);   
}

# Save current time to $running_file, then at the end move $running_file to $last_run_time
if (! open (RUNFILE, ">$running_file")) {
   sendMail(sprintf("Cannot create $running_file.\n"));
   print LOGFILE "Cannot create $running_file.\n";
   close(LOGFILE);
   exit(1);
}
print RUNFILE $current_time;
close(RUNFILE);
print LOGFILE "Created $running_file\n";

# Save a long listing of the ftp directory to debug problems that happen.
system("ls -l $ftp_dir/HKNC* >>$ftp_listing\n");
system("ls -l $ftp_dir2/HKLO* >>$ftp_listing\n");
print LOGFILE "Created a long listing of $ftp_dir.\n";
print LOGFILE "Created a long listing of $ftp_dir2.\n";

# Find GTS-BUFR files that are newer than $last_run_time in ftp directory and copy to $raw_gts_bufr_dir.
# Only copy bufr files that start with HKNC_
# Avoid files that are soundings.bufr per Greg 9/12/19
# Also, copy files from $ftp_dir2 that start with HKLO_
# This is a new station to process. 10/23/19

my $age_of_last_run_time = -M $last_run_time;
my @bufr_files;
find(sub {
   push @bufr_files, $File::Find::name if (-f $File::Find::name and /HKNC_.*sounding\.bufr$/ and (-M $_) <= $age_of_last_run_time);
   }, $ftp_dir);
foreach my $bufr_file (@bufr_files) {
   system("cp -p $bufr_file $raw_gts_bufr_dir/. >> $err_file");
   print LOGFILE "cp -p $bufr_file $raw_gts_bufr_dir/. >> $err_file\n";
}
print LOGFILE "Copied *.bufr files from $ftp_dir that are newer than $last_run_time file.\n";

# Zero out array before find next station.
@bufr_files = ();
find(sub {
   push @bufr_files, $File::Find::name if (-f $File::Find::name and /HKLO_.*\.bufr$/ and (-M $_) <= $age_of_last_run_time);
   }, $ftp_dir2);
foreach my $bufr_file (@bufr_files) {
   system("cp -p $bufr_file $raw_gts_bufr_dir/. >> $err_file");
   print LOGFILE "cp -p $bufr_file $raw_gts_bufr_dir/. >> $err_file\n";
}
print LOGFILE "Copied *.bufr files from $ftp_dir2 that are newer than $last_run_time file.\n";

# Check if there were any files moved for processing
# If no files to process, print message to logfile, then exit.
if (! opendir(PDIR, $raw_gts_bufr_dir)) {
   sendMail(sprintf("Cannot open $raw_gts_bufr_dir.\n"));
   print LOGFILE "Cannot open $raw_gts_bufr_dir.\n";
   close(LOGFILE);
   exit(1);
}
my @raw_files = grep(/^.*\.bufr$/,readdir(PDIR));
closedir(PDIR);
if ($#raw_files == -1) {
   print LOGFILE "No *.bufr files found to process. Exiting...\n";
   system("rm $running_file >> $err_file");
   print LOGFILE "rm $running_file >> $err_file\n";
   close(LOGFILE);
   exit(1);
}
# Create the file that lists the snapshot file and which raw bufr files were processed.
if (! open (CROSSFILE, ">>$datafiles_listing")) {
   sendMail(sprintf("Cannot open $datafiles_listing.\n"));
   print LOGFILE "Cannot open $datafiles_listing.\n";
} else {
   foreach my $raw_file (@raw_files) {
      print CROSSFILE "$snapshot_file_short: $raw_file\n";
   }
   close(CROSSFILE);
}

if (! chdir($software_dir)) {
   sendMail(sprintf("Cannot chdir to $software_dir.\n"));
   print LOGFILE "Cannot chdir to $software_dir.\n";
   close(LOGFILE);
   exit(1);
}

# Process all *.bufr files, including sorting the files.
system("$to_ascii_pgm $raw_gts_bufr_dir $output_ascii_dir $output_preproc_dir >& $log_dir/preproc_HIGHWAY_$current_time.log");
print LOGFILE "$to_ascii_pgm $raw_gts_bufr_dir $output_ascii_dir $output_preproc_dir >& $log_dir/preproc_HIGHWAY_$current_time.log\n";
print LOGFILE "Ran $to_ascii_pgm.\n";
system("$to_esc_pgm HIGHWAY $output_preproc_dir $output_esc_dir 0 >& $log_dir/runHIGHWAY_rt_$current_time.log");
print LOGFILE "$to_esc_pgm HIGHWAY $output_preproc_dir $output_esc_dir 0 >& $log_dir/runHIGHWAY_rt_$current_time.log\n";
print LOGFILE "Ran $to_esc_pgm.\n";
system("ant sort_esc >& $log_dir/ant_sort.log");
print LOGFILE "ant sort_esc >& $log_dir/ant_sort.log\n";
print LOGFILE "Ran sorting command.\n";

# Process all *_01_SONDE.cls and *_01_PIBAL.cls files and create skewT plots.
# Only process the first sonde if there are more than one in the original raw file.
if (! opendir(PDIR, $output_esc_dir)) {
   sendMail(sprintf("Cannot open $output_esc_dir.\n"));
   print LOGFILE "Cannot open $output_esc_dir.\n";
   close(LOGFILE);
   exit(1);
}
my @esc_files = sort(grep(/^.*_01_[SP].*\.cls$/, readdir(PDIR)));
closedir(PDIR);
foreach my $file (@esc_files) {
   if (-z "$output_esc_dir/$file") {
      print LOGFILE "Zero length cls file: $output_esc_dir/$file\n";
   } else {
      system("$to_sharppy_pgm $output_esc_dir/$file >> $err_file");
      print LOGFILE "$to_sharppy_pgm $output_esc_dir/$file >> $err_file\n";
      print LOGFILE "Converted $output_esc_dir/$file to sharppy format.\n";
      system("$to_skewt_pgm $output_esc_dir/$file.sharppy >>$log_dir/runSkewTplot_$current_time.log 2>>$log_dir/runSkewTplot_$current_time.err");
      print LOGFILE "$to_skewt_pgm $output_esc_dir/$file.sharppy >>$log_dir/runSkewTplot_$current_time.log 2>>$log_dir/runSkewTplot_$current_time.err\n";
      print LOGFILE "Created skew-t from $output_esc_dir/$file.sharppy.\n";
   }
}
# Check if there were any skew-ts created
# If no skew-ts, print message to logfile and send email.
if (! opendir(PDIR, $output_plots)) {
   sendMail(sprintf("Cannot open $output_plots.\n"));
   print LOGFILE "Cannot open $output_plots.\n";
} else {
   my @plot_files = grep(/^.*\.png$/, readdir(PDIR));
   closedir(PDIR);
   if ($#plot_files == -1) {
      sendMail(sprintf("No skew-ts created for $current_time (UTC).\n Check this file for more information:\n $snapshot_file\n"));
      print LOGFILE "No skew-ts created for $current_time (UTC).\n";
      print LOGFILE "Check this file for more information:\n";
      print LOGFILE "$snapshot_file\n";
   } else {
      system("cp -p $output_plots/* $catalog_dir/. >> $err_file");
      print LOGFILE "cp -p $output_plots/* $catalog_dir/. >> $err_file\n";
      print LOGFILE "Copied all skew-t plots to catalog ingest directory.\n";
   }
}
if (! chdir($data_proc_dir)) {
   sendMail(sprintf("Cannot chdir to $data_proc_dir.\n"));
   print LOGFILE "Cannot chdir to $data_proc_dir.\n";
   close(LOGFILE);
   exit(1);
}

system("tar czf $snapshot_file DO_NOT_DELETE_last_run_time snapshots/$datafiles_listing_short logs output_ascii_data output_esc output_plots output_preproc_data raw_gts_bufr_data >> $err_file");
print LOGFILE "tar czf $snapshot_file DO_NOT_DELETE_last_run_time snapshots/$datafiles_listing_short logs output_ascii_data output_esc output_plots output_preproc_data raw_gts_bufr_data >> $err_file\n";
print LOGFILE "Tar, gzip all files created in this run.\n";
system("rm logs/* output_ascii_data/* output_esc/* output_plots/* output_preproc_data/* raw_gts_bufr_data/* >> $err_file");
print LOGFILE "rm logs/* output_ascii_data/* output_esc/* output_plots/* output_preproc_data/* raw_gts_bufr_data/* >> $err_file\n";
print LOGFILE "Removed all data files from this run.\n";

# Move $running_file to $last_run_time.
system("mv $running_file $last_run_time >> $err_file");
print LOGFILE "mv $running_file $last_run_time >> $err_file\n";
print LOGFILE "Moved $running_file to $last_run_time.\n";
close(LOGFILE);

##-----------------------------------------------------------------------------
# @signature sendMail()
# <p>Send email to monitors if there was an error during the cron job.</p>
##-----------------------------------------------------------------------------
sub sendMail
{
   my ($body) = @_;
   MAIL::send_mail("HIGHWAY Real-time Plots Output - ERROR", $0 . "\n\n" . $body, @monitors);
}
