#!/usr/bin/perl

# script to create an html page containing
# the ceop plots (raw vs converted) 
# SJS 06/09/2009

use Getopt::Std;
use File::Basename;

use strict;

if ( $#ARGV < 0 ) {
  print "create_html.pl\n";
  print "\t-s: station id (ie: C1, C2..etc) \n";
  print "\t-p: plot directory\n";
  print "\t-b: date begin (YYYYMMDD)\n";
  print "\t-e: date end (YYYYMMDD)\n";
  print "\t-o: output html filename\n";
  exit();
}

# the number of columns to display
my $num_columns = 3;

# the scale for the thumbnail images
# (% of original image size)
my $image_scale = .30;

# get the command line options
our($opt_p, $opt_s, $opt_b, $opt_e, $opt_o);
getopt('psbeo');
my $plot_dir = $opt_p;
my $date_begin = $opt_b;
my $date_end = $opt_e;
my $out_fname = $opt_o;
my $station_id = {
                   'C1' => 'C1_Barrow',
                   'C2' => 'C2_Atqasuk'
                 };

my $station = $station_id->{$opt_s};
print_html($station, $date_begin, $date_end, $plot_dir, 
           $image_scale, $num_columns, $out_fname);

sub print_html {

  my $station = shift;
  my $date_begin = shift;
  my $date_end = shift;
  my $plot_dir = shift;
  my $image_scale = shift;
  my $num_columns = shift;
  my $out_fname = shift;

  my $image_num = 1;
  my $date = $date_begin;

  my $d1 = $date_begin;
  my $d2 = $date_end;
  $date_begin =~ /(\d{4})(\d{2})(\d{2})/;
  $date_begin = "$1/$2/$3";
  $date_end =~ /(\d{4})(\d{2})(\d{2})/;
  $date_end = "$1/$2/$3";

  open(HTML, ">$out_fname") || die "cannot open $out_fname";

  print HTML "<html>\n";
  print HTML "<head><title></title></head>\n";
  print HTML "<body>\n";
  print HTML "<h3>$station ($date_begin - $date_end)</h3>\n";
  
  opendir(DIR, $plot_dir) || die "cannot open $plot_dir";
  my @list_of_all_files = readdir(DIR);
  my ($param, $stn, $date_time, $ext);
  my $begin_end_date = "$d1\_$d2";
  my $ext = "png";
  my $file_mask = "$station\.$begin_end_date\.$ext";
  my @list_of_files = grep(/$file_mask/, @list_of_all_files);
  closedir(DIR);

  my ($file, @tmp, $parameter, $thumbnail_fname);
  print HTML "<table border='1'>\n";
  foreach $file (sort(@list_of_files)) {
    $thumbnail_fname = create_thumbnail("$plot_dir/$file", $image_scale);
    @tmp = split(/\./, $file);
    $parameter = $tmp[0];
    if ( ($image_num-1) == 0 ) {
      print HTML "<tr>\n";
    } elsif ( ( ($image_num-1) % $num_columns) == 0 ) {
      print HTML "<\/tr><tr>\n";
    } # endif
    print HTML "<td>\n";
    print HTML "<table border='0' align='center'><tr><td>$parameter</td></tr></table>\n";
    print HTML "<a href='$file'><img src='".basename($thumbnail_fname)."'/></a>\n";
    $image_num++;
  } # end foreach
  print HTML "</table>\n";
  print HTML "</body>\n";
  print HTML "</html>\n";

  close(HTML);

}
sub create_thumbnail {

  # create a thumbnail for the image
  my $fname = shift;

  # the scaling factor for the thumbnail images
  my $image_scale = shift;

  # first, get the dimensions for the image
  my ($source_width, $source_height) = get_dimensions($fname);

  # now, create the thumbnail
  my $target_width = $source_width*$image_scale;
  my $target_height = $source_height*$image_scale;

  # first, construct the thumbnail filename
  my $fname_only = basename($fname);
  # get the path for the thumbnail image
  my $path_only = dirname($fname);

  # now, create the thumbnail file name
  my @tmp = split(/\./, $fname_only);
  my $ext = pop(@tmp);
  push(@tmp, "small.$ext");
  my $thumbnail_fname = join(".", @tmp);
  # add the path to the thumbnail filename
  $thumbnail_fname = "$path_only/$thumbnail_fname";

  # create the thumbnail using ImageMagick convert command
  unlink $thumbnail_fname if  ( -e $thumbnail_fname );
  my $source_dim = $source_width."x".$source_height;
  my $target_dim = $target_width."x".$target_height;
  my $command = "/usr/bin/convert -size $source_dim $fname -thumbnail $target_dim -unsharp 0x.5 $thumbnail_fname";
  my $status =`$command`;

  die "Can't create $thumbnail_fname" if ($status ne '');

  return $thumbnail_fname;

}
sub get_dimensions {

   # return the dimensions for the image
   my $full_path = shift;

   # use the identify command to get the dimensions for the image
   my ($width, $height);
#   my $command = "/usr/bin/identify -ping -format '%w %h' $full_path";
   my $command = "identify -ping -format '%w %h' $full_path";

   # run the command and parse out the dimensions
   open(IDENTIFY, "$command |") || die "ERROR: $!";
   while (<IDENTIFY>) {
     if(/(\d+)\s+(\d+)/) {
        $width = $1;
        $height = $2;
     } # endif
   } # end while
   close(IDENTIFY);

   return ($width, $height);

}
