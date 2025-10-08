#!/bin/perl -w
# program stereographic_pos.pl by Darren R. Gallant April 28th,1997
# Darren R. Gallant JOSS
# program calculates latitude and logitude using stereographic position
# input files must be in class format
$wind_tol = 0.0001;
use POSIX;
$pi = 3.14159265;$radian = $pi/180;$degree = 1/$radian;
if(@ARGV < 1){
  print "\nUsage is stereographic_pos.pl class_file(s)\n";
  exit;
}
foreach(@ARGV){
  $file = shift(@ARGV);
  if($file =~ /\.gz/){
    open(FILE,"gzcat $file|") || die "Can't open $file file\n";
  }else{
    open(FILE,"$file") || die "Can't open $file file\n";
  }
  if($file =~ /\.cls/){
    $pos = index($file,".cls");
    $outfile = substr($file,0,$pos+4);
  }elsif($file =~ /\.cls.qc/){
    $pos = index($file,".cls.qc");
    $outfile = substr($file,0,$pos+7);
  }elsif($file =~ /\.class/){
    $pos = index($file,".class");
    $outfile = substr($file,0,$pos+6);
  }else{
    print "ERROR $file doesn't end with .cls , .cls.qc or .class\n";
    exit;
  }
  $cont_flag = 0;
  if($pos > -1){
    print "Processing $file\n";
    $outfile = $outfile.".3";
    print "Outfile $outfile\n";
    open(OUT,">$outfile") || die "Can't open $outfile\n";
    $cont_flag = 1;
  }# end if
  if($cont_flag){
    $i = 0;$first = 0;$radius = 6371220.0;
    while(<FILE>){
      if($i < 15){
        print OUT $_;
      }else{
        @input = split(' ',$_);
        if($first == 0){
	      $l_time = $input[0];$l_uwind = $input[5];$l_vwind = $input[6];
	      $l_lat = $input[11];$l_lon = $input[10];
	      $radius = $radius + $input[14];
	      $l_alt = $input[14];
	      $first++;
        }else{
          $time = $input[0];$uwind = $input[5];$vwind = $input[6];
          if(($l_lon == 9999.000)||($uwind == 9999.0)||($l_uwind == 9999.0)){
            $input[10] = 9999.000;
            $input[11] = 999.000;
          }else{
            if(abs($uwind) <= $wind_tol && abs($vwind) <= $wind_tol){
              $input[11] = $l_lat;
              $input[10] = $l_lon;
            }else{
              $difft = abs($l_time - $time);$diffu = ($uwind + $l_uwind)/2.0;
              $diffv = ($vwind + $l_vwind)/2.0;
              $x = $difft*$diffu;
              $y = $difft*$diffv;
              $rho = sqrt($x*$x + $y*$y);
              $c = 2.0*POSIX::atan($rho/(2.0*$radius*1.0));
              $input[11] = POSIX::asin(cos($c)*sin($l_lat*$radian)+$y*sin($c)*cos($l_lat*$radian)/$rho);$input[11] = $input[11]*$degree;# latitude
              $input[10] = $l_lon + POSIX::atan($x*sin($c)/($rho*cos($l_lat*$radian)*cos($c)-($y*sin($l_lat*$radian)*sin($c))))*$degree;# longitude
            }# end if-else
            $l_time = $time;
            $l_uwind = $uwind;
            $l_vwind = $vwind;
            $l_lon = $input[10];
            $l_lat = $input[11];
            $radius = $radius + $input[14] - $l_alt;# increasing altitude
            $l_alt = $input[14];
          }#end if-else
        }# end if
        &printer(@input);
      }# end if-else
      $i++;
    }# end while <FILE>
  }else{
    print "$file doesn't end with .cls ext\n";
  }# if-else
  #print "FINI\n";
  close(OUT);
}# end foreach @ARGV

sub printer{
    printf OUT ("%6.1f %6.1f %5.1f %5.1f %5.1f %6.1f %6.1f %5.1f %5.1f %5.1f %8.3f %7.3f %5.1f %5.1f %7.1f %4.1f %4.1f %4.1f %4.1f %4.1f %4.1f\n",@_[0..20]);
}# end sub printer

