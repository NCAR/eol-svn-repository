#!/bin/perl -w
# program LEARcls2QCF.pl January 10th,2002
# Perl script takes LEAR ATD class files and converts them to QCF
# Darren R. Gallant JOSS
use POSIX;
use lib "/home/gallant/bin/perl";
use Formats::Class qw(:DEFAULT);
use FileHandle;
$main_dir = ".";
@months = keys %MONTHS;
$TRUE = $CONSTANTS{"TRUE"};$FALSE = $CONSTANTS{"FALSE"};
$INFO{"Data Type"} = "High Resolution Dropsonde Sounding";
$STN_INFO{name} = "LEAR";
$stn_id = "LEAR";
#$STN_INFO{name} = "FALC";
#$stn_id = "FALC";
$logfile = "$main_dir/SOUNDINGS.log";
$log = FileHandle->new();
open($log,">>$logfile") || die "Can't open $logfile\n";
print "Opening file: $logfile\n";&timestamp;
if(@ARGV < 1){
  print "Usage is LEARcls2QCF.pl file(s)\n";
  exit;
}
@files = grep(/^D\d{8}\_\d{6}QC\.cls(|\.gz)$/i,@ARGV);
foreach $file(@files){
    print "Processing file $file\n";
    &writefile(&filename(\$file));
}# end foreach file
print "FINI\n";&timestamp;undef $log;

sub findmth{
    my $i = 0;
    while($i < scalar(@months)){
	if($MONTHS{$months[$i]}{"MM"} == $_[0]){
	    return $months[$i];
        }
	$i++;
    }
    return "NULL";
}# end sub findmth


sub by_number{$a<=>$b};

sub filename{
    my $file_ref = $_[0];my @OUTFILE = ();
    if($$file_ref =~ /^D(\d{4})(\d{2})(\d{2})\_(\d{2})(\d{2})(\d{2})QC\.cls/){
	($year,$mth,$day,$hour,$min,$sec) = ($1,$2,$3,$4,$5,$6);
        $mth = sprintf("%02d",$mth);
    }# end if-elsif
    $month = &findmth($mth);
    if($month ne "NULL"){
	if($day <= $MONTHS{$month}{"DAYS"}){
	    #$INFO{"Release Site"} = "$stn_id $STN_INFO{name}";
	    $outfile = $stn_id.$MONTHS{$month}{"FILE"}.$day;
            $outfile = $outfile.$hour.$min.".cls";
            if($hour == 0){
		$nominal = "00:00:00";
	    }elsif($hour > 0 && $hour <= 3){
		$nominal = "03:00:00";
	    }elsif($hour > 3 && $hour <= 6){
		$nominal = "06:00:00";
	    }elsif($hour > 6 && $hour <= 9){
		$nominal = "09:00:00";
	    }elsif($hour > 9 && $hour <= 12){
		$nominal = "12:00:00";
	    }elsif($hour > 12 && $hour <= 15){
		$nominal = "15:00:00";
	    }elsif($hour > 15 && $hour <= 18){
		$nominal = "18:00:00";
	    }elsif($hour > 18 && $hour <= 21){
		$nominal = "21:00:00";
	    }else{
		$nominal = "00:00:00";$day++;
		if($day > $MONTHS{$month}{"DAYS"}){
		    $day = 1;$month = $MONTHS{$month}{"NEXT"};
		    if($MONTHS{$month}{"MM"} eq "01"){$year++;}#end if
		}#end if
	    }#end if-elsif(6)-else
            $mm = $MONTHS{$month}{"FILE"};
            $NOMINAL = "$year, $mth, $day, $nominal";
            $out = FileHandle->new();    
	    open($out,">$outfile")||die "Can't open $outfile\n";
	    print "Class file:$outfile\n";
            @OUTFILE = &getheader(\$$file_ref);
            
	}# end if
   }# end if
   return @OUTFILE;
}#end sub filename

sub getheader{
    my $file_ref = $_[0];my @OUTFILE = ();my @INFILE = ();my @input = ();
    my $data = FileHandle->new();
    if($$file_ref =~ /\.gz/){
	open($data,"gzcat $$file_ref|") || die "Can't open $$file_ref\n";
    }else{
	open($data,$$file_ref) || die "Can't open $$file_ref\n";
    }# end if
    @INFILE = $data->getlines;undef $data;
    $INFO{"Input File"} = $$file_ref;
    for $line(@INFILE){
	if($line =~ /Launch Location.+:[ ]+(.+)/){
	    @input = split(',',$1);
	    if(scalar(@input) == 5){
		$INFO{LON} = sprintf("%8.3f",$input[2]);
		$INFO{LAT} = sprintf("%7.3f",$input[3]);
		$INFO{ALT} = sprintf("%7.1f",$input[4]);
	    }else{
		($INFO{LON},$INFO{LAT},$INFO{ALT}) = split(' ',$input[2]);
            }# end if-else
        }elsif($line =~ /GMT Launch Time.+:[ ]+(.+)/){
	    $INFO{UTC} = $1;
        }elsif($line =~ /Launch Site Type\/Site ID:[ ]+(.+)/){
	    $INFO{"Release Site"} = $1;
	}elsif($line =~ /Project ID:[ ]+(.+)/){
	    $INFO{"Project ID"} = &trim($1);
        }elsif($line =~ /Data Type:[ ]+(.+)/){
	    #$INFO{"Data Type"} = $1;
        }elsif($line =~ /Sonde Id:[ ]+(.+)/){
	    $INFO{"Sonde Type"} = $1;
            #$INFO{"Manufacturer"} = "Vaisala";
        }elsif($line =~ /System Operator.+:[ ]+(.+)/){
	    $INFO{"System Operator"} = $1;
	}elsif($line =~ /(\-|)\d+\.\d+/ && $line !~ /[a-zA-Z\/]/){
	    push(@OUTFILE,&check_line($line));
        }# end if
	$line_cnt++;
    }# end foreach
    return @OUTFILE;
}# end sub get_header

sub writefile{
    my $line_cnt = 0;
    my @OUTFILE = @_;my @input = ();my ($time,$alt) = ($MISS{time},$MISS{Alt});
    foreach $line(&writeheader){print $out $line;}
    for $line(@OUTFILE){
	@input = split(' ',$line);
	if($input[$field{time}] >= 0 && $input[$field{time}] != $MISS{time}){
	    if($input[$field{Alt}] != $MISS{Alt}){
                if($time != $MISS{time} && $alt != $MISS{Alt}){
		    $input[$field{Wcmp}] = &CALC_W($input[$field{Alt}],$alt,$input[$field{time}],$time);
		    unless ($input[$field{Wcmp}] == $MISS{Wcmp}){$input[$field{Qdz}] = 99.0;}
                }# end if
		$time = $input[$field{time}];$alt = $input[$field{Alt}];
	    }# end if
        }# end if
	&printer(\@input,\$out);
	$line_cnt++;
    }# end foreach
    $line = sprintf("%15s %s %4d %s\n",$outfile,"contains",$line_cnt,"lines of data");print $line;print $log $line; 
    undef $out;undef(@OUTFILE);
}# end sub writefile

sub check_line{
    my @input = split(' ',$_[0]);my @outline = &init_line;
    for $i (2..14){
	if($input[$i] !~ /9{3,5}\.0/ && $i != 9){
	    $outline[$i] = $input[$i];
            #if($i == 1){$outline[15] = 99.0;}# unchecked Qp
	    if($i == 2){$outline[16] = 99.0;}# unchecked Qt
            if($i == 4){$outline[17] = 99.0;}# unchecked Qrh
            if($i == 5){$outline[18] = 99.0;}# unchecked Qu
            if($i == 6){$outline[19] = 99.0;}# unchecked Qv  
            if($i == 3){
		if($outline[$i] < -99.9){$outline[$i] = -99.9;}
            }# end if   
	}# end if
    }# end foreach
    for $i (0..1){
	unless($input[$i] == 9999.0){$outline[$i] = $input[$i];}
	if($i == 1){$outline[15] = 99.0;}# unchecked Qp
    }# end foreach 
    return &line_printer(@outline);
}# end sub check_line

sub timestamp{
    my($sec,$min,$hour,$mday,$mon,$year,$wday,$julian,$isdst) = gmtime(time);
    $TIME = sprintf("%02d%s%02d%s%02d",$hour,":",$min,":",$sec);
    $DATE = sprintf("%02d%s%02d%s%02d",$mon+1,"/",$mday,"/",substr($year+1900,2,2));
    print $log "UTC time and day $TIME $DATE\n";  
}# end sub timestamp

sub CALC_W{
    local ($alt,$l_alt,$time,$l_time) = @_;
    if($time != 9999.0 && abs($time - $l_time) > 0 && $time >= 0 && $l_time >= 0){
      $w = ($alt - $l_alt)/($time - $l_time);
      if($w > 999.9){
	  $w = 999.9;
      }elsif($w < -99.9){
	  $w = -99.9;
      }# end if-elseif
      return $w;
    }else{
      return 999.0;
    }# end if-else
}# end sub calc_w











