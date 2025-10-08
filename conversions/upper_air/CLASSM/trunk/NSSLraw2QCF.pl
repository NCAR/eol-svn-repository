#!/bin/perl -w
# program NSSLraw2QCF.pl June 6,2002
# Perl script takes NSSL raw glass files and converts them to QCF
# Darren R. Gallant JOSS
use POSIX;
use lib "/home/gallant/bin/perl";
use Formats::Class qw(:DEFAULT &calc_UV &calc_dewpt &remove_dropping &calc_alt &calc_WND);
use FileHandle;
$main_dir = "/work/1R/IHOP_SND/NSSL-CLASS";
$raw_file_dir = "$main_dir/raw";
@months = keys %MONTHS;
$TRUE = $CONSTANTS{"TRUE"};$FALSE = $CONSTANTS{"FALSE"};$ALT = $FALSE;$INTERP = $FALSE;
$INFO{"Data Type"} = "High Resolution Sounding";
$INFO{"Project ID"} = "IHOP 2002";
$stn_id="NSSL";
$release_site = "NSSL Mobile Class Sounding";
$logfile = "SOUNDINGS.log";
$log = FileHandle->new();
$CHECK{RTI} = 200;# 200 deg C/km Rapid temp Increase
$CHECK{Super}{ALOFT} = -30;# -30 deg C/km Super Adiabatic lapse rate
$CHECK{Super}{"Near Surface"} = -100;# -100 deg C/km Super Adiabatic lapse rate for
# Pressures above $p_limit
$p_limit = 875.0;# Pressure level for seperating Near Surface and Aloft Super Check 
$CHECK{Rapid_p} = 3.0;# 3 mb/s Rapid pressure change 
open($log,">>$logfile") || die "Can't open $logfile\n";
print "Opening file: $logfile\n";&timestamp;
if(@ARGV < 1){
    print "Usage is NSSLraw2QCF.pl file(s)\n";
    exit;
}
@files = grep(/^X\d{7}\.NSL(|\.gz)$/i,@ARGV);
if(grep(/\-alt/,@ARGV)){$ALT = $TRUE;}
if(grep(/\-10sec/,@ARGV)){$INTERP = $TRUE;}
foreach $file(@files){
    print "$file\n";
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
    my $file_ref = $_[0];my @OUTFILE = ();my @MERGEFILE = ();my @RAWDATA = ();
    my ($mth,$day,$hour,$min,$limit,$line,$i);my @wind = ();my @hires = ();
    if($$file_ref =~ /X(\d)(\d{2})(\d{2})(\d{2})\.NSL/){
	($mth,$day,$hour,$min) = ($1,$2,$3,$4);
        my $rawfile = join("","m",$mth,$day,$hour,$min,".nsl");
        if(-e "$raw_file_dir/$rawfile"){
	    print "$rawfile\n";
            my $link = "$raw_file_dir/$rawfile";
	    @RAWDATA = &getrawdata(\$link);
        }# end if
        $year = "2002";$mth = "0$mth";
        $sec = "00";
    }# end if-elsif
    $month = &findmth($mth);
    if($month ne "NULL"){
	if($day <= $MONTHS{$month}{"DAYS"}){
	    $INFO{"Release Site"} = $release_site;
	    $mergefile = $stn_id.$MONTHS{$month}{"FILE"}.$day;
            $mergefile = $mergefile.$hour.$min.".merge";
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
            $merge = FileHandle->new();
	    open($out,">$outfile")||die "Can't open $outfile\n";
	    open($merge,">$mergefile")||die "Can't open $mergefile\n";
	    print "Class file:$outfile\n";
            @OUTFILE = &getheader(\$$file_ref);
	}else{
	    print "$$file_ref contains incorrect timestamp\n";
            print $log "$$file_ref contains incorrect timestamp\n";
	}# end if
   }else{
       print "$$file_ref contains incorrect timestamp\n";
       print $log "$$file_ref contains incorrect timestamp\n";
   }# end if
   $limit = scalar(@OUTFILE)-1;$i = 0;
   HIRES:foreach $line(@RAWDATA){
       @hires = split(' ',$line);
       if($limit == 0 || $i > $limit){
	   push(@MERGEFILE,$line);
       }else{
	   while($i <= $limit){
	       @wind = split(' ',$OUTFILE[$i]);
	       #printf("%6.1f %6.1f\n",$hires[$field{time}],$wind[$field{time}]);
	       if($hires[$field{time}] == $wind[$field{time}]){
		   foreach $parem("Dir","Spd","Ucmp","Vcmp","Qu","Qv"){
		       $hires[$field{$parem}] = $wind[$field{$parem}];
                   }# end foreach
		   $i++;
		   push(@MERGEFILE,&line_printer(@hires));next HIRES;
               }elsif($wind[$field{time}] < $hires[$field{time}]){
		   push(@MERGEFILE,&line_printer(@wind));$i++;
               }else{
		   push(@MERGEFILE,$line);next HIRES;
               }# end if-elsif-else
           }# end while
       }#end if-else
   }# end foreach
   foreach $line(@OUTFILE[$i..$limit]){push(@MERGEFILE,$line);}# end foreach 
   return &calc_alt(@MERGEFILE);
   #return @RAWDATA;
}#end sub filename

sub getheader{
    my $file_ref = $_[0];my @OUTFILE = ();my @INFILE = ();my @input = ();
    my ($int_lon,$min_lon,$dir_lon,$int_lat,$min_lat,$dir_lat,$alt);
    my $data = FileHandle->new();
    if($$file_ref =~ /\.gz/){
	open($data,"gzcat $$file_ref|") || die "Can't open $$file_ref\n";
    }else{
	open($data,$$file_ref) || die "Can't open $$file_ref\n";
    }# end if
    @INFILE = $data->getlines;undef $data;
    $line = sprintf("%24s %s %4d %s\n",$$file_ref,"contains",scalar(@INFILE),"lines of data");print $line;print $log $line;
    $INFO{"Input File"} = $$file_ref;
    for $line(@INFILE){
	if($line =~ /Launch Location.+:[ ]+(.+)/){
	    @input = split(',',$1);
	    $INFO{LON} = sprintf("%8.3f",$input[2]);
	    if(scalar(@input) == 5){
		$INFO{LAT} = sprintf("%7.3f",$input[3]);
		$INFO{ALT} = sprintf("%7.1f",$input[4]);
	    }else{
		($INFO{LAT},$INFO{ALT}) = split(' ',$input[3]);
            }# end if-else
        }elsif($line =~ /GMT Launch Time.+:[ ]+(.+)/){
	    @input = split(',',$1);
            my $year = $input[0];
            my $mth = sprintf("%02d",$input[1]);
            my $day = sprintf("%02d",$input[2]);
            my($hh,$min,$sec) = split(':',$input[3]);
            $hh = sprintf("%02d",$hh);
            $min = sprintf("%02d",$min);
            $sec = sprintf("%02d",int($sec/10.));
	    $INFO{UTC} = "$year, $mth, $day, $hh:$min:$sec";
        }elsif($line =~ /Sonde Id:[ ]+(.+)/){
	    $INFO{"Sonde Type"} = $1;
        }elsif($line =~ /Met Processor\/Met Smoothing:[ ]+(.+)/){
	    $INFO{"Met Processor"} = $1;
        }elsif($line =~ /Winds Type\/Processor\/Smoothing:[ ]+(.+)/){
	    $INFO{"Winds Type"} = $1;
        }elsif($line =~ /System Operator\/Comments:[ ]+(.+)/){
	    $INFO{"System Operator"} = $1;
	}elsif($line =~ /(\-|)\d+\.\d+/ && $line !~ /[a-zA-Z\/]/){
	    @input = split(' ',$line);
	    unless($input[$field{time}] <= 0){
		foreach $parem(qw(press temp dewpt RH Alt)){
		    $input[$field{$parem}] = $MISS{$parem};
		}#end foreach
            }# end foreach
	    push(@OUTFILE,&check_line(&line_printer(@input)));
        }# end if
	$line_cnt++;
    }# end foreach
    return @OUTFILE;
}# end sub get_header

sub writefile{
    my $line_cnt = 0;my @NEWFILE = ();
    my @OUTFILE = @_;my @input = @before = @after = ();my ($lapse,$dT,$dz,$FIRST,$limit);
    $lapse = 999.0;my $WRITE;my $rap_p = 9999.0;
    my ($time,$alt,$press,$temp) = ($MISS{time},$MISS{Alt},$MISS{press},$MISS{temp});
    foreach $line(&writeheader){print $out $line;print $merge $line;}
    @OUTFILE = &remove_dropping(@OUTFILE);
    $FIRST = $TRUE;my $CONT;my $CALC;
    for $line(@OUTFILE){
        $WRITE = $FALSE;
	@input = split(' ',$line);
	if($input[$field{time}] >= 0 && $input[$field{time}] != $MISS{time}){
	    if($input[$field{press}] != $MISS{press} && $press != $MISS{press}){
		if($input[$field{time}] - $time > 0){
		    $rap_p = ($input[$field{press}]-$press)/($input[$field{time}] - $time);
		    if($FIRST){$FIRST = $FALSE;}
                }else{
		    $rap = 9999.0
		}# if-else
                #if($FIRST){$FIRST = $FALSE;}
	    }else{
		$rap_p = 9999.0;
            }# end if-else
	    if($input[$field{temp}] != $MISS{temp} && $temp != $MISS{temp} && $input[$field{Alt}] != $MISS{Alt} && $alt != $MISS{Alt}){
		$dT = $input[$field{temp}] - $temp;
		$dz = $input[$field{Alt}] - $alt;
		if($dz > 0){
		    $lapse = ($dT/$dz)*1000;
                }else{
		    $lapse = 999.0;
                }# end if-else
            }else{
		$lapse = 999.0;
            }# end if-else
        }# end if
	if($rap_p == 9999.0 && $FIRST){
	    $WRITE = $TRUE;
	    if($input[$field{time}] >= 0){
		$press = $input[$field{press}];
		$time = $input[$field{time}];
                $temp = $input[$field{temp}];
		$alt = $input[$field{Alt}];
            }# end if
        }elsif($rap_p != 9999.0 && $lapse != 999.0){
	    if($rap_p < 0 && abs($rap_p) < $CHECK{Rapid_p}){
		if($input[$field{press}] < $press){
		    if($press < $p_limit){
			$SUPER = $CHECK{Super}{ALOFT};
                    }else{
			$SUPER = $CHECK{Super}{"Near Surface"};
                    }#end if-else
		    if($lapse > $SUPER && $lapse < $CHECK{RTI}){ 
			$WRITE = $TRUE;
			$press = $input[$field{press}];
			$time = $input[$field{time}];
			$temp = $input[$field{temp}];
			$alt = $input[$field{Alt}];
                    }# end if
                }# end if
            }# end if
        }elsif($rap_p != 9999.0){
	    if($rap_p < 0 && abs($rap_p) < $CHECK{Rapid_p}){
		if($input[$field{press}] < $press){
		    $WRITE = $TRUE;
		    $press = $input[$field{press}];
		    $time = $input[$field{time}];
		    $temp = $input[$field{temp}];
		    $alt = $input[$field{Alt}];
                }# end if
            }# end if
	}elsif($input[$field{time}]%10 == 0 && int($input[$field{time}])==$input[$field{time}]){
	    $WRITE = $TRUE;
	}# end if-else
        #printf ("%6.1f %6.1f %10.5f %10.5f\n",@input[0..1],$lapse,$rap_p);
	&printer(\@input,\$merge);
	if($WRITE){
	    @input = &calc_WND(@input);
	    @input = split(' ',&check_line(&line_printer(@input)));
            if($ALT || $INTERP){
		push(@NEWFILE,&line_printer(@input));
            }else{
		&printer(\@input,\$out);
            }# end if-else
	    #printf ("%6.1f %6.1f %10.5f %10.5f\n",@input[0..1],$lapse,$rap_p);
	    $line_cnt++;
        }# end if
    }# end foreach
    @OUTFILE = ();
    if($INTERP){
	print "Interpolating\n";
        @OUTFILE = @NEWFILE;@NEWFILE = ();
	$limit = scalar(@OUTFILE);#print $limit,"\n";
	for($i=0;$i<$limit;$i++){
	    @input = split(' ',$OUTFILE[$i]);
	    foreach $parem(qw(press temp RH Ucmp Vcmp)){
		if($input[$field{$parem}] == $MISS{$parem}){
		    if($i > 0 && $i < $limit-1){
			$j = $i+1;@before = split(' ',$OUTFILE[$j]);$CONT = $TRUE;$CALC = $FALSE;
			while($CONT){
			    if($before[$field{time}] < $input[$field{time}] && $before[$field{$parem}] != $MISS{$parem}){
				$CONT = $FALSE;$CALC = $TRUE;
                            }else{
				$j--;
				if($j > 0){
				    @before = split(' ',$OUTFILE[$j]);
				}else{
				    $CONT = $FALSE;
                                }# end if-else
                            }# end if-else
			}# end while
			if($CALC){
			    $j = $i-1;@after = split(' ',$OUTFILE[$j]);$CONT = $TRUE;
			    while($CONT){
				if($after[$field{time}] > $input[$field{time}] && $after[$field{$parem}] != $MISS{$parem}){
				    $CONT = $FALSE;$CALC = $TRUE;
				}else{
				    $j++;
				    if($j < $limit){
					@after = split(' ',$OUTFILE[$j]);
				    }else{
					$CONT = $FALSE;$CALC = $FALSE;
                                    }# end if-else
                                }# end if-else
			    }# end while
                        }# end if
			if($CALC){
			    $input[$field{$parem}] = &interp($input[$field{time}],$before[$field{time}],$after[$field{time}],$before[$field{$parem}],$after[$field{$parem}],$MISS{$parem});
                        }# end if
                    } # end if
                }# end if
            }# end foreach $parem
	    @input = &calc_WND(@input);
	    @input = &calc_dewpt(@input);
	    @input = split(' ',&check_line(&line_printer(@input)));
	    if($ALT){
		push(@NEWFILE,&line_printer(@input));
            }else{
		&printer(\@input,\$out);
            }# end if-else
	}# end for loop
    }# end if
    if($ALT){
	$line_cnt = 0;
	print "Calculating altitudes\n";
	foreach $line(&calc_alt(@NEWFILE)){
	    @input = split(' ',$line);
	    if($input[$field{time}] < 0 || ($input[$field{time}]%10 == 0 && int($input[$field{time}])==$input[$field{time}])){
		&printer(\@input,\$out);$line_cnt++;
            }# end if
        }#end foreach loop
	$line = sprintf("%15s %s %4d %s\n",$outfile,"contains",$line_cnt,"lines of data");print $line;print $log $line;
        undef(@NEWFILE);
    }# end if
    undef $out;undef $merge;undef(@OUTFILE);undef(@NEWFILE);
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
            if($i == 2 || $i == 3){
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
    my $TIME = sprintf("%02d%s%02d%s%02d",$hour,":",$min,":",$sec);
    my $DATE = sprintf("%02d%s%02d%s%02d",$mon+1,"/",$mday,"/",substr($year+1900,2,2));
    print $log "UTC time and day $TIME $DATE\n";  
}# end sub timestamp


sub getrawdata{
    my $file_ref = $_[0];my @OUTFILE = ();
    my $fh = FileHandle->new();
    if($$file_ref =~ /\.gz/){
	open($fh,"gzcat $$file_ref|") || die "Can't open $$file_ref\n";
    }else{
	open($fh,$$file_ref) || die "Can't open $$file_ref\n";
    }# end if
    my @INFILE = $fh->getlines;undef $fh;
    foreach $line(@INFILE){
	my @outline = &init_line;
	if($line =~ /.+(\+\d+|\-\d+)[ ]+\+(\d+)[ ]+\+(\d+)[ ]+(\+\d+|\-\d+)[ ]+\+(\d+)/){
	    ($time,$press,$logp,$temp,$rh) = ($1,$2,$3,$4,$5);
	    $outline[$field{time}] = $time/10;
	    $outline[$field{press}] = $press/100;
	    $outline[$field{temp}] = $temp/100;
	    $outline[$field{RH}] = $rh/10;
	    @outline = &calc_dewpt(@outline);
	    unless($outline[$field{RH}] == $MISS{RH}){
		unless($outline[$field{Qrh}] == 4.0){
		    $outline[$field{Qrh}] = 99.0;#unchecked Qrh
		}# end unless
            }# end unless
            #printf("%6.1f %6.1f %6.1f %6.1f\n",$time,$press,$temp,$rh);
	    #print &line_printer(@outline);
	    push(@OUTFILE,&check_line(&line_printer(@outline)));
        }# end if
    }# end foreach
    return @OUTFILE;
}# end sub getrawdata


sub interp{
    my($x,$x0,$x1,$y0,$y1,$miss) = @_;
    my $dt = 0;$dt = $x0 - $x1;my $px = 0;
    if(abs($dt) > 0.0 && $y0 != $miss && $y1 != $miss){
	$px = ($x-$x1)*$y0/$dt + ($x-$x0)*$y1/-$dt;
    }else{
	$px = $miss;
    }#end if-else
    return $px;
}# end sub interp





