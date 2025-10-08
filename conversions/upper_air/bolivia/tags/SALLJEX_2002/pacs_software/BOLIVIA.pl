#!/bin/perl -w
# program BOLIVIA.pl January 17th, 2001
# Darren R. Gallant JOSS
use POSIX;
use lib "/home/gallant/bin/perl";
use Formats::Class qw(:DEFAULT &calc_UV &calc_dewpt &stereographic &check_obs %mand_press);
use FileHandle;
@months = keys %MONTHS;
@mandatory = keys %mand_press;
for($i=0;$i<scalar(@mandatory);$i++){$mandatory[$i]=$mandatory[$i].".0";}
$TRUE = $CONSTANTS{"TRUE"};$FALSE = $CONSTANTS{"FALSE"};
if(@ARGV < 1){
  print "Usage is BOLIVIA.pl file(s)\n";
  exit;
}
@files = grep(/\d{8,10}\.dat(|\.gz)$/i,@ARGV);
$TIME = $FALSE;if(grep(/nominal/i,@ARGV)){$TIME = $TRUE;}
$POSITION = $FALSE;if(grep(/position/i,@ARGV)){$POSITION = $TRUE;}
$LAT = -17.66;$LON = -63.12;$ALT = 373.0;
$logfile = "/work/PACS_SND/bolivia_raobs/SOUNDINGS.log";
$log = FileHandle->new();
open($log,">>$logfile") || die "Can't open $logfile\n";
print "Opening file: $logfile\n";&timestamp;
@timestamps = ();
foreach $file(sort @files){
    if($file =~ /(\d{8,10})\.dat/i){
        $TIMESTAMP = $1;push(@timestamps,$TIMESTAMP);
        $INFO{$TIMESTAMP}{"Location"} = $FALSE;
        $INFO{$TIMESTAMP}{"Input File"} = $file;
        $INFO{$TIMESTAMP}{"Comments"} = "";
        $fh = FileHandle->new();
	if($file =~ /\.gz/){
	    open($fh,"gzcat $file|") || die "Can't open $file\n";
	}else{
	    open($fh,$file) || die "Can't open $file\n";
	}# end if-else
        print "Opening file: $file\n";
	print $log "Opening file: $file\n";
    }# end if
    $l_time = 9999.0;$l_alt = 99999.0;
    @DATA = $fh->getlines;undef $fh;
    foreach $line(@DATA){
	if($line =~ /Launch Time/ && !exists($INFO{$TIMESTAMP}{"FILE"})){
	    &filename($line);
        }#end if
	unless(grep(/Launch Time/,@DATA) || exists($INFO{$TIMESTAMP}{"FILE"})){
	    &filename($TIMESTAMP);
        }# end unless
	if($line=~/(\d):(\d{2}):(\d{2}),(\d{3,5}),(\d{2,4}\.\d{1,2}|\d{2,4}),(\-\d{1,3}\.\d+|\d{1,3}\.\d+|\d{1,3}|\-\d{1,3}),(\d{1,2}),(\-\d{1,3}\.\d+|\d{1,3}\.\d+|\d{1,3}|\-\d{1,3}),(\d{1,3}|\/{4}),(\d{1,3}\.\d|\d{1,3}|\/{4})/){
	    @outline = &init_line;
	    #print " high res $line";
	    ($hour,$min,$sec,$height,$press,$temp,$rh,$dewpt,$wdir,$wspd)= ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10);
            unless($sec!~/(|\-)\d+/ || $min!~/(|\-)\d+/ || $hour!~/(|\-)\d+/){
		if($min != -1 && $sec != -1){
                    #printf("%02d%s%02d%s%02d\n",$hour,":",$min,":",$sec);
		    $outline[$field{"time"}] = $hour*3600+$min*60.0+$sec;
		}else{
		    $outline[$field{"time"}] = $MISS{time};
		}# end if-else
	    }else{
                $outline[$field{"time"}] = $MISS{time};
            }# end unless-else
	    #time in seconds
	    @outline = &fill_press(\@outline,\$press);
	    unless($temp == 0 && $rh == 0 && $dewpt == 0){
		@outline = &fill_temp(\@outline,\$temp);
		@outline = &fill_RH(\@outline,\$rh,\$dewpt);
            }# end unless
	    @outline = &fill_WIND(\@outline,\$wspd,\$wdir);#print "wspd= ",$wspd," wdir=",$wdir,"\n";
	    if($height =~ /\d+/){$outline[$field{"Alt"}] = $height;}# geopotential altitude in gpm
		
	    if($outline[$field{"time"}] != 9990.0 && $outline[$field{"Alt"}] != 99999.0){
		    
		$outline[$field{"Wcmp"}]=&calc_w($outline[$field{"Alt"}],$l_alt,$outline[$field{"time"}],$l_time);
		if($outline[$field{"Wcmp"}]!=999.0){$outline[$field{"Qdz"}] = 99.0;} # unchecked Qdz
		$l_time = $outline[$field{"time"}];$l_alt = $outline[$field{"Alt"}];
	    }# end if    
	    if($outline[$field{"time"}] == 0.0){
		$outline[$field{Lat}] = $LAT;
		$outline[$field{Lon}] = $LON;
		$outline[$field{Alt}] = $ALT;
	    }# end if
	    if($outline[$field{"time"}] != $MISS{"time"}){
		if(!grep(/$outline[$field{"time"}]/,keys %{$metdata{$TIMESTAMP}})){
		    
		    $metdata{$TIMESTAMP}{$outline[$field{"time"}]}=&line_printer(@outline);
		    #print $metdata{$TIMESTAMP}{$outline[$field{"time"}]};
		}# end if
	    }elsif($outline[$field{"Alt"}] != $MISS{"Alt"}){
		if(!grep(/^$outline[$field{"Alt"}]$/,keys %{$altdata{$TIMESTAMP}})){
		    
		    $altdata{$TIMESTAMP}{$outline[$field{"Alt"}]}=&line_printer(@outline);
                }# end if
            }elsif($outline[$field{"press"}] != $MISS{"press"}){
		if(!grep(/^$outline[$field{"press"}]$/,keys %{$pressdata{$TIMESTAMP}})){
		    
		    $pressdata{$TIMESTAMP}{$outline[$field{"press"}]}=&line_printer(@outline);
                }# end if
            }# end if-elsif(3)
	 }elsif($line=~/(\d):(\d{2}):(\d{2}),(\d{3,5}|,\d{3,5}|\d{1,2},\d{1,3}|\/{4}),(\d+\.\d+|\d+|\{4}).(\-(?:,)\d+\.\d+|\-(?:,)\d+|\-\d+|\d+|\/{4}),(\d{1,3}|\/{4}),(\-\d+\.\d+|\d+\.\d+|\-\d+|\d+|\/{4}),(\d{1,3}|\/{4}),(\d+\.\d|\d+|\/{4})/){
         #}elsif($line=~/(\d):(\d{2}):(\d{2}),+(\d{3,5}|\d{1,2},\d{1,3}),(\d+\.\d+|\d+),((?:\-|)\d+\.\d+|(?:\-|)\d+),(\d{1,3}),((?:\-|)\d+\.\d+|(?:\-|)\d+),(\d{1,3}|\/{4}),(\d+\.\d+|\d+|\/{4})/){
	    @outline = &init_line;
	    #print " high res $line";
	    ($hour,$min,$sec,$height,$press,$temp,$rh,$dewpt,$wdir,$wspd)= ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10);$temp =~ s/,//cs;
	    #if($height =~ /,/){
		#($part1,$part2) = split(',',$height);
		#$height = $part1*1000+$part2;
            #}# end if
            unless($sec!~/(|\-)\d+/ || $min!~/(|\-)\d+/ || $hour!~/(|\-)\d+/){
		if($min != -1 && $sec != -1){
                    #printf("%02d%s%02d%s%02d\n",$hour,":",$min,":",$sec);
		    $outline[$field{time}] = $hour*3600+$min*60.0+$sec;
		}else{
		    $outline[$field{time}] = $MISS{time};
		}# end if-else
	    }else{
                $outline[$field{time}] = $MISS{time};
            }# end unless-else
	    #time in seconds
	    @outline = &fill_press(\@outline,\$press);
	    #unless($temp == 0 && $rh == 0 && $dewpt == 0){
		@outline = &fill_temp(\@outline,\$temp);
		@outline = &fill_RH(\@outline,\$rh,\$dewpt);
            #}# end unless
	    @outline = &fill_WIND(\@outline,\$wspd,\$wdir);#print "wspd= ",$wspd," wdir=",$wdir,"\n";
	    if($height =~ /\d+/){$outline[$field{Alt}] = $height;}# geopotential altitude in gpm
		
	    if($outline[$field{time}] != 9990.0 && $outline[$field{"Alt"}] != 99999.0){
		    
		$outline[$field{Wcmp}]=&calc_w($outline[$field{"Alt"}],$l_alt,$outline[$field{"time"}],$l_time);
		if($outline[$field{Wcmp}]!=999.0){$outline[$field{"Qdz"}] = 99.0;} # unchecked Qdz
		$l_time = $outline[$field{time}];$l_alt = $outline[$field{Alt}];
	    }# end if    
	    if($outline[$field{time}] == 0.0){
		$outline[$field{Lat}] = $LAT;
		$outline[$field{Lon}] = $LON;
	    }# end if
	    if($outline[$field{time}] != $MISS{time}){
		if(!grep(/$outline[$field{time}]/,keys %{$metdata{$TIMESTAMP}})){
		    
		    $metdata{$TIMESTAMP}{$outline[$field{"time"}]}=&line_printer(@outline);
		    #print $metdata{$TIMESTAMP}{$outline[$field{"time"}]};
		}# end if
	    }elsif($outline[$field{"Alt"}] != $MISS{"Alt"}){
		if(!grep(/^$outline[$field{"Alt"}]$/,keys %{$altdata{$TIMESTAMP}})){
		    
		    $altdata{$TIMESTAMP}{$outline[$field{"Alt"}]}=&line_printer(@outline);
                }# end if
            }elsif($outline[$field{"press"}] != $MISS{"press"}){
		if(!grep(/^$outline[$field{"press"}]$/,keys %{$pressdata{$TIMESTAMP}})){
		    
		    $pressdata{$TIMESTAMP}{$outline[$field{"press"}]}=&line_printer(@outline);
                }# end if
            }# end if-elsif(3)
	 #}elsif($line=~/(\d{2}):(\d{2}):(\d{2})\.000[ ]+(\d{3,5})[ ]+(\d{2,4}\.\d{2})[ ]+(\-\d{1,3}\.\d{2}|\d{1,3}\.\d{2})[ ]+(\d{1,3})[ ]+(\-\d{1,3}\.\d{2}|\d{1,3}\.\d{2})[ ]+(\d{1,3}|\/{4})[ ]+(\d{1,3}\.\d|\/{4})/){
         }elsif($line=~/(\d):(\d{2}):(\d{2}),(\d{3,5}|\/{4}),(\d{2,4}\.\d{1,2}),(\-\d{1,3}\.\d{1,2}|\d{1,3}\.\d{1,2}),(\d{1,3}),(\-\d{1,3}\.\d{1,2}|\d{1,3}\.\d{1,2}),(\d{1,3}|\/{4}),(\d{1,3}\.\d+|\d{1,3}|\/{4})/){
	    @outline = &init_line;
	    #print " high res $line";
	    ($hour,$min,$sec,$height,$press,$temp,$rh,$dewpt,$wdir,$wspd)= ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10);
            unless($sec!~/(|\-)\d+/ || $min!~/(|\-)\d+/ || $hour!~/(|\-)\d+/){
		if($min != -1 && $sec != -1){
                    #printf("%02d%s%02d%s%02d\n",$hour,":",$min,":",$sec);
		    $outline[$field{"time"}] = $hour*3600+$min*60.0+$sec;
		}else{
		    $outline[$field{"time"}] = $MISS{time};
		}# end if-else
	    }else{
                $outline[$field{"time"}] = $MISS{time};
            }# end unless-else
	    #time in seconds
	    @outline = &fill_press(\@outline,\$press);
	    unless($temp == 0 && $rh == 0 && $dewpt == 0){
		@outline = &fill_temp(\@outline,\$temp);
		@outline = &fill_RH(\@outline,\$rh,\$dewpt);
            }# end unless
	    @outline = &fill_WIND(\@outline,\$wspd,\$wdir);#print "wspd= ",$wspd," wdir=",$wdir,"\n";
	    if($height =~ /\d+/){$outline[$field{"Alt"}] = $height;}# geopotential altitude in gpm
		
	    if($outline[$field{"time"}] != 9990.0 && $outline[$field{"Alt"}] != 99999.0){
		    
		$outline[$field{"Wcmp"}]=&calc_w($outline[$field{"Alt"}],$l_alt,$outline[$field{"time"}],$l_time);
		if($outline[$field{"Wcmp"}]!=999.0){$outline[$field{"Qdz"}] = 99.0;} # unchecked Qdz
		$l_time = $outline[$field{"time"}];$l_alt = $outline[$field{"Alt"}];
	    }# end if    
	    if($outline[$field{"time"}] == 0.0){
		$outline[$field{Lat}] = $LAT;
		$outline[$field{Lon}] = $LON;
		$outline[$field{Alt}] = $ALT;
	    }# end if
	    if($outline[$field{"time"}] != $MISS{"time"}){
		if(!grep(/$outline[$field{"time"}]/,keys %{$metdata{$TIMESTAMP}})){
		    
		    $metdata{$TIMESTAMP}{$outline[$field{"time"}]}=&line_printer(@outline);
		    #print $metdata{$TIMESTAMP}{$outline[$field{"time"}]};
		}# end if
	    }elsif($outline[$field{"Alt"}] != $MISS{"Alt"}){
		if(!grep(/^$outline[$field{"Alt"}]$/,keys %{$altdata{$TIMESTAMP}})){
		    
		    $altdata{$TIMESTAMP}{$outline[$field{"Alt"}]}=&line_printer(@outline);
                }# end if
            }elsif($outline[$field{"press"}] != $MISS{"press"}){
		if(!grep(/^$outline[$field{"press"}]$/,keys %{$pressdata{$TIMESTAMP}})){
		    
		    $pressdata{$TIMESTAMP}{$outline[$field{"press"}]}=&line_printer(@outline);
                }# end if
            }# end if-elsif(3)
	 }# end if 
    }# end while loop
    @DATA = ();
}# end foreach file
foreach $time (sort {$a<=>$b} @timestamps){
    if(scalar(keys %{$metdata{$time}}) || scalar(keys %{$altdata{$time}}) || scalar(keys %{$pressdata{$time}})){
	#print "TIMESTAMP $time\n";
	$outfile = $INFO{$time}{"FILE"};
        $out = FileHandle->new();
        open($out,">$outfile") || die "Can't open $outfile\n";
	print "Class file:$outfile\n";
        &init_header($time);
        foreach $header_line(&writeheader){ print $out $header_line;}
        &writefile(\$time,\$out,\$outfile);
    }# end if
}# end foreach $time
&timestamp;undef $log;print "FINI\n";

sub filename{
    my(@input,$dd,$abrev,$found,$mth,$MM,$mm,$year,$hh,$min,$outfile,$nominal,$LEAP,$month);
    my $monthdays = 0;
    my $stn_id = "SACZ";my $i = 0;
    $LEAP = $FALSE;$month = "NULL";
    $year = -1;$dd = -1;$abrev = -1;$hh = -1;$min = -1;
    if(!grep(/$TIMESTAMP/,keys %TIMES)){
        if($_[0] =~ /Launch Time:(?:\s|,)(\d{1,2})(?:\s|\-|,)(\w{3})(?:\s|\-|,)(\d{4}|\d{2},\d{2})(?:\s|,)at(?:\s|,)(\d{2}):(\d{2})/i){
	    ($dd,$abrev,$year,$hh,$min) = ($1,$2,$3,$4,$5);
	    $year =~ s/,//gs;
        }elsif($_[0] =~ /Launch Time:\s+(\d{1,2})\s+(\w{3})\s+(\d{4})\s+at\s+(\d{2}):(\d{2})/i){
	    ($dd,$abrev,$year,$hh,$min) = ($1,$2,$3,$4,$5);
	}elsif($_[0] =~ /(\d{2})(\d{2})(\d{2})(\d{2})(\d{2})/){
	    ($year,$MM,$dd,$hh,$min) = ($1,$2,$3,$4,$5);
	    $year = "19$year";
            $found = $FALSE;
	    while(!$found && defined($months[$i])){
		$month = $months[$i];
		$abrev = $MONTHS{$month}{ABREV};
		if($MM eq $MONTHS{$month}{MM}){
		    $found = $TRUE;
	        }else{$i++;}
	    }# end while loop
        }elsif($_[0] =~ /(\d{2})(\d{2})(\d{2})(\d{2})/){
	    ($MM,$dd,$hh,$min) = ($1,$2,$3,$4,$5);
	    $year = "1999";
            $found = $FALSE;
	    while(!$found && defined($months[$i])){
		$month = $months[$i];
		$abrev = $MONTHS{$month}{ABREV};
		if($MM eq $MONTHS{$month}{MM}){
		    $found = $TRUE;
	        }else{$i++;}
	    }# end while loop
        }# end if
	#print "$dd $abrev $year $hh $min\n";
        unless($year == -1 || $dd == -1 || $abrev =~ /\d/ || $hh == -1 || $min == -1){
	$found = $FALSE;$abrev = lc($abrev);$i=0;
        if($month eq "NULL"){
	    while(!$found && defined($months[$i])){
		$month = $months[$i];
		if($abrev =~ /$MONTHS{$month}{"ABREV"}/i){
		    $found = $TRUE;
		}else{$i++;}
	    }# end while loop
	}# end if
        $mth = $MONTHS{$month}{"MM"};
	if(($year%4==0) && (($year%100)||($year%400==0))){$LEAP = $TRUE;}
        #print "$dd $mth $year $hh:$min\n";
	if($LEAP && $month eq "february"){
	    $monthdays = $MONTHS{$month}{"LEAP"};
        }else{
	    $monthdays = $MONTHS{$month}{"DAYS"};
        }# end if
	if($dd > 0 && $dd <= $monthdays){
	    if($dd < 10 && length($dd) < 2){$dd = "0$dd";}
	    if($hh < 10 && length($hh) < 2){$hh = "0$hh";}
	    if($min < 10 && length($min) < 2){$min = "0$min";}
	    #$TIMESTAMP = substr($year,2,2).$mth.$dd.$hh;
	    #print "timestamp $TIMESTAMP\n"; 
	    $INFO{$TIMESTAMP}{"GMT"} = "$year, $mth, $dd, $hh:$min:00";
	    $outfile = $stn_id.$MONTHS{$month}{"FILE"}.$dd;
	    $INFO{$TIMESTAMP}{"FILE"} = $outfile.$hh.$min.".cls";
	    if($hh == 0){
		$nominal = "00:00:00";
	    }elsif($hh > 0 && $hh <= 3){
		$nominal = "03:00:00";
	    }elsif($hh > 3 && $hh <= 6){
		$nominal = "06:00:00";
	    }elsif($hh > 6 && $hh <= 9){
		$nominal = "09:00:00";
	    }elsif($hh > 9 && $hh <= 12){
		$nominal = "12:00:00";
	    }elsif($hh > 12 && $hh <= 15){
		$nominal = "15:00:00";
	    }elsif($hh > 15 && $hh <= 18){
		$nominal = "18:00:00";
	    }elsif($hh > 18 && $hh <= 21){
		$nominal = "21:00:00";
	    }else{
		$nominal = "00:00:00";$dd++;
		if($dd > $monthdays){
		    $dd = 1;$month = $MONTHS{$month}{"NEXT"};
		    if($MONTHS{$month}{"MM"} eq "01"){$year++;}#end if
		}#end if
	    }#end if-elsif(6)-else
	    if($dd < 10 && length($dd) < 2){$dd = "0$dd";} 
	    $mm = $MONTHS{$month}{"FILE"};
            $mth = $MONTHS{$month}{"MM"};
	    $INFO{$TIMESTAMP}{"NOMINAL"} = $INFO{$TIMESTAMP}{"GMT"};
	    if($TIME){
                $INFO{$TIMESTAMP}{"NOMINAL"} = "$year, $mth, $dd, $nominal";
		$outfile = $stn_id.$mm.$dd;
		$INFO{$TIMESTAMP}{"FILE"} = $outfile.(substr($nominal,0,2)).".cls";print $outfile,"\n";
	    }# end if
        }# end if
        }# end unless
    }# end if
}# end sub filename

sub knots2meters{
    my $spd = $_[0];
    return $spd*0.514444444;
}

sub init_header{
    my $time = $_[0];
    my ($stn_name,$stn_id) = ("Santa Cruz ,Bolivia","SACZ");
    $INFO{"Data Type"} = "High Resolution Sounding";
    $INFO{"Project ID"} = "PACS";
    $site = "$stn_name $stn_id";
    $INFO{"Release Site"} = $site;
    $INFO{"UTC"} = $INFO{$time}{"GMT"};
    $INFO{"Nominal"} = $INFO{$time}{"NOMINAL"};
    $INFO{"LAT"} = $LAT;
    $INFO{"LON"} = $LON;
    $INFO{"ALT"} = $ALT;
    $INFO{"Input File"} = $INFO{$time}{"Input File"};
    $INFO{"Comments"} = $INFO{$time}{"Comments"};
}# end sub init_header

sub writefile{
    my ($time_ref,$dh_ref,$file_ref) = @_;
    my $timestamp = $$time_ref,my $dh = $$dh_ref;my $time;
    my(@timeinput,@pressinput,@altinput,@input,$LIMIT);
    my(@ALTITUDE,@PRESSURE);
    my @OUTFILE = ();my @TMPFILE = ();
    if(defined(keys %{$metdata{$timestamp}})){
	foreach $time (sort {$a<=>$b} keys %{$metdata{$timestamp}}){
	    if(exists($metdata{$timestamp}{$time})){
		@input = split(' ',$metdata{$timestamp}{$time});
	    }# end if
	    unless($time >= 10000.0){push(@OUTFILE,&line_printer(@input));}
	}# end foreach
	undef %{$metdata{$timestamp}};
    }# end if
    if(scalar(keys %{$altdata{$timestamp}})){
	@TMPFILE = @OUTFILE;@OUTFILE = ();
	@ALTITUDE = sort ({ $a <=> $b } keys %{$altdata{$timestamp}});
	$LIMIT = scalar(@ALTITUDE)-1;$i = 0; 
        DATA:foreach $line(@TMPFILE){
	    @input = split(' ',$line);
	    if($LIMIT == -1 || $input[$field{"Alt"}] == $MISS{"Alt"} || $i > $LIMIT){
		push(@OUTFILE,&line_printer(@input));
	    }else{
		while($i <= $LIMIT){
		    $alt = $ALTITUDE[$i];
		    if(exists($altdata{$timestamp}{$alt})){
			if($alt < $input[$field{"Alt"}]){
			    push(@OUTFILE,&line_printer(split(' ',$altdata{$timestamp}{$alt})));$i++;
			}else{
			    push(@OUTFILE,&line_printer(@input));next DATA;
			}# end if-else
		    }else{
			push(@OUTFILE,&line_printer(@input));next DATA;
		    }# end if-else
		}# end while loop
		push(@OUTFILE,&line_printer(@input));
	    }# end if-else
	}# end foreach
	foreach $alt(@ALTITUDE[$i..$LIMIT]){
	    push(@OUTFILE,&line_printer(split(' ',$altdata{$timestamp}{$alt})));
	    $line_cnt++;
	}# end for loop
	undef %{$altdata{$timestamp}};
    }# end if
    if(scalar(keys %{$pressdata{$timestamp}})){
	@PRESSURE = sort ({ $b <=> $a } keys %{$pressdata{$timestamp}});
	$LIMIT = scalar(@PRESSURE)-1;$i = 0;
	@TMPFILE = @OUTFILE;@OUTFILE = ();
        DATA:foreach $line(@TMPFILE){
	    @input = split(' ',$line);
	    if($LIMIT == -1 || $input[$field{"press"}] == $MISS{"press"} || $i > $LIMIT){
		push(@OUTFILE,&line_printer(@input));
	    }else{
		while($i <= $LIMIT){
		    $press = $PRESSURE[$i];
		    if(exists($pressdata{$timestamp}{$press})){
			if($press >= $input[$field{"press"}]){
			    push(@OUTFILE,&line_printer(split(' ',$pressdata{$timestamp}{$press})));$i++;
			}else{
			    push(@OUTFILE,&line_printer(@input));next DATA;
			}# end if-else
		    }else{
			push(@OUTFILE,&line_printer(@input));next DATA;
		    }# end if-else
		}# end while loop
                push(@OUTFILE,&line_printer(@input));
	    }# end if-else
	}# end foreach
	foreach $press(@PRESSURE[$i..$LIMIT]){
	    push(@OUTFILE,&line_printer(split(' ',$pressdata{$timestamp}{$press})));
	    $line_cnt++;
        }# end for loop
	undef %{$pressdata{$timestamp}};
    }# end if
    if($POSITION){
	&stereographic(\@OUTFILE,\$dh);
    }else{
	foreach $line (@OUTFILE){
	    @input = split(' ',$line);
	    &printer(\@input,\$dh);
        }# end foreach
    }# end if
    printf("%s %5d %s\n","File contains",scalar(@OUTFILE),"lines");
    printf $log ("%15s %s %5d %s\n",$$file_ref,"contains",scalar(@OUTFILE),"lines");
}# end sub writefile

sub fill_press{
    my($array_ref,$press_ref) = @_;
    #print "press $$press_ref\n";
    if($$press_ref !~ /\d+/){
	return @{$array_ref};
    }elsif($$press_ref =~ /\d+\.\d|\d+/){
	@{$array_ref}[$field{"press"}] = $$press_ref; #pressure in mb
	@{$array_ref}[$field{"Qp"}] = 99; # unchecked Qp
        return @{$array_ref};
    }# end if
}# end sub fill_press

sub fill_temp{
    my($array_ref,$temp_ref) = @_;
    #print "temp $$temp_ref\n";
    if($$temp_ref !~ /\d+/){
	return @{$array_ref};
    }elsif($$temp_ref =~ /\d+\.\d+|\-\d+\.\d+|\d+/){
	if($$temp_ref < 1000.0 && $$temp_ref > -99.9 && $$temp_ref != 999.9){
	    @{$array_ref}[$field{"temp"}] = $$temp_ref; # temperature in Celsius
	    @{$array_ref}[$field{"Qt"}] = 99.0;# unchecked Qt
	}# end if
        return @{$array_ref};
    }# end if
}# end sub fill_temp

sub fill_RH{
    my($array_ref,$rh_ref,$dewpt_ref) = @_;
    my @outline = &init_line;
    if($$rh_ref !~ /\d+/ || $$dewpt_ref !~ /\d+/){
	 return @{$array_ref};
    }elsif($$rh_ref =~ /\d{1,3}/ && $$dewpt_ref =~ /(|\-)\d{1,3}\.\d+|\d+/){
        if($$rh_ref >= 0.0 && $$rh_ref <= 100.0){
	    @{$array_ref}[$field{"RH"}] = $$rh_ref; # relative humidity in percent
	    @outline = &calc_dewpt(@{$array_ref});
	    if($outline[$field{dewpt}]<999.9&&$outline[$field{dewpt}]>=-99.9){
		$outline[$field{"Qrh"}] = 99.0;# unchecked Qrh
	    }# end if
	    return @outline;
	}else{
	    if($$rh_ref > 100.0 && $$rh_ref != $MISS{RH}){
		@{$array_ref}[$field{"RH"}] = $$rh_ref; # relative humidity in percent
		@{$array_ref}[$field{"Qrh"}] = 2.0;# Questionable Qrh           
	        @{$array_ref}[$field{"dewpt"}] = @{$array_ref}[$field{"temp"}];# set DewPt = Temp
            }# end if
	    return @{$array_ref};
        }# end if-else
    }# end if
    return @{$array_ref};
}# end sub fill_RH

sub fill_WIND{
    my($array_ref,$wspd_ref,$wdir_ref) = @_;
    #print "wind $$wspd_ref $$wdir_ref\n";
    if($$wspd_ref !~ /\d+/ || $$wdir_ref !~ /\d+/){
	return @{$array_ref};
    }elsif($$wspd_ref =~ /\d+/ && $$wdir_ref =~ /\d+/){
        unless($$wdir_ref == 999 || $$wspd_ref == 99.9){
	    @{$array_ref}[$field{"Dir"}] = $$wdir_ref;@{$array_ref}[$field{"Spd"}] = $$wspd_ref;
	    unless($$wspd_ref == 0 && $$wdir_ref == 0){
		@{$array_ref} = &calc_UV(@{$array_ref});
	    }
        }# end unless
	return @{$array_ref};
    }else{
	return @{$array_ref};
    }# end if-else
}# end sub fill_WIND

sub check_missing{
    my $array_ref = $_[0];
    if($$array_ref[$field{"temp"}] != $MISS{"temp"}){
	return $TRUE;
    }# end if
    if($$array_ref[$field{"dewpt"}] != $MISS{"dewpt"} || $$array_ref[$field{"RH"}] != $MISS{"RH"}){
	return $TRUE;
    }# end if
    if($$array_ref[$field{"Ucmp"}] != $MISS{"Ucmp"} || $$array_ref[$field{"Vcmp"}] != $MISS{"Vcmp"}){
	return $TRUE;
    }# end if
    return $FALSE;
}# end if

sub timestamp{
    my($sec,$min,$hour,$mday,$mon,$year,$wday,$julian,$isdst) = gmtime(time);
    my $TIME = sprintf("%02d%s%02d%s%02d",$hour,":",$min,":",$sec);
    my $DATE = sprintf("%02d%s%02d%s%02d",$mon+1,"/",$mday,"/",substr($year+1900,2,2));
    print $log "UTC time and day $TIME $DATE\n";  
}# end sub timestamp





