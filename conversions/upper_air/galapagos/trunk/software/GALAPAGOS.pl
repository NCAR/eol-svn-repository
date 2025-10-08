#!/bin/perl -w
# program GALAPAGOS.pl March 8th,1999
# Darren R. Gallant JOSS
use POSIX;
use lib "../lib";
use Formats::Class qw(:DEFAULT &calc_UV &calc_dewpt &stereographic &check_obs %mand_press);
use FileHandle;
$| = 1;

@months = keys %MONTHS;
@mandatory = keys %mand_press;
for($i=0;$i<scalar(@mandatory);$i++){$mandatory[$i]=$mandatory[$i].".0";}
$TRUE = $CONSTANTS{"TRUE"};$FALSE = $CONSTANTS{"FALSE"};
if(@ARGV < 1){
  print "Usage is GALAPAGOS.pl file(s)\n";
  exit;
}

$OUTDIR = "../output";
mkdir($OUTDIR) unless(-e $OUTDIR); 

@files = grep(/\d{10}\.dat(|\.gz)$/i,@ARGV);
$TIME = $FALSE;if(grep(/nominal/i,@ARGV)){$TIME = $TRUE;}
$POSITION = $FALSE;if(grep(/position/i,@ARGV)){$POSITION = $TRUE;}
@timestamps = ();
foreach $file(sort @files){
    if($file =~ /(\d{10})\.dat/i){
        $TIMESTAMP = $1;push(@timestamps,$TIMESTAMP);
        $INFO{$TIMESTAMP}{"Location"} = $FALSE;
        $INFO{$TIMESTAMP}{"Serial Number"} = $INFO{$TIMESTAMP}{"Software"} = "";
        $INFO{$TIMESTAMP}{"Comments"} = "";
        $fh = FileHandle->new();
	if($file =~ /\.gz/){
	    open($fh,"gzcat $file|") || die "Can't open $file\n";
	}else{
	    open($fh,$file) || die "Can't open $file\n";
	}# end if-else
        print "Opening file: $file\n";
    }# end if
    $l_time = 9999.0;$l_alt = 99999.0;
    @DATA = $fh->getlines;undef $fh;#print @DATA,"\n";
    foreach $line(@DATA){
	if($line =~ /Started at/ && !exists($INFO{$TIMESTAMP}{"FILE"})){
	    &filename($line);
        }#end if
	unless(grep(/(Started at|Stated at)/,@DATA)){
	    if($line =~ /Start Up Date/ && !exists($INFO{$TIMESTAMP}{"FILE"})){
		&filename($line);
	    }#end if
	}# end if
        if($line =~ /Stated at/ && !exists($INFO{$TIMESTAMP}{"FILE"})){
	    &filename($line);
        }#end if
	if($line =~ /Location/ && !$INFO{$TIMESTAMP}{"Location"}){
	    &location($line);
	}# end if
	if($line =~ /RS-number/i){
	    if($line =~ /(\d+)/){$INFO{$TIMESTAMP}{"Serial Number"} = $1;}
        }# end if
	if($line =~ /Sounding program (.*)/i){
	    $INFO{$TIMESTAMP}{"Software"} = $1;
	}
        if($line =~ /(\w{4,5}\.txt)/i){

	    $INFO{$TIMESTAMP}{"Comments"} = "Original Nexdata file: $1";
        }
        $INFO{$TIMESTAMP}{"Input File"} = $file;
        #if(length($line) < 80 && $line =~ /\d+?\.\d+?/ && $line !~ /[A-Za-z]/){
	    #$pos = length($line)-1;
            #chop($line);
	    #while(length($line) < 80){
		#$line = sprintf("%s%s",$line," ");
	    #}
	    #$line = sprintf("%s\n",$line);
        #}
	if($line =~ /\t+/){
	    $line =~ tr/a-zA-Z0-9\.\n\-/ /cs;
        }# end if
	#print length($line),"\n";
	if($line=~/^[ ]+(\d{1,3}|\-\d|[ ]{3})[ ]{3,4}(\d{1,2}|\-\d|[ ]{2})[ ]{6,7}(\d{1,2}\.\d|\-\d{1,2}\.\d|[ ]{4})[ ]{5,9}(\d{1,5}|[ ]{5})[ ]{5,7}(\d{1,4}\.\d|[ ]{6})[ ]{3,5}(\d{1,3}\.\d|\-\d{1,3}\.\d|[ ]{5})[ ]{3,5}(\d{1,3}|[ ]{3})[ ]{3,6}(\d{1,3}\.\d|\-\d{1,3}\.\d|[ ]{5})[ ]{3,5}(\d{1,3}|\/+|[ ]{3})[ ]{4,5}(\d{1,2}\.\d|\d{1,3}|\/+|[ ]{4})/){
	
	    @outline = &init_line;
	    #print " high res $line";
	    ($min,$sec,$ascrate,$height,$press,$temp,$rh,$dewpt,$wdir,$wspd)= ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10);
            unless($sec !~ /(|\-)\d+/ || $min !~ /(|\-)\d+/){
		if($min != -1 && $sec != -1){
		    $outline[$field{"time"}] = $min * 60.0 + $sec; 
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
		$outline[$field{"Lat"}] = $INFO{$TIMESTAMP}{"LAT"};#latitude
		$outline[$field{"Lon"}] = $INFO{$TIMESTAMP}{"LON"};#longitude
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
	 }elsif($line=~/^[ ]*(\d{1,3}|\-1)[ ]{1,2}(\d{1,2}|\-1)[ ]{3,5}(\d{1,2}\.\d|\-\d{1,2}\.\d|999\.9)[ ]+(\d{1,5}|[ ]{5})[ ]+(\d{1,4}\.\d|[ ]{6})[ ]{2,4}(\d{1,3}\.\d|\-\d{1,3}\.\d|999\.9)[ ]+(\d{1,3}|999)[ ]+(\d{1,3}\.\d|\-\d{1,3}\.\d|999\.9)[ ]+(\d{1,3}|\/+|999)[ ]+(\d{1,2}\.\d|\d{1,3}|\/+|999\.9)/){
	    @outline = &init_line;
	    #print " high res $line";
	    ($min,$sec,$ascrate,$height,$press,$temp,$rh,$dewpt,$wdir,$wspd)= ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10);
            unless($sec !~ /(|\-)\d+/ || $min !~ /(|\-)\d+/){
		if($min != -1 && $sec != -1){
		    $outline[$field{"time"}] = $min * 60.0 + $sec; 
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
		$outline[$field{"Lat"}] = $INFO{$TIMESTAMP}{"LAT"};#latitude
		$outline[$field{"Lon"}] = $INFO{$TIMESTAMP}{"LON"};#longitude
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

         }elsif($line=~/(\d{1,3}|\-\d)[ ]+(\d{1,2}|\-\d)[ ]+(\d{1,2}\.\d|\-\d{1,2}\.\d|\d+)[ ]+(\d{1,5})[ ]+(\d{1,4}\.\d)[ ]+(\d{1,3}\.\d|\-\d{1,3}\.\d)[ ]+(\d{1,3})[ ]+(\d{1,3}\.\d|\-\d{1,3}\.\d)/){
	    @outline = &init_line;
	    #print " high res thermo $line";
	    ($min,$sec,$ascrate,$height,$press,$temp,$rh,$dewpt)= ($1,$2,$3,$4,$5,$6,$7,$8);

            unless($sec !~ /(|\-)\d+/ || $min !~ /(|\-)\d+/){
		if($min != -1 && $sec != -1){
		    $outline[$field{"time"}] = $min * 60.0 + $sec; 
		}else{
		    $outline[$field{"time"}] = $MISS{time};
		}# end if-else
	    }else{
                $outline[$field{"time"}] = $MISS{time};
            }# end unless-else
	    #time in seconds
	    @outline = &fill_press(\@outline,\$press);
	    @outline = &fill_temp(\@outline,\$temp);
	    @outline = &fill_RH(\@outline,\$rh,\$dewpt);
	    
	    if($height =~ /\d+/){$outline[$field{"Alt"}] = $height;}# geopotential altitude in gpm
		
	    if($outline[$field{"time"}] != 9990.0 && $outline[$field{"Alt"}] != 99999.0){
		    
		$outline[$field{"Wcmp"}]=&calc_w($outline[$field{"Alt"}],$l_alt,$outline[$field{"time"}],$l_time);
		if($outline[$field{"Wcmp"}]!=999.0){$outline[$field{"Qdz"}] = 99.0;} # unchecked Qdz
		$l_time = $outline[$field{"time"}];$l_alt = $outline[$field{"Alt"}];
	    }# end if    
	    if($outline[$field{"time"}] == 0.0){
		$outline[$field{"Lat"}] = $INFO{$TIMESTAMP}{"LAT"};#latitude
		$outline[$field{"Lon"}] = $INFO{$TIMESTAMP}{"LON"};#longitude
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
	 }elsif($line=~/^[ ]+(\d+|\-\d)[ ]+(\d+|\-\d)[ ]+(\d{1,5}|[ ]{5})[ ]+(\d{2,4}\.\d|[ ]{6})[ ]+(\d{1,2}\.\d|\-\d{1,2}\.\d|[ ]{4})[ ]+(\d{1,2}|[ ]{2})[ ]+(\d{1,2}\.\d|\-\d{1,3}\.\d|[ ]{3})[ ]+(T|U|)/){
	    @outline = &init_line;
            print "thermo $line";
	    ($min,$sec,$height,$press,$temp,$rh,$dewpt)= ($1,$2,$3,$4,$5,$6,$7);
	    unless($sec !~ /(|\-)\d+/ || $min !~ /(|\-)\d+/){
		if($min != -1 && $sec != -1){
		    $outline[$field{"time"}] = $min * 60.0 + $sec; 
		}else{
		    $outline[$field{"time"}] = $MISS{time};
		}# end if-else
	    }else{
                $outline[$field{"time"}] = $MISS{time};
            }# end unless-else
	    #time in seconds
	    @outline = &fill_press(\@outline,\$press);
	    @outline = &fill_temp(\@outline,\$temp);
	    @outline = &fill_RH(\@outline,\$rh,\$dewpt);
	    if($outline[$field{"time"}] == 0.0){
		$outline[$field{"Lat"}] = $INFO{$TIMESTAMP}{"LAT"};#latitude
		$outline[$field{"Lon"}] = $INFO{$TIMESTAMP}{"LON"};#longitude
	    }# end if
            if($height =~ /\d+/){
		$outline[$field{"Alt"}] = $height;# geopotential altitude in gpm
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
	 }elsif($line=~/^[ ]+(\d{1,2}|\-\d|[ ]{2})[ ]{3,4}(\d{1,2}|\-\d|[ ]{2})[ ]{3,7}(\d{1,5}|[ ]{5})[ ]{3,5}(\d{1,4}\.\d|[ ]{6})[ ]{3,5}(\d{1,3}|[ ]+|\/+|[ ]{3})[ ]{3,4}(\d{1,2}\.\d|\d{1,2}|\/+|[ ]{4})[ ]+(F|D)/){
	    @outline = &init_line;
	    #print "wind $line";
	    ($min,$sec,$height,$press,$wdir,$wspd)= ($1,$2,$3,$4,$5,$6,$7);
            unless($sec !~ /(|\-)\d+/ || $min !~ /(|\-)\d+/){
		if($min != -1 && $sec != -1){
		    $outline[$field{"time"}] = $min * 60.0 + $sec; 
		}else{
		    $outline[$field{"time"}] = $MISS{time};
		}# end if-else
	    }else{
                $outline[$field{"time"}] = $MISS{time};
            }# end unless-else
	    #time in seconds
	    @outline = &fill_press(\@outline,\$press);
	    @outline = &fill_WIND(\@outline,\$wspd,\$wdir);
	    if($outline[$field{"time"}] == 0.0){
	       $outline[$field{"Lat"}] = $INFO{$TIMESTAMP}{"LAT"};#latitude
	       $outline[$field{"Lon"}] = $INFO{$TIMESTAMP}{"LON"};#longitude
	    }# end if
	    if($height =~ /\d+/){
		$outline[$field{"Alt"}] = $height;# geopotential altitude in gpm
	    }# end if
            if($outline[$field{"time"}] != $MISS{"time"}){
		if(!grep(/$outline[$field{"time"}]/,keys %{$metdata{$TIMESTAMP}})){
		    
		    $metdata{$TIMESTAMP}{$outline[$field{"time"}]}=&line_printer(@outline);
		    #print $metdata{$TIMESTAMP}{$outline[$field{"time"}]};
                }else{
		    if(exists($metdata{$TIMESTAMP}{$outline[$field{"time"}]})){
			@input = split(' ',$metdata{$TIMESTAMP}{$outline[$field{"time"}]});
			foreach $elem("temp","dewpt","RH","Qt","Qrh"){
			    $outline[$field{$elem}] = $input[$field{$elem}];
			}# end foreach
			$metdata{$TIMESTAMP}{$outline[$field{"time"}]}=&line_printer(@outline);
		    }else{
			$metdata{$TIMESTAMP}{$outline[$field{"time"}]}=&line_printer(@outline);
		    }# end if
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
	 }elsif($line=~/^[ ]*(\d{2,4}\.\d|[ ]{5})[ ]{3,6}(\d{1,5}|[ ]{5})[ ]{3,5}(\d{1,3}\.\d|\-\d{1,3}\.\d|[ ]{5})[ ]{3,5}(\d{1,2}|[ ]{2})[ ]{3,5}(\d{1,3}\.\d|\-\d{1,3}\.\d|[ ]{5})[ ]{3,5}(\d{1,3}|[ ]{3}|\/+)[ ]{3,4}(\d{1,2}\.\d|\d{1,2}|\/+|[ ]{4})/){
	     @outline = &init_line;
             #print "mand $line";
	     ($press,$height,$temp,$rh,$dewpt,$wdir,$wspd)= ($1,$2,$3,$4,$5,$6,$7);
             if(grep(/$press/,@mandatory)){
	        @outline = &fill_press(\@outline,\$press);
	        @outline = &fill_temp(\@outline,\$temp);
		@outline = &fill_RH(\@outline,\$rh,\$dewpt);
		@outline = &fill_WIND(\@outline,\$wspd,\$wdir);
		if($height =~ /\d+/){
		    $outline[$field{"Alt"}] = $height;# geopotential altitude in gpm
                }# end if
	        if($outline[$field{"Alt"}] != $MISS{"Alt"}){
		    if(!grep(/^$outline[$field{"Alt"}]$/,keys %{$altdata{$TIMESTAMP}})){
			
			$altdata{$TIMESTAMP}{$outline[$field{"Alt"}]}=&line_printer(@outline);
		    }# end if
		}elsif($outline[$field{"press"}] != $MISS{"press"}){
		    if(!grep(/^$outline[$field{"press"}]$/,keys %{$pressdata{$TIMESTAMP}})){
			
			$pressdata{$TIMESTAMP}{$outline[$field{"press"}]}=&line_printer(@outline);
		    }# end if
		}# end if-elsif(2)
             }# end if
	 }# end if 
    }# end while loop
    @DATA = ();
#}# end foreach file

    foreach $time (sort {$a<=>$b} @timestamps){
	if(scalar(keys %{$metdata{$time}}) || scalar(keys %{$altdata{$time}}) || scalar(keys %{$pressdata{$time}})){
	    #print "TIMESTAMP $time\n";
	    $outfile = $INFO{$time}{"FILE"};
	    $out = FileHandle->new();
	    open($out,">$outfile") || die "Can't open $outfile\n";
	    print "Class file:$outfile\n";
	    &init_header($time);
	    foreach $header_line(&writeheader){ print $out $header_line;}
	    &writefile(\$time,\$out);
	    system("mv $outfile $OUTDIR/$outfile");
	    
	    delete($INFO{$time});
	    delete($metdata{$time});
	    delete($altdata{$time});
	    delete($pressdata{$time});
	}
    }
}
   
print "FINI\n";

sub location{
    my @input = ();my ($LON,$LAT,$ALT,$LON_DIR,$LAT_DIR);
    if(!grep(/$TIMESTAMP/,keys %TIMES)){
	@input = split(' ',$_[0]);
	if($_[0] =~ /Location.+(\d+\.\d+)\D+(\w|[ ])\D+(\d+\.\d+)\D+(\w|[ ])\D+(\d+)/i){
	    ($LAT,$LAT_DIR,$LON,$LON_DIR,$ALT) = ($1,$2,$3,$4,$5);

	    printf("Longitude: %s\n",$LON);

	    $INFO{$TIMESTAMP}{"LAT"} = $LAT;
	    if($LAT_DIR=~/S/i){
		$INFO{$TIMESTAMP}{"LAT"}=-$INFO{$TIMESTAMP}{"LAT"};
            }elsif($LAT_DIR eq " "){
		$LAT_DIR = "S";
                $INFO{$TIMESTAMP}{"LAT"}=-$INFO{$TIMESTAMP}{"LAT"};    
	    }# end if
	    $INFO{$TIMESTAMP}{"LON"} = $LON;
	    if($LON_DIR =~ /W/i){
		$INFO{$TIMESTAMP}{"LON"}=-$INFO{$TIMESTAMP}{"LON"};
            }elsif($LON_DIR eq " "){
		$LON_DIR = "W";
                $INFO{$TIMESTAMP}{"LON"}=-$INFO{$TIMESTAMP}{"LON"};
	    }#end if
	    $INFO{$TIMESTAMP}{"ALT"} = $ALT;
            $INFO{$TIMESTAMP}{"Location"} = $TRUE;
	}# end if
    }# end if
}# end sub location

sub filename{
    my(@input,$dd,$abrev,$found,$mth,$mm,$year,$hh,$min,$outfile,$nominal,$LEAP);
    my $monthdays = 0;
    my $stn_id = "SNCR";my $i = 0;
    $LEAP = $FALSE;
    $year = -1;$dd = -1;$abrev = -1;$hh = -1;$min = -1;

    if(!grep(/$TIMESTAMP/,keys %TIMES)){

        if($_[0] =~ /Started at:[ ]*(\d+).*(\w{3}).*(\d{2}).*(\d{2}).*(\d{2})/i){
	    ($dd,$abrev,$year,$hh,$min) = ($1,$2,$3,$4,$5);
	}elsif($_[0] =~ /Stated at:[ ]*(\d+).*(\w{3}).*(\d{2}).*(\d{2}).*(\d{2})/i){
	    ($dd,$abrev,$year,$hh,$min) = ($1,$2,$3,$4,$5);
	}elsif($_[0] =~ /Start Up Date[ ]*(\d+).*(\w{3}).*(\d{2}).*(\d{2}).*(\d{2})/i){
	    ($dd,$abrev,$year,$hh,$min) = ($1,$2,$3,$4,$5);
	}elsif($_[0] =~ /Started at:[ ]+(\d+).*(\w{3}).*(\d{2}).*(\d+).*(\d{2})/i){
	    ($dd,$abrev,$year,$hh,$min) = ($1,$2,$3,$4,$5);
        }# end if-elsif(2)
	#print "$dd $abrev $year $hh $min\n";
        unless($year == -1 || $dd == -1 || $abrev =~ /\d/ || $hh == -1 || $min == -1){
	if($year > 90){
	    $year = "19$year";
	}else{
	    $year = "20$year";
        }
        $found = $FALSE;$abrev = lc($abrev);
	while(!$found && defined($months[$i])){
		$month = $months[$i];
		if($abrev =~ /$MONTHS{$month}{"ABREV"}/i){
			$found = $TRUE;
		}else{$i++;}
	}# end while loop
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
#	    $INFO{$TIMESTAMP}{"FILE"} = $outfile.$hh.$min.".cls";
	    $INFO{$TIMESTAMP}{"FILE"} = sprintf("%s_%04d%02d%02d%02d%02d.cls",$stn_id,$year,$MONTHS{$month}{"MM"},$dd,$hh,$min);

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
		$outfile = $stn_id.$mm.$dd;print $outfile,"\n";
		$INFO{$TIMESTAMP}{"FILE"} = $outfile.(substr($nominal,0,2)).".cls";
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
    my ($stn_name,$stn_id) = ("San Cristobal","SNCR");
    $INFO{"Data Type"} = "High Resolution Sounding";
    $INFO{"Project ID"} = "PACS";
    $site = "$stn_name $stn_id 84008";
    $INFO{"Release Site"} = $site;
    $INFO{"UTC"} = $INFO{$time}{"GMT"};
    $INFO{"Nominal"} = $INFO{$time}{"NOMINAL"};
    $INFO{"LAT"} = $INFO{$time}{"LAT"};
    $INFO{"LON"} = $INFO{$time}{"LON"};
    $INFO{"ALT"} = $INFO{$time}{"ALT"};
    $INFO{"Input File"} = $INFO{$time}{"Input File"};
    $INFO{"Comments"} = $INFO{$time}{"Comments"};
    if(defined($INFO{$time}{"Serial Number"})){
	$INFO{"Serial Number"} = $INFO{$time}{"Serial Number"};
    }
    if(defined($INFO{$TIMESTAMP}{"Software"})){
	$INFO{"System Operator"} = $INFO{$TIMESTAMP}{"Software"};
    }
}# end sub init_header

sub writefile{
    my ($time_ref,$dh_ref) = @_;
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
}# end sub writefile

sub fill_press{
    my($array_ref,$press_ref) = @_;
    #print "press $$press_ref\n";
    if($$press_ref !~ /\d+/){
	return @{$array_ref};
    }elsif($$press_ref =~ /\d+\.\d/){
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
    }elsif($$temp_ref =~ /\d+\.\d|\-\d+\.\d/){
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
    }elsif($$rh_ref =~ /\d{1,2}/ && $$dewpt_ref =~ /(|\-)\d{1,3}\.\d/){
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







