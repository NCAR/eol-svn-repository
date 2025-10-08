#!/opt/bin/perl -w

#-----------------------------------------------
# make_stnlst.pl
#
# Reads final output *.0qc surface data files 
# and creates the 3 station *.out files.
#
# 23 Oct 02, ds
#-----------------------------------------------

$project_name    =  "GAPP/CEOP";
$frequency       = "hourly";
$accuracy        =  3;
$id_type         = 53;
$fixed_mobile    = 'f';
$commission_code = "(N)";
$country         = "US";
$dst             = 'n';
$time_zone       =  0.0;
$platform        =  264;
$station_count   =  1;

$station_id = "start";
$last_time = $first_time = 999999;

$surface_infile = "final/GAPP_SGP.cfr";
$station_list = "stn_info";
 
$station_out = "GAPP_SGP_station.out";
$CD_station_out = "GAPP_SGP_stationCD.out";
$stn_id_out = "GAPP_SGP_stn_id.out";

print "\nRunning make station list program...\n";

%state = ("AL"=>1, "AK"=>2, "AZ"=>4, "AR"=>5, "CA"=>6, "CO"=>8, "CT"=>9, "DE"=>10, "DC"=>11, "FL"=>12, "GA"=>13, "HI"=>15, "ID"=>16, "IL"=>17, "IN"=>18, "IA"=>19, "KS"=>20, "KY"=>21, "LA"=>22, "ME"=>23, "MD"=>24, "MA"=>25, "MI"=>26, "MN"=>27, "MS"=>28, "MO"=>29, "MT"=>30, "NE"=>31, "NV"=>32, "NH"=>33, "NJ"=>34, "NM"=>35, "NY"=>36, "NC"=>37, "ND"=>38, "OH"=>39, "OK"=>40, "OR"=>41, "PA"=>42, "RI"=>44, "SC"=>45, "SD"=>46, "TN"=>47, "TX"=>48, "UT"=>49, "VT"=>50, "VA"=>51, "WA"=>53, "WV"=>54, "WI"=>55, "WY"=>56, "XX"=>99);

#-----------------------------------------------
# Read stations file (sorted on lat/lon)
#-----------------------------------------------

open (STATIONS, $station_list) || die "Can't open $station_list";
@stationlist = <STATIONS>;										
close (STATIONS);
 
#-----------------------------------------------
# pull station IDs from stations array,
# and put in an array of station ids
#-----------------------------------------------

$j = 0;
@ids = ();
$items = @stationlist;
while ($j < $items) {
    $ids[$j] = (split(" ", $stationlist[$j]))[5];
	print "id = $ids[$j] \n";
    $j++;
}
$idsize = @ids;

#-----------------------------------------------
# open our input file
#-----------------------------------------------

open (SFCINFILE, $surface_infile) || die "Can't open $surface_infile";
 
#-----------------------------------------------
# start reading in the lines
#-----------------------------------------------

while ($sfc_line = <SFCINFILE>) {
   @line = split (" ", $sfc_line);
   $station_id = $line[6];                                      # get station ID from line
   ($year, $mon, $day) =  split("/", $line[0]);					# get our date elements, too
   $this_time = sprintf("%4d%02d%02d", $year, $mon, $day); 

   #-----------------------------------------------
   # Test to see if this is a new id to match.
   # get station IDs from list, one by one
   # if match, fill out station information 
   #-----------------------------------------------

   if (!defined($stn_info{$station_id}{"stnid_ext"})) {                           
	   $lat = $line[7];
	   $lon = $line[8];
   	   $j = $matched = 0;
       do {
          $id_tomatch = $ids[$j];                               		
          if ($station_id eq $id_tomatch) {                     		
			 print "station $station_id is matched on $j tries\n";
			 $matched = 1;
             $first_time =$this_time; 
             $last_time = $first_time;
             $temp_state_code = substr($stationlist[$j], 0, 2);
			 $long_name = &trim(substr($stationlist[$j], 3, 15));
             $stn_info{$station_id}{"project"} = $project_name;
             $stn_info{$station_id}{"stnid_ext"} = $station_id;
             $stn_info{$station_id}{"id_type"} = $id_type;
             $stn_info{$station_id}{"lat"} = $lat;
             $stn_info{$station_id}{"lon"} = $lon;
             $stn_info{$station_id}{"occur"} = 0;
             $stn_info{$station_id}{"accuracy"} = $accuracy;
             $stn_info{$station_id}{"name"} = $long_name.",".$temp_state_code;
             $stn_info{$station_id}{"comm_code"} = $commission_code;
             $stn_info{$station_id}{"begin_date"} = $first_time;
             $stn_info{$station_id}{"end_date"} = $last_time;
             $stn_info{$station_id}{"country"} = $country;
             $stn_info{$station_id}{"state"} = $state{$temp_state_code};
             $stn_info{$station_id}{"county"} = "???";
             $stn_info{$station_id}{"time_zone"} = $time_zone;
             $stn_info{$station_id}{"dst_switch"} = $dst;
             $stn_info{$station_id}{"platform"} = $platform;
             $stn_info{$station_id}{"frequency"} = $frequency;
             $stn_info{$station_id}{"elev"} = substr($stationlist[$j], 44, 5);
             $stn_info{$station_id}{"fixed_mobile"} = $fixed_mobile;
             $stn_info{$station_id}{"stnid_int"} = $station_count++;
          }
		  $j++;
          if ($j == $idsize && !$matched) {                  				# no match, but all read?
             die "\nThis ID is not in the station list: $station_id\n\n";   # then it's not there
		  }
       } while ($station_id ne $id_tomatch);                                # keep looping if there is no match
    } else {

	   #-----------------------------------------------
	   # if earlier or later time, record it
	   #-----------------------------------------------
	   if ($this_time < $stn_info{$station_id}{"begin_date"}) {
	   	   $stn_info{$station_id}{"begin_date"}= $this_time;
	   } elsif ($this_time > $stn_info{$station_id}{"end_date"}) {
	       $stn_info{$station_id}{"end_date"}= $this_time;
   	   }
	}
}
 
close (SFCINFILE);

foreach $station_name (sort keys %stn_info) {         # let's look at what we have
  print " $station_name: ";
  print "first=$stn_info{$station_name}{begin_date} ";
  print "last=$stn_info{$station_name}{end_date}\n";
}
 
&write_stations_rec($station_out, %stn_info);
&write_CD_stations_rec($CD_station_out, %stn_info);
&write_stn_id_rec($stn_id_out, %stn_info);

 
#------------------------------------------------------------------------------------------    
# write_stations_rec.c - writes a record to the 'stations' output file.
#------------------------------------------------------------------------------------------    

sub write_stations_rec 
{
    local ($out_stream, %stnptr) = @_;
    open (STNSOUT1, ">$out_stream") || die "Can't open $out_stream";

    foreach $station_name (sort keys %stnptr) {
        printf STNSOUT1 ("%-15s %10d %10.5f %11.5f %3d %5d %-46.46s %-3s %-8s %-8s %-2s %02d %-3s %6.2f %-1s %4d %-15s %9.1f %1s\n",
           $stnptr{$station_name}{"project"},  
            $stnptr{$station_name}{"stnid_int"}, 
            $stnptr{$station_name}{"lat"},   
            $stnptr{$station_name}{"lon"}, 
            $stnptr{$station_name}{"occur"}, 
            $stnptr{$station_name}{"accuracy"},
            $stnptr{$station_name}{"name"},  
            $stnptr{$station_name}{"comm_code"},  
            $stnptr{$station_name}{"begin_date"}, 
            $stnptr{$station_name}{"end_date"},    
            $stnptr{$station_name}{"country"}, 
            $stnptr{$station_name}{"state"}, 
            $stnptr{$station_name}{"county"},
            $stnptr{$station_name}{"time_zone"}, 
            $stnptr{$station_name}{"dst_switch"},
            $stnptr{$station_name}{"platform"},
            $stnptr{$station_name}{"frequency"}, 
            $stnptr{$station_name}{"elev"},
            $stnptr{$station_name}{"fixed_mobile"});
	}
    close (STNSOUT1);
} 

   
#------------------------------------------------------------------------------------------    
# write_CD_stations_rec.c - writes a record to the 'stationsCD' output file
#------------------------------------------------------------------------------------------    

sub write_CD_stations_rec 
{
    local ($out_stream, %stnptr) = @_;
    open (STNSOUT2, ">$out_stream") || die "Can't open $out_stream";
 
    foreach $station_name (sort keys %stnptr) { 
        printf STNSOUT2 ("%-15s %4d %10.5f %11.5f %3d %5d %-46.46s %-3s %-8s %-8s %-2s %02d %-3s %6.2f %-1s %4d %-15s %9.1f %1s\n",
            $stnptr{$station_name}{"stnid_ext"},
            $stnptr{$station_name}{"id_type"},
            $stnptr{$station_name}{"lat"},
            $stnptr{$station_name}{"lon"},
            $stnptr{$station_name}{"occur"},
            $stnptr{$station_name}{"accuracy"},
            $stnptr{$station_name}{"name"},
            $stnptr{$station_name}{"comm_code"},
            $stnptr{$station_name}{"begin_date"},
            $stnptr{$station_name}{"end_date"},
            $stnptr{$station_name}{"country"},
            $stnptr{$station_name}{"state"},
            $stnptr{$station_name}{"county"},
            $stnptr{$station_name}{"time_zone"},
            $stnptr{$station_name}{"dst_switch"},
            $stnptr{$station_name}{"platform"},
            $stnptr{$station_name}{"frequency"},
            $stnptr{$station_name}{"elev"},
            $stnptr{$station_name}{"fixed_mobile"});
    }
    close (STNSOUT2);
}


#------------------------------------------------------------------------------------------    
# write_stn_id_rec - writes a record to the 'stn_id' output file.
#------------------------------------------------------------------------------------------    

sub write_stn_id_rec 
{
    local ($out_stream, %stnptr) = @_;
    open (STNSOUT3, ">$out_stream") || die "Can't open $out_stream";
 
    foreach $station_name (sort keys %stnptr) {
        if (length ($stnptr{$station_name}{"stnid_ext"}) > 18) {
            printf STNSOUT3 ("%-15s %10d %4d %-18s\n",
                $stnptr{$station_name}{"project"},
                $stnptr{$station_name}{"stnid_int"},
                $stnptr{$station_name}{"id_type"},
                $stnptr{$station_name}{"stnid_ext"});
        } else {
            printf STNSOUT3 ("%-15s %10d %4d %-s\n",
                $stnptr{$station_name}{"project"},
                $stnptr{$station_name}{"stnid_int"},
                $stnptr{$station_name}{"id_type"},
                $stnptr{$station_name}{"stnid_ext"});
        }
    }
    close (STNSOUT3);
}


#--------------------------------------------------------------------------------
#  trim - trims everything off ends of strings from first space on
#
#  input:
#            $str        the string to trim
#  output:
#            $new_str    the trimmed string
#--------------------------------------------------------------------------------
sub trim
{
    local ($str) = @_;
 
    $first_space = index($str, " ");
    $new_str = substr($str, 0, $first_space);
    return $new_str;
}
