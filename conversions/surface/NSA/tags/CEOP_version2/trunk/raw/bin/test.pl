#!/usr/bin/perl

use strict;

my @in_file;
push(@in_file, "NSA_GNDRAD_flagging.txt");
push(@in_file, "NSA_METTWR_flagging.txt");
push(@in_file, "NSA_SKYRAD_flagging.txt");

# stuff all the lines into an array (except the header)
my @lines;
open(IN, $in_file[1]) || die "cannot open $in_file[1]";
while (<IN>) {
   chop;
   push(@lines, $_) if ( !/^Station/ );
} # end while
close(IN);

my %param_index;
my @arr;
my %hash1 = {};
my %hash2 = {};
my %hash3 = {};
$hash1{'test1'} = [1,2,3];
print "test: $hash1{'test1'}\n";exit();

$param_index{'C1'}{'test1'} = [];
$param_index{'C1'}{'AtmPress'} = 0;
$param_index{'C2'}{'AtmPress'} = 0;
$param_index{'C1'}{'T2M_AVG'} = 1;
$param_index{'C2'}{'T2m_AVG'} = 1;
$param_index{'C1'}{'DP2M_AVG'} = 2;
$param_index{'C2'}{'DP2m_AVG'} = 2;
$param_index{'C1'}{'RH2M_AVG'} = 3;
$param_index{'C2'}{'RH2m_AVG'} = 3;
$param_index{'C1'}{'spec_hum'} = 4;
$param_index{'C2'}{'spec_hum'} = 4;
$param_index{'C1'}{'WS10M_U_WVT'} = 5;
$param_index{'C2'}{'WinSpeed_U_WVT'} = 5;
$param_index{'C1'}{'WD10M_DU_WVT'} = 6;
$param_index{'C2'}{'WinDir_DU_WVT'} = 6;
$param_index{'C1'}{'u_wind'} = 7;
$param_index{'C2'}{'u_wind'} = 7;
$param_index{'C1'}{'v_wind'} = 8;
$param_index{'C2'}{'v_wind'} = 8;
$param_index{'C1'}{'PcpRate'} = 9;
$param_index{'C2'}{'PCPRate'} = 9;
$param_index{'C1'}{'CumSnow'} = 10;
$param_index{'C2'}{'CumSnow'} = 10;
$param_index{'C1'}{'down_short_hemisp'} = 11;
$param_index{'C2'}{'down_short_hemisp'} = 11;
$param_index{'C1'}{'up_short_hemisp'} = 12;
$param_index{'C2'}{'up_short_hemisp'} = 12;
$param_index{'C1'}{'down_long_hemisp_shaded1'} = 13;
$param_index{'C2'}{'down_long_hemisp_shaded1'} = 13;
$param_index{'C1'}{'up_long_hemisp'} = 14;
$param_index{'C2'}{'up_long_hemisp'} = 14;
$param_index{'C1'}{'net'} = 15;
$param_index{'C2'}{'net'} = 15;
$param_index{'C1'}{'sfc_ir_temp'} = 16;
$param_index{'C2'}{'sfc_ir_temp'} = 16;
$param_index{'C1'}{'par_in'} = 17;
$param_index{'C2'}{'par_in'} = 17;
$param_index{'C1'}{'par_out'} = 18;
$param_index{'C2'}{'par_out'} = 18;


my @sfc_parameter_list;
push(@sfc_parameter_list, "AtmPress");
push(@sfc_parameter_list, "T2m_AVG");           
push(@sfc_parameter_list, "T2M_AVG");           
push(@sfc_parameter_list, "DP2m_AVG");          
push(@sfc_parameter_list, "DP2M_AVG");          
push(@sfc_parameter_list, "RH2m_AVG");          
push(@sfc_parameter_list, "RH2M_AVG");          
push(@sfc_parameter_list, "spec_hum");          
push(@sfc_parameter_list, "WinSpeed_U_WVT");    
push(@sfc_parameter_list, "WS10M_U_WVT");       
push(@sfc_parameter_list, "WinDir_DU_WVT");     
push(@sfc_parameter_list, "WD10M_DU_WVT");      
push(@sfc_parameter_list, "u_wind");            
push(@sfc_parameter_list, "v_wind");            
push(@sfc_parameter_list, "PCPRate");           
push(@sfc_parameter_list, "PcpRate");           
push(@sfc_parameter_list, "CumSnow");
push(@sfc_parameter_list, "down_short_hemisp");     
push(@sfc_parameter_list, "up_short_hemisp");     
push(@sfc_parameter_list, "down_long_hemisp_shaded1");  
push(@sfc_parameter_list, "up_long_hemisp");     
push(@sfc_parameter_list, "net");     
push(@sfc_parameter_list, "sfc_ir_temp");     
push(@sfc_parameter_list, "par_in");     
push(@sfc_parameter_list, "par_out" );     

my $stn = "C1";
my $cmd = "grep $stn $in_file[1] | sort";
#*****************************
sub convert_datetime() {
  my $date = shift;
  my $time = shift;

  my ($month, $day, $year) = split(/\//, $date);
  return "$year$month$day.$time";

}
