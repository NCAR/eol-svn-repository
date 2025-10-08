#!/usr/bin/perl

sub get_type {

   my %type;
   my $field_name = shift;

   $type{'base_time'} = "base_time";
   $type{'time_offset'} = "time_offset";
   $type{'lat'} = "lat";
   $type{'lon'} = "lon";
   $type{'alt'} = "elev";
   $type{'AtmPress'} = "stn_pres";
   $type{'T2M_AVG'} = "temp_air";
   $type{'T2m_AVG'} = "temp_air";
   $type{'DP2M_AVG'} = "dew_pt";
   $type{'DP2m_AVG'} = "dew_pt";
   $type{'RH2M_AVG'} = "rel_hum";
   $type{'RH2m_AVG'} = "rel_hum";
   $type{'spec_hum'} = "spec_hum";
   $type{'WS10M_U_WVT'} = "wind_spd";
   $type{'WinSpeed_U_WVT'} = "wind_spd";
   $type{'WD10M_DU_WVT'} = "wind_dir";
   $type{'WinDir_DU_WVT'} = "wind_dir";
   $type{'u_wind'} = "U_wind";
   $type{'v_wind'} = "V_wind";
   $type{'PcpRate'} = "precip";
   $type{'PCPRate'} = "precip";
   $type{'CumSnow'} = "snow";
   $type{'down_short_hemisp'} = "short_in";
   $type{'up_short_hemisp'} = "short_out";
   $type{'down_long_hemisp_shaded1'} = "long_in";
   $type{'up_long_hemisp'} = "long_out";
   $type{'net'} = "net_rad";
   $type{'sfc_ir_temp'} = "skintemp";
   $type{'par_in'} = "par_in";
   $type{'par_out'} = "par_out";

   return $type{$field_name} if $type{$field_name};
   return undef;

}
1;
