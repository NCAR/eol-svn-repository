#!/usr/bin/perl

use strict;

# look for the following varaibles in the netcdf files
my $mettwr2h_dir = "/net/work/CEOP/version2/data_processing/other/NSA/raw/cdf/mettwr2h";
my $mettwr4h_dir = "/net/work/CEOP/version2/data_processing/other/NSA/raw/cdf/mettwr4h";

my @mettwr2h_fields;
push(@mettwr2h_fields,"time_offset");
push(@mettwr2h_fields,"AtmPress");
push(@mettwr2h_fields,"WinSpeed_U_WVT");
push(@mettwr2h_fields,"qc_WinSpeed_U_WVT");
push(@mettwr2h_fields,"WinDir_DU_WVT");
push(@mettwr2h_fields,"qc_WinDir_DU_WVT");
push(@mettwr2h_fields,"T5m_AVG");
push(@mettwr2h_fields,"qc_T5m_AVG");
push(@mettwr2h_fields,"T2m_AVG");
push(@mettwr2h_fields,"qc_T2m_AVG");
push(@mettwr2h_fields,"RH5m_AVG");
push(@mettwr2h_fields,"qc_RH5m_AVG");
push(@mettwr2h_fields,"RH2m_AVG");
push(@mettwr2h_fields,"qc_RH2m_AVG");
push(@mettwr2h_fields,"DP2m_AVG");
push(@mettwr2h_fields,"qc_DP2m_AVG");
push(@mettwr2h_fields,"DP5m_AVG");
push(@mettwr2h_fields,"qc_DP5m_AVG");
push(@mettwr2h_fields,"PCPRate");
push(@mettwr2h_fields,"qc_PCPRate");
push(@mettwr2h_fields,"CumSnow");
push(@mettwr2h_fields,"qc_CumSnow");

my @mettwr4h_fields;
push(@mettwr4h_fields,"time_offset");
push(@mettwr4h_fields,"AtmPress");
push(@mettwr4h_fields,"qc_AtmPress");
push(@mettwr4h_fields,"WS2M_U_WVT");
push(@mettwr4h_fields,"qc_WS2M_U_WVT");
push(@mettwr4h_fields,"WS10M_U_WVT");
push(@mettwr4h_fields,"qc_WS10M_U_WVT");
push(@mettwr4h_fields,"WS20M_U_WVT");
push(@mettwr4h_fields,"qc_WS20M_U_WVT");
push(@mettwr4h_fields,"WS40M_U_WVT");
push(@mettwr4h_fields,"qc_WS40M_U_WVT");
push(@mettwr4h_fields,"WD2M_DU_WVT");
push(@mettwr4h_fields,"qc_WD2M_DU_WVT");
push(@mettwr4h_fields,"WD10M_DU_WVT");
push(@mettwr4h_fields,"qc_WD10M_DU_WVT");
push(@mettwr4h_fields,"WD20M_DU_WVT");
push(@mettwr4h_fields,"qc_WD20M_DU_WVT");
push(@mettwr4h_fields,"WD40M_DU_WVT");
push(@mettwr4h_fields,"qc_WD40M_DU_WVT");
push(@mettwr4h_fields,"T2M_AVG");
push(@mettwr4h_fields,"qc_T2M_AVG");
push(@mettwr4h_fields,"T10M_AVG");
push(@mettwr4h_fields,"qc_T10M_AVG");
push(@mettwr4h_fields,"T20M_AVG");
push(@mettwr4h_fields,"qc_T20M_AVG");
push(@mettwr4h_fields,"T40M_AVG");
push(@mettwr4h_fields,"qc_T40M_AVG");
push(@mettwr4h_fields,"RH2M_AVG");
push(@mettwr4h_fields,"qc_RH2M_AVG");
push(@mettwr4h_fields,"RH10M_AVG");
push(@mettwr4h_fields,"qc_RH10M_AVG");
push(@mettwr4h_fields,"RH20M_AVG");
push(@mettwr4h_fields,"qc_RH20M_AVG");
push(@mettwr4h_fields,"RH40M_AVG");
push(@mettwr4h_fields,"qc_RH40M_AVG");
push(@mettwr4h_fields,"DP2M_AVG");
push(@mettwr4h_fields,"qc_DP2M_AVG");
push(@mettwr4h_fields,"DP10M_AVG");
push(@mettwr4h_fields,"qc_DP10M_AVG");
push(@mettwr4h_fields,"DP20M_AVG");
push(@mettwr4h_fields,"qc_DP20M_AVG");
push(@mettwr4h_fields,"DP40M_AVG");
push(@mettwr4h_fields,"qc_DP40M_AVG");
push(@mettwr4h_fields,"PcpRate");
push(@mettwr4h_fields,"qc_PcpRate");
push(@mettwr4h_fields,"CumSnow");
push(@mettwr4h_fields,"qc_CumSnow");

# get rid of the output file if it exists
my $out_fname;

#$out_fname = "mettwr2h.find_var.out";
#unlink($out_fname) if (-e$out_fname);
#my ($field, $cmd);
#foreach $field (@mettwr2h_fields) {
#  $cmd = "./find_var.pl -d $mettwr2h_dir -n $field -o $out_fname";
#  system($cmd);
#}
$out_fname = "mettwr4h.find_var.out";
unlink($out_fname) if (-e$out_fname);
my ($field, $cmd);
foreach $field (@mettwr4h_fields) {
  $cmd = "./find_var.pl -d $mettwr4h_dir -n $field -o $out_fname";
  system($cmd);
}
