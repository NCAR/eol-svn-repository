echo ""

# echo "separate stations in surface output..."

# perl -e 'while (<>) {print $_ if(/E1_/);}' ../CPPA_SGP_SGP*.sfc > sfc/CPPA_SGP_E1_Larned_20050101_20091231.sfc
# perl -e 'while (<>) {print $_ if(/E2_/);}' ../CPPA_SGP_SGP*.sfc > sfc/CPPA_SGP_E2_Hillsboro_20050101_20091231.sfc
# perl -e 'while (<>) {print $_ if(/E3_/);}' ../CPPA_SGP_SGP*.sfc > sfc/CPPA_SGP_E3_Le_Roy_20050101_20091231.sfc
# perl -e 'while (<>) {print $_ if(/E4_/);}' ../CPPA_SGP_SGP*.sfc > sfc/CPPA_SGP_E4_Plevna_20050101_20091231.sfc
# perl -e 'while (<>) {print $_ if(/E5_/);}' ../CPPA_SGP_SGP*.sfc > sfc/CPPA_SGP_E5_Halstead_20050101_20091231.sfc
# perl -e 'while (<>) {print $_ if(/E6_/);}' ../CPPA_SGP_SGP*.sfc > sfc/CPPA_SGP_E6_Towanda_20050101_20091231.sfc
# perl -e 'while (<>) {print $_ if(/E7_/);}' ../CPPA_SGP_SGP*.sfc > sfc/CPPA_SGP_E7_Elk_Falls_20050101_20091231.sfc
# perl -e 'while (<>) {print $_ if(/E8_/);}' ../CPPA_SGP_SGP*.sfc > sfc/CPPA_SGP_E8_Coldwater_20050101_20091231.sfc
# perl -e 'while (<>) {print $_ if(/E9_/);}' ../CPPA_SGP_SGP*.sfc > sfc/CPPA_SGP_E9_Ashton_20050101_20091231.sfc
# perl -e 'while (<>) {print $_ if(/E10_/);}' ../CPPA_SGP_SGP*.sfc > sfc/CPPA_SGP_E10_Tyro_20050101_20091231.sfc
# perl -e 'while (<>) {print $_ if(/E11_/);}' ../CPPA_SGP_SGP*.sfc > sfc/CPPA_SGP_E11_Byron_20050101_20091231.sfc
# perl -e 'while (<>) {print $_ if(/E12_/);}' ../CPPA_SGP_SGP*.sfc > sfc/CPPA_SGP_E12_Pawhuska_20050101_20091231.sfc
# perl -e 'while (<>) {print $_ if(/E13_/);}' ../CPPA_SGP_SGP*.sfc > sfc/CPPA_SGP_E13_Lamont_20050101_20091231.sfc
# perl -e 'while (<>) {print $_ if(/E14_/);}' ../CPPA_SGP_SGP*.sfc > sfc/CPPA_SGP_E14_Lamont_20050101_20091231.sfc
# perl -e 'while (<>) {print $_ if(/C1_/);}' ../CPPA_SGP_SGP*.sfc > sfc/CPPA_SGP_C1_Lamont_20050101_20091231.sfc
# perl -e 'while (<>) {print $_ if(/C2_/);}' ../CPPA_SGP_SGP*.sfc > sfc/CPPA_SGP_C2_Lamont_20050101_20091231.sfc
# perl -e 'while (<>) {print $_ if(/E15_/);}' ../CPPA_SGP_SGP*.sfc > sfc/CPPA_SGP_E15_Ringwood_20050101_20091231.sfc
# perl -e 'while (<>) {print $_ if(/E16_/);}' ../CPPA_SGP_SGP*.sfc > sfc/CPPA_SGP_E16_Vici_20050101_20091231.sfc
# perl -e 'while (<>) {print $_ if(/E18_/);}' ../CPPA_SGP_SGP*.sfc > sfc/CPPA_SGP_E18_Morris_20050101_20091231.sfc
# perl -e 'while (<>) {print $_ if(/E19_/);}' ../CPPA_SGP_SGP*.sfc > sfc/CPPA_SGP_E19_El_Reno_20050101_20091231.sfc
# perl -e 'while (<>) {print $_ if(/E20_/);}' ../CPPA_SGP_SGP*.sfc > sfc/CPPA_SGP_E20_Meeker_20050101_20091231.sfc
# perl -e 'while (<>) {print $_ if(/E21_/);}' ../CPPA_SGP_SGP*.sfc > sfc/CPPA_SGP_E21_Okmulgee_20050101_20091231.sfc
# perl -e 'while (<>) {print $_ if(/E22_/);}' ../CPPA_SGP_SGP*.sfc > sfc/CPPA_SGP_E22_Cordell_20050101_20091231.sfc
# perl -e 'while (<>) {print $_ if(/E23_/);}' ../CPPA_SGP_SGP*.sfc > sfc/CPPA_SGP_E23_Ft.Cobb_20050101_20091231.sfc
# perl -e 'while (<>) {print $_ if(/E24_/);}' ../CPPA_SGP_SGP*.sfc > sfc/CPPA_SGP_E24_Cyril_20050101_20091231.sfc
# perl -e 'while (<>) {print $_ if(/E25_/);}' ../CPPA_SGP_SGP*.sfc > sfc/CPPA_SGP_E25_Seminole_20050101_20091231.sfc
# perl -e 'while (<>) {print $_ if(/E26_/);}' ../CPPA_SGP_SGP*.sfc > sfc/CPPA_SGP_E26_Cement_20050101_20091231.sfc
# perl -e 'while (<>) {print $_ if(/E27_/);}' ../CPPA_SGP_SGP*.sfc > sfc/CPPA_SGP_E27_Earlsboro_20050101_20091231.sfc

# echo "separate stations in flux output..."
# echo "  Note: E21 is done by hand to fix DST times to UTS from 01/20/2009 at 2030 to 01/22/2009 at 1800 (add 5 hours)"

# perl -e 'while (<>) {print $_ if(/E1_/);}' ../CPPA_SGP_SGP*.flx > flx/CPPA_SGP_E1_Larned_20050101_20091231.flx
# perl -e 'while (<>) {print $_ if(/E2_/);}' ../CPPA_SGP_SGP*.flx > flx/CPPA_SGP_E2_Hillsboro_20050101_20091231.flx
# perl -e 'while (<>) {print $_ if(/E3_/);}' ../CPPA_SGP_SGP*.flx > flx/CPPA_SGP_E3_Le_Roy_20050101_20091231.flx
# perl -e 'while (<>) {print $_ if(/E4_/);}' ../CPPA_SGP_SGP*.flx > flx/CPPA_SGP_E4_Plevna_20050101_20091231.flx
# perl -e 'while (<>) {print $_ if(/E5_/);}' ../CPPA_SGP_SGP*.flx > flx/CPPA_SGP_E5_Halstead_20050101_20091231.flx
# perl -e 'while (<>) {print $_ if(/E6_/);}' ../CPPA_SGP_SGP*.flx > flx/CPPA_SGP_E6_Towanda_20050101_20091231.flx
# perl -e 'while (<>) {print $_ if(/E7_/);}' ../CPPA_SGP_SGP*.flx > flx/CPPA_SGP_E7_Elk_Falls_20050101_20091231.flx
# perl -e 'while (<>) {print $_ if(/E8_/);}' ../CPPA_SGP_SGP*.flx > flx/CPPA_SGP_E8_Coldwater_20050101_20091231.flx
# perl -e 'while (<>) {print $_ if(/E9_/);}' ../CPPA_SGP_SGP*.flx > flx/CPPA_SGP_E9_Ashton_20050101_20091231.flx
# perl -e 'while (<>) {print $_ if(/E10_/);}' ../CPPA_SGP_SGP*.flx > flx/CPPA_SGP_E10_Tyro_20050101_20091231.flx
# perl -e 'while (<>) {print $_ if(/E11_/);}' ../CPPA_SGP_SGP*.flx > flx/CPPA_SGP_E11_Byron_20050101_20091231.flx
# perl -e 'while (<>) {print $_ if(/E12_/);}' ../CPPA_SGP_SGP*.flx > flx/CPPA_SGP_E12_Pawhuska_20050101_20091231.flx
# perl -e 'while (<>) {print $_ if(/E13_/);}' ../CPPA_SGP_SGP*.flx > flx/CPPA_SGP_E13_Lamont_20050101_20091231.flx
# perl -e 'while (<>) {print $_ if(/E14_/);}' ../CPPA_SGP_SGP*.flx > flx/CPPA_SGP_E14_Lamont_20050101_20091231.flx
# perl -e 'while (<>) {print $_ if(/C1_/);}' ../CPPA_SGP_SGP*.flx > flx/CPPA_SGP_C1_Lamont_20050101_20091231.flx
# perl -e 'while (<>) {print $_ if(/C2_/);}' ../CPPA_SGP_SGP*.flx > flx/CPPA_SGP_C2_Lamont_20050101_20091231.flx
# perl -e 'while (<>) {print $_ if(/E15_/);}' ../CPPA_SGP_SGP*.flx > flx/CPPA_SGP_E15_Ringwood_20050101_20091231.flx
# perl -e 'while (<>) {print $_ if(/E16_/);}' ../CPPA_SGP_SGP*.flx > flx/CPPA_SGP_E16_Vici_20050101_20091231.flx
# perl -e 'while (<>) {print $_ if(/E18_/);}' ../CPPA_SGP_SGP*.flx > flx/CPPA_SGP_E18_Morris_20050101_20091231.flx
# perl -e 'while (<>) {print $_ if(/E19_/);}' ../CPPA_SGP_SGP*.flx > flx/CPPA_SGP_E19_El_Reno_20050101_20091231.flx
# perl -e 'while (<>) {print $_ if(/E20_/);}' ../CPPA_SGP_SGP*.flx > flx/CPPA_SGP_E20_Meeker_20050101_20091231.flx
# # perl -e 'while (<>) {print $_ if(/E21_/);}' ../CPPA_SGP_SGP*.flx > flx/CPPA_SGP_E21_Okmulgee_20050101_20091231.flx
# perl -e 'while (<>) {print $_ if(/E22_/);}' ../CPPA_SGP_SGP*.flx > flx/CPPA_SGP_E22_Cordell_20050101_20091231.flx
# perl -e 'while (<>) {print $_ if(/E23_/);}' ../CPPA_SGP_SGP*.flx > flx/CPPA_SGP_E23_Ft.Cobb_20050101_20091231.flx
# perl -e 'while (<>) {print $_ if(/E24_/);}' ../CPPA_SGP_SGP*.flx > flx/CPPA_SGP_E24_Cyril_20050101_20091231.flx
# perl -e 'while (<>) {print $_ if(/E25_/);}' ../CPPA_SGP_SGP*.flx > flx/CPPA_SGP_E25_Seminole_20050101_20091231.flx
# perl -e 'while (<>) {print $_ if(/E26_/);}' ../CPPA_SGP_SGP*.flx > flx/CPPA_SGP_E26_Cement_20050101_20091231.flx
# perl -e 'while (<>) {print $_ if(/E27_/);}' ../CPPA_SGP_SGP*.flx > flx/CPPA_SGP_E27_Earlsboro_20050101_20091231.flx
    
echo "separate stations in soil output..."
 
perl -e 'while (<>) {print $_ if(/E1_/);}' ../CPPA_SGP_SGP*.stm > stm/CPPA_SGP_E1_Larned_20050101_20091231.stm
perl -e 'while (<>) {print $_ if(/E2_/);}' ../CPPA_SGP_SGP*.stm > stm/CPPA_SGP_E2_Hillsboro_20050101_20091231.stm
perl -e 'while (<>) {print $_ if(/E3_/);}' ../CPPA_SGP_SGP*.stm > stm/CPPA_SGP_E3_Le_Roy_20050101_20091231.stm
perl -e 'while (<>) {print $_ if(/E4_/);}' ../CPPA_SGP_SGP*.stm > stm/CPPA_SGP_E4_Plevna_20050101_20091231.stm
perl -e 'while (<>) {print $_ if(/E5_/);}' ../CPPA_SGP_SGP*.stm > stm/CPPA_SGP_E5_Halstead_20050101_20091231.stm
perl -e 'while (<>) {print $_ if(/E6_/);}' ../CPPA_SGP_SGP*.stm > stm/CPPA_SGP_E6_Towanda_20050101_20091231.stm
perl -e 'while (<>) {print $_ if(/E7_/);}' ../CPPA_SGP_SGP*.stm > stm/CPPA_SGP_E7_Elk_Falls_20050101_20091231.stm
perl -e 'while (<>) {print $_ if(/E8_/);}' ../CPPA_SGP_SGP*.stm > stm/CPPA_SGP_E8_Coldwater_20050101_20091231.stm
perl -e 'while (<>) {print $_ if(/E9_/);}' ../CPPA_SGP_SGP*.stm > stm/CPPA_SGP_E9_Ashton_20050101_20091231.stm
perl -e 'while (<>) {print $_ if(/E10_/);}' ../CPPA_SGP_SGP*.stm > stm/CPPA_SGP_E10_Tyro_20050101_20091231.stm
perl -e 'while (<>) {print $_ if(/E11_/);}' ../CPPA_SGP_SGP*.stm > stm/CPPA_SGP_E11_Byron_20050101_20091231.stm
perl -e 'while (<>) {print $_ if(/E12_/);}' ../CPPA_SGP_SGP*.stm > stm/CPPA_SGP_E12_Pawhuska_20050101_20091231.stm
perl -e 'while (<>) {print $_ if(/E13_/);}' ../CPPA_SGP_SGP*.stm > stm/CPPA_SGP_E13_Lamont_20050101_20091231.stm
perl -e 'while (<>) {print $_ if(/E14_/);}' ../CPPA_SGP_SGP*.stm > stm/CPPA_SGP_E14_Lamont_20050101_20091231.stm
perl -e 'while (<>) {print $_ if(/C1_/);}' ../CPPA_SGP_SGP*.stm > stm/CPPA_SGP_C1_Lamont_20050101_20091231.stm
perl -e 'while (<>) {print $_ if(/C2_/);}' ../CPPA_SGP_SGP*.stm > stm/CPPA_SGP_C2_Lamont_20050101_20091231.stm
perl -e 'while (<>) {print $_ if(/E15_/);}' ../CPPA_SGP_SGP*.stm > stm/CPPA_SGP_E15_Ringwood_20050101_20091231.stm
perl -e 'while (<>) {print $_ if(/E16_/);}' ../CPPA_SGP_SGP*.stm > stm/CPPA_SGP_E16_Vici_20050101_20091231.stm
perl -e 'while (<>) {print $_ if(/E18_/);}' ../CPPA_SGP_SGP*.stm > stm/CPPA_SGP_E18_Morris_20050101_20091231.stm
perl -e 'while (<>) {print $_ if(/E19_/);}' ../CPPA_SGP_SGP*.stm > stm/CPPA_SGP_E19_El_Reno_20050101_20091231.stm
perl -e 'while (<>) {print $_ if(/E20_/);}' ../CPPA_SGP_SGP*.stm > stm/CPPA_SGP_E20_Meeker_20050101_20091231.stm
perl -e 'while (<>) {print $_ if(/E21_/);}' ../CPPA_SGP_SGP*.stm > stm/CPPA_SGP_E21_Okmulgee_20050101_20091231.stm
perl -e 'while (<>) {print $_ if(/E22_/);}' ../CPPA_SGP_SGP*.stm > stm/CPPA_SGP_E22_Cordell_20050101_20091231.stm
perl -e 'while (<>) {print $_ if(/E23_/);}' ../CPPA_SGP_SGP*.stm > stm/CPPA_SGP_E23_Ft.Cobb_20050101_20091231.stm
perl -e 'while (<>) {print $_ if(/E24_/);}' ../CPPA_SGP_SGP*.stm > stm/CPPA_SGP_E24_Cyril_20050101_20091231.stm
perl -e 'while (<>) {print $_ if(/E25_/);}' ../CPPA_SGP_SGP*.stm > stm/CPPA_SGP_E25_Seminole_20050101_20091231.stm
perl -e 'while (<>) {print $_ if(/E26_/);}' ../CPPA_SGP_SGP*.stm > stm/CPPA_SGP_E26_Cement_20050101_20091231.stm
perl -e 'while (<>) {print $_ if(/E27_/);}' ../CPPA_SGP_SGP*.stm > stm/CPPA_SGP_E27_Earlsboro_20050101_20091231.stm

echo "delete zero length files (view names in zero_len.files)..."

./del_zero_length.shell 
 
# echo "rename tower output for the single station in it"
# 
# cp ../CPPA_SGP_SGP_20050101_20091231.twr twr/CPPA_SGP_C1_Lamont_20031001_20041231.twr
