#!/bin/csh

set nc2asc = "/opt/local/bin/nc2asc"

#/opt/local/bin/nc2asc -b RAF-CLOUDS -i DC3rf01.nc -o DC3-RAF-CLOUDS_GV_20120518_R1.ICT
#/opt/local/bin/nc2asc -b RAF-AEROSOL_template -i DC3rf01.nc -o DC3-RAF-AEROSOL_GV_20120518_R1.ICT
#/opt/local/bin/nc2asc -b RAF-NAV_template -i DC3rf01.nc -o DC3-RAF-NAV_GV_20120518_R1.ICT

foreach type (`cat types`)
${nc2asc} -b RAF-${type}_template -i ../DC3rf01.nc -o DC3-RAF-${type}_GV_20120518_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf02.nc -o DC3-RAF-${type}_GV_20120519_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf03.nc -o DC3-RAF-${type}_GV_20120521_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf04.nc -o DC3-RAF-${type}_GV_20120525_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf05.nc -o DC3-RAF-${type}_GV_20120526_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf06.nc -o DC3-RAF-${type}_GV_20120529_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf07.nc -o DC3-RAF-${type}_GV_20120530_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf08.nc -o DC3-RAF-${type}_GV_20120601_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf09.nc -o DC3-RAF-${type}_GV_20120605_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf10.nc -o DC3-RAF-${type}_GV_20120606_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf11.nc -o DC3-RAF-${type}_GV_20120607_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf12.nc -o DC3-RAF-${type}_GV_20120611_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf13.nc -o DC3-RAF-${type}_GV_20120615_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf14.nc -o DC3-RAF-${type}_GV_20120616_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf15.nc -o DC3-RAF-${type}_GV_20120617_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf16.nc -o DC3-RAF-${type}_GV_20120621_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf17.nc -o DC3-RAF-${type}_GV_20120622_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf18.nc -o DC3-RAF-${type}_GV_20120623_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf19.nc -o DC3-RAF-${type}_GV_20120625_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf20.nc -o DC3-RAF-${type}_GV_20120627_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf21.nc -o DC3-RAF-${type}_GV_20120628_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf22.nc -o DC3-RAF-${type}_GV_20120630_R1.ICT
end
