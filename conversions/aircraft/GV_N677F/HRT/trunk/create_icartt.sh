#!/bin/csh

set nc2asc = "/opt/local/bin/nc2asc"

foreach type (`cat types`)
${nc2asc} -b RAF-${type}_template -i ../DC3rf01h.nc -o DC3-RAF-${type}-HRT_GV_20120518_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf02h.nc -o DC3-RAF-${type}-HRT_GV_20120519_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf03h.nc -o DC3-RAF-${type}-HRT_GV_20120521_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf04h.nc -o DC3-RAF-${type}-HRT_GV_20120525_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf05h.nc -o DC3-RAF-${type}-HRT_GV_20120526_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf06h.nc -o DC3-RAF-${type}-HRT_GV_20120529_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf07h.nc -o DC3-RAF-${type}-HRT_GV_20120530_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf08h.nc -o DC3-RAF-${type}-HRT_GV_20120601_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf09h.nc -o DC3-RAF-${type}-HRT_GV_20120605_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf10h.nc -o DC3-RAF-${type}-HRT_GV_20120606_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf11h.nc -o DC3-RAF-${type}-HRT_GV_20120607_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf12h.nc -o DC3-RAF-${type}-HRT_GV_20120611_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf13h.nc -o DC3-RAF-${type}-HRT_GV_20120615_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf14h.nc -o DC3-RAF-${type}-HRT_GV_20120616_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf15h.nc -o DC3-RAF-${type}-HRT_GV_20120617_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf16h.nc -o DC3-RAF-${type}-HRT_GV_20120621_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf17h.nc -o DC3-RAF-${type}-HRT_GV_20120622_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf18h.nc -o DC3-RAF-${type}-HRT_GV_20120623_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf19h.nc -o DC3-RAF-${type}-HRT_GV_20120625_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf20h.nc -o DC3-RAF-${type}-HRT_GV_20120627_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf21h.nc -o DC3-RAF-${type}-HRT_GV_20120628_R1.ICT
${nc2asc} -b RAF-${type}_template -i ../DC3rf22h.nc -o DC3-RAF-${type}-HRT_GV_20120630_R1.ICT
end
