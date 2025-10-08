#
# Prepare temp for gnuplot
#
changenum 121_p3.out 0
mv dump.out temp.out
changenum 122_p3.out 2400
cat dump.out >> temp.out
changenum 123_p3.out 4800
cat dump.out >> temp.out
changenum 124_p3.out 7200
cat dump.out >> temp.out
changenum 125_p3.out 9600
cat dump.out >> temp.out
changenum 128_p3.out 12000
cat dump.out >> temp.out

#
# Prepare Dew Pt temp for gnuplot
#
changenum 121_p4.out 0
mv dump.out dwpt.out
changenum 122bak_p4.out 2400
cat dump.out >> dwpt.out
changenum 123_p4.out 4800 
cat dump.out >> dwpt.out 
changenum 124_p4.out 7200 
cat dump.out >> dwpt.out
changenum 125_p4.out 9600
cat dump.out >> dwpt.out
changenum 128_p4.out 12000
cat dump.out >> dwpt.out
