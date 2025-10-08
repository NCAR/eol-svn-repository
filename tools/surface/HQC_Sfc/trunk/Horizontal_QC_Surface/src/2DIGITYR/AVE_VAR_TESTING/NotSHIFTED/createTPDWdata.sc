#
# Prepare temp for gnuplot
#
changenum 121_p3.out 0
mv dump.out stnp.out
changenum 132_p3.out 2400
cat dump.out >> stnp.out
changenum 152_p3.out 4800
cat dump.out >> stnp.out
changenum 166_p3.out 7200
cat dump.out >> stnp.out

#
# Prepare Dew Pt temp for gnuplot
#
changenum 121_p4.out 0
mv dump.out slp.out
changenum 132_p4.out 2400
cat dump.out >> slp.out
changenum 152_p4.out 4800 
cat dump.out >> slp.out 
changenum 166_p4.out 7200 
cat dump.out >> slp.out

#
# Prepare Wind Speed for gnuplot
#
changenum 121_p5.out 0
mv dump.out cslp.out
changenum 132_p5.out 2400
cat dump.out >> cslp.out
changenum 152_p5.out 4800 
cat dump.out >> cslp.out 
changenum 166_p5.out 7200 
cat dump.out >> cslp.out

#
# Prepare Wind Direction for gnuplot 
# 
changenum 121_p6.out 0
mv dump.out cslp.out
changenum 132_p6.out 2400
cat dump.out >> cslp.out
changenum 152_p6.out 4800
cat dump.out >> cslp.out
changenum 166_p6.out 7200
cat dump.out >> cslp.out 
