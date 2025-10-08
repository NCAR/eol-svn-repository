#
# Prepare Station Pressure for gnuplot
#
changenum 121_p0.out 0
mv dump.out stnp.out
changenum 132_p0.out 2400
cat dump.out >> stnp.out
changenum 152_p0.out 4800
cat dump.out >> stnp.out
changenum 166_p0.out 7200
cat dump.out >> stnp.out

# 
# Prepare Sea Level Pressure for gnuplot
#
changenum 121_p1.out 0
mv dump.out slp.out
changenum 132_p1.out 2400
cat dump.out >> slp.out
changenum 152_p1.out 4800 
cat dump.out >> slp.out 
changenum 166_p1.out 7200 
cat dump.out >> slp.out

# 
# Prepare Calc Sea Level Pressure for gnuplot
#
changenum 121_p2.out 0
mv dump.out cslp.out
changenum 132_p2.out 2400
cat dump.out >> cslp.out
changenum 152_p2.out 4800 
cat dump.out >> cslp.out 
changenum 166_p2.out 7200 
cat dump.out >> cslp.out
#
# Prepare temp for gnuplot
#
changenum 121_p3.out 0
mv dump.out temp.out
changenum 132_p3.out 2400
cat dump.out >> temp.out
changenum 152_p3.out 4800
cat dump.out >> temp.out
changenum 166_p3.out 7200
cat dump.out >> temp.out

#
# Prepare Dew Pt temp for gnuplot
#
changenum 121_p4.out 0
mv dump.out dwpt.out
changenum 132_p4.out 2400
cat dump.out >> dwpt.out
changenum 152_p4.out 4800 
cat dump.out >> dwpt.out 
changenum 166_p4.out 7200 
cat dump.out >> dwpt.out

#
# Prepare Wind Speed for gnuplot
#
changenum 121_p5.out 0
mv dump.out wsp.out
changenum 132_p5.out 2400
cat dump.out >> wsp.out
changenum 152_p5.out 4800 
cat dump.out >> wsp.out 
changenum 166_p5.out 7200 
cat dump.out >> wsp.out

#
# Prepare Wind Direction for gnuplot 
# 
changenum 121_p6.out 0
mv dump.out wdir.out
changenum 132_p6.out 2400
cat dump.out >> wdir.out
changenum 152_p6.out 4800
cat dump.out >> wdir.out
changenum 166_p6.out 7200
cat dump.out >> wdir.out 
