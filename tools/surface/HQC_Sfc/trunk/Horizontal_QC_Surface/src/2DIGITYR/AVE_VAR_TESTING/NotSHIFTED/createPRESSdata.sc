changenum 121_p0.out 0
mv dump.out stnp.out
changenum 132_p0.out 2400
cat dump.out >> stnp.out
changenum 152_p0.out 4800
cat dump.out >> stnp.out
changenum 166_p0.out 7200
cat dump.out >> stnp.out

changenum 121_p1.out 0
mv dump.out slp.out
changenum 132_p1.out 2400
cat dump.out >> slp.out
changenum 152_p1.out 4800 
cat dump.out >> slp.out 
changenum 166_p1.out 7200 
cat dump.out >> slp.out

changenum 121_p2.out 0
mv dump.out cslp.out
changenum 132_p2.out 2400
cat dump.out >> cslp.out
changenum 152_p2.out 4800 
cat dump.out >> cslp.out 
changenum 166_p2.out 7200 
cat dump.out >> cslp.out
