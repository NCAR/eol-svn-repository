Run "gnuplot" on the following.

----------------
set terminal postscript landscape monochrome dashed "Helvetica" 14
set output 'stnp_var.ps'
set title "TREX Hourly  Station Pressure Variances - RUN3"
set xlabel "Variance Values" 0,0
set ylabel "Counts"
set nokey
set xrange [0:80]
set xtics (0,5,9,15,20,25,34,40,50,60,70)
set grid
plot "stnp_var.out" using 3:4 with boxes
--------------------
set terminal postscript landscape monochrome dashed "Helvetica" 14
set output 'slp_var.ps'
set title "TREX Hourly  Sea Level Pressure Variances - RUN3"
set xlabel "Variance Values" 0,0
set ylabel "Counts"
set nokey
set xrange [0:80]
set xtics (0,5,15,25,35,50,60,70)
set grid
plot "slp_var.out" using 3:4 with boxes
--------------------
set terminal postscript landscape monochrome dashed "Helvetica" 14 
set output 'cslp_var.ps'
set title "TREX Hourly  Calculated Sea Level Pressure Variances - RUN3" 
set xlabel "Variance Values" 0,0 
set ylabel "Counts" 
set nokey
set xrange [0:80] 
set xtics (0,5,11,20,30,41,50,60,68)
set grid
plot "cslp_var.out" using 3:4 with boxes 
--------------------
set terminal postscript landscape monochrome dashed "Helvetica" 14 
set output 'temp_var.ps'  
set title "TREX Hourly  Temperature Variances - RUN3"  
set xlabel "Variance Values" 0,0   
set ylabel "Counts"  
set nokey 
set xrange [0:70]  
set xtics (0,3,8,15,20,28,30,40,46,55,60)
set grid
plot "temp_var.out" using 3:4 with boxes  
--------------------
set terminal postscript landscape monochrome dashed "Helvetica" 14  
set output 'dewpt_var.ps'  
set title "TREX Hourly  Dew Point Temperature Variances - RUN3"  
set xlabel "Variance Values" 0,0    
set ylabel "Counts"   
set nokey  
set xrange [0:70]
set xtics (0,3,10,15,20,25,29,35,40,50,60)
set grid
plot "dewpt_var.out" using 3:4 with boxes  
--------------------
set terminal postscript landscape monochrome dashed "Helvetica" 14  
set output 'wdsp_var.ps'  
set title "TREX Hourly  Wind Speed Variances - RUN3"  
set xlabel "Variance Values" 0,0    
set ylabel "Counts"   
set nokey  
set yrange[0:55000]
set xrange[0:20]
set xtics (0.5,1.5,3,4,5,6,7.5,8,9,10,11,12,13.5,15,16,18,20)
set grid
plot "wdsp_var.out" using 3:4 with boxes  
--------------------
set terminal postscript landscape monochrome dashed "Helvetica" 14
set output 'wdir_var.ps'
set title "TREX Hourly  Wind Direction Variances - RUN3"
set xlabel "Variance Values" 0,0
set ylabel "Counts"
set nokey
set yrange[0:20000]
set xrange[0:25000]
set xtics (0,3500,6000,9750,12000,16500,20000,25000)
set grid
plot "wdir_var.out" using 3:4 with boxes
