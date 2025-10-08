set terminal postscript landscape monochrome dashed "Helvetica" 14
set output 'stnp_var.ps'
set title "GCIP/ESOP-96 Station Pressure Variances"
set xlabel "Variances" 0,0
set ylabel "Counts"
set nokey
set xrange [0:110]
set xtics (0,4,5,10,15,20,25,30,35,42,45,50,55,60,65,70,75,80,85,90,95,100,105,110)
set grid
plot "stnp_var.out" using 3:4 with boxes
--------------------
set terminal postscript landscape monochrome dashed "Helvetica" 14
set output 'slp_var.ps'
set title "GCIP/ESOP-96 Sea Level Pressure Variances"
set xlabel "Variances" 0,0
set ylabel "Counts"
set nokey
set xrange [0:110]
set xtics (0,5,10,15,20,25,30,35,40,45,50,57,60,65,70,75,80,85,90,95,100,105,110)
set grid
plot "slp_var.out" using 3:4 with boxes
--------------------
set terminal postscript landscape monochrome dashed "Helvetica" 14 
set output 'cslp_var.ps'
set title "GCIP/ESOP-96 Calculated Sea Level Pressure Variances" 
set xlabel "Variances" 0,0 
set ylabel "Counts" 
set nokey
set xrange [0:110] 
set xtics (0,5,10,15,20,25,30,35,40,45,50,55,60,65,72,75,80,85,90,95,100,105,110)
set grid
plot "cslp_var.out" using 3:4 with boxes 
--------------------
set terminal postscript landscape monochrome dashed "Helvetica" 14 
set output 'temp_var.ps'  
set title "GCIP/ESOP-96 Temperature Variances"  
set xlabel "Variances" 0,0   
set ylabel "Counts"  
set nokey 
set xrange [-1:110]  
set xtics (0,0.01,2,5,10,15,20,25,30,35,42,45,50,55,60,65,70,75,80,85,90,95,100,105,110) 
set grid
plot "temp_var.out" using 3:4 with boxes  
--------------------
set terminal postscript landscape monochrome dashed "Helvetica" 14  
set output 'dewpt_var.ps'  
set title "GCIP/ESOP-96 Dew Point Temperature Variances"  
set xlabel "Variances" 0,0    
set ylabel "Counts"   
set nokey  
set xrange [-1:110]   
set xtics (0,0.06,2,5,10,15,20,25,30,35,40,45,51,53,60,65,70,75,80,85,90,95,100,105,110)  
set grid
plot "dewpt_var.out" using 3:4 with boxes  
--------------------
set terminal postscript landscape monochrome dashed "Helvetica" 14  
set output 'wdsp_var.ps'  
set title "GCIP/ESOP-96 Wind Speed Variances"  
set xlabel "Variances" 0,0    
set ylabel "Counts"   
set nokey  
set yrange[0:75000]
set xrange[-0.5:55]
set xtics (0.20,5,9.25,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,105,110)  
set grid
plot "wdsp_var.out" using 3:4 with boxes  
--------------------
set terminal postscript landscape monochrome dashed "Helvetica" 14
set output 'wdsp_detail_var.ps'
set title "GCIP/ESOP-96 Wind Speed Variances (Details)"
set xlabel "Variances" 0,0
set ylabel "Counts"
set nokey
set yrange[0:75000]
set xrange[-0.5:12.5]
set xtics (0.01,0.2,1,2,3,4,5,6,7,8,8.75,9.5,12.5,15)
set grid
plot "wdsp_var.out" using 3:4 with boxes
--------------------
set terminal postscript landscape monochrome dashed "Helvetica" 14   
set output 'wdir_var.ps'   
set title "GCIP/ESOP-96 Wind Direction Variances"  
set xlabel "Variances" 0,0     
set ylabel "Counts"    
set nokey   
set yrange[0:40000]
set xrange[-5:46000]
set xtics (0,1500,5000,10000,15500,20000,25000,30000,35000,40000,45000,50000)   
set grid
plot "wdir_var.out" using 3:4 with boxes
--------------------
set terminal postscript landscape monochrome dashed "Helvetica" 14
set output 'wdir_detail_var.ps'
set title "GCIP/ESOP-96 Wind Direction Variances (Details)"
set xlabel "Variances" 0,0
set ylabel "Counts"
set nokey
set yrange[0:40000]
set xrange[-50:15800]
set xtics (0,1000,1500,2500,4000,6000,8000,10000,12000,14000,15500)
set grid
plot "wdir_var.out" using 3:4 with boxes
