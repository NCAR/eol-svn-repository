Version 1
********************************************************************************

The plot_ceop.pl script uses the ceop_template, gnuplot and convert to create 
.gif plots of data in the CEOP formats.  The template should be modified to set the desired title, 
xrange, formats, missing value etc.  Simply execute plot_ceop.pl with no arguments to see a 
usage statement.  This script can also be used to plot qcf data.

Dan Sullivan
October, 2003

Based on the plotit.pl script written in May, 2003.
********************************************************************************
This version does not write out what is awks to files named 
<station>_<format>.dat, even though the directions on use says it does.
Instead the AWK command is run once for each plot created.  This slows the code
down significantly.  To avoid long wait times with large data volumes, grep each
station to a separate file then use a series of commands in a script like:

plot_ceop.pl sfc -o sfc -d ../out/final/GAPP_SGP_C1_Lamont_20021001_20030331.sfc
plot_ceop.pl sfc -o sfc -d ../out/final/GAPP_SGP_E10_Tyro_20021001_20030331.sfc
plot_ceop.pl sfc -o sfc -d ../out/final/GAPP_SGP_E11_Byron_20021001_20030331.sfc
plot_ceop.pl sfc -o sfc -d ../out/final/GAPP_SGP_E12_Pawhuska_20021001_20030331.
sfc

