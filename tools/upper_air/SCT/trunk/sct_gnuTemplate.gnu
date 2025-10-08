# @ Monica Jacobs- July 2011
# gnuTemplate.gnu- This is the template used in SoundingGraph.java to create gnuplot scripts with 
# the variables filled in

#sets plot to output as a .gif file
set terminal gif


set size "SIZE" #sets the size of the plot
set title "TITLE" #sets the plot title
set datafile missing "?" #ignores any data containing a question mark
set ylabel "YLABEL" #sets the label for the y-axis
set nokey #does not display a legend on the plot
set xlabel "XLABEL" #sets the label for the x-axis
set output "IMAGE"#sets the name of the image the plot is being output as
plot "FILE"
 
~
~
~
~
