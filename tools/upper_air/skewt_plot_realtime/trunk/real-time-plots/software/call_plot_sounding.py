#!/opt/local/anaconda3/bin/python

##Module-----------------------------------------------------------------------
## <p>The call_plot_soundings.py script is a script called from the Real-time-plots.pl
## script and does the following:</p>
## <ol>
##   <li>Retrieve the arguments sent from the Real-time-plots.pl to save the output to a log file.
##   <li>Activate the conda environment (skewt-env) that has been set up on eol-hurricane for
##       creating the skew-t plots for the field catalog.</li> 
##   <li>Call the script that does the plotting of the skew-t.<\li>
## </ol>
##
## @author Janet Scannell 
## @version 1.0 April 2025
##
###Module-----------------------------------------------------------------------
#

import sys
import subprocess

# Activate conda environment
subprocess.run(". /opt/local/anaconda3/etc/profile.d/conda.sh && conda activate skewt-env",
        shell=True, check=True)

# Get all arguments passed in from the command line.

arguments = ' ' . join(sys.argv[1:])

# Call the plot routine that will create the plot wihin the skewt environment

argument_list = sys.argv[1] + " >>" + sys.argv[2] + " 2>>" + sys.argv[3]
#print(argument_list)
subprocess.run("/net/work/Projects/test_GTS_BUFR_NearRT_Plot_Generation/real-time-plots/software/plot_sounding.py " + argument_list, shell=True, check=True)

