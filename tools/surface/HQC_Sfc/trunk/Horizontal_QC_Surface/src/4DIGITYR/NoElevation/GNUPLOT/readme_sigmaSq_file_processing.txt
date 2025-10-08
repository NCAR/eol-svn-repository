How To plot the Sigma square (variances) AND Generate Table 3-2 for the HQC Processing/Documentation
---------------------------------------------------------------------------------------------------- 

Do the following:

1. In the SIGMASQ dir (post HQC processing of a surface composite),
make a file named file_list.txt that is a listing of all the "final"
sigmasq files. Warning: You must remove the .gz from the ends of every
line in the file_list.txt file. The code assumes the files are gzipped.
Each line in that file should contain the name of only one file such as
"20060891000.sig". Beware that once HQC has completed, if searching has
variance occurred during HQC  (and it generally does) the dir will also
contain *.sig.bak.gz files. Remove the names of all those "*.bak" files 
from the file_list.txt file. Only include the *.sig.gz files and remove the
".gz" suffix from those listed. If you leave the *.bak.gz files in, 
duplicate variance (sigmasq) values will be included in the final counts. 
Once the file_list.txt contains a unique list of the sigmasq files,
then execute:     compute_var_cts     in the SIGMASQ directory.

This will generate file: var_ct.out

Note that at the very end of the var_ct.out file there are 2 tables. One
of these is the table that is always included in the final composite document's
HQC section.  The table is entitled, "Table 3-2 Ranges of HQC flag limit values for Current project."
Note the warning in the file regarding >180 degree differences. Those should be set
to 180 degrees since you can't have a difference of > 180 degrees.

2. Create a GNUPLOT dir in the dir above the SIGMASQ dir.
In that new dir, copy the var_ct.out file. Run the split_pts script.
This will split  var_ct.out file into parameter *.out files:

   cslp_var.out    slp_var.out     temp_var.out    wdir_var.out
   dewpt_var.out   stnp_var.out    wdsp_var.out

3. See the xxxx_Var_plot.cmd file. This file contains sets of gnuplot commands
that can be cut and pasted into a gnuplot window to generate *.ps  (postscript)
image files.  If you don't include the final "ps" line in each parameter section,
the image will display in the gnuplot window.   Otherwise, generate the *.ps files:

   cslp_var.ps    dewpt_var.ps   slp_var.ps     stnp_var.ps    temp_var.ps    
   wdir_var.ps    wdsp_var.ps

Then run the "convertit" script to generate the final *.gif images:
   cslp_var.gif    slp_var.gif     temp_var.gif    wdsp_var.gif
   dewpt_var.gif   stnp_var.gif    wdir_var.gif

4. Often these final *.gif images are included on the DMG internal web pages for
that project's HQC processing as documentation.

5. The two "products" generated from this processing/plotting are: final *.gif images
of the variances (sigmasq values) which are just another sanity check AND Table 3-2 which
must be included in the Composite documentation that is delivered with each data order.

---end of file---
