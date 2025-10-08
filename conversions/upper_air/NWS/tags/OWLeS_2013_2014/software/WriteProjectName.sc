for f in *.cls
do
   echo "--------------------------------------------------------"
   echo "WriteProjectName.pl $f"
   WriteProjectName.pl $f
   diff $f $f.corr
done
echo "All ESC Class file Wrong Project Name corrected! Run WriteProjectName.pl on all ESC data files."
