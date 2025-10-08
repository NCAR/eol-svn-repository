for f in *.cls
do
   echo "--------------------------------------------------------"
   echo "convertESCLocalToUTC.pl $f -5 -30"
   convertESCLocalToUTC.pl $f -5 -30
done
echo "All ESC Class file Local Times converted to UTC! Run convertESCLocalToUTC.pl on all ESC data files."

