for f in *.cls
do
   echo "--------------------------------------------------------"
   echo "convertESCLocalToUTC.pl $f -7 0"
   convertESCLocalToUTC.pl $f -7 0
done
echo "All ESC Class file Local Times converted to UTC! Run convertESCLocalToUTC.pl on all ESC data files."

