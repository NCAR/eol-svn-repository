for f in *.CSV
do
   echo "--------------------------------------------------------"
   echo "preproc_raw_data.pl $f"
   preproc_raw_data.pl $f
done
echo "Times fixed for all Raw *.CSV files!"

