for f in *.CSV
do
   echo "--------------------------------------------------------"
   echo "preproc_raw_Meisei_data.pl $f"
   preproc_raw_Meisei_data.pl $f
done
echo "Times fixed for all Raw *.CSV files! Executed preproc_raw_Meisei_data.pl on all raw data."

