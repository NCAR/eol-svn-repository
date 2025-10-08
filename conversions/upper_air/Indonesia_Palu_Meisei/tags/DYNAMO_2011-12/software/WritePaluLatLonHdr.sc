for f in *.cls
do
   echo "--------------------------------------------------------"
   echo "WritePaluLatLonHeader.pl $f"
   WritePaluLatLonHeader.pl $f
   diff $f $f.corrLoc
done
echo "All ESC Class file Bad Lat/Lons corrected! Run WritePaluLatLonHeader.pl on all ESC data files."
