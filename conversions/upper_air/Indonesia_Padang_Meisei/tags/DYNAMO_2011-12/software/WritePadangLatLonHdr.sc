for f in *.cls
do
   echo "--------------------------------------------------------"
   echo "WritePadangLatLonHeader.pl $f"
   WritePadangLatLonHeader.pl $f
   diff $f $f.corrLoc
done
echo "All ESC Class file Bad Lat/Lons corrected! Run WritePadangLatLonHeader.pl on all ESC data files."
