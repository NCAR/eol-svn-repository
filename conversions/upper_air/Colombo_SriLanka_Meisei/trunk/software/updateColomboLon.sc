for f in *.cls
do
   echo "--------------------------------------------------------"
   echo "WriteLatLonHeader_V1.pl $f"
   WriteLatLonHeader_V1.pl $f
done
echo "Release Lons fixed for all dayfiles (*.cls) files!"

