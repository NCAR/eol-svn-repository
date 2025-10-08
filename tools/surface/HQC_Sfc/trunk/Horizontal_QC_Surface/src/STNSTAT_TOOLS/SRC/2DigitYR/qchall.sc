:
# Run the qc program for hourly surface data

USAGE="Usage:  $0"

case $# in
	0)	;;
	*)	echo $USAGE;
		exit 1;;
esac

echo
echo "Running qc program for all hourly surface data"
echo
echo -n "Continue[y/n]? "
read reply
if [ $reply != "y" ]
then
  echo "exiting..."
  exit
fi

echo "Begin QC All Hourly: `date`" |tee -a $QCPATH/qchall.log

QCPATH="/fest/hrly_sfc/qc"
rm -f $QCPATH/qchall.log

ASOS="/fest/hrly_sfc/asos/work"
AWOS="/fest/hrly_sfc/awosq/work"
HPLAINS="/fest/hrly_sfc/hplains/work"
WDPN="/fest/hrly_sfc/wdpn/work"
ISWS="/fest/hrly_sfc/isws/work"
NCDC="/fest/hrly_sfc/ncdc/work"
PROFS="/fest/hrly_sfc/profs/work"
PAM="/fest/hrly_sfc/pam/work"

echo "Processing ASOS data..." |tee -a $QCPATH/qchall.log
cd $ASOS
$QCPATH/qcsc asos60_*.0qc |tee $QCPATH/qchall.log

echo "Processing AWOS data..." |tee -a $QCPATH/qchall.log
cd $AWOS
$QCPATH/qcsc awosq60_*.0qc |tee -a $QCPATH/qchall.log

echo "Processing HPLAINS data..." |tee -a $QCPATH/qchall.log
cd $HPLAINS
$QCPATH/qcsc hplains_*.0qc |tee -a $QCPATH/qchall.log

echo "Processing WDPN data..." |tee -a $QCPATH/qchall.log
cd $WDPN
$QCPATH/qcsc wdpn_*.0qc |tee -a $QCPATH/qchall.log

echo "Processing PROFS data..." |tee -a $QCPATH/qchall.log
cd $PROFS
$QCPATH/qcsc profs_*.0qc |tee -a $QCPATH/qchall.log

echo "Processing NCDC data..." |tee -a $QCPATH/qchall.log
cd $NCDC
$QCPATH/qcsc ncdc_*.0qc |tee -a $QCPATH/qchall.log

echo "Processing ISWS data..." |tee -a $QCPATH/qchall.log
cd $ISWS
$QCPATH/qcsc isws60_*.0qc |tee -a $QCPATH/qchall.log

echo "Processing PAM data..." |tee -a $QCPATH/qchall.log
cd $PAM
#$QCPATH/qcsc pam_*.0qc |tee -a $QCPATH/qchall.log

echo "Running final summary report..." |tee -a $QCPATH/qchall.log
cd $QCPATH
$QCPATH/stnstat.sc -hall

echo " " |tee -a $QCPATH/qchall.log
echo "End: QC All Hourly: `date`" |tee -a $QCPATH/qchall.log
echo " " |tee -a $QCPATH/qchall.log
echo "End of Job!" |tee -a $QCPATH/qchall.log
