:
# Run the qc program for 5-minute surface data

USAGE="Usage:  $0"

case $# in
	0)	;;
	*)	echo $USAGE;
		exit 1;;
esac

echo
echo "Running qc program for all 5 minute surface data"
echo
echo -n "Continue[y/n]? "
#read reply
#if [ $reply != "y" ]
#then
#  echo "exiting..."
#  exit
#fi

echo "Begin QC All  5 minute: `date`"

QCPATH="/fest/hrly_sfc/qc"
rm -f $QCPATH/qcmall.log

ASOS="/fest/other_sfc/asos5/work"
ISWS="/fest/other_sfc/isws5/work"
PROFS="/fest/other_sfc/profs/work"
PAM="/fest/other_sfc/pam/work"

echo "Processing ASOS data..."
cd $ASOS
$QCPATH/qcsc asos5_*.0qc |tee $QCPATH/qcmall.log

echo "Processing PROFS data..."
cd $PROFS
$QCPATH/qcsc profs5_*.0qc |tee -a $QCPATH/qcmall.log

echo "Processing ISWS data..."
cd $ISWS
$QCPATH/qcsc isws5_*.0qc |tee -a $QCPATH/qcmall.log

echo "Processing PAM data..."
cd $PAM
#$QCPATH/qcsc pam5_*.0qc |tee -a $QCPATH/qcmall.log

echo "Running final summary report..."
cd $QCPATH
$QCPATH/stnstat.sc -mall

echo
echo "End: QC All 5 minute: `date`"
echo
echo End of Job!
