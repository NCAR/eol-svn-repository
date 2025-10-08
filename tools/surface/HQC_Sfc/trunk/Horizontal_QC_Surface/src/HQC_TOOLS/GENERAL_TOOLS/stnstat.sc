:
# Print statistics for any set of qcf files
#   flags: -debug = run in debug mode
#          -hall  = use all hourly qcf files
#          -mall  = use all 5 minute qcf files
#          -nominal = expect qcf files to contain both nominal and obs times.
#
# 24 Jan 93 lec
#   Modified for GIDS
#
USAGE="Usage:  $0 [-debug] [-title "REPORT_TITLE_TEXT"] [-nominal] [-hrly | -5min] [ [-hall | -mall] | [FILE1.qcf] [FILE2.qcf] [FILE3.qcf] ... ]"

PROMPT="y";
PRTHALL="n";
PRTMALL="n";
DEBUG="n";
HOURLY="n";
TITLE="Surface Composite Statistics";
while [ $# != 0 ]
do
	case $1 in
		-debug)	DEBUG="y";
				shift;;
		-nom|-nominal)	HOURLY="y";
				shift;;
		-5min|-5minute)	HOURLY="n";
				shift;;
		-title)	TITLE=$2;
				shift;
				shift;;
		-hall)	PRTHALL="y";
		      	PRTMALL="n";
		      	PROMPT="n";
				TITLE="Hourly Surface Composite Statistics (all platforms)"
				shift;;
		-mall)	PRTMALL="y";
		      	PRTHALL="n";
		      	PROMPT="n";
				TITLE="5-Minute Surface Composite Statistics (all platforms)"
				shift;;
		*)	PROMPT="n";
		     PRTMALL="n";
		     PRTHALL="n";
			FILES=$*;
			shift;;
	esac
done
TITLE="\"$TITLE\""
echo "DEBUG=$DEBUG"
echo "PROMPT=$PROMPT"
echo "PRTHALL=$PRTHALL"
echo "PRTMALL=$PRTMALL"
echo "HOURLY=$HOURLY"
echo "TITLE=$TITLE"
echo "FILES=$FILES"

echo
if [ $PRTHALL = "y" ]
then
   echo "Print statistics for all hourly qcf files"
elif [ $PRTMALL = "y" ]
then
   echo "Print statistics for all 5 minute qcf files"
elif [ $PROMPT = "y" ]
then
   echo "Print statistics for qcf files by prompting"
elif [ $PROMPT = "n" ]
then
   echo "Print statistics for all supplied qcf files"
fi
#echo
#echo -n "Continue[y/n]? "
#read reply
#if [ $reply != "y" ]
#then
#  echo "exiting..."
#  exit
#fi

if [ $DEBUG = "y" ]
then
   echo "  DEBUG mode ON"
fi

echo "Begin  `date`"

QCPATH="."
IN="$QCPATH/stnstat.in"

OUT="./stnstat.out"

if [ -r $IN ]
then
   echo "$IN file already exists; Removing..."
   rm -f $IN
fi
if [ $PRTHALL = "y" ]
then
echo "Process all hourly files"
# Pathnames must be verified.
  ls ./*.qcf > $IN
#  ls ./ASOS_5min_may26-27.qcf > $IN
#  ls /spare1/gids/asos/asos60_*.qcf > $IN
#  ls /spare1/gids/awosq/awosq60_*.qcf >> $IN
#  ls /spare1/gids/hpcn/out/final/hplains_*.qcf >> $IN
#  ls /spare1/gids/wdpn/out/final/wdpn_*.qcf >> $IN
#  ls /spare1/gids/ilmeso/out/icn_*.qcf >> $IN
#  ls /spare1/gids/ncdcsao/out/nominal/final/ncdc_*.qcf >> $IN
#  ls /spare1/gids/ncdcsao/out/specials/final/spncdc_*.qcf >> $IN
#  ls /spare1/gids/profs/data/hourly/profs_*.qcf >> $IN
#  ls /spare1/gids/pam/pam_*.qcf >> $IN
   echo "quit" >> $IN

      $QCPATH/stnstat -nominal < $IN > $OUT

#   enscript -b$TITLE -r -FTimes-Bold14 -fCourier9 -r $OUT
elif [ $PRTMALL = "y" ]
then
echo "Process all 5minute files"
   ls *.qcf> $IN
   echo "quit" >> $IN

   $QCPATH/stnstat -nominal < $IN > $OUT

elif [ $PROMPT = "y" ]
then
   if [ $HOURLY = "y" ]
   then
      $QCPATH/stnstat -nominal > $OUT
   else
      $QCPATH/stnstat > $OUT
   fi
#   enscript -b$TITLE -r -FTimes-Bold14 -fCourier9 -r $OUT
elif [ $PROMPT = "n" ]
then
   for f in $FILES
   do
      echo $f >> $IN
   done
   echo "quit" >> $IN
   if [ $HOURLY = "y" ]
   then
echo "running: $QCPATH/stnstat -nominal < $IN > $OUT"
      $QCPATH/stnstat -nominal < $IN > $OUT
   else
      $QCPATH/stnstat < $IN > $OUT
   fi
#echo  "running: enscript -t$TITLE -r -FTimes-Bold14 -fCourier9 -r $OUT"
#   enscript -t$TITLE -r -FTimes-Bold14 -fCourier9 -r $OUT
fi

#rm $IN
echo
echo "End of Job!     Finish: `date`"
