#!/bin/tcsh

foreach i (gndrad mettwr2h mettwr4h skyrad)
	cd $i
	echo "getting the vars from $i..."
	get_vars.sh
	cd -
end

