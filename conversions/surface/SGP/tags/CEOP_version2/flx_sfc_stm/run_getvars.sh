#!/bin/tcsh

foreach i ( EBBR ECOR IRT SIRS SMOS SWATS MET )
	cd {$i}/raw
	echo " "
	echo "getting the vars from $i in "
	pwd
	echo " "
	./get_vars.sh
	cd -
end

