#!/bin/tcsh

foreach i ( EBBR ECOR IRT SIRS SMOS SWATS MET )
	cd {$i}/raw
	echo "getting the vars from $i..."
	get_vars.sh
	cd -
end

