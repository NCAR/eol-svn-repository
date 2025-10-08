#!/bin/tcsh

foreach i ( ECOR IRT SIRS SMOS SWATS TWR10x )
	cd {$i}/raw
	echo "getting the vars from $i..."
	get_vars.sh
	cd -
end

