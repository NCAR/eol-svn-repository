#!/bin/tcsh

foreach i ( EBBR ECOR IRT SIRS SMOS SWATS TWR10x )
	cd ../{$i}/raw
	echo "getting the $i header..."
	ncdump -h *20080615*cdf > ../../version2_{$i}.header
	cd -
end

