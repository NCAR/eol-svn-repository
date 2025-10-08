#!/bin/tcsh

foreach i ( EBBR ECOR IRT SIRS SMOS SWATS TWR10x )
	cd {$i}/raw
	echo "getting the $i header..."
	ncdump -h *20040615*cdf > ../eop4_{$i}.header
	cd -
end

