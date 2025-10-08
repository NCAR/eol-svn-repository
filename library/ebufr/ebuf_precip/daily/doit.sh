#!/bin/csh

foreach i (*.pqc)
	dp2e $i
	gzip $i
end
