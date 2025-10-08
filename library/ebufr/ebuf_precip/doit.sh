#!/bin/csh

foreach i (*.pqc)
	p2e_4dyr $i
	gzip $i
end
