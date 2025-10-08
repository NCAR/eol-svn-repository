#!/bin/csh

foreach i (*.qcf)
	nh2e_4dyr $i
	gzip $i
end
