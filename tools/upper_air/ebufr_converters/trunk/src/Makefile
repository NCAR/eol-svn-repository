all: class2ebufr_Solaris class2ebufr_Unix ebufr2class

class2ebufr_Solaris:
	f77 -o ../bin/class2ebufr_Solaris c2e.f ../Libs/Code.a ../Libs/libGsun.a

class2ebufr_Unix:
	f77 -o ../bin/class2ebufr_Unix c2e.f ../Libs/Code.a ../Libs/libGunix.a

ebufr2class:
	f77 -o ../bin/ebufr2class -g ebscf.f ../Libs/libeb.a -I../Commons
