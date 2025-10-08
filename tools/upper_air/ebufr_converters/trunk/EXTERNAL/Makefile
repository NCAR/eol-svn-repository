all: SUN UNIX

SUN:
	f77 -c sun-abort.f close-unit.f ofps-getFXYU.f sun-write-EBUFR-buffer.f
	ar -rv ../Libs/libGsun.a sun-abort.o close-unit.o ofps-getFXYU.o sun-write-EBUFR-buffer.o

UNIX:
	f77 -c abort.f close-unit.f getFXYU.f write-EBUFR-buffer.f
	ar -rv ../Libs/libGunix.a abort.o close-unit.o getFXYU.o write-EBUFR-buffer.o
