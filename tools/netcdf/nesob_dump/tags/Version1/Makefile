#Makefile for ncdump(1).

c_sources   = nesob_dump.c vardata.c dumplib.c MakeQCF_record.c
headers     = nesob_dump.h vardata.h dumplib.h

lib_netcdf  = /usr/local/lib/libnetcdf.a
ld_netcdf   = -L/usr/local/lib -lnetcdf

INCLUDES    = -I/usr/local/include

PROGRAM     = nesob_dump 
PROGRAM2    = MakeQCF_record

OBJS        = nesob_dump.o vardata.o dumplib.o

$(PROGRAM): $(lib_netcdf) $(OBJS)
	$(LINK.c) $(OBJS) $(ld_netcdf) $(LIBS)

	mv a.out nesob_dump

$(PROGRAM2): $(lib_netcdf) MakeQCF_record.o
	$(LINK.c) MakeQCF_record.o $(ld_netcdf) $(LIBS)

	mv a.out MakeQCF_record
