SHELL=/bin/sh
#
#    Makefile for s/w that converts
#    NESOB data to QCF format.
#
#    1 aug 97, ds 
#	 for thunder
#
#    8 sep 2009, jag
#	for tsunami and merlot

LIBPATH= /opt/lib
INCLUDES=-Iopt/include

CC=gcc
##CC=cc
CFLAGS=-g

# PROGS=nesob_dump makeQCF_record process_nesob

# all:	${PROGS}

nesob_dump: nesob_dump.o vardata.o dumplib.o
	$(CC) $(CFLAGS) $(INCLUDES) -o nesob_dump nesob_dump.o vardata.o
	dumplib.o -L$(LIBPATH) -lnetcdf

#  makeQCF_record: makeQCF_record.o vardata.o dumplib.o
#	$(CC) $(CFLAGS) $(INCLUDES) -o makeQCF_record makeQCF_record.o vardata.o dumplib.o -L$(LIBPATH) -lnetcdf
#	
# process_nesob: process_nesob.o vardata.o dumplib.o
#	$(CC) $(CFLAGS) $(INCLUDES) -o process_nesob vardata.o dumplib.o -L$(LIBPATH) -lnetcdf
	
clean:
	rm *.o
