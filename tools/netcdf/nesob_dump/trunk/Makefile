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

#LIBS=		-L${JLOCAL}/lib -lnetcdf
LIBS=           -L${JLOCAL}/lib -Wl,-Bstatic -lnetcdf -Wl,-Bdynamic
INCLUDES=	-I${JLOCAL}/include

##CC=gcc
CC=cc
CFLAGS=-g

# PROGS=nesob_dump makeQCF_record process_nesob

# all:	${PROGS}

nesob_dump: nesob_dump.o vardata.o dumplib.o
	$(CC) $(CFLAGS) $(INCLUDES) -o nesob_dump nesob_dump.o vardata.o dumplib.o $(LIBS)

#  makeQCF_record: makeQCF_record.o vardata.o dumplib.o
#	$(CC) $(CFLAGS) $(INCLUDES) -o makeQCF_record makeQCF_record.o vardata.o dumplib.o $(LIBS)
#	
# process_nesob: process_nesob.o vardata.o dumplib.o
#	$(CC) $(CFLAGS) $(INCLUDES) -o process_nesob vardata.o dumplib.o $(LIBS)
	
clean:
	rm *.o
