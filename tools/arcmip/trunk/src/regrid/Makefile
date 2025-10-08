
CFLAGS=-g
#CC=gcc
CC=cc

NETCDFINC=-I/opt/include
NETCDFLIB=-L/opt/lib -lnetcdf
GDINC=-I/web/include
GDLIB=-L/web/lib -lgd

PROGS= mkease findnn printnn ifnn mkdom mkdist regrid nnmap


all:	$(PROGS)


mkease:	mkease.o ezlhconv.o
	$(CC) $(CFLAGS) -o mkease mkease.o ezlhconv.o -lm

mkease.c:	ezlhconv.h arcmip_ease.h

ezlhconv.c:	ezlhconv.h

findnn.c:	ezlhconv.h

findnn.o:	findnn.c
	$(CC) $(CFLAGS) $(NETCDFINC) -c findnn.c

findnn:	findnn.o
	$(CC) $(CFLAGS) -o findnn findnn.o $(NETCDFLIB) -lm

printnn:	printnn.c
	$(CC) $(CFLAGS) -o printnn printnn.c

nnmap:	nnmap.c ezlhconv.o arcmip_ease.h
	$(CC) $(CFLAGS) $(GDINC) $(NETCDFINC) -o nnmap nnmap.c ezlhconv.o $(GDLIB) $(NETCDFLIB) -lm

ifnn:	ifnn.c
	$(CC) $(CFLAGS) $(NETCDFINC) -o ifnn ifnn.c $(NETCDFLIB) -lm

mkdom:	mkdom.c mkdomdata.c getelev.c
	$(CC) $(CFLAGS) $(NETCDFINC) -o mkdom mkdom.c mkdomdata.c getelev.c $(NETCDFLIB) -lm

mkdist:	mkdist.c mkdistdata.c
	$(CC) $(CFLAGS) $(NETCDFINC) -o mkdist mkdist.c mkdistdata.c $(NETCDFLIB) -lm

regrid:	regrid.c arcmip_ease.h
	$(CC) $(CFLAGS) -o regrid $(NETCDFINC) regrid.c $(NETCDFLIB) -lm

clean:
	$(RM) -f *.o core a.out $(PROGS)
