#
#    Makefile for converting and processing of NCDC precip
#
# 06/03/96 ds

HOME=/data3/NCDC_Precip/precip_hrly
BIN=$(HOME)/exe

CC=cc
#CFLAGS = -g
CFLAGS=-O

PROGS = process_precip jac2 pqc sck snm create_stationlist dayfs_15min dayfs_hourly convert_precip_to_ebufr

all:	${PROGS}

process_precip: process_precip.c
	$(LINK.c) -o $(BIN)/process_precip process_precip.c

jac2:	jac2.c
	$(CC) $(CFLAGS) -o $(BIN)/jac2 jac2.c

pqc:	pqc.c
	$(CC) $(CFLAGS) -o $(BIN)/pqc pqc.c

sck:	sck.c
	$(CC) $(CFLAGS) -o $(BIN)/sck sck.c

snm:	snm.c
	$(CC) $(CFLAGS) -o $(BIN)/snm snm.c

create_stationlist: create_stationlist.c
	$(CC) $(CFLAGS) -o $(BIN)/create_stationlist create_stationlist.c

dayfs_15min: dayfs_15min.c
	$(CC) $(CFLAGS) -o $(BIN)/dayfs_15min dayfs_15min.c

dayfs_hourly: dayfs_hourly.c
	$(CC) $(CFLAGS) -o $(BIN)/dayfs_hourly dayfs_hourly.c

convert_precip_to_ebufr: convert_precip_to_ebufr.c
	$(LINK.c) -o $(BIN)/convert_precip_to_ebufr convert_precip_to_ebufr.c

