#
# $Id: Makefile,v 2.10 1995/03/10 17:43:37 john Exp $
#
# Makefile for E-BUFR Read Routines
#
#*
#* Copyright (C) 1992 by UCAR
#*      University Corporation for Atmospheric Research
#*
#* Permission to use, copy, modify, and distribute this software and its
#* documentation for any purpose and without fee is hereby granted, provided
#* that the above copyright notice and this permission notice appear in all
#* copies and in all supporting documentation, and that the name of UCAR
#* not be used in advertising or publicity pertaining to distribution of
#* the software in source or compiled form, whether alone or as part of
#* another program, without specific, written prior permission.
#*
#* Any modifications that you make to this software must be explicitly
#* identified as such and include your name and the date of modification.
#*
#* In addition, UCAR gives you permission to link the compiled version of
#* this software with other programs, and to distribute those without any
#* restriction coming from the use of this file.
#*
#* Although this software is provided in the hope that it will be useful,
#* UCAR makes no representations about the suitability of this software
#* for any purpose. This software and any accompanying written materials
#* are provided "as is" without warranty of any kind. UCAR expressly
#* disclaims all warranties of any kind, either express or implied,
#* including but not limited to the implied warranties of merchantibility
#* and fitness for a particular purpose. UCAR does not indemnify any
#* infringement of copyright, patent, or trademark through use or
#* modification of this software.
#*
#* UCAR does not normally provide maintenance or updates for its software.
#
#
#
# Written at UCAR/Office of Field Project Support.
#
#

SHELL = /bin/sh

# where you want the libeb.a library to end up
LIBDIR=.

# debug
FFLAGS = -u -g -C

# optimized (on Sun)
#FFLAGS = -u -fast -O3 -pic

# alternate optimized:
#FFLAGS = -O

# some systems need the following 2 lines uncommented and/or edited
#    contact your local system administrator
#COMPILE.f = f77
#LINK.f = f77

OBJECTS=bd.o bferr.o getval.o \
	bfinfo.o bfsec3.o bfsec4.o \
	ebread.o getobs.o getrec.o \
	lddesc.o nxtbts.o rdhrec.o \
	elmnt.o rdrec.o \
	getnus.o getcfs.o ldtbls.o \
	bfsec0.o bfsec1.o
INCLUDES=ebufr_parm.inc ebufr_obs.inc \
	 ebufr_vars.inc ebufr_data.inc

.KEEP_STATE:

all:	exampl bare

exampl:	exampl.o $(LIBDIR)/libeb.a $(INCLUDES)
	$(LINK.f) -o exampl exampl.o $(LIBDIR)/libeb.a

bare:	bare.o $(LIBDIR)/libeb.a $(INCLUDES)
	$(LINK.f) -o bare bare.o $(LIBDIR)/libeb.a

$(LIBDIR)/libeb.a:	$(OBJECTS)
	ar ruv $(LIBDIR)/libeb.a $(OBJECTS)
#	ranlib $(LIBDIR)/libeb.a

sharelib:	$(OBJECTS)
	ld -o libeb.so.1.0 -assert pure-text $(OBJECTS)

$(OBJECTS):	$(INCLUDES)

$(INCLUDES):

clean:
	rm -f *.o core *.trace fort.*
