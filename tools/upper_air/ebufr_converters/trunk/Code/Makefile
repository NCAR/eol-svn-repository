#
# $Id: Makefile,v 1.3 1992/08/18 15:14:37 john Exp $
#
#
# $Log: Makefile,v $
# Revision 1.3  1992/08/18 15:14:37  john
# Added (more) copyright information.
#
# Revision 1.2  1992/08/17  22:07:40  john
# Added copyright information.
#
# Revision 1.1  1992/08/17  14:49:48  john
# Initial revision
#
#
#
# Copyright Notice for the E-BUFR Generic Encoder Routines
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
#* this file with other programs, and to distribute those without any
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

SHELL = /bin/sh
#FFLAGS = -g -ansi
FFLAGS = -fast -O3 -ansi
UNINCLUDE = ../Tools/uninclude

#GERDIR= /data10/darren/ebuf_utils/generic_encoder
GERDIR= ..
OBJDIR= $(GERDIR)/Objects
DISTDIR= $(GERDIR)/Distribution
OBJECTS= buffer.o encode-observation.o misc.o write-EBUFR-header-records.o
SOURCE=  buffer.f encode-observation.f misc.f write-EBUFR-header-records.f

SUBDIR= Write-rec-sec-level Read-routines observation-table file-table code-table parameter-table FXY-table static-table read-Control-file

all:	${OBJECTS} ${SUBDIR}
	echo "Finished with compile"
	touch ../Libs/Code.a
	rm ../Libs/Code.a
	ar ruv ../Libs/Code.a ../Objects/*.o
	ranlib ../Libs/Code.a

FRC:

.RECURSIVE: ${SUBDIR}

.SUFFIXES: .for .f .o ;

.for.o:
	f77 -c $(FFLAGS) $<
	cp $@ ${OBJDIR}

.f.o:
	f77 -c $(FFLAGS) $<
	cp $@ ${OBJDIR}

${SUBDIR}: FRC
	cd $@; ${MAKE} ${MFLAGS} OBJDIR=../${OBJDIR}  all

objclean:
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} objclean); done
	-rm -f *.o

distribute:
	for i in ${SUBDIR} ; \
	  do (cd $$i ; ${MAKE} ${MFLAGS} DISTDIR=${DISTDIR} distribute ) ; done
	for i in ${SOURCE} ; \
	  do cat ":ACopyright.f" $$i | \
	     ${UNINCLUDE} > ${DISTDIR}/Code/$$i ; done
