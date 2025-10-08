#
# $Id: Makefile,v 1.2 1992/08/18 15:14:56 john Exp $
#
#
# $Log: Makefile,v $
# Revision 1.2  1992/08/18 15:14:56  john
# Added (more) copyright information.
#
# Revision 1.1  1992/08/17  14:52:32  john
# Initial
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
FFLAGS = -ansi -fast -O3
UNINCLUDE = ../../Tools/uninclude

# Make sure that this file name doesn't normally occur in this directory!
DistName= read-control-file-dist.f

OBJDIR= ../../Objects
OBJECTS= initialize-control-data.o read-control-file.o read-file-descriptions.o read-observation-descriptions.o read-observation-list.o read-one-file-description.o read-static-information.o ctl_file_access.o

# Flags passed down to subdirectory makefiles.
MFLAGS=

all:	${OBJECTS}

#${OBJECTS}: ../../Commons/ctl_file.f

.SUFFIXES: .for .f .o ;

.for.o:
	f77 -c $(FFLAGS) $<
	cp $@ ${OBJDIR}

.f.o:
	f77 -c $(FFLAGS) $<
	cp $@ ${OBJDIR}

objclean:
	-rm *.o

distribute:
	rm -f ${DistName}
	for i in `echo *.f` ; \
		do ( ${UNINCLUDE} < $$i >> ${DistName} ; \
		     echo ""            >> ${DistName} ; \
		     echo ""            >> ${DistName} ; \
		     echo ""          >> ${DistName} ; ) ; done
	mv -f ${DistName} ${DISTDIR}/Code
