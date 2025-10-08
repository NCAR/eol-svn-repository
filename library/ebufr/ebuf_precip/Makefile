#
# $Id: Makefile,v 1.3 1994/07/08 16:35:34 john Exp $
#

#
# simple Makefile for p2e
#

#FFLAGS=-u -g -xld
FFLAGS=-u -fast -O3

all:	p2e dp2e cleanfort

p2e_4dyr:	p2e_4dyr.f
	f77 $(FFLAGS) -o p2e_4dyr p2e_4dyr.f -L/home/john/lib -lGER_S54

p2e:	p2e.f
	f77 $(FFLAGS) -o p2e p2e.f -L/home/john/lib -lGER_S54

dp2e:	dp2e.f
	f77 $(FFLAGS) -o dp2e dp2e.f -L/home/john/lib -lGER_S54

cleanfort:
	rm -f *.trace
	rm -f fort.*
	rm -f core

clean:	cleanfort
	rm -f p2e dp2e
