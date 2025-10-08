SHELL = /bin/sh
#    Makefile for readasos library
#

BIN=../bin
FFLAGS = -g

.KEEP_STATE:

qcflib: qcflib.a(opnfil.o clsfil.o analyz.o rdsite.o rdsite-asos.o staalt.o \
        msg.o msgi.o msgr.o qcfhdr.o qcfrec.o qreset.o ck2m.o getyes.o \
        cm2f.o cf2m.o fahren.o celsus.o ckel2c.o cc2kel.o cin2mb.o \
        gemsea.o token.o layer.o vipr.o blocker.o pwea.o vis.o  \
        cmi2me.o cover.o intchk.o cin2mm.o hand.o julian.o msgc.o \
        chtoken.o reject.o advance.o handarh.o juliancvt.o)
	ar rv $@.a $?
	chmod 0755 $@.a

qcflib.a(%.o): %.o
	@true
