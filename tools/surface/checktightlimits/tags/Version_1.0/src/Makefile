CC=cc
CFLAGS = -g -L../lib -I../include
BIN = ../bin/

check_QCF_tightlimits: check_QCF_tightlimits.c readQCF.c check_qcftightvalues.c
	$(CC) $(CFLAGS) -o ${BIN}check_QCF_tightlimits check_QCF_tightlimits.c readQCF.c check_qcftightvalues.c
	chmod 0775 ${BIN}$@
 
clean:
	rm -f *.o
