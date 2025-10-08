CC=cc
CFLAGS = -g -L../lib -I../include
BIN = ../bin/

check_QCF_file: check_QCF_file.c readQCF.c check_qcfvalues.c
	$(CC) $(CFLAGS) -o ${BIN}check_QCF_file check_QCF_file.c readQCF.c check_qcfvalues.c
	chmod 0775 ${BIN}$@
 
clean:
	rm -f *.o
