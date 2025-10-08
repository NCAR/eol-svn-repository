#
#
#

all:	cudom ezcvt

ezlhconv.o:	ezlhconv.c

cudom:	ezlhconv.o cudom.c
	cc -o cudom cudom.c ezlhconv.o -lm

ezcvt:	ezlhconv.o ezcvt.o
	cc -o ezcvt ezcvt.c ezlhconv.o -lm

clean:
	$(RM) *.o core a.out cudom ezcvt

