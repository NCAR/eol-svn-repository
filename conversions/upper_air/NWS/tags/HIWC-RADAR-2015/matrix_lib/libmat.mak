
MATRIX1O =  matbox.o matchar.o matindex.o matfunc.o matmap.o \
            matobj.o matenv.o

MATRIX2O =  matref.o matmisc.o matsort.o matpair.o matrix.o \
            matreal.o 

MATRIX3O =  matgen.o matcomp.o matopt.o matsumm.o matsub.o \
            matkron.o matfile.o matmath.o 

MATRIX4O  =  matpsf.o

MATDECO = matdec.o ludec.o choldec.o qrhdec.o svddec.o \
          matlu.o matsvd.o matqrh.o matchol.o

MATOLSO = matols.o olschol.o olsqrh.o olssvd.o
MATSUPO = matspec.o matdist.o matrand.o


libmat.a : $(MATRIX1O) $(MATRIX2O) $(MATRIX3O) $(MATRIX4O)\
	 $(MATDECO)  $(MATOLSO)  $(MATSUPO)
	ar r libmat.a $(MATSUPO)
	ar r libmat.a $(MATDECO)
	ar r libmat.a $(MATOLSO)
	ar rv libmat.a $(MATRIX1O)
	ar rv libmat.a $(MATRIX2O)
	ar rv libmat.a $(MATRIX3O)
	ar rv libmat.a $(MATRIX4O)
	mv libmat.a ../lib
	rm *.o

matbox.o : matbox.hpp

matenv.o : matrix.hpp

matgen.o : matrix.hpp

matcomp.o : matrix.hpp

matopt.o : matrix.hpp

matsumm.o : matrix.hpp

matkron.o : matrix.hpp

matfunc.o : matbox.hpp

matobj.o : matbox.hpp

matreal.o : matrix.hpp

matmap.o : matrix.hpp

matref.o : matrix.hpp

matmisc.o : matrix.hpp

matsub.o : matrix.hpp

matrix.o : matrix.hpp

matchar.o : matrix.hpp

matindex.o : matrix.hpp

matsort.o : matrix.hpp

matpair.o : matrix.hpp

matfile.o : matfile.hpp

matmath.o : matmath.hpp

matpsf.o  : matpsf.hpp

matdec.o : matdec.hpp 

matchol.o : matchol.hpp

choldec.o : choldec.hpp

matlu.o : matlu.hpp

ludec.o : ludec.hpp

matqrh.o : matqrh.hpp

qrhdec.o : qrhdec.hpp

matsvd.o : matsvd.hpp

svddec.o : svddec.hpp

matols.o : matols.hpp matdec.hpp

olschol.o : olschol.hpp matchol.hpp

olsqrh.o : olsqrh.hpp matqrh.hpp

olssvd.o : olssvd.hpp matsvd.hpp

matbox.hpp : matrix.hpp
	touch matbox.hpp

matfile.hpp : matrix.hpp
	touch matfile.hpp

matmath.hpp : matrix.hpp
	touch matmath.hpp

matspec.hpp : matrix.hpp
	touch matspec.hpp 

matrand.hpp : matrix.hpp matspec.hpp
	touch matrand.hpp 

matdec.hpp : matrix.hpp
	touch matdec.hpp

matqrh.hpp : matrix.hpp
	touch matqrh.hpp

matsvd.hpp : matrix.hpp
	touch matsvd.hpp

matchol.hpp : matrix.hpp
	touch matchol.hpp

matlu.hpp : matrix.hpp
	touch matlu.hpp

qrhdec.hpp : matrix.hpp matdec.hpp
	touch qrhdec.hpp

svddec.hpp : matrix.hpp matdec.hpp
	touch svddec.hpp

choldec.hpp : matrix.hpp matdec.hpp
	touch choldec.hpp

ludec.hpp : matrix.hpp matdec.hpp
	touch ludec.hpp

matols.hpp : matrix.hpp matdec.hpp
	touch matols.hpp

olsqrh.hpp : matrix.hpp matols.hpp
	touch olsqrh.hpp

olssvd.hpp : matrix.hpp matols.hpp
	touch olssvd.hpp

olschol.hpp : matrix.hpp matols.hpp
	touch olschol.hpp

matspec.o : matspec.hpp

matdist.o : matspec.hpp

matrand.o : matrand.hpp











