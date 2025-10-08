#
# compile domain_netcdf.F
#

DEVTOP = ./
SHELL = /bin/sh
LIBS = -ldf -lz -lnetcdf -L/usr/local/lib
INCLDIR = $(DEVTOP)include
FC = f77
FFLAGS = -I$(INCLDIR) -e 
CPP = /usr/lib/cpp
RM = rm -f
MAKE = make -r
AR = ar ru
CP = cp

.SUFFIXES:
.SUFFIXES: .F .f .c .o

.F.o:
	@echo '.F.o rule: '$@
	$(FC) $(FFLAGS) -c $<
.f.o:
	@echo '.f.o rule: '$@
	$(FC) $(FFLAGS) -c $<

.F.f:
	@echo '.F.f rule: ',$@
	$(CPP) -I$(LIBINCLUDE) -C -P $*.F > $@

OBJS = domain_netcdf.o

domain_netcdf: $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LIBS)

clean: 
	$(RM) $(OBJS)
	$(RM) domain_netcdf

# DO NOT DELETE THIS LINE -- make depend depends on it.

domain_netcdf.o: $(INCLDIR)/netcdf.inc

