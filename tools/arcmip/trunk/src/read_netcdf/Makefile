#
# compile read_netcdf.F
#

DEVTOP = ./
SHELL = /bin/sh
LIBS = -lz -lnetcdf -L/usr/local/lib
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

OBJS = read_netcdf.o

read_netcdf: $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LIBS)

clean: 
	$(RM) $(OBJS)
	$(RM) read_netcdf

# DO NOT DELETE THIS LINE -- make depend depends on it.

read_netcdf.o: $(INCLDIR)/domain.h $(INCLDIR)/netcdf.inc
read_netcdf.o: $(INCLDIR)/col_netcdf.cb
read_netcdf.o: $(INCLDIR)/may_netcdf.cb
read_netcdf.o: $(INCLDIR)/mon_netcdf.cb

