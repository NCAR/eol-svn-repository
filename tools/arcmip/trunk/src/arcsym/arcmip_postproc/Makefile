#
# compile arcmip_postproc.F
#

DEVTOP = ./
SHELL = /bin/sh
LIBS = -ldf -lz -L/usr/local/lib
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

OBJS = arcmip_postproc.o

arcmip_postproc: $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LIBS)

clean: 
	$(RM) $(OBJS)
	$(RM) arcmip_postproc

# DO NOT DELETE THIS LINE -- make depend depends on it.

arcmip_postproc.o: $(INCLDIR)/domain_size.h $(INCLDIR)/domain_var.cb
arcmip_postproc.o: $(INCLDIR)/set_param.h $(INCLDIR)/arcmip_sig_3d.cb
arcmip_postproc.o: $(INCLDIR)/arcmip_2d.cb $(INCLDIR)/arcmip_3d.cb
arcmip_postproc.o: $(INCLDIR)/arcmip_col_2d.cb $(INCLDIR)/arcmip_col_3d.cb
arcmip_postproc.o: $(INCLDIR)/arcmip_mon_2d.cb $(INCLDIR)/arcmip_mon_3d.cb
arcmip_postproc.o: $(INCLDIR)/arcmip_may_2d.cb $(INCLDIR)/arcmip_may_3d.cb
arcmip_postproc.o: $(INCLDIR)/lsm_rad_var.cb

