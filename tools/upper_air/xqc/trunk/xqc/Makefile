#
# $Id: Makefile,v 1.1 1992/12/03 00:52:01 mark Exp $
#
#
# $Log: Makefile,v $
# Revision 1.1  1992/12/03  00:52:01  mark
# Initial revision
#
#

# Simple Makefile for xqc
# Version 1.0 alpha, 1992 May 19
# Mark Bradford, STORM Project Office
# (303) 497-8969
# bradfrd2@ncar.ucar.edu

CFLAGS = -m32 -I. -I/usr/openwin/include -D_FILE_OFFSET_BITS=64 -g

SRCS=	xqc.c	classio.c	Skewt.c	derive.c\
	DrawText.c

OBJS=	xqc.o	classio.o	Skewt.o	derive.o\
	DrawText.o

#CC=	gcc
LDR=	cc
LDFLAGS= -m32 -g
LIBS=-L/usr/openwin/lib -lX11 -lxview -lolgx -lm 
COMMAND=xqc.t

#
# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
$(COMMAND):	$(OBJS)
	$(LDR) $(LDFLAGS) -o $(COMMAND) $(OBJS) $(LIBS)

clean:
	rm -f *.o *.l

clobber:	clean
	rm -f $(COMMAND)
# DO NOT DELETE THIS LINE -- make depend depends on it.

#Skewt.o: config.h /usr/include/math.h /usr/openwin/include/X11/Intrinsic.h
Skewt.o: config.h /usr/include/math.h 
#Skewt.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
Skewt.o: /usr/include/sys/types.h
#Skewt.o: /usr/include/sys/feature_tests.h /usr/include/sys/machtypes.h
#Skewt.o: /usr/include/sys/isa_defs.h /usr/include/sys/select.h
Skewt.o: /usr/include/sys/select.h
Skewt.o: /usr/include/sys/time.h /usr/include/sys/time.h
#Skewt.o: /usr/openwin/include/X11/X.h /usr/openwin/include/X11/Xfuncproto.h
#Skewt.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
#Skewt.o: /usr/openwin/include/X11/Xutil.h
#Skewt.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
Skewt.o: /usr/include/string.h
#Skewt.o: /usr/openwin/include/X11/Core.h /usr/openwin/include/X11/Composite.h
#Skewt.o: /usr/openwin/include/X11/Constraint.h
#Skewt.o: /usr/openwin/include/X11/Object.h /usr/openwin/include/X11/RectObj.h
Skewt.o: /usr/openwin/include/xview/xview.h
Skewt.o: /usr/openwin/include/xview/xview_xvin.h /usr/include/signal.h
Skewt.o: /usr/include/sys/signal.h /usr/include/sys/unistd.h
Skewt.o: /usr/openwin/include/pixrect/pixrect.h
Skewt.o: /usr/openwin/include/pixrect/pr_planegroups.h
Skewt.o: /usr/openwin/include/pixrect/pr_util.h
Skewt.o: /usr/openwin/include/pixrect/memvar.h
Skewt.o: /usr/openwin/include/pixrect/pixfont.h
Skewt.o: /usr/openwin/include/pixrect/traprop.h
Skewt.o: /usr/openwin/include/pixrect/pr_line.h /usr/include/stdlib.h
Skewt.o: /usr/openwin/include/xview/xv_c_types.h
Skewt.o: /usr/openwin/include/xview/generic.h
Skewt.o: /usr/openwin/include/xview/pkg_public.h
Skewt.o: /usr/openwin/include/xview/pkg.h /usr/openwin/include/xview/attr.h
Skewt.o: /usr/openwin/include/xview/base.h /usr/include/malloc.h
Skewt.o: /usr/include/errno.h /usr/include/sys/errno.h
Skewt.o: /usr/openwin/include/xview/notify.h /usr/include/stdio.h
#Skewt.o: /usr/include/sys/wait.h /usr/include/sys/siginfo.h
Skewt.o: /usr/include/sys/wait.h 
#Skewt.o: /usr/include/sys/machsig.h /usr/include/sys/procset.h
Skewt.o: /usr/include/sys/resource.h /usr/include/sys/ucontext.h
#Skewt.o: /usr/include/sys/regset.h /usr/include/stdarg.h
Skewt.o: /usr/openwin/include/xview/xv_error.h
Skewt.o: /usr/openwin/include/xview/sun.h /usr/openwin/include/xview/server.h
Skewt.o: /usr/openwin/include/xview/screen.h
Skewt.o: /usr/openwin/include/xview/pixwin.h
Skewt.o: /usr/openwin/include/xview/rect.h
Skewt.o: /usr/openwin/include/xview/rectlist.h
Skewt.o: /usr/openwin/include/xview/win_input.h
Skewt.o: /usr/openwin/include/xview/win_event.h
Skewt.o: /usr/openwin/include/xview/xv_version.h
Skewt.o: /usr/openwin/include/xview/icon.h
Skewt.o: /usr/openwin/include/xview/window.h
Skewt.o: /usr/openwin/include/xview/drawable.h
Skewt.o: /usr/openwin/include/xview/attrol.h
Skewt.o: /usr/openwin/include/xview/svrimage.h
Skewt.o: /usr/openwin/include/xview/frame.h
Skewt.o: /usr/openwin/include/xview/openmenu.h
Skewt.o: /usr/openwin/include/xview/canvas.h
Skewt.o: /usr/openwin/include/xview/openwin.h
Skewt.o: /usr/openwin/include/xview/xv_xrect.h ui.h ui_symbol.h ui_param.h
Skewt.o: ui_error.h /usr/include/setjmp.h defs.h /usr/include/memory.h pd.h
Skewt.o: message.h derive.h GraphProc.h GC.h PixelCoord.h DrawText.h xqc.h
#DrawText.o: /usr/include/math.h /usr/openwin/include/X11/Intrinsic.h
DrawText.o: /usr/include/math.h 
#DrawText.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
DrawText.o: /usr/include/sys/types.h
#DrawText.o: /usr/include/sys/feature_tests.h /usr/include/sys/machtypes.h
#DrawText.o: /usr/include/sys/isa_defs.h /usr/include/sys/select.h
DrawText.o: /usr/include/sys/select.h
DrawText.o: /usr/include/sys/time.h /usr/include/sys/time.h
#DrawText.o: /usr/openwin/include/X11/X.h
#DrawText.o: /usr/openwin/include/X11/Xfuncproto.h
#DrawText.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
#DrawText.o: /usr/openwin/include/X11/Xutil.h
#DrawText.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
DrawText.o: /usr/include/string.h
#DrawText.o: /usr/openwin/include/X11/Core.h
#DrawText.o: /usr/openwin/include/X11/Composite.h
#DrawText.o: /usr/openwin/include/X11/Constraint.h
#DrawText.o: /usr/openwin/include/X11/Object.h
#DrawText.o: /usr/openwin/include/X11/RectObj.h ui.h ui_symbol.h ui_param.h
DrawText.o: ui.h ui_symbol.h ui_param.h
DrawText.o: /usr/include/stdio.h message.h DrawText.h sfont_1.c xqc.h
xqc.o: /usr/openwin/include/xview/xview.h
xqc.o: /usr/openwin/include/xview/xview_xvin.h /usr/include/signal.h
#xqc.o: /usr/include/sys/feature_tests.h /usr/include/sys/signal.h
xqc.o: /usr/include/sys/signal.h
xqc.o: /usr/include/sys/unistd.h /usr/include/sys/types.h
#xqc.o: /usr/include/sys/machtypes.h /usr/include/sys/isa_defs.h
xqc.o: /usr/include/sys/select.h /usr/include/sys/time.h
xqc.o: /usr/include/sys/time.h /usr/openwin/include/pixrect/pixrect.h
xqc.o: /usr/openwin/include/pixrect/pr_planegroups.h
xqc.o: /usr/openwin/include/pixrect/pr_util.h
xqc.o: /usr/openwin/include/pixrect/memvar.h
xqc.o: /usr/openwin/include/pixrect/pixfont.h
xqc.o: /usr/openwin/include/pixrect/traprop.h
xqc.o: /usr/openwin/include/pixrect/pr_line.h /usr/include/stdlib.h
xqc.o: /usr/openwin/include/xview/xv_c_types.h
xqc.o: /usr/openwin/include/xview/generic.h
xqc.o: /usr/openwin/include/xview/pkg_public.h
xqc.o: /usr/openwin/include/xview/pkg.h /usr/openwin/include/xview/attr.h
xqc.o: /usr/openwin/include/xview/base.h /usr/include/string.h
xqc.o: /usr/include/malloc.h /usr/include/errno.h /usr/include/sys/errno.h
xqc.o: /usr/openwin/include/xview/notify.h /usr/include/stdio.h
#xqc.o: /usr/include/sys/wait.h /usr/include/sys/siginfo.h
xqc.o: /usr/include/sys/wait.h 
#xqc.o: /usr/include/sys/machsig.h /usr/include/sys/procset.h
xqc.o: /usr/include/sys/resource.h /usr/include/sys/ucontext.h
#xqc.o: /usr/include/sys/regset.h /usr/include/stdarg.h
xqc.o: /usr/openwin/include/xview/xv_error.h /usr/openwin/include/xview/sun.h
xqc.o: /usr/openwin/include/xview/server.h
xqc.o: /usr/openwin/include/xview/screen.h
xqc.o: /usr/openwin/include/xview/pixwin.h /usr/openwin/include/xview/rect.h
xqc.o: /usr/openwin/include/xview/rectlist.h
#xqc.o: /usr/openwin/include/xview/win_input.h /usr/openwin/include/X11/Xlib.h
xqc.o: /usr/openwin/include/xview/win_input.h 
#xqc.o: /usr/openwin/include/X11/X.h /usr/openwin/include/X11/Xfuncproto.h
#xqc.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
xqc.o: /usr/openwin/include/xview/win_event.h
xqc.o: /usr/openwin/include/xview/xv_version.h
xqc.o: /usr/openwin/include/xview/icon.h /usr/openwin/include/xview/window.h
xqc.o: /usr/openwin/include/xview/drawable.h
xqc.o: /usr/openwin/include/xview/attrol.h
xqc.o: /usr/openwin/include/xview/svrimage.h
xqc.o: /usr/openwin/include/xview/frame.h
xqc.o: /usr/openwin/include/xview/openmenu.h
xqc.o: /usr/openwin/include/xview/panel.h /usr/openwin/include/xview/canvas.h
xqc.o: /usr/openwin/include/xview/openwin.h /usr/openwin/include/xview/font.h
xqc.o: /usr/openwin/include/xview/cms.h /usr/openwin/include/xview/xv_xrect.h
xqc.o: /usr/openwin/include/xview/cursor.h
#xqc.o: /usr/openwin/include/X11/cursorfont.h /usr/include/sgtty.h
xqc.o: /usr/include/sgtty.h
xqc.o: /usr/include/sys/ttychars.h /usr/include/sys/stat.h
xqc.o: /usr/include/ctype.h /usr/include/pwd.h /usr/include/unistd.h xqc.h
xqc.o: GC.h
#classio.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
classio.o: /usr/include/stdio.h 
classio.o: /usr/include/errno.h /usr/include/sys/errno.h
#classio.o: /usr/include/sys/types.h /usr/include/sys/machtypes.h
classio.o: /usr/include/sys/types.h 
#classio.o: /usr/include/sys/isa_defs.h /usr/include/sys/select.h
classio.o: /usr/include/sys/select.h
classio.o: /usr/include/sys/time.h /usr/include/sys/time.h
#classio.o: /usr/include/dirent.h /usr/include/sys/dirent.h
classio.o: /usr/include/dirent.h 
#classio.o: /usr/include/sys/stat.h xqc.h /usr/openwin/include/X11/Xlib.h
classio.o: /usr/include/sys/stat.h xqc.h 
#classio.o: /usr/openwin/include/X11/X.h /usr/openwin/include/X11/Xfuncproto.h
#classio.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
derive.o: /usr/include/math.h message.h derive.h
