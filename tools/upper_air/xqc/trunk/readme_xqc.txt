The original XQC (sounding quality control) software tool
was written by Mark Bradford in the late 1900's. As of 
June 2011 it was still in use and a copy of his original
software as supplied by him is located in the xqc directory.
See the tags area for a duplicate copy of the original tool.
In June 2011, summer intern, Monica Jacobs, began working 
on the task described in XQC_taskDescription.doc document. 
She completed doing an investigation on what might be a 
better method &/or how to redevelop the XQC tool into a 
more modern tool.  Her recommendations and findings can 
be found in the XQC_report.pdf doc.  She also began working 
on a mockup (Java) form of the tool and that 
is located in the gui.java file.  June 2011 - LEC

In November 2013, Janet Scannell investigated porting the original XQC software
to tikal.  The original software was written for Sun OS using Xview libraries.
The Xview binary libraries for Fedora 8 were downloaded from 
http://www.physionet.org/physiotools/xview/.  The binary libraries for Fedora 8
are required to make the original XQC software work properly.  An earlier version
of the Xview libraries caused an XAllocID error to occur.  This version of the
Xview libraries was used on tikal, which was on release RHEL 6.5 at the time.
The Xview binary libraries that were used on tikal are included in the 
xview-libraries directory in svn.
The Xview libraries add the /usr/openwin/lib and /usr/openwin/include directories.
Changes were made to the Makefile and ui_param.h to compile the software. Now
the original version of the XQC software runs on tikal.  The software is located
in /usr/local/src/xqc on tikal and the xqc executable is linked from 
/usr/local/bin/xqc to /usr/local/src/xqc/xqc.t.  January 2014 - JNS

The detailed instructions for compiling and installing XQC are as follows:

1) Have the systems group install the Xview libraries on the destination computer.

2) Add the software from subversion into the /usr/local/src/xqc directory.

3) In the /usr/local/src/xqc directory, type make
   This command will create the xqc executable, xqc.t.

4) Create a symbolic link in /usr/local/bin.
   a) cd /usr/local/bin
   b) ln -s ../src/xqc/xqc.t

September 2015 - jns
The xqc program started giving errors when accessing the file system.  This problem
coincided with a move of /net/work (where the soundings are stored) to an xfs file 
system.  The problem is an 
incompatibility between xfs, which runs on 64 bit due to the file system > 1TB, and
xqc, which runs on 32 bit due to the xview libraries (which are not available in
64 bit).  A web page that describes this problem in detail is:
http://www.tcm.phy.cam.ac.uk/sw/inodes64.html
The xqc code was recompiled with -D_FILE_OFFSET_BITS=64 added to CFLAGS in the 
Makefile.  The code compiled with no errors and appears to be working now.

August 2016 - jns

Systems group notified us that they would be replacing tikal with a server running
CentOS 7.  The current server barolo was running CentOS 7.2.  The systems group
installed the following xview libraries on barolo:

-rw-r--r-- 1 newbery systems 608444 Aug  4 14:37 xview-3.2p1.4-68.2.x86_64.rpm
-rw-r--r-- 1 newbery systems 490612 Aug  4 14:37 xview-clients-3.2p1.4-68.2.x86_64.rpm
-rw-r--r-- 1 newbery systems 900260 Aug  4 14:37 xview-devel-3.2p1.4-68.2.x86_64.rpm
-rw-r--r-- 1 newbery systems 546324 Aug  4 14:39 xview-devel-examples-3.2p1.4-68.2.x86_64.rpm

I compiled the xqc software changing the following lines in the Makefile:

CFLAGS = -m32 -I. -I/usr/openwin/include -D_FILE_OFFSET_BITS=64 -g
LDFLAGS= -m32 -g

to

CFLAGS = -I. -I/usr/openwin/include -g
LDFLAGS= -g

The software compiled, but when I tried to run it, I received the following error:

XView warning: bad attribute, attr # 0x491c8921

The systems group removed the 64 bit xview libraries and installed the following 32 bit libraries:

 xview                        i686         3.2p1.4-68.2         /xview-3.2p1.4-68.2.i686                        1.4 M
 xview-clients                i686         3.2p1.4-68.2         /xview-clients-3.2p1.4-68.2.i686                1.5 M
 xview-debuginfo              i686         3.2p1.4-68.2         /xview-debuginfo-3.2p1.4-68.2.i686               14 M
 xview-devel                  i686         3.2p1.4-68.2         /xview-devel-3.2p1.4-68.2.i686                  2.9 M
 xview-devel-examples         i686         3.2p1.4-68.2         /xview-devel-examples-3.2p1.4-68.2.i686         4.7 M

I recompiled the xqc software with the original Makefile and the software ran as expected with no errors.

March 2021 - jns

Systems group notified us they would be updating tikal to CentOS 8.  The systems group set up a 
dev server (on mercury) running CentOS 8, so I could test the xqc software.  The xview libraries were
installed:
xview-devel-3.2p1.4-25.22.el8.i686
xview-3.2p1.4-25.22.el8.i686

The make command was installed:
make-4.2.1-10.el8.x86_64

The /usr/local directories were made writable.

The file /usr/include/gnu/stubs-32.h is needed for compiling.  These packages were installed:
glibc-devel-2.28-148.el8.i686
Installing dependencies:
glibc-headers-2.28-148.el8.i686
libgcc-8.4.1-1.el8.i686
libxcrypt-4.1.1-4.el8.i686
libxcrypt-devel-4.1.1-4.el8.i686

The file X11/Xlib.h was missing.  These packages were installed:
libX11-devel-1.6.8-4.el8.x86_64
Installing dependencies:
libXau-devel-1.0.9-3.el8.x86_64
libxcb-devel-1.13.1-1.el8.x86_64
xorg-x11-proto-devel-2020.1-3.el8.noarch

The file X11/Intrinsic.h is needed.  These packages were installed:
libXt-devel-1.1.5-12.el8.x86_64
libSM-devel-1.2.3-1.el8.x86_64
libICE-devel-1.0.9-15.el8.x86_64

There was no symbolic link /usr/lib/libX11.so pointing to /usr/lib/X11.so.6, so this link was created.

Created symbolic link /usr/local/bin/xqc -> ../src/xqc/xqc.t

May 2021 - jns

Since xqc is a legacy application that requires 32-bit software to run, there will be a permanent
legacy virtual machine (vm) to run xqc on.  The vm name is legacy8.eol.ucar.edu.

There were a lot of warnings during the compile, but the software compiles and runs successfully.

