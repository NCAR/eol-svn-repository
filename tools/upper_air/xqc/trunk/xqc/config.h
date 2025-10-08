
/*
 * $Log: config.h,v $
 * Revision 1.1  1992/12/03  00:52:11  mark
 * Initial revision
 *
 *
 */

/*
 * Basic configuration parameters.
 */
# define CONFIG_VERSION "$Id: config.h,v 1.1 1992/12/03 00:52:11 mark Exp $"

# ifndef YES
#	define YES 1
# 	define NO  0
# endif

/*
 * Directories in which FCC/ZEB files live.  These, alas, need to be defined
 * twice, once in quotes for programs, once without for Makefiles.  Someday
 * when we all have ANSI compilers that can be avoided, but we are not 
 * there yet.
 */
# define BASEDIR	"/rdss/zeb"
# define FCCDIR		BASEDIR
# define RDSSDIR	"/rdss"
# define LIBDIR		"/rdss/zeb/lib"
# define ETCDIR		"/rdss/zeb/etc"	/* Shouldn't really be used for much */
# define BINDIR		"/rdss/zeb/bin"
# define DATADIR	"/rdss/zeb/data"

# ifdef MAKING_MAKEFILE
/*
 * Unquoted versions for makefiles.
 */
# define D_BASEDIR	/rdss/zeb
# define D_FCCDIR	BASEDIR
# define D_LIBDIR	/rdss/zeb/lib
# define D_ETCDIR	/rdss/zeb/etc	/* Shouldn't really be used for much */
# define D_BINDIR	/rdss/zeb/bin
# define D_DATADIR	/rdss/zeb/data
# define D_FCCINC	/rdss/zeb/include

# define D_NETCDFDIR	/usr/local	/* Location of NetCDF libraries and */
					/* include files.		    */
# define D_RDSSDIR	/rdss
# define D_RDSSLIB	/usr/local/lib
# define D_RDSSINC	/rdss/include

# define ZebLibrary	D_LIBDIR/libfcc.a
# define RDSSLibrary	D_RDSSLIB/librdss.a
# endif

/*
 * Configurable options:
 * 
 * 	Plots
 * 		CAP
 * 			lightning
 * 			various overlays (or overlays at all)
 * 			raster variations
 * 			contour variations
 * 			vector variations
 * 			tracks
 * 		Skewt
 * 		Xsection
 * 		Tseries
 * 
 * 	Position widget stuff
 * 	Movie control?
 * 
 * 	Data store format types
 * 		netcdf
 * 		boundary
 * 		raster
 * 
 * 	Data menu
 */


/*
 * The various plot types.
 */
# define C_PT_CAP	YES		/* Constant altitude plots	*/
# define C_PT_SKEWT	YES		/* Skew T plots			*/
# define C_PT_XSECT	YES		/* Cross section plots		*/
# define C_PT_TSERIES	YES		/* Time series plots		*/
# define C_PT_XYGRAPH	YES		/* XY-Graph plots		*/

/*
 * CAP Subplots.
 */
# if C_PT_CAP
/*
 * These YES if you want them.
 */
#	define C_CAP_OVERLAY	YES	/* Overlays			*/
#	define C_CAP_VECTOR	YES	/* Vector plots			*/
# 	define C_CAP_LIGHTNING	YES	/* Lightning location		*/
#	define C_CAP_RASTER	YES	/* Raster plots			*/
# else
/*
 * These are always NO -- do not change them!
 */
#	define C_CAP_OVERLAY	NO	/* Overlays			*/
#	define C_CAP_VECTOR	NO	/* Vector plots			*/
# 	define C_CAP_LIGHTNING	NO	/* Lightning location		*/
#	define C_CAP_RASTER	NO	/* Raster plots			*/
# endif





/*
 * The following is template info for Makefiles, and is usually only
 * used therein.
 */
# ifdef MAKING_MAKEFILE
/*
 * Using Open Windows?  Set this to YES.
 */
# define OPENWIN	NO

/*
 * Does your X system support the shared memory extension?  The answer is
 * yes for MIT X at release 4 or greater, or for Openwin 2.0 or above.  If
 * unsure, run "xdpyinfo" and look for the MIT-SHM extension.
 */
# define XSharedMemory YES

/*
 * Symbolic versions of a couple of directories, for convenience.
 */
	 FCCINC=D_FCCINC
	 RDSSINC=D_RDSSINC
	 NETCDFINC=D_NETCDFDIR/include
	 NETCDFLIB=D_NETCDFDIR/lib
/*
 * Compiler information.  Use of gcc is recommended if you have it.
 * If OPENWIN is YES, then do static linking (-g or -Bstatic).
 *
 * IncludeDirs is where the C compiler is told to look for "system" include
 * files.  If you have files in strange places (X window system, perhaps),
 * add the directories here.
 */
# define CCompiler gcc
# define CCOptions -g -O
# define FortOptions -O
# define IncludeDirs -I$(FCCINC) -I$(RDSSINC) -I$(NETCDFINC)

/*
 * Library information.
 *
 * The libraries for the X window system.  Change as needed.  If your X
 * libraries are not in /usr/lib or /usr/local/lib, throw in the appropriate
 * -L option here.
 */
# define XLibraries -lXaw -lXmu -lXt -lXext -lX11

/*
 * Fortran libraries needed to build the graphics process.  These are
 * set properly for Suns with Fortran 1.3 or beyond.
 */
# define FortranLibs -L/usr/lang -lF77 -lV77

/*
 * The netCDF library.
 */
# define CDFLibrary  -L$(NETCDFLIB) -lnetcdf

/*
 * Other libraries which are routinely needed.
 */
# define MiscLibs RDSSLibrary -ltermcap -lm

/*
 * Here we choose pieces of the system to create.
 */
# define	BUILD_INGEST	NO	/* Do we make ingest modules?	*/
# define	BUILD_OPTIMIZER NO	/* Scan optimizer?		*/
/*
 * Set this if you want the ingest-oriented DataStore utilities (being
 * NetXfr, LastData and Archiver).  Most sites do not need this stuff.  It
 * will not build properly under a non-ANSI compiler.
 */
# define	RT_DS_TOOLS	NO	/* Build real-time DS tools	*/
# endif
