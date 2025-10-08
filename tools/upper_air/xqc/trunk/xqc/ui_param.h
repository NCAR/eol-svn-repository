
/*
 * $Log: ui_param.h,v $
 * Revision 1.1  1992/12/03  00:53:04  mark
 * Initial revision
 *
 *
 */

/* $Id: ui_param.h,v 1.1 1992/12/03 00:53:04 mark Exp $ */
/*
 * Basic UI parameters.
 */
# ifndef UI_PARAM_SYMBOLS
# define UI_PARAM_SYMBOLS

/*
 * Hack to avoid the need for command line flags.
 */
# ifdef sun
#  ifndef UNIX
#   define UNIX
#  endif
# endif

# ifdef titan
#  ifndef UNIX
#   define UNIX
#  endif
# endif

# ifdef UNIX
# include <stdio.h>
# endif

/*
 * Data types of interest.
 */
typedef unsigned char byte;	/* Basic byte variable */
typedef char bool;		/* Boolean variable	*/
struct date_st
{
	long	ds_yymmdd;	/* Day portion	*/
	long	ds_hhmmss;	/* time portion */
};
typedef struct date_st date;		/* Date in date/time format */

/*
 * Define these if nobody else has already done it.
 */
# ifndef TRUE
# define TRUE 1
# define FALSE 0
# endif
# ifndef NULL
# define NULL 0
# endif

/*
 * Macros for VMS system services.
 */
# define ss_err(s)	(((s) & 0x1) == 0)	/* Is this an error status */
# define ___ 0		/* Null parameter. */

/*
 * The comment character.
 */
# define COMMENT '!'

/*
 * Utilities.
 */
char *getvm ();
# define NEW(type) ((type *) getvm (sizeof (type)))

/*
 * Cheap absolute value.
 */
# define ABS(v) (((v) < 0) ? -(v) : v)

/*
 * I'm not too fond of do...while loops, so I make them look like this:
 */
# define repeat do
# define until(b) while (! (b))

/*
 * Fast copy
 */
# ifdef VMS
#	define COPY(from,to,len) { int _L_ = len; \
		if (_L_ < 65535) lib$movc3 (&_L_, from, to); \
		else uiu_slowcopy (_L_, from, to); };
# else
#	define COPY(from,to,len) memcpy (to, from, len)
# endif

/*
 * Conditional def's for arguments for sprintrmt
 *
 * Usage:	error (fmt, ARGS)
 *		char *fmt;
 *		int ARGS;
 *		{
 *			. . .
 *			sprintrmt (buf, fmt, SPRINTARGS);
 *			. . .
 *		}
 */
# ifdef VMS
#	define ARGS args
#	define SPRINTARGS &args
# else
#	define ARGS a1, a2, a3, a4, a5, a6, a7, a8, a9, a10
#	define SPRINTARGS a1, a2, a3, a4, a5, a6, a7, a8, a9, a10
# endif

/*
 * System dependent standard input and output units
 */
# ifdef VMS
#	define IN_STD	"SYS$INPUT"
#	define OUT_STD	"SYS$OUTPUT"
#	define LUN	int
# endif

# ifdef UNIX
#	define IN_STD "stdin"
#	define OUT_STD "stdout"
#	define LUN FILE *
long dview (), dopen ();
# endif

# ifdef CRAY
#	include	<stdio.h>
#	define IN_STD	"$IN"
#	define OUT_STD	"$OUT"
#	define LUN	char *
# endif

/*# ifndef IN_STD
	Error: I dont have anything defined for this system
# endif*/

/*
 * This is (essentially) the maximum length of an input line.
 */
# define MAXLINE 1024

/*
 * The maximum CSAVE length.  This number determines how big the biggest
 * loop can be.
 */
# define NSAVE 400

# endif
