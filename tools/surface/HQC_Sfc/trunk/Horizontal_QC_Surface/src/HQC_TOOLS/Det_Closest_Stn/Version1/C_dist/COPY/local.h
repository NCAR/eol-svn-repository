/*----------------------------------------------------------
 * local.h - This is the header file containing the 
 *           general definitions required by most routines.
 *---------------------------------------------------------*/
#ifndef LOCAL_H
#define LOCAL_H

#define NAMELEN_MAX 256

/* Max number of days in project 2 yrs */
#define MAX_NUM_PROJ_DAYS 731

/* project dependent: Storm Wave = approx. 604 stns in hi res comp. Aprox. 970 in hourly. */
/* Was 1371 */
#define MAXNUMSTNS  10
 
#define STRIPLINE(file) {int c;while((c=getc(file))!='\n')if(c==EOF)break;}

/* Set UNIX environment variable to 1 only for UNIX systems */
#define UNIX_ENV  1

/* Following flags are set to be control open and close files
   in UNIX and non-UNIX environments.*/
#define FILE_NOT_COMPRESSED 0
#define FILE_COMPRESSED     1

#ifdef  __STDC__
#define VOID void
#define PROTOTYPES
#else  /*!__STDC__*/
#define VOID char
#endif /*__STDC__*/

#define  TRUE  1
#define  FALSE 0

typedef char STRING27[27];

#endif /* LOCAL_H */
