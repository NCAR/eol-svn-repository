/*----------------------------------------------------------
 * local.h - This is the header file containing the 
 *           general definitions required by most routines.
 *---------------------------------------------------------*/
#ifndef LOCAL_H
#define LOCAL_H

#define NAMELEN_MAX 256

/* Max number of days in project 2 yrs */
#define MAX_NUM_PROJ_DAYS 731

/* project dependent: Storm Wave = approx. 604 stns */
#define MAXNUMSTNS 700
 
#define STRIPLINE(file) {int c;while((c=getc(file))!='\n')if(c==EOF)break;}

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
