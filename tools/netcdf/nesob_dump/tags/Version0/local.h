/*----------------------------------------------------------
 * local.h - This is the header file containing the 
 *           general definitions required by most routines.
 *---------------------------------------------------------*/
#ifndef LOCAL_H
#define LOCAL_H

#define NAMELEN_MAX 256
#define MAXNUMSTNS 250  /* project dependent */

/*
 * STRIPLINE is macro to move file pointer past next newline.
 */
#define STRIPLINE(file) {int c;while((c=getc(file))!='\n')if(c==EOF)break;}

#ifdef  __STDC__
#define VOID void
#define PROTOTYPES
#else  /*!__STDC__*/
#define VOID char
#endif /*__STDC__*/

#define  TRUE  1
#define  FALSE 0

typedef char STRING16[16];

#endif /* LOCAL_H */
