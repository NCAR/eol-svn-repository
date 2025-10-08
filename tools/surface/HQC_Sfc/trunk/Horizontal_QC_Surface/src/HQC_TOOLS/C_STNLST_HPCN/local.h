/* $Id$ */
/*
 * $Log$
 *
 */

/*----------------------------------------------------------
 * local - This is the header file containing the 
 *     general definitions used by most fn.
 *
 * 09 May 94 lec
 *   Created.
 *---------------------------------------------------------*/
#ifndef LOCAL
#define LOCAL

/*
 * STRIPLINE is macro to move file pointer past next newline.
 */
#define STRIPLINE(file) {int c; while((c=getc(file))!='\n')if(c==EOF)break;}


#define NAMELEN   256
#define MAX_CHARS 700
#define MAX_ELEMS 12000 /* this is the max allowed number of recs that can be
                           can be read from existing master stn list. For GIDS-1
                           that list was called fest_stn_list */

#endif /* LOCAL */
