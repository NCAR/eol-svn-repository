/*----------------------------------------------------------
 * general.h - This is the header file containing the 
 *     function definitions for the general.c module.
 *
 *---------------------------------------------------------*/
#ifndef GENERAL_H
#define GENERAL_H

#define MAX_CHARS 700
#define MAX_ELEM  1000

#ifdef __STDC__

   extern void read_record( /*in/out*/ FILE       **data_stream,
                            /*out*/    char       new_line[MAX_CHARS]);

#else /*!__STDC__*/

   extern void read_record ();

#endif /*__STDC__*/

#endif /* GENERAL_H */
