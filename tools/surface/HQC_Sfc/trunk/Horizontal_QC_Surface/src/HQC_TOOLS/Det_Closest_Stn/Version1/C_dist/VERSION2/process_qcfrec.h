/*----------------------------------------------------------
 * process_qcfrec.h - This is the header file containing the 
 *     function definitions for the process_qcfrec.c module.
 *
 *---------------------------------------------------------*/
#ifndef PROCESS_QCFREC_H
#define PROCESS_QCFREC_H

#include "local.h"
#include "qcfrec.h"

#define NUMQCPARMS  7

enum qcparm { stnprs=0, slp=1,  cslp=2, 
              temp=3,   dewpt=4, 
              windsp=5, winddir=6};



#ifdef __STDC__

   extern void open_file ( /*in*/  char   *path_name,
                           /*in*/  char   *mode,
                           /*in*/  int    file_compressed,
                           /*out*/ FILE   **opened_file );

   extern void close_file (/*in*/ FILE   **opened_file,
                           /*in*/ int    file_compressed);

#else /*!__STDC__*/

   extern void open_file ();
   extern void close_file ();

#endif /*__STDC__*/

#endif /* PROCESS_QCFREC_H */
