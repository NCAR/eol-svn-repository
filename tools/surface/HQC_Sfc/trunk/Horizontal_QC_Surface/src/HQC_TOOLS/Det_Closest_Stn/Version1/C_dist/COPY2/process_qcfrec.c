/*-------------------------------------------------------
 * process_qcfrec.c - module containing functions to 
 *    process qcf records.
 *
 * 04 Mar 97 lec
 *   Added include for string.h.
 *-------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
 
#include "qcfrec.h"
#include "process_qcfrec.h"

#define debug 0

/*--------------------------------------------------------
 * open_file.c - Opens the requested file, if it exists.
 *
 * Input: path name of file to be opened.
 *
 * Output: Opened file stream.
 *-------------------------------------------------------*/
void open_file ( /*in*/  char   *path_name,
                 /*in*/  char   *mode,
                 /*in*/  int    file_compressed,
                 /*out*/ FILE   **opened_file )
   {
   char   cat_cmd [NAMELEN_MAX+10] = "\0";

#if UNIX_ENV
   /*----------------------------------------
    * In UNIX environment, open a pipe to the 
    * compressed file. The gunzip -c command
    * leaves the original file untouched and
    * only uncompress the file to the stdout.
    * This allows the s/w to skip the system
    * calls that compress and uncompress
    * each file. The gunzip cmd assumes the
    * file has a .gz suffix. If not in UNIX
    * environment or file is not compressed,
    * just open the file regularly.
    *----------------------------------------*/
   if (file_compressed)
      {
      sprintf (cat_cmd, "gunzip -c %-s", path_name);

/*    printf ("file_compressed = TRUE! popen with cat_cmd: xxx%-sxxx\n", cat_cmd);  DEBUG */

      *opened_file = popen (cat_cmd, mode);
      }
   else
      *opened_file = fopen (path_name, mode);
   
#else
   *opened_file = fopen (path_name, mode);
#endif

   if (*opened_file == NULL)
      {
      fprintf (stderr, "Error: Can NOT open %-s.\n", path_name);
      exit(1);
      }

   } /* open_file()*/

/*--------------------------------------------------------
 * close_file.c - Closes the requested file.
 * 
 * Input: Stream to be closed.
 * 
 *-------------------------------------------------------*/
void close_file (/*in*/ FILE   **opened_file,
                 /*in*/ int    file_compressed)
   {
   int   stat = 0;

   /*---------------------------------------------
    * If in UNIX environment and the file was
    * compressed, file has been opened via the
    * popen (pipe) cmd. If not in UNIX environ,
    * or file not compressed, just close normally.
    *---------------------------------------------*/
#if UNIX_ENV
   if (file_compressed)
      {
      stat = pclose(*opened_file);
/*    printf ("UNIX_ENV --(1) close_file: file_compressed - use pclose\n"); */

      if (stat !=0 && stat !=EOF && stat != 36096)
         printf (" WARNING: file_compressed -> pclose stat = %d\n", stat); /* debug */

      if (stat == EOF)
        {
        fprintf (stderr, "Error: Can NOT pclose opened file. Status = %d.\n", stat); 
        exit(1); 
        } 
      }
   else
      {
/*    printf ("UNIX_ENV --(2) close_file: file_NOT_compressed - use normal fclose\n"); */
      if (fclose (*opened_file) == EOF)
         {   
         fprintf (stderr, "Error: Can NOT close opened file.\n");
         exit(1);
         }
      }
#else
/* printf ("NOT UNIX_ENV - (3) close_file: file_NOT_compressed - use normal fclose\n"); */
   if (fclose (*opened_file) == EOF)
     {
     fprintf (stderr, "Error: Can NOT close opened file.\n");
     exit(1);
     }
#endif 

#if debug
   printf ("end close_file\n");
#endif
   } /* close_file() */
