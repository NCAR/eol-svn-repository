/*--------------------------------------------------------
 * main.c - a test of the signal error handlers.
 *
 *-------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <errno.h>

#include <sys/signal.h>
#include <signal.h>

#include "local.h"
#include "process_qcfrec.h"
#include "qcfrec.h"
#include "locate_sigmasq.h"
#include "signal_err.h"


/* local functions */
#ifdef __STDC__
   int main (int argc, char *argv[]);
#else /*!__STDC__*/
   int main ();
#endif /*__STDC__*/


/*---------------------------------------------------------
 * main()
 *--------------------------------------------------------*/

int main( int argc, char *argv[])
   {
   /* local variables */
   char         sigma_output_file_name[NAMELEN_MAX] = "\0";
   FILE         *sigma_output_stream;

   char         junk_char = '\0';
   long int     ii, mm, zz, yyy = 0; 
   int          jj,kk,z,yy,xx,xxy, junk = 0;


   printf ("Begin program test.\n");

   /*-----------------------------------------
    * Handle "Broken Pipe" errors. Tell system
    * to ignore pipe signals. HERE
    *----------------------------------------*/
   if (signal (SIGPIPE, sig_pipe)== SIG_ERR)
      printf ("signal_error");

/*   sig_pipe(1); */

   /*---------------------------------------------------
    * Open, read the sigmasq (variance) file
    *---------------------------------------------------*/
   sprintf (sigma_output_file_name, "%-s", "19961301200.sig");

   /*--------------------------------------- 
    * In UNIX environment, don't uncompress 
    * (gunzip) the file. Just use a pipe to 
    * access the data in the file. In all 
    * other environments, uncompress and use 
    * fopen() as usual. 
    *---------------------------------------*/ 
   open_file (sigma_output_file_name, "r", FILE_COMPRESSED, &sigma_output_stream);


   /*------------------------------------------
    * Read a bit and then close the file early!
    *-----------------------------------------*/
   for (ii=0;ii<5;ii++)
      {
      fscanf (sigma_output_stream, "%d%1c", &jj, &junk_char);

      if (feof(sigma_output_stream))
        printf ("WARNING(1): hit premature End Of File on %-s.\n",sigma_output_file_name);
      }

   /*---------------------------------------
    * In UNIX environment, have used a
    * pipe open to access compressed
    * file without using time to uncompress.
    * Close_file() will then close pipe.
    * If file is not compressed, close_file()
    * close normally with fclose command.
    *----------------------------------------*/
   printf ("Close the file.\n");
   close_file (&sigma_output_stream, FILE_COMPRESSED);

   printf ("End test\n");
   }  /* main() */
