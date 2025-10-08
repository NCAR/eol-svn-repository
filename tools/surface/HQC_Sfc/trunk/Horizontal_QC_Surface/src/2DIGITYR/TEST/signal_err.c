
#include <signal.h>

/* Call using the following */
   /*-----------------------------------------
    * Handle "Broken Pipe" errors. Tell system
    * to ignore pipe signals. HERE
    *----------------------------------------*/
/*   if (signal (SIGPIPE, sig_pipe) == SIG_ERR)
      printf ("(MAIN) signal error!\n");
*/


/* 
 * Sig Pipe Error handler
 */
void sig_pipe(int signo)
   {
   printf ("---SIGPIPE caught---\n");
/*   exit(1); */
   }
