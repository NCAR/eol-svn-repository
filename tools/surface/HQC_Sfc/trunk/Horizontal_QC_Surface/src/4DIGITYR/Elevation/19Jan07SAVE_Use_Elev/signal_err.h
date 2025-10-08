/*----------------------------------------------------------
 * signal_err.h - This is the header file containing the
 *     function definitions for the signal.c module.
 *     The signal error handling module.
 *
 *---------------------------------------------------------*/
#ifndef SIGNAL_ERR_H
#define SIGNAL_ERR_H

#ifdef __STDC__
   extern void sig_pipe(int signo);
#else /*!__STDC__*/
   extern void sig_pipe();
#endif /*__STDC__*/

#endif /* SIGNAL_ERR_H */

