/*----------------------------------------------------------
 * locate_sigmasq_4DYR.h - Header file for locate_sigmasq_4DYR.c
 *
 * 10 Nov 95 lec 
 *   Updated some ints to long ints. 
 * 19 August 2002 lec
 *   Update for 4 digit years.
 *---------------------------------------------------------*/
#ifndef LOCATE_SIGMASQ
#define LOCATE_SIGMASQ

#include "local.h"
#include "process_qcfrec_4DYR.h"

#ifdef __STDC__
   extern void rewrite_sigmasq_file( /*in*/ char      sigma_output_file_name[NAMELEN_MAX],
                                     /*in*/ long int  numstns_inp, 
                                     /*in*/ STRING27  stn_list[MAXNUMSTNS],
                                     /*in*/ float     sigma_sq[MAXNUMSTNS][NUMQCPARMS]);

   extern void locate_sigmasq(/*in*/  char     sigfile_last_searched[NAMELEN_MAX],/*(pathname)YYYYJJJHHMM.sig*/
                              /*in*/  char     sigpathname[NAMELEN_MAX], /* Sigmasq file pathname */
                              /*in*/  long int project_begin_YYYYJJJ,    /* Year/jdate that project begins */
                              /*in*/  long int project_end_YYYYJJJ,    /* Year/jdate that project ends */
                              /*in*/  char     input_stn[27],          /* Stn needing sigma_sq value */
                              /*in*/  int      data_type,              /* Type of data being QC'd (temp,etc.)*/
                              /*out*/ float    sigma_sq[NUMQCPARMS]);  /* Variance array for current stn */

#else /*!__STDC__*/

   extern void rewrite_sigmasq_file();
   extern void locate_sigmasq();

#endif /*__STDC__*/

#endif /* LOCATE_SIGMASQ */
