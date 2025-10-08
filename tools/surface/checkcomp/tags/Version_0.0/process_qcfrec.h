/*----------------------------------------------------------
 * process_qcfrec.h - This is the header file containing the 
 *     function definitions for the process_qcfrec.c module.
 *
 *---------------------------------------------------------*/
#ifndef PROCESS_QCFREC_H
#define PROCESS_QCFREC_H

#ifdef __STDC__

   extern void construct_qcfptr ( /*in/out*/  struct qcfrec **qcfptr );
   extern void destruct_qcfptr  ( /*in*/   struct qcfrec **qcfptr );

   extern void reset_qcfrec ( /*in/out*/  struct qcfrec *qcfptr );

   extern void write_qcfrec ( /*in/out*/ FILE    **output_stream,
                              /*in*/     QCFREC  *qcfptr );

   extern void read_qcfrec ( /*in/out*/ FILE    **input_stream,
                             /*in*/     QCFREC  *qcfptr );


   extern void open_file ( /*in*/  char  *path_name,
                           /*in*/  char  *mode,
                           /*out*/ FILE  **opened_file);

   extern void close_file ( /*in*/ FILE  **opened_file);

#else /*!__STDC__*/

   extern void construct_qcfptr ();
   extern void destruct_qcfptr ();

   extern void reset_qcfrec ();
   extern void write_qcfrec (); 
   extern void read_qcfrec (); 

   extern void open_file ();
   extern void close_file ();

#endif /*__STDC__*/

#endif /* PROCESS_QCFREC_H */
