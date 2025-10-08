/*--------------------------------------------------------
 * main - 
 *       
 *-------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "local.h"
#include "qcfrec.h"
#include "gQC.h"


/*---------------------------------------------------------
 * main() - 
 *
 *--------------------------------------------------------*/
int main()
   {
   /* local variables */
   char		input_file_name[NAMELEN_MAX] = "testqcf.in\0";
   char         output_file_name[15] = "main.out\0";

   FILE		*input_stream;
   FILE         *output_stream;

   QCFREC       *qcfptr; 

   int   n = 22;
   int   pmethod = 0;
   char  qcflag = 'U';
   float ave, svar, adev, sdev = 0.0;

   /* AWOSQ 20min data; t=940401 00:00 */

   float A [MAXNUMSTNS][4] = { {31.08000,-97.68000,0.0,0.0},
                              {31.15000,-97.42000,0.0,0.0},
                              {31.48000,-97.32000,0.0,0.0},
                              {33.63000,-95.45000,0.0,0.0},
                              {34.42000,-103.08000,0.0,0.0},
                              {34.60000,-91.57000,0.0,0.0},
                              {35.73000,-91.65000,0.0,0.0},
                              {36.13000,-90.62000,0.0,0.0},
                              {36.19000,-94.49000,0.0,0.0},
                              {36.29000,-92.59000,0.0,0.0},
                              {36.35000,-94.22000,0.0,0.0},
                              {36.37000,-94.11000,0.0,0.0},
                              {36.46000,-105.67000,0.0,0.0},
                              {37.05000,-100.97000,0.0,0.0},
                              {37.25000,-104.33000,0.0,0.0},
                              {38.06000,-97.28000,0.0,0.0},
                              {38.35000,-98.86000,0.0,0.0},
                              {38.85000,-99.27000,0.0,0.0},
                              {39.65000,-106.92000,0.0,0.0},
                              {40.43000,-104.63000,0.0,0.0},
                              {40.45000,-105.02000,0.0,0.0} };


   /* Test temperatures in degrees C! */ 
   float theta_o[MAXNUMSTNS] = {19.44, 18.33, 18.89, 16.67, 18.89,
                                11.67, 12.22, 10.56, 14.44, 15.56,
                                14.44, 14.44, 13.89, 20.56, 15.00,
                                18.33, 17.78, 12.22, 17.22, 15.00,
                                17.78, 17.22  };

   float b[2] = {31.08000, -97.68000};  /*lat/lon ILE */

   float theta_obs = 19.44; /* temp at point b */

  /*
    * Open the ascii input and output files.
    */
#if 0
   printf ("Open in file\n");
   open_file (input_file_name, "r", &input_stream);
   printf ("Open out file\n");
   open_file (output_file_name, "w", &output_stream);

   /*
    * Construct a qcfrec pointer
    */
   printf ("Construct ptr\n");
   construct_qcfptr (&qcfptr);

   printf ("Reset ptr\n");
   reset_qcfrec( qcfptr );

   printf ("write ptr\n");
   write_qcfrec (output_stream, qcfptr);
  
   printf ("read ptr\n");
   read_qcfrec (&input_stream, qcfptr);

   printf ("write ptr\n");
   write_qcfrec (output_stream, qcfptr);

   printf ("free ptr\n");
   destruct_qcfptr (&qcfptr);

   close_file (&input_stream);
   close_file (&output_stream);
#endif

   printf ("Enter pmethod: \n");
   scanf ("%d", &pmethod);
   printf ("pmethod is %d.\n", pmethod);

   printf ("Enter observation at ref pt (19.44 C): \n"); 
   scanf ("%f", &theta_obs); 
   printf ("theta_obs is %f.\n", theta_obs);

   /*
    * Compute sigma (variance). For debugging purposes,
    * pass in just theta_o data and test fn. 
    */
   determine_sigma(theta_o, n, &ave, &svar, &adev, &sdev); 
   printf ("ave, svar, adev, sdev: %f %f %f %f\n", ave, svar, adev, sdev);

   determine_qcflag(A, theta_o, n, pmethod, b, theta_obs, sdev, &qcflag);

   printf ("b: %f %f \n", b[0], b[1]);
   printf ("qcflag: %c \n", qcflag);

   }  /* main() */
