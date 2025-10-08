/*--------------------------------------------------------
 * main_qc.c -  Performs Horizontal Quality Control.
 *       
 *-------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>

#include "local.h"
#include "process_qcfrec.h"
#include "qcfrec.h"
#include "gQC.h"
#include "date.h"

/*---------------------------------------------------------
 * main() - 
 *
 *--------------------------------------------------------*/
int main()
   {
   /* local variables */
   char		init_input_file_name[NAMELEN_MAX];
   char		input_file_name[NAMELEN_MAX];
   char         qcf_output_file_name[15] = "\0";
   char         debug_output_file_name[15] = "test.out\0";

   FILE		*init_input_stream;
   FILE		*input_stream;
   FILE         *qcf_output_stream;
   FILE         *debug_output_stream;

   QCFREC       *qcfptr; 
   QCFREC       *current_qcfptr; 

   int   i,ii,j,jj,k,kk = 0;
   int   n = 0;

   int   pmethod;

   float min_weight;
   int data_freq;

   float bmax;
   float dmin;

   char  qcflag = 'U';

   float A [MAXNUMSTNS][4];
   long  numstns = 0;

   float theta_o[NUMQCPARMS][MAXNUMSTNS]; /* observations at stns in array A[][] */
   int   numtheta_o[NUMQCPARMS];

   char  current_stn[16] = "ILE\0";     /* Id string for stn b. */
   float b[2];                          /*lat/lon ILE */
   float theta_obs;                     /* temp at point b at t = 0:00. Ob at stn b the current stn.*/

   double x,y = 0.0;

   float stn_data[NUMQCPARMS][SIGMA_PERIOD];          /* for now do time 00:00. Obs at stn b */
   int   stn_numdata[NUMQCPARMS] = {0,0,0,0,0,0,0};   /* number of data points in numdata */
   float sigma_sq[NUMQCPARMS]    = {0,0,0,0,0,0,0};

   float ave, svar, adev, sdev = 0.0;

   int   current_yr = 0;
   char  begin_date[7] ="\0\0\0\0\0\0\0";       /* Input date to begin processing */
   char  end_date[7]   ="\0\0\0\0\0\0\0";       /* Input date to end processing   */

   int   begin_jdate  = 0;                /* beginning Julian date of data - input value!! */
   int   end_jdate    = 0;                /* ending Julian date of data - input value!! */

   char  date[7]      = "\0\0\0\0\0\0\0";

   int   current_date = 0;                /* Julian date */
   int   julian       = 0;


   /*
    * Initialize from input file.
    */
   printf ("\nInput name of QC initialization file: (e.g., QCinit.inp)\n");
   scanf ("%s", init_input_file_name);
   printf ("Input file name: xxx%-sxxx\n", init_input_file_name);
   
   open_file ("QCinit.inp\0", "r", &init_input_stream);
 
   fscanf (init_input_stream, "%d", &pmethod);   STRIPLINE(init_input_stream);
   fscanf (init_input_stream, "%f", &min_weight);STRIPLINE(init_input_stream);
   fscanf (init_input_stream, "%d", &data_freq); STRIPLINE(init_input_stream);
   fscanf (init_input_stream, "%f", &bmax);      STRIPLINE(init_input_stream);
   fscanf (init_input_stream, "%f", &dmin);      STRIPLINE(init_input_stream);
   fscanf (init_input_stream, "%d", &current_yr);STRIPLINE(init_input_stream); 
   fscanf (init_input_stream, "%s", begin_date); STRIPLINE(init_input_stream);
   begin_date[6] = '\0';
   fscanf (init_input_stream, "%s", end_date);
   end_date[6] = '\0';

   printf ("p, min_weight, data_freq, bmax, dmin: %d %f %d %f %f\n",
            pmethod, min_weight, data_freq, bmax, dmin);
   printf ("current_yr:%d\n", current_yr);
   printf ("begin_date, end_date: xxx%-sxxxx xxx%-sxxx\n", begin_date, end_date);


   open_file (debug_output_file_name, "w", &debug_output_stream);  /* debug for now */

   fprintf (debug_output_stream, "Current stn (b), lat, lon: %-s %f %f\n", current_stn, b[0], b[1]); 

   date_to_julian (begin_date, &begin_jdate);
   date_to_julian (end_date, &end_jdate);

   /*
    * Construct a qcfrec pointer then read the data.
    */
   construct_qcfptr (&qcfptr);
   printf ("Construct qcfptr\n");

   for (k=0;k<NUMQCPARMS;k++)
      {
      stn_numdata[k]= 0;
      for (kk=0;kk<SIGMA_PERIOD; kk++)
         stn_data[k][kk] = 0.0;
      }

   /*
    * Form sigma for all qcparms for test stn.
    */
   for (current_date=begin_jdate; current_date<begin_jdate+SIGMA_PERIOD; current_date++)
      {
      /*
       * Form name of input file and open. Assume name
       * is of the form yyddmm.0qc....for now.
       */
      printf ("\nCurrent julian date is: %d\n", current_date);
      julian_to_date (current_date, current_yr, date);

      sprintf (input_file_name, "../data/AWOSQ20/AWOSQ20_%-s.0qc", date);
      printf ("Open file: %-sxxx\n", input_file_name);

      open_file (input_file_name, "r", &input_stream);

      /*
       * Store off the data values to form sigma for a particular station,
       * parameter, and time. To form sigma for stn ILE at time
       * 00:20, only use data at stn ILE at times 00:20 on these
       * 30 days. Save a one day sum which will be subtracted out
       * and save the total sum over the complete 30 day period for
       * time t and particular stn.   DO NOT INCLUDE the weight of
       * the current station in the theta_e! We don't cause distance of 
       * zero is not allowed.
       */
      while (!feof(input_stream))
         {
         reset_qcfrec( qcfptr );   /* may not need this! */
 
         read_qcfrec (&input_stream, qcfptr);
 
         if (!strncmp(current_stn, qcfptr->statn,15))
            {
            /*
             * For now just pick out the parms at a particular time.
             */
            if ((qcfptr->hour_nom == 0) && (qcfptr->minute_nom == 0))
               {
               if (qcfptr->staprs >0.00)
                  {
                  stn_data[stnprs][stn_numdata[stnprs]] = qcfptr->staprs;
                  stn_numdata[stnprs]++;
                  }

               if (qcfptr->seaprs >0.00)
                  {
                  stn_data[slp][stn_numdata[slp]] = qcfptr->seaprs;
                  stn_numdata[slp]++;
                  }

               if (qcfptr->cmpsea >0.00)
                  { 
                  stn_data[cslp][stn_numdata[cslp]] = qcfptr->cmpsea;
                  stn_numdata[cslp]++;
                  }
 
               if (qcfptr->temp >-999.00)
                  { 
                  stn_data[temp][stn_numdata[temp]] = qcfptr->temp;
                  stn_numdata[temp]++;
                  }
 
               if (qcfptr->dewpnt >-999.00)
                  { 
                  stn_data[dewpt][stn_numdata[dewpt]] = qcfptr->dewpnt;
                  stn_numdata[dewpt]++;
                  }
 
               if (qcfptr->wndspd >-999.00)
                  { 
                  stn_data[windsp][stn_numdata[windsp]] = qcfptr->wndspd;
                  stn_numdata[windsp]++;
                  }
 
               if (qcfptr->wnddir >-999.00)
                  { 
                  stn_data[winddir][stn_numdata[winddir]] = qcfptr->wnddir;
                  stn_numdata[winddir]++;
                  }
 

               printf ("Current_stn, qcfptr->statn:  xxx%-sxxx  xxx%-sxxx\n", 
                        current_stn, qcfptr->statn); 
               printf ("qcfptr->hour_nom, qcfptr->minute_nom: %d %d\n",
                        qcfptr->hour_nom, qcfptr->minute_nom); 
               }

            } /* stn match */
 
         } /* while */

   printf ("Found %ld temp data points for current_stn: %-sxxx\n", stn_numdata[temp], current_stn);

   close_file (&input_stream);

   } /* for SIGMA_PERIOD days */

   /*
    * Compute variance (variance = std*std = sigma squared)
    * a particular stn and parameter over a two week
    * period.
    */
   for (j=0;j<NUMQCPARMS;j++)
      {
      if (stn_numdata[j] >1)
         {
         sigma_sq[j]=0.0;
         ave=svar=adev=sdev=0.0;

         for (kk=0;kk<SIGMA_PERIOD; kk++)
            {
            fprintf (debug_output_stream, "stn_data[%d][%d] = %7.2f\n", j, kk, stn_data[j][kk]);
            printf ("stn_data[%d][%d] = %7.2f\n", j, kk, stn_data[j][kk]);
            }

         determine_sigma(stn_data[j], stn_numdata[j], &ave, &svar, &adev, &sdev);
         sigma_sq[j] = svar;

         printf ("\n(var)sigma_sq[%d] = %f\n", j, sigma_sq[j]);
         fprintf (debug_output_stream, "/n(var)sigma_sq[%d] = %f\n", j, sigma_sq[j]);
         printf ("parm (%d), ave, svar, adev, sdev: %f %f %f %f\n\n", j, ave, svar, adev, sdev);
         fprintf (debug_output_stream,"parm (%d), ave, svar, adev, sdev: %f %f %f %f\n\n", 
                  j, ave, svar, adev, sdev);
         }
      else
         {
         fprintf (debug_output_stream,"\nstn_numdata[%d] not >1\n\n", j);
         printf ("stn_numdata[%d] not >1\n", j);
         }
      }

   /*
    * Construct a current qcfrec pointer.
    */
   construct_qcfptr (&current_qcfptr);

   /*
    * Have sigma_sq...get surrounding observations...compute qcflag.
    */
   for (current_date=begin_jdate; current_date<=begin_jdate/*end_jdate*/; current_date++) 
      {
      /*
       * Form name of input file and open. Assume name
       * is of the form yyddmm.0qc....for now.
       */
      printf ("\nCurrent julian date is: %d\n", current_date);
      julian_to_date (current_date, current_yr, date);

      sprintf (input_file_name, "../data/AWOSQ20/AWOSQ20_%-s.0qc", date);
      open_file (input_file_name, "r", &input_stream);
      printf ("Open file: xxx%-sxxx\n", input_file_name);

      sprintf (qcf_output_file_name, "%-s.out", input_file_name);
      printf ("Open file: xxx%-sxxx\n", qcf_output_file_name);
      open_file (qcf_output_file_name, "w", &qcf_output_stream);

      numstns = 0;

      while (!feof(input_stream))
         {
         reset_qcfrec( qcfptr );   /* may not need this! */
         read_qcfrec (&input_stream, qcfptr);

         if (numstns == 0)
            copy_qcfrec(current_qcfptr, qcfptr);
 
         /*
          * Fill up A's lat/lons and theta_o values.
          */
         if ((qcfptr->hour_nom == 0) && (qcfptr->minute_nom == 0))
            {
            A[numstns][0] = qcfptr->lat;
            A[numstns][1] = qcfptr->lon;

            if (qcfptr->staprs >0.00)
               {
               theta_o[stnprs][numstns] = qcfptr->staprs;
               numtheta_o[stnprs]++;
               }
 
            if (qcfptr->seaprs >0.00)
               {
               theta_o[slp][numstns] = qcfptr->seaprs;
               numtheta_o[slp]++;
               }
 
            if (qcfptr->cmpsea >0.00)
               {
               theta_o[cslp][numstns] = qcfptr->cmpsea;
               numtheta_o[cslp]++;
               }
 
            if (qcfptr->temp >-999.00)
               {
               theta_o[temp][numstns] = qcfptr->temp;
               numtheta_o[temp]++;
               }
 
            if (qcfptr->dewpnt >-999.00)
               {
               theta_o[dewpt][numstns] = qcfptr->dewpnt;
               numtheta_o[dewpt]++;
               }
 
            if (qcfptr->wndspd >-999.00)
               {
               theta_o[windsp][numstns] = qcfptr->wndspd;
               numtheta_o[windsp]++;
               }
 
            if (qcfptr->wnddir >-999.00)
               {
               theta_o[winddir][numstns] = qcfptr->wnddir;
               numtheta_o[winddir]++;
               }

            numstns++;   /* actually equals the number of recs at this time!! */
            }
         else
            break;

         } /* while */
      close_file (&input_stream);

      } /* for */

   printf ("Completed filling A now compute qcflag\n");

   /*
    * Compute distances and weights only once. They apply for all parameters
    * at this time. Beware applying to more times, cause at different times
    * may have different set of stns (or some stns may drop out on some parameters).
    */
   
   /*
    *  Compute distance from ref station and put in A[,4]
    */
   for (ii=0;ii<numstns;ii++)
      {
      ll2xydrv( current_qcfptr->lat, current_qcfptr->lon, &x, &y, A[ii][0], A[ii][1]); 
      A[ii][3]=sqrt(x*x+y*y);           /* distance */

      fprintf (debug_output_stream, "x, y, A[%d][dist]: %lf %lf %f\n",
              ii, x, y, A[ii][3]); 
      printf ("x, y, A[%d][3]: %lf %lf %f\n", ii, x, y, A[ii][3]);
      }
  
   /*
    * Determine "weights" (influence) of other stations.
    */
    weight(A, numstns, pmethod);

   /*
    * Determine QC flag for all parameters. (for ILE at 00:00 as test)
    */
   for (jj=0;jj<NUMQCPARMS;jj++)
      {
      /*
       * Leave Missing (M), Not Measured (N), and Glitch (X) flags unchanged.
       * Also if value is -999.99, set flag to missing.
       *
       * In orig QC:
       *   Flag Calc SLP same as Stn pressure.
       *   Wind speed must be >= 0.0.
       *   See also other wind dir adjustments if diff > 180.
       *   Wind dir must be between 0 and 360 degrees.
       *   If wind sp = 0 (calm) then set wind dir qc flag same as wind speed.
       *   Precip must be >=0.0.
       *   Precip can be estimated ('E'). Looks like Bad overrides E, but D does not.
       *   Can't have precip in clear air. (qcf.cc(1).cldamt.eq.0 but precip >0 then set both (?) to D)
       *   Dew Point temperature cannot be greater than dry bulb temperature.
       */
      switch (jj) {
         case 0:
            qcflag = current_qcfptr->staflg;
            break;
         case 1:
            qcflag = current_qcfptr->seaflg;
            break;
         case 2:
            qcflag = current_qcfptr->cmpflg;
            break;
         case 3:
            qcflag = current_qcfptr->tmpflg;
            break;
         case 4:
            qcflag = current_qcfptr->dewflg;
            break;
         case 5:
            qcflag = current_qcfptr->spdflg;
            break;
         case 6:
            qcflag = current_qcfptr->dirflg;
            break;
         default:
            printf ("Error: Unknown parameter");
      } /* switch */
 
      if (qcflag != 'M' && qcflag != 'N' && qcflag != 'X')
         {
         /*
          * QC the parameter.
          */
         theta_obs = stn_data[jj][0];/*Use current_qcfptr values-Ob at stn ILE @ t=00:00 on day 940401.*/
         qcflag = 'U';
      
         if (numtheta_o[jj] >1)
            determine_qcflag(A, theta_o[jj], numtheta_o[jj], 
                             theta_obs, sigma_sq[jj], min_weight, &qcflag);

         fprintf (debug_output_stream,
                  "\ntheta_obs, current_qcfptr->lat, current_qcfprt->lon: %7.2f %10.5f %11.5f \n", 
                  theta_obs, current_qcfptr->lat, current_qcfptr->lon);
         fprintf (debug_output_stream,"sigma_sq[%d], qcflag: %f %c \n", jj, sigma_sq[jj], qcflag);
         printf ("theta_obs,  current_qcfptr->lat, lon: %7.2f %10.5f %11.5f \n",
                  theta_obs, current_qcfptr->lat, current_qcfptr->lon);
         printf ("sigma_sq[%d], qcflag:  %f %c \n", jj, sigma_sq[jj], qcflag);

         /*
          * Assign qcflag for this parameter.
          */
         switch (jj) {
            case 0:
               current_qcfptr->staflg = qcflag;
               break;
            case 1:
               current_qcfptr->seaflg = qcflag;
               break;
            case 2:
               current_qcfptr->cmpflg = qcflag;
               break;
            case 3:
               current_qcfptr->tmpflg = qcflag;
               break;
            case 4: 
               current_qcfptr->dewflg = qcflag;
               break;
            case 5: 
               current_qcfptr->spdflg = qcflag;
               break;
            case 6: 
               current_qcfptr->dirflg = qcflag;
               break;
            default:
               printf ("Error: Attempt to set qcflag for unknown parameter");
         } /* switch */
      } /* qcflag != M,N,X */


      /*
       * Gross limit check the precip.
       */
      if (current_qcfptr->prcflg != 'M' && current_qcfptr->prcflg != 'N' &&
          current_qcfptr->prcflg != 'X')
         {
         if (current_qcfptr->precip < -999.00)
            current_qcfptr->prcflg = 'M';
         else if(current_qcfptr->precip >bmax)
            current_qcfptr->prcflg = 'B';
         else if (current_qcfptr->precip >=dmin)     /* > or >= ???? */
            current_qcfptr->prcflg = 'D';
         else
            current_qcfptr->prcflg = 'G';
         }
      } /* for */

   /*
    * Write QC'd record to qcf file.
    */
   write_qcfrec (&qcf_output_stream, current_qcfptr);

   printf ("free ptr\n");
   destruct_qcfptr (&qcfptr);

   close_file (&qcf_output_stream);
   close_file (&debug_output_stream);

   }  /* main() */
