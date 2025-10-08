/*----------------------------------------------------------------------
 * postproc_HQCdebug.c - This code post processes the debug from the
 *   HQC (main_qc) program and computes averages for the computed
 *   difference between the expected and actual values (all parms),
 *   ave of the computed bad and dub tolerances, and ave variances
 *   used in the computations.
 *
 * INPUT : List of files to be processed in a file named :
 *         'filelist.list'. This should be a list of files
 *         with the HQC debug output format as created by main_qc.c.
 *
 * OUTPUT: File with updates. (*.out)
 *
 * WARNING:
 *          There's a lot of hardcoded stuff in this s/w.
 *
 * 000 03 Jan 96 lec
 *    Created.
 *---------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <errno.h>

#define MAX_CHARS   256

/*----------------------------------------------------------------------
 *read_record() - This routine reads and returns to the caller one
 *   line from the specified input file.
 *
 * 000 09 Nov 93 lec
 *    Created.
 *---------------------------------------------------------------------*/
void read_record( /*in/out*/ FILE       **data_stream,
                  /*out*/    char       new_line[MAX_CHARS])
   {
   int   j;
   FILE  *input_stream;
   char  c;
 
   input_stream = *data_stream;
 
   for (j=0;j<MAX_CHARS;j++) new_line[j]='\0';
 
   j = -1;
   while((c=getc(input_stream))!='\n' && j< MAX_CHARS)
      {
      if(c==EOF)break;
      new_line[++j] = c;
 
      } /* while */
 
   *data_stream = input_stream;
 
   } /* read_record() */


/*----------------------------------------------------------------------
 * main()
 *
 * 000 03 Jan 96 lec
 *    Created.
 *---------------------------------------------------------------------*/
int main()
   {
   /* local variables */
   FILE         *input_stream;
   FILE         *output_stream;
   FILE         *output_DT0stream;
   FILE         *output_DT1stream;
   FILE         *output_DT2stream;
   FILE         *output_DT3stream;
   FILE         *output_DT4stream;
   FILE         *output_DT5stream;
   FILE         *output_DT6stream;
   FILE         *data_stream3;

   int          items;
   int          j, i;

   char		input_file_name[MAX_CHARS] = "\0";
   char		output_file_name[MAX_CHARS] = "\0";

   char		output_DT0file_name[MAX_CHARS] = "DT0dmp.out\0";
   char		output_DT1file_name[MAX_CHARS] = "DT1dmp.out\0";
   char		output_DT2file_name[MAX_CHARS] = "DT2dmp.out\0";
   char		output_DT3file_name[MAX_CHARS] = "DT3dmp.out\0";
   char		output_DT4file_name[MAX_CHARS] = "DT4dmp.out\0";
   char		output_DT5file_name[MAX_CHARS] = "DT5dmp.out\0";
   char		output_DT6file_name[MAX_CHARS] = "DT6dmp.out\0";

   char         new_line[MAX_CHARS] = "\0";

   int          data_type, add_ct;

   float        theta_obs, theta_e, theta_diffsq,
                alphaVar_bad, alphaVar_dub, var;

   float        theta_diff_abs = 0.0;

   char         station[27] = "\0";
   char         QCflag = '\0';

   /* All stations included in these averages. Number indicates parameter. */
   /* diffs are sums of absolute values */

   float        all_diff0_ave = 0;
   long         all_diff0_ave_ct = 0;
   float        all_tolbad0_ave = 0.0;
   long         all_tolbad0_ave_ct = 0;
   float        all_toldub0_ave = 0.0; 
   long         all_toldub0_ave_ct = 0; 
   float        all_var0_ave = 0.0;
   long         all_var0_ave_ct = 0;

   float        all_diff1_ave = 0;
   long         all_diff1_ave_ct = 0; 
   float        all_tolbad1_ave = 0.0; 
   long         all_tolbad1_ave_ct = 0; 
   float        all_toldub1_ave = 0.0;  
   long         all_toldub1_ave_ct = 0; 
   float        all_var1_ave = 0.0; 
   long         all_var1_ave_ct = 0;

   float        all_diff2_ave = 0;
   long         all_diff2_ave_ct = 0; 
   float        all_tolbad2_ave = 0.0; 
   long         all_tolbad2_ave_ct = 0; 
   float        all_toldub2_ave = 0.0;  
   long         all_toldub2_ave_ct = 0; 
   float        all_var2_ave = 0.0; 
   long         all_var2_ave_ct = 0;

   float        all_diff3_ave = 0;
   long         all_diff3_ave_ct = 0; 
   float        all_tolbad3_ave = 0.0; 
   long         all_tolbad3_ave_ct = 0; 
   float        all_toldub3_ave = 0.0;  
   long         all_toldub3_ave_ct = 0; 
   float        all_var3_ave = 0.0; 
   long         all_var3_ave_ct = 0;
   long         total_temp = 0;
   long         diffT_lt1 = 0;
   long         diffT_ge1_lt2 = 0;
   long         diffT_ge2_lt3 = 0;
   long         diffT_ge3_lt4 = 0;
   long         diffT_ge4 = 0;
   long         good_temp = 0;
   float        good_temp_diff_ave = 0.0;
   long         bad_temp;
   float        bad_temp_diff_ave = 0.0;
   long         dub_temp = 0;
   float        dub_temp_diff_ave  = 0.0;

   float        all_diff4_ave = 0;
   long         all_diff4_ave_ct = 0; 
   float        all_tolbad4_ave = 0.0; 
   long         all_tolbad4_ave_ct = 0; 
   float        all_toldub4_ave = 0.0;  
   long         all_toldub4_ave_ct = 0; 
   float        all_var4_ave = 0.0; 
   long         all_var4_ave_ct = 0;
   long         total_dew = 0;
   long         diffTD_lt1 = 0;
   long         diffTD_ge1_lt2 = 0;
   long         diffTD_ge2_lt3 = 0;
   long         diffTD_ge3_lt4 = 0;
   long         diffTD_ge4 = 0;
   long         good_dew = 0;
   float        good_dew_diff_ave = 0.0;
   long         bad_dew; 
   float        bad_dew_diff_ave = 0.0;
   long         dub_dew = 0;
   float        dub_dew_diff_ave  = 0.0; 

   float        all_diff5_ave = 0;
   long         all_diff5_ave_ct = 0; 
   float        all_tolbad5_ave = 0.0; 
   long         all_tolbad5_ave_ct = 0; 
   float        all_toldub5_ave = 0.0;  
   long         all_toldub5_ave_ct = 0; 
   float        all_var5_ave = 0.0; 
   long         all_var5_ave_ct = 0;

   float        all_diff6_ave = 0;
   long         all_diff6_ave_ct = 0; 
   float        all_tolbad6_ave = 0.0; 
   long         all_tolbad6_ave_ct = 0; 
   float        all_toldub6_ave = 0.0;  
   long         all_toldub6_ave_ct = 0; 
   float        all_var6_ave = 0.0; 
   long         all_var6_ave_ct = 0;

   /* 
    * Read each record in.
    */
   if (( data_stream3 = fopen("./filelist.txt", "r")) == NULL)
      perror ("Error: Can't open filelist.txt for reading");

   fscanf (data_stream3, "%s", input_file_name);

   while (!feof(data_stream3))
      {
      if ( feof(data_stream3))
         {
         if (fclose (input_stream) == EOF)
            perror ("Can't close input stream.");

         if (fclose (output_stream) == EOF)
            perror ("Can't close output_stream.");
         break;
         }

      printf ("\nInput name was: %-s\n", input_file_name);

      sprintf (output_file_name, "%-s.out", input_file_name);
      printf ("Output file will be named: %-s\n", output_file_name);
 
      if (( input_stream = fopen(input_file_name, "r")) == NULL)
         perror ("Error: Can't open input file");
 
      if (( output_stream = fopen(output_file_name, "w")) == NULL)
         perror ("Error: Can't open output file.");

      if (( output_DT0stream = fopen(output_DT0file_name, "w")) == NULL)
         perror ("Error: Can't open output file.");
      if (( output_DT1stream = fopen(output_DT1file_name, "w")) == NULL)
         perror ("Error: Can't open output file.");
      if (( output_DT2stream = fopen(output_DT2file_name, "w")) == NULL)
         perror ("Error: Can't open output file.");
      if (( output_DT3stream = fopen(output_DT3file_name, "w")) == NULL)
         perror ("Error: Can't open output file.");
      if (( output_DT4stream = fopen(output_DT4file_name, "w")) == NULL)
         perror ("Error: Can't open output file.");
      if (( output_DT5stream = fopen(output_DT5file_name, "w")) == NULL)
         perror ("Error: Can't open output file.");
      if (( output_DT6stream = fopen(output_DT6file_name, "w")) == NULL)
         perror ("Error: Can't open output file.");


      add_ct = 0;

      while (!feof(input_stream))
         {
         read_record( &input_stream, new_line);

         if ( feof(input_stream))
            {
            if (fclose (input_stream) == EOF)
               perror ("Can't close input stream.");

            break;
            }

/*          printf ("new_line: %-sxxx\n", new_line); */

         if (!strncmp("alpha_sq\0", &new_line[0],8) ||
            !strncmp("Alpha_sq\0", &new_line[0],8) ||
            !strncmp("Project \0", &new_line[0],8) ||
            !strncmp("Area of \0", &new_line[0],8) ||
            !strncmp("Data Fre\0", &new_line[0],8) ||
            !strncmp("First da\0", &new_line[0],8) ||
            !strncmp("Last day\0", &new_line[0],8) ||
            !strncmp("Input fi\0", &new_line[0],8) ||
            !strncmp("Sigma fi\0", &new_line[0],8) ||
            !strncmp("Precipit\0", &new_line[0],8) ||
            !strncmp("PARAMETE\0", &new_line[0],8) ||
            !strncmp("Current \0", &new_line[0],8) ||
            !strncmp("Distance\0", &new_line[0],8)   )
            {
            fprintf (output_stream, "%-s\n", new_line);
            }

         if (!strncmp("Compute dist and weights\0", &new_line[0],24))
            {
            strncpy (station, &new_line[50], 26);
            station[26] = '\0';
            }

         if (!strncmp("Current julian\0", &new_line[0],14))
            {
            fprintf (output_stream,
                "\n\nData                                   Points\n");
            fprintf (output_stream,
                    "type Station                           in AOI  ObsVal     ExpVal     Obs-Exp     TolBAD     TolDUB    VAR     QCflag\n");
            fprintf (output_stream,
                "--------------------------------------------------------------------------------------------------------------------\n");


            fprintf (output_DT0stream, "\nData                                   Points\n");
            fprintf (output_DT0stream,
                     "type Station                           in AOI  ObsVal     ExpVal     Obs-Exp     TolBAD    TolDUB    VAR     QCflag\n");
            fprintf (output_DT0stream,
                "--------------------------------------------------------------------------------------------------------------------\n");

            fprintf (output_DT1stream, "\nData                                   Points\n");
            fprintf (output_DT1stream,
                    "type Station                           in AOI  ObsVal     ExpVal     Obs-Exp     TolBAD     TolDUB    VAR     QCflag\n");
            fprintf (output_DT1stream, 
                "--------------------------------------------------------------------------------------------------------------------\n"); 

            fprintf (output_DT2stream, "\nData                                   Points\n");
            fprintf (output_DT2stream,
                    "type Station                           in AOI  ObsVal     ExpVal     Obs-Exp     TolBAD     TolDUB    VAR     QCflag\n");
            fprintf (output_DT2stream, 
                "--------------------------------------------------------------------------------------------------------------------\n"); 

            fprintf (output_DT3stream, "\nData                                   Points\n");
            fprintf (output_DT3stream,
                    "type Station                           in AOI  ObsVal     ExpVal     Obs-Exp     TolBAD     TolDUB    VAR     QCflag\n");
            fprintf (output_DT3stream, 
                "--------------------------------------------------------------------------------------------------------------------\n"); 

            fprintf (output_DT4stream, "\nData                                   Points\n");
            fprintf (output_DT4stream,
                    "type Station                           in AOI  ObsVal     ExpVal     Obs-Exp     TolBAD     TolDUB    VAR     QCflag\n");
            fprintf (output_DT4stream, 
                "--------------------------------------------------------------------------------------------------------------------\n"); 

            fprintf (output_DT5stream, "\nData                                   Points\n");
            fprintf (output_DT5stream,
                    "type Station                           in AOI  ObsVal     ExpVal     Obs-Exp     TolBAD     TolDUB    VAR     QCflag\n");
            fprintf (output_DT5stream, 
                "--------------------------------------------------------------------------------------------------------------------\n"); 

            fprintf (output_DT6stream, "\nData                                   Points\n");
            fprintf (output_DT6stream,
                    "type Station                           in AOI  ObsVal     ExpVal     Obs-Exp     TolBAD     TolDUB    VAR     QCflag\n");
            fprintf (output_DT6stream, 
                "--------------------------------------------------------------------------------------------------------------------\n"); 

            }


         if (!strncmp("Determine_qcflag\0", &new_line[0],16))
            add_ct = 0;

         if (!strncmp("Add theta_o\0", &new_line[0],11)) 
            add_ct++;

         if (!strncmp("theta_obs, theta_e\0", &new_line[0],18))  
            {  
            fscanf( input_stream, "%f %f %f\n", &theta_obs, &theta_e, &theta_diffsq); 

            read_record( &input_stream, new_line);
            data_type = atoi(&new_line[13]);          /* DATA TYPE */

            read_record( &input_stream, new_line);    /* blank     */
            read_record( &input_stream, new_line);    /* text      */
            fscanf( input_stream, "%f %f %f %f %c\n", 
                  &theta_diffsq, &alphaVar_bad, &alphaVar_dub, &var, &QCflag); 

            theta_diff_abs = fabs(theta_obs-theta_e);

            /* 
             * Got everything - dump it and reset
             */
             fprintf (output_stream, "DT%1d  %-20s %10d  %10.2f %10.2f %10.4lf %10.4lf %10.4lf %10.4f %3c\n", 
                    data_type, station, add_ct, theta_obs, theta_e, (theta_obs-theta_e), 
                    sqrt(alphaVar_bad), sqrt(alphaVar_dub), var, QCflag); 


            switch ( data_type ) {
               case 0:
                  all_diff0_ave = all_diff0_ave + theta_diff_abs;
                  all_diff0_ave_ct++;
                  all_tolbad0_ave = all_tolbad0_ave + sqrt(alphaVar_bad);
                  all_tolbad0_ave_ct++;
                  all_toldub0_ave = all_toldub0_ave + sqrt(alphaVar_dub);
                  all_toldub0_ave_ct++;
                  all_var0_ave = all_var0_ave + var;
                  all_var0_ave_ct++;

                  fprintf (output_DT0stream, "DT%1d  %-20s %10d  %10.2f %10.2f %10.4lf %10.4lf %10.4lf %10.4f %3c\n",
                    data_type, station, add_ct, theta_obs, theta_e, (theta_obs-theta_e),
                    sqrt(alphaVar_bad), sqrt(alphaVar_dub), var, QCflag);

                  break;
               case 1:
                  all_diff1_ave = all_diff1_ave + theta_diff_abs;
                  all_diff1_ave_ct++;
                  all_tolbad1_ave = all_tolbad1_ave + sqrt(alphaVar_bad);
                  all_tolbad1_ave_ct++;
                  all_toldub1_ave = all_toldub1_ave + sqrt(alphaVar_dub);
                  all_toldub1_ave_ct++;
                  all_var1_ave = all_var1_ave + var;
                  all_var1_ave_ct++;

                  fprintf (output_DT1stream, "DT%1d  %-20s %10d  %10.2f %10.2f %10.4lf %10.4lf %10.4lf %10.4f %3c\n",
                    data_type, station, add_ct, theta_obs, theta_e, (theta_obs-theta_e),
                    sqrt(alphaVar_bad), sqrt(alphaVar_dub), var, QCflag);
                  break;

               case 2:
                  all_diff2_ave = all_diff2_ave + theta_diff_abs;
                  all_diff2_ave_ct++;
                  all_tolbad2_ave = all_tolbad2_ave + sqrt(alphaVar_bad);
                  all_tolbad2_ave_ct++;
                  all_toldub2_ave = all_toldub2_ave + sqrt(alphaVar_dub);
                  all_toldub2_ave_ct++;
                  all_var2_ave = all_var2_ave + var;
                  all_var2_ave_ct++;

                  fprintf (output_DT2stream, "DT%1d  %-20s %10d  %10.2f %10.2f %10.4lf %10.4lf %10.4lf %10.4f %3c\n",
                    data_type, station, add_ct, theta_obs, theta_e, (theta_obs-theta_e),
                    sqrt(alphaVar_bad), sqrt(alphaVar_dub), var, QCflag);
                  break;
               case 3:
                  all_diff3_ave = all_diff3_ave + theta_diff_abs;
                  all_diff3_ave_ct++;
                  all_tolbad3_ave = all_tolbad3_ave + sqrt(alphaVar_bad);
                  all_tolbad3_ave_ct++;
                  all_toldub3_ave = all_toldub3_ave + sqrt(alphaVar_dub);
                  all_toldub3_ave_ct++;
                  all_var3_ave = all_var3_ave + var;
                  all_var3_ave_ct++;

/*                   fprintf (output_DT3stream, "fabs(theta_obs-theta_e),all_diff3_ave, all_diff3_ave_ct, 1/2: %10.4f %10.4f %ld %10.4f\n",
                          fabs(theta_obs-theta_e), all_diff3_ave, all_diff3_ave_ct, all_diff3_ave/all_diff3_ave_ct); */
                  total_temp++;

                  if ( theta_diff_abs < 1.0 )  diffT_lt1++;
                  if (theta_diff_abs >= 1.0 && theta_diff_abs < 2.0) diffT_ge1_lt2 ++;
                  if (theta_diff_abs >= 2.0 && theta_diff_abs < 3.0) diffT_ge2_lt3 ++;
                  if (theta_diff_abs >= 3.0 && theta_diff_abs < 4.0) diffT_ge3_lt4 ++;
                  if (theta_diff_abs >= 4.0)  diffT_ge4++;

                  if (QCflag == 'G') 
                     {
                     good_temp++;
                     good_temp_diff_ave = good_temp_diff_ave + theta_diff_abs;
                     }
                  if (QCflag == 'B') 
                     {
                     bad_temp++;
                     bad_temp_diff_ave = bad_temp_diff_ave + theta_diff_abs;
                     }
                  if (QCflag == 'D')
                     {
                     dub_temp++; 
                     dub_temp_diff_ave = dub_temp_diff_ave + theta_diff_abs;
                     } 



                  fprintf (output_DT3stream, "DT%1d  %-20s %10d  %10.2f %10.2f %10.4lf %10.4lf %10.4lf %10.4f %3c\n",
                    data_type, station, add_ct, theta_obs, theta_e, (theta_obs-theta_e),
                    sqrt(alphaVar_bad), sqrt(alphaVar_dub), var, QCflag);
                  break; 

               case 4:
                  all_diff4_ave = all_diff4_ave + theta_diff_abs;
                  all_diff4_ave_ct++;
                  all_tolbad4_ave = all_tolbad4_ave + sqrt(alphaVar_bad);
                  all_tolbad4_ave_ct++;
                  all_toldub4_ave = all_toldub4_ave + sqrt(alphaVar_dub);
                  all_toldub4_ave_ct++;
                  all_var4_ave = all_var4_ave + var;
                  all_var4_ave_ct++;

                  total_dew++;

                  if ( theta_diff_abs < 1.0 )  diffTD_lt1++; 
                  if (theta_diff_abs >= 1.0 && theta_diff_abs < 2.0) diffTD_ge1_lt2 ++;
                  if (theta_diff_abs >= 2.0 && theta_diff_abs < 3.0) diffTD_ge2_lt3 ++;
                  if (theta_diff_abs >= 3.0 && theta_diff_abs < 4.0) diffTD_ge3_lt4 ++;
                  if (theta_diff_abs >= 4.0)  diffTD_ge4++;
 
                  if (QCflag == 'G') 
                     {
                     good_dew++;
                     good_dew_diff_ave = good_dew_diff_ave + theta_diff_abs;
                     }
                  if (QCflag == 'B') 
                     {
                     bad_dew++; 
                     bad_dew_diff_ave = bad_dew_diff_ave + theta_diff_abs;
                     } 
                  if (QCflag == 'D')
                     { 
                     dub_dew++;  
                     dub_dew_diff_ave = dub_dew_diff_ave + theta_diff_abs;
                     }  

                  fprintf (output_DT4stream, "DT%1d  %-20s %10d  %10.2f %10.2f %10.4lf %10.4lf %10.4lf %10.4f %3c\n",
                    data_type, station, add_ct, theta_obs, theta_e, (theta_obs-theta_e),
                    sqrt(alphaVar_bad), sqrt(alphaVar_dub), var, QCflag);
                  break;

               case 5:
                  all_diff5_ave = all_diff5_ave + theta_diff_abs;
                  all_diff5_ave_ct++;
                  all_tolbad5_ave = all_tolbad5_ave + sqrt(alphaVar_bad);
                  all_tolbad5_ave_ct++;
                  all_toldub5_ave = all_toldub5_ave + sqrt(alphaVar_dub);
                  all_toldub5_ave_ct++;
                  all_var5_ave = all_var5_ave + var;
                  all_var5_ave_ct++;
 
                  fprintf (output_DT5stream, "DT%1d  %-20s %10d  %10.2f %10.2f %10.4lf %10.4lf %10.4lf %10.4f %3c\n",
                    data_type, station, add_ct, theta_obs, theta_e, (theta_obs-theta_e),
                    sqrt(alphaVar_bad), sqrt(alphaVar_dub), var, QCflag);
                  break; 
               case 6:
                  all_diff6_ave = all_diff6_ave + theta_diff_abs;
                  all_diff6_ave_ct++;
                  all_tolbad6_ave = all_tolbad6_ave + sqrt(alphaVar_bad);
                  all_tolbad6_ave_ct++;
                  all_toldub6_ave = all_toldub6_ave + sqrt(alphaVar_dub);
                  all_toldub6_ave_ct++;
                  all_var6_ave = all_var6_ave + var;
                  all_var6_ave_ct++;

                  fprintf (output_DT6stream, "DT%1d  %-20s %10d  %10.2f %10.2f %10.4lf %10.4lf %10.4lf %10.4f %3c\n",
                    data_type, station, add_ct, theta_obs, theta_e, (theta_obs-theta_e),
                    sqrt(alphaVar_bad), sqrt(alphaVar_dub), var, QCflag);
                  break;
               default:
                  printf ("Error: bad data_type!\n");
               } /* switch */

            add_ct = 0;

            } /* theta_e theta_obs */

         } /* while feof(input_stream) */

         /*
          * Dump the ave values.
          */
         fprintf (output_stream, "\n\nFollowing Averages contain ALL station/networks:\n          Ave_ABS_diff, Ave_TolBad, Ave_TolDub, Ave_Variance\n");
         fprintf (output_DT0stream, "\n\nFollowing Averages contain ALL station/networks:\n       Ave_ABS_diff, Ave_TolBad, Ave_TolDub, Ave_Variance\n");
         fprintf (output_DT1stream, "\n\nFollowing Averages contain ALL station/networks:\n       Ave_ABS_diff, Ave_TolBad, Ave_TolDub, Ave_Variance\n");
         fprintf (output_DT2stream, "\n\nFollowing Averages contain ALL station/networks:\n       Ave_ABS_diff, Ave_TolBad, Ave_TolDub, Ave_Variance\n");
         fprintf (output_DT3stream, "\n\nFollowing Averages contain ALL station/networks:\n       Ave_ABS_diff, Ave_TolBad, Ave_TolDub, Ave_Variance\n");
         fprintf (output_DT4stream, "\n\nFollowing Averages contain ALL station/networks:\n       Ave_ABS_diff, Ave_TolBad, Ave_TolDub, Ave_Variance\n");
         fprintf (output_DT5stream, "\n\nFollowing Averages contain ALL station/networks:\n       Ave_ABS_diff, Ave_TolBad, Ave_TolDub, Ave_Variance\n");
         fprintf (output_DT6stream, "\n\nFollowing Averages contain ALL station/networks:\n       Ave_ABS_diff, Ave_TolBad, Ave_TolDub, Ave_Variance\n");

         if (all_diff0_ave_ct!=0 && all_tolbad0_ave_ct!=0 &&
             all_toldub0_ave_ct!=0 && all_var0_ave_ct!=0)
            {
            all_diff0_ave = all_diff0_ave/(float)all_diff0_ave_ct;
            all_tolbad0_ave = all_tolbad0_ave/(float)all_tolbad0_ave_ct;
            all_toldub0_ave = all_toldub0_ave/(float)all_toldub0_ave_ct;
            all_var0_ave = all_var0_ave/(float)all_var0_ave_ct;
            } 
         fprintf (output_stream, "Stn Press: %10.4f %10.4f %10.4f %10.4f\n",
           all_diff0_ave, all_tolbad0_ave, all_toldub0_ave, all_var0_ave);
 
         fprintf (output_DT0stream, "Stn Press: %10.4f %10.4f %10.4f %10.4f\n",
           all_diff0_ave, all_tolbad0_ave, all_toldub0_ave, all_var0_ave);


         if (all_diff1_ave_ct!=0 && all_tolbad1_ave_ct!=0 && 
             all_toldub1_ave_ct!=0 && all_var1_ave_ct!=0)
            { 
            all_diff1_ave = all_diff1_ave/(float)all_diff1_ave_ct;
            all_tolbad1_ave = all_tolbad1_ave/(float)all_tolbad1_ave_ct;
            all_toldub1_ave = all_toldub1_ave/(float)all_toldub1_ave_ct;
            all_var1_ave = all_var1_ave/(float)all_var1_ave_ct;
           } 
         fprintf (output_stream, "SLP      : %10.4f %10.4f %10.4f %10.4f\n",
           all_diff1_ave, all_tolbad1_ave, all_toldub1_ave, all_var1_ave);

         fprintf (output_DT1stream, "SLP      : %10.4f %10.4f %10.4f %10.4f\n",
           all_diff1_ave, all_tolbad1_ave, all_toldub1_ave, all_var1_ave);

         if (all_diff2_ave_ct!=0 && all_tolbad2_ave_ct!=0 && 
             all_toldub2_ave_ct!=0 && all_var2_ave_ct!=0)
            { 
            all_diff2_ave = all_diff2_ave/(float)all_diff2_ave_ct;
            all_tolbad2_ave = all_tolbad2_ave/(float)all_tolbad2_ave_ct;
            all_toldub2_ave = all_toldub2_ave/(float)all_toldub2_ave_ct;
            all_var2_ave = all_var2_ave/(float)all_var2_ave_ct;
            }
 
         fprintf (output_stream, "CSLP     : %10.4f %10.4f %10.4f %10.4f\n",
           all_diff2_ave, all_tolbad2_ave, all_toldub2_ave, all_var2_ave);

         fprintf (output_DT2stream, "CSLP     :%10.4f %10.4f %10.4f %10.4f\n",
           all_diff2_ave, all_tolbad2_ave, all_toldub2_ave, all_var2_ave);

/* Temperature */

         if (all_diff3_ave_ct!=0 && all_tolbad3_ave_ct!=0 && 
             all_toldub3_ave_ct!=0 && all_var3_ave_ct!=0)
            { 
            all_diff3_ave = all_diff3_ave/((float)(all_diff3_ave_ct));
            all_tolbad3_ave = all_tolbad3_ave/((float)(all_tolbad3_ave_ct));
            all_toldub3_ave = all_toldub3_ave/((float)(all_toldub3_ave_ct));
            all_var3_ave = all_var3_ave/((float)(all_var3_ave_ct));
            }
 
         fprintf (output_stream, "Temp     : %10.4f %10.4f %10.4f %10.4f\n",
           all_diff3_ave, all_tolbad3_ave, all_toldub3_ave, all_var3_ave);
         fprintf (output_DT3stream, "Temp    : %10.4f %10.4f %10.4f %10.4f\n",
           all_diff3_ave, all_tolbad3_ave, all_toldub3_ave, all_var3_ave);

         if (good_temp != 0)
           good_temp_diff_ave  = good_temp_diff_ave/((float)(good_temp));
         if (bad_temp != 0)
           bad_temp_diff_ave  = bad_temp_diff_ave/((float)(bad_temp));
         if (dub_temp != 0)
           dub_temp_diff_ave  = dub_temp_diff_ave/((float)(dub_temp));

         fprintf (output_DT3stream, "\nAVE(ABS(Exp-Actual)) -    G, B, D:\n           %10.4f %10.4f %10.4f\n", 
                  good_temp_diff_ave,  bad_temp_diff_ave, dub_temp_diff_ave);
         fprintf (output_DT3stream, "\nTotal_temp=%ld, Num pts     G, B, D:  %ld %ld %ld\n",
                  total_temp, good_temp, bad_temp, dub_temp);
         fprintf (output_DT3stream, "Total_temp=%ld, Percent pts G, B, D:  %10.4f %10.4f %10.4f\n",
                  total_temp, 100.0*((float)good_temp/(float)total_temp), 
                              100.0*((float)bad_temp/(float)total_temp), 
                              100.0*((float)dub_temp/(float)total_temp) );

         fprintf (output_DT3stream, "\nNumber        ABS(Exp-Actual)diffs <1.0C: %ld\n", diffT_lt1);
         fprintf (output_DT3stream, "\nNumber 1.0C <=ABS(Exp-Actual)diffs <2.0C: %ld\n", diffT_ge1_lt2);
         fprintf (output_DT3stream, "\nNumber 2.0C <=ABS(Exp-Actual)diffs <3.0C: %ld\n", diffT_ge2_lt3);
         fprintf (output_DT3stream, "\nNumber 3.0C <=ABS(Exp-Actual)diffs <4.0C: %ld\n", diffT_ge3_lt4);
         fprintf (output_DT3stream, "\nNumber        ABS(Exp-Actual)diffs >=4.0C: %ld\n", diffT_ge4);

         total_temp = 0;
         diffT_lt1 = 0; 
         diffT_ge1_lt2 = 0;
         diffT_ge2_lt3 = 0;
         diffT_ge3_lt4 = 0;
         diffT_ge4 = 0;


/* Dew POINT */

         if (all_diff4_ave_ct!=0 && all_tolbad4_ave_ct!=0 && 
             all_toldub4_ave_ct!=0 && all_var4_ave_ct!=0)
            { 
            all_diff4_ave = all_diff4_ave/((float)(all_diff4_ave_ct));
            all_tolbad4_ave = all_tolbad4_ave/((float)(all_tolbad4_ave_ct));
            all_toldub4_ave = all_toldub4_ave/((float)(all_toldub4_ave_ct));
            all_var4_ave = all_var4_ave/((float)(all_var4_ave_ct));
            }
 
         fprintf (output_stream, "Dew Pt   : %10.4f %10.4f %10.4f %10.4f\n",
           all_diff4_ave, all_tolbad4_ave, all_toldub4_ave, all_var4_ave);
         fprintf (output_DT4stream, "Dew Pt   : %10.4f %10.4f %10.4f %10.4f\n",                 
           all_diff4_ave, all_tolbad4_ave, all_toldub4_ave, all_var4_ave);

         if (good_dew != 0)
           good_dew_diff_ave  = good_dew_diff_ave/((float)(good_dew)); 
         if (bad_dew != 0) 
           bad_dew_diff_ave  = bad_dew_diff_ave/((float)(bad_dew)); 
         if (dub_dew != 0)
           dub_dew_diff_ave  = dub_dew_diff_ave/((float)(dub_dew));

         fprintf (output_DT4stream, "\nAVE(ABS(Exp-Actual)) -    G, B, D:\n           %10.4f %10.4f %10.4f\n", 
                  good_dew_diff_ave,  bad_dew_diff_ave, dub_dew_diff_ave);
         fprintf (output_DT4stream, "\nTotal_dew=%ld, Num pts     G, B, D:  %ld %ld %ld\n", 
                  total_dew, good_dew, bad_dew, dub_dew);
         fprintf (output_DT4stream, "Total_dew=%ld, Percent pts G, B, D:  %10.4f %10.4f %10.4f\n",
                  total_dew, 100.0*((float)good_dew/(float)total_dew), 
                             100.0*((float)bad_dew/(float)total_dew),
                             100.0*((float)dub_dew/(float)total_dew)  ); 
         fprintf (output_DT4stream, "Percent pts G, B, D:  %10.4f %10.4f %10.4f\n",
                  100.0*((float)good_dew/(float)total_dew), 100.0*((float)bad_dew/(float)total_dew), 
                  100.0*((float)dub_dew/(float)total_dew) ); 


         fprintf (output_DT4stream,"\nNumber [ABS(Exp-Actual)diffs <1.0C]: %ld\n", diffTD_lt1);
         fprintf (output_DT4stream,"\nNumber [1.0C <=ABS(Exp-Actual)diffs <2.0C]: %ld\n", diffTD_ge1_lt2);
         fprintf (output_DT4stream,"\nNumber [2.0C <=ABS(Exp-Actual)diffs <3.0C]: %ld\n", diffTD_ge2_lt3); 
         fprintf (output_DT4stream,"\nNumber [3.0C <=ABS(Exp-Actual)diffs <4.0C]: %ld\n", diffTD_ge3_lt4);
         fprintf (output_DT4stream,"\nNumber [ABS(Exp-Actual)diffs >=4.0C]: %ld\n", diffTD_ge4);   
 
         total_temp = 0;
         diffTD_lt1 = 0;    
         diffTD_ge1_lt2 = 0; 
         diffTD_ge2_lt3 = 0;
         diffTD_ge3_lt4 = 0;
         diffTD_ge4 = 0;

         total_dew = 0;

/* Wind Speed */

         if (all_diff5_ave_ct!=0 && all_tolbad5_ave_ct!=0 && 
             all_toldub5_ave_ct!=0 && all_var5_ave_ct!=0)
            { 
            all_diff5_ave = all_diff5_ave/(float)all_diff5_ave_ct;
            all_tolbad5_ave = all_tolbad5_ave/(float)all_tolbad5_ave_ct;
            all_toldub5_ave = all_toldub5_ave/(float)all_toldub5_ave_ct;
            all_var5_ave = all_var5_ave/(float)all_var5_ave_ct;
            } 

         fprintf (output_stream, "Wind Sp  : %10.4f %10.4f %10.4f %10.4f\n",
           all_diff5_ave, all_tolbad5_ave, all_toldub5_ave, all_var5_ave);
         fprintf (output_DT5stream, "Wind Sp  : %10.4f %10.4f %10.4f %10.4f\n",
           all_diff5_ave, all_tolbad5_ave, all_toldub5_ave, all_var5_ave);

 
         if (all_diff6_ave_ct!=0 && all_tolbad6_ave_ct!=0 && 
             all_toldub6_ave_ct!=0 && all_var6_ave_ct!=0)
            { 
            all_diff6_ave = all_diff6_ave/(float)all_diff6_ave_ct;
            all_tolbad6_ave = all_tolbad6_ave/(float)all_tolbad6_ave_ct;
            all_toldub6_ave = all_toldub6_ave/(float)all_toldub6_ave_ct;
            all_var6_ave = all_var6_ave/(float)all_var6_ave_ct;
            }
 
         fprintf (output_stream, "Wind Dir : %10.4f %10.4f %10.4f %10.4f\n",
           all_diff6_ave, all_tolbad6_ave, all_toldub6_ave, all_var6_ave);
         fprintf (output_DT6stream, "Wind Dir : %10.4f %10.4f %10.4f %10.4f\n",
           all_diff6_ave, all_tolbad6_ave, all_toldub6_ave, all_var6_ave);


      fscanf (data_stream3, "%s", input_file_name);

      } /* while feof(data_stream3) */

   if (fclose (data_stream3) == EOF)
      perror ("Can't close data_stream3.");

   }  /* main() */
