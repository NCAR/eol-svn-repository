#ifndef lint
static char *rcsid = "$Id$";
#endif


/*
 * $Log$
 *
 */

/*--------------------------------------------------------
 * build_stn_info - Main module which controls the processing
 *     required to create the `stations' file and the 
 *     `stn_id' file which become CODIAC database files.
 *     The `stations' file is analogous to the Master
 *     station list. It is a complete list of ALL stns
 *     (lat/lon/occur/period of coverage/etc) in a 
 *     particular project.
 *
 * Input:
 *
 * Output:
 *
 * 09 May 94 lec
 *   Created.
 *-------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "local.h"
#include "process_qcdata.h"
#include "process_stn_data.h"


/* local functions */
#ifdef __STDC__
   int main (int argc, char *argv[]);
#else /*!__STDC__*/
   int main ();
#endif /*__STDC__*/


/*----------------------------------------------------------------------
 * main() - This is the main program which controls the basic
 *     processing and control.
 * Input: xxx - type of data being processed.
 *
 * Output:
 *
 * 09 May 94 lec
 *   Created.
 *---------------------------------------------------------------------*/
int main( argc, argv)
int argc;
char *argv[];
   {
   /* local variables */
   FILE     *data_input_stream;
   FILE     *station_output_stream;
   FILE     *stn_id_output_stream;

   char     old_stn_input_file_name[NAMELEN] = "\0";
   char     new_stn_input_file_name[NAMELEN] = "\0";
   char     data_input_file_name[NAMELEN] = "\0";
   char     metadata_input_file_name[NAMELEN] = "\0";
   char     station_output_file_name[NAMELEN] = "station.out\0";
   char     stn_id_output_file_name[NAMELEN] = "stn_id.out\0";

   int      arg = 0;
   int      i = 0;
   int      write_recs = 0;

   STNREC   *prev_stnptr;

   char     ct_str[10] = "\0\0\0\0\0\0\0\0\0\0";

   char     new_stnid[26] = "\0";
   char     current_stnid[26] = "???\0";
   char     new_platform[26] = "???\0";
   char     current_platform[26] = "\0";
   char     newline[MAX_CHARS];

   char     junk_str[26] = "\0";
   char     error_str[NAMELEN] = 
   "build_stn_info -{qcf,pqcf,dpqcf,usgs_sqcf,tva_sqcf,spec_qcf} -I infile -IM metadata_infile -IO old_stn_file -IN new_stn_file -C stn_ct\n";

   static long     num_old_stns = 0;
   static long     num_new_stns = 0;
   static long     num_meta_stns = 0;
   static STNREC   *stnptr = NULL;
   static long     current_stnid_ct = 1;

   printf ("Begin build_stn_info processing! arg, argc: %d %d\n", arg, argc);

   /*
    * Determine what type of files are being processed.
    * This s/w should be able to handle: qcf, hourly pqcf, 
    * daily pqcf, sqcf (TVA and USGS),....(add to the list).
    */
   arg = 1;
   if (argc == 1)
      {
      fprintf (stderr, error_str);
      exit(1);
      }

   if (!strcmp(argv[arg], "-qcf"))       /* standard quality control (QC) format  - surface*/
      {                                  /* Both nominal and obs times.                    */
      printf ("QCF data\n");
      current_data_type = qcf;
      }
   else if (!strcmp(argv[arg], "-pqcf"))      /* precip QC format       */
      { 
      printf ("PQCF data\n"); 
      current_data_type = pqcf;
      } 
   else if (!strcmp(argv[arg], "-dpqcf"))     /* daily precip QC format */
      { 
      printf ("DPQCF data\n"); 
      current_data_type = dpqcf;
      } 
   else if (!strcmp(argv[arg], "-usgs_sqcf")) /* USGS hourly streamflow QC format */
      { 
      printf ("USGS_SQCF data\n"); 
      current_data_type = usgs_sqcf;
      } 
   else if (!strcmp(argv[arg], "-tva_sqcf"))  /* TVA hourly (bi-hrly data) streamflow QC format */
      { 
      printf ("TVA_SQCF data\n"); 
      current_data_type = tva_sqcf;
      } 
   else if (!strcmp(argv[arg], "-spec_qcf"))  /* NCDC specials qcf format - one time only */
      {
      printf ("NCDC specials data\n");
      current_data_type = spec_qcf;
      } 
   else
      {
      current_data_type = UNKNOWN;
      fprintf (stderr, "Error on input data type!\n");
      fprintf (stderr, error_str);
      exit(1);
      }

   arg++;
   if (arg == argc)
      {
      fprintf (stderr, error_str);
      exit(1);
      }

   if (!strcmp( argv[arg], "-I"))
      {
      arg++;
      if (arg == argc)
         {
         fprintf (stderr, error_str);
         exit(1);
         }

      strcpy (data_input_file_name, argv[arg]);
      }

   arg++; 
   if (arg == argc)
      { 
      fprintf (stderr, error_str);
      exit(1); 
      } 

   if (!strcmp( argv[arg], "-IM")) 
      { 
      arg++;
      if (arg == argc) 
         { 
         fprintf (stderr, error_str);
         exit(1); 
         } 

      strcpy (metadata_input_file_name, argv[arg]);
      } 

   arg++;
   if (arg == argc)
      {
      fprintf (stderr, error_str);
      exit(1);
      }

   if (!strcmp( argv[arg], "-IO"))
      {
      arg++;
      if (arg == argc)
         {
         fprintf (stderr, error_str);
         exit(1);
         }

      strcpy (old_stn_input_file_name, argv[arg]);
      }

   arg++;
   if (arg == argc)
      {
      fprintf (stderr, error_str);
      exit(1);
      }
 
   if (!strcmp( argv[arg], "-IN"))
      {
      arg++;
      if (arg == argc)
         {
         fprintf (stderr, error_str);
         exit(1);
         }
 
      strcpy (new_stn_input_file_name, argv[arg]);
      }


   arg++;
   if (arg == argc)
      {
      fprintf (stderr, "WARNING: internal stn id count will start with one!");
      }
 
   if (!strcmp( argv[arg], "-C"))
      {
      arg++;
      if (arg == argc)
         {
         fprintf (stderr, error_str);
         exit(1);
         }
 
      strcpy (ct_str, argv[arg]);
      current_stnid_ct = atol (ct_str);
      }


   if (!strcmp(data_input_file_name, "\0"))
      {
      fprintf (stderr, "Error on data input file name!\n");
      fprintf (stderr, error_str);
      exit(1);
      }

   if (!strcmp(metadata_input_file_name, "\0"))
      { 
      fprintf (stderr, "Error on META data input file name!\n");
      fprintf (stderr, error_str);
      exit(1); 
      }

   if (!strcmp(old_stn_input_file_name, "\0"))
      {
      fprintf (stderr, "Error on old stn input file name!\n");
      fprintf (stderr, error_str);
      exit(1);
      }


   /*
    * Open the quality controlled data (one of the
    * specified qc formats), and the `existing' stations
    * file and the stn_id file. 
    */
   if (( data_input_stream = fopen(data_input_file_name, "r")) == NULL)
      perror ("Error: Can't open data_input_stream");

   /*
    * Open the ascii output file for writing.
    */
   if (( station_output_stream = fopen(station_output_file_name, "w")) == NULL)
      perror ("Error: Can't open station output_file");

   if (( stn_id_output_stream = fopen(stn_id_output_file_name, "w")) == NULL)
      perror ("Error: Can't open stn_id_output_file");

   /* Initialization */

   /*
    * Read in default station record values only once.
    */
   printf ("Main: get_defaults!\n");
   get_defaults();

   /*
    * Read in old stn info (i.e., old master stn list).
    * For GIDS-1 this is fest_stn_list.
    */
   printf ("Main: read_old_stn_info!\n");
   read_old_stn_info (old_stn_input_file_name, &num_old_stns);

   /*
    * Read in new stn info (i.e., new/forming master stn list).
    */
   printf ("Main: read_new_stn_info!\n");
   read_new_stn_info (new_stn_input_file_name, &num_new_stns);


   /*
    * Construct STNREC pointers.
    */
   stnptr = (STNREC *) malloc (sizeof(STNREC));
   if (stnptr == NULL)
      perror ("Error: Can't malloc STNREC space!");


   prev_stnptr = (STNREC *) malloc (sizeof(STNREC));
   if (prev_stnptr == NULL)
      perror ("Error: Can't malloc prev STNREC space!");


   /*
    * Initialize station ptr records.
    */
  /*  printf ("Main:reset_stnrec!\n"); */
   reset_stnrec (stnptr);
   reset_stnrec (prev_stnptr);

  /* printf ("Main: project: %-sxxx\n", stnptr->project); */


   /*
    * Process the metadata only once, when required
    */
   switch (current_data_type)
      {
      case qcf :
         break;
      case pqcf :
         break;
      case dpqcf :
         process_dpqcf_metadata(metadata_input_file_name, &num_meta_stns);
         break;
      case usgs_sqcf :
         process_usgs_sqcf_metadata(metadata_input_file_name, &num_meta_stns);
         break;
      case tva_sqcf :
         process_tva_sqcf_metadata(metadata_input_file_name, &num_meta_stns);
         break;
      case spec_qcf :
         break;
      default:
         printf ("Error: current_data_type is unknown!");
         break;
      } /* switch */


   /*
    * Process all the data in the qc data file.
    * Each of these stns must be accounted for.
    * Data is expected to be sorted by stn ID and
    * then by date/time.
    */
   while (!feof(data_input_stream))
      {
      read_record( &data_input_stream, newline);
      /* printf ("newline: %-sxxx\n", newline); */
 
      if ( feof(data_input_stream) || !strncmp(newline, "\0",1)
          || !strncmp(newline, "  ",2) )
         {
         /*
          * Write out last rec and exit!
          */
        /* printf ("Write out last rec of input file!\n"); */
         if (!stns_equal( stnptr, prev_stnptr ) && write_recs)
            {
            write_stations_rec (station_output_stream, stnptr);

            write_stn_id_rec (stn_id_output_stream, stnptr); /* was not in check */
            }

         break;
         }

      /*
       * Pick the new info out of the current newline.
       */
      for (i=0;i++;i<25)
         {
         new_stnid[i] = '\0';
         new_platform[i] = '\0';
         }

      switch (current_data_type)
         {
         case qcf :
            strncpy (new_stnid, &newline[41], 10);
            strncpy (new_platform, &newline[30], 10);
            break;
         case pqcf :
            strncpy (new_stnid, &newline[29], 10);
            strncpy (new_platform, &newline[19], 10);
            break;
         case dpqcf :
            strncpy (new_stnid, &newline[18], 15);
            strncpy (new_platform, &newline[8], 10);
            break;
         case usgs_sqcf :
            strncpy (new_stnid, &newline[18], 10);
            strncpy (new_platform, &newline[8], 10);
            break;
         case tva_sqcf :
            strncpy (new_stnid, &newline[48], 10);
            strncpy (new_platform, &newline[37], 10);
            break;
         case spec_qcf :
            strncpy (new_stnid, &newline[26], 10);
            strncpy (new_platform, &newline[15], 10);
            break;
         default:
            printf ("Error: current_data_type is unknown!");
            break;
         } /* switch */

      /*
       * Only process the first rec for each stn.
       * Once stn identified and stnptr full, skip
       * next processing section until last rec for
       * current stn. Extract and set end of period
       * of coverage from last rec for current stn.
       *
       * This s/w assumes that the data has been
       * sorted by lat/lon/name/platform/time.
       *
       * If the stn id changes or the platform changes
       * then we have a new stn. The below check should
       * be a general strcmp against platform types, but
       * to process the QCF data, we only compare the
       * first 4 chars. In the Hrly Sfc Cmp (QCF) data
       * there are different types of ASOS and AWOS data
       * (e.g., ASOS1) which pt to same platform type of
       * ASOS or AWOS. Checking only first 4 chars prevents
       * calling diff ASOS types at exact same loc and with
       * exact same name, diff stations. Could maybe check
       * for ASOS/AWOS, but may be other platforms with 
       * similar types. Worst case here is that we assume
       * each platform is uniquely identified by first 4 chars.
       * This should probably be updated. Maybe the real fix
       * is to either id these diff ASOS types as diff platforms!
       * They weren't in FEST and we must stay by FEST standards
       * for GIDS1!
       */
/*    printf ("new_stnid, current_stnid:: %-sxxx %-sxxx\n", 
               new_stnid, current_stnid);  
      printf ("new_platform, current_platform:: %-sxxx %-sxxx\n",
               new_platform, current_platform);  */

      if ( strcmp(new_stnid, current_stnid) || strncmp(new_platform, current_platform, 4)) 
         {
         /*
          * Have a new stn. Don't write out
          * anything if this is the first rec in file.
          */
         /* printf ("Have a new stn!!!!\n"); */
         if (strncmp (current_stnid, "???", 3))
            {
            /*
             * Write out recs to files which become input
             * to database.
             */
            if (!stns_equal( stnptr, prev_stnptr ) && write_recs)
               {
               write_stations_rec (station_output_stream, stnptr);
 
               write_stn_id_rec (stn_id_output_stream, stnptr); /* was not in check */
               }
   
            copy_stn( stnptr, prev_stnptr); /* copy stnptr to prev_stnptr! */
            }
 
         /*
          * Reset the stn ptr.
          */
        /*  printf ("reset_stnrec!\n"); */
         reset_stnrec (stnptr);
         write_recs = 0;

         /*
          * Process the current rec just read in.
          */
         switch (current_data_type)
            {
            case qcf :
               process_qcfrec (newline, num_new_stns, num_old_stns,  current_stnid_ct,
                               metadata_input_file_name, stnptr, &write_recs);
               break;
            case pqcf :
               process_pqcfrec (newline, num_old_stns,  current_stnid_ct,
                                metadata_input_file_name, stnptr);
               write_recs = 1; /* this should be in call */
               break;
            case dpqcf :
               process_dpqcfrec(newline, num_new_stns, num_old_stns, num_meta_stns, current_stnid_ct,
                                metadata_input_file_name, stnptr, &write_recs);
               break;
            case usgs_sqcf :
               process_usgs_sqcfrec (newline, num_old_stns, num_meta_stns, current_stnid_ct,
                                     metadata_input_file_name, stnptr);
               write_recs = 1; /* this should be in call */
               break;
            case tva_sqcf :
               process_tva_sqcfrec (newline, num_old_stns,  current_stnid_ct,
                                    metadata_input_file_name, stnptr);
               write_recs = 1; /* this should be in call */
               break;
            case spec_qcf :
               process_spec_qcfrec (newline, num_new_stns, num_old_stns,  current_stnid_ct,
                                    metadata_input_file_name, stnptr, &write_recs);
               break;

            default:
               printf ("Error: current_data_type is unknown!");
               break;
            } /* switch */

        /*  printf ("Main: Set current_stn_id = new_stnid\n"); */
         strcpy (current_stnid, new_stnid);
         strcpy (current_platform, new_platform);

         /*
          * Increment the internal station id.
          * Used by database only.
          */
          if (write_recs)
             {
            /*  printf ("Increment the current_stnid_ct\n"); */
             current_stnid_ct++;
             }
          else
             {
            /*  printf ("DONT increment the current_stnid_ct - already in new stn list!!!\n"); */
             }

         } /* new_stnid == current_stnid */
      else 
         {
         /*
          * Stn id is same as for prev. rec.
          * Update the end of period of coverage
          * date each time...so will be up to date
          * when write out rec. Only update date if
          * it is greater than existing end date!
          */
         /* printf ("Stns are SAME!!!\n"); */

         if (current_data_type == dpqcf || current_data_type == usgs_sqcf)
            {
            strncpy( junk_str, newline, 4 ); /* yr */
            strncpy( &junk_str[4], &newline[5], 2 ); /* mo */

            /*
             * Because this data is in the output by monthly
             * records, assume that all days for last month
             * are present. For these formats, not currently
             * checking if new date > end_date....but should!!
             */
            /* printf ("set end date for dpqcf/usgs streamflow\n"); */

            strncpy( &junk_str[6], "31", 2 ); /* dy */
 
            if (!strncmp (&newline[5], "02", 2))
               strncpy( &junk_str[6], "29", 2 ); /* dy  - WARNING:set for leap yr of 1992!!*/
            else if ( !strncmp (&newline[5], "04", 2) ||
                      !strncmp (&newline[5], "06", 2) ||
                      !strncmp (&newline[5], "09", 2) ||
                      !strncmp (&newline[5], "11", 2)     )
               strncpy( &junk_str[6], "30", 2 ); 

            strncpy( stnptr->end_date, junk_str, 9);
            }
         else /* qcf and all others */
            {
            strncpy( junk_str, "19", 2 ); /* assume 1900's */
            strncpy( &junk_str[2], newline, 2 ); /* yr */
            strncpy( &junk_str[4], &newline[3], 2 ); /* mo */
            strncpy( &junk_str[6], &newline[6], 2 ); /* dy */
            junk_str[9] = '\0';

/*             if (date1_gt_date2(junk_str, stnptr->end_date) ) */
               strncpy( stnptr->end_date, junk_str, 9);
            }

/*          printf ("Updated end_date: %-sxxx\n", stnptr->end_date);  */

         } /* if new_stnid == current_stnid */
      } /* while input data */

   /*
    * Free ptr and Close all input and output stream.
    */
   free (stnptr);

   if (fclose (data_input_stream) == EOF)
      perror ("Can't close data_input_stream");

   if (fclose (station_output_stream) == EOF)
      perror ("Can't close station_output_stream");

   if (fclose (stn_id_output_stream) == EOF)
      perror ("Can't close stn_id_output_stream");

   printf ("build_stn_info processing complete!\n");
   }  /* main() */
