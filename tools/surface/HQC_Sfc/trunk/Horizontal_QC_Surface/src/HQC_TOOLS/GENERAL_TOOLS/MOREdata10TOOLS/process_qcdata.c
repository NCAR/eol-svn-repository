#ifndef lint
static char *rcsid = "$Id$";
#endif


/*
 * $Log$
 *
 */

/*----------------------------------------------------------
 * process_qcdata.c - This is module contains the functions 
 *     which read the QC data and associated metadata.
 *     All data types which can be procesed are defined 
 *     here. To add another data type, add the type to
 *     the QC_data_types enumerated list defined in the
 *     process_qcdata.h header file, add the fn defs to the
 *     same header file and place the actual fn code
 *     in this file.
 *
 * 09 May 94 lec
 *   Created.
 *---------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>

#include "local.h"
#include "process_qcdata.h"

static STNREC  metadata_stn_info[MAX_ELEMS];


/*----------------------------------------------------------------------
 *read_record() - Generically reads in a record of max length of 
 *    MAX_CHARS. MAX_CHARS is defined in process_qcdata.h.
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


/*--------------------------------------------------------
 * process_spec_qcfrec - This fn fills the input qcf data's
 *  station ptr with the best possible info. This info
 *  may be extracted from the 'old' input stn list or
 *  other metadata. This fn is used to process the NCDC
 *  specials which are qcf format with only the obs time.
 *
 * 20 May 94 lec
 *   Created.
 *-------------------------------------------------------*/
void process_spec_qcfrec( /*in*/     char   new_line[MAX_CHARS],
                          /*in*/     long   num_new_stns,
                          /*in*/     long   num_old_stns,
                          /*in*/     long   current_stnid_ct,
                          /*in*/     char metadata_input_file_name[MAX_CHARS],
                          /*in/out*/ STNREC *stnptr,
                          /*in/out*/ int    *write_recs)
   {
   int  stn_found = 0;
   int  i = 0;
   char junk_str[26] = "\0";
 
   static long  nonlocated_stns = 0;
 
 
   /* printf ("Enter process_spec_qcfrec()\n");
   printf ("new_line: %-sxxx\n", new_line); */
 
   for(i=0;i<25;i++) junk_str[i] = '\0';
 
   /*
    * Extract date/platform/external stnid/lat/lon. This
    * is the beginning time of the period of coverage, since this
    * routine is only called when the first rec for this
    * stn is encountered.
    */
   strncpy( junk_str, "19", 2 ); /* assume 1900's */
   strncpy( &junk_str[2], new_line, 2 ); /* yr 2*/
 
   strncpy( &junk_str[4], &new_line[3], 2 ); /* mo */
   strncpy( &junk_str[6], &new_line[6], 2 ); /* dy */
 
   /* printf ("junk_str date: %-sxxx\n", junk_str);  */
   strncpy( stnptr->begin_date, junk_str, 9);
   strncpy( stnptr->end_date, stnptr->begin_date, 9);
 
   strncpy( stnptr->frequency, "??????\0", 7);
   /* printf ("stnptr - beg, end, freq: %-sxxx %-sxxx %-sxxx\n",
          stnptr->begin_date,  stnptr->end_date, stnptr->frequency);*/
 
   /*
    * Platform names are related to integers via a
    * previously created table. See platform_defs file.
    */
   strncpy( junk_str, &new_line[15], 10 );         /* network */
   junk_str[10] = '\0';
   stnptr->platform = determine_platform(junk_str);
 
   stnptr->stnid_int = current_stnid_ct;           /* internal id */
   strncpy (stnptr->stnid_ext, &new_line[26], 10); /* external id actually 18 chars */
 
   /*printf ("stnptr - platform, stnid_ext: %dxxx %-sxxx\n",
            stnptr->platform, stnptr->stnid_ext); */
 
   strncpy (junk_str, &new_line[37], 10);
   junk_str[11] = '\0';
   /* printf ("junk_str lat: %-sxxx\n", junk_str);*/
   stnptr->lat = atof (junk_str);

   strncpy (junk_str, &new_line[48], 11);
   junk_str[12] = '\0';
   /* printf ("junk_str lon: %-sxxx\n", junk_str); */
   stnptr->lon = atof (junk_str);

   strncpy (junk_str, &new_line[64], 7);
   junk_str[7] = '\0';
   /* printf ("junk_str elev: %-sxxx\n", junk_str); */
   stnptr->elev = atof (junk_str);

  /*printf ("stnptr - lat, lon, elev: %f %f %f\n",
           stnptr->lat, stnptr->lon, stnptr->elev);  */


   /*
    * Now have all info that can be extracted from
    * data.
    *
    * Try and locate this stn in the new
    * master stn list, if found, ensure complete
    * match. If complete match, then add nothing to
    * new master stn list. If not a match, then
    * search the old master stn list. If not even
    * in the old list, then search other metadata
    * and write out new stn rec.
    */
   if( locate_stn_in_new_master_list (stnptr->stnid_ext, num_new_stns, stnptr))
      {
      printf ("Stn located in NEW master stn list! Don't write out new rec! %-s\n", stnptr->stnid_ext);
      *write_recs = 0;
      }
   else if( locate_stn_in_old_master_list (stnptr->stnid_ext, num_old_stns, stnptr))
      {
      printf ("Located stn in old master stn list! Write out new rec! %-s\n", stnptr->stnid_ext);
      *write_recs = 1;
      }
   else
      {
      /*
       * Gather extra info from metadata, if possible. This data didn't
       * really have much metadata for GCIP. If stn not found set name
       * to external ID. Any unidentified stns should be manually updated.
       */
      nonlocated_stns++;
      printf ("Stn  %-s NOT located! Gather more info from METADATA! Total unlocated stns: %ld\n",
               stnptr->stnid_ext, nonlocated_stns);

      strncpy (stnptr->name, stnptr->stnid_ext, 10); /* copy external id into name */
      *write_recs = 1;
      }

/*    printf ("Exit process_spec_qcfrec\n"); */
   } /* process_spec_qcfrec() */


/*--------------------------------------------------------
 * process_qcfrec - This fn fills the input qcf data's
 *  station ptr with the best possible info. This info
 *  may be extracted from the 'old' input stn list or
 *  other metadata.
 *
 * 09 May 94 lec
 *   Created. 
 *-------------------------------------------------------*/
void process_qcfrec( /*in*/     char   new_line[MAX_CHARS],
                     /*in*/     long   num_new_stns,
                     /*in*/     long   num_old_stns,
                     /*in*/     long   current_stnid_ct,
                     /*in*/     char metadata_input_file_name[MAX_CHARS],
                     /*in/out*/ STNREC *stnptr,
                     /*in/out*/ int    *write_recs)
   {
   int  stn_found = 0;
   int  i = 0;
   char junk_str[26] = "\0";

   static long  nonlocated_stns = 0;


  /*  printf ("Enter process_qcfrec()\n");
   printf ("new_line: %-sxxx\n", new_line); */

   for(i=0;i<25;i++) junk_str[i] = '\0';

   /*
    * Extract date/platform/external stnid/lat/lon. This
    * is the beginning time of the period of coverage, since this
    * routine is only called when the first rec for this
    * stn is encountered. 
    */
   strncpy( junk_str, "19", 2 ); /* assume 1900's */
   strncpy( &junk_str[2], new_line, 2 ); /* yr 2*/

   strncpy( &junk_str[4], &new_line[3], 2 ); /* mo */
   strncpy( &junk_str[6], &new_line[6], 2 ); /* dy */

   /* printf ("junk_str date: %-sxxx\n", junk_str);  */
   strncpy( stnptr->begin_date, junk_str, 9);
   strncpy( stnptr->end_date, stnptr->begin_date, 9);

   strncpy( stnptr->frequency, "??????\0", 7);
  /*  printf ("stnptr - beg, end, freq: %-sxxx %-sxxx %-sxxx\n",
          stnptr->begin_date,  stnptr->end_date, stnptr->frequency);  */

   /*
    * Platform names are related to integers via a
    * previously created table. See platform_defs file.
    */
   strncpy( junk_str, &new_line[30], 10 );         /* network */
   junk_str[10] = '\0';
   stnptr->platform = determine_platform(junk_str);

   stnptr->stnid_int = current_stnid_ct;           /* internal id */
   strncpy (stnptr->stnid_ext, &new_line[41], 10); /* external id  actually 18 chars*/

  /*  printf ("stnptr - platform, stnid_ext: %dxxx %-sxxx\n",
            stnptr->platform, stnptr->stnid_ext); */
 
   strncpy (junk_str, &new_line[52], 10);
   junk_str[11] = '\0';
   /* printf ("junk_str lat: %-sxxx\n", junk_str);*/
   stnptr->lat = atof (junk_str);
    
   strncpy (junk_str, &new_line[63], 11);
   junk_str[12] = '\0';
   /* printf ("junk_str lon: %-sxxx\n", junk_str); */
   stnptr->lon = atof (junk_str);
    
   strncpy (junk_str, &new_line[79], 7);
   junk_str[7] = '\0';
   /* printf ("junk_str elev: %-sxxx\n", junk_str); */
   stnptr->elev = atof (junk_str);

  /* printf ("stnptr - lat, lon: %f %f %f\n",
           stnptr->lat, stnptr->lon, stnptr->elev);  */

 
   /*
    * Now have all info that can be extracted from
    * data. 
    * 
    * Try and locate this stn in the new
    * master stn list, if found, ensure complete
    * match. If complete match, then add nothing to
    * new master stn list. If not a match, then 
    * search the old master stn list. If not even
    * in the old list, then search other metadata
    * and write out new stn rec. 
    */
   if( locate_stn_in_new_master_list (stnptr->stnid_ext, num_new_stns, stnptr))
      {
      printf ("Stn located in NEW master stn list! Don't write out new rec! %-s\n", stnptr->stnid_ext); 
      *write_recs = 0;
      }
   else if( locate_stn_in_old_master_list (stnptr->stnid_ext, num_old_stns, stnptr))
      {
      printf ("Located stn in old master stn list! Write out new rec! %-s\n", stnptr->stnid_ext);
      *write_recs = 1;
      }
   else
      {
      /*
       * Gather extra info from metadata, if possible. This data didn't
       * really have much metadata for GCIP. If stn not found set name
       * to external ID. Any unidentified stns should be manually updated.
       */
      nonlocated_stns++;
      printf ("Stn  %-s NOT located! Gather more info from METADATA! Total unlocated stns: %ld\n",
               stnptr->stnid_ext, nonlocated_stns);

      strncpy (stnptr->name, stnptr->stnid_ext, 10); /* copy external id into name */
      *write_recs = 1;
      }   

/*    printf ("Exit process_qcfrec\n"); */
   } /* process_qcfrec() */


/*--------------------------------------------------------
 * process_qcf_metadata - 
 *
 * 09 May 94 lec
 *   Created.
 *-------------------------------------------------------*/
void process_qcf_metadata()
   {
   fprintf (stderr, "Called process_qcf_metadata\n");
   }

/*--------------------------------------------------------
 * process_pqcfrec - This fn gathers the metadata from 
 *  several sources for pqc hourly formatted files.
 *
 * 09 May 94 lec
 *   Created.
 *-------------------------------------------------------*/
void process_pqcfrec( /*in*/     char   new_line[MAX_CHARS],
                      /*in*/     long   num_stns,
                      /*in*/     long   current_stnid_ct,
                      /*in*/     char metadata_input_file_name[MAX_CHARS],
                      /*in/out*/ STNREC *stnptr)
   {
   int  stn_found = 0;
   int  i = 0;
   char junk_str[26] = "\0";

   static long  nonlocated_stns = 0;


   /* printf ("Enter process_pqcfrec()\n");
   printf ("new_line: %-sxxx\n", new_line); */

   for(i=0;i<25;i++) junk_str[i] = '\0';

   /*
    * Extract date/platform/external stnid/lat/lon. This 
    * is the beginning time of the period of coverage, since this
    * routine is only called when the first rec for this
    * stn is encountered. NOTE: the hrly precip format
    * does NOT contain the elevation!
    */
   strncpy( junk_str, "19", 2 ); /* assume 1900's */
   strncpy( &junk_str[2], new_line, 2 ); /* yr 2*/

   strncpy( &junk_str[4], &new_line[3], 2 ); /* mo */
   strncpy( &junk_str[6], &new_line[6], 2 ); /* dy */

   /* printf ("junk_str date: %-sxxx\n", junk_str);  */
   strncpy( stnptr->begin_date, junk_str, 9);
   strncpy( stnptr->end_date, stnptr->begin_date, 9);

   strncpy( stnptr->frequency, "hourly\0", 7); 
   /* printf ("stnptr - beg, end, freq: %-sxxx %-sxxx %-sxxx\n", 
          stnptr->begin_date,  stnptr->end_date, stnptr->frequency);  */

   /*
    * Platform names are related to integers via a 
    * previously created table. See platform_defs file.
    * This data is 'Rec rainga'. In FEST this data was
    * tagged as 'COOP'. These NWS recording raingauges
    * are associated with a platform number of 46.
    *
    * If 'USGS' then these are the raingauges of platform
    * number 74. Platform table doesn't differentiate 
    * between USGS streamflow and precip gauges. Must
    * do manually.
    */
   strncpy( junk_str, &new_line[18], 10 );         /* network */
   junk_str[10] = '\0';
   stnptr->platform = determine_platform(junk_str);

   if (!strncmp (junk_str, "USGS", 4))
      stnptr->platform = 74;
 
   stnptr->stnid_int = current_stnid_ct;           /* internal id */
   strncpy (stnptr->stnid_ext, &new_line[29], 18); /* external id actually 18 chars*/

   printf ("stnptr - platform, stnid_ext: %dxxx %-sxxx\n",
            stnptr->platform, stnptr->stnid_ext);

   strncpy (junk_str, &new_line[40], 10);
   junk_str[11] = '\0';
   /* printf ("junk_str lat: %-sxxx\n", junk_str);*/
   stnptr->lat = atof (junk_str);

   strncpy (junk_str, &new_line[51], 11);
   junk_str[12] = '\0';
   /* printf ("junk_str lon: %-sxxx\n", junk_str); */
   stnptr->lon = atof (junk_str);

   /* printf ("stnptr - lat, lon: %fxxx %fxxx\n",
           stnptr->lat, stnptr->lon);  */

   /*
    * Now have all info that can be extracted from 
    * hourly precip data. This data has NO elev!
    * So can only search on lat/lon/platform.
    * 
    * Try and locate this stn in the existing
    * master stn list. For GIDS-1, search the
    * fest_stn_list, since we have data from
    * STORMFEST. Special rules apply to this
    * GIDS-1 processing, such as must retain
    * occurance values from FEST stn list.
    *
    * If stn is located in master stn list,
    * then the stnptr is filled with this
    * `old' data...which should be complete.
    */
   if( locate_stn_in_old_master_list (stnptr->stnid_ext, num_stns, stnptr))
      {
      /* printf ("Stn located! Using info from old master stn list!\n"); */
      }
   else
      {
      /*
       * Gather extra info from metadata, if possible. This data didn't 
       * really have much metadata for GCIP. If stn not found set name
       * to external ID. Any unidentified stns should be manually updated.
       */
      nonlocated_stns++;
      printf ("Stn not located! Gather more info from metadata! Total unlocated stns: %ld\n",
               nonlocated_stns);

      strncpy (stnptr->name, stnptr->stnid_ext, 18); /* copy external id into name */

      if (stnptr->platform == 16) /* HPLAINS */
         {
         stnptr->accuracy = 2;
         strncpy( stnptr->frequency, "hourly\0", 7 );
         }
      }

/*    printf ("Exit process_pqcfrec\n"); */
   } /* process_pqcfrec() */

/*--------------------------------------------------------
 * process_pqcf_metadata - Currently there is NO pqcf
 *   metadata.
 *
 * 09 May 94 lec
 *   Created.
 *-------------------------------------------------------*/
void process_pqcf_metadata()
   {
   fprintf (stderr, "Called process_pqcf_metadata - No metadata available\n");
   }


/*--------------------------------------------------------
 * process_dpqcfrec - Processes the Daily precip composite.
 *
 * 09 May 94 lec
 *   Created.
 *-------------------------------------------------------*/
void process_dpqcfrec( /*in*/     char   new_line[MAX_CHARS],
                       /*in*/     long   num_new_stns,
                       /*in*/     long   num_old_stns,
                       /*in*/     long   num_meta_stns,
                       /*in*/     long   current_stnid_ct,
                       /*in*/     char metadata_input_file_name[MAX_CHARS],
                       /*in/out*/ STNREC *stnptr,
                       /*in/out*/ int    *write_recs)
   {
   int  stn_found = 0;
   int  i = 0;
   char junk_str[26] = "\0";
 
   static long  nonlocated_stns = 0;
 
 
   /*printf ("Enter process_dpqcfrec()\n");
   printf ("new_line: %-sxxx\n", new_line);  */
 
   for(i=0;i<25;i++) junk_str[i] = '\0';
 
   /*
    * Extract date/platform/external stnid/lat/lon. This
    * is the beginning time of the period of coverage, since this
    * routine is only called when the first rec for this
    * stn is encountered.
    */
   strncpy( junk_str, new_line, 4 ); /* yr */
    
   strncpy( &junk_str[4], &new_line[5], 2 ); /* mo */

   /*
    * No day in data. Assume first of current month.
    */
   strncpy( &junk_str[6], "01", 2 ); /* dy */
   /* printf ("junk_str date: %-sxxx\n", junk_str);  */

   strncpy( stnptr->begin_date, junk_str, 9);

   /*
    * For this data, the records are monthly so set
    * end date to at least end of month for first
    * record.
    */
   strncpy( &junk_str[6], "31", 2 ); /* dy */

   if (!strncmp (&new_line[5], "02", 2))
      strncpy( &junk_str[6], "29", 2 ); /* dy  - WARNING:set for leap yr of 1992!!*/
   else if ( !strncmp (&new_line[5], "04", 2) ||
             !strncmp (&new_line[5], "06", 2) ||
             !strncmp (&new_line[5], "09", 2) ||
             !strncmp (&new_line[5], "11", 2)     )
      strncpy( &junk_str[6], "30", 2 );

   strncpy( stnptr->end_date, junk_str, 9);

    
   strncpy( stnptr->frequency, "??????\0", 7);
   /*printf ("stnptr - beg, end, freq: %-sxxx %-sxxx %-sxxx\n",
          stnptr->begin_date,  stnptr->end_date, stnptr->frequency);  */
 

   /*
    * Platform names are related to integers via a
    * previously created table. See platform_defs file.
    */
   strncpy( junk_str, &new_line[8], 10 );         /* network */
   junk_str[10] = '\0';
   stnptr->platform = determine_platform(junk_str);
    
   stnptr->stnid_int = current_stnid_ct;           /* internal id */
   strncpy (stnptr->stnid_ext, &new_line[18], 15); /* external id actually 18 chars allowed */
    
   /*printf ("stnptr - platform, stnid_ext: %dxxx %-sxxx\n", 
            stnptr->platform, stnptr->stnid_ext); */
 
   strncpy (junk_str, &new_line[34], 10);
   junk_str[11] = '\0';
   /* printf ("junk_str lat: %-sxxx\n", junk_str);*/
   stnptr->lat = atof (junk_str);
    
   strncpy (junk_str, &new_line[45], 11);
   junk_str[12] = '\0';
   /* printf ("junk_str lon: %-sxxx\n", junk_str); */
   stnptr->lon = atof (junk_str);
    
   stnptr->accuracy = 2;
   stnptr->elev = -9999.9;
    
   /*printf ("stnptr - lat, lon: %f %f %f\n",
           stnptr->lat, stnptr->lon, stnptr->elev);  */
 
 
   /*
    * Try and locate this stn in the new
    * master stn list, if found, ensure complete
    * match. If complete match, then add nothing to
    * new master stn list. If not a match, then
    * search the old master stn list. If not even
    * in the old list, then search other metadata
    * and write out new stn rec.
    */
   if( locate_stn_in_new_master_list (stnptr->stnid_ext, num_new_stns, stnptr))
      {
      printf ("Stn located in NEW master stn list! Don't write out new rec! %-s\n", stnptr->stnid_ext);
      *write_recs = 0;
      }
   else if( locate_stn_in_old_master_list (stnptr->stnid_ext, num_old_stns, stnptr))
      {
      printf ("Located stn in old master stn list! Write out new rec! %-s\n", stnptr->stnid_ext);
      *write_recs = 1;
      }
   else if( locate_stn_in_metadata_by_ID (stnptr->stnid_ext, num_meta_stns, stnptr))
      {
      printf ("Located stn in metadata! Write out new rec! %-s\n", stnptr->stnid_ext);
      *write_recs = 1;
      }
   else
      {
      /*
       * Gather extra info from metadata, if possible. This data didn't
       * really have much metadata for GCIP. If stn not found set name
       * to external ID. Any unidentified stns should be manually updated.
       */
      nonlocated_stns++;
      printf ("Stn  %-s NOT located! Gather more info from METADATA! Total unlocated stns: %ld\n",
               stnptr->stnid_ext, nonlocated_stns);
 
      strncpy (stnptr->name, stnptr->stnid_ext, 18); /* copy external id into name */
      *write_recs = 1;
      }
 
/*    printf ("Exit process_dpqcfrec\n"); */
   } /* process_dpqcfrec() */


/*--------------------------------------------------------
 * process_dpqcf_metadata - this fn loads the DPQCF metadata.
 *
 * 09 May 94 lec
 *   Created.
 *-------------------------------------------------------*/
void process_dpqcf_metadata( /*in*/     char metadata_input_file_name[MAX_CHARS],
                             /*in/out*/ long *num_meta_stns)
   {
   FILE     *input_stream;

   long int i = -1;
   long int j = -1;
   int      kk = 0;

   char     newline[MAX_CHARS];
   char     junk_str[75];
   char     junk_char;

   fprintf (stderr, "Called process_dpqcf_metadata. meta file: %-sxxx\n", metadata_input_file_name);
   *num_meta_stns = 0;

   /*
    * Load usgs sqcf metadata into metadata array.
    */
   if (( input_stream = fopen(metadata_input_file_name, "r")) == NULL)
      perror ("Error: Can't open input_file");
 
   while (!feof(input_stream))
      {
      /*
       * Reinitialize the strings.
       */
      i++;
      for (j=0;j<75;j++)junk_str[j] = '\0';
      for (j=0;j<MAX_CHARS;j++)newline[j] = '\0';
      for (j=0;j<16;j++)metadata_stn_info[i].project[j] = '\0';
      for (j=0;j<19;j++)metadata_stn_info[i].stnid_ext[j] = '\0';
      for (j=0;j<51;j++)metadata_stn_info[i].name[j] = '\0';
      for (j=0;j<9;j++)metadata_stn_info[i].begin_date[j] = '\0';
      for (j=0;j<9;j++)metadata_stn_info[i].end_date[j] = '\0';
      for (j=0;j<3;j++)metadata_stn_info[i].country[j] = '\0';
      for (j=0;j<3;j++)metadata_stn_info[i].state[j] = '\0';
      for (j=0;j<4;j++)metadata_stn_info[i].county[j] = '\0';
      for (j=0;j<15;j++)metadata_stn_info[i].frequency[j] = '\0';

      read_record (&input_stream, newline);
     /*  printf ("newline:%-sxxx\n", newline);*/

      if( feof (input_stream))
         break;

      strncpy (junk_str, newline,16);
      for (kk=0;kk<15;kk++)
         {
         if (junk_str[kk] != ' ')
            metadata_stn_info[i].stnid_ext[kk] = junk_str[kk];
         else
            break;
         }
      metadata_stn_info[i].stnid_ext[kk] == '\0';

      for (j=0;j<75;j++)junk_str[j] = '\0';
      strncpy (junk_str, &newline[15],10);
      metadata_stn_info[i].lat = atof (junk_str);

      for (j=0;j<75;j++)junk_str[j] = '\0';
      strncpy (junk_str, &newline[26],10);
      metadata_stn_info[i].lon = atof (junk_str);

      metadata_stn_info[i].occur = 0;
      metadata_stn_info[i].accuracy = 2;

      strncpy (metadata_stn_info[i].name, &newline[53],50);
      strncpy (metadata_stn_info[i].country, "US",2);
      strncpy (metadata_stn_info[i].state, &newline[46],2);
      strncpy (metadata_stn_info[i].county, "???", 3); /* counties unknown */
 
      junk_char = newline[38]; /* time_zone flag */

      if (junk_char == 'M')
         metadata_stn_info[i].time_zone = 7.00;
      else if (junk_char == 'C')
         metadata_stn_info[i].time_zone = 6.00;
      else if (junk_char == 'E')
         metadata_stn_info[i].time_zone = 5.00;
      else
         printf ("ERROR: dpqcf metadata - unknown times_zone\n");

      junk_char = newline[41]; /* DST switch flag */

      if (junk_char == 'D')
         metadata_stn_info[i].dst_switch ='y';
      else
         metadata_stn_info[i].dst_switch ='n';

      /*
       * This metadata contains platform number.
       */
      strncpy (junk_str, &newline[43], 2);
      junk_str[2] = '\0';

      metadata_stn_info[i].platform = atoi (junk_str);

      strncpy (metadata_stn_info[i].frequency, "daily",5);

      metadata_stn_info[i].elev = -9999.9;
      metadata_stn_info[i].fixed_mobile =  'f';

#if 0
      printf ("metadata_stn_info[%d].project: %-sxxx::\n", i, metadata_stn_info[i].project);
      printf ("metadata_stn_info[%d].stnid_int: %dxxx::\n", i, metadata_stn_info[i].stnid_int);
      printf ("metadata_stn_info[%d].lat: %fxxx::\n", i, metadata_stn_info[i].lat);
      printf ("metadata_stn_info[%d].lon: %fxxx::\n", i, metadata_stn_info[i].lon);
      printf ("metadata_stn_info[%d].occur: %dxxx::\n", i, metadata_stn_info[i].occur);
      printf ("metadata_stn_info[%d].accuracy: %dxx::\n", i, metadata_stn_info[i].accuracy);
      printf ("metadata_stn_info[%d].name: %-sxxx::\n", i, metadata_stn_info[i].name);
      printf ("metadata_stn_info[%d].begin_date: %-sxxx::\n", i, metadata_stn_info[i].begin_date);
      printf ("metadata_stn_info[%d].end_date: %-sxxx::\n", i, metadata_stn_info[i].end_date);
      printf ("metadata_stn_info[%d].country: %-sxxx::\n", i, metadata_stn_info[i].country);
      printf ("metadata_stn_info[%d].state: %-sxxx::\n", i, metadata_stn_info[i].state);
      printf ("metadata_stn_info[%d].county: %-sxxx::\n", i, metadata_stn_info[i].county);
      printf ("metadata_stn_info[%d].time_zone: %fxxx::\n", i, metadata_stn_info[i].time_zone);
      printf ("metadata_stn_info[%d].dst_switch: %cxxx::\n", i, metadata_stn_info[i].dst_switch);
      printf ("metadata_stn_info[%d].platform: %dxx::\n", i, metadata_stn_info[i].platform);
      printf ("metadata_stn_info[%d].frequency: %-sxxx::\n", i, metadata_stn_info[i].frequency);
      printf ("metadata_stn_info[%d].elev: %fxxx::\n", i, metadata_stn_info[i].elev);
      printf ("metadata_stn_info[%d].fixed_mobile: %cxxx::\n", i, metadata_stn_info[i].fixed_mobile);

#endif
 
      } /* while */
 
   *num_meta_stns = i;
 
   printf ("Num_stns in metadata_stn_info file: %ld\n", *num_meta_stns);
 
   if (fclose (input_stream) == EOF)
      perror ("Error: Can't close input_file");
 
   } /* process_dpqcf_metadata() */


/*--------------------------------------------------------
 * process_usgs_sqcfrec -  This fn sets the stn info for 
 *  the usgs streamflow data. Assumes none of these
 *  stns is in existing stn list (for GIDS-1, that is 
 *  fest_stn_list).  For GIDS-1, all data had to be 
 *  gathered from metadata. In future, this would need
 *  to be updated to search existing stn list.
 * 
 * 09 May 94 lec
 *   Created.
 *-------------------------------------------------------*/
void process_usgs_sqcfrec( /*in*/     char   new_line[MAX_CHARS],
                           /*in*/     long   num_stns,
                           /*in*/     long   num_meta_stns,
                           /*in*/     long   current_stnid_ct,
                           /*in*/     char   metadata_input_file_name[MAX_CHARS], 
                           /*in/out*/ STNREC *stnptr)
   {
   int  stn_found = 0;
   int  i, kk = 0;
   char junk_str[26] = "\0";

   static long  nonlocated_stns = 0;


   fprintf (stderr, "Called process_usgs_sqcfrec\n");
   /* printf ("new_line: %-sxxx\n", new_line); */

   for(i=0;i<25;i++) junk_str[i] = '\0';

   /*
    * Extract date/platform/external stnid/lat/lon. This
    * is the beginning time of the period of coverage, since this
    * routine is only called when the first rec for this
    * stn is encountered.
    */
   strncpy( junk_str, new_line, 4 );         /* yr */
   strncpy( &junk_str[4], &new_line[5], 2 ); /* mo */
   strncpy( &junk_str[6], "01", 2 );         /* dy - recs are monthly. */

   strncpy( stnptr->begin_date, junk_str, 9);
   strncpy( stnptr->end_date, stnptr->begin_date, 9);
 
   strncpy( stnptr->frequency, "daily\0", 5);

   /* printf ("beg,end dates:%-sxxx %-sxxx\n", stnptr->begin_date,
            stnptr->end_date); */

   /*
    * Platform names are related to integers via a
    * previously created table. See platform_defs file.
    */
   stnptr->platform = 75;

   stnptr->stnid_int = current_stnid_ct;           /* internal id */

   strncpy (junk_str, &new_line[18],10);            /* external id */
   for (kk=0;kk<10;kk++)
      {
      if (junk_str[kk] != ' ')
         stnptr->stnid_ext[kk] = junk_str[kk];
      else
         break;
      }
   stnptr->stnid_ext[kk] == '\0';

   stnptr->accuracy = 2;

   /*
    * For usgs streamflow, the lat, lon, elev are NOT
    * in the data. They are only stored in the metadata.
    * Extract this info and more metadata. Since
    * this s/w is currently tailored to GIDS-1, we know
    * that there is no need to search the existing
    * stn info file for these stns...they aren't there.
    * In the future, this s/w should search the existing
    * list and only add in the new stations. Because of
    * this future need, the call to this fn includes parameters
    * which are not currently used, but will be used in
    * the future.
    *
    * FUTURE: For each new stn, locate the lat,lon, etc. in
    * metadata. Search for stn in existing list using lat/lon.
    * if not in existing list, write out new record using
    * metadata info, else use existing list's info.
    */
   if( locate_stn_in_metadata_by_ID (stnptr->stnid_ext, num_meta_stns, stnptr))
      {
      printf ("Stn located! Using info in metadata!\n");
      }
   else
      {
      nonlocated_stns++;
      printf ("Stn not located! Total unlocated stns: %ld\n", nonlocated_stns);
      strncpy (stnptr->name, stnptr->stnid_ext, 10); /* copy external id into name */
      }
     
   /* printf ("Exit process_usgs_sqcfrec\n"); */
   }

/*--------------------------------------------------------
 * process_usgs_sqcf_metadata - This s/w fills the 
 *  metadata array so that it can be search for each stn.
 *
 * 09 May 94 lec
 *   Created.
 *-------------------------------------------------------*/
void process_usgs_sqcf_metadata( /*in*/     char metadata_input_file_name[MAX_CHARS],
                                 /*in/out*/ long *num_meta_stns)
   {
   FILE     *input_stream;

   long int i = -1;
   long int j = -1;
   int      kk = 0;

   char     newline[MAX_CHARS];
   char     junk_str[75];
   char     junk_stn;

   fprintf (stderr, "Called process_usgs_sqcfmetadata. meta file: %-sxxx\n", metadata_input_file_name);
   *num_meta_stns = 0;

   /*
    * Load usgs sqcf metadata into metadata array.
    */
   if (( input_stream = fopen(metadata_input_file_name, "r")) == NULL)
      perror ("Error: Can't open input_file");

   while (!feof(input_stream))
      {
      /*
       * Reinitialize the strings.
       */
      i++;
      for (j=0;j<75;j++)junk_str[j] = '\0';
      for (j=0;j<MAX_CHARS;j++)newline[j] = '\0';
      for (j=0;j<16;j++)metadata_stn_info[i].project[j] = '\0';
      for (j=0;j<19;j++)metadata_stn_info[i].stnid_ext[j] = '\0';
      for (j=0;j<51;j++)metadata_stn_info[i].name[j] = '\0';
      for (j=0;j<9;j++)metadata_stn_info[i].begin_date[j] = '\0';
      for (j=0;j<9;j++)metadata_stn_info[i].end_date[j] = '\0';
      for (j=0;j<3;j++)metadata_stn_info[i].country[j] = '\0';
      for (j=0;j<3;j++)metadata_stn_info[i].state[j] = '\0';
      for (j=0;j<4;j++)metadata_stn_info[i].county[j] = '\0';
      for (j=0;j<15;j++)metadata_stn_info[i].frequency[j] = '\0';
 
      read_record (&input_stream, newline);
     /*  printf ("newline:%-sxxx\n", newline);*/
 
      if( feof (input_stream))
         break;

      strncpy (junk_str, newline,10);
      for (kk=0;kk<10;kk++)
         {
         if (junk_str[kk] != ' ')
            metadata_stn_info[i].stnid_ext[kk] = junk_str[kk];
         else
            break;
         }
      metadata_stn_info[i].stnid_ext[kk] == '\0';

      for (j=0;j<75;j++)junk_str[j] = '\0';
      strncpy (junk_str, &newline[12],10);
      metadata_stn_info[i].lat = atof (junk_str);

      for (j=0;j<75;j++)junk_str[j] = '\0';
      strncpy (junk_str, &newline[23],10);
      metadata_stn_info[i].lon = atof (junk_str);

      metadata_stn_info[i].occur = 0;
      metadata_stn_info[i].accuracy = 2;
 
      strncpy (metadata_stn_info[i].name, &newline[47],50);
      strncpy (metadata_stn_info[i].country, "US",2);
      strncpy (metadata_stn_info[i].state, &newline[40],2);
      strncpy (metadata_stn_info[i].county, "???", 3); /* counties unknown */
 
      junk_stn = newline[35]; /* time_zone flag */

      if (junk_stn == 'M')
         metadata_stn_info[i].time_zone = 7.00;
      else if (junk_stn == 'C')
         metadata_stn_info[i].time_zone = 6.00;
      else if (junk_stn == 'E') 
         metadata_stn_info[i].time_zone = 5.00;
      else
         printf ("ERROR: usgs streamflow metadata - unknown times_zone\n");
 
      junk_stn = newline[38]; /* DST switch flag */

      if (junk_stn == 'D')
         metadata_stn_info[i].dst_switch ='y';
      else
         metadata_stn_info[i].dst_switch ='n';

      metadata_stn_info[i].platform = 75;
 
      strncpy (metadata_stn_info[i].frequency, "daily",5);
 
      metadata_stn_info[i].elev = -9999.9;
      metadata_stn_info[i].fixed_mobile =  'f';

#if 0
      printf ("metadata_stn_info[%d].project: %-sxxx::\n", i, metadata_stn_info[i].project);
      printf ("metadata_stn_info[%d].stnid_int: %dxxx::\n", i, metadata_stn_info[i].stnid_int);
      printf ("metadata_stn_info[%d].lat: %fxxx::\n", i, metadata_stn_info[i].lat);
      printf ("metadata_stn_info[%d].lon: %fxxx::\n", i, metadata_stn_info[i].lon);
      printf ("metadata_stn_info[%d].occur: %dxxx::\n", i, metadata_stn_info[i].occur);
      printf ("metadata_stn_info[%d].accuracy: %dxx::\n", i, metadata_stn_info[i].accuracy);
      printf ("metadata_stn_info[%d].name: %-sxxx::\n", i, metadata_stn_info[i].name);
      printf ("metadata_stn_info[%d].begin_date: %-sxxx::\n", i, metadata_stn_info[i].begin_date);
      printf ("metadata_stn_info[%d].end_date: %-sxxx::\n", i, metadata_stn_info[i].end_date);
      printf ("metadata_stn_info[%d].country: %-sxxx::\n", i, metadata_stn_info[i].country);
      printf ("metadata_stn_info[%d].state: %-sxxx::\n", i, metadata_stn_info[i].state);
      printf ("metadata_stn_info[%d].county: %-sxxx::\n", i, metadata_stn_info[i].county);
      printf ("metadata_stn_info[%d].time_zone: %fxxx::\n", i, metadata_stn_info[i].time_zone);
      printf ("metadata_stn_info[%d].dst_switch: %cxxx::\n", i, metadata_stn_info[i].dst_switch);
      printf ("metadata_stn_info[%d].platform: %dxx::\n", i, metadata_stn_info[i].platform);
      printf ("metadata_stn_info[%d].frequency: %-sxxx::\n", i, metadata_stn_info[i].frequency);
      printf ("metadata_stn_info[%d].elev: %fxxx::\n", i, metadata_stn_info[i].elev);
      printf ("metadata_stn_info[%d].fixed_mobile: %cxxx::\n", i, metadata_stn_info[i].fixed_mobile);

#endif

      } /* while */
 
   *num_meta_stns = i;
 
   printf ("Num_stns in metadata_stn_info file: %ld\n", *num_meta_stns);
 
   if (fclose (input_stream) == EOF)
      perror ("Error: Can't close input_file");

   } /* process_usgs_sqcf_metadata() */


/*--------------------------------------------------------
 * process_tva_sqcfrec - This fn sets the stn info for
 *  tva streamflow stns. For GIDS-1, There were only
 *  14 stns and none were listed in old stn list named
 *  fest_stn_list. All info had to come from gathered
 *  metadata.
 * 
 * 09 May 94 lec
 *   Created. This fn has not been tested. It was easier
 *   just to take the orig 14 stns and put into specified
 *   formats.
 *-------------------------------------------------------*/
void process_tva_sqcfrec( /*in*/     char   new_line[MAX_CHARS],
                          /*in*/     long   num_stns,
                          /*in*/     long   current_stnid_ct,
                          /*in*/     char metadata_input_file_name[MAX_CHARS], 
                          /*in/out*/ STNREC *stnptr)
   {
   int  stn_found = 0;
   int  i = 0;
   char junk_str[26] = "\0";
   long  num_meta_stns = 0;

   static long  nonlocated_stns = 0;


   fprintf (stderr, "Called process_tva_sqcfrec\n");
   /* printf ("new_line: %-sxxx\n", new_line); */

   for(i=0;i<25;i++) junk_str[i] = '\0';

   /*
    * Extract date/platform/external stnid/lat/lon. This
    * is the beginning time of the period of coverage, since this
    * routine is only called when the first rec for this
    * stn is encountered. 
    */
   strncpy( junk_str, "19", 2 );         /* assume 1900's */
   strncpy( &junk_str[2], new_line, 2 ); /* yr 2*/

   strncpy( &junk_str[4], &new_line[3], 2 ); /* mo */
   strncpy( &junk_str[6], &new_line[6], 2 ); /* dy */

   strncpy( stnptr->begin_date, junk_str, 9);
   strncpy( stnptr->end_date, stnptr->begin_date, 9);

   strncpy( stnptr->frequency, "2 hourly\0", 8);

   /*
    * Platform names are related to integers via a
    * previously created table. See platform_defs file.
    */
   stnptr->platform = 94;
 
   stnptr->stnid_int = current_stnid_ct;           /* internal id */
   strncpy (stnptr->stnid_ext, &new_line[48], 10); /* external id */
    
   stnptr->accuracy = 2;

   /*
    * For tva streamflow, the lat, lon, elev are NOT
    * in the data. They are only stored in the metadata.
    * Extract this info and more from metadata. Since
    * this s/w is currently tailored to GIDS-1, we know
    * that there is no need to search the existing
    * stn info file for these stns...they aren't there.
    * In the future, this s/w should search the existing
    * list and only add in the new stations. Because of
    * this future need, the call to this fn includes parameters
    * which are not currently used, but will be used in
    * the future.
    *
    * FUTURE: For each new stn, locate the lat,lon, etc. in
    * metadata. Search for stn in existing list using lat/lon.
    * if not in existing list, write out new record using
    * metadata info, else use existing list's info.
    */
   if( locate_stn_in_metadata_by_ID (stnptr->stnid_ext, num_meta_stns, stnptr))
      {
      printf ("Stn located! Using info in metadata!\n");
      }
   else
      {
      nonlocated_stns++;
      printf ("Stn not located! Total unlocated stns: %ld\n", nonlocated_stns);
      strncpy (stnptr->name, stnptr->stnid_ext, 10); /* copy external id into name */
      }
         
   /* printf ("Exit process_tva_sqcfrec\n"); */
   } /* process_tva_sqcfrec() */

/*--------------------------------------------------------
 * process_tva_sqcf_metadata - This fn reads in tva sqcf 
 *    metadata.
 * 
 * 09 May 94 lec
 *   Created.
 *-------------------------------------------------------*/
void process_tva_sqcf_metadata(/*in*/     char metadata_input_file_name[MAX_CHARS],
                               /*in/out*/ long *num_meta_stns)
   {
   /*
    * This fn is called just once upon entry to build stn info
    * and should read in metadata into an array which can be
    * searched.
    */
   fprintf (stderr, "Called process_tva_sqcf_metadata - Do Nothing!\n");
   }

/*--------------------------------------------------------
 * locate_stn_in_metadata_by_ID - return 1 if found in metadata
 *  and stnptr updated with data, else return 0. This
 *  fn is identical to locate_stn_in_master_list, except
 *  that it searches the meta data NOT the existing
 *  stn list. Assumes data sorted by stnid (external).
 *
 * 10 May 94 lec
 *   Created.
 *-------------------------------------------------------*/
int locate_stn_in_metadata_by_ID (/*in*/char stnid_ext[19],
                                  /*in*/int num_meta_stns,
                                  /*in/out*/ STNREC  *stnptr)
   {
   double    input_stnid;    /* originally long int. Now have ID's of 15 chars. */
   double    tbl_stnid;

   int       min,  max, j;
   int       located_stn = 0;
 
 
   input_stnid = atof (stnid_ext);

   /*
    * Use binary search for stn id.
    */
   located_stn = 0;
 
   for (min=0, max=num_meta_stns, j=num_meta_stns/2; min<=max; j=min+(max-min)/2 )
      {
      tbl_stnid =  atof(metadata_stn_info[j].stnid_ext);
 
/* was:       if (inrange(input_stnid, tbl_stnid, 0))  - probs with 15 chars*/
       if (input_stnid == tbl_stnid) 
         {
         /* printf ("located stnid!\n"); */
         located_stn = 1;
 
         break;
         }
      else if (tbl_stnid > input_stnid)
         max = j - 1;
      else min = j + 1;
      }


   /*
    * If stn has been located in existing stn list file, we let
    * this data override data read in from file...except for
    * begin and end times.
    */
   if (located_stn)
      {
      stnptr->lat = metadata_stn_info[j].lat; 
      stnptr->lon = metadata_stn_info[j].lon;

      if (stnptr->occur == 0)
         stnptr->occur = metadata_stn_info[j].occur;
      else
         {
         printf ("WARNING: stn occur = %d MISMATCH with stn_list occur = %d. Keeping stn occur!!\n");
         printf ("Modify input stn_list file to match data for stn: %-s\n", stnptr->stnid_ext);
         }

      stnptr->accuracy = metadata_stn_info[j].accuracy;

      strncpy( stnptr->name, metadata_stn_info[j].name, 51);

      strncpy( stnptr->country, metadata_stn_info[j].country, 3);
      strncpy( stnptr->state, metadata_stn_info[j].state, 3);
      strncpy( stnptr->county, metadata_stn_info[j].county, 4);

      stnptr->time_zone = metadata_stn_info[j].time_zone;
      stnptr->dst_switch = metadata_stn_info[j].dst_switch;
       
      strncpy( stnptr->frequency, metadata_stn_info[j].frequency, 16);
 
      stnptr->elev = metadata_stn_info[j].elev;
      stnptr->fixed_mobile = metadata_stn_info[j].fixed_mobile;
 
      return(1);
      }
   else
      {
      printf ("Can't locate stn in metadata_stn_info file:%-sxx \n", stnptr->stnid_ext);
      return(0);
      }


   }/* locate_stn_in_metadata_by_ID() */

/*--------------------------------------------------------
 * locate_stn_in_metadata_by_latlon - return 1 if found in metadata
 *  and stnptr updated with data, else return 0. This 
 *  fn is similar to locate_stn_in_master_list, except 
 *  that it searches the meta data NOT the existing 
 *  stn list. Assumes metadata sorted by lat/lon. 
 * 
 * 10 May 94 lec
 *   Created.
 *-------------------------------------------------------*/
int locate_stn_in_metadata_by_latlon (/*in*/char stnid_ext[19], 
                                       /*in*/int num_meta_stns, 
                                       /*in/out*/ STNREC  *stnptr)
   {
   int       min,  max, j;
   int       kk;
   int       located_stn = 0;
   int       seq_search = 0;
 
   /*
    * Use binary search for lat/lon. The sequential search
    * until find platform and possible elev match. Note that
    * some data sets don't have elev.
    */
   /*printf ("Enter locate_stn_in_metadata: stnid_ext, num_meta_stns: %-sxxx, %d\n:",
            stnid_ext, num_meta_stns); */

   located_stn = 0;
   seq_search = 0;

   /*printf ("locate stn: stnptr->stnid_ext, lat,lon,elev,platform: %-sxxx %f %f %f %d\n",
        stnptr->stnid_ext, stnptr->lat, stnptr->lon, stnptr->elev, stnptr->platform); */

   for (min=0, max=num_meta_stns, j=num_meta_stns/2; j>=0, j<=num_meta_stns, min<=max; j=min+(max-min)/2 )
      {
       /*printf ("min, max, j: %d %d %d\n", min, max, j); 
       printf ("   Compare: stnptr->lat, metadata_stn_info[j].lat: %f %f\n",
               stnptr->lat, metadata_stn_info[j].lat);
       printf ("   Compare: stnptr->lon, metadata_stn_info[j].lon: %f %f\n", 
               stnptr->lon, metadata_stn_info[j].lon);    

       printf ("   Compare: stnptr->platform, metadata_stn_info[j].platform: %d %d\n",
               stnptr->platform, metadata_stn_info[j].platform); */
 
      if (metadata_stn_info[j].lat ==  stnptr->lat &&
          metadata_stn_info[j].lon == stnptr->lon &&
          (stnptr->platform == metadata_stn_info[j].platform)  )
         {
         /*
          * Check for external ID match. 
          */
         /*printf ("LAT,LON, PLAT match!Check stnptr->stnid_ext, metadata_stn_info[j].stnid_ext:: %-s %-s\n",
                  stnptr->stnid_ext,  metadata_stn_info[j].stnid_ext); */

         if (!strncmp (stnptr->stnid_ext,  metadata_stn_info[j].stnid_ext, 18))
            {
            /*
             * Absolute match. Check no more!
             */
            located_stn = 1;
            break;
            }

         /*
          * Found match on lat and lon. Check for
          * elevation match if not missing.
          * Platform MUST match, else may have coloc stn.
          * Match elev to within one meter.
          * For now say match if all but elev matches.
          */
         /*printf ("lat, lon, platform match - check elev if not missing\n");  */
         located_stn = 1;
         
         if (stnptr->elev <= -999.9)
            {
            /*
             * Elev missing, consider it a match.
             */
            /*printf ("Elev is missing - no check, accept stn match!\n");  8
            located_stn = 1;
            }
         else
            {
            /*
             * Elev exists, so it must match or else
             * not same stn!
             */
            /*printf ("Check elevation!\n");  */
            if( (metadata_stn_info[j].elev <= stnptr->elev + 1) &&
                (metadata_stn_info[j].elev >= stnptr->elev - 1) )
               {
               /*
                * This is definitely the same stn! Set
                * stnptr data from metadata_stn_info.
                */
               located_stn = 1;
               /*printf ("Elev matches!\n");  */
               }
            else
               {
               /* printf ("match FAILED on elev! Keep searching.\n"); */
               located_stn = 0;
               }
            } /* elev missing? */

         break;
         } /* check on lat/lon */

      else if (metadata_stn_info[j].lat != stnptr->lat &&
                metadata_stn_info[j].lat > stnptr->lat)
         {
         /* printf ("meta lat > lat; max = j-1. %f %f\n", metadata_stn_info[j].lat, stnptr->lat);   */
         max = j - 1;
         }
      else if (metadata_stn_info[j].lat != stnptr->lat &&
                metadata_stn_info[j].lat < stnptr->lat)
         {
         /* printf ("meta lat < lat; min = j+1. %f %f\n", metadata_stn_info[j].lat, stnptr->lat); */
         min = j + 1;
         }
/*      else if (inrange(metadata_stn_info[j].lat, stnptr->lat, 0)) */
      else if (metadata_stn_info[j].lat == stnptr->lat) /* should match exactly! */
         {
         printf ("Do seq search. meta lat, lat: %f %f\n", metadata_stn_info[j].lat, stnptr->lat); 
         printf ("meta lon, lon: %f %f\n", metadata_stn_info[j].lon, stnptr->lon); 
         seq_search = 1;
         break;
         }
      else
         min = j + 1;

      } /* for */


   if (!located_stn && seq_search)
      {
      /* printf ("Begin seq search, j =%d \n",j);  */

      if (metadata_stn_info[kk].lon < stnptr->lon )
         {
         /*printf ("meta lon < lon \n"); */
         for (kk = j; kk<=num_meta_stns; kk++)
           {
           /* printf ("11 j, kk:: %d %d\n", j, kk); */

            if ( !inrange(metadata_stn_info[kk].lat, stnptr->lat, 0))
               break;

            if ( metadata_stn_info[kk].lon == stnptr->lon &&
                 (metadata_stn_info[kk].platform == stnptr->platform) &&
                 (!strncmp (stnptr->stnid_ext,  metadata_stn_info[j].stnid_ext, 18)) )
               {
               located_stn = 1;
               j = kk;
               break;
               }
            }   

         if (!located_stn)
            {
            for (kk = j; kk>=0; kk--)
              {
              /* printf ("12 j, kk:: %d %d\n", j, kk); */
              if ( !inrange(metadata_stn_info[kk].lat, stnptr->lat, 0))
                 break;

              if ( metadata_stn_info[kk].lon == stnptr->lon &&
                  (metadata_stn_info[kk].platform == stnptr->platform) &&
                  (!strncmp (stnptr->stnid_ext,  metadata_stn_info[j].stnid_ext, 18)) )
                  {
                  located_stn = 1;
                  j = kk;
                  break;
                  }
               } /* for */
            }
         } /* meta lon < lon */
      else
         {
         /* printf ("meta lon > lon \n"); */

         for (kk = j; kk>=0; kk--)
           {
           /*printf ("21 j, kk:: %d %d\n", j, kk); */
           if ( inrange(metadata_stn_info[kk].lat, stnptr->lat, 0))
              break;

           if ( metadata_stn_info[kk].lon == stnptr->lon &&
                (metadata_stn_info[kk].platform == stnptr->platform) &&
                (!strncmp (stnptr->stnid_ext,  metadata_stn_info[j].stnid_ext, 18) ))
               {
               located_stn = 1;
               j = kk;
               break;
               }
            }   

         if (!located_stn)
            {
            for (kk = j; kk<=num_meta_stns; kk++)
              {
              /*printf ("22 j, kk:: %d %d\n", j, kk); */
              if ( inrange( metadata_stn_info[kk].lat, stnptr->lat, 0))
                 break;

              if ( metadata_stn_info[kk].lon == stnptr->lon &&
                   (metadata_stn_info[kk].platform == stnptr->platform) &&
                   (!strncmp (stnptr->stnid_ext,  metadata_stn_info[j].stnid_ext, 18) ))
                  {
                  located_stn = 1;
                  j = kk;
                  break;
                  }
               } /* for */
            } /* !located_stn */
         } /* meta lon < lon */

      } /* !located_stn && seq_search */


   /*
    * If stn has been located in existing stn list file, we let
    * this data override data read in from file...except for
    * begin and end times.
    */
   if (located_stn)
      {
      stnptr->lat = metadata_stn_info[j].lat;  /* should be same */
      stnptr->lon = metadata_stn_info[j].lon;  /* should be same */

      if (stnptr->occur == 0)
         stnptr->occur = metadata_stn_info[j].occur;
      else
         {
         printf ("WARNING: stn occur. MISMATCH with stn_list occur. Keeping stn occur for stn: %-s\n",
                 stnptr->stnid_ext);
         printf ("Modify input stn_list file to match data for stn: %-s\n", stnptr->stnid_ext);
         }

      stnptr->accuracy =  metadata_stn_info[j].accuracy;

      strncpy( stnptr->name, metadata_stn_info[j].name, 51); /* should be same */

      strncpy( stnptr->country, metadata_stn_info[j].country, 3);
      strncpy( stnptr->state, metadata_stn_info[j].state, 3);
      strncpy( stnptr->county, metadata_stn_info[j].county, 4);

      stnptr->time_zone = metadata_stn_info[j].time_zone;
      stnptr->dst_switch = metadata_stn_info[j].dst_switch;

      strncpy( stnptr->frequency, metadata_stn_info[j].frequency, 16); /* should be same */

      stnptr->elev = metadata_stn_info[j].elev; /* should be same */
      stnptr->fixed_mobile = metadata_stn_info[j].fixed_mobile;

      return(1);
      }
   else
      {
      printf ("Can't locate stn in metadata_stn_info file:%-sxx \n", stnptr->stnid_ext);
      return(0);
      }
   } /* locate_stn_in_metadata_by_latlon() */
