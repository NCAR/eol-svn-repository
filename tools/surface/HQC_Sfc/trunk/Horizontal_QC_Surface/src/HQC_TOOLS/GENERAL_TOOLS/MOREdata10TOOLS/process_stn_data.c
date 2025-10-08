#ifndef lint
static char *rcsid = "$Id$";
#endif


/*
 * $Log$
 *
 */

/*-------------------------------------------------------
 * process_stn_data.c - This module contains the
 *     fns which process the stn data ptr, including 
 *     initializing and writing out the stn data to the 
 *     stations file and the stn_id file. These files are
 *     input into the CODIAC system. The stations file is
 *     analogous to the master stn list for a project.
 *
 * 09 May 94 lec
 *   Created.
 *-------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
 
#include "local.h"
#include "process_stn_data.h"


/* Variables Global to this module */

static STNREC  *def_stnptr; /* default stn info */
static STNREC  old_stn_info[MAX_ELEMS];
static STNREC  new_stn_info[MAX_ELEMS];


/*----------------------------------------------------------------------
 * inrange() - This fn is used to determine if float values such as
 *   lat and lon are with in the input range value of each other.
 *   Note that since input numbers are converted to longs, saving the
 *   two most significant decimal places, the range input actually
 *   is applied as tenths!
 *
 * 000 10 May 94 lec
 *    Created.
 *---------------------------------------------------------------------*/
int inrange( /*in*/ float x,
             /*in*/ float y,
             /*in*/ int   range)
   {
   long int  x_int, y_int;

   /*
    * First convert floats to ints. We save two decimal places
    * for comparison.
    */
   if (x < 0.0)
     x_int = x*100.0 - 0.5;
   else x_int = x*100.0 + 0.5;

   if (y < 0.0)
     y_int = y*100.0 - 0.5;
   else y_int = y*100.0 + 0.5;

   /* printf ("x, y, x_int, y_int:: %f %f %d %d\n", x, y, x_int, y_int);  */

   if ( (x_int <= y_int+range)&&
        (x_int >= y_int-range) )
      return (1);
   else
      return (0);
 
   } /* inrange() */



/*----------------------------------------------------------------------
 * read_old_stn_info() - reads data from existing stn list. For GIDS-1
 *   this is the fest_stn_list. Data must be in specific order in this
 *   existing file.
 *
 * 000 10 May 94 lec
 *    Created.
 *---------------------------------------------------------------------*/
void  read_old_stn_info( /*in*/  char          input_file_name[MAX_CHARS],
                         /*out*/ long          *num_stns)
   {
   FILE     *input_stream;
    
   long int i = -1;
   long int j = -1;

   char     newline[MAX_CHARS];
   char     junk_str[75];

 
   *num_stns = 0;
 
   if (( input_stream = fopen(input_file_name, "r")) == NULL)
      perror ("Error: Can't open input_file");

  /*  printf ("\n\nEnter read_old_stn_info - open file\n"); */

   while (!feof(input_stream))
      {
      /*
       * Reinitialize the strings.
       */
      i++;

      for (j=0;j<75;j++)junk_str[j] = '\0';
      for (j=0;j<MAX_CHARS;j++)newline[j] = '\0';

      for (j=0;j<16;j++)old_stn_info[i].project[j] = '\0';
      for (j=0;j<19;j++)old_stn_info[i].stnid_ext[j] = '\0';
      for (j=0;j<51;j++)old_stn_info[i].name[j] = '\0';
      for (j=0;j<9;j++)old_stn_info[i].begin_date[j] = '\0';
      for (j=0;j<9;j++)old_stn_info[i].end_date[j] = '\0';
      for (j=0;j<3;j++)old_stn_info[i].country[j] = '\0';
      for (j=0;j<3;j++)old_stn_info[i].state[j] = '\0';
      for (j=0;j<4;j++)old_stn_info[i].county[j] = '\0';
      for (j=0;j<15;j++)old_stn_info[i].frequency[j] = '\0';

      read_record (&input_stream, newline);
     /*  printf ("newline:%-sxxx\n", newline);*/

      if( feof (input_stream))
         break;

      strncpy ( old_stn_info[i].project, newline, 15);

      strncpy (junk_str, &newline[18],5);
      junk_str[5] = '\0';
      old_stn_info[i].stnid_int = atoi (junk_str);

      for (j=0;j<75;j++)junk_str[j] = '\0';
      strncpy (junk_str, &newline[24],10);
      old_stn_info[i].lat = atof (junk_str);

      for (j=0;j<75;j++)junk_str[j] = '\0';
      strncpy (junk_str, &newline[36],11);
      old_stn_info[i].lon = atof (junk_str);

      for (j=0;j<75;j++)junk_str[j] = '\0';
      strncpy (junk_str, &newline[47], 6); 
      old_stn_info[i].occur = atoi (junk_str); 

      for (j=0;j<75;j++)junk_str[j] = '\0';
      strncpy (junk_str, &newline[54],9); 
      old_stn_info[i].accuracy = atoi (junk_str); 

      strncpy (old_stn_info[i].name, &newline[65],50); 
      strncpy (old_stn_info[i].begin_date, &newline[119],8); 
      strncpy (old_stn_info[i].end_date, &newline[129],8); 

      strncpy (old_stn_info[i].country, &newline[139],2); 
      strncpy (old_stn_info[i].state, &newline[148],2); 
      strncpy (old_stn_info[i].county, &newline[155],3); 

      for (j=0;j<75;j++)junk_str[j] = '\0';
      strncpy (junk_str, &newline[165],7); 
      old_stn_info[i].time_zone = atof (junk_str); 

      old_stn_info[i].dst_switch =  newline[174]; 

      for (j=0;j<75;j++)junk_str[j] = '\0';
      strncpy (junk_str, &newline[186],8); 
      old_stn_info[i].platform = atoi (junk_str); 

      strncpy (old_stn_info[i].frequency, &newline[196],15); 

      for (j=0;j<75;j++)junk_str[j] = '\0';
      strncpy(junk_str, &newline[213],8);
      old_stn_info[i].elev = atof (junk_str);

      old_stn_info[i].fixed_mobile = newline[223];
#if 0 
      printf ("old_stn_info[%d].project: %-sxxx::\n", i, old_stn_info[i].project);
      printf ("old_stn_info[%d].stnid_int: %dxxx::\n", i, old_stn_info[i].stnid_int);
      printf ("old_stn_info[%d].lat: %fxxx::\n", i, old_stn_info[i].lat);
      printf ("old_stn_info[%d].lon: %fxxx::\n", i, old_stn_info[i].lon);
      printf ("old_stn_info[%d].occur: %dxxx::\n", i, old_stn_info[i].occur);
      printf ("old_stn_info[%d].accuracy: %dxx::\n", i, old_stn_info[i].accuracy);
      printf ("old_stn_info[%d].name: %-sxxx::\n", i, old_stn_info[i].name);
      printf ("old_stn_info[%d].begin_date: %-sxxx::\n", i, old_stn_info[i].begin_date);
      printf ("old_stn_info[%d].end_date: %-sxxx::\n", i, old_stn_info[i].end_date);
      printf ("old_stn_info[%d].country: %-sxxx::\n", i, old_stn_info[i].country);
      printf ("old_stn_info[%d].state: %-sxxx::\n", i, old_stn_info[i].state);
      printf ("old_stn_info[%d].county: %-sxxx::\n", i, old_stn_info[i].county);
      printf ("old_stn_info[%d].time_zone: %fxxx::\n", i, old_stn_info[i].time_zone);
      printf ("old_stn_info[%d].dst_switch: %cxxx::\n", i, old_stn_info[i].dst_switch);
      printf ("old_stn_info[%d].platform: %dxx::\n", i, old_stn_info[i].platform);
      printf ("old_stn_info[%d].frequency: %-sxxx::\n", i, old_stn_info[i].frequency);
      printf ("old_stn_info[%d].elev: %fxxx::\n", i, old_stn_info[i].elev);
      printf ("old_stn_info[%d].fixed_mobile: %cxxx::\n", i, old_stn_info[i].fixed_mobile);
#endif 
      } /* while */
 
   *num_stns = i;

   printf ("Num_stns in old_stn_info file: %ld\n", *num_stns);
 
   if (fclose (input_stream) == EOF)
      perror ("Error: Can't close input_file");
 
   } /* read_old_stn_info() */

/*----------------------------------------------------------------------
 * read_new_stn_info() - reads data from existing stn list. This is the
 *  new/forming stn list. Specific format expected.
 *
 * 000 10 May 94 lec
 *    Created.
 *---------------------------------------------------------------------*/
void  read_new_stn_info( /*in*/  char          input_file_name[MAX_CHARS],
                         /*out*/ long          *num_stns)
   {
   FILE     *input_stream;
   
   long int i = -1;
   long int j = -1;
 
   char     newline[MAX_CHARS];
   char     junk_str[75];
 
 
   *num_stns = 0;

   if (( input_stream = fopen(input_file_name, "r")) == NULL)
      perror ("Error: Can't open input_file");

   /*printf ("\n\nEnter read_new_stn_info - open file\n");  */

   while (!feof(input_stream))
      {
      /*
       * Reinitialize the strings.
       */
      i++;
         
      for (j=0;j<75;j++)junk_str[j] = '\0';
      for (j=0;j<MAX_CHARS;j++)newline[j] = '\0';
 
      for (j=0;j<16;j++)new_stn_info[i].project[j] = '\0';
      for (j=0;j<19;j++)new_stn_info[i].stnid_ext[j] = '\0';
      for (j=0;j<51;j++)new_stn_info[i].name[j] = '\0';
      for (j=0;j<9;j++)new_stn_info[i].begin_date[j] = '\0';
      for (j=0;j<9;j++)new_stn_info[i].end_date[j] = '\0';
      for (j=0;j<3;j++)new_stn_info[i].country[j] = '\0';
      for (j=0;j<3;j++)new_stn_info[i].state[j] = '\0';
      for (j=0;j<4;j++)new_stn_info[i].county[j] = '\0';
      for (j=0;j<15;j++)new_stn_info[i].frequency[j] = '\0';
 
      read_record (&input_stream, newline);
      /*printf ("newline:%-sxxx\n", newline); */
      
      if( feof (input_stream))
         break;
 
      strncpy ( new_stn_info[i].project, newline, 15);

      strncpy (junk_str, &newline[21],6);
      junk_str[5] = '\0';
      new_stn_info[i].stnid_int = atoi (junk_str);

      for (j=0;j<75;j++)junk_str[j] = '\0';
      strncpy (junk_str, &newline[27],10);
      new_stn_info[i].lat = atof (junk_str);
 
      for (j=0;j<75;j++)junk_str[j] = '\0';
      strncpy (junk_str, &newline[38],11);
      new_stn_info[i].lon = atof (junk_str);
 
      for (j=0;j<75;j++)junk_str[j] = '\0';
      strncpy (junk_str, &newline[50], 3);
      new_stn_info[i].occur = atoi (junk_str);
 
      for (j=0;j<75;j++)junk_str[j] = '\0';
      strncpy (junk_str, &newline[56],3);
      new_stn_info[i].accuracy = atoi (junk_str);
 
      strncpy (new_stn_info[i].name, &newline[60],50);
      strncpy (new_stn_info[i].begin_date, &newline[111],8);
      strncpy (new_stn_info[i].end_date, &newline[120],8);
 
      strncpy (new_stn_info[i].country, &newline[129],2);
      strncpy (new_stn_info[i].state, &newline[132],2);
      strncpy (new_stn_info[i].county, &newline[135],3);
 
      for (j=0;j<75;j++)junk_str[j] = '\0';
      strncpy (junk_str, &newline[140],5);
      new_stn_info[i].time_zone = atof (junk_str);

      new_stn_info[i].dst_switch =  newline[146];

      for (j=0;j<75;j++)junk_str[j] = '\0';
      strncpy (junk_str, &newline[149],3);
      new_stn_info[i].platform = atoi (junk_str);

      strncpy (new_stn_info[i].frequency, &newline[153],15);
 
      for (j=0;j<75;j++)junk_str[j] = '\0';
      strncpy(junk_str, &newline[172],6);
      new_stn_info[i].elev = atof (junk_str);
 
      new_stn_info[i].fixed_mobile = newline[179];
#if 0
      printf ("new_stn_info[%d].project: %-sxxx::\n", i, new_stn_info[i].project);
      printf ("new_stn_info[%d].stnid_int: %dxxx::\n", i, new_stn_info[i].stnid_int);
      printf ("new_stn_info[%d].lat: %fxxx::\n", i, new_stn_info[i].lat);
      printf ("new_stn_info[%d].lon: %fxxx::\n", i, new_stn_info[i].lon);
      printf ("new_stn_info[%d].occur: %dxxx::\n", i, new_stn_info[i].occur);
      printf ("new_stn_info[%d].accuracy: %dxx::\n", i, new_stn_info[i].accuracy);
      printf ("new_stn_info[%d].name: %-sxxx::\n", i, new_stn_info[i].name);
      printf ("new_stn_info[%d].begin_date: %-sxxx::\n", i, new_stn_info[i].begin_date);
      printf ("new_stn_info[%d].end_date: %-sxxx::\n", i, new_stn_info[i].end_date);
      printf ("new_stn_info[%d].country: %-sxxx::\n", i, new_stn_info[i].country);
      printf ("new_stn_info[%d].state: %-sxxx::\n", i, new_stn_info[i].state);
      printf ("new_stn_info[%d].county: %-sxxx::\n", i, new_stn_info[i].county);
      printf ("new_stn_info[%d].time_zone: %fxxx::\n", i, new_stn_info[i].time_zone);
      printf ("new_stn_info[%d].dst_switch: %cxxx::\n", i, new_stn_info[i].dst_switch);
      printf ("new_stn_info[%d].platform: %dxx::\n", i, new_stn_info[i].platform);
      printf ("new_stn_info[%d].frequency: %-sxxx::\n", i, new_stn_info[i].frequency);
      printf ("new_stn_info[%d].elev: %fxxx::\n", i, new_stn_info[i].elev);
      printf ("new_stn_info[%d].fixed_mobile: %cxxx::\n", i, new_stn_info[i].fixed_mobile);
#endif
      } /* while */

   *num_stns = i;

   printf ("Num_stns in new_stn_info file: %ld\n", *num_stns);
 
   if (fclose (input_stream) == EOF)
      perror ("Error: Can't close input_file");


   } /* read_new_stn_info() */
 
/*----------------------------------------------------------------------
 * locate_stn_in_old_master_list() - assumes data is sorted by lat/lon/occur in
 *   old_stn_info file (AKA master stn list). This fn returns a -1 if
 *   stn was not located and a 1 if the stn was located. If the stn
 *   was located, then the input stnptr is updated with the info from
 *   the old_stn_info. This is important for GIDS-1 since we must
 *   retain the occurrance values in the FEST data that was merged
 *   into GIDS-1. This s/w uses a combination of binary search and 
 *   sequential search methods, because searching on real numbers.
 *   Use binary search to locate lat, then sequentially search in
 *   either or both directions for lon/platform match.
 *
 * 000 10 May 94 lec
 *    Created.
 *---------------------------------------------------------------------*/
int  locate_stn_in_old_master_list( /*in*/  char  stnid_ext[16],
                                /*in*/  long  num_stns,
                                /*in/out*/ STNREC *stnptr)
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
   located_stn = 0;
   seq_search = 0;
   
   /* printf ("enter locate stn: stnptr->lat,lon,elev,platform: %f %f %f %d\n",
        stnptr->lat, stnptr->lon, stnptr->elev, stnptr->platform); */
 
   for (min=0, max=num_stns, j=num_stns/2; j>=0, j<=num_stns, min<=max; j=min+(max-min)/2 ) 
      {
      /* printf ("min, max, j: %d %d %d\n", min, max, j); 
      printf ("Compare:: stnptr->platform, old_stn_info[j].platform: %d %d\n",
               stnptr->platform, old_stn_info[j].platform); */

      if (inrange(old_stn_info[j].lat, stnptr->lat, 0) &&
          inrange(old_stn_info[j].lon, stnptr->lon, 0) &&
          (stnptr->platform == old_stn_info[j].platform)  )
         {
         /*
          * Found match on lat and lon. Check for 
          * elevation match if not missing.
          * Platform MUST match, else may have coloc stn.
          * Match elev to within one meter.
          * For now say match if all but elev matches.
          */
         /* printf ("lat, lon, platform match - check elev if not missing\n");  */
         located_stn = 1; 

         if (stnptr->elev <= -999.9) 
            {
            /*
             * Elev missing, consider it a match.
             */
            /* printf ("Elev is missing - no check, accept stn match!\n"); */
            located_stn = 1;
            }
         else
            {
            /*
             * Elev exists, so it must match or else
             * not same stn!
             */
            /* printf ("Check elevation!\n"); */
            if( (old_stn_info[j].elev <= stnptr->elev + 1) &&
                (old_stn_info[j].elev >= stnptr->elev - 1) ) 
               {
               /*
                * This is definitely the same stn! Set
                * stnptr data from old_stn_info.
                */
               located_stn = 1;
               /* printf ("Elev matches!\n");  */
               }
            else
               {
               /* printf ("match FAILED on elev! Keep searching.\n"); */
               located_stn = 0; 
               }
            } /* elev missing? */

         break;
         } /* check on lat/lon */

      else if (!inrange(old_stn_info[j].lat, stnptr->lat, 0) &&
                old_stn_info[j].lat > stnptr->lat)
         { 
         /*printf ("old lat > lat; max = j-1\n");  */
         max = j - 1;
         } 
      else if (!inrange(old_stn_info[j].lat, stnptr->lat, 0) &&
               old_stn_info[j].lat < stnptr->lat)
         {  
         /* printf ("old lat < lat; min = j+1\n");  */
         min = j + 1;
         }
      else if (inrange(old_stn_info[j].lat, stnptr->lat, 0))
         {
         /* printf ("Do seq search\n"); */
         seq_search = 1;
         break;
         }
      else
         min = j + 1;

      } /* for */


   if (!located_stn && seq_search)
      {
      /* printf ("Begin seq search, j =%d \n",j);
      printf ("old lon[j], stnptr->lon:: %f  %f\n", old_stn_info[j].lon, stnptr->lon); */

      if (old_stn_info[j].lon < stnptr->lon )
         {
         /* printf ("old lon < lon \n"); */
         for (kk = j; kk<=num_stns; kk++)
           {
           /* printf ("11 j, kk:: %d %d\n", j, kk); */

            if ( !inrange(old_stn_info[kk].lat, stnptr->lat, 0))
               break;

            if ( inrange(old_stn_info[kk].lon, stnptr->lon, 0) &&
                 (old_stn_info[kk].platform == stnptr->platform))
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
              /* printf ("12 j, kk:: %d %d\n", j, kk);*/
              if ( !inrange(old_stn_info[kk].lat, stnptr->lat, 0)) 
                 break; 

              if ( inrange(old_stn_info[kk].lon, stnptr->lon, 0) &&
                  (old_stn_info[kk].platform == stnptr->platform))
                  {  
                  located_stn = 1;
                  j = kk;
                  break;
                  } 
               } /* for */
            }
         } /* old lon < lon */
      else
         {
         /* printf ("NEW:::old lon > lon \n"); */

         for (kk = j; kk>=0; kk--)
           {
           /* printf ("21 j, kk:: %d %d\n", j, kk); */
           if ( !inrange(old_stn_info[kk].lat, stnptr->lat, 0)) 
              break; 

           if ( inrange(old_stn_info[kk].lon, stnptr->lon, 0) &&
                (old_stn_info[kk].platform == stnptr->platform))
               {  
               located_stn = 1;
               j = kk;
               break;
               } 
           } /* for kk */  
 
         if (!located_stn) 
            {
            for (kk = j; kk<=num_stns; kk++) 
              {
              /*printf ("22 j, kk:: %d %d\n", j, kk); */
              if ( !inrange(old_stn_info[kk].lat, stnptr->lat, 0)) 
                 break; 

              if ( inrange(old_stn_info[kk].lon, stnptr->lon, 0) &&
                   (old_stn_info[kk].platform == stnptr->platform)) 
                  {   
                  located_stn = 1; 
                  j = kk; 
                  break; 
                  }  
               } /* for */
            } /* !located_stn */
         } /* old lon < lon */ 

      } /* !located_stn && seq_search */


   /*
    * If stn has been located in existing stn list file, we let
    * this data override data read in from file...except for
    * begin and end times.
    */
   if (located_stn)
      {
      /* printf ("Have located stn : %-s\n", stnptr->stnid_ext); */

      stnptr->lat = old_stn_info[j].lat;  /* should be same */
      stnptr->lon = old_stn_info[j].lon;  /* should be same */

      if (stnptr->occur == 0)
         stnptr->occur = old_stn_info[j].occur;
      else
         {
         printf ("WARNING: stn occur = %d MISMATCH with stn_list occur = %d. Keeping stn occur!!\n");
         printf ("Modify input stn_list file to match data for stn: %-s\n", stnptr->stnid_ext); 
         }

      stnptr->accuracy = old_stn_info[j].accuracy;

      strncpy( stnptr->name, old_stn_info[j].name, 51); /* should be same */

      strncpy( stnptr->country, old_stn_info[j].country, 3);
      strncpy( stnptr->state, old_stn_info[j].state, 3);
      strncpy( stnptr->county, old_stn_info[j].county, 4);
 
      stnptr->time_zone = old_stn_info[j].time_zone;
      stnptr->dst_switch = old_stn_info[j].dst_switch;

      strncpy( stnptr->frequency, old_stn_info[j].frequency, 16); /* should be same */

      stnptr->elev = old_stn_info[j].elev; /* should be same */
      stnptr->fixed_mobile = old_stn_info[j].fixed_mobile;

      /* printf ("Locate stn in old_stn_info file:%-sxx \n", stnptr->stnid_ext); */
      return(1);
      }
   else
      {
      /* printf ("Can't locate stn in old_stn_info file:%-sxx \n", stnptr->stnid_ext); */
      return(0);
      }
 
   } /* locate_stn_in_old_master_list() */

/*----------------------------------------------------------------------
 * locate_stn_in_new_master_list() - assumes data is sorted by lat/lon/occur in
 *   new_stn_info file (AKA forming master stn list). This fn returns a 0 if
 *   stn was not located and a 1 if the stn was located. If the stn
 *   was located, then the stn is already in the new stn list. Do nothing.
 *   If not found, must search for info in old master stn list (if available).
 *   This fn is almost identical to the other locate fns. Maybe they
 *   could be combined.
 * 
 * 000 10 May 94 lec
 *    Created.
 *---------------------------------------------------------------------*/
int  locate_stn_in_new_master_list( /*in*/  char  stnid_ext[16],
                                    /*in*/  long  num_stns,
                                    /*in/out*/ STNREC *stnptr)
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
   located_stn = 0;
   seq_search = 0;

   /* printf ( "enter locate new stn: stnptr->lat,lon,elev,platform: %f %f %f %d\n",
        stnptr->lat, stnptr->lon, stnptr->elev, stnptr->platform);  */

   for (min=0, max=num_stns, j=num_stns/2; j>=0, j<=num_stns, min<=max; j=min+(max-min)/2 )
      {
      /* printf ("min, max, j: %d %d %d\n", min, max, j);
      printf ("Compare:: stnptr->platform, new_stn_info[j].platform: %d %d\n",
               stnptr->platform, new_stn_info[j].platform);  */
 
      if (inrange(new_stn_info[j].lat, stnptr->lat, 1) &&
          inrange(new_stn_info[j].lon, stnptr->lon, 1) &&
          (stnptr->platform == new_stn_info[j].platform)  )
         {
         /*
          * Found match on lat and lon. Check for
          * elevation match if not missing.
          * Platform MUST match, else may have coloc stn.
          * Match elev to within one meter.
          * For now say match if all but elev matches.
          */
         /* printf ("lat, lon, platform match - check elev if not missing\n");  */
         located_stn = 1;
 
         if (stnptr->elev <= -999.9)
            {
            /*
             * Elev missing, consider it a match.
             */
            /* printf ("Elev is missing - no check, accept stn match!\n"); */
            located_stn = 1;
            }
         else
            {
            /*
             * Elev exists, so it must match or else
             * not same stn!
             */
            /* printf ("Check elevation! new_stn_info[j].elev, stnptr->elev: %f %f\n",  
                     new_stn_info[j].elev, stnptr->elev); */
            if( (new_stn_info[j].elev <= stnptr->elev + 1) &&
                (new_stn_info[j].elev >= stnptr->elev - 1) )
               {
               /*
                * This is definitely the same stn! Set
                * stnptr data from new_stn_info.
                */
               located_stn = 1;
               /* printf ("Elev matches!\n"); */
               }
            else
               {
               /* printf ("match FAILED on elev! Keep searching.\n"); */
               located_stn = 0;
               }
            } /* elev missing? */
 
         break;
         } /* check on lat/lon */

      else if (!inrange(new_stn_info[j].lat, stnptr->lat, 1) &&
                new_stn_info[j].lat > stnptr->lat)
         {  
         /* printf ("old lat > lat; max = j-1\n");  */
         max = j - 1;
         }  
      else if (!inrange(new_stn_info[j].lat, stnptr->lat, 1) &&
               new_stn_info[j].lat < stnptr->lat)
         {  
         /* printf ("old lat < lat; min = j+1\n");  */
         min = j + 1;
         }  
      else if (inrange(new_stn_info[j].lat, stnptr->lat, 1))
         {  
        /*  printf ("Do seq search\n"); */
         seq_search = 1;
         break;
         }  
      else
         min = j + 1;

      } /* for */


   if (!located_stn && seq_search)
      {
      /* printf ("Begin seq search, j =%d \n",j);
      printf ("old lon[j], stnptr->lon:: %f  %f\n", new_stn_info[j].lon, stnptr->lon); */

      if (new_stn_info[j].lon < stnptr->lon )
         {
         /* printf ("old lon < lon \n"); */
         for (kk = j; kk<=num_stns; kk++)
           {
           /* printf ("11 j, kk:: %d %d\n", j, kk); */

            if ( !inrange(new_stn_info[kk].lat, stnptr->lat, 1))
               break;

            if ( inrange(new_stn_info[kk].lon, stnptr->lon, 1) &&
                 (new_stn_info[kk].platform == stnptr->platform))
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
              if ( !inrange(new_stn_info[kk].lat, stnptr->lat, 1))
                 break;

              if ( inrange(new_stn_info[kk].lon, stnptr->lon, 1) &&
                  (new_stn_info[kk].platform == stnptr->platform))
                  {
                  located_stn = 1;
                  j = kk;
                  break;
                  }
               } /* for */
            }
         } /* old lon < lon */
      else
         {
         /* printf ("NEW:::old lon > lon \n"); */
 
         for (kk = j; kk>=0; kk--)
           {
           /* printf ("21 j, kk:: %d %d\n", j, kk); */
           if ( !inrange(new_stn_info[kk].lat, stnptr->lat, 1))
              break;
 
           if ( inrange(new_stn_info[kk].lon, stnptr->lon, 1) &&
                (new_stn_info[kk].platform == stnptr->platform))
               {
               located_stn = 1;
               j = kk;
               break;
               }
           } /* for kk */
 
         if (!located_stn)
            {
            for (kk = j; kk<=num_stns; kk++)
              {
              /* printf ("22 j, kk:: %d %d\n", j, kk); */
              if ( !inrange(new_stn_info[kk].lat, stnptr->lat, 1))
                 break;

              if ( inrange(new_stn_info[kk].lon, stnptr->lon, 1) &&
                   (new_stn_info[kk].platform == stnptr->platform))
                  {
                  located_stn = 1;
                  j = kk;
                  break;
                  }
               } /* for */
            } /* !located_stn */
         } /* old lon < lon */
 
      } /* !located_stn && seq_search */
 
 
   /*
    * If stn has been located in existing stn list file, we let
    * this data override data read in from file...except for
    * this data override data read in from file...except for
    * begin and end times.
    */
   if (located_stn)
      {
      /* printf ("Located stn in NEW stn list. DO NOT WRITE RECORD! : %-s\n", stnptr->stnid_ext); */
      return(1);
      }
   else
      {
      /* printf ("Can't locate stn in NEW_stn_info file:%-sxx \n", stnptr->stnid_ext);*/
      return(0);
      }
 
   } /* locate_stn_in_new_master_list() */
 


/*-------------------------------------------------------
 * determine_platform - relates input acronym to internal
 *  db numbering scheme.
 *
 * 000 10 May 94 lec
 *    Created.
 *-------------------------------------------------------*/
int  determine_platform( /*in*/  char  current_acronym[11])
   {

   if (!strncmp (current_acronym, "WDPN", 4))
      return(1);
   if (!strncmp (current_acronym, "COOP", 4)) 
      return(2); 
   if (!strncmp (current_acronym, "HPLAINS", 7)) 
      return(16); 
   if (!strncmp (current_acronym, "ISWS", 4)) 
      return(17); 
   if (!strncmp (current_acronym, "PAM", 3)) 
      return(32); 
   if (!strncmp (current_acronym, "ACARS", 5)) 
      return(38); 
   if (!strncmp (current_acronym, "ASOS", 4)) 
      return(40); 
   if (!strncmp (current_acronym, "AWOS", 4)) 
      return(41); 
   if (!strncmp (current_acronym, "Rec rainga", 10)) 
      return(46); 
   if (!strncmp (current_acronym, "PROFS", 5)) 
      return(49); 
   if (!strncmp (current_acronym, "Profiler", 8)) 
      return(50); 
   if (!strncmp (current_acronym, "CLASS", 5)) 
      return(53); 
   if (!strncmp (current_acronym, "NWS sonde", 9)) 
      return(54); 
   if (!strncmp (current_acronym, "USGS", 4)) 
      {
      printf ("WARNING: can't tell the diff tween USGS raingages and streamflow! (72,74, & 75)\n");
      return(74); 
      }

   if (!strncmp (current_acronym, "SAO", 3) || !strncmp (current_acronym, "NCDC", 4) ) 
      return(82); 
   if (!strncmp (current_acronym, "TVA", 3) ) 
      {
      printf ("WARNING: can't tell the diff tween TVA raingages and streamflow! (95 & 94)\n");
      return(95); 
      }
   else
      {
      printf ("Unknown platform type = %-s!!!\n", current_acronym);
      return(-1);
      }

   } /* determine_platform() */


/*-------------------------------------------------------
 * get_defaults.c - gets default values for stnrec
 *     initialization.
 *
 *  Input: uninitialized global default values.
 *     Input file with defaults (order specfic!!!)
 *     named "stn_info_defaults.inp".
 *
 *  Output: Global default values set. 
 *
 * WARNING: The name of the input file is assumed to be
 *   "stn_info_defaults.inp" and the order of this file
 *   is expected to be in the order of the STNREC structure.
 *
 * 000 10 May 94 lec
 *    Created.
 *-------------------------------------------------------*/
void get_defaults (void)
   {
   FILE    *defaults_input_stream;
   int     j = 0;
   char    junk[51] = "\0";
   int     junk_int;
   float   junk_float;
   char    c, junk_char;
   int     count = 0;
   int     items = 0;

 
   if (( defaults_input_stream = fopen("stn_info_defaults.inp", "r")) == NULL)
      perror ("Error: Can't open stn_info_defaults.inp");

  /*  printf ("Successfully opened stn_info_defaults.inp\n"); */

   /*
    * Construct a STNREC pointer. 
    * WHEN will this static ptr be freed?
    */
   def_stnptr = (STNREC *) malloc (sizeof(STNREC));
      
   if (def_stnptr == NULL)
      perror ("Error: Can't malloc STNREC space!");

  /*  printf ("Created def_stnptr\n"); */

   /*
    * Read the data. ORDER IS IMPORTANT IN INPUT FILE!!
    */
   count++;
   for (j=0; j<50; j++) junk[j] = '\0';

   j = -1;
   while((c=getc(defaults_input_stream))!='\n' && j< 15)
      {
      if(c==EOF || c=='\n')break;
      junk[++j] = c;
      } /* while */
   def_stnptr->project[14] = '\0';

   strncpy( def_stnptr->project, junk, 15);

   printf ("project: %-sxxx\n", def_stnptr->project);  

   count++;
   items = fscanf (defaults_input_stream, "%d", &junk_int );
   if (items != 1)
      {
      if ( !feof(defaults_input_stream))
         fprintf (stderr,
           "Error: Problem reading item %d from default_input_stream!\n",
           count);
      }
   else
     def_stnptr->stnid_int = junk_int;
   printf ("stnid_int: %d\n", def_stnptr->stnid_int); 

   STRIPLINE(defaults_input_stream);

   count++; 
   for (j=0; j<20; j++) junk[j] = '\0';

   items = fscanf (defaults_input_stream, "%s", junk); 
   if (items != 1)
      { 
      if ( !feof(defaults_input_stream)) 
         fprintf (stderr,
           "Error: Problem reading item %d from default_input_stream!\n", 
           count); 
      } 
   else
      strncpy( def_stnptr->stnid_ext, junk, 18);
   printf( "stnid_ext: %-sxxx\n",def_stnptr->stnid_ext);
   STRIPLINE(defaults_input_stream);

   count++; 
   items = fscanf (defaults_input_stream, "%f", &junk_float); 
   if (items != 1)
      { 
      if ( !feof(defaults_input_stream)) 
         fprintf (stderr,
           "Error: Problem reading item %d from default_input_stream!\n", 
           count); 
      } 
   else
      def_stnptr->lat = junk_float;

   printf( "lat: %9.5fxxx\n",def_stnptr->lat);
   STRIPLINE(defaults_input_stream);

   junk_float = 0.0;
   count++;  
   items = fscanf (defaults_input_stream, "%f", &junk_float);  
   if (items != 1)
      {  
      if ( !feof(defaults_input_stream))  
         fprintf (stderr,
           "Error: Problem reading item %d from default_input_stream!\n",  
           count); 
      }  
   else 
      def_stnptr->lon = junk_float;

   printf( "lon: %10.5fxxx\n",def_stnptr->lon);
   STRIPLINE(defaults_input_stream);

   count++;  
   items = fscanf (defaults_input_stream, "%d", &junk_int);  
   if (items != 1)
      {  
      if ( !feof(defaults_input_stream))  
         fprintf (stderr,
           "Error: Problem reading item %d from default_input_stream!\n",  
           count); 
      }  
   else 
      def_stnptr->occur = junk_int;

   printf( "occ: %dxxx\n",def_stnptr->occur);
   STRIPLINE(defaults_input_stream);
 
   count++;  
   items = fscanf (defaults_input_stream, "%d", &junk_int);  
   if (items != 1)
      {  
      if ( !feof(defaults_input_stream))  
         fprintf (stderr,
           "Error: Problem reading item %d from default_input_stream!\n",  
           count); 
      }  
   else 
      def_stnptr->accuracy = junk_int;

   printf( "accuracy: %dxxx\n",def_stnptr->accuracy);
   STRIPLINE(defaults_input_stream);

   count++;
   for (j=0; j<50; j++) junk[j] = '\0';

   j = -1;
   while((c=getc(defaults_input_stream))!='\n' && j< 50)
      {
      if(c==EOF || c=='\n')break;
      junk[++j] = c; 
      } /* while */ 

   strncpy( def_stnptr->name, junk, 50);

   printf ("name: %-sxxx\n", def_stnptr->name);

   count++;
   for (j=0; j<10; j++) junk[j] = '\0';

   items = fscanf (defaults_input_stream, "%s", junk);
   if (items != 1)
      {
      if ( !feof(defaults_input_stream))
         fprintf (stderr,
           "Error: Problem reading item %d from default_input_stream!\n",
           count);
      }
   else
      strncpy( def_stnptr->begin_date, junk, 8);

   printf ("begin_date: %-sxxx\n", def_stnptr->begin_date);
   STRIPLINE(defaults_input_stream);


   count++;
   for (j=0; j<10; j++) junk[j] = '\0';
 
   items = fscanf (defaults_input_stream, "%s", junk);
   if (items != 1) 
      { 
      if ( !feof(defaults_input_stream)) 
         fprintf (stderr, 
           "Error: Problem reading item %d from default_input_stream!\n", 
           count); 
      } 
   else 
      strncpy( def_stnptr->end_date, junk, 8); 

   printf ("end_date: %-sxxx\n", def_stnptr->end_date);
   STRIPLINE(defaults_input_stream);


   count++;
   for (j=0; j<3; j++) junk[j] = '\0';
 
   items = fscanf (defaults_input_stream, "%s", junk);
   if (items != 1) 
      { 
      if ( !feof(defaults_input_stream)) 
         fprintf (stderr, 
           "Error: Problem reading item %d from default_input_stream!\n", 
           count); 
      } 
   else 
      strncpy( def_stnptr->country, junk, 2); 

   printf ("country: %-sxxx\n", def_stnptr->country);
   STRIPLINE(defaults_input_stream);

   count++;
   for (j=0; j<3; j++) junk[j] = '\0';
 
   items = fscanf (defaults_input_stream, "%s", junk); 
   if (items != 1) 
      {  
      if ( !feof(defaults_input_stream)) 
         fprintf (stderr,  
           "Error: Problem reading item %d from default_input_stream!\n",  
           count); 
      } 
   else  
      strncpy( def_stnptr->state, junk, 2);  

   printf ("state: %-sxxx\n", def_stnptr->state);
   STRIPLINE(defaults_input_stream);

   count++;
   for (j=0; j<5; j++) junk[j] = '\0';
 
   items = fscanf (defaults_input_stream, "%s", junk); 
   if (items != 1) 
      {  
      if ( !feof(defaults_input_stream)) 
         fprintf (stderr,  
           "Error: Problem reading item %d from default_input_stream!\n",  
           count); 
      } 
   else  
      strncpy( def_stnptr->county, junk, 3);  

   printf ("county: %-sxxx\n", def_stnptr->county);
   STRIPLINE(defaults_input_stream);


   count++;
   items = fscanf (defaults_input_stream, "%f", &junk_float);
   if (items != 1)
      {
      if ( !feof(defaults_input_stream))
         fprintf (stderr,
           "Error: Problem reading item %d from default_input_stream!\n",
           count);
      }
   else 
      def_stnptr->time_zone = junk_float;

   printf ("time_zone: %5.2fxxx\n", def_stnptr->time_zone);
   STRIPLINE(defaults_input_stream);

   count++;
   items = fscanf (defaults_input_stream, "%1c", &junk_char);
   if (items != 1)
      {
      if ( !feof(defaults_input_stream))
         fprintf (stderr,
           "Error: Problem reading item %d from default_input_stream!\n",
           count);
      }
   else 
      def_stnptr->dst_switch = junk_char;

   printf ("dst_switch: %c\n", def_stnptr->dst_switch);
   STRIPLINE(defaults_input_stream);

   count++;
   items = fscanf (defaults_input_stream, "%d", &junk_int);
   if (items != 1)
      {
      if ( !feof(defaults_input_stream))
         fprintf (stderr,
           "Error: Problem reading item %d from default_input_stream!\n",
           count);
      }
   else
      def_stnptr->platform = junk_int;

   printf( "platform: %dxxx\n",def_stnptr->platform);

   STRIPLINE(defaults_input_stream);

   count++;
   for (j=0; j<20; j++) junk[j] = '\0';

   j = -1;
   while((c=getc(defaults_input_stream))!='\n' && j< 15)
      {
      if(c==EOF || c=='\n')break;
      junk[++j] = c;
      } /* while */

   strncpy( def_stnptr->frequency, junk, 15);

   printf ("frequency: %-sxxx\n", def_stnptr->frequency);


   count++;
   items = fscanf (defaults_input_stream, "%f", &junk_float);
   if (items != 1)
      {
      if ( !feof(defaults_input_stream))
         fprintf (stderr,
           "Error: Problem reading item %d from default_input_stream!\n",
           count);
      }
   else 
      def_stnptr->elev = junk_float;

   printf ("elev: %7.1fxxx\n", def_stnptr->elev);
   STRIPLINE(defaults_input_stream);

   count++;
   items = fscanf (defaults_input_stream, "%1c", &junk_char); 
   if (items != 1) 
      { 
      if ( !feof(defaults_input_stream)) 
         fprintf (stderr,
           "Error: Problem reading item %d from default_input_stream!\n",
           count);
      } 
   else 
      def_stnptr->fixed_mobile = junk_char;

   printf ("fixed_mobile: %c\n", def_stnptr->fixed_mobile);
   STRIPLINE(defaults_input_stream);

  printf ("Close defaults_input_stream\n");

   /*
    * Close input stream.
    */
   if (fclose (defaults_input_stream) == EOF)
      perror ("Can't close defaults_input_stream");

  /*  printf ("Exit get_def!\n"); */

   } /* get_defaults() */

/*-------------------------------------------------------
 * date1_gt_date2.c - Returns 1 if date1 greater than date2,
 *  returns zero otherwise.
 *
 *  Input: date1 and date2 are the string form of dates
 *         hanging off a stnptr.
 *
 *-------------------------------------------------------*/
int date1_gt_date2 ( /*in*/ char   date1[9],
                     /*in*/ char   date2[9])
   {
   int  yr1 = 0;
   int  mo1 = 0;
   int  day1 = 0;

   int  yr2 = 0;
   int  mo2 = 0;
   int  day2 = 0;

   char junk_str[5] = "\0\0\0\0\0";

   /* printf ("date1, date2: %-sxxx %-sxxx\n", date1, date2);  */

   if (!strncmp(date1, date2)) /* equal */
      {
      /* printf ("dates equal\n"); */
      return(0);
      }
 
   strncpy (junk_str, date1, 4);
   yr1 = atoi (junk_str);

   strncpy (junk_str, date2, 4);       
   yr2 = atoi (junk_str);
         
   /*printf ("yr1, yr2: %d %d\n", yr1, yr2);  */
   if (yr1 > yr2) 
      return(1);

   if (yr1 < yr2)
      return(0);

   strcpy (junk_str, "\0\0\0\0\0");

   strncpy (junk_str, &date1[4], 2);       
   junk_str[2] = '\0';
   mo1 = atoi (junk_str);

   strncpy (junk_str, &date2[4], 2);        
   junk_str[2] = '\0';
   mo2 = atoi (junk_str); 


   /*printf ("mo1, mo2: %d %d\n", mo1, mo2);  */
   if (mo1 > mo2)
      return(1);

   if (mo1 < mo2)
      return(0);

   strcpy (junk_str, "\0\0\0\0\0"); 
 
   strncpy (junk_str, &date1[6], 2);        
   junk_str[2] = '\0';
   day1 = atoi (junk_str); 
 
   strncpy (junk_str, &date2[6], 2);         
   junk_str[2] = '\0';
   day2 = atoi (junk_str);

   /*printf ("day1, day2: %d %d\n", day1, day2);  */

   if (day1 > day2) 
      return(1);

   if (day1 < day2) 
      return(0); 

   } /* date1_gt_date2() */



/*-------------------------------------------------------
 * stns_equal.c - Returns 1 if stns are equal, returns
 *  zero otherwise.
 *
 *  Input: instnptr1 and instnptr2 are the pointer to a
 *         structure of type stnrec.
 *
 *-------------------------------------------------------*/
int stns_equal ( /*in*/     STNREC *instnptr1,
                 /*in/out*/ STNREC *instnptr2)
   {
   if (strncmp( instnptr2->project, instnptr1->project, 16)) 
      return(0);

   if ( instnptr2->stnid_int != instnptr1->stnid_int)
      return(0);

   if (strncmp (instnptr2->stnid_ext, instnptr1->stnid_ext,19))
      return(0);

   if (inrange(instnptr2->lat, instnptr1->lat,0)) return(0);
   if (inrange(instnptr2->lon, instnptr1->lon,0)) return(0);
 
   if (instnptr2->occur != instnptr1->occur) return(0);
 
   if (instnptr2->accuracy != instnptr1->accuracy) return(0);
 
   if (strncmp( instnptr2->name, instnptr1->name, 51)) return(0);
 
   if (strncmp( instnptr2->begin_date, instnptr1->begin_date, 9)) return(0);
   if (strncmp( instnptr2->end_date, instnptr1->end_date, 9)) return(0);
 
   if (strncmp( instnptr2->country, instnptr1->country, 3)) return(0);
   if (strncmp( instnptr2->state, instnptr1->state, 3)) return(0);
   if (strncmp( instnptr2->county, instnptr1->county, 4))return(0);
 
   if (instnptr2->time_zone != instnptr1->time_zone) return(0);
 
   if (instnptr2->dst_switch != instnptr1->dst_switch) return(0);
 
   if (instnptr2->platform = instnptr1->platform) return(0);

   if (strncmp( instnptr2->frequency, instnptr1->frequency, 16)) return(0);

   if (instnptr2->elev != instnptr1->elev) return(0); /* do inrange here? */

   if (instnptr2->fixed_mobile != instnptr1->fixed_mobile) return(0);

   return(1);

   } /* stns_equal() */

/*-------------------------------------------------------
 * copy_stn.c - Copies instn to outstn. This fn can
 *  replace reset_stns.
 *
 *  Input: instnptr and outstnptr are the pointer to a 
 *         structure of type stnrec.
 *
 *  Output: outstnptr.
 *
 *-------------------------------------------------------*/
void copy_stn ( /*in*/     STNREC *instnptr,
                /*in/out*/ STNREC *outstnptr)
   {
   strncpy( outstnptr->project, instnptr->project, 16);

   outstnptr->stnid_int = instnptr->stnid_int;
   strncpy ( outstnptr->stnid_ext, instnptr->stnid_ext,19);

   outstnptr->lat = instnptr->lat;                         
   outstnptr->lon = instnptr->lon;

   outstnptr->occur = instnptr->occur;

   outstnptr->accuracy = instnptr->accuracy;

   strncpy( outstnptr->name, instnptr->name, 51);

   strncpy( outstnptr->begin_date, instnptr->begin_date, 9);
   strncpy( outstnptr->end_date, instnptr->end_date, 9);

   strncpy( outstnptr->country, instnptr->country, 3);
   strncpy( outstnptr->state, instnptr->state, 3);
   strncpy( outstnptr->county, instnptr->county, 4);

   outstnptr->time_zone = instnptr->time_zone;       
 
   outstnptr->dst_switch = instnptr->dst_switch;

   outstnptr->platform = instnptr->platform;     
 
   strncpy( outstnptr->frequency, instnptr->frequency, 16);
 
   outstnptr->elev = instnptr->elev;
 
   outstnptr->fixed_mobile = instnptr->fixed_mobile;
 
   } /* copy_stn() */
 
/*-------------------------------------------------------
 * reset_stnrec.c - reinitializes the input stnrec.
 *
 *  Input: stnptr is the pointer to a structure of
 *         type stnrec.
 *
 *  Output: stnptr which has been reinitialized.
 *
 *-------------------------------------------------------*/
void reset_stnrec ( /*in/out*/  STNREC *stnptr )
   {

   strncpy( stnptr->project, def_stnptr->project, 16);

   stnptr->stnid_int = def_stnptr->stnid_int;
   strncpy (stnptr->stnid_ext, def_stnptr->stnid_ext,19);
 
   stnptr->lat = def_stnptr->lat;
   stnptr->lon = def_stnptr->lon;
 
   stnptr->occur = def_stnptr->occur;
 
   stnptr->accuracy = def_stnptr->accuracy;
 
   strncpy( stnptr->name, def_stnptr->name, 51);
 
   strncpy( stnptr->begin_date, def_stnptr->begin_date, 9);
   strncpy( stnptr->end_date, def_stnptr->end_date, 9);
 
   strncpy( stnptr->country, def_stnptr->country, 3);
   strncpy( stnptr->state, def_stnptr->state, 3);
   strncpy( stnptr->county, def_stnptr->county, 4);
 
   stnptr->time_zone = def_stnptr->time_zone;

   stnptr->dst_switch = def_stnptr->dst_switch;
   
   stnptr->platform = def_stnptr->platform;

   strncpy( stnptr->frequency, def_stnptr->frequency, 16);

   stnptr->elev = def_stnptr->elev;

   stnptr->fixed_mobile = def_stnptr->fixed_mobile;

   } /* reset_stnrec() */



/*--------------------------------------------------------
 * write_stations_rec.c - writes a record to the 'stations'
 *    output file.
 *
 * Input: output_stream is pointer to stations file.
 *        Data to be written out.
 *
 * Output:         
 *
 * 09 May 94 lec
 *   Created.
 *-------------------------------------------------------*/
void write_stations_rec ( /*in/out*/ FILE    *stations_output_stream, 
                          /*in*/     STNREC  *stnptr )
   {
  /*  printf ("Writing station rec\n"); */
  /* printf ("beg, end: %-sxxx %-sxxx\n", stnptr->begin_date, stnptr->end_date); */

   fprintf( stations_output_stream, 
    "%-15s %10d %10.5f %11.5f %3d %5d %-50s %-8s %-8s %-2s %-2s %-3s %6.2f %-1c %4d %-15s %9.1f %1c\n",
            stnptr->project,  
            stnptr->stnid_int, 
            stnptr->lat,   
            stnptr->lon, 
            stnptr->occur, 
            stnptr->accuracy,
            stnptr->name,  
            stnptr->begin_date, 
            stnptr->end_date,    
            stnptr->country, 
            stnptr->state, 
            stnptr->county,
            stnptr->time_zone, 
            stnptr->dst_switch,
            stnptr->platform,
            stnptr->frequency, 
            stnptr->elev,
            stnptr->fixed_mobile);

   } /* write_stations_rec() */

/*--------------------------------------------------------
 * write_stn_id_rec.c - writes a record to the 'stn_id'
 *    output file.
 *
 * Input: output_stream is pointer to stn_id file.
 *        Data to be written out.
 *
 * Output:   
 *
 * 09 May 94 lec
 *   Created.
 *-------------------------------------------------------*/
void write_stn_id_rec ( /*in/out*/ FILE    *stn_id_output_stream,
                        /*in*/     STNREC  *stnptr )
   {
  /*  printf ("Writing stn_id rec!\n"); */

  /*  printf ("project: %-sxxx\n", stnptr->project);
   printf ("stnid_int: %dxxx\n", stnptr->stnid_int);
   printf ("stnid_ext: %-sxxx\n", stnptr->stnid_ext); */

   if (strlen (stnptr->stnid_ext) > 18)
      fprintf( stn_id_output_stream,
            "%-15s %10d %-18s\n",
            stnptr->project,
            stnptr->stnid_int,
            stnptr->stnid_ext);
   else
      fprintf( stn_id_output_stream,
            "%-15s %10d %-s\n",
            stnptr->project,
            stnptr->stnid_int,
            stnptr->stnid_ext);

   } /* write_stn_id_rec() */
