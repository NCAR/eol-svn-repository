/* $Id$ */
/*
 * $Log$
 *
 */

/*----------------------------------------------------------
 * process_stn_data - This is the header file containing the 
 *     function definitions for process_stn_data module. 
 *     The fns included here write out the stations file
 *     and the stn_id file. These files are input into 
 *     the CODIAC system. The stations file is analogous
 *     to the master stn list for a project.
 *
 * 09 May 94 lec
 *   Created.
 *---------------------------------------------------------*/
#ifndef PROCESS_STN_DATA
#define PROCESS_STN_DATA

#include "local.h"


#define STNREC struct stnrec
STNREC
    { 
    char  project[16];   /* Project with which data is associated (e.g., VORTEX, STORMFEST).*/
    int   stnid_int;     /* Station id internal to database system.*/
    char  stnid_ext[19]; /* Station id external to DB system. Called id_num in stn-id table.*/
    float lat;           /* Station latitude  */ 
    float lon;           /* Station longitude */
    int   occur;         /* Station occurence */ 
    int   accuracy;      /* Accuracy of lat and lon values. */
    char  name[51];      /* 50 char name of station. */
    char  begin_date[9]; /* Begin date of stns period of coverage. YYYYMMDD */
    char  end_date[9];   /* End date of stns period of coverage. YYYYMMDD   */
    char  country[3];    /* Country stn is located within. */
    char  state[3];      /* State stn is located within.   */
    char  county[4];     /* County stn is located within.  */
    float time_zone;     /* Time zone stn is located within - Offset from GMT */
    char  dst_switch;    /* Daylight Savings Time indicator. 'Y' or 'N' */
    int   platform;      /* Int ptr to db table relating ints to names/acronomys */
    char  frequency[15]; /* Frequency of observation */
    float elev;          /* stn elevation */
    char  fixed_mobile;  /* Flag indicating is stn mobile. 'f' or 'm' */
};
/* following struct not used! */

#define PLTFRMREC struct pltfrmrec
PLTFRMREC
    { 
    int   platform_id;   /* Platform number internal to database system.*/
    char  name[31];      /* Platform name */
    char  desc[61];      /* Short description.*/
    char  acronym[10];   /* Acronym for previous name. */
    };


#ifdef __STDC__
   extern int  inrange( /*in*/ float x_int,
                        /*in*/ float y_int,
                        /*in*/ int range);

   extern int  locate_platform( /*in*/  char  current_acronym[11]);

   extern void read_old_stn_info( /*in*/  char   input_file_name[MAX_CHARS],
                                  /*out*/ long   *num_stns);

   extern void read_new_stn_info( /*in*/  char   input_file_name[MAX_CHARS],
                                  /*out*/ long   *num_stns);

   extern int  locate_stn_in_old_master_list( /*in*/  char  stnid_ext[19],
                                              /*in*/  long  num_stns,
                                              /*in/out*/ STNREC *stnptr);

   extern int  locate_stn_in_new_master_list( /*in*/  char  stnid_ext[19],
                                              /*in*/  long  num_stns,
                                              /*in/out*/ STNREC *stnptr);
   extern void get_defaults (void);
   extern void copy_stn ( /*in*/      STNREC *instnptr,
                          /*in/out*/  STNREC *outstnptr );

   extern int  stns_equal ( /*in*/     STNREC *instnptr1,
                           /*in/out*/ STNREC *instnptr2);

   extern int date1_gt_date2 ( /*in*/ char   date1[9],
                               /*in*/ char   date2[9]);

   extern void reset_stnrec ( /*in/out*/  STNREC *stnptr );

   extern void write_stations_rec( /*in/out*/ FILE    *stations_output_stream,
                                   /*in*/     STNREC  *stnptr );
   
   extern void write_stn_id_rec( /*in/out*/ FILE    *stn_id_output_stream,
                                 /*in*/     STNREC  *stnptr );

#else /*!__STDC__*/
   extern int  locate_platform();
   extern int  inrange();
   extern void read_old_stn_info();
   extern void read_new_stn_info();
   extern int  locate_stn_in_old_master_list();
   extern int  locate_stn_in_new_master_list();

   extern void get_defaults ();
   extern void copy_stn ();
   extern int  stns_equal ();
   extern int date1_gt_date2 ();
   extern void reset_stnrec ();

   extern void write_stations_rec();
   extern void write_stn_id_rec();
 
#endif /*__STDC__*/

#endif /* PROCESS_STN_DATA */
