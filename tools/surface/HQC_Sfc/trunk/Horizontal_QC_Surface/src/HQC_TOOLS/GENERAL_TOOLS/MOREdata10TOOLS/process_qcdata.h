/* $Id$ */
/*
 * $Log$
 *
 */

/*----------------------------------------------------------
 * process_qcdata - This is the header file containing the 
 *     function definitions for process_qcdata module. All data
 *     types which can be processed are defined here.
 *
 * 09 May 94 lec
 *   Created.
 *---------------------------------------------------------*/
#ifndef PROCESS_QCDATA
#define PROCESS_QCDATA

#include "local.h"
#include "process_stn_data.h"

enum QC_data_types  {
                     qcf,       /* standard quality control (QC) format  - surface (nom and obs times)*/
                     pqcf,      /* precip QC format       */
                     dpqcf,     /* daily precip QC format */
                     usgs_sqcf, /* USGS hourly streamflow QC format */
                     tva_sqcf,  /* TVA hourly (bi-hrly data) streamflow QC format */
                     spec_qcf,  /* NCDC specials (QC) format - only obs time.*/
                     UNKNOWN
                    } current_data_type;


#ifdef __STDC__

   extern void read_record( /*in/out*/ FILE       **data_stream,
                            /*out*/    char       new_line[MAX_CHARS]);

   extern void process_qcfrec( /*in*/     char   new_line[MAX_CHARS],
                               /*in*/     long   num_new_stns,
                               /*in*/     long   num_old_stns,
                               /*in*/     long   current_stnid_ct,
                               /*in*/     char   metadata_input_file_name[MAX_CHARS],
                               /*in/out*/ STNREC *stnptr,
                               /*in/out*/ int    *write_recs);

   extern void process_qcf_metadata();

   extern void process_pqcfrec( /*in*/     char   new_line[MAX_CHARS],
                                /*in*/     long   num_stns,
                                /*in*/     long   current_stnid_ct,
                                /*in*/     char   metadata_input_file_name[NAMELEN],
                                /*in/out*/ STNREC *stnptr);

   extern void process_pqcf_metadata();

   extern void process_dpqcfrec(  /*in*/     char   new_line[MAX_CHARS],
                                  /*in*/     long   num_new_stns,
                                  /*in*/     long   num_old_stns,
                                  /*in*/     long   num_meta_stns,
                                  /*in*/     long   current_stnid_ct,
                                  /*in*/     char   metadata_input_file_name[MAX_CHARS],
                                  /*in/out*/ STNREC *stnptr,
                                  /*in/out*/ int    *write_recs);

   extern void process_dpcf_metadata(/*in*/char input_file_name[MAX_CHARS],
                                     /*in/out*/ long *num_meta_stns);

   extern void process_usgs_sqcfrec( /*in*/     char   new_line[MAX_CHARS],
                           /*in*/     long   num_stns,
                           /*in*/     long   num_meta_stns,
                           /*in*/     long   current_stnid_ct,
                           /*in*/     char metadata_input_file_name[MAX_CHARS],
                           /*in/out*/ STNREC *stnptr);

   extern void process_usgs_sqcf_metadata(/*in*/char input_file_name[MAX_CHARS],
                                          /*in/out*/ long *num_meta_stns);

   extern void process_tva_sqcfrec( /*in*/     char   new_line[MAX_CHARS],
                                    /*in*/     long   num_stns,
                                    /*in*/     long   current_stnid_ct,
                                    /*in*/     char   metadata_input_file_name[NAMELEN],
                                    /*in/out*/ STNREC *stnptr);

   extern void process_tva_sqcf_metadata(/*in*/     char metadata_input_file_name[MAX_CHARS],
                                         /*in/out*/ long *num_meta_stns);

   extern void process_spec_qcfrec( /*in*/     char   new_line[MAX_CHARS],
                                    /*in*/     long   num_new_stns,
                                    /*in*/     long   num_old_stns,
                                    /*in*/     long   current_stnid_ct,
                                    /*in*/     char   metadata_input_file_name[MAX_CHARS],
                                    /*in/out*/ STNREC *stnptr,
                                    /*in/out*/ int    *write_recs);

   extern int locate_stn_in_metadata_by_latlon (/*in*/char stnid_ext[19], 
                                                /*in*/int num_meta_stns, 
                                                /*in/out*/ STNREC  *stnptr);

   extern int locate_stn_in_metadata_by_ID (/*in*/char stnid_ext[19],
                                            /*in*/int num_meta_stns,
                                            /*in/out*/ STNREC  *stnptr);

#else /*!__STDC__*/

   extern void read_record();

   extern void process_qcfrec ();
   extern void process_qcf_metadata();

   extern void process_pqcfrec();
   extern void process_pqcf_metadata();
 
   extern void process_dpqcfrec();
   extern void process_dpqcf_metadata();
 
   extern void process_usgs_sqcfrec();
   extern void process_usgs_sqcf_metadata();
 
   extern void process_tva_sqcfrec();
   extern void process_tva_sqcf_metadata();
   extern void process_spec_qcfrec();
 
   extern int locate_stn_in_metadata ();
   extern int locate_stn_in_metadata_by_ID ();

#endif /*__STDC__*/

#endif /* PROCESS_QCDATA */
