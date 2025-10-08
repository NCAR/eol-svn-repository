/*----------------------------------------------------------
 stnrec.h - This is the header file containing  
 *          the station record definition.
 *---------------------------------------------------------*/

#ifndef STNREC_H
#define STNREC_H

#define NUM_OF_STATIONS  25

typedef struct stnrec { 
    char  project[16];   /* Project with which data is associated (e.g., VORTEX, STORMFEST).*/
    int   stnid_int;     /* Station id internal to database system.*/
    int   id_type;       /* Type of Station id. Indicates origin of stnid_ext(e.g. WBAN#, NWS, etc.).*/
    char  stnid_ext[19]; /* Station id external to DB system. Called id_num in stn-id table.*/
    float lat;           /* Station latitude  */ 
    float lon;           /* Station longitude */
    int   occur;         /* Station occurence */ 
    int   accuracy;      /* Accuracy of lat and lon values. */
    char  name[48];      /* 48 char name of station. Commission indicator fills to 50 chars.*/
    char  comm_code[4];  /* Commission indicator set to (C) if stn commissioned, else (N).*/
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
} STNREC;
                           
void station_rec__initialize(QCFREC *qcfptr, STNREC *def_stnptr, int *id_num);
void station_rec__set_time(QCFREC *qcfptr, STNREC *stnptr); 
void station_rec__write(STNREC station_array[], int num_stations);
void write_stations_rec(FILE *stations_output_stream, STNREC *stnptr);
void write_CD_stations_rec(FILE *CD_stations_output_stream, STNREC *stnptr);
void write_stn_id_rec(FILE *stn_id_output_stream, STNREC *stnptr);

#endif /* STNREC_H */

