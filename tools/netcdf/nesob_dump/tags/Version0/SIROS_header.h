/*----------------------------------------------------------
 NESOB_qcfrec.h - This is the header file containing the 
 *            Quality Control Format (qcf) record definition,
 *            and the station record definition.
 *---------------------------------------------------------*/
#ifndef NESOB_QCFREC_H
#define NESOB_QCFREC_H

#define CHANNEL     6
#define NUM_OF_STATIONS  25

typedef struct qcfrec {
    int year_nom;                               /* Nominal year for date of observation */
    int month_nom;                              /* Nominal month for date of observation */
    int day_nom;                                /* Nominal day for date of observation */
    int hour_nom;                               /* Nominal hour of observation */
    int minute_nom;                             /* Nominal minute of observation */
    int second_nom;                             /* Nominal second of observation */
    int year;                                   /* Observation year for date of observation */
    int month;                                  /* Observation month for date of observation */
    int day;                                    /* Observation day for date of observation */
    int hour;                                   /* Observation hour of observation */
    int minute;                                 /* Observation minute of observation */
    int second;                                 /* Observation second of observation */
    char qnet[11];                              /* network identifier (platform abbrev.) */
    char statn[15];                             /* station identifier */
    float lat;                                  /* station latitude */
    float lon;                                  /* station longitude */
    int occur;                                  /* station occurence */
    float staelv;                               /* station elevation in meters */
    float channel[CHANNEL];                     /* channel frequency */
    float hemisp_narrowband[CHANNEL];           /* Hemispheric Irradiance */
    float diffuse_hemisp_narrowband[CHANNEL];   /* Diffuse Hemispheric Irradiance */
    float direct_norm_narrowband[CHANNEL];      /* Direct Normal Irradiance */
    float flag_zero_divisor[CHANNEL];           /* Divide by zero error flag */
    float flag_ln_error[CHANNEL];               /* Natural log error flag */
    float flag_zero_cosine[CHANNEL];            /* Zero cosine error flag */
    float flag_nighttime[CHANNEL];              /* Night time error flag */
    float hemisp_broadband;                     /* Hemispheric Irradiance */
    float diffuse_hemisp_broadband;             /* Diffuse Hemispheric Irradiance */
    float direct_norm_broadband;                /* Direct Normal Irradiance */
    float mfrsr_temp;                           /* MFRSR Detector Temperature */
    float up_long_hemisp;                       /* 10 meter Upwelling Longwave Hemispheric Irradiance, Pyrgeometer */
    float up_long_dome_temp;                    /* 10 meter Longwave Dome Temperature, Pyrgeometer */
    float up_long_case_temp;                    /* 10 meter Longwave Case Temperature, Pyrgeometer */
    float down_long_diffuse_hemisp;             /* Downwelling Longwave Diffuse Hemispheric Irradiance, Ventilated Pyrgeometer */
    float down_long_dome_temp;                  /* Ventilated Pyrgeometer Dome Temperature */
    float down_long_case_temp;                  /* Ventilated Pyrgeometer Case Temperature */
    float up_short_hemisp;                      /* 10 meter Upwelling Shortwave Hemispheric Irradiance, Pyranometer */
    float down_short_hemisp;                    /* Downwelling Shortwave Hemispheric Irradiance, Ventilated Pyranometer */
    float down_short_diffuse_hemisp;            /* Downwelling Shortwave Diffuse Hemispheric Irradiance, Ventilated Pyranometer */
    float short_direct_normal;                  /* Shortwave Direct Normal Irradiance, Pyrheliometer */
    float therm_volt;                           /* Thermistor Excitation Voltage, Data Logger */
    float logger_volt;                          /* Data Logger Supply Voltage */
    float logger_temp;                          /* Data Logger Temperature */
} QCFREC; 


         /*--------------------------------------  
          * NESOB 
          *-------------------------------------*/

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
    int   platform;      /* Int ptr to db table relating ints to names/acronyms */
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

#endif /* NESOB_QCFREC_H */
