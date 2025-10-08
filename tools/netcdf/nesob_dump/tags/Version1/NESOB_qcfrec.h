/*----------------------------------------------------------
 * NESOB_qcfrec.h - This is the header file containing the 
 *            Quality Control Format (qcf) record definition
 *            for Heat Flux data
 *---------------------------------------------------------*/
#ifndef NESOB_QCFREC_H 
#define NESOB_QCFREC_H 

#define NUMDIMS  1
#define NUMVARS  70

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
    int occur;                                  /* station occurence */ 
    int base_time;                              /* Base time in Epoch */ 
    double time_offset;                         /* Time offset from base_time */ 
    float tref;                                 /* Reference temperature */ 
    float tair_top;                             /* Top air temperature */ 
    float tair_bot;                             /* Bottom air temperature */ 
    float thum_top;                             /* Temperature of the top humidity sensor chamber */ 
    float thum_bot;                             /* Temperature of the bottom humidity sensor chamber */ 
    float hum_top;                              /* Top relative humidity */ 
    float hum_bot;                              /* Bottom relative humidity */ 
    float vp_top;                               /* Top vapor pressure */ 
    float vp_bot;                               /* Bottom vapor pressure */ 
    float q;                                    /* Net radiation */ 
    float pres;                                 /* Atmospheric pressure */ 
    float sm1;                                  /* Soil moisture 1 (mass water/mass dry soil) */ 
    float sm2;                                  /* Soil moisture 2 (mass water/mass dry soil) */ 
    float sm3;                                  /* Soil moisture 3 (mass water/mass dry soil) */ 
    float sm4;                                  /* Soil moisture 4 (mass water/mass dry soil) */ 
    float sm5;                                  /* Soil moisture 5 (mass water/mass dry soil) */ 
    float ts1;                                  /* Soil temperature 1 */ 
    float ts2;                                  /* Soil temperature 2 */ 
    float ts3;                                  /* Soil temperature 3 */ 
    float ts4;                                  /* Soil temperature 4 */ 
    float ts5;                                  /* Soil temperature 5 */ 
    float shf1;                                 /* Soil heat flow 1 */ 
    float shf2;                                 /* Soil heat flow 2 */ 
    float shf3;                                 /* Soil heat flow 3 */ 
    float shf4;                                 /* Soil heat flow 4 */ 
    float shf5;                                 /* Soil heat flow 5 */ 
    float c_shf1;                               /* Corrected soil heat flow 1 */ 
    float c_shf2;                               /* Corrected soil heat flow 2 */ 
    float c_shf3;                               /* Corrected soil heat flow 3 */ 
    float c_shf4;                               /* Corrected soil heat flow 4 */ 
    float c_shf5;                               /* Corrected soil heat flow 5 */ 
    float cs1;                                  /* Soil heat capacity 1 */ 
    float cs2;                                  /* Soil heat capacity 2 */ 
    float cs3;                                  /* Soil heat capacity 3 */ 
    float cs4;                                  /* Soil heat capacity 4 */ 
    float cs5;                                  /* Soil heat capacity 5 */ 
    float ces1;                                 /* Change in energy storage 1 */ 
    float ces2;                                 /* Change in energy storage 2 */ 
    float ces3;                                 /* Change in energy storage 3 */ 
    float ces4;                                 /* Change in energy storage 4 */ 
    float ces5;                                 /* Change in energy storage 5 */ 
    float g1;                                   /* Soil heat flow at the surface 1 */ 
    float g2;                                   /* Soil heat flow at the surface 2 */ 
    float g3;                                   /* Soil heat flow at the surface 3 */ 
    float g4;                                   /* Soil heat flow at the surface 4 */ 
    float g5;                                   /* Soil heat flow at the surface 5 */ 
    float ave_shf;                              /* Average soil heat flow at the surface */ 
    float bowen;                                /* Bowen ratio */ 
    float e;                                    /* Latent heat flux */ 
    float h;                                    /* Sensible heat flux */ 
    float wind_s;                               /* Scalar Wind speed */ 
    float res_ws;                               /* Resultant wind speed */ 
    float wind_d;                               /* Wind direction (relative to true north) */ 
    float sigma_wd;                             /* Standard deviation of wind direction (sigma theta) */ 
    float home_15;                              /* Exchange Mechanism Position Indicator 0 to 15 mins */ 
    float home_30;                              /* Exchange Mechanism Position Indicator 15 to 30 mins */ 
    float qcmin1-24;                            /* Flag fields with values below minimum thresholds */ 
    float qcmax1-24;                            /* Flag fields with values above maximum thresholds */ 
    float qcdelta1-24;                          /* Flag fields with deltas (field[N] - field[N-1]) above delta thresholds */ 
    float qcmin25-48;                           /* Flag fields with values below minimum thresholds */ 
    float qcmax25-48;                           /* Flag fields with values above maximum thresholds */ 
    float qcdelta25-48;                         /* Flag fields with deltas (field[N] - field[N-1]) above delta thresholds */ 
    float qcmin49-72;                           /* Flag fields with values below minimum thresholds */ 
    float qcmax49-72;                           /* Flag fields with values above maximum thresholds */ 
    float qcdelta49-72;                         /* Flag fields with deltas (field[N] - field[N-1]) above delta thresholds */ 
    float lat;                                  /* north latitude */ 
    float lon;                                  /* east longitude */ 
    float alt;                                  /* altitude */ 
} QCFREC;

#endif /* NESOB_QCFREC_H */