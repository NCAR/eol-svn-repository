/*----------------------------------------------------------
 * qcfrec.h - This is the header file containing the 
 *            Quality Control Format (qcf) record definition.
 *---------------------------------------------------------*/
#ifndef QCFREC_H
#define QCFREC_H

#define QCFREC struct qcfrec 
QCFREC
    {
    int year_nom;    /* !Nominal year for date of observation */
    int month_nom;   /* !Nominal month for date of observation */
    int day_nom;     /* !Nominal day for date of observation */
    int hour_nom;    /* !Nominal hour of observation */
    int minute_nom;  /* !Nominal minute of observation */
    int year;        /* !Observation year for date of observation */
    int month;       /* !Observation month for date of observation */
    int day;         /* !Observation day for date of observation */
    int hour;        /* !Observation hour of observation */
    int minute;      /* !Observation minute of observation */
    char qnet[11];   /* !network identifier (platform abbrev.) */
    char statn[16];  /* !station identifier */
    float lat;       /* !station latitude */
    float lon;       /* !station longitude */
    int occur;       /* !station occurence */
    float staelv;    /* !station elevation in meters */
    float staprs;    /* !station pressure in mb */
    char staflg;     /* !qc flag for station pressure */
    float seaprs;    /* !sea level pressure in mb */
    char seaflg;     /* !qc flag for sea level pressure */
    float cmpsea;    /* !computed sea level pressure in mb */
    char cmpflg;     /* !qc flag for computed sea level pressure */
    float temp;      /* !dry bulb temperature in Celsius */
    char tmpflg;     /* !qc flag for dry bulb temperature */
    float dewpnt;    /* !dew point temperature in Celsius */
    char dewflg;     /* !qc flag for dew point */
    float wndspd;    /* !wind speed in meters/second */
    char spdflg;     /* !qc flag for wind speed */
    float wnddir;    /* !wind direction in degrees azimuth */
    char dirflg;     /* !qc flag for wind direction */
    float precip;    /* !total precipitation in mm */
    char prcflg;     /* !qc flag for total precipitation */
    char sg;         /* !squall/gust indicator */
    float squall;    /* !squall/gust speed in meters/second */
    char sqlflg;     /* !qc flag for squall/gust speed */
    int prswea;      /* !present weather code value */
    char pwflg;      /* !qc flag for present weather */
    float visib;     /* !visibility in meters */
    char visflg;     /* !qc flag for visibility */
    float celht1;    /* !ceiling height (first layer)- hundreds of ft */
    int celfg1;      /* !ceiling flag (first layer) code value */
    char c1flg;      /* !qc flag for ceiling height/flag */
    int clamt1;      /* !cloud amount (first layer) code value */
    char ca1flg;     /* !qc flag for cloud amount */
    float celht2;    /* !ceiling height (second layer)- hundreds of ft */
    int celfg2;      /* !ceiling flag (second layer) code value */
    char c2flg;      /* !qc flag for ceiling height/flag */
    int clamt2;      /* !cloud amount (second layer) code value */
    char ca2flg;     /* !qc flag for cloud amount */
    float celht3;    /* !ceiling height (third layer)- hundreds of ft */
    int celfg3;      /* !ceiling flag (third layer) code value */
    char c3flg;      /* !qc flag for ceiling height/flag */
    int clamt3;      /* !cloud amount (third layer) code value */
    char ca3flg;     /* !qc flag for cloud amount */
}; 

#endif /* QCFREC_H */
