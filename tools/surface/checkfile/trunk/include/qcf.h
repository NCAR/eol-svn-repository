/*---------------------------------------------------------------------
** File /home/lia/convert_data/okmeso/include/qcf.h
**
** Modified 10/17/2000 JAG
**	Years were increased to 4 digits to accomidate the century.
**
**--------------------------------------------------------------------*/

struct qcf
    {
    char nomDate[11];      /* 4 digit UTC nominal date of observation */
    char nomTime[6];       /* UTC nominal time of observation */
    char actualDate[11];   /* 4 digit UTC actual date of observation */
    char actualTime[6];    /* UTC actual time of observation */
    char networkId[11];    /* network identifier (platform abbrev.) */
    char stationId[16];    /* station identifier */
    float lat;             /* latitude,longitude of station */
    float lon;             /* latitude,longitude of station */
    int occurence;         /* station occurence */
    float elevation;       /* station elevation in meters */

    float pressure;        /* station pressure in mb */
    char press_flag;       /* qc flag for station pressure */

    float sealvlprs;       /* sea level pressure in mb */
    char seaflag;          /* qc flag for sea level pressure */

    float comp_sealvl;     /* computed sea level pressure in mb */
    char compflag;         /* qc flag for computed sea level pressure */

    float temperature;     /* dry bulb temperature in Celsius */
    char tmp_flag;         /* qc flag for dry bulb temperature */
    float dew_point;       /* dew point temperature in Celsius */
    char dewflag;          /* qc flag for dew point */

    float windSpeed;       /* wind speed in meters/second */
    char wspdflag;         /* qc flag for wind speed */
    float windDir;         /* wind direction in degrees azimuth */
    char wdirflag;         /* qc flag for wind direction */

    float precip;          /* total precipitation in mm */
    char prcflag;          /* qc flag for total precipitation */

    char squallGust;       /* squall/gust indicator */
    float squallGustSpeed; /* squall/gust speed in meters/second */
    char squallGustFlag;   /* qc flag for squall/gust speed */

    int present_wx;        /* present weather code value */
    char pw_flag;          /* qc flag for present weather */

    float visibility;      /* visibility in meters */
    char vis_flag;         /* qc flag for visibility */

    float ceilingHt1;      /* ceiling height (first layer)- hundreds of ft */
    int ch1_code;          /* ceiling flag (first layer) code value */
    char ch1_flag;         /* qc flag for ceiling height/flag */
    int cloud_amt1;        /* cloud amount (first layer) code value */
    char ca1_flag;         /* qc flag for cloud amount */

    float ceilingHt2;      /* ceiling height (second layer)- hundreds of ft */
    int ch2_code;          /* ceiling flag (second layer) code value */
    char ch2_flag;         /* qc flag for ceiling height/flag */
    int cloud_amt2;        /* cloud amount (second layer) code value */
    char ca2_flag;         /* qc flag for cloud amount */

    float ceilingHt3;      /* ceiling height (third layer)- hundreds of ft */
    int ch3_code;          /* ceiling flag (third layer) code value */
    char ch3_flag;         /* qc flag for ceiling height/flag */
    int cloud_amt3;        /* cloud amount (third layer) code value */
    char ca3_flag;         /* qc flag for cloud amount */
    };

typedef struct qcf QCF_REC_TYPE;

struct date_struct
    {
    char year[5];          /* 4 digit year */
    char month[3];
    char day[3];
    };

struct time_struct
    {
    char hour[3];
    char minute[3];
    };

typedef struct date_struct DATE_TYPE;
typedef struct time_struct TIME_TYPE;
