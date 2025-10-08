/* Set the values for the RICO project */

#define LAT_TYPE                      HOT

/* Set Limits for Pressure values */
#define DEC_PRESS_CHECK             200.0
#define MISS_MAX_PRESS             1100.0
#define QUEST_MAX_PRESS            1050.0
#define QUEST_MIN_PRESS               0.0

/* Set Limits for Temperature values */
#define MISS_MAX_TEMP                75.0
#define QUEST_MAX_TEMP               45.0
#define QUEST_MIN_TEMP              -90.0

/* Set Limits for Dew Point values. */
#define MISS_MAX_DEWPT               75.0
#define QUEST_MAX_DEWPT              33.0
#define QUEST_MIN_DEWPT             -99.9

/* Set Limits for Altitude values */
#define QUEST_MAX_ALT             40000.0
#define QUEST_MIN_ALT                 0.0

/* Set Limits for Ascension Rate values. */
#ifdef DROP
#define BAD_ASC_RATE_CHANGE           9.0
#define QUEST_ASC_RATE_CHANGE         5.0
#define QUEST_MAX_ASC_RATE           20.0
#else
#define BAD_ASC_RATE_CHANGE           5.0
#define QUEST_ASC_RATE_CHANGE         3.0
#define QUEST_MAX_ASC_RATE           10.0
#endif

/* Set Limits for Wind Velocity values. */
#define BAD_MAX_WIND_SPD            150.0
#define MISS_MAX_WIND_SPD           200.0
#define QUEST_MAX_WIND_SPD          100.0
#define QUEST_MIN_WIND_SPD            0.0

/* Define values for pressures changes. */
#ifdef DROP
#define BAD_RAPID_PRESS_INC           3.0
#define QUEST_RAPID_PRESS_INC         1.5
#else
#define BAD_RAPID_PRESS_INC           2.0
#define QUEST_RAPID_PRESS_INC         1.0
#endif

/* Define values for temperature changes. */
#define BAD_RAPID_TEMP_INC          100.0
#define BAD_STRAT_RAPID_TEMP_INC    200.0
#define PRESS_LIMIT                 250.0
#define RAPID_TEMP_INC                  5
#define STRAT_RAPID_TEMP_INC           10

/* Define values for COLD weather temperature change. */
#define BAD_SFC_RAPID_TEMP_INC      200.0
#define SFC_PRESS_LIMIT               0.0
#define SFC_RAPID_TEMP_INC            0.0

/* Define values for checking the Adiabatic Lapse Rate. */
#define ADIABATIC_LAPSE_RATE         0.01
#define BAD_LAPSE_RATE              -30.0
#define LAPSE_RATE_LIMIT              1.5
#define QUEST_LAPSE_RATE            -15.0
#define SFC_BAD_LAPSE_RATE         -200.0
#define SFC_QUEST_LAPSE_RATE       -100.0

#ifdef ISS
#define LAPSE_RATE_DIFF               .25
#define PRESS_SIGN                      <
#else
#define LAPSE_RATE_DIFF               .15
#define PRESS_SIGN                     <=
#endif
