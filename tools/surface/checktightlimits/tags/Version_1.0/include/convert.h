/*---------------------------------------------------------------------
** File /home/lia/convert_data/include/convert.h
**
**--------------------------------------------------------------------*/

#define FALSE 0
#define TRUE  1
#define MAX_SKY_TOKENS 16

enum datavalue_types { elevation, latitude, longitude, pressure,
        seaLvlPressure, cSeaLvlPressure, temperature, dewPoint, 
        windSpeed, windDir, precipitation, squallGust, visibility, 
        ceilingHeight1, ceilingHeight2, ceilingHeight3,
        ceilingFlag1, ceilingFlag2, ceilingFlag3,
        cloudAmount1, cloudAmount2, cloudAmount3 };

typedef enum datavalue_types DATAVALUE_TYPE;

