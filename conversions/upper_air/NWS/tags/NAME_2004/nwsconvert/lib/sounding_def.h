/******************************************/
/*                                        */
/* SOUNDING_DEF.H:                        */
/*          A file with some definations  */
/* for using in C++ programs to convert   */
/* soundings.                             */
/*                                        */
/* Kendall Southwick                      */
/*                                        */
/******************************************/

#define MAX_OB 2500
#define LINE_LEN 256
#define MAX_ALT_CHG 40
#define MAX_PRESS_CHG 4

typedef struct obser_node
{
  float time;
  float press;
  float temp;
  float dewpt;
  float rh;
  float u_cmp;
  float v_cmp;
  float wind_spd;
  float wind_dir;
  float z_cmp;
  float lon;
  float lat;
  float elev;
  float azim;
  float alt;
  float qp;
  float qt;
  float qrh;
  float qu, qv;
  float qz;
} OBSER;

typedef OBSER SOUND_DATA_ARR[MAX_OB];






