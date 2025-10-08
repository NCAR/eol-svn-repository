/* stations structure */
#define MAXSTNS 2000
struct stnrec
   {
   char network[11];	/* list of network/stations */
   char station[16];	/* list of network/stations */
   float lat;			/* station latitude */
   float lon;			/* station longitude */
   float elev;			/* station elevation (km) */
   unsigned short int dx;	/* x distance (m) from nearest SW MAPS grid pt */
   unsigned short int dy;	/* y distance (m) from nearest SW MAPS grid pt */
   short int buddy[5];	/* array of nearest buddies */
   float km[5];		/* array of distances of nearest buddies */
   };
