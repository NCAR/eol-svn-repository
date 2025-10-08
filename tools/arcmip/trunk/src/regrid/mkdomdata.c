
#include <stdio.h>
#include <stdlib.h>
#include <netcdf.h>
#include "arcmip_ease.h"


short get_elev();



void mkncdata(ncid,zsfc_id,lat_id,lon_id)
int ncid;
int zsfc_id;
int lat_id;
int lon_id;
{
FILE *fp;
int start_col,start_row,ease_cols,ease_rows,ease_len;
short *ease_zsfc;
double *ease_lat,*ease_lon;
int stat;
int i;
short e;

errno=0;
if (!(fp=fopen(EASE_FILE,"rb"))) {
 fprintf(stderr,"cannot open ease file %d\n",EASE_FILE);
 exit(1);
 }
for (; (fgetc(fp) != 12) && !feof(fp); ) ; /* look for ^L after text string */
fread(&start_col,sizeof(int),1,fp);
fread(&start_row,sizeof(int),1,fp);
fread(&ease_cols,sizeof(int),1,fp);
fread(&ease_rows,sizeof(int),1,fp);
ease_len=ease_rows*ease_cols;
fprintf(stderr,"EASE:\n start col=%d row=%d\n num cols=%d rows=%d len=%d\n",
 start_col,start_row,ease_cols,ease_rows,ease_len);
ease_zsfc = (short*)malloc(ease_len*sizeof(short));
if (!ease_zsfc) {
 fprintf(stderr,"malloc error %d ease zsfc\n",errno);
 exit(1);
 }
ease_lat = (double*)malloc(ease_len*sizeof(double));
if (!ease_lat) {
 fprintf(stderr,"malloc error %d ease lat\n",errno);
 exit(1);
 }
ease_lon = (double*)malloc(ease_len*sizeof(double));
if (!ease_lon) {
 fprintf(stderr,"malloc error %d ease lon\n",errno);
 exit(1);
 }
if (fread(ease_lat,sizeof(double),ease_len,fp) < ease_len) {
 fprintf(stderr,"ease_lat read error %d\n",errno);
 exit(1);
 }
if (fread(ease_lon,sizeof(double),ease_len,fp) < ease_len) {
 fprintf(stderr,"ease_lon read error %d\n",errno);
 exit(1);
 }
fclose(fp);

elev_setup();
for (i=0; i<ease_len; i++) {
 e = get_elev(ease_lat[i],ease_lon[i]);
 if (e == -500) e = 0; /* ocean */
 ease_zsfc[i] = e;
 }
elev_cleanup();

stat = nc_put_var_short(ncid, zsfc_id, ease_zsfc);
check_err(stat,__LINE__,__FILE__);
stat = nc_put_var_double(ncid, lat_id, ease_lat);
check_err(stat,__LINE__,__FILE__);
stat = nc_put_var_double(ncid, lon_id, ease_lon);
check_err(stat,__LINE__,__FILE__);

free(ease_zsfc);
free(ease_lat);
free(ease_lon);
}
