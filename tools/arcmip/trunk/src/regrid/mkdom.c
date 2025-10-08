#include <stdio.h>
#include <stdlib.h>
#include <netcdf.h>
#include "arcmip_ease.h"

void
check_err(const int stat, const int line, const char *file) {
    if (stat != NC_NOERR) {
	   (void) fprintf(stderr, "line %d of %s: %s\n", line, file, nc_strerror(stat));
        exit(1);
    }
}

int
main() {			/* create ease_domain.nc */

   int  ncid;			/* netCDF id */

   /* dimension ids */
   int col_dim;
   int row_dim;

   /* dimension lengths */
   size_t col_len = 100;
   size_t row_len = 92;

   /* variable ids */
   int lat_id;
   int lon_id;

   /* rank (number of dimensions) for each variable */
#  define RANK_lat 2
#  define RANK_lon 2

   /* variable shapes */
   int lat_dims[RANK_lat];
   int lon_dims[RANK_lon];

   /* attribute vectors */
   int cdf_start_col[1];
   int cdf_start_row[1];

FILE *fp;
int start_col,start_row,ease_cols,ease_rows,ease_len;
double *ease_lat,*ease_lon;


   /* enter define mode */
   int stat = nc_create("ease_domain.nc", NC_CLOBBER, &ncid);
   check_err(stat,__LINE__,__FILE__);

   /* define dimensions */
   stat = nc_def_dim(ncid, "col", col_len, &col_dim);
   check_err(stat,__LINE__,__FILE__);
   stat = nc_def_dim(ncid, "row", row_len, &row_dim);
   check_err(stat,__LINE__,__FILE__);

   /* define variables */

   lat_dims[0] = row_dim;
   lat_dims[1] = col_dim;
   stat = nc_def_var(ncid, "lat", NC_FLOAT, RANK_lat, lat_dims, &lat_id);
   check_err(stat,__LINE__,__FILE__);

   lon_dims[0] = row_dim;
   lon_dims[1] = col_dim;
   stat = nc_def_var(ncid, "lon", NC_FLOAT, RANK_lon, lon_dims, &lon_id);
   check_err(stat,__LINE__,__FILE__);

   /* assign attributes */
   stat = nc_put_att_text(ncid, lat_id, "long_name", 8, "latitude");
   check_err(stat,__LINE__,__FILE__);
   stat = nc_put_att_text(ncid, lat_id, "units", 3, "deg");
   check_err(stat,__LINE__,__FILE__);
   stat = nc_put_att_text(ncid, lon_id, "long_name", 9, "longitude");
   check_err(stat,__LINE__,__FILE__);
   stat = nc_put_att_text(ncid, lon_id, "units", 3, "deg");
   check_err(stat,__LINE__,__FILE__);
   stat = nc_put_att_text(ncid, NC_GLOBAL, "title", 14, "ease_domain.nc");
   check_err(stat,__LINE__,__FILE__);
   stat = nc_put_att_text(ncid, NC_GLOBAL, "type", 6, "domain");
   check_err(stat,__LINE__,__FILE__);
   stat = nc_put_att_text(ncid, NC_GLOBAL, "modeler", 9, "UCAR/JOSS");
   check_err(stat,__LINE__,__FILE__);
   stat = nc_put_att_text(ncid, NC_GLOBAL, "fullname", 28, "ARCMIP 50km EASE-Grid Domain");
   check_err(stat,__LINE__,__FILE__);
   cdf_start_col[0] = 113;
   stat = nc_put_att_int(ncid, NC_GLOBAL, "start_col", NC_INT, 1, cdf_start_col);
   check_err(stat,__LINE__,__FILE__);
   cdf_start_row[0] = 101;
   stat = nc_put_att_int(ncid, NC_GLOBAL, "start_row", NC_INT, 1, cdf_start_row);
   check_err(stat,__LINE__,__FILE__);
   stat = nc_put_att_text(ncid, NC_GLOBAL, "ease_comment", 86, "ARCMIP grid starts at specified row and column of a 50km northern azimuthal EASE-Grid.");
   check_err(stat,__LINE__,__FILE__);
   stat = nc_put_att_text(ncid, NC_GLOBAL, "x_index", 41, "col = index of grid points in x direction");
   check_err(stat,__LINE__,__FILE__);
   stat = nc_put_att_text(ncid, NC_GLOBAL, "y_index", 41, "row = index of grid points in y direction");
   check_err(stat,__LINE__,__FILE__);
   stat = nc_put_att_text(ncid, NC_GLOBAL, "comment", 90, "Prepared by UCAR/JOSS, 2003. http://www.joss.ucar.edu/arcmip/regrid/  codiac@joss.ucar.edu");
   check_err(stat,__LINE__,__FILE__);

   /* leave define mode */
   stat = nc_enddef (ncid);
   check_err(stat,__LINE__,__FILE__);



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

stat = nc_put_var_double(ncid, lat_id, ease_lat);
check_err(stat,__LINE__,__FILE__);
stat = nc_put_var_double(ncid, lon_id, ease_lon);
check_err(stat,__LINE__,__FILE__);



   stat = nc_close(ncid);
   check_err(stat,__LINE__,__FILE__);
   return 0;
}
