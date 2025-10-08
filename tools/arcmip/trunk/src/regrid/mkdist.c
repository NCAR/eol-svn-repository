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
main() {			/* create distance.nc */

   int  ncid;			/* netCDF id */

   /* dimension ids */
   int col_dim;
   int row_dim;
   int neighbor_dim;

   /* dimension lengths */
   size_t col_len = 100;
   size_t row_len = 92;
   size_t neighbor_len = 4;

   /* variable ids */
   int model_index_id;
   int distance_id;
   int avgdist_id;

   /* rank (number of dimensions) for each variable */
#  define RANK_model_index 3
#  define RANK_distance 3
#  define RANK_avgdist 2

   /* variable shapes */
   int model_index_dims[RANK_model_index];
   int distance_dims[RANK_distance];
   int avgdist_dims[RANK_avgdist];

   /* attribute vectors */
   int cdf_start_col[1];
   int cdf_start_row[1];
   int cdf_model_iew[1];
   int cdf_model_jns[1];

   /* enter define mode */
   int stat = nc_create("distance.nc", NC_CLOBBER, &ncid);
   check_err(stat,__LINE__,__FILE__);

   /* define dimensions */
   stat = nc_def_dim(ncid, "col", col_len, &col_dim);
   check_err(stat,__LINE__,__FILE__);
   stat = nc_def_dim(ncid, "row", row_len, &row_dim);
   check_err(stat,__LINE__,__FILE__);
   stat = nc_def_dim(ncid, "neighbor", neighbor_len, &neighbor_dim);
   check_err(stat,__LINE__,__FILE__);

   /* define variables */

   model_index_dims[0] = neighbor_dim;
   model_index_dims[1] = row_dim;
   model_index_dims[2] = col_dim;
   stat = nc_def_var(ncid, "model_index", NC_INT, RANK_model_index, model_index_dims, &model_index_id);
   check_err(stat,__LINE__,__FILE__);

   distance_dims[0] = neighbor_dim;
   distance_dims[1] = row_dim;
   distance_dims[2] = col_dim;
   stat = nc_def_var(ncid, "distance", NC_DOUBLE, RANK_distance, distance_dims, &distance_id);
   check_err(stat,__LINE__,__FILE__);

   avgdist_dims[0] = row_dim;
   avgdist_dims[1] = col_dim;
   stat = nc_def_var(ncid, "avgdist", NC_DOUBLE, RANK_avgdist, avgdist_dims, &avgdist_id);
   check_err(stat,__LINE__,__FILE__);

   /* assign attributes */
   stat = nc_put_att_text(ncid, model_index_id, "long_name", 22, "index into model array");
   check_err(stat,__LINE__,__FILE__);
   stat = nc_put_att_text(ncid, distance_id, "long_name", 38, "distance from center of EASE-Grid cell");
   check_err(stat,__LINE__,__FILE__);
   stat = nc_put_att_text(ncid, distance_id, "units", 2, "km");
   check_err(stat,__LINE__,__FILE__);
   stat = nc_put_att_text(ncid, avgdist_id, "long_name", 46, "average distance from center of EASE-Grid cell");
   check_err(stat,__LINE__,__FILE__);
   stat = nc_put_att_text(ncid, avgdist_id, "units", 2, "km");
   check_err(stat,__LINE__,__FILE__);
   stat = nc_put_att_text(ncid, NC_GLOBAL, "title", 11, "distance.nc");
   check_err(stat,__LINE__,__FILE__);
   stat = nc_put_att_text(ncid, NC_GLOBAL, "type", 8, "distance");
   check_err(stat,__LINE__,__FILE__);
   stat = nc_put_att_text(ncid, NC_GLOBAL, "modeler", 9, "UCAR/JOSS");
   check_err(stat,__LINE__,__FILE__);
   stat = nc_put_att_text(ncid, NC_GLOBAL, "fullname", 39, "ARCMIP 50km EASE-Grid Nearest Neighbors");
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


/* changed */
   stat = nc_put_att_text(ncid, NC_GLOBAL, "model_name", strlen(MODEL_NAME), MODEL_NAME);
   check_err(stat,__LINE__,__FILE__);
   cdf_model_iew[0] = MODEL_COLS;
   stat = nc_put_att_int(ncid, NC_GLOBAL, "model_iew", NC_INT, 1, cdf_model_iew);
   check_err(stat,__LINE__,__FILE__);
   cdf_model_jns[0] = MODEL_ROWS;
   stat = nc_put_att_int(ncid, NC_GLOBAL, "model_jns", NC_INT, 1, cdf_model_jns);
   check_err(stat,__LINE__,__FILE__);


   stat = nc_put_att_text(ncid, NC_GLOBAL, "model_j", 23, "model_index / model_iew");
   check_err(stat,__LINE__,__FILE__);
   stat = nc_put_att_text(ncid, NC_GLOBAL, "model_i", 23, "model_index % model_iew");
   check_err(stat,__LINE__,__FILE__);
   stat = nc_put_att_text(ncid, NC_GLOBAL, "comment", 174, "Data is the index and distance for the model cells closest to the center of the EASE-Grid cell. These cells were used for interpolations of model variables during regridding.");
   check_err(stat,__LINE__,__FILE__);
   stat = nc_put_att_text(ncid, NC_GLOBAL, "joss_comment", 90, "Prepared by UCAR/JOSS, 2003. http://www.joss.ucar.edu/arcmip/regrid/  codiac@joss.ucar.edu");
   check_err(stat,__LINE__,__FILE__);

   /* leave define mode */
   stat = nc_enddef (ncid);
   check_err(stat,__LINE__,__FILE__);


mkdistdata(ncid,model_index_id,distance_id,avgdist_id);


   stat = nc_close(ncid);
   check_err(stat,__LINE__,__FILE__);
   return 0;
}
