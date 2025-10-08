
#include <stdio.h>
#include <stdlib.h>
#include <netcdf.h>
#include "arcmip_ease.h"



void mkdistdata(ncid,model_index_id,distance_id,avgdist_id)
int ncid;
int model_index_id;
int distance_id;
int avgdist_id;
{
FILE *fp;
char buf[1024];
int i,j,stat;
int start_col,start_row,ease_cols,ease_rows,ease_len;
int num_nbors,nbor_len;
int (*nbor_idx)[NUM_NBORS];
double (*nbor_dist)[NUM_NBORS];
double *nbor_avg;
int *nc_idx;
double *nc_dist;
double sum;


fp=fopen(NBOR_FILE,"rb");

/* look for ^L after text string, echo string, & save string to check model name */
for (i=0; (i<1024) && ((buf[i]=fgetc(fp)) != 12) && !feof(fp); fputc(buf[i],stderr), i++) ;
buf[--i]='\0'; /* replace newline with null */
for (;(i>0) && (buf[i]!=' '); i--) ;
i++;
if (strcmp(buf+i,MODEL_NAME))
  fprintf(stderr,"Warning: compiled model name %s != neighbor file name %s.\n",MODEL_NAME,buf+i);

fread(&start_col,sizeof(int),1,fp);
fread(&start_row,sizeof(int),1,fp);
fread(&ease_cols,sizeof(int),1,fp);
fread(&ease_rows,sizeof(int),1,fp);
fread(&num_nbors,sizeof(int),1,fp);
ease_len=ease_cols*ease_rows;
nbor_len=ease_len*num_nbors;

if (num_nbors != NUM_NBORS)
 fprintf(stderr,"Warning: neighbor file %s has %d neighbors but code #define is %d.\n",
  NBOR_FILE,num_nbors,NUM_NBORS);

nbor_idx=(int(*)[NUM_NBORS])malloc(nbor_len*sizeof(int));
nbor_dist=(double(*)[NUM_NBORS])malloc(nbor_len*sizeof(double));
nbor_avg=(double*)malloc(ease_len*sizeof(double));
nc_idx=(int*)malloc(nbor_len*sizeof(int));
nc_dist=(double*)malloc(nbor_len*sizeof(double));
if (!nbor_idx || !nbor_dist || !nc_idx || !nc_dist || !nbor_avg) {
 fprintf(stderr,"malloc error %d\n",errno);
 exit(1);
 }
if (fread(nbor_idx,sizeof(int),nbor_len,fp) < nbor_len) {
 fprintf(stderr,"error %d reading neighbor indices\n",errno);
 exit(1);
 }
if (fread(nbor_dist,sizeof(double),nbor_len,fp) < nbor_len) {
 fprintf(stderr,"error %d reading neighbor ratios\n",errno);
 exit(1);
 }

fclose(fp);

for (i=0; i<ease_len; i++) {
 sum = 0.0;
 for (j=0; j<num_nbors; j++) {
   nc_idx[i+ease_len*j] = nbor_idx[i][j];
   nc_dist[i+ease_len*j] = nbor_dist[i][j];
   sum += nbor_dist[i][j];
   }
 nbor_avg[i] = sum / num_nbors;
 }

stat = nc_put_var_int(ncid, model_index_id, (int*)nc_idx);
check_err(stat,__LINE__,__FILE__);
stat = nc_put_var_double(ncid, distance_id, (double*)nc_dist);
check_err(stat,__LINE__,__FILE__);
stat = nc_put_var_double(ncid, avgdist_id, (double*)nbor_avg);
check_err(stat,__LINE__,__FILE__);

free(nbor_idx);
free(nbor_dist);
free(nc_idx);
free(nc_dist);
free(nbor_avg);
}
