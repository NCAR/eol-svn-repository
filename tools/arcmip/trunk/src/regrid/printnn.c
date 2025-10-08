/* printnn.c - print the nearest model grid point neighbors to centers of EASE-Grid cells */

#include <stdio.h>
#include <errno.h>
#include "arcmip_ease.h"

int start_col,start_row,ease_cols,ease_rows,ease_len;
int num_nbors,nbor_len;
int *nbor_idx;
double *nbor_dist;



main()
{
FILE *fp;
char c;
int i,j;
int ei,ex,ey;

fp=fopen(NBOR_FILE,"rb");

for (; ((c=fgetc(fp)) != 12) && !feof(fp); fputc(c,stdout)) ; /* look for ^L after text string */

fread(&start_col,sizeof(int),1,fp);
fread(&start_row,sizeof(int),1,fp);
fread(&ease_cols,sizeof(int),1,fp);
fread(&ease_rows,sizeof(int),1,fp);
fread(&num_nbors,sizeof(int),1,fp);
ease_len=ease_cols*ease_rows;
nbor_len=ease_len*num_nbors;
printf(" start col=%d row=%d\n num cols=%d rows=%d len=%d\n num neighbors=%d len=%d\n\n",
 start_col,start_row,ease_cols,ease_rows,ease_len,num_nbors,nbor_len);

if (num_nbors != NUM_NBORS)
 fprintf(stderr,"Warning: neighbor file %s has %d neighbors but code #define is %d.\n",
  NBOR_FILE,num_nbors,NUM_NBORS);

nbor_idx = (int*)malloc(nbor_len*sizeof(int));
nbor_dist = (double*)malloc(nbor_len*sizeof(double));
if (!nbor_idx || !nbor_dist) {
 fprintf(stderr,"malloc error %d\n",errno);
 exit(1);
 }
if (fread(nbor_idx,sizeof(int),nbor_len,fp) < nbor_len) {
 fprintf(stderr,"error %d reading neighbor indices\n",errno);
 exit(1);
 }
if (fread(nbor_dist,sizeof(double),nbor_len,fp) < nbor_len) {
 fprintf(stderr,"error %d reading neighbor distances\n",errno);
 exit(1);
 }
fclose(fp);

for (i=0; i<nbor_len; i+=num_nbors) {
 ei=i/num_nbors;
 ey=ei/ease_cols;
 ex=ei%ease_cols;
 printf("E%d %3d,%3d\n",ei,ex+start_col,ey+start_row);
 for (j=0; j<num_nbors; j++)
  printf("  N%d idx=%d %d,%d dist=%f km\n",
   j,nbor_idx[i+j],
   nbor_idx[i+j]%MODEL_COLS+MODEL_START_COL,nbor_idx[i+j]/MODEL_COLS+MODEL_START_ROW,
   nbor_dist[i+j]
   );
 }
}
