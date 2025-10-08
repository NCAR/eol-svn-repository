/* nnmap.c - make a map showing nearest neighbor distances */

#include <stdio.h>
#include <string.h>
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

mkmap("distances.gif","../../ease.gif","distmap.gif","model.gif");
}



#include "gd.h"
#include "gdfontt.h"

int mkmap(imgfnam,i2in,i2out,i3out)
char *imgfnam,*i2in,*i2out,*i3out;
{
FILE *fp;
gdImagePtr im,im2,im3;
int i,j,ei,ex,ey,imgx,imgy,x,y;
int colors[256],colors2[256];
double sum,avg;
double scale=1846.0;
int color,maxcolor2,white2,red2,black2;
int white3,red3,black3;
char buf[1024];

imgx = ease_cols * 4;
imgy = ease_rows * 4;
if (!(im = gdImageCreate(imgx,imgy))) {
 fprintf(stderr,"cannot create image");
 return(1);
 }
fp=fopen(i2in,"rb");
if (!(im2 = gdImageCreateFromGif(fp))) {
 fprintf(stderr,"cannot create image");
 return(1);
 }
fclose(fp);
fp=fopen(i2in,"rb"); /* im3 also uses i2in */
if (!(im3 = gdImageCreateFromGif(fp))) {
 fprintf(stderr,"cannot create image");
 return(1);
 }
fclose(fp);

for (i=0; i<256; i++)
 colors[i] = gdImageColorAllocate(im, i, i, i); /* gray scale */

#if 0
for (i=0; i<38; i++)
 colors[i] = gdImageColorAllocate(im, i*2, i*2, i*2);
for (; i<98; i++)
 colors[i] = gdImageColorAllocate(im, 0, 0, (i-38)*4+15);
for (; i<158; i++)
 colors[i] = gdImageColorAllocate(im, (i-98)*4+15, 0, 255);
for (; i<218; i++)
 colors[i] = gdImageColorAllocate(im, 255, (i-158)*4+15, 255);
for (; i<256; i++)
 colors[i] = gdImageColorAllocate(im, i, i, i);
#endif


white2 = gdImageColorExact(im2,255,255,255);
if (white2 == -1) white2 = gdImageColorAllocate(im2,255,255,255);
red2 = gdImageColorExact(im2,255,0,0);
if (red2 == -1) red2 = gdImageColorAllocate(im2,255,0,0);
black2 = gdImageColorExact(im2,0,0,0);
if (black2 == -1) black2 = gdImageColorAllocate(im2,0,0,0);
maxcolor2=253-gdImageColorsTotal(im2);
colors2[0]=black2;
colors2[maxcolor2]=white2;
for (i=1; i<maxcolor2; i++)
 colors2[i] = gdImageColorAllocate(im2, i, i, i); /* gray scale */
sprintf(buf,"ARCMIP EASE-Grid average distance for %s model points",MODEL_NAME);
gdImageString(im2,gdFontTiny,1,1,buf,white2);

for (i=0; i<nbor_len; i+=num_nbors) {
 ei=i/num_nbors;
 ey=ei/ease_cols;
 ex=ei%ease_cols;
 for (sum=0.0,j=0; j<num_nbors; j++)
  sum+=nbor_dist[i+j];
 avg=sum/4.0;
 color = (int)(avg/scale * 255);
 if (color > 255) {
  fprintf(stderr,"warning: scaled value %d too big; avg=%f scale=%f\n",color,avg,scale);
  color=255;
  }
gdImageFilledRectangle(im,ex*4,ey*4,ex*4+3,ey*4+3,colors[color]);
if (color > maxcolor2) color=maxcolor2;
gdImageSetPixel(im2,ex+start_col,ey+start_row,colors2[color]);
 }

#if 0
gdImageFilledRectangle(im2,161,144,163,146,red2); /* "center" 162,145 = 72N 153W */
#endif

white3 = gdImageColorExact(im3,255,255,255);
if (white3 == -1) white3 = gdImageColorAllocate(im3,255,255,255);
red3 = gdImageColorExact(im3,255,0,0);
if (red3 == -1) red3 = gdImageColorAllocate(im3,255,0,0);
black3 = gdImageColorExact(im3,0,0,0);
if (black3 == -1) black3 = gdImageColorAllocate(im3,0,0,0);
read_model();
drawmodel(im3,red3);
gdImageRectangle(im3,113,101,212,192,white3); /* my ARCMIP EASE-Grid */
gdImageString(im3,gdFontTiny,114,194,"ARCMIP EASE-Grid 50km",white3);
sprintf(buf,"%s model points",MODEL_NAME);
gdImageString(im3,gdFontTiny,114,91,buf,red3);

	fp = fopen(imgfnam, "wb");
	if (!fp) {
	 fprintf(stderr, "Unable to write to %s -- exiting\n",imgfnam);
	 return(2);
	 }
	gdImageGif(im, fp);
	fclose(fp);

	fp = fopen(i2out, "wb");
	if (!fp) {
	 fprintf(stderr, "Unable to write to %s -- exiting\n",i2out);
	 return(2);
	 }
	gdImageGif(im2, fp);
	fclose(fp);

	fp = fopen(i3out, "wb");
	if (!fp) {
	 fprintf(stderr, "Unable to write to %s -- exiting\n",i3out);
	 return(2);
	 }
	gdImageGif(im3, fp);
	fclose(fp);

	if (im) gdImageDestroy(im);
	if (im2) gdImageDestroy(im2);
	if (im3) gdImageDestroy(im3);
	return 0;
}



int model_len;
double *model_lat,*model_lon;

drawmodel(im,col)
gdImagePtr im;
int col;
{
int i,j,mx,my,ex,ey;
double r,s;

for (i=0; i<model_len; i++) {
  mx=i%MODEL_COLS;
  my=i/MODEL_COLS;
  r=s=-1.0;
  ezlh_convert("N5",model_lat[i],model_lon[i],&r,&s);
  ex=r+0.5;
  ey=s+0.5;
  gdImageSetPixel(im,ex,ey,col);
  }
}


#include "netcdf.h"


read_model()
{
char *model_fnam=MODEL_FILE;
int ncid;
int model_latid=MODEL_LATID;
int model_lonid=MODEL_LONID;
char name[NC_MAX_NAME];
int ndims;
int dimids[NC_MAX_VAR_DIMS];
size_t length;
int i,j;

if (nc_open(model_fnam,0,&ncid) != NC_NOERR) {
 fprintf(stderr,"cannot nc_open model file %s\n",model_fnam);
 exit(1);
 }

if (nc_inq_var(ncid, model_latid, name, 0, &ndims, dimids, 0) != NC_NOERR) {
 fprintf(stderr,"cannot read lat var info\n");
 exit(1);
 }
fprintf(stderr,"lat id %d = %s\n",model_latid,name);
for (i=0, j=1; i<ndims; i++) {
 if (nc_inq_dimlen(ncid,dimids[i],&length) != NC_NOERR) {
  fprintf(stderr,"cannot get dim %d length for lat dim %d\n",dimids[i],i);
  exit(1);
  }
 fprintf(stderr,"lat dim %d = %d\n",i,length);
 j*=length;
 }
fprintf(stderr,"lat len = %d\n",j);
model_len=j;
if (model_len != MODEL_ROWS*MODEL_COLS)
  fprintf(stderr,"model_len = %d which != #define %d * %d = %d\n",
   model_len,MODEL_ROWS,MODEL_COLS,MODEL_ROWS*MODEL_COLS);

if (nc_inq_var(ncid, model_lonid, name, 0, &ndims, dimids, 0) != NC_NOERR) {
 fprintf(stderr,"cannot read lon var info\n");
 exit(1);
 }
fprintf(stderr,"lon id %d = %s\n",model_lonid,name);
for (i=0, j=1; i<ndims; i++) {
 if (nc_inq_dimlen(ncid,dimids[i],&length) != NC_NOERR) {
  fprintf(stderr,"cannot get dim %d length for lat dim %d\n",dimids[i],i);
  exit(1);
  }
 j*=length;
 }
fprintf(stderr,"lon len = %d\n",j);
if (j != model_len) {
 fprintf(stderr,"lat len = %d but lon len = %d\n",model_len,j);
 exit(1);
 }

model_lat = (double*)malloc(model_len*sizeof(double));
model_lon = (double*)malloc(model_len*sizeof(double));
if (!model_lat || !model_lon) {
 fprintf(stderr,"malloc error model\n");
 exit(1);
 }

if (nc_get_var_double(ncid,model_latid,model_lat) != NC_NOERR) {
 fprintf(stderr,"cannot read model lat\n");
 exit(1);
 }
if (nc_get_var_double(ncid,model_lonid,model_lon) != NC_NOERR) {
 fprintf(stderr,"cannot read model lon\n");
 exit(1);
 }

nc_close(ncid);
}
