/* mkease.c - write binary representation of the ARCMIP EASE-Grid */
/* john@joss.ucar.edu */

#include <stdio.h>
#include "arcmip_ease.h"
#include "ezlhconv.h"

main()
{
double *lat,*lon;
int x,y,i;
char *grid=GRID;
FILE *fp;
int ease_len = EASE_COLS*EASE_ROWS;

if (!(fp=fopen(EASE_FILE,"wb"))) {
 fprintf(stderr,"cannot open %s\n",EASE_FILE);
 exit(1);
 }
fprintf(fp,"ARCMIP EASE-Grid%c",12); /* need this here or fprintf/fwrite core dumps */
/* XXX */

lat = (double*)malloc(ease_len*sizeof(double));
lon = (double*)malloc(ease_len*sizeof(double));
if (!lat || !lon) {
 fprintf(stderr,"malloc error\n");
 exit(1);
 }

for (y=0; y<EASE_ROWS; y++) {
 for (x=0; x<EASE_COLS; x++)
  if (i=ezlh_inverse(grid,
   (double)(x+EASE_START_COL), (double)(y+EASE_START_ROW),
   &lat[x+EASE_COLS*y], &lon[x+EASE_COLS*y]))
   {
   fprintf(stderr,"inverse(%d,%d)=%d\n", x+EASE_START_COL, y+EASE_START_ROW, i);
   exit(i*-1);
   }
  }

x=EASE_START_COL; fwrite(&x,sizeof(int),1,fp);
x=EASE_START_ROW; fwrite(&x,sizeof(int),1,fp);
x=EASE_COLS; fwrite(&x,sizeof(int),1,fp);
x=EASE_ROWS; fwrite(&x,sizeof(int),1,fp);
fwrite(lat,sizeof(double),ease_len,fp);
fwrite(lon,sizeof(double),ease_len,fp);
fclose(fp);

fprintf(stderr,"EASE:\n start col=%d row=%d\n num cols=%d rows=%d len=%d\n",
 EASE_START_COL,EASE_START_ROW,EASE_COLS,EASE_ROWS,ease_len);
}
