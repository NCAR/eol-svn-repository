/* cudom.c - convert & print the CU SHEBA/ARCSyM grid to our "N5" EASE-Grid */
/* John Allison <john@joss.ucar.edu> */

#include <stdio.h>
#include <math.h>

main()
{
char *grid="N5";
double r,s,lat,lon;
int i,j,k;
char buf[256];
int n,x,y;
double olat,olon,elev;
char *ss;
char *usect;
int unused,oneuse,dupuse,maxuse;
int mini,minj,maxi,maxj;

usect=malloc(360*360);
for (ss=usect,i=0;i<360*360;i++,ss++) *ss='\0';

/*
  x  y lat (deg) lon (deg)  elev (m) land use
  1  1     73.02    133.63      0.00 ocean                                   
*/

fputs("  x  y lat (deg) lon (deg)   r   s elat(deg) elon(deg)  elev (m) land use\n",stdout);

for (;fgets(buf,256,stdin);){
 if (sscanf(buf,"%d %d %lf %lf %lf %n",&x,&y,&olat,&olon,&elev,&n) <= 0) continue;
 for (ss=buf+n;*ss;ss++) ;
 for (ss--; isspace(*ss) && ss>buf; *ss--='\0') ;
 r=s=-1.0;
 i=ezlh_convert(grid,olat,olon,&r,&s);
 k=r+0.5;
 r=k;
 j=s+0.5;
 s=j;
 lat=lon=-999.0;
 i=ezlh_inverse(grid, r, s, &lat, &lon);

 usect[k*360+j]++;

 printf(" %2d %2d %9.2f %9.2f %3d %3d %9.2f %9.2f %9.2f %s\n",
  x,y,olat,olon,k,j,lat,lon,elev,buf+n);

 }

fputs("\nEASE-Grid cell reports:\nrow col used n times\n",stdout);
unused=oneuse=dupuse=maxuse=0;
mini=minj=361;
maxi=maxj=-1;
for (i=0; i<360; i++)
 for (j=0; j<360; j++) {
   k=i*360+j;
   switch (usect[k]) {
   case 0: unused++; break;
   case 1: oneuse++; break;
   default:
    dupuse++;
    if (usect[k]>maxuse) maxuse=usect[k];
    fprintf(stdout,"%3d %3d used %d times\n",i,j,usect[k]);
    break;
   }
  if (usect[k]) {
   if (i>maxi) maxi=i;
   if (j>maxj) maxj=j;
   if (i<mini) mini=i;
   if (j<minj) minj=j;
   }
  }
fputs("\n",stdout);
fprintf(stdout,"%d grid cells unused\n%d grid cells have one use\n%d grid cells have duplicate uses\n%d is most uses for a single cell\n",
 unused,oneuse,dupuse,maxuse);
fputs("\n",stdout);
fprintf(stdout,"Min col (r) = %d , Min row (s) = %d\n",mini,minj);
fprintf(stdout,"Max col (r) = %d , Max row (s) = %d\n",maxi,maxj);

}
