/* ezcvt.c - converts to/from our "N5" EASE-Grid and latlons
 * reads stdin, writes stdout
 * if at least 1 coord on stdin has a '.' then assume latlon else row,col
 */
/* John Allison <john@joss.ucar.edu> */

#include <stdio.h>
#include <math.h>

main()
{
char *grid="N5";
char s1[128],s2[128];
double r,s,lat,lon;
int i,j,k;

for (;scanf("%s %s",s1,s2)>0;) {
 if (strchr(s1,'.') || strchr(s2,'.')) { /* s1=lat,s2=lon */
   lat=atof(s1);
   lon=atof(s2);
   if (lon > 180.0) lon = lon - 360.0; /* those crazy swiss */
   r=s=-1.0;
   i=ezlh_convert(grid,lat,lon,&r,&s);
   printf("%s %f,%f => i=%d r=%f s=%f\n",grid,lat,lon,i,r,s);
   k=r+0.5;
   r=k;
   j=s+0.5;
   s=j;
   lat=lon=-999.0;
   i=ezlh_inverse(grid, r, s, &lat, &lon);
   printf("   inverse %d,%d => i=%d lat=%f lon=%f\n",k,j,i,lat,lon);
   }
 else { /* s1=row,s2=col */
   r=atof(s1);
   s=atof(s2);
   lat=lon=-999.0;
   i=ezlh_inverse(grid, r, s, &lat, &lon);
   printf("%s %f,%f inverse=> i=%d lat=%f lon=%f\n",grid,r,s,i,lat,lon);
   r=s=-1.0;
   i=ezlh_convert(grid,lat,lon,&r,&s);
   printf("   %f,%f => i=%d r=%f s=%f\n",lat,lon,i,r,s);
   }
 }

}
