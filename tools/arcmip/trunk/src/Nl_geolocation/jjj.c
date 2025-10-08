#include <stdio.h>
#include <math.h>

main()
{
docalcs("Nl");
docalcs("N5");
}

docalcs(grid)
char *grid;
{
double r,s,lat,lon;
int i;

i=ezlh_convert(grid,72.,-153.,&r,&s);
printf("%s 72.,-153. i=%d r=%f s=%f\n",grid,i,r,s);
i=ezlh_inverse(grid, r, s, &lat, &lon);
printf("%s inverse i=%d lat=%f lon=%f\n",grid,i,lat,lon);

i=ezlh_convert(grid,90.,0.,&r,&s);
printf("%s 90.,0. i=%d r=%f s=%f\n",grid,i,r,s);
i=ezlh_convert(grid,89.,1.,&r,&s);
printf("%s 89.,1. i=%d r=%f s=%f\n",grid,i,r,s);
i=ezlh_convert(grid,0.,-135.,&r,&s);
printf("%s 0.,-135. i=%d r=%f s=%f\n",grid,i,r,s);
i=ezlh_convert(grid,-0.34,45.,&r,&s);
printf("%s -0.34,45. i=%d r=%f s=%f\n",grid,i,r,s);
i=ezlh_convert(grid,0.,45.,&r,&s);
printf("%s 0.,45. i=%d r=%f s=%f\n",grid,i,r,s);

i=ezlh_inverse(grid, 1., 1., &lat, &lon);
printf("%s inverse(1.,1.) i=%d lat=%f lon=%f\n",grid,i,lat,lon);
i=ezlh_inverse(grid, 180., 180., &lat, &lon);
printf("%s inverse(180.,180.) i=%d lat=%f lon=%f\n",grid,i,lat,lon);
i=ezlh_inverse(grid, 359., 359., &lat, &lon);
printf("%s inverse(359.,359.) i=%d lat=%f lon=%f\n",grid,i,lat,lon);
i=ezlh_inverse(grid, 360., 360., &lat, &lon);
printf("%s inverse(360.,360.) i=%d lat=%f lon=%f\n",grid,i,lat,lon);
i=ezlh_inverse(grid, 361., 361., &lat, &lon);
printf("%s inverse(361.,361.) i=%d lat=%f lon=%f\n",grid,i,lat,lon);
i=ezlh_inverse(grid, 719., 719., &lat, &lon);
printf("%s inverse(719.,719.) i=%d lat=%f lon=%f\n",grid,i,lat,lon);
i=ezlh_inverse(grid, 720., 720., &lat, &lon);
printf("%s inverse(720.,720.) i=%d lat=%f lon=%f\n",grid,i,lat,lon);
i=ezlh_inverse(grid, 721., 721., &lat, &lon);
printf("%s inverse(721.,721.) i=%d lat=%f lon=%f\n",grid,i,lat,lon);

printf("\n");
}
