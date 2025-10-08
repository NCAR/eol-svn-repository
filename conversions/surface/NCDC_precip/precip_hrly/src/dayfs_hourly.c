#include <stdio.h>
#include <stdlib.h>
#include <string.h>

main()
{
char *buf,fnam[35];
FILE *fp;

if (!(buf=(char*)malloc(2048))) {
  fprintf(stderr,"Error malloc\n");
  exit(1);
  }

strcpy(fnam, "../out/final/NCDC60_");
strcpy(fnam+26,".pqcf");

for (;fgets(buf,2048,stdin);) {
  fnam[20]=buf[0]; fnam[21]=buf[1];
  fnam[22]=buf[3]; fnam[23]=buf[4];
  fnam[24]=buf[6]; fnam[25]=buf[7];
  if (!(fp=fopen(fnam,"a"))) {
    fprintf(stderr,"Error opening %s\n",fnam);
    exit(1);
    }
  fputs(buf,fp);
  fclose(fp);
  }

return(0);
}
