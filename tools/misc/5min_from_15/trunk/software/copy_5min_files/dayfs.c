#include <stdio.h>
#include <stdlib.h>
#include <string.h>

main()
{
char *buf,fnam[16];
FILE *fp;

if (!(buf=(char*)malloc(2048))) {
  fprintf(stderr,"Error malloc\n");
  exit(1);
  }

strcpy(fnam+6,".pqcf");

for (;fgets(buf,2048,stdin);) {
  fnam[0]=buf[0]; fnam[1]=buf[1];
  fnam[2]=buf[3]; fnam[3]=buf[4];
  fnam[4]=buf[6]; fnam[5]=buf[7];
  if (!(fp=fopen(fnam,"a"))) {
    fprintf(stderr,"Error opening %s\n",fnam);
    exit(1);
    }
  fputs(buf,fp);
  fclose(fp);
  }

return(0);
}
