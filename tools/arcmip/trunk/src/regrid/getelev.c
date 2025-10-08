
#include <stdio.h>
#include <sys/types.h>


unsigned char *tiles[8];
int fd[8];
double left[8] = {
 -180., -90., 0., 90.,
 -180., -90., 0., 90.,
 };
double top[8] = {
 90., 90., 90., 90.,
 50., 50., 50., 50.,
 };
char *filepfx = "/stage/jallison/dem/";
char *files[8] = {
 "a11g", "b10g", "c10g", "d10g",
 "e10g", "f10g", "g10g", "h10g",
 };
size_t len[8] = {
 103680000, 103680000, 103680000, 103680000,
 129600000, 129600000, 129600000, 129600000,
 };



short get_elev(lat,lon)
double lat,lon;
{
int i,x,y,tile;
short e;

 if (lon < -90.0)
   tile=0;
 else if (lon < 0.0)
   tile=1;
 else if (lon < 90.0)
   tile=2;
 else 3;
 if (lat <= 50.0) tile+=4;

 x = (lon - left[tile]) * 120;
 y = (top[tile] - lat) * 120;
 i = x + 10800 * y;
 i *= 2;
 e = tiles[tile][i+1]*256  + tiles[tile][i];

#if 0
 fprintf(stderr,"%f %f => tile %d (%d,%d)=%d => %d\n",
  lat,lon,tile,y,x,i,e);
#endif

 return(e);
}



#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>

int errno;


elev_setup()
{
int i;
struct stat sbuf;
char fbuf[1024];

for (i=0;i<8;i++) {
 strcpy(fbuf,filepfx);
 strcat(fbuf,files[i]);
 errno=0;
 if (stat(fbuf,&sbuf)) {
  fprintf(stderr,"Error %d stat(%s)\n",errno,fbuf);
  exit(16);
  }
 if (sbuf.st_size != len[i]) exit(17);
 if ((fd[i] = open(fbuf,O_RDONLY)) < 0) exit(18);
 errno=0;
 if ((tiles[i] = (unsigned char*)mmap(0,len[i],PROT_READ,MAP_SHARED|MAP_NORESERVE,fd[i],0)) == MAP_FAILED) {
   fprintf(stderr,"Error %d mapping tile %d (%d=%s)\n",errno,i,fd[i],fbuf);
   exit(19);
   }
 }
}

elev_cleanup()
{
int i;
for (i=0;i<8;i++) {
 munmap((void*)tiles[i],len[i]);
 close(fd[i]);
 }
}
