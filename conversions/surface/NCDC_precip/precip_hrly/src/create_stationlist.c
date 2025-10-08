/*******************************************************************************
 * 	create_stationlist.c -- creates a list of all the stations and there 
 *				  information for the NCDC COOP data.
 *
 *      Usage: create_stationlist <f for 15min, h for hourly> <datafile>
 *
 *  000 5 Jul 95 mhc
 *     Created.
 *
 *******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXLINE 1500

struct Data
{
  char Description[25];
  float Elevation;
};


struct Data GetStationFileInfo( char* );
int Print_Station_Record(FILE*, int, int, int, int, int, int, 
                                char*, char*, float, float);

int main(int argc, char* argv[])
{
 
  FILE* IN;
  FILE* OUT;
  FILE* HPD;

  char buf1[MAXLINE];
  char buf2[MAXLINE];
  char buf3[MAXLINE];
  char Frequency[10];
  char StartDate[10];
  char TempDate[10];
  char PrevDate[10];
  char TempId[20];
  char Id[20];
  char HpdId[20];
  char HpdFreq[5];

  char* data_file;
  char* station_file;

  int StartYear, StartMonth, StartDay;
  int  StopYear, StopMonth, StopDay, Valid_Hourly, Done;

  float Lat, Lon, TempLat, TempLon;

  if (argc < 3) 
  {
    perror("Usage: create_stationlist.c <f for 15min, h for hourly> <datafile>");
    exit(1);
  }

  ++argv;

  if( strncmp(*argv, "f", 1) == 0 ) strcpy(Frequency,"15 minute");
  else strcpy(Frequency,"hourly");

  ++argv;
  
  data_file = (char*)(malloc(strlen(*argv) + 1));
  strcpy(data_file,*argv);
  
  station_file = "../out/final/station.list";

  if ((IN = fopen(data_file,"r")) == NULL)
  {
    perror ("Cannot open data file");
    exit(1);
  }
    
  if ((OUT = fopen(station_file,"w")) == NULL)
  { 
    perror ("Cannot open output file");
    exit(1);
  }

  fgets(buf1, MAXLINE, IN); 

  sscanf(buf1, "%s %*s %*s %*s %s %f %f", &StartDate, &Id, &Lat, &Lon);
	/* skip time and network with supression char (*) */
        /* two %*s's used for network since it is Rec rainga */

  strcpy(PrevDate, StartDate); /* give prev date an initial value */

  while( fgets(buf2, MAXLINE, IN) )
  {
    sscanf(buf2, "%s %*s %*s %*s %s %f %f", &TempDate, &TempId, &TempLat, &TempLon);
	/* skip time and network with supression char (*) */
        /* two %*s's used for network since it is Rec rainga */

    if( strcmp(TempId, Id) != 0 )  
       /* New station, so print out previous info on previous station */
    {
      sscanf(StartDate,"%2d/%2d/%2d", &StartYear, &StartMonth, &StartDay);
      sscanf(PrevDate,"%2d/%2d/%2d", &StopYear, &StopMonth, &StopDay);

      if ((HPD = fopen("hpd_stns","r")) == NULL)
      {
        perror ("Cannot open hpd_stns file");
        exit(1);
      }

      Done=0;
      Valid_Hourly=0;      

      if( strcmp(Frequency,"15 minute") == 0) Done = 1;

      while( !Done )
      {
        if( !fgets(buf3, MAXLINE, HPD) )
        { 
          Done=1;
          printf("Could not match Id: %s. Data Dropped\n", Id);
        }

        sscanf(buf3,"%s %*f %*f %s", &HpdId, &HpdFreq);

        if( strcmp(HpdId, Id) == 0 )
        {  
          if( strncmp(HpdFreq, "HLY", 3) == 0 )
            Valid_Hourly = 1;
          else Valid_Hourly = 0;
          Done = 1;          
        }
      }

      fclose(HPD);

      if( (strcmp(Frequency,"15 minute") == 0) || (Valid_Hourly) )
        Print_Station_Record(OUT, StartYear, StartMonth, StartDay, StopYear, 
                                  StopMonth, StopDay, Id, Frequency, Lat, Lon);

      strcpy(StartDate, TempDate);
      strcpy(Id, TempId);
      Lat = TempLat;
      Lon = TempLon;
    }

    strcpy(PrevDate, TempDate);

  }

     /* write out the data for the last station */

  sscanf(StartDate,"%2d/%2d/%2d", &StartYear, &StartMonth, &StartDay);
  sscanf(PrevDate,"%2d/%2d/%2d", &StopYear, &StopMonth, &StopDay);

  if ((HPD = fopen("hpd_stns","r")) == NULL)
  {
    perror ("Cannot open hpd_stns file");
    exit(1);
  }

  Done=0;
  Valid_Hourly=0;      

  if( strcmp(Frequency,"15 minute") == 0) Done = 1;

  while( !Done )
  {
    if( !fgets(buf3, MAXLINE, HPD) )
    { 
      Done=1;
      printf("Could not match Id: %s. Data Dropped\n", Id);
    }

    sscanf(buf3,"%s %*f %*f %s", &HpdId, &HpdFreq);

    if( strcmp(HpdId, Id) == 0 )
    {  
      if( strncmp(HpdFreq, "HLY", 3) == 0 )
        Valid_Hourly = 1;
      else Valid_Hourly = 0;
      Done = 1;          
    }
  }

  fclose(HPD);

  if( (strcmp(Frequency,"15 minute") == 0) || (Valid_Hourly) )
    Print_Station_Record(OUT, StartYear, StartMonth, StartDay, StopYear,
                              StopMonth, StopDay, Id, Frequency, Lat, Lon);

  fclose(IN);
  fclose(OUT);
  free(data_file);

}  /* End main */


struct Data GetStationFileInfo( char* Id )
{
  char buf[MAXLINE];

  char TempDescrip[25];
  char ValidDescription[25];
  char TempId[15];

  float TempElev;

  int NonBlanks, i, j;

  struct Data NetData;

  FILE* SIL;
  char* station_info_list = "fest_sites_sorted";

  if ((SIL = fopen(station_info_list,"r")) == NULL)
  {
    perror ("Cannot open station list input file");
    exit(1);
  }

  while( fgets(buf, MAXLINE, SIL) )
  {
    strncpy(NetData.Description, "     ", 24);
    NetData.Elevation = 0.0; 
    NonBlanks = 0;

	/* Get Description */
    for(i=0; i<21; i++) TempDescrip[i] = buf[i];

    for(i=0; i<21; i++)
      if( TempDescrip[i] != ' ' ) NonBlanks += 1;

    if( NonBlanks != 0 )
      strcpy(ValidDescription, TempDescrip);

	/* Get Id Name */
    for (j=21;j<27;j++) TempId[j-21]=buf[j]; 
    for (j=28;j<30;j++) TempId[j-22]=buf[j]; 
        /* j = 27 is a space in station file which needs to be removed */
    TempId[8]='\0';

	/* Get Elevation */
    sscanf(buf+62,"%f", &TempElev);

    if( strcmp(Id, TempId) == 0 )
    {
      strcpy(NetData.Description, ValidDescription);
      NetData.Elevation = TempElev / 10;
      fclose(SIL);
      return( NetData );
    } 
  } /* end while fgets */

  /* if id was never found close file and return invalid struct */
  strcpy(NetData.Description, "INVALID");
  NetData.Elevation = -999.9;
  fclose(SIL);
  return( NetData );

}  /* end GetStationFileInfo */

 
int Print_Station_Record(FILE* STAT_LIST, int StartYear, int StartMonth, 
                         int StartDay, int StopYear, int StopMonth, int StopDay,
                         char* Id, char* Frequency, float Lat, float Lon)
{

  char* Fmt = "%-15s %10d %10.5f %11.5f %3d %5d %-50s 19%2d%02d%02d 19%2d%02d%02d %-2s %02d %-3s %6.2f %c %4d %-15s %9.1f %c %-15s\n";

  char* Project = "GCIP/GIST";
  char* Country = "US";
  char* County  = "???";
  char TempState[2];
  int Platform =  46;
  int State;
  float TimeZone = 0.0;  
  struct Data NetData;

  NetData = GetStationFileInfo(Id);
 
/* NOTE: the following returns a state code that is different than */
/* the state codes used for GIST, some conversion needs to be added */
 
  TempState[0] = Id[0];
  TempState[1] = Id[1];
  State = atoi(TempState);

  fprintf( STAT_LIST, Fmt, Project, 0, Lat, Lon, 0, 2, NetData.Description, 
                      StartYear, StartMonth, StartDay, 
                      StopYear, StopMonth, StopDay, Country, State,
                      County, TimeZone, 'n', Platform, Frequency,
                      NetData.Elevation, 'f', Id );


}  /* End Print_Station_Record */
