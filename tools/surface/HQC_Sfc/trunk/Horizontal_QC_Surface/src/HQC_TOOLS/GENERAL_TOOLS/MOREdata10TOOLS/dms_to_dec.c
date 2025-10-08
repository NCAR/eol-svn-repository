#ifndef lint
static char *rcsid = "$Id$";
#endif


/*
 * $Log$
 *
 */

/*----------------------------------------------------------------------
 * dms_to_deg.c - This code converts dms to deg.
 *
 * INPUT : 
 *
 * OUTPUT: 
 *
 * 000  01 Dec 93 lec
 *    Created.
 *---------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>


/*
 * STRIPLINE is macro to move file pointer past next newline.
 */
#define STRIPLINE(file) {int c; while((c=getc(file))!='\n')if(c==EOF)break;}

#define MAX_CHARS 256


/*----------------------------------------------------------------------
 *read_record() 
 *
 * 000 09 Nov 93 lec
 *    Created.
 *---------------------------------------------------------------------*/
void read_record( /*in/out*/ FILE       **data_stream, 
                  /*out*/    char       new_line[MAX_CHARS])
   {
   int   j;
   FILE  *input_stream;
   char  c;

   input_stream = *data_stream;

   for (j=0;j<MAX_CHARS;j++) new_line[j]='\0';

   j = -1;
   while((c=getc(input_stream))!='\n' && j< MAX_CHARS)
      {
      if(c==EOF)break;
      new_line[++j] = c;

      } /* while */

   *data_stream = input_stream;

   } /* read_record() */

/*----------------------------------------------------------------------
 * main()
 *
 * 000 09 Nov 93 lec
 *    Created.
 *---------------------------------------------------------------------*/
int main()
   {
   /* local variables */
   FILE    *input_stream; 
   FILE    *output_stream;

   char    input_file_name[MAX_CHARS] = "\0";
   char    output_file_name[MAX_CHARS] = "\0";

   int     j, i, k;

   char     latdms[10],
            londms[10];

   float    deg, min, sec;
   float    lat, lon;

   char     new_line[MAX_CHARS];
   char     name[81];

   char     junkstr1[MAX_CHARS];
   char     junkstr2[MAX_CHARS];
   char     tmpstr[10];


   printf ("Enter input file name:\n");
   scanf ("%s", input_file_name);
   sprintf (output_file_name, "%-s.out", input_file_name);

   if (( input_stream = fopen(input_file_name, "r")) == NULL) 
      perror ("Error: Can't open input_file");

   if (( output_stream = fopen(output_file_name, "w")) == NULL)
      perror ("Error: Can't open output_file");

   /*
    * Process all data. 
    */
   while (!feof(input_stream))
      {
      for (i=0; i<81; i++) name[i] = '\0';
      for (i=0; i<10; i++) latdms[i] = '\0';
      for (i=0; i<10; i++) londms[i] = '\0';
      for (i=0; i<MAX_CHARS; i++) junkstr1[i] = '\0';
      for (i=0; i<MAX_CHARS; i++) junkstr2[i] = '\0';

      read_record( &input_stream, new_line);

      if (!strncmp (new_line, "   ", 3) || 
          !strncmp (new_line, "\0", 1)  ||
          !strncmp (new_line, "\n", 1))  break;

      /* printf ("new_line: %sxxx\n", new_line); */

      strncpy (junkstr1, new_line,29);
      printf ("junkstr1: %-sxxx\n", junkstr1); 

      strncpy (latdms, &new_line[29],6);
      strncpy (londms, &new_line[37],7);
      printf ("latdms, londms: %-sxxx %-sxxx\n", latdms, londms); 

      strncpy (junkstr2, &new_line[47], MAX_CHARS);
      printf ("junkstr2: %-sxxx\n", junkstr2); 

      /*
       * Pick out lat and lon and convert from dms to decimal.
       */
      for (i=0; i<10; i++) tmpstr[i] = '\0';

      strncpy(tmpstr, latdms, 2);
      tmpstr[2] = '\0';
      deg = atof(tmpstr);
 
      strcpy(tmpstr,"");
      strncpy(tmpstr, &latdms[2], 2);
      tmpstr[2] = '\0';
      min = atof(tmpstr);
 
      strcpy(tmpstr,"");
      strncpy(tmpstr, &latdms[4], 2);
      tmpstr[2] = '\0';
      sec = atof(tmpstr);
 
      min = min + sec/60.0;
      lat = deg + min/60.0;

      printf ("lat: sec, deg, min, lat:: %f %f %f %f\n", sec, deg, min , lat);


      /*
       * Now form the longitude.
       */
      strcpy(tmpstr,"\0\0\0\0\0");
      strncpy(tmpstr, &londms[0], 3);
      tmpstr[3] = '\0';
      deg = atof(tmpstr);
 
      strcpy(tmpstr,"");
      strncpy(tmpstr, &londms[3], 2);
      tmpstr[2] = '\0';
 
      min = atof(tmpstr);
 
      strcpy(tmpstr,"");
      strncpy(tmpstr, &londms[5], 2);
      tmpstr[2] = '\0';
      sec = atof(tmpstr);
 
      min = min + sec/60.0;
      lon = -1.0*(deg + min/60.0); /* this statement assumes western hemisphere.*/

      printf ("lon: sec, deg, min, lon:: %f %f %f %f\n", sec, deg, min , lon);

      fprintf (output_stream, "%-s%9.5f  %10.5f%-s\n",
               junkstr1, lat, lon, junkstr2);

      } /* while */


   if (fclose (input_stream) == EOF)
      perror ("Can't close input_stream");

   if (fclose (output_stream) == EOF)
      perror ("Can't close output_stream");

   }  /* main() */
