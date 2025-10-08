/* MakeQCF_record 
**
** The first s/w to work on the NESOB NetCDF data.
** It opens the filename it is given, so give it the
** name of a NetCDF file from the directory it is run.
** It will read in the dimension names and sizes, and
** the variable names and types, and construct a
** suitable QCF record header file with a name given to
** it on the command line or, as default, the name
** "NESOB_qcfrec.h".  This header file is then included
** in the compile of the process_nesob s/w.  
**
** The MakeQCF_record s/w can be
** run in each directory of data, or in a higher directory
** which contains the data divided by station, but each
** with the same structure.
**
** SYNTAX:
**     MakeQCF_record <in: netCDF filename> [brief description] [<out: header filename>]
**
** 4 aug 97, ds
** rev 19 May 99, ds
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>

#include "netcdf.h"
#include "ncdump.h"
#include "dumplib.h"


void ToUpperCase(char *StringIn, char *StringOut);
static char* type_name(nc_type  type);
void get_comment(int ncid, int varid, int numvars, int numatts, struct ncatt *attPtr);
    
char *progname;

int main (int argc, char *argv[])
{
   int      ncid,                           /* Netcdf file ID */
            numdims,                        /* number of dimensions defined for this netCDF dataset */
            numvars,                        /* number of variables  */
            numgatts,                       /* number of global attributes */
            unlimdimid;                     /* ID of the unlimited dimension, if there is one */
                                            /*    otherwise -1 is returned                    */
   
   nc_type  type;                           /* Netcdf variable type */
   int nc_status;                           /* return from netcdf calls */
   int dimid, varid;
   int vardims;                             /* number of dimensions in a given variable */
   int *dimensions, i;
   int attnum;
    
   nc_type  f_type;
   size_t   f_len;                          /* Netcdf attribute lengths */
   char unlimdim_name[NC_MAX_NAME];         /* name and length for the  */
   int  unlimdim_length;                    /*    unlimited dimension   */

   char *DefineName;
   char *namePtr;
   char *strPtr;
   
   FILE *fp;
   char  input_filename[100] = "00";
   char  output_filename[100] = "00";

   char TempStr[100];   
   char TempStr2[100];
   char description_line[60];   
   struct ncatt att;
	   
   /* set up some pointers to arrays of structures to hold our metadata */  
   struct ncdim (*DimRecord)[];
   struct ncvar (*VarRecord)[];
    
   /*
    * Expect file name on the command line.
    */
   if (argc < 2)
      {
	  printf("\n\tThis software will read the metadata in a netCDF file, and write\n");
	  printf("\tout a header file of its dimensions and variables: types and sizes.\n");	
      printf("\n\tUsage: %s <filename-in> [brief description] [<name-of-file-to-create>] \n\n", argv[0]);
	  printf("   (If no name-of-file-to-create is given, NESOB_qcfrec.h is used by default)\n\n");
      exit(1);
      }

   /*
   ** Set up our file names for i/o, using 
   **  NESOB_qcfrec.h if no other name given.
   */
   strcpy (input_filename, argv[1]);
   if (argc == 4)
   		strcpy (output_filename, argv[3]);
   else
   		strcpy(output_filename, "NESOB_qcfrec.h");

   if (argc > 2)
   		strcpy(description_line, argv[2]);
   else
   		strcpy(description_line, "on a need-to-know basis.");

   printf("%s will create a QCF record header file in this directory, named %s,\nfor data from the %s file...\n", argv[0], output_filename, input_filename);

   DefineName = malloc(NC_MAX_NAME);
   if (DefineName == NULL)
        perror("malloc problem\n");
   
   /*
   ** Open the netCDF dataset and get its netCDF ID 
   */
   nc_status = nc_open(input_filename, NC_NOWRITE, &ncid);
   if (nc_status != NC_NOERR) {
       error("%s: %s", input_filename, nc_strerror(nc_status));
   }

   /*
   ** Given the netCDF ID, get the number of dimensions, number of variables, 
   **   number of global attributes, and the ID of the unlimited dimension     
   */
   NC_CHECK( nc_inq(ncid, &numdims, &numvars, &numgatts, &unlimdimid)); 
   
   printf("********* numdims = %d\n", numdims);
   printf("********* numvars = %d\n", numvars);
   
   if (unlimdimid == -1)
        perror("The unlimited dimension is usually time.  This dataset has none.  Can't process.\n");
   else {
        NC_CHECK( nc_inq_dim(ncid, unlimdimid, unlimdim_name, &unlimdim_length));
        printf("There are %ld records, and the unlimited dimension is %s.\n", (long)unlimdim_length, unlimdim_name);
   }

   /*
   ** Allocate space for an array of dimension records
   */
   DimRecord = calloc(numdims, sizeof(struct ncdim));
   if (DimRecord == NULL)
        perror("calloc problem\n");

   /*
   ** Dimension IDs are numbered from 0 to the number of dimensions - 1
   ** Given the dimension ID, get the dimension name and length
   */
   for (dimid = 0; dimid < numdims; dimid++) {
        NC_CHECK( nc_inq_dim(ncid, dimid, (*DimRecord)[dimid].name, &(*DimRecord)[dimid].size));
   }

   /*
   ** Allocate space for an array of variable records
   */
   VarRecord = calloc(numvars, sizeof(struct ncvar));
   if (VarRecord == NULL)
        perror("calloc problem\n");
      
   /*
   ** Variable IDs are numbered from 0 to the number of variables - 1
   ** Given the variable ID, get the variable name, type, shape, and
   **    the number of attributes assigned to each variable.
   */
   for (varid = 0; varid < numvars; varid++) {
        NC_CHECK( nc_inq_var(ncid, varid, (*VarRecord)[varid].name, &(*VarRecord)[varid].type, 
                  &(*VarRecord)[varid].ndims, (*VarRecord)[varid].dims, &(*VarRecord)[varid].natts));
   }

   /*
   ** Let's make that qcf record header
   */
   
   if ((fp = fopen(output_filename, "w")) == NULL) 
       perror("Can't open file\n.");

   fprintf(fp, "/*----------------------------------------------------------\n");
   fprintf(fp, " * %s - This is the header file containing the \n", output_filename);
   fprintf(fp, " *            Quality Control Format (qcf) record definition\n");
   fprintf(fp, " *            for %s\n", description_line);
   fprintf(fp, " *---------------------------------------------------------*/\n");
   fprintf(fp, "#ifndef NESOB_QCFREC_H \n");
   fprintf(fp, "#define NESOB_QCFREC_H \n\n");

   fprintf(fp, "#define NUMDIMS  %d\n", numdims);
   fprintf(fp, "#define NUMVARS  %d\n", numvars);
   
   for(dimid = 0; dimid < numdims; dimid++) {
       if (strcmp((*DimRecord)[dimid].name, unlimdim_name) != 0) {
		   ToUpperCase((*DimRecord)[dimid].name, DefineName);
           fprintf(fp, "#define  %s   %d\n", DefineName, (*DimRecord)[dimid].size);
	   } 
   }

   fprintf(fp, "\n");
  
   fprintf(fp, "typedef struct qcfrec { \n");
   fprintf(fp, "    int year_nom;                               /* Nominal year for date of observation */\n");
   fprintf(fp, "    int month_nom;                              /* Nominal month for date of observation */\n");
   fprintf(fp, "    int day_nom;                                /* Nominal day for date of observation */\n");
   fprintf(fp, "    int hour_nom;                               /* Nominal hour of observation */ \n");
   fprintf(fp, "    int minute_nom;                             /* Nominal minute of observation */ \n");
   fprintf(fp, "    int second_nom;                             /* Nominal second of observation */ \n");
   fprintf(fp, "    int year;                                   /* Observation year for date of observation */ \n");
   fprintf(fp, "    int month;                                  /* Observation month for date of observation */ \n");
   fprintf(fp, "    int day;                                    /* Observation day for date of observation */ \n");
   fprintf(fp, "    int hour;                                   /* Observation hour of observation */ \n");
   fprintf(fp, "    int minute;                                 /* Observation minute of observation */ \n");
   fprintf(fp, "    int second;                                 /* Observation second of observation */ \n");
   fprintf(fp, "    char qnet[11];                              /* network identifier (platform abbrev.) */ \n");
   fprintf(fp, "    char statn[15];                             /* station identifier */ \n");
   fprintf(fp, "    int occur;                                  /* station occurence */ \n");

   for(varid=0; varid < numvars; varid++) {
       if ((*VarRecord)[varid].ndims >= 1) {
           if ((*VarRecord)[varid].ndims > 1) {
               sprintf(TempStr, "    %s %s", type_name((*VarRecord)[varid].type), (*VarRecord)[varid].name);
               for (vardims=1; vardims < (*VarRecord)[varid].ndims; vardims++) {
		           ToUpperCase((*DimRecord)[(*VarRecord)[varid].dims[vardims]].name, DefineName);
				   sprintf(TempStr2, "[%s]", DefineName);
                   strcat(TempStr, TempStr2);
		       }
           } else {
               if ((*VarRecord)[varid].dims[0] != unlimdimid) { 
                   ToUpperCase((*DimRecord)[(*VarRecord)[varid].dims[0]].name, DefineName);
                   sprintf(TempStr, "    %s %s[%s]", type_name((*VarRecord)[varid].type), (*VarRecord)[varid].name, DefineName);
               } else
                   sprintf(TempStr, "    %s %s", type_name((*VarRecord)[varid].type), (*VarRecord)[varid].name);
           } 
       } else
           sprintf(TempStr, "    %s %s", type_name((*VarRecord)[varid].type), (*VarRecord)[varid].name);
	   strcat(TempStr, ";");	   
	   /* put in the long_name attribute from the variable as a comment */	   
	   get_comment(ncid, varid, numvars, (*VarRecord)[varid].natts, &att);
	   if (att.string) {
		   strcpy(TempStr2, TempStr);
		   strncat(TempStr2, "                                                         ", 47 - strlen(TempStr2));
           fprintf(fp, "%s /* %s */ \n", TempStr2, att.string);
		   free(att.string);
	   }
	   else
	       fprintf(fp, "%s\n", TempStr2);	        
   }
   fprintf(fp, "} QCFREC;\n"); 
   fprintf(fp, "\n#endif /* NESOB_QCFREC_H */");
   fclose(fp);
}


void ToUpperCase(char *StringIn, char *StringOut)
{
		   char *strPtr;
		   char *namePtr;
		   
		   strPtr = StringIn;
		   namePtr = StringOut;
		   while(*strPtr) 
		       *namePtr++ = (char)toupper(*strPtr++);
		   *namePtr = '\0';
}


static char * type_name(nc_type type) 
{
    switch (type) {
      case NC_BYTE:
	    return "byte";
      case NC_CHAR:
	    return "char";
      case NC_SHORT:
	    return "short";
      case NC_INT:
	    return "int";
      case NC_FLOAT:
	    return "float";
      case NC_DOUBLE:
	    return "double";
      default:
	    error("type_name: bad type %d", type);
	    return "bogus";
    }
}


void get_comment(int ncid, int varid, int numvars, int numatts, struct ncatt *attPtr)
{

   /*
   ** Now we know the number of attributes for each variable.
   ** Next, get the name for each attribute of each variable,
   **   and using that, get the length and type of the attribute.
   ** If attribute name = "long_name" print it as a comment in header.  
   */
   
   int attnum;
  
   attPtr->string = '\0';		
   for (; varid < numvars; varid++) {
       for (attnum=0; attnum < numatts; attnum++) {
          NC_CHECK( nc_inq_attname(ncid, varid, attnum, attPtr->name));
          NC_CHECK( nc_inq_att(ncid, varid, attPtr->name, &attPtr->type, &attPtr->len));
		  if (strcmp(attPtr->name, "long_name") == 0) {
   			  attPtr->string = (char *) malloc(attPtr->len);
	  		  if (!attPtr->string) {
	              error("Out of memory");
		          NC_CHECK(nc_close(ncid));
	          }
		      NC_CHECK( nc_get_att_text(ncid, varid, attPtr->name, attPtr->string));
			  return;
		  }
       }
   }
}
