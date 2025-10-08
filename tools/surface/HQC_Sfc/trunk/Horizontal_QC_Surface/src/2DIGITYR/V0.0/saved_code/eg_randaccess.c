/* randr.c */
/* reads one agent's record, selected by user, from file */
#include "stdio.h"
main()
{
	struct
	  {
	  char name[40];	/* name */
	  int agnumb;		/* code number */
	  double height;	/* height */
	  }  agent;

	FILE *fptr;
	int recno;		/* record number */
	long int offset;	/* must be long */


	if ( (fptr=fopen("agents.rec","r"))==NULL ) 
           {
	   printf("Can't open file agents.rec"); 
           exit();
           }

	printf("Enter record number: ");	/* get record num */
	scanf("%d", &recno);

	offset = recno * sizeof(agent);		/* find offset */

	if(fseek(fptr, offset, 0) != 0)		/* go there */
	   { 
           printf("Can't move pointer there."); 
           exit();
           }

	fread(&agent,sizeof(agent,1,fptr);	/* read record */

	printf("\nName: %s\n", agent.name);	/* print name */
	printf("Number: %03d\n", agent.agnumb);	/* print number */
	printf("Height: %.2lf\n", agent.height); /* print height */
	fclose(fptr);
}

