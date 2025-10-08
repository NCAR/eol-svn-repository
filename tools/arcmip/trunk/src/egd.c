/* egd.c - play with ease-grid image, based on some sample code from gd */

#include <stdio.h>
#include <string.h>
#include "gd.h"
#include "gdfontt.h"

int main(int argc, char **argv)
{
FILE *in;
FILE *out;
gdImagePtr im;
int i;
int black,white,red,green,blue;

if (argc != 3) {
 fprintf(stderr, "Usage: egd in.gif out.gif\n");
 exit(1);
 }

	im = 0;
	in = fopen(argv[1], "rb");	
	if (!in) {
		fprintf(stderr,
			"Error: can't open file %s.\n", argv[1]);
	}
	/* Now load the image. */	
	im = gdImageCreateFromGif(in);
	fclose(in);
	/* If the load failed, it must not be a GIF file. */
	if (!im) {
		fprintf(stderr,
			"Error: %s is not a valid gif file.\n", argv[1]);
		exit(1);	
	}


black = gdImageColorAllocate(im, 0, 0, 0);
white = gdImageColorAllocate(im, 255, 255, 255);
red = gdImageColorAllocate(im, 255, 0, 0);
green = gdImageColorAllocate(im, 0, 255, 0);
blue = gdImageColorAllocate(im, 0, 0, 255);

gdImageRectangle(im,100,100,260,260,white); /* CASPR */
gdImageRectangle(im,113,101,212,192,white); /* my ARCMIP EASE-Grid */
gdImageRectangle(im,118,106,207,187,white); /* domain.txt */
gdImageFilledRectangle(im,161,144,163,146,white); /* "center" 162,145 = 72N 153W */
gdImageFilledRectangle(im,179,179,181,181,white); /* "pole" 180,180 = 90N 0W */

gdImageString(im,gdFontTiny,114,194,"ARCMIP EASE-Grid 50km",white);
gdImageString(im,gdFontTiny,101,251,"CASPR-ARCMIP EASE GRID data set",white);
gdImageString(im,gdFontTiny,119,107,"domain.txt",white);
gdImageString(im,gdFontTiny,161,147,"72N153W",white);
gdImageString(im,gdFontTiny,179,170,"90N0W",white);

		/* Open a temporary file. */
		out = fopen(argv[2], "wb");
		if (!out) {
			fprintf(stderr,
				"Unable to write to %s -- exiting\n",argv[2]);
			exit(1);
		}
		/* Write the new gif. */
		gdImageGif(im, out);
		fclose(out);

	/* Delete the image from memory. */
	if (im) {
		gdImageDestroy(im);
	}
	/* All's well that ends well. */
	return 0;
}

