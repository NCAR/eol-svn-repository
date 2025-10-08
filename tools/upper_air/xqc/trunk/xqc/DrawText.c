
/*
 * $Log: DrawText.c,v $
 * Revision 1.1  1992/12/03  00:51:49  mark
 * Initial revision
 *
 *
 */

/*
 * Text drawing into an X11 Drawable.  Use an X font if possible, otherwise,
 * draw the text ourselves using a stroke font.  We need this so we can use 
 * rotated text.
 */
static char *rcsid = "$Id: DrawText.c,v 1.1 1992/12/03 00:51:49 mark Exp $";

/*		Copyright (C) 1987,88,89,90,91 by UCAR
 *	University Corporation for Atmospheric Research
 *		   All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced
 * or used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular purpose.
 * UCAR does not indemnify any infringement of copyright, patent, or trademark
 * through use or modification of this software.  UCAR does not provide 
 * maintenance or updates for its software.
 */

# include <math.h>
# include <X11/Intrinsic.h>
# include <ui.h>
# include <message.h>
# include "DrawText.h"
# include "sfont_1.c"
#include "xqc.h"

# define DEG_TO_RAD(x)	((x) * 0.017453292)
# define ALL_BITS	0xFFFFFFFF

/*
 * X font stuff
 */
# define MAXFONTS	30
# define MAXFONTHEIGHT	40
# define FONTPAT	"-*-helvetica-medium-r-*-*-*-*-*-*-*-*-*-*"

static char		**Fontnames = 0;
static XFontStruct	*Fonts;
static int		Nfonts;
static short		Fndx[MAXFONTHEIGHT];

/*
 * Our local graphics context
 */
static GC		Local_gc = (GC) 0;

/*
 * Label blanking flag.
 */
static int BlankLabel = TRUE;

/*
 * Forward declarations
 */
# ifdef __STDC__
	void	DT_XTextInfo (char *, int, int, int, int *, int *, int *);
	void	DT_TextInfo (char *, double, int, int, float *, float *, 
			float *, float *);
	bool	DT_HaveXFont (int);
# else
	void	DT_XTextInfo (), DT_TextInfo ();
	bool	DT_HaveXFont ();
# endif




void
DrawText (d, gc, x, y, text, rot, scale, hjust, vjust)
Drawable	d;
GC	gc;
int	x, y, hjust, vjust;
String	text;
float	rot, scale;
/*
 * Draw a text string into an X Drawable. Use an X font if possible, 
 * otherwise, draw it using a stroke font.
 *
 * ENTRY:
 *	w	widget to use for size info
 *	d	X Drawable where text is to go (must be associated with w)
 *	gc	Graphics context to use (the font will be ignored, since
 *		DrawText chooses its own, but the rest is used)
 *	x, y	pixel location for the text
 *	text	text string to draw
 *	rot	text rotation in degrees counterclockwise from horizontal
 *	scale	text height relative to total height of the Drawable (0.0-1.0)
 * For the two justifications below, horizontal and vertical are taken with
 * respect to the rotated text
 *	hjust	horizontal justification -- JustifyLeft, JustifyRight, or
 *		JustifyCenter
 *	vjust	vertical justification -- JustifyBottom, JustifyTop,
 *		JustifyBaseline, or JustifyCenter
 * EXIT:
 *	The text string has been drawn into the Drawable
 */
{
	int			cheight, width, hoffset, voffset, i;
/*
 * Get the geometry of the widget (which must have the same geometry as 
 * the drawable)
 */
/*	XtQueryGeometry (w, NULL, &geom); */
/*
 * Desired character height (in pixels)
 */
/*	cheight = (int)(scale * geom.height);*/
	cheight = (int)(scale * 700);
/*
 * Do the text with the stroke font, if necessary
 */
	if (rot != 0.0 || ! DT_HaveXFont (cheight))
	{
		DT_StrokeText (d, gc, x, y, text, rot, scale, hjust, vjust);
		return;
	}
/*
 * Get our local graphics context if we don't have one yet
 */
	if (! Local_gc)
		Local_gc = XCreateGC (XtDisplay (w), d, 0, NULL);
/*
 * Copy the user's GC into the local one and set the font based on the 
 * selected text height
 */
	XCopyGC (XtDisplay (w), gc, ALL_BITS, Local_gc);
	XSetFont (XtDisplay (w), Local_gc, Fonts[Fndx[cheight]].fid);
/*
 * Find the text width, horizontal offset, and vertical offset
 */
	DT_XTextInfo (text, cheight, hjust, vjust, &width, &hoffset, &voffset);
/*
 * Draw the text
 */
	XDrawString (XtDisplay (w), d, Local_gc, x + hoffset, y + voffset, 
		text, strlen (text));
}




void
DT_XTextInfo (text, cheight, hjust, vjust, width, hoffset, voffset)
char	*text;
int	cheight, hjust, vjust;
int	*width, *hoffset, *voffset;
/*
 * Return the width, hoffset, and voffset in pixels for the given text, 
 * height, horizontal justification, and vertical justification.  This
 * routine assumes that an X font has been loaded for the requested text
 * height.
 */
{
	*width = XTextWidth (&(Fonts[Fndx[cheight]]), text, strlen (text));
/*
 * Find the horizontal and vertical offsets based on the justification
 */
	switch (hjust)
	{
	    case JustifyLeft:
		*hoffset = 0;
		break;
	    case JustifyRight:
		*hoffset = -(*width);
		break;
	    case JustifyCenter:
		*hoffset = -(*width) / 2;
		break;
	    default:
		*hoffset = 0;
		msg_ELog (EF_PROBLEM, "BUG! Bad hjust in DrawText!");
	}

	switch (vjust)
	{
	    case JustifyTop:
		*voffset = Fonts[Fndx[cheight]].ascent;
		break;
	    case JustifyBottom:
		*voffset = 0;
		break;
	    case JustifyCenter:
		*voffset = Fonts[Fndx[cheight]].ascent / 2;
		break;
	    default:
		*voffset = 0;
		msg_ELog (EF_PROBLEM, "BUG! Bad vjust in DrawText!");
	}
}


dt_SetBlankLabel(flag)
int flag;
{
	BlankLabel = flag;
}



void
DT_StrokeText (d, gc, x, y, text, rot, scale, hjust, vjust)
Drawable	d;
GC	gc;
int	x, y, hjust, vjust;
String	text;
float	rot, scale;
/*
 * Draw a text string into an X Drawable using a stroke font
 *
 * ENTRY:
 *	w	widget to use for size info
 *	d	X Drawable where text is to go (must be associated with w)
 *	gc	X graphics context to use for drawing
 *	x, y	pixel location for the text
 *	text	text string to draw
 *	rot	text rotation in degrees counterclockwise from horizontal
 *	scale	text height relative to total height of the Drawable (0.0-1.0)
 * For the two justifications below, horizontal and vertical are taken with
 * respect to the rotated text
 *	hjust	horizontal justification
 *	vjust	vertical justification
 * EXIT:
 *	The text string has been drawn into the Drawable
 */
{
	XPoint	points[200];
	float	xoffset, yoffset;
	float	textwidth, textheight;
	char	*cp;
	int	cbot;
	float	xpos, ypos, del_x, del_y;
	float	cos_rot = cos (DEG_TO_RAD (rot));
	float	sin_rot = sin (DEG_TO_RAD (rot));
	float	pixscale;
	XPoint	box[4];
/*
 * Get the character bottom level from the stoke font data
 */
	cp = Gt_sf_1[0];
	cbot = cp[3];
/*
 * Get the x and y starting offset, width, and height for unscaled text
 */
	DT_TextInfo (text, rot, hjust, vjust, &xoffset, &yoffset, &textwidth,
		&textheight);
/*
 * Get the geometry of the widget (which must have the same geometry as 
 * the drawable)
 */
/*	XtQueryGeometry (w, NULL, &geom); */
/*
 * Find the pixel scale factor based on character height
 */
	pixscale = scale * 700 / textheight;
/*
 * Initialize the text position
 */
	xpos = (float) x + pixscale * xoffset;
	ypos = (float) y + pixscale * yoffset;
/*
 * Get our local graphics context if we don't have one yet
 */
	if (! Local_gc)
		Local_gc = XCreateGC (XtDisplay (w), d, 0, NULL);
/*
 * Make a black box where the text will be written if the BlankLabel
 * flag has been set to true.
 */
	if(BlankLabel)
	{
		box[0].x = (int)(xpos + 0.5);
		box[0].y = (int)(ypos + 0.5);

		box[1].x = (int)(xpos + 0.5 + pixscale * textwidth * cos_rot);
		box[1].y = (int)(ypos + 0.5 - pixscale * textwidth * sin_rot);

		box[2].x = (int)(xpos + 0.5 + 
		pixscale * (textwidth * cos_rot - textheight * sin_rot));
		box[2].y = (int)(ypos + 0.5 - 
		pixscale * (textwidth * sin_rot + textheight * cos_rot));

		box[3].x = (int)(xpos + 0.5 - pixscale * textheight * sin_rot);
		box[3].y = (int)(ypos + 0.5 - pixscale * textheight * cos_rot);


		XCopyGC (XtDisplay (w), gc, ALL_BITS, Local_gc);
		if (reverseColor)
		    {
		    XSetForeground (XtDisplay (w), Local_gc, 
				    WhitePixel (display, screen_number));
		    }
		else 
		    {
		    XSetForeground (XtDisplay (w), Local_gc, 
				    BlackPixel (display,
						screen_number));
		    }

		XFillPolygon (XtDisplay (w), d, Local_gc, box, 4, Convex, 
			CoordModeOrigin);
	}
/*
 * Now step through each character
 */
 	while (*text)
	{
		int	npts = 0, npair;
		char	*cdata;
	/*
	 * Locate the vector info.
	 */
	 	cp = Gt_sf_1[*text++];
		cdata = cp + 4;
	/*
	 * Now step through and draw the lines.
	 */
	 	for (npair = 0; npair < cp[0]; npair++)
		{
		/*
		 * Check for the end of a group of lines, and put out our
		 * info if this is such an end.
		 */
			if (*cdata == -128)
			{
				if (npts > 0)
					XDrawLines (XtDisplay (w), d, gc,
						points, npts, CoordModeOrigin);
				npts = 0;
				cdata += 2;
			}
		/*
		 * Otherwise throw in another set of points.
		 */
		 	else
			{
			/*
			 * Delta position (unrotated)
			 */
				del_x = pixscale * (*cdata++ - cp[2]);
				del_y = pixscale * (cbot - *cdata++);
			/*
			 * New point, with rotation
			 */
				points[npts].x = (short)(xpos +
					del_x * cos_rot + del_y * sin_rot);
				points[npts].y = (short)(ypos -
					del_x * sin_rot + del_y * cos_rot);

				npts++;
			}
		}
	/*
	 * Advance the position to the next character slot.
	 */
	 	xpos += cos_rot * ((cp[3] - cp[2]) * pixscale);
	 	ypos -= sin_rot * ((cp[3] - cp[2]) * pixscale);
	}
}




void
DT_TextBox (d, x, y, text, rot, scale, hjust, vjust, sx, sy, ex, ey)
Drawable	d;
char	*text;
int	x, y;
float	rot, scale;
int	hjust, vjust;
int	*sx, *sy, *ex, *ey;
/*
 * Find the positions for the lower left and upper right corners of
 * the box holding a piece of text as it would be drawn by DrawText.
 *
 * ENTRY:
 *	w	widget to use for size info
 *	d	X Drawable where text is to go (must be associated with w)
 *	text	text string to draw
 *	x, y	pixel location for the text
 *	rot	text rotation in degrees counterclockwise from horizontal
 *	scale	text height relative to total height of the Drawable (0.0-1.0)
 * For the two justifications below, horizontal and vertical are taken with
 * respect to the rotated text
 *	hjust	horizontal justification -- JustifyLeft, JustifyRight, or
 *		JustifyCenter
 *	vjust	vertical justification -- JustifyBottom, JustifyTop,
 *		JustifyBaseline, or JustifyCenter
 * EXIT:
 *	sx, sy	is the real start point, with justification, scaling and
 *		rotation taken into account.
 *	ex, ey	is the ending point
 */
{
	int	cheight, desc;
	char	*cp;
	float	xoffset, yoffset, pixscale;
	float	width, height;
	float	fsx, fsy;
	float	cos_rot = cos (DEG_TO_RAD (rot));
	float	sin_rot = sin (DEG_TO_RAD (rot));
/*
 * Get the geometry of the widget (and presumably of the Drawable)
 */
/*	XtQueryGeometry (w, NULL, &geom); */
/*
 * If we can draw the text using X fonts, we treat it specially
 */
	cheight = (int)(scale * 700);
	if (rot == 0.0 && DT_HaveXFont (cheight))
	{
		int	hoffset, voffset, w;

		DT_XTextInfo (text, cheight, hjust, vjust, &w, &hoffset,
			&voffset);

		*sx = x + hoffset;
		*sy = y + voffset;

		*ex = x + hoffset + w;
		*ey = y + voffset - cheight;

		return;
	}
/*
 * If we get here, it's because we have to use the stroke font.
 * Find the starting position offset, width, and height for unscaled text.
 */
	DT_TextInfo (text, rot, hjust, vjust, &xoffset, &yoffset, &width, 
		&height);
/*
 * Find the pixel scale factor based on character height
 */
	pixscale = scale * (float) 700 / (float) height;
/*
 * Find the lower left corner (starting) position with floating precision
 */
	fsx = x + pixscale * xoffset;
	fsy = y + pixscale * yoffset;
/*
 * Return the starting position as integer pixel locations
 */
	*sx = (int)(fsx + 0.5);
	*sy = (int)(fsy + 0.5);
/*
 * Return the upper right corner (ending) position as integer pixel locations
 */
	*ex = (int)(fsx + pixscale * (cos_rot*width - sin_rot*height) + 0.5);
	*ey = (int)(fsy - pixscale * (sin_rot*width + cos_rot*height) + 0.5);
}




void
DT_TextInfo (text, rot, hjust, vjust, xoffset, yoffset, width, height)
char	*text;
float	rot;
int	hjust, vjust;
float	*xoffset, *yoffset, *width, *height;
/*
 * Return the unscaled starting offset, width, and height for the given 
 * text string, given its rotation and justification
 */
{
	char	*cp;
	float	descender;
	float	hoffset, voffset;
	float	cos_rot = cos (DEG_TO_RAD (rot));
	float	sin_rot = sin (DEG_TO_RAD (rot));
/*
 * Get the character height.  This particular measure, which includes 
 * descenders, may be excessive, but we'll try it.
 */
	cp = Gt_sf_1[0];
	*height = (float)(cp[0] - cp[3]);
	descender = (float)(cp[2] - cp[3]);
/*
 * Get the unscaled text width
 */
	*width = 0.0;
	while (*text)
	{
		cp = Gt_sf_1[*text++];
		*width += (float)(cp[3] - cp[2]);
	}
/*
 * Find the vertical offset for the given justification 
 * (ignoring rotation for now)
 */
 	switch (vjust)
	{
	    case JustifyBottom:
		voffset = 0.0;
		break;
	    case JustifyBaseline:
		voffset = -descender;
		break;
	    case JustifyTop:
		voffset = -(*height);
		break;
	    case JustifyCenter:
		voffset = -(*height) / 2.0;
		break;
	}
/*
 * Find the horizontal offset for the given justification 
 * (ignoring rotation for now)
 */
 	switch (hjust)
	{
	     case JustifyLeft:
		hoffset = 0.0;
		break;
	     case JustifyRight:
		hoffset = -(*width);
		break;
	     case JustifyCenter:
		hoffset = -(*width) / 2.0;
		break;
	}
/*
 * Put in the necessary rotation
 */
	*xoffset = cos_rot * hoffset - sin_rot * voffset;
	*yoffset = -(sin_rot * hoffset + cos_rot * voffset);
}




bool
DT_HaveXFont (size)
int	size;
/*
 * Do we have an X font 'size' pixels high (within about 20%)?
 */
{
	int	i, index;
	float	ratio, bestratio;
	XFontStruct	*fs;
/*
 * Initialize if we haven't gotten the font names yet
 */
	if (! Fontnames)
	{
		char	*getenv ();
		char	*pattern = getenv ("XFONTPATTERN") ? 
				getenv ("XFONTPATTERN") : FONTPAT;
	/*
	 * Get the font names matching the desired (or default) pattern
	 */
		Fontnames = XListFontsWithInfo (XtDisplay (w), pattern,
			MAXFONTS, &Nfonts, &Fonts);
	/*
	 * Initialize the font index array
	 */
		for (i = 0; i < MAXFONTHEIGHT; i++)
			Fndx[i] = -1;
	}
/*
 * If we already have a font loaded for the requested pixel size, 
 * return TRUE
 */
	if (Fndx[size] >= 0)
		return (TRUE);
/*
 * See which one (if any) of the fonts will work best for this pixel size
 */
	bestratio = 999.0;
	for (i = 0; i < Nfonts; i++)
	{
	/*
	 * Find the ratio of actual size to desired size or its inverse,
	 * whichever is bigger
	 */
		ratio = (float)(Fonts[i].ascent + Fonts[i].descent) / size;
		if (ratio < 1.0)
			ratio = 1.0 / ratio;
	/*
	 * If the ratio is > 1.2, we don't want to use this font for the
	 * requested size
	 */
		if (ratio > 1.2)
			continue;
	/*
	 * This is the best match so far if the ratio is less than
	 * the current best ratio
	 */
		if (ratio < bestratio)
		{
			Fndx[size] = i;
			bestratio = ratio;
		}
	}
/*
 * If we didn't get a usable font return FALSE
 */
	index = Fndx[size];

	if (index < 0)
		return (FALSE);
/*
 * We got a usable font.  Load it and return TRUE
 */
	fs = XLoadQueryFont (XtDisplay (w), Fontnames[index]);
	Fonts[index] = *fs;
	return (TRUE);
}
