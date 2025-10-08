
/*
 * $Log: GraphProc.h,v $
 * Revision 1.1  1992/12/03  00:51:58  mark
 * Initial revision
 *
 *
 */

/* $Id: GraphProc.h,v 1.1 1992/12/03 00:51:58 mark Exp $ */
/*
 * Graphics process definitions.
 */
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

/*
 * This flag is set when something happens which should abort an ongoing
 * plot operation.
 */
extern bool Abort;
/*
 * Our name.
 */
extern char Ourname[40];

/*
 * Keep the variable table around, since we use it at times.
 */
extern stbl Vtable;

/*
 * Two plot descriptions are maintained in the graphics process -- the
 * "current" plot description, and the defaults table.
 */
extern plot_description Pd, Defaults;

/*
 * The current plot parameters.
 */
extern ztime PlotTime;		/* Time currently shown on the screen	*/
extern enum pmode PlotMode;	/* The current plot mode		*/
extern bool MovieMode;		/* Movie mode?				*/
/*
 * Post processing stuff.
 */
extern int PostProcMode;	/* Post processing mode?		*/
extern ztime PostProcTime;	/* Post processing mode history time	*/
/*
 * Needed for opening the FrameFile.
 */
extern char FrameFilePath[40];	/* Path to the FrameFile 		*/
extern int  FrameFileFlag;	/* True when FrameFile should be opened */
extern int  MaxFrames;		/* Maximun number of frames		*/
/*
 * Our plot type and user plot limits
 */
# define Pltype PlotType	/* temp kludge jc */
extern int	PlotType;
extern float	Xlo, Xhi, Ylo, Yhi;
extern float	Alt;		/* CAP plot altitude			*/

/*
 *  Maximum number of frames in the frame cache.
 */
# define NCACHE 100

/*
 * How much of the screen is used for the top annotation.
 */
# define TOPANNOTHEIGHT	0.035
/*
 * The state of the window.
 */
enum wstate { UP, DOWN };
extern enum wstate WindowState;

/*
 * A couple of variables for passing event locationns out of UserEvent.  You
 * should only use these if your are sure you're being called as a result
 * of a user event -- preferably a pointer event -- or they may fool you.
 */
extern int	Event_X, Event_Y;
/*
 * Window specific stuff.  We protect it in an ifdef so that not all the
 * code has to include all those files.
 */
# ifdef _XtIntrinsic_h
extern GC Gcontext;			/* A global graphics context	*/
extern Widget Top;			/* The top level widget		*/
extern Widget Graphics, GrShell;	/* The graphics widget		*/
extern Display *Disp;			/* The display we use		*/
extern XtAppContext Actx;		/* The application context	*/
extern Cursor BusyCursor, NormalCursor;	/* Our cursors		*/
# endif
extern int FrameCount;			/* How many frames?		*/
extern int DisplayFrame;		/* Frame to display		*/
extern int DrawFrame;			/* Frame to draw in		*/

/*
 * Routines of interest.
 */
/* Basic graphic utilities */
extern int SetLWidth FP((char *, char *, char *, int));
extern void FixLWidth FP((int));
extern void FixForeground FP((long));
extern void SetClip FP ((int));
extern void ResetGC FP ((void));
extern void SetColor FP ((char *, char *, char *, char *));

/* Plot control modules. */
extern void pc_CancelPlot FP ((void));
extern void pc_PlotHandler FP ((void));
extern void pc_ParamChange FP ((char *));
extern int pc_TimeTrigger FP ((char *));

/* Plot executive modules. */
extern void px_PlotExec FP ((char *));
extern void px_GlobalPlot FP ((ztime *));
extern void px_FixPlotTime FP ((ztime *));
extern char *px_FldDesc FP ((char *, char *));

/* Grid access */
extern bool ga_GridBBox FP ((ztime *, char *, float *, float *, float *,
		float *));
extern void ga_RotateGrid FP ((float *, float *, int, int));
extern bool ga_AvailableAlts FP ((ztime *, char *, float *, int *));
extern float *ga_GetGrid FP ((ztime *, char *, char *, int *, int *, float *,
		float *, float *, float *, float *));

/* Frame cache routines */
extern void fc_InvalidateCache FP ((void));
extern void fc_UnMarkFrames FP ((void));
extern void fc_CreateFrameFile FP ((void));
extern void fc_SetNumFrames FP ((int));
extern char *fc_GetInfo FP ((int));
extern void fc_AddFrame FP ((ztime *, int));
extern int fc_LookupFrame FP ((ztime *));
extern int fc_GetFrame FP ((void));
extern void fc_MarkFrames FP ((ztime *, int));

/* Movie control */
extern void mc_DefMovieWidget FP ((void));
extern void mc_ParamChange FP ((void));
extern void mc_PDChange FP ((void));
extern void mc_Dial FP ((int));

/* Icons */
extern void I_DoIcons FP ((void));
extern void I_ColorIcons FP ((char *));
extern int ov_PositionIcon FP ((char *, int, int, int));

/* User events */
extern void Ue_Override FP ((void (*) (), void (*) (), void (*) ()));
extern void Ue_ResetOverride FP ((void));

/* Annotation utilities */
extern void An_AddAnnotProc FP ((void (*) (), char *, char *, int, int,
		int, int));
extern void An_DoSideAnnot FP (());
extern void An_ColorBar FP ((char *, char *, int, int, int));
extern void An_ColorNumber FP ((char *, char *, int, int, int));
extern void An_ColorVector FP ((char *, char *, int, int, int));
extern void An_ColorString FP ((char *, char *, int, int, int));
extern int An_GetLeft FP (());
extern void An_GetSideParams FP ((char *, float *, int *));
extern void An_ResetAnnot FP ((int));
extern void An_SetScale FP ((double));
extern void An_AnnotLimits FP ((int *, int *, int *, int *));
extern void An_SAUsed FP ((int));

/* Coord space transformations */
extern void cvt_ToXY FP ((double, double, float *, float *));
extern void cvt_ToLatLon FP ((double, double, float *, float *));
extern void cvt_GetOrigin FP ((float *, float *));
extern bool cvt_Origin FP ((double, double));

/* Other stuff */
extern int GetLocation FP ((char *, ztime *, Location *));
extern int AgeCheck FP ((char *, ztime *));
extern void sync FP ((void));
extern int  reset_limits FP ((char *, char *, char *));
extern void eq_ResetAbort FP ((void));
extern void eq_ReturnPD FP ((void));
extern void tr_InitAcWidget ();

/* This stuff contains window system oriented stuff, so is only brought
   in if this module is doing X things. */
# ifdef _XtIntrinsic_h
	extern bool ct_LoadTable FP ((char *, XColor**, int *));
	extern void ct_FreeColors FP ((void));
	extern void ct_DeleteTable FP ((char *));
	extern int ct_GetColorByName FP ((char *, XColor *));
	extern int ct_GetColorByRGB FP ((XColor *));
	extern void An_TopAnnot FP ((char *, Pixel));
	extern void An_GetTopParams FP ((XColor *, int *));
	extern Widget LeftRightButtons FP ((Widget, void *,XtTranslations));
	extern void draw_vector FP ((Display *, Drawable, GC, int, int,
		double, double, double));
# endif
