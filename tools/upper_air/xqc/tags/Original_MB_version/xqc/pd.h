
/*
 * $Log: pd.h,v $
 * Revision 1.1  1992/12/03  00:52:53  mark
 * Initial revision
 *
 *
 */

/*
 * Plot description related stuff.
 *
 * $Id: pd.h,v 1.1 1992/12/03 00:52:53 mark Exp $
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
 * The format of a raw plot description.  This stuff needs to be moved around
 * so the format is public.
 */
typedef struct rawpd
{
	int	rp_len;		/* Length of the PD data		*/
	char 	*rp_data;	/* The actual information		*/
} raw_plot_description;


/*
 * Internal plot descriptions are hidden.
 */
typedef void *plot_description;


/*
 * The PD routines.
 */
# ifdef __STDC__
	plot_description pd_Load (raw_plot_description *raw);
	raw_plot_description *pd_Unload (plot_description pd);
	void pd_RPDRelease (raw_plot_description *raw);
	void pd_Merge (plot_description dest, plot_description src);
	bool pd_Retrieve (plot_description pd, char *comp, char *param,
		char *target, int type);
	char **pd_CompList (plot_description pd);
	void pd_Release (plot_description pd);
	plot_description pda_GetPD (char *name);
	void pda_StorePD (plot_description pd, char *name);
	bool pda_Search (plot_description pd, char *comp, char *param,
		char *qual, char *dest, int type);
	bool pda_ReqSearch (plot_description pd, char *comp, char *param,
		char *qual, char *dest, int type);
	plot_description pd_CopyPD (plot_description pd);
	void pd_Store (plot_description pd, char *comp, char *param,
		char *value, int type);
	void pd_RemoveParam (plot_description pd, char *comp, char *param);
	int pd_RemoveComp (plot_description pd, char *name);
	plot_description pd_ReadComponent (plot_description, char *, char *);
	void pd_AddComponent (plot_description, plot_description, int);
	void pd_MoveComponent (plot_description, char *, int);
	bool pd_CompExists (plot_description, char *);
# else
	plot_description pd_Load ();
	raw_plot_description *pd_Unload ();
	void pd_RPDRelease ();
	void pd_Merge ();
	bool pd_Retrieve ();
	char **pd_CompList ();
	void pd_Release ();
	plot_description pda_GetPD ();
	void pda_StorePD ();
	bool pda_Search ();
	bool pda_ReqSearch ();
	plot_description pd_CopyPD ();
	void pd_Store ();
	void pd_RemoveParam ();
	int pd_RemoveComp ();
	plot_description pd_ReadComponent ();
	void pd_AddComponent ();
	void pd_MoveComponent ();
	bool pd_CompExists ();
# endif
