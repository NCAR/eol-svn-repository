/*----------------------------------------------------------------
DisplayBean.java:  This file defines the DisplayBean class.  An
  instance of this class is defined in session scope of the dln
  app to maintain the characteristics of the current display.
  This instance is also passed to the DatasetDBA class to pull
  the datasets from the database.

Properties:
===========
 displayView - the current listing view type
	   Possible values: PROJECT, LOADERS, CHECKERS, RECENT 

 displayId - the id in the database of the project, loader
   or checker being displayed in the listing view
	 This is a string variable, this is generally the name
   of the use or project.

 datasetCount - the number datasets in the current listing
   view

 sortField - the field by which to sort the listing
   Possible values: DATE, TITLE, LOADING, CHECKING, PROJ,
									 	STORM_ID

 sortDirection - the direction by which to sort the listing
   Possible values: ASC, DESC


 currentView - the current view
  Possible values: LISTING, DATASET 

 currentDatasetNumber - the dataset the user is viewing if currentView
  is set to DATASET, if not should be set to -1

Important Functions:
===================
	public void setDefaults()
    -Sets the default display values

  **There are getter and setter functions for all properties listed
    above.

Author: Dan Sullivan
Date: 1-2-2002
----------------------------------------------------------------*/

package dln.format;

public class DisplayBean
{
	private int displayView,			
							datasetCount,
							sortField,
							sortDirection,
							currentView,
							currentDatasetNumber;

	private String displayName, displayId;

	private boolean showChecked, showDocumented, showLoaded, showMaster;

	// Constant values for the displayView
	public static final int PROJECT = 1, 
													LOADERS = 2, 
													CHECKERS = 3, 
													RECENT = 4;

	// Constant values for the sortDirection
	public static final int ASC = 1, DESC = 2;

	// Constant values for the sortField
	public static final int DATE = 5, 
										TITLE = 6, 
										LOADING = 7, 
										CHECKING = 8, 
										PROJ = 9, 
									 	STORM_ID = 10;	

	// Constant values for the currentView 
	public static final int LISTING = 1, DATASET = 2;	
		
	public DisplayBean()
	{
		setDefaults();
		showChecked = true;
		showDocumented = true;
		showLoaded = true;
		showMaster = true;
	}

	public DisplayBean( int dView )
	{
		setDefaults( dView );
		showChecked = true;
		showDocumented = true;
		showLoaded = true;
		showMaster = true;
	}

	public void setDefaults()
	{
		setDefaults( -1 );
	}

	public void setDefaults( int dView )
	{
		displayView = dView;
		displayName = "";
		sortField = DATE;
		sortDirection = DESC;
		datasetCount = 0;

		currentDatasetNumber = 0;	
	}


	public int getDisplayView()
	{ return displayView; }

	public String getDisplayName()
	{ return displayName; }

	public String getDisplayId()
	{ return displayId; }

	public int getDatasetCount()
	{ return datasetCount; }

	public int getSortField()
	{ return sortField; }

	public int getSortDirection()
	{ return sortDirection; }

	public int getCurrentView()
	{ return currentView; }

	public int getCurrentDatasetNumber()
	{ return currentDatasetNumber; }

	public boolean getShowChecked()
	{ return showChecked; }

	public boolean getShowDocumented()
	{ return showDocumented; }

	public boolean getShowLoaded()
	{ return showLoaded; }

	public boolean getShowMaster()
	{ return showMaster; }

	public void setDisplayView( int d )
	{ 
		displayView = d; 
	}

	public void setDisplayName( String d )
	{ 
		displayName = d; 
	}

	public void setDisplayId( String id )
	{ 
		displayId = id; 
	}

	public void setDatasetCount( int c )
	{ datasetCount = c; }

	public void setSortField( int f )
	{ 
		sortField = f; 
	}

	public void setSortDirection( int d )
	{ 
		sortDirection = d; 
	}

	public void setCurrentView( int v )
	{ currentView = v; }

	public void setCurrentDatasetNumber( int n )
	{ currentDatasetNumber = n; }

	public void setShowChecked( boolean b )
	{ 
		showChecked = b; 
	}

	public void setShowDocumented( boolean b )
	{ 
		showDocumented = b; 
	}

	public void setShowLoaded( boolean b )
	{
		showLoaded = b; 
	}

	public void setShowMaster( boolean b )
	{
		showMaster = b;
	}
}
