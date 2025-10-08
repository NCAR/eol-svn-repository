package dln.beans;

/******************************************************************************

DatasetBean.java: The DatasetBean class is a simple container class to hold
one entry in the dln:dataset table.  Each attribute has its own get and
set functions.

Functions to note:
-----------------
	public int populate( ResultSet rs ) throws SQLException
		-Populates this dataset from the given result set

	public int populate( int index, ResultSet rs ) throws SQLException
		-Populates this dataset from the given result set starting at the given 
			index

	public DatasetBean getDBPreparedBean()
		-Returns a new DatasetBean formatted to be entered into the database,
			sets various fields to null instead of the empty string and applies
			some defaults.

	public static String formatTextArea( String notes, int lineMax )
		-Formats the given string to shrink the size of line to linMax.  This
			is used to format the notes field to make things look pretty

Author: Dan Sullivan
Date: ??
******************************************************************************/

import java.util.*;
import java.io.Serializable;
import java.sql.*;

public class DatasetBean implements Serializable
{
	private int 		dsid;
	private String	stormId,
									title,
									notes,
									project,
									loader, checker, intContact,
									extContact, extEmail,										
									ingest, archive, remoteUrl; 

	java.sql.Date date;

	private int readme, master, checked, loaded;

	private UserBean loaderBean, checkerBean, intContactBean;

	public DatasetBean()
	{
		stormId = "99.999";

		title = notes = project = "";
		loader = checker = intContact = "";
		extContact  = extEmail = "";
		ingest = archive = remoteUrl = "";

		readme = master = checked = loaded = 0;

		date = null;
	}

	public int getDsid()
	{ return dsid; }

	public void setDsid( int dsid )
	{ this.dsid = dsid; }

	public String getStormId()
	{ return stormId; }

	public void setStormId( String id )
	{ stormId = id; }

	public String getTitle()
	{ return title; }

	public void setTitle( String t )
	{ title = t; }

	public String getNotes()
	{ return notes; }	

	public void setNotes( String n )
	{ notes = resolveNull(n); }

	public String getProject()
	{ return project; }

	public void setProject( String p )
	{ project = p; }

	public String getLoader()
	{ return loader; }

	public void setLoader( String l )
	{ loader = l; }

	public String getChecker()
	{ return checker; }

	public void setChecker( String c )
	{ checker = c; }

	public String getIntContact()
	{ return intContact; }

	public void setIntContact( String c )
	{ intContact = c; }

	public String getExtContact()
	{ return extContact; }

	public void setExtContact( String e )
	{ extContact = resolveNull( e ); }

	public String getExtEmail()
	{ return extEmail; }

	public void setExtEmail( String e )
	{ extEmail = resolveNull( e ); }

	public String getIngest()
	{ return ingest; }

	public void setIngest( String i )
	{ ingest = resolveNull(i); }

	public String getArchive()
	{ return archive; }

	public void setArchive( String a )
	{ archive = resolveNull(a); }

	public String getRemoteUrl()
	{ return remoteUrl; }

	public void setRemoteUrl( String url )
	{ remoteUrl = resolveNull(url); } 


	public int getReadme()
	{ return readme; }

	public boolean isReadme()
	{ return readme != 0; }

	public void setReadme( int x )
	{
		if( x == 0 || x == 1 )
			readme = x;
		else
			readme = -1;
	}

	public void setReadme( String readme )
	{
		if( readme != null )
			this.readme = 1;
		else
			this.readme = 0;
	}
	public void setReadme( boolean x )
	{
		if( x )
			readme = 1;
		else
			readme = 0;
	}

	public int getMaster()
	{ return master; }

	public boolean isMaster()
	{ return master != 0; }

	public void setMaster( int x )
	{
		if( x == 0 || x == 1 )
			master = x;
		else
			master = -1;
	}

	public void setMaster( boolean x )
	{
		if( x )
			master = 1;
		else
			master = 0;
	}

	public int getChecked()
	{ return checked; }

	public boolean isChecked()
	{ return checked != 0; }

	public void setChecked( int x )
	{
		if( x == 1 || x == 0 )
			checked = x;
		else
			checked = -1;
	}

	public void setChecked( boolean x )
	{
		if( x )
			checked = 1;
		else
			checked = 0;
	}

	public int getLoaded()
	{ return loaded; }

	public boolean isLoaded()
	{ return loaded != 0; }

	public void setLoaded( int x )
	{
		if( x == 0 || x == 1 )
			loaded = x;
		else
			loaded = -1;
	}

	public void setLoaded( boolean x )
	{
		if( x )
			loaded = 1;
		else
			loaded = 0;
	}

	public java.sql.Date getDate()
	{ return date; }

	public void setDate( String d )
	{ date = java.sql.Date.valueOf(d); }

	public void setDate( java.sql.Date date )
	{ this.date = date; }

	public void setDateToday()
	{
		date = new java.sql.Date( new java.util.Date().getTime() );
	}


	public void setLoaderBean( UserBean loaderBean )
	{ this.loaderBean = loaderBean; }

	public UserBean getLoaderBean()
	{ return loaderBean; }

	public void setCheckerBean( UserBean checkerBean )
	{ this.checkerBean = checkerBean; }

	public UserBean getCheckerBean()
	{ return checkerBean; }
	
	public void setIntContactBean( UserBean intContactBean )
	{ this.intContactBean = intContactBean; }

	public UserBean getIntContactBean()
	{ return intContactBean; }

	private String resolveNull( String str )
	{
		if( str == null )
			str = "";

		return str;	
	}

	public int populate( int index, ResultSet rs ) throws SQLException
	{
		setDsid( rs.getInt( index++ ) );
		setStormId( rs.getString( index++ ) );
		setTitle( rs.getString( index++ ) );
		setReadme( rs.getInt( index++ ) );
		setMaster( rs.getInt( index++ ) );
		setChecked( rs.getInt( index++ ) );
		setLoaded( rs.getInt( index++ ) );
		setDate( rs.getDate( index++ ) );
		setNotes( rs.getString( index++ ) );
		setProject( rs.getString( index++ ) );
		setLoader( rs.getString( index++ ) );
		setChecker( rs.getString( index++ ) );
		setIntContact( rs.getString( index++ ) );
		setExtContact( rs.getString( index++ ) );
		setExtEmail( rs.getString( index++ ) );
		setIngest( rs.getString( index++ ) );
		setArchive( rs.getString( index++ ) );
		setRemoteUrl( rs.getString( index++ ) );

		return index;
	}

	public int populate( ResultSet rs ) throws SQLException
	{
		return populate( 1, rs );
	}

	public DatasetBean getDBPreparedBean()
	{
		DatasetBean ds = new DatasetBean();

		ds.setLoaderBean( loaderBean );
		ds.setCheckerBean( checkerBean );
		ds.setIntContactBean( intContactBean );

		ds.setDsid( dsid );
		ds.stormId = blankToNull( stormId );
		ds.title =  blankToNull( title );
		ds.notes = blankToNull( notes );
		ds.project = blankToNull( project );
		ds.loader = blankToNull( loader );
		ds.checker = blankToNull( checker );
		ds.intContact = blankToNull( intContact );
		ds.extContact = blankToNull( extContact );
		ds.extEmail = blankToNull( extEmail );
		ds.ingest = blankToNull( ingest );
		ds.archive = blankToNull( archive );
		ds.remoteUrl = blankToNull( remoteUrl );

		if( date == null )
			ds.setDate( new java.sql.Date( new java.util.Date().getTime() ) );
		else
			ds.setDate( date );

		ds.setReadme( readme );
		ds.setMaster( master );
		ds.setChecked( checked );
		ds.setLoaded( loaded );

		return ds;
	}

	public String blankToNull( String str )
	{
		if( str.equals( "" ) )
			str = null;

		return str;
	}

	public static String formatTextArea( String notes, int lineMax )
	{
		int max = lineMax;

		if( notes == null )
			return notes;

		String ret = new String();
		if( notes.length() <= max )
			return notes;
		
		StringTokenizer lines = new StringTokenizer( notes, "\r\n" );
		while( lines.hasMoreTokens() )
		{
			String ret_line = new String();
			String line = lines.nextToken();

			if( line.length() > max + 1 )
			{
				StringTokenizer words = new StringTokenizer( line, " " );
				int len = 0;
				while( words.hasMoreTokens() )
				{
					String word = words.nextToken();
	
					if( len + word.length() > max )
					{
						ret_line = ret_line + "\n";
						len = 0;
					}

					ret_line = ret_line + word + " ";
					len += word.length() + 1;
				}
			}
			else
				ret_line = line;
			ret = ret + "\n" + ret_line;
		}

		return ret;
	}
}
