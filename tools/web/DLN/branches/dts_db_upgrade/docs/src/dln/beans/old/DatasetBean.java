package dln.beans;

import database.*;
import java.util.*;
import java.io.*;

public class DatasetBean implements Serializable
{
	private String	stormId,
									title,
									notes,
									project,
									loader, checker, intContact,
									extContact, extEmail,										
									ingest, archive, remoteUrl; 

	DBDateBean date;

	private int readme, master, checked, loaded;


	public DatasetBean()
	{
		reset();
	}

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
	{ notes = n; }

	public String getProject()
	{ return project; }

	public void setProject( String p )
	{ project = p; }

	public String getLoader()
	{ return loader; }

	public void setLoader( String l )
	{ loader = l ; }

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
	{ extContact = e; }

	public String getExtEmail()
	{ return extEmail; }

	public void setExtEmail( String e )
	{ extEmail = e; }

	public String getIngest()
	{ return ingest; }

	public void setIngest( String i )
	{ ingest = i; }

	public String getArchive()
	{ return archive; }

	public void setArchive( String a )
	{ archive = a; }

	public String getRemoteUrl()
	{ return remoteUrl; }

	public void setRemoteUrl( String url )
	{ remoteUrl = url; } 


	public DBIntegerBean getReadme()
	{ return new DBIntegerBean( readme ); }

	public boolean isReadme()
	{ return readme != 0; }

	public void setReadme( int x )
	{
		if( x == 0 || x == 1 )
			readme = x;
		else
			readme = -1;
	}

	public void setReadme( boolean x )
	{
		if( x )
			readme = 1;
		else
			readme = 0;
	}

	public DBIntegerBean getMaster()
	{ return new DBIntegerBean( master ); }

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

	public DBIntegerBean getChecked()
	{ return new DBIntegerBean( checked ); }

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

	public DBIntegerBean getLoaded()
	{ return new DBIntegerBean( loaded ); }

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
		System.out.println( "Setting loaded: " + x ); 
		if( x )
			loaded = 1;
		else
			loaded = 0;
	}

	public void setLoaded( String s )
	{
		System.out.println( "Setting loaded: " + s ); 
	}

	public DBDateBean getDate()
	{ return date; }

	public void setDate( String d )
	{ date = new DBDateBean( d ); }

	public void reset()
	{
		stormId = "99.999";

		title = notes = project = "";
		loader = checker = intContact = "";
		extContact  = "";
		extEmail = "E-mail";
		ingest = archive = remoteUrl = "";

		readme = master = checked = loaded = 0;

		date = new DBDateBean( new Date() );
	}

	public void populate( Hashtable data )
	{
		stormId = getField( data, "storm_id" );  
		title = getField( data, "title" );
		notes = getField( data, "notes" );
		project = getField( data, "project" );
		loader = getField( data, "loader" ); 
		checker = getField( data, "checker" ); 
		intContact = getField( data, "int_contact" );
		extContact = getField( data, "ext_contact" ); 
		extEmail = getField( data, "ext_email" );										
		ingest = getField( data, "ingest" ); 
		archive = getField( data, "archive" ); 
		remoteUrl = getField( data, "remote_url" ); 

		date = new DBDateBean( getField( data, "date" ) );

		readme = getIntField( data, "readme" );
		master = getIntField( data, "master" );
		checked = getIntField( data, "checked" );
		loaded = getIntField( data, "loaded" ); 
	}

	private String getField( Hashtable data, String field )
	{
		if( data.get( field ) == null )
			return "";
		else
			return data.get( field ).toString();
	} 

	private int getIntField( Hashtable data, String field )
	{
		try
		{
			return ( (DBIntegerBean) data.get( field ) ).getInteger();
		}
		catch( Exception e )
		{
			// This should catch the ClassCastException and NullPointerExcpetion
			return 0;
		}	
	}
}
