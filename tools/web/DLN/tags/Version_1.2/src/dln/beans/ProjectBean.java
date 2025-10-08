package dln.beans;

/******************************************************************************

ProjectBean.java: The ProjectBean is a simple container class to hold a single
record in the dln:project table.

Functions to Note:
-----------------
	public int populate( ResultSet rs ) throws SQLException
		-Populate this ProjectBean with the given ResultSet starting with index 1

	public int populate( int index, ResultSet rs ) throws SQLException
		-Populate this ProjectBean with the given ResultSet starting with the
			given index

Author: Dan Sullivan
Date: ??
******************************************************************************/

import java.io.Serializable;
import java.sql.*;

public class ProjectBean implements Serializable
{
	private String pname, stormIdPrefix;
	private int active;

	public ProjectBean()
	{
		pname = null;
		stormIdPrefix = null;

		active = 0;
	}

	public void setPname( String pname )
	{ this.pname = pname; }

	public String getPname()
	{ return pname; }

	public void setStormIdPrefix( String prefix )
	{ this.stormIdPrefix = prefix; }

	public String getStormIdPrefix()
	{ return stormIdPrefix; }

	public void setActive( boolean active )
	{
		if( active )
			this.active = 1;
		else
			this.active = 0;
	}

	public void setActive( int active )
	{
		if( active == 1 || active == 0 )
			this.active = active;
		else
			this.active = 0;
	}

	public boolean isActive()
	{ return (active==1); }

	public int getActive()
	{ return active; }

	public int populate( ResultSet rs ) throws SQLException
	{
		return populate( 1, rs );
	}

	public int populate( int index, ResultSet rs ) throws SQLException
	{
		setPname( rs.getString( index++ ) );
		setStormIdPrefix( rs.getString( index++ ) );
		setActive( rs.getInt( index++ ) );		

		return index;
	}
}
