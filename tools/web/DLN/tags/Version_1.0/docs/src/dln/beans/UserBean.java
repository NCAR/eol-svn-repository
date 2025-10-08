package dln.beans;

/******************************************************************************

UserBean.java: The UserBean class is a simple container class to hold one 
record of the dln:user table.

Functions to Note:
------------------
	public int populate( ResultSet rs ) throws SQLException
		-Populates this UserBean with the data in the ResultSet starting with index 1

	public int populate( int index, ResultSet rs ) throws SQLException
		-Populates this UserBean with the data in the ResultSet starting with the
			given index

Author: Dan Sullivan
Date: ??

******************************************************************************/

import java.util.*;
import java.io.Serializable;
import java.sql.*;

public class UserBean implements Serializable
{
	private String uid, firstName, lastName, email;
	private int active;

	public UserBean()
	{
		uid = null;
		firstName = null;
		lastName = null;
		email = null;
	
		active = 0;	
	}

	public void setUid( String uid )
	{ this.uid = uid; }

	public String getUid()
	{ return uid; }

	public void setFirstName( String firstName )
	{ this.firstName = firstName; }

	public String getFirstName()
	{ return firstName; }

	public void setLastName( String lastName )
	{ this.lastName = lastName; }

	public String getLastName()
	{ return lastName; }

	public void setEmail( String email )
	{ this.email = email; }

	public String getEmail()
	{ return email; }

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
		setUid( rs.getString( index++ ) );	
		setFirstName( rs.getString( index++ ) );	
		setLastName( rs.getString( index++ ) );	
		setEmail( rs.getString( index++ ) );	
		setActive( rs.getInt( index++ ) );	
	
		return index;
	}

}
