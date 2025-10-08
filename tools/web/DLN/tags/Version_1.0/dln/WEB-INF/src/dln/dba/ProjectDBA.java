package dln.dba;

/******************************************************************************

ProjectDBA.java: The ProjectDBA provides several static methods by which 
the dln:project table can be manipulated.  All interaction with the project
table for the DLN app is done through this class.

Functions to Note:
------------------

	public static Vector getAllProjects()
		-Returns all the projects in the database as a Vector of ProjectBeans

	public static Vector getAllActiveProjects()
		-Returns all active projects in the database as a Vector of ProjectBeans

	public static Vector getInAllActiveProjects()
		-Returns all inactive projects in the database as a Vector of ProjectBeans

	public static Vector getAllProjects( int active)
		-Returns all projects with the given value of active in the database 
			as a Vector of ProjectBeans

	public static ProjectBean getFromDB( String pname )
		-Returns the project with the given project name, returns null if it
			does not exist

	public static boolean updateDB( ProjectBean project )
		-Updates the project in the database

	public static boolean updateDB( ProjectBean project, String sourcePname )
		-Updates the project in the database where pname is the given sourcePname,
			this allows users to change the name of a project.

	public static boolean insertDB( ProjectBean project )
		-Insert the given project into the database
	
Static Instance Variables:
--------------------------
	public static String orderBy = "pname ASC";
		-The order by which to sort the projects when the getXXXXProjects functions
			are called.

Author: Dan Sullivan
Date: ??
******************************************************************************/

import dln.beans.*;
import java.sql.*;
import java.util.*;

public class ProjectDBA
{
	public static String orderBy = "pname ASC";
	
	public static Vector getAllProjects()
	{
		return getAllProjects( -1 );
	}

	public static Vector getAllActiveProjects()
	{
		return getAllProjects( 1 );
	}	

	public static Vector getInAllActiveProjects()
	{
		return getAllProjects( 0 );
	}	

	public static Vector getAllProjects( int active)
	{
		Vector projects = new Vector();

		try
		{
			Connection connection = DLNConnection.getConnection();

			String sql = "SELECT * FROM project";

			if( active == 1 || active == 0 )
				sql = sql + " WHERE active=?";

			if( orderBy != null )
				sql = sql + " ORDER BY " + orderBy;

			PreparedStatement statement = connection.prepareStatement( sql );

			if( active == 1 || active == 0 )
				statement.setInt( 1, active );

			ResultSet rs = statement.executeQuery();

			boolean moreRecs = rs.next();
		
			while( moreRecs )
			{
				ProjectBean proj = new ProjectBean();
				
				proj.populate( rs );	

				projects.add( proj );

				moreRecs = rs.next();
			}
		}
		catch( SQLException sqle )
		{
			System.err.println( "SQL ERROR: " + sqle.toString() );
		}

		return projects;
	}

	public static ProjectBean getFromDB( String pname )
	{
		ProjectBean project = null;
		Connection connection = null;
		PreparedStatement statement = null;

		try
		{
			connection = DLNConnection.getConnection();

			String sql = "SELECT * FROM project WHERE pname = ?";

			statement = connection.prepareStatement( sql );
			statement.setString( 1, pname );
			ResultSet rs = statement.executeQuery();

			if( rs.next() )
			{
				project = new ProjectBean();
				project.populate( rs );				
			}
		}
		catch( SQLException sqle )
		{
			System.err.println( "ERROR: " + sqle.toString() );
			project = null;
		}
		finally
		{
			if( connection != null )
				try{ connection.close(); } catch( SQLException e ) {}

			if( statement != null )
				try{ statement.close(); } catch( SQLException e ) {}
		}
	
		return project;	
	}

	public static boolean updateDB( ProjectBean project )
	{
		return updateDB( project, project.getPname() );
	}

	public static boolean updateDB( ProjectBean project, String sourcePname )
	{
		boolean success = true;

		try
		{
			Connection connection = DLNConnection.getConnection();

			String sql = "UPDATE project SET " +
										"pname = ?, " +
										"storm_id_prefix = ?, " +
										"active = ? " +
									 "WHERE pname = ?";

			PreparedStatement statement = connection.prepareStatement( sql );
			int i = 1;

			statement.setString( i++, project.getPname() );	
			statement.setString( i++, project.getStormIdPrefix() );	
			statement.setInt( i++, project.getActive() );	
			statement.setString( i++, sourcePname );	
		
			statement.executeUpdate();	

			connection.close();
		}
		catch( SQLException sqle )
		{
			System.err.println( "SQL ERROR: " + sqle.toString() );
			success = false;
		}

		return success;
	}

	public static boolean insertDB( ProjectBean project )
	{
		boolean success = true;

		try
		{
			Connection connection = DLNConnection.getConnection();

			String sql = "INSERT INTO project VALUES (?, ?, ?)";

			PreparedStatement statement = connection.prepareStatement( sql );
			int i = 1;

			statement.setString( i++, project.getPname() );
			statement.setString( i++, project.getStormIdPrefix() );
			statement.setInt( i++, project.getActive() );

			statement.executeUpdate();

			connection.close();
		}
		catch( SQLException sqle )
		{
			System.err.println( "SQL ERROR: " + sqle.toString() );
			success = false;
		}

		return success;
	}
}

