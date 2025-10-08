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

public class ProjectDBA {

    public static String orderBy = "jedi7.project.project_id ASC";
    
    public static Vector getAllActiveProjects() { return getAllProjects(1); }	

    public static Vector getAllInActiveProjects() { return getAllProjects(0); }	

    public static Vector getAllProjects() { return getAllProjects(-1); }
    
    public static Vector getAllProjects(int active) {
	Vector projects = new Vector();

	try {
	    Connection connection = DLNConnection.getConnection();
	    
	    String sql = "SELECT jedi7.project.project_id,dataset_id_prefix,active FROM jedi7.project LEFT JOIN jedi7.dataset_prefix_project ON jedi7.project.project_id=jedi7.dataset_prefix_project.project_id LEFT JOIN dts.project ON jedi7.project.project_id=dts.project.project_id";
	    
	    if( active == 1 || active == 0 ) { sql = sql + " WHERE active=?"; }
	    if( orderBy != null ) { sql = sql + " ORDER BY " + orderBy; }
	    
	    PreparedStatement statement = connection.prepareStatement( sql );
	    
	    if( active == 1 || active == 0 ) { statement.setInt( 1, active ); }


	    ResultSet rs = statement.executeQuery();
	    while(rs.next()) {
		ProjectBean proj = new ProjectBean();
		proj.setId(rs.getString(1));
		proj.setPrefix(rs.getString(2));
		proj.setActive(rs.getInt(3));

		projects.add( proj );
	    }
	} catch( SQLException sqle ) {
	    System.err.println( "SQL ERROR (ProjectDBA): " + sqle.toString() + " " +sqle.getMessage() );
	}
	
	return projects;
    }


    public static ProjectBean getFromDB(String project) {
	try { return getFromDB(DLNConnection.getConnection(),project); }
	catch (SQLException e) {
	    System.err.println(e);
	    e.printStackTrace();
	    return null;
	}
    }

    public static ProjectBean getFromDB(Connection connection, String pname) {
	ProjectBean project = null;
	PreparedStatement statement = null;
	
	try {
	    String sql = "SELECT jedi7.project.project_id,dataset_id_prefix,active FROM jedi7.project LEFT JOIN jedi7.dataset_prefix_project ON jedi7.project.project_id=jedi7.dataset_prefix_project.project_id LEFT JOIN dts.project ON jedi7.project.project_id=dts.project.project_id WHERE jedi7.project.project_id = ?";
	    
	    statement = connection.prepareStatement( sql );
	    statement.setString( 1, pname );
	    ResultSet rs = statement.executeQuery();
	    
	    if(rs.next()) {
		project = new ProjectBean();
		project.setId(rs.getString(1));
		project.setPrefix(rs.getString(2));
		project.setActive(rs.getInt(3));
	    }
	} catch( SQLException sqle ) {
	    System.err.println( "ERROR ProjectDBA::getFromDB: " + sqle.toString() );
	    project = null;
	}
	
	return project;	
    }
    
    /*
	public static boolean updateDB( ProjectBean project )
	{
		return updateDB( project, project.getPname() );
	}
    */
    public static boolean updateDB( ProjectBean project, String sourcePname ) {
	boolean success = true;
	
	try {
	    Connection connection = DLNConnection.getConnection();

	    PreparedStatement test = connection.prepareStatement("SELECT project_id FROM dts.project WHERE project_id=?");
	    test.setString(1,sourcePname);
	    if (test.executeQuery().next()) {
		String sql = "UPDATE dts.project SET active=? WHERE project_id=?";
		PreparedStatement stmt = connection.prepareStatement( sql );
		stmt.setInt(1,project.getActive());
		stmt.setString(2,sourcePname);
		stmt.executeUpdate();
	    } else {
		String sql = "INSERT INTO dts.project(project_id,active) VALUES(?,?)";
		PreparedStatement stmt = connection.prepareStatement(sql);
		stmt.setString(1,sourcePname);
		stmt.setInt(2,project.getActive());
		stmt.executeUpdate();
	    }
	    connection.close();
	} catch( SQLException sqle ) {
	    System.err.println( "SQL ERROR: " + sqle.toString() );
	    success = false;
	}
	return success;
    }
    /*
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
    */
}

