package dln.dba;

/******************************************************************************

UserDBA.java: The UserDBA provides several static methods by which 
the dln:user table can be manipulated.  All interaction with the user
table for the DLN app is done through this class.

Functions to Note:
------------------
	public static Vector getAllUsers()
		-Returns all the users in the database as a Vector of UserBeans

	public static Vector getAllActiveUsers()
		-Returns all active users in the database as a Vector of UserBeans

	public static Vector getAllInactiveUsers()	
		-Returns all inactive users in the database as a Vector of UserBeans

	public static Vector getAllUsers( int active ) 
		-Returns all users in the database with the given value of active
			as a Vector of UserBeans

	public static UserBean getFromDB( String uid )
		-Returns a single user with the given user id, returns null if does
			not exist

	public static boolean insertDB( UserBean user )
		-Inserts the user into the database

	public static boolean updateDB( UserBean user )
		-Updates the given user in the database

	public static boolean updateDB( UserBean user, String uidSource )
		-Updates the given user in the database where the uid is uidSource,
		this allows us to change user ids.


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

public class UserDBA {

    public static String orderBy = "person_name ASC";

    public static HashMap createUserMap() {
	HashMap userMap = new HashMap();
	userMap.put(new Integer(0),new UserBean());

	try {
	    Connection conn = DLNConnection.getConnection();
	    ResultSet rs = conn.prepareStatement("SELECT contact_id,person_name,email,active_editor FROM jedi7.contact").executeQuery();
	    while (rs.next()) {
		UserBean user = new UserBean(rs.getInt(1),rs.getString(2),rs.getString(3),rs.getBoolean(4));
		userMap.put(new Integer(user.getId()),user);
	    }
	} catch (SQLException e) {
	    System.err.println(e);
	    e.printStackTrace();
	}

	return userMap;
    }
	
    public static Vector getAllActiveUsers() { return getAllUsers(1); }

    public static Vector getAllInActiveUsers() { return getAllUsers(0); }	

    public static Vector getAllUsers() { return getAllUsers(-1); }
    
    public static Vector getAllUsers(int active) {
	Vector users = new Vector();
	
	try {
	    Connection connection = DLNConnection.getConnection();
	    String sql = "SELECT contact_id,active_editor,person_name,email FROM jedi7.contact";
			
	    if (active == 1 || active == 0) { sql = sql + " WHERE active_editor = ?"; }
	    if (orderBy != null) { sql = sql + " ORDER BY " + orderBy; }

	    PreparedStatement statement = connection.prepareStatement( sql );
	    
	    if (active == 1 || active == 0 ) { statement.setInt( 1, active ); }

	    ResultSet rs = statement.executeQuery();
	    while (rs.next()) {
		UserBean user = new UserBean();
		user.setId(rs.getInt(1));
		user.setActive(rs.getInt(2));
		user.setName(rs.getString(3));
		user.setEmail(rs.getString(4));
		users.add(user);
	    }
	    
	    connection.close();
	} catch( SQLException sqle ) {
	    System.out.println( "SQL ERROR (UserDBA::getAllUsers()): " + sqle.toString() );
	    sqle.printStackTrace();	
	}
	
	return users;
    }


    public static UserBean getFromDB(int id) {
	try {
	    return getFromDB(DLNConnection.getConnection(),id);
	} catch (SQLException e) {
	    System.err.println(e);
	    e.printStackTrace();
	    return null;
	}
    }

    public static UserBean getFromDB(Connection connection, int id) {
	UserBean user = null;

	try {
	    String sql = "SELECT contact_id,active_editor,person_name,email FROM jedi7.contact WHERE contact_id = ?";
	    
	    PreparedStatement statement = connection.prepareStatement(sql);
	    statement.setInt(1,id);
			
	    ResultSet rs = statement.executeQuery();
	    if (rs.next()) {
		user = new UserBean();
		user.setId(rs.getInt(1));
		user.setActive(rs.getInt(2));
		user.setName(rs.getString(3));
		user.setEmail(rs.getString(4));
	    }
	} catch( SQLException sqle ) {
	    System.err.println( sqle );
	    sqle.printStackTrace();
	}
	
	return user;
    }

    /*
	public static boolean insertDB( UserBean user )
	{
		boolean success = true;

		try
		{
			Connection connection = DLNConnection.getConnection();

			String sql = "INSERT INTO user VALUES (?,?,?,?,?)";

			PreparedStatement statement = connection.prepareStatement( sql );
			int i = 1;
			
			statement.setString( i++, user.getUid() );
			statement.setString( i++, user.getFirstName() );
			statement.setString( i++, user.getLastName() );
			statement.setString( i++, user.getEmail() );
			statement.setInt( i++, user.getActive() );

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
     
	public static boolean updateDB( UserBean user )
	{
		return updateDB( user, user.getUid() );
	}

	public static boolean updateDB( UserBean user, String uidSource )
	{
		boolean success = true;

		try
		{
			Connection connection = DLNConnection.getConnection();

			String sql = "UPDATE user SET " +
										"uid = ?, " +
										"first_name = ?, " +
										"last_name = ?, " +
										"email = ?, " +
										"active = ? " +
									 "WHERE uid = ?";

			PreparedStatement statement = connection.prepareStatement( sql );
			int i = 1;

			statement.setString( i++, user.getUid() );
			statement.setString( i++, user.getFirstName() );
			statement.setString( i++, user.getLastName() );
			statement.setString( i++, user.getEmail() );
			statement.setInt( i++, user.getActive() );
			statement.setString( i++, uidSource );

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


