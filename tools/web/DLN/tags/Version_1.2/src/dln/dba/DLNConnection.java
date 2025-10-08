package dln.dba;

/******************************************************************************

DLNConnection.java: Establishes a Connection to the dln database.


	public static Connection getConnection() throws SQLException
		-Returns a Connection to the dln database, note this function is static

Author: Dan Sullivan
Date: ??
******************************************************************************/


import java.sql.*;
import java.io.*;

public class DLNConnection implements Serializable
{
	public static Connection getConnection() throws SQLException
	{
		Connection con = null;

		try 
		{
			Class.forName( "org.gjt.mm.mysql.Driver").newInstance();
		}
		catch( Exception exp )
		{
			System.out.println( "Unable to load driver." );
			exp.printStackTrace();
		}

		con = DriverManager.getConnection( "jdbc:mysql://localhost/dmg_dln?user=dts-full&password=l@micbc" );

		return con;
	}
}
