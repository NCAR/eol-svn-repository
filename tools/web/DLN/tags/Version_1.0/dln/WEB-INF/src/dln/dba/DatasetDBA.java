package dln.dba;

/******************************************************************************

DatasetDBA.java: The DatasetDBA provides several static methods by which 
the dln:dataset table can be manipulated.  All interaction with the dataset
table for the DLN app is done through this class.

Functions To Note:
------------------
	public static Vector getDatasets( DisplayBean display )
		-Returns a Vector of datasets based on the current display the user is viewing
			the DisplayBean hold information as to which view to display, the 
			types of datasets to hide, the sort order etc.

	public static boolean getFromDB( DatasetBean ds, int dsid )
		-Pulls the dataset from the database with the given id and populates the
			DatasetBean

	public static boolean updateDB( DatasetBean ds )
		-Updates the database with the given DatasetBean

	public static boolean insertDB( DatasetBean ds )
		-Inserts the DatasetBean into the database

	public static boolean deleteFromDB( DatasetBean ds )
		-Deletes the DatasetBean from the database

	public static boolean setFlag( int dsid, String flag, int flagVal )
		-Sets the given 'flag' or field in the dataset to the flagVal.  Flags 
			include loaded, checked, master_list, readme.  
		Example: DatasetDBA.setFlag( 10, "loaded", 1 )
			This signifies the dataset with primary key 10 has been loaded into
				CODIAC.
 
Author: Dan Sullivan
Date: ??
******************************************************************************/

import dln.beans.*;
import dln.dba.*;
import dln.format.*;

import java.util.*;
import java.sql.*;

public class DatasetDBA
{
	public static Vector getDatasets( DisplayBean display )
	{
		Vector dss = new Vector();
		int displayView = display.getDisplayView();
		int sortField = display.getSortField();

		try
		{
			Connection connection = DLNConnection.getConnection();
			
			String sql = "SELECT D.*, U1.*, U2.*, U3.* FROM dataset D, user U1, user U2, user U3";

			sql = sql + " WHERE D.loader=U1.uid AND D.checker=U2.uid AND D.int_contact=U3.uid";

			// Determine the WHERE clause from the display view
			if( displayView == DisplayBean.PROJECT )
				sql = sql + " AND D.project = ?";
			else if( displayView ==  DisplayBean.LOADERS ) 
				sql = sql + " AND D.loader = ?";
			else if( displayView ==  DisplayBean.CHECKERS )
				sql = sql + " AND D.checker = ?";
			else
			{
				sql = sql + "";
			}

			if( !display.getShowChecked() )
				sql = sql + " AND D.checked=0 ";

			if( !display.getShowDocumented() )
				sql = sql + " AND D.readme=0 ";

			if( !display.getShowLoaded() )
				sql = sql + " AND D.loaded=0 ";

			if( !display.getShowMaster() )
				sql = sql + " AND D.master=0 ";

			// Determine the ORDER BY clause 
			if( sortField ==  DisplayBean.TITLE )
				sql = sql + " ORDER BY D.title ";
			else if( sortField ==  DisplayBean.LOADING )
				sql = sql + " ORDER BY U1.first_name ";
			else if( sortField ==  DisplayBean.CHECKING )
				sql = sql + " ORDER BY U2.first_name ";
			else if( sortField ==  DisplayBean.STORM_ID )
				sql = sql + " ORDER BY D.storm_id ";
			else if( sortField ==  DisplayBean.DATE )
				sql = sql + " ORDER BY D.date ";
			else if( sortField ==  DisplayBean.PROJ )
				sql = sql + " ORDER BY D.project ";
			else
					sql = sql + " ORDER BY D.date ";

			if( display.getSortDirection() == DisplayBean.ASC )
				sql = sql + "ASC "; 
			else
				sql = sql + "DESC ";

			PreparedStatement statement = connection.prepareStatement( sql );

			statement.setString( 1, display.getDisplayId() );
			ResultSet rs = statement.executeQuery();
			boolean moreRecs = rs.next();

			while( moreRecs )
			{
				int index = 1;
				DatasetBean ds = new DatasetBean();
				UserBean loader = new UserBean();
				UserBean checker = new UserBean();
				UserBean intContact = new UserBean();
		
				index = ds.populate( index, rs );
				index = loader.populate( index, rs );	
				index = checker.populate( index, rs );	
				index = intContact.populate( index, rs );	

				ds.setLoaderBean( loader );
				ds.setCheckerBean( checker );
				ds.setIntContactBean( intContact );

				dss.add( ds );
				moreRecs = rs.next();
			}

		}
		catch( SQLException sqle )
		{
			System.err.println( "SQL ERROR: " + sqle.toString() );
		}

		return dss;
	}

	public static boolean getFromDB( DatasetBean ds, int dsid )
	{
		boolean success = true;

		try
		{
			Connection connection = DLNConnection.getConnection();
			
			String sql = "SELECT D.*, U1.*, U2.*, U3.* FROM dataset D, user U1, user U2, user U3 " +
									 "WHERE D.loader=U1.uid AND D.checker=U2.uid AND D.int_contact=U3.uid AND dsid=?";

			PreparedStatement statement = connection.prepareStatement( sql );

			statement.setInt( 1, dsid );

			ResultSet rs = statement.executeQuery();

			success = rs.next();
			if( success )
			{
				UserBean loader = new UserBean();
				UserBean checker = new UserBean();
				UserBean intContact = new UserBean();

				int index = ds.populate( rs );
				index = loader.populate( index, rs ); 
				index = checker.populate( index, rs ); 
				index = intContact.populate( index, rs ); 

				ds.setLoaderBean( loader );
				ds.setCheckerBean( checker );
				ds.setIntContactBean( intContact );
			}
		}
		catch( SQLException sqle )
		{
			System.err.println( "SQL ERROR: " + sqle.toString() );
		}

		return success;
	}

	public static boolean updateDB( DatasetBean ds )
	{
		boolean success = true;
		
		try
		{
			Connection connection = DLNConnection.getConnection();

			String sql = "UPDATE dataset SET " +
										"storm_id = ?, " +
										"title = ?, " +
										"readme = ?, " +
										"master = ?, " +
										"checked = ?, " +
										"loaded = ?, " +
										"notes = ?, " +
										"project = ?, " +
										"loader = ?, " +
										"checker = ?, " +
										"int_contact = ?, " +
										"ext_contact = ?, " +
										"ext_email = ?, " +
										"ingest = ?, " +
										"archive = ?, " +
										"remote_url = ? " +
									"WHERE dsid = ?";

			PreparedStatement statement = connection.prepareStatement(sql);

			int i = 1;
			statement.setString( i++, ds.getStormId() );
			statement.setString( i++, ds.getTitle() );
			statement.setInt( i++, ds.getReadme() );
			statement.setInt( i++, ds.getMaster() );
			statement.setInt( i++, ds.getChecked() );
			statement.setInt( i++, ds.getLoaded() );
			statement.setString( i++, ds.getNotes() );
			statement.setString( i++, ds.getProject() );
			statement.setString( i++, ds.getLoader() );
			statement.setString( i++, ds.getChecker() );
			statement.setString( i++, ds.getIntContact() );
			statement.setString( i++, ds.getExtContact() );
			statement.setString( i++, ds.getExtEmail() );
			statement.setString( i++, ds.getIngest() );
			statement.setString( i++, ds.getArchive() );
			statement.setString( i++, ds.getRemoteUrl() );
			statement.setInt( i++, ds.getDsid() );

			statement.executeUpdate();	

			connection.close();
		}
		catch( SQLException sqle )
		{
			System.err.println( "ERROR: " + sqle.toString() );
			success = false;
		}
	
		return success;	
	}


	public static boolean insertDB( DatasetBean ds )
	{
		boolean success = true;
		Connection connection = null;
		PreparedStatement statement = null;	
		Statement statement2 = null;	

		ds.setDateToday();

		try
		{
			connection = DLNConnection.getConnection();

			String sql = "INSERT INTO dataset VALUES ( 0, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ? )";

			statement = connection.prepareStatement( sql );

			int i = 1;
			statement.setString( i++, ds.getStormId() );
			statement.setString( i++, ds.getTitle() );
			statement.setInt( i++, ds.getReadme() );
			statement.setInt( i++, ds.getMaster() );
			statement.setInt( i++, ds.getChecked() );
			statement.setInt( i++, ds.getLoaded() );
			statement.setDate( i++, ds.getDate() );
			statement.setString( i++, ds.getNotes() );
			statement.setString( i++, ds.getProject() );
			statement.setString( i++, ds.getLoader() );
			statement.setString( i++, ds.getChecker() );
			statement.setString( i++, ds.getIntContact() );
			statement.setString( i++, ds.getExtContact() );
			statement.setString( i++, ds.getExtEmail() );
			statement.setString( i++, ds.getIngest() );
			statement.setString( i++, ds.getArchive() );
			statement.setString( i++, ds.getRemoteUrl() );

			statement.executeUpdate();

			sql = "SELECT MAX(dsid) FROM dataset";

			statement2 = connection.createStatement();
			ResultSet rs = statement2.executeQuery( sql );

			if( rs.next() )
				ds.setDsid( rs.getInt( 1 ) );
			
		}
		catch( SQLException sqle )
		{
			System.err.println( "ERROR: " + sqle.toString() );
			success = false;
		}
		finally
		{
			if( connection != null )
				try { connection.close(); } catch( SQLException e ) { }

			if( statement != null )
				try{ statement.close(); } catch( SQLException e ){ }

			if( statement2 != null )
				try{ statement2.close(); } catch( SQLException e ){ }
		}
			
		return success;
	}

	public static boolean deleteFromDB( DatasetBean ds )
	{
		boolean success = true;
		Connection connection = null;
		PreparedStatement statement = null;

		try
		{
			connection = DLNConnection.getConnection();

			String sql = "DELETE FROM dataset WHERE dsid = ?";

			statement = connection.prepareStatement( sql );

			statement.setInt( 1, ds.getDsid() );

			statement.executeUpdate();

		}
		catch( SQLException sqle )
		{
			System.err.println( "ERROR: " + sqle.toString() );
			success = false;
		}
		finally
		{
			if( connection != null )
				try{ connection.close(); } catch( SQLException e ){}

			if( statement != null )
				try{ statement.close(); } catch( SQLException e ){}
		}

		return success;
	}

	public static boolean setFlag( int dsid, String flag, int flagVal )
	{
		boolean success = false;
		Connection connection = null;
		PreparedStatement statement = null;
		
		try
		{
			connection = DLNConnection.getConnection();

			String sql = "UPDATE dataset SET " + flag + " = ? WHERE dsid = ?";

			statement = connection.prepareStatement( sql );
			statement.setInt( 1, flagVal );
			statement.setInt( 2, dsid );

			statement.executeUpdate();
		}
		catch( SQLException sqle )
		{
			System.err.println( "ERROR: " + sqle.toString() );
			success = false;
		}
		finally
		{
			if( connection != null )
				try{ connection.close(); } catch( SQLException e ){}

			if( statement != null )
				try{ statement.close(); } catch( SQLException e ){}
		}

		return success;	
	}
}
