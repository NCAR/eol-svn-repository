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

public class DatasetDBA {


    public static Vector getDatasets( DisplayBean display ) {
	Vector dss = new Vector();
	int displayView = display.getDisplayView();
	int sortField = display.getSortField();
	
	try {
	    Connection connection = DLNConnection.getConnection();
	    String sql = "SELECT jedi7.dataset.dataset_id,name,source_contact_id,internal_contact_id,dts.dataset.entered_date,ingest_dir,load_contact_id,load_status_id,load_data_dir,load_archive_dir,approved_contact_id,approved_status_id,project_id,readme_flag,dts.note.note_id,note_text,load_contact.person_name,approved_contact.person_name,load_status.is_resolved,approved_status.is_resolved,dataset_type FROM jedi7.dataset LEFT JOIN dts.dataset ON jedi7.dataset.dataset_id=dts.dataset.dataset_id LEFT JOIN dts.dataset_ingest ON jedi7.dataset.dataset_id=dts.dataset_ingest.dataset_id LEFT JOIN dts.dataset_load ON jedi7.dataset.dataset_id=dts.dataset_load.dataset_id LEFT JOIN dts.dataset_note ON jedi7.dataset.dataset_id=dts.dataset_note.dataset_id LEFT JOIN dts.note ON dts.dataset_note.note_id=dts.note.note_id JOIN jedi7.dataset_project ON jedi7.dataset.dataset_id=jedi7.dataset_project.dataset_id LEFT JOIN jedi7.contact AS load_contact ON dts.dataset_load.load_contact_id=load_contact.contact_id LEFT JOIN jedi7.contact AS approved_contact ON dts.dataset.approved_contact_id=approved_contact.contact_id LEFT JOIN dts.status AS load_status ON dts.dataset_load.load_status_id=load_status.status_id LEFT JOIN dts.status AS approved_status ON dts.dataset.approved_status_id=approved_status.status_id WHERE ";

	    if( displayView == DisplayBean.PROJECT )
		sql += "project_id=?";
	    else if( displayView ==  DisplayBean.LOADERS ) 
		sql += "load_contact_id=?";
	    else if( displayView ==  DisplayBean.CHECKERS )
		sql += "approved_contact_id=?";

	    
	    if (!display.getShowDocumented()) { sql += " AND readme_flag=1"; }
	    if (!display.getShowChecked()) { sql += " AND (approved_status.is_resolved IS NULL OR approved_status.is_resolved!=1)"; }
	    if (!display.getShowLoaded()) { sql += " AND (load_status.is_resolved IS NULL OR load_status.is_resolved!=1)"; }

	    if (sortField == DisplayBean.DATE) { sql += " ORDER BY entered_date"; }
	    else if (sortField == DisplayBean.LOADING) { sql += " ORDER BY load_contact.person_name"; }
	    else if (sortField == DisplayBean.CHECKING) { sql += " ORDER BY approved_contact.person_name"; }
	    else if (sortField == DisplayBean.TITLE) { sql += " ORDER BY name"; }
	    else { sql += " ORDER BY jedi7.dataset.dataset_id"; }

	    sql += display.getSortDirection() == DisplayBean.ASC ? "" : " DESC";
	    

	    PreparedStatement statement = connection.prepareStatement(sql);
	    statement.setString(1,display.getDisplayId());

	    ResultSet data = statement.executeQuery();
	    while (data.next()) {
		DatasetBean dataset = new DatasetBean();
		dataset.setDatasetId(data.getString(1));
		dataset.setName(data.getString(2));
		dataset.setExternalContactId(data.getInt(3));
		dataset.setInternalContactId(data.getInt(4));
		dataset.setEnteredDate(data.getDate(5));
		dataset.setIngestDirectory(data.getString(6));
		dataset.setLoaderId(data.getInt(7));
		dataset.setLoadedStatusId(data.getInt(8));
		dataset.setLoadDirectory(data.getString(9));
		dataset.setArchiveDirectory(data.getString(10));
		dataset.setCheckerId(data.getInt(11));
		dataset.setCheckedStatusId(data.getInt(12));
		dataset.setDisplayProject(data.getString(13));
		dataset.setReadme(data.getBoolean(14));
		dataset.setNoteId(data.getInt(15));
		dataset.setNote(data.getString(16));
                dataset.setDatasetType(data.getString(21));

		PreparedStatement stmt = connection.prepareStatement("SELECT dataset_id FROM master_list.dataset_project WHERE dataset_id=? AND project_id=?");
		stmt.setString(1,dataset.getDatasetId());
		stmt.setString(2,data.getString(13));
		dataset.setMaster(stmt.executeQuery().next());
		stmt.close();

		stmt = connection.prepareStatement("SELECT project_id FROM jedi7.dataset_project WHERE dataset_id=?");
		stmt.setString(1,dataset.getDatasetId());
		ResultSet projects = stmt.executeQuery();
		while (projects.next()) {
		    dataset.addProject(projects.getString(1));
		}
		projects.close();
		stmt.close();

		stmt = connection.prepareStatement("SELECT jedi7.xlink.xlink_id,href,title,purpose FROM jedi7.dataset_xlinks JOIN jedi7.xlink ON jedi7.dataset_xlinks.xlink_id=jedi7.xlink.xlink_id WHERE dataset_id=?");
		stmt.setString(1,dataset.getDatasetId());
		ResultSet links = stmt.executeQuery();
		while (links.next()) {
		    dataset.addLink(new XlinkBean(links.getInt(1),links.getString(2),links.getString(3),links.getString(4)));
		}

		// Need to handle the master list filter differently than the other ones.
		if (!display.getShowMaster()) {
		    if (!dataset.getMaster()) { dss.add(dataset); }
		} else {
		    dss.add(dataset);
		}
	    }
	    statement.close();
	    connection.close();

	} catch(SQLException sqle){
	    System.err.println( "SQL ERROR: " + sqle.toString() );
	    sqle.printStackTrace();
	}
	
	return dss;
    }
    


    public static boolean getFromDB(DatasetBean dataset, String id) {
	boolean success = true;
	
	try {
	    Connection connection = DLNConnection.getConnection();
	    String sql = "SELECT jedi7.dataset.dataset_id,name,source_contact_id,internal_contact_id,dts.dataset.entered_date,ingest_dir,load_contact_id,load_status_id,load_data_dir,load_archive_dir,approved_contact_id,approved_status_id,project_id,readme_flag,dts.note.note_id,note_text,dataset_type FROM jedi7.dataset LEFT JOIN dts.dataset ON jedi7.dataset.dataset_id=dts.dataset.dataset_id LEFT JOIN dts.dataset_ingest ON jedi7.dataset.dataset_id=dts.dataset_ingest.dataset_id LEFT JOIN dts.dataset_load ON jedi7.dataset.dataset_id=dts.dataset_load.dataset_id LEFT JOIN dts.dataset_note ON jedi7.dataset.dataset_id=dts.dataset_note.dataset_id LEFT JOIN dts.note ON dts.dataset_note.note_id=dts.note.note_id JOIN jedi7.dataset_project ON jedi7.dataset.dataset_id=jedi7.dataset_project.dataset_id WHERE jedi7.dataset.dataset_id=?";
	    
	    PreparedStatement statement = connection.prepareStatement( sql );
	    statement.setString(1,id);
	    
	    ResultSet data = statement.executeQuery();
	    while (data.next()) {
		dataset.setDatasetId(data.getString(1));
		dataset.setName(data.getString(2));
		dataset.setExternalContactId(data.getInt(3));
		dataset.setInternalContactId(data.getInt(4));
		dataset.setEnteredDate(data.getDate(5));
		dataset.setIngestDirectory(data.getString(6));
		dataset.setLoaderId(data.getInt(7));
		dataset.setLoadedStatusId(data.getInt(8));
		dataset.setLoadDirectory(data.getString(9));
		dataset.setArchiveDirectory(data.getString(10));
		dataset.setCheckerId(data.getInt(11));
		dataset.setCheckedStatusId(data.getInt(12));
		dataset.setDisplayProject(data.getString(13));
		dataset.setReadme(data.getBoolean(14));
		dataset.setNoteId(data.getInt(15));
		dataset.setNote(data.getString(16));
		dataset.setDatasetType(data.getString(17));

		PreparedStatement stmt = connection.prepareStatement("SELECT dataset_id FROM master_list.dataset_project WHERE dataset_id=? AND project_id=?");
		stmt.setString(1,dataset.getDatasetId());
		stmt.setString(2,data.getString(13));
		dataset.setMaster(stmt.executeQuery().next());
		stmt.close();

		stmt = connection.prepareStatement("SELECT project_id FROM jedi7.dataset_project WHERE dataset_id=?");
		stmt.setString(1,dataset.getDatasetId());
		ResultSet projects = stmt.executeQuery();
		while (projects.next()) {
		    dataset.addProject(projects.getString(1));
		}
		projects.close();
		stmt.close();
		
	    }
	    statement.close();
	    connection.close();
	    success = true;
	} catch( SQLException sqle ) {
	    System.err.println( "SQL ERROR: " + sqle.toString() );
	    sqle.printStackTrace();
	    success = false;
	}
	
	return success;
    }

    public static boolean updateDB( DatasetBean ds ) {
	boolean success = true;

	try {
	    Connection connection = DLNConnection.getConnection();
	    connection.setAutoCommit(false);

	    String sql = "UPDATE jedi7.dataset SET name=?,internal_contact_id=? WHERE dataset_id=?";
	    PreparedStatement stmt = connection.prepareStatement(sql);
	    stmt.setString(1,ds.getName());
	    stmt.setInt(2,ds.getInternalContactId());
	    stmt.setString(3,ds.getDatasetId());
	    stmt.executeUpdate();
	    
	    dtsDatasetTable(connection,ds);
	    dtsDatasetIngestTable(connection,ds);
	    dtsDatasetLoadTable(connection,ds);
	    dtsDatasetNote(connection,ds);
	    jediDatasetProjectTable(connection,ds);

	    connection.commit();

	    connection.setAutoCommit(true);
	    connection.close();
	} catch( SQLException sqle ) {
	    System.err.println( "ERROR: " + sqle.toString() );
	    sqle.printStackTrace();
	    success = false;
	}

	
	return success;	
    }

    private static void jediDatasetProjectTable(Connection conn, DatasetBean dataset) throws SQLException {
	PreparedStatement test = conn.prepareStatement("SELECT project_id FROM jedi7.dataset_project WHERE dataset_id=?");
	test.setString(1,dataset.getDatasetId());
	ResultSet rs = test.executeQuery();
	ArrayList projects = new ArrayList();
	while (rs.next()) { projects.add(rs.getString(1)); }
	rs.close();
	test.close();

	Iterator itr = dataset.getProjects().iterator();
	while (itr.hasNext()) {
	    String project = (String)itr.next();
	    if (!projects.contains(project)) {
		PreparedStatement stmt = conn.prepareStatement("INSERT INTO jedi7.dataset_project(dataset_id,project_id) VALUES(?,?)");
		stmt.setString(1,dataset.getDatasetId());
		stmt.setString(2,project);
		stmt.executeUpdate();
		stmt.close();
	    }
	}
    }

    private static void dtsDatasetIngestTable(Connection conn, DatasetBean dataset) throws SQLException {
	PreparedStatement test = conn.prepareStatement("SELECT dataset_id FROM dts.dataset_ingest WHERE dataset_id=?");
	test.setString(1,dataset.getDatasetId());
	if (test.executeQuery().next()) {
	    String sql = "UPDATE dts.dataset_ingest SET ingest_dir=?,row_revise_contact_id=1 WHERE dataset_id=?";
	    PreparedStatement stmt = conn.prepareStatement(sql);
	    stmt.setString(1,dataset.getIngestDirectory());
	    stmt.setString(2,dataset.getDatasetId());
	    stmt.executeUpdate();
	    stmt.close();
	} else if (!dataset.getIngestDirectory().equals("")) {
	    String sql = "INSERT INTO dts.dataset_ingest(dataset_id,ingest_dir,row_revise_contact_id) VALUES(?,?,1)";
	    PreparedStatement stmt = conn.prepareStatement(sql);
	    stmt.setString(1,dataset.getDatasetId());
	    stmt.setString(2,dataset.getIngestDirectory());
	    stmt.executeUpdate();
	    stmt.close();
	}
	test.close();
    }

    private static void dtsDatasetLoadTable(Connection conn, DatasetBean dataset) throws SQLException {
	PreparedStatement test = conn.prepareStatement("SELECT dataset_id FROM dts.dataset_load WHERE dataset_id=?");
	test.setString(1,dataset.getDatasetId());
	if (test.executeQuery().next()) {
	    String sql = "UPDATE dts.dataset_load SET load_contact_id=?,load_status_id=?,load_data_dir=?,load_archive_dir=?,row_revise_contact_id=? WHERE dataset_id=?";
	    PreparedStatement stmt = conn.prepareStatement(sql);
	    if (dataset.getLoaderId() == 0) { stmt.setNull(1,Types.INTEGER); }
	    else { stmt.setInt(1,dataset.getLoaderId()); }
	    if (dataset.getLoadedStatusId() == 0) { stmt.setNull(2,Types.INTEGER); }
	    else { stmt.setInt(2,dataset.getLoadedStatusId()); }
	    stmt.setString(3,dataset.getLoadDirectory());
	    stmt.setString(4,dataset.getArchiveDirectory());
	    stmt.setInt(5,1);
	    stmt.setString(6,dataset.getDatasetId());
	    stmt.executeUpdate();
	    stmt.close();
	} else if (dataset.getLoaderId() != 0 || dataset.getLoadedStatusId() != 0 || !dataset.getLoadDirectory().equals("") || !dataset.getArchiveDirectory().equals("")) {
	    String sql = "INSERT INTO dts.dataset_load(dataset_id,load_contact_id,load_status_id,load_data_dir,load_archive_dir,row_revise_contact_id) VALUES(?,?,?,?,?,?)";
	    PreparedStatement stmt = conn.prepareStatement(sql);
	    stmt.setString(1,dataset.getDatasetId());
	    if (dataset.getLoaderId() == 0) { stmt.setNull(2,Types.INTEGER); }
	    else { stmt.setInt(2,dataset.getLoaderId()); }
	    if (dataset.getLoadedStatusId() == 0) { stmt.setNull(3,Types.INTEGER); }
	    else { stmt.setInt(3,dataset.getLoadedStatusId()); }
	    stmt.setString(4,dataset.getLoadDirectory());
	    stmt.setString(5,dataset.getArchiveDirectory());
	    stmt.setInt(6,1);
	    stmt.executeUpdate();
	    stmt.close();
	}
	test.close();
    }

    private static void dtsDatasetNote(Connection conn, DatasetBean dataset) throws SQLException {
	if (dataset.getNoteId() == 0) {
	    PreparedStatement stmt = conn.prepareStatement("INSERT INTO dts.note(author_id,note_text,entered_date,row_revise_contact_id) VALUES(?,?,?,?)",Statement.RETURN_GENERATED_KEYS);
	    stmt.setInt(1,1);
	    stmt.setString(2,dataset.getNote());
	    stmt.setDate(3,new java.sql.Date(Calendar.getInstance().getTimeInMillis()));
	    stmt.setInt(4,1);
	    stmt.executeUpdate();
	    ResultSet keys = stmt.getGeneratedKeys();
	    keys.next();
	    int note_id = keys.getInt(1);
	    stmt.close();
	    stmt = conn.prepareStatement("INSERT INTO dts.dataset_note(dataset_id,note_id) VALUES(?,?)");
	    stmt.setString(1,dataset.getDatasetId());
	    stmt.setInt(2,note_id);
	    stmt.executeUpdate();
	    stmt.close();
	} else {
	    PreparedStatement stmt = conn.prepareStatement("UPDATE dts.note SET note_text=?,author_id=?,row_revise_contact_id=? WHERE note_id=?");
	    stmt.setString(1,dataset.getNote());
	    stmt.setInt(2,1);
	    stmt.setInt(3,1);
	    stmt.setInt(4,dataset.getNoteId());
	    stmt.executeUpdate();
	    stmt.close();
	}
    }

    private static void dtsDatasetTable(Connection conn, DatasetBean dataset) throws SQLException {
	PreparedStatement test = conn.prepareStatement("SELECT dataset_id FROM dts.dataset WHERE dataset_id=?");
	test.setString(1,dataset.getDatasetId());
	if (test.executeQuery().next()) {
	    String sql = "UPDATE dts.dataset SET readme_flag=?, approved_contact_id=?, approved_status_id=?, row_revise_contact_id=?, dataset_type=?  WHERE dataset_id=?";
	    PreparedStatement stmt = conn.prepareStatement(sql);
	    stmt.setBoolean(1,dataset.getReadme());
	    if (dataset.getCheckerId() == 0) {
		stmt.setNull(2,Types.INTEGER);
	    } else {
		stmt.setInt(2,dataset.getCheckerId());
	    }
	    if (dataset.getCheckedStatusId() == 0) {
		stmt.setNull(3,Types.INTEGER);
	    } else {
		stmt.setInt(3,dataset.getCheckedStatusId());
	    }
	    stmt.setInt(4,1);
            stmt.setString(5,dataset.getDatasetType());
	    stmt.setString(6,dataset.getDatasetId());
	    stmt.executeUpdate();
	    stmt.close();
	} else {
	    String sql = "INSERT INTO dts.dataset(dataset_id,entered_contact_id,entered_date,readme_flag,approved_contact_id,approved_status_id,row_revise_contact_id,dataset_type) VALUES(?,?,?,?,?,?,?,?)";
	    PreparedStatement stmt = conn.prepareStatement(sql);
	    stmt.setString(1,dataset.getDatasetId());
	    stmt.setInt(2,1);
	    stmt.setDate(3,new java.sql.Date(Calendar.getInstance().getTimeInMillis()));
	    stmt.setBoolean(4,dataset.getReadme());
	    if (dataset.getCheckerId() == 0) {
		stmt.setNull(5,Types.INTEGER);
	    } else {
		stmt.setInt(5,dataset.getCheckerId());
	    }
	    if (dataset.getCheckedStatusId() == 0) {
		stmt.setNull(6,Types.INTEGER);
	    } else {
		stmt.setInt(6,dataset.getCheckedStatusId());
	    }
	    stmt.setInt(7,1);
            stmt.setString(8,dataset.getDatasetType());
	    stmt.executeUpdate();
	    stmt.close();
	}
	test.close();
    }


    public static boolean insertDB( DatasetBean ds ) {
	boolean success = true;

	try {
	    Connection connection = DLNConnection.getConnection();
	    connection.setAutoCommit(false);

	    String sql = "SELECT MIN(begin_date),MAX(end_date),MIN(minlat),MIN(minlon),MAX(maxlat),MAX(maxlon) FROM jedi7.project WHERE";
	    Iterator itr = ds.getProjects().iterator();
	    while (itr.hasNext()) {
		sql += " project_id='"+itr.next()+"'";
		if (itr.hasNext()) { sql += " OR"; }
	    }
	    ResultSet params = connection.prepareStatement(sql).executeQuery();
	    params.next();
	    

	    sql = "INSERT INTO jedi7.dataset(dataset_id,name,internal_contact_id,begin_date,end_date,minlat,minlon,maxlat,maxlon) VALUES (?,?,?,?,?,?,?,?,?)";
	    PreparedStatement stmt = connection.prepareStatement(sql);
	    stmt.setString(1,ds.getDatasetId());
	    stmt.setString(2,ds.getName());
	    stmt.setInt(3,ds.getInternalContactId());
	    stmt.setDate(4,params.getDate(1));
	    stmt.setDate(5,params.getDate(2));
	    stmt.setBigDecimal(6,params.getBigDecimal(3));
	    stmt.setBigDecimal(7,params.getBigDecimal(4));
	    stmt.setBigDecimal(8,params.getBigDecimal(5));
	    stmt.setBigDecimal(9,params.getBigDecimal(6));
	    stmt.executeUpdate();
	    
	    dtsDatasetTable(connection,ds);
	    dtsDatasetIngestTable(connection,ds);
	    dtsDatasetLoadTable(connection,ds);
	    dtsDatasetNote(connection,ds);
	    jediDatasetProjectTable(connection,ds);

	    connection.commit();

	    connection.setAutoCommit(true);
	    connection.close();
	} catch( SQLException sqle ) {
	    System.err.println( "ERROR: " + sqle.toString() );
	    sqle.printStackTrace();
	    success = false;
	}
	
	return success;
    }

    public static boolean deleteFromDB(DatasetBean ds) {
	boolean success = true;

	try {
	    Connection connection = DLNConnection.getConnection();
	    connection.setAutoCommit(false);

	    { 
		String sql = "DELETE FROM dts.dataset WHERE dataset_id=?";
		PreparedStatement stmt = connection.prepareStatement(sql);
		stmt.setString(1,ds.getDatasetId());
		stmt.executeUpdate();
	    }
	    {
		String sql = "DELETE FROM jedi7.dataset WHERE dataset_id=?";
		PreparedStatement stmt = connection.prepareStatement(sql);
		stmt.setString(1,ds.getDatasetId());
		stmt.executeUpdate();
	    }

	    connection.setAutoCommit(true);
	    connection.close();
	} catch (SQLException e) {
	    System.err.println( "ERROR: " + e.toString() );
	    success = false;
	}
	return success;
    }

    public static boolean setCheckStatus(String datasetId, int statusId) {
	boolean success = true;
	try {
	    Connection connection = DLNConnection.getConnection();
	    PreparedStatement test = connection.prepareStatement("SELECT dataset_id FROM dts.dataset WHERE dataset_id=?");
	    test.setString(1,datasetId);
	    if (test.executeQuery().next()) {
		PreparedStatement stmt = connection.prepareStatement("UPDATE dts.dataset SET approved_status_id=? WHERE dataset_id=?");
		if (statusId == 0) { stmt.setNull(1,Types.INTEGER); }
		else { stmt.setInt(1,statusId); }
		stmt.setString(2,datasetId);
		stmt.executeUpdate();
	    } else {
		DatasetBean dataset = new DatasetBean();
		dataset.setDatasetId(datasetId);
		dataset.setCheckedStatusId(statusId);
		dtsDatasetTable(connection,dataset);
	    }
	    connection.close();
	} catch (SQLException e) {
	    System.err.println("ERROR: "+e.toString());
	    success = false;
	}
	return success;
    }

    public static boolean setLoadStatus(String datasetId, int statusId) {
	boolean success = true;
	try {
	    Connection connection = DLNConnection.getConnection();
	    PreparedStatement test = connection.prepareStatement("SELECT dataset_id FROM dts.dataset_load WHERE dataset_id=?");
	    test.setString(1,datasetId);
	    if (test.executeQuery().next()) {
		PreparedStatement stmt = connection.prepareStatement("UPDATE dts.dataset_load SET load_status_id=? WHERE dataset_id=?");
		if (statusId == 0) { stmt.setNull(1,Types.INTEGER); }
		else { stmt.setInt(1,statusId); }
		stmt.setString(2,datasetId);
		stmt.executeUpdate();
	    } else {
		DatasetBean dataset = new DatasetBean();
		dataset.setDatasetId(datasetId);
		dataset.setLoadedStatusId(statusId);
		dtsDatasetTable(connection,dataset);
		dtsDatasetLoadTable(connection,dataset);
	    }
	    connection.close();
	} catch (SQLException e) {
	    System.err.println("ERROR: "+e.toString());
	    success = false;
	}
	return success;
    }

    public static boolean setReadmeFlag(String datasetId, boolean flag) {
	boolean success = true;
	try {
	    Connection connection = DLNConnection.getConnection();
	    
	    PreparedStatement test = connection.prepareStatement("SELECT dataset_id FROM dts.dataset WHERE dataset_id=?");
	    test.setString(1,datasetId);
	    if (test.executeQuery().next()) {
		PreparedStatement stmt = connection.prepareStatement("UPDATE dts.dataset SET readme_flag=? WHERE dataset_id=?");
		stmt.setBoolean(1,flag);
		stmt.setString(2,datasetId);
		stmt.executeUpdate();
	    } else {
		DatasetBean dataset = new DatasetBean();
		dataset.setDatasetId(datasetId);
		dataset.setReadme(flag);
		dtsDatasetTable(connection,dataset);
	    }
	    connection.close();
	} catch (SQLException e) {
	    System.err.println( "ERROR: " + e.toString() );
	    success = false;	    
	}
	return success;
    }
    /*

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

*/
}
