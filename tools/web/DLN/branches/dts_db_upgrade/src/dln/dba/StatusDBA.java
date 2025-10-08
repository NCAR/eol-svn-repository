package dln.dba;

import dln.beans.*;
import java.sql.*;
import java.util.*;

public class StatusDBA {

    public static HashMap createStatusMap() {
	HashMap statusMap = new HashMap();
	statusMap.put(new Integer(0),new StatusBean(0,"",false));

	try {
	    Connection conn = DLNConnection.getConnection();
	    ResultSet rs = conn.prepareStatement("SELECT status_id,status_name,is_resolved FROM dts.status").executeQuery();
	    while (rs.next()) {
		StatusBean status = new StatusBean(rs.getInt(1),rs.getString(2),rs.getBoolean(3));
		statusMap.put(new Integer(status.getId()),status);
	    }
	} catch (SQLException e) {
	    System.err.println(e);
	    e.printStackTrace();
	}

	return statusMap;
    }

    public static Vector getAllStatuses() {
	Vector dss = new Vector();
	
	try {
	    Connection conn = DLNConnection.getConnection();
	    PreparedStatement stmt = conn.prepareStatement("SELECT status_id,status_name,is_resolved FROM dts.status");
	    
	    ResultSet rs = stmt.executeQuery();
	    while (rs.next()) {
		StatusBean status = new StatusBean();
		status.setId(rs.getInt(1));
		status.setName(rs.getString(2));
		status.setDone(rs.getBoolean(3));
		dss.add(status);
	    }
	} catch (SQLException e) {
	    System.err.println(e);
	    e.printStackTrace();
	}


	return dss;
    }

    public static StatusBean getFromDB(int id) {
	try {
	    return getFromDB(DLNConnection.getConnection(),id);
	} catch (SQLException e) {
	    System.err.println(e);
	    e.printStackTrace();
	    return null;
	}
    }

    public static StatusBean getFromDB(Connection connection, int id) {
	StatusBean status = null;
	
	try {
	    String sql = "SELECT status_id,status_name,is_resolved FROM dts.status WhERE status_id=?";
	    PreparedStatement statement = connection.prepareStatement(sql);
	    statement.setInt(1,id);
			
	    ResultSet rs = statement.executeQuery();
	    if (rs.next()) {
		status = new StatusBean();
		status.setId(rs.getInt(1));
		status.setName(rs.getString(2));
		status.setDone(rs.getBoolean(3));
	    }
	} catch( SQLException sqle ) {
	    System.err.println( sqle );
	    sqle.printStackTrace();
	}
	
	return status;
    }
}


