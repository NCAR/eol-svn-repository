package dln.beans;

import java.sql.*;
import java.util.*;

/**
 * The StatusBean class is the representation of a status for a DLN data set task.
 * 
 * @author jclawson
 */
public class StatusBean extends DefaultBean implements Comparable<StatusBean> {

	private static final long serialVersionUID = 1966578489691628396L;

	private boolean resolved;
	private int sortIndex, statusId;
	private String name, style;
	
	/**
	 * Create a new instance of a StatusBean.  This is private because it should only
	 * be created when reading the statuses from the database in this class.
	 * @param statusId The database ID for the status.
	 */
	private StatusBean(int statusId) { this.statusId = statusId; }
	
	/**
	 * Compare this StatusBean to the specified StatusBean for sort order.  This uses
	 * the sort index defined in the database for each status.
	 * @param status The StatusBean to compare to <code>this</code> StatusBean.
	 * @return A negative integer, zero, or positive integer if <code>this</code> StatusBean
	 * is less than, equal to, or greater than the specified StatusBean.
	 */
	public int compareTo(StatusBean status) {
		return getSortIndex().compareTo(status.getSortIndex());
	}
	
	/**
	 * Get a list of all of the statuses in the database.
	 * @return The list of the statuses in the database.
	 * @throws SQLException if there is a problem loading the statuses from the database.
	 */
	public static List<StatusBean> getAllStatuses() throws SQLException {
		Connection connection = getConnection();
		List<StatusBean> list = new ArrayList<StatusBean>(getAllStatusesMap(connection).values());
		try { connection.close(); } catch (SQLException e) {}
		return list;
	}

	/**
	 * Get the statuses in the database as a map of the statuses by their database IDs.
	 * @return The mapping of the statuses by their IDs.
	 * @throws SQLException if there is a problem loading the statuses from the database.
	 */
	public static Map<Integer, StatusBean> getAllStatusesMap() throws SQLException {
		Connection connection = getConnection();
		Map<Integer, StatusBean> mapping = getAllStatusesMap(connection);
		try { connection.close(); } catch (SQLException e) {}
		return mapping;
	}

	/**
	 * Get the statuses in the database as a map of the statuses by their database IDs.
	 * @param connection the connection to use to load the statuses.
	 * @return The mapping of the statuses by their IDs.
	 * @throws SQLException if there is a problem loading the statuses from the database.
	 */
	public static Map<Integer, StatusBean> getAllStatusesMap(Connection connection) throws SQLException {
		TreeMap<Integer, StatusBean> mapping = new TreeMap<Integer, StatusBean>();
		
		// Define the SQL to pull the status information from the database.
		String sql = "SELECT status_id, name, css_style, sort_index, is_resolved FROM status";
		
		// Define the statement to be executed on the database.
		PreparedStatement stmt = connection.prepareStatement(sql);
		
		// Execute the query on the database and obtain the results.
		ResultSet results = stmt.executeQuery();
		
		// Loop through the rows of the results.
		while (results.next()) {
			StatusBean status = new StatusBean(results.getInt(1));
			status.setName(results.getString(2));
			status.setStyle(results.getString(3));
			status.setSortIndex(results.getInt(4));
			status.setResolved(results.getBoolean(5));
			
			// The status has been defined, so add it to the map.
			mapping.put(status.getStatusId(), status);
		}
		
		// Close the open database streams.
		try { results.close(); } catch (SQLException e) {}
		try { stmt.close(); } catch (SQLException e) {}			
		
		return mapping;
	}

	/**
	 * Get the name of the status.
	 * @return The status' name.
	 */
	public String getName() { return name; }
	
	/**
	 * Get the index used for sorting the status.
	 * @return The status' sort index.
	 */
	public Integer getSortIndex() { return sortIndex; }
	
	/**
	 * Get the database ID for the status.
	 * @return The status' database ID.
	 */
	public int getStatusId() { return statusId; }
	
	/**
	 * Get the CSS inline style (not style class) used to display the status in the DTS.
	 * @return The status' CSS style.
	 */
	public String getStyle() { return style; }
	
	/**
	 * Determine if the status is considered to be resolved with no further work needed.
	 * @return <code>true</code> if the status is resolved, <code>false</code> if it is not.
	 */
	public boolean isResolved() { return resolved; }
	
	/**
	 * Set the name for the status.
	 * @param name The status' name.
	 */
	public void setName(String name) { this.name = name; }
	
	/**
	 * Set the resolved flag for the status.
	 * @param resolved <code>true</code> if the status is considered resolved, <code>false</code>
	 * if it is not.
	 */
	public void setResolved(boolean resolved) { this.resolved = resolved; }
	
	/**
	 * Set the index used for sorting the status.
	 * @param sortIndex The status' sort index.
	 */
	public void setSortIndex(int sortIndex) { this.sortIndex = sortIndex; }
	
	/**
	 * Set the CSS inline style (not style class) for the status.
	 * @param style The status' CSS style.
	 */
	public void setStyle(String style) { this.style = style; }
	
	/**
	 * Get the String representation of the status.
	 * @return The status' String representation.
	 */
	public String toString() { return getName(); }
}