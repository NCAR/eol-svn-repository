package dln.beans;

import java.sql.*;
import java.util.*;

/**
 * The IngestTypeBean class is the representation of a ingest type in the DTS.
 * 
 * @author jclawson
 */
public class IngestTypeBean extends DefaultBean implements Comparable<IngestTypeBean> {

	private static final long serialVersionUID = -2326889971013316881L;

	private int typeId;
	private String name;
	
	/**
	 * Create a new instance of a IngestTypeBean.  This is private because it should only
	 * be created when reading the ingest types from the database in this class.
	 * @param id The database ID for the ingest type.
	 */
	private IngestTypeBean(int id) { typeId = id; }
	
	/**
	 * Compare this IngestTypeBean to the specified IngestTypeBean for sort order.  This uses
	 * the sort index defined in the database for each ingest type.
	 * @param type The IngestTypeBean to compare to <code>this</code> IngestTypeBean.
	 * @return A negative integer, zero, or positive integer if <code>this</code> 
	 * IngestTypeBean is less than, equal to, or greater than the specified IngestTypeBean.
	 */
	public int compareTo(IngestTypeBean type) {
		return getName().compareTo(type.getName());
	}
	
	/**
	 * Get a list of all of the ingest types in the database.
	 * @return The list of the ingest types in the database.
	 * @throws SQLException if there is a problem loading the ingest types from the database.
	 */
	public static List<IngestTypeBean> getAllIngestTypes() throws SQLException {
		Connection connection = getConnection();
		List<IngestTypeBean> list = new ArrayList<IngestTypeBean>(getAllIngestTypesMap(connection).values());
		try { connection.close(); } catch (SQLException e) {}
		return list;
	}
	
	/**
	 * Get the ingest types in the database as a map of the ingest types by their database IDs.
	 * @return The mapping of the ingest types by their IDs.
	 * @throws SQLException if there is a problem loading the ingest types from the database.
	 */
	public static Map<Integer, IngestTypeBean> getAllIngestTypesMap() throws SQLException {
		Connection connection = getConnection();
		Map<Integer, IngestTypeBean> mapping = getAllIngestTypesMap(connection);
		try { connection.close(); } catch (SQLException e) {}
		return mapping;
	}
	
	/**
	 * Get the ingest types in the database as a map of the ingest types by their database IDs.
	 * @param connection the connection to use to load the ingest types.
	 * @return The mapping of the ingest types by their IDs.
	 * @throws SQLException if there is a problem loading the ingest types from the database.
	 */
	public static Map<Integer, IngestTypeBean> getAllIngestTypesMap(Connection connection) throws SQLException {
		TreeMap<Integer, IngestTypeBean> mapping = new TreeMap<Integer, IngestTypeBean>();
		
		// Define the SQL to pull the type information from the database.
		String sql = "SELECT ingest_type_id, name FROM ingest_type";
		
		// Define the statement to be executed on the database.
		PreparedStatement stmt = connection.prepareStatement(sql);
		
		// Execute the query on the database and obtain the results.
		ResultSet results = stmt.executeQuery();
		
		// Loop through the rows of the results.
		while (results.next()) {
			IngestTypeBean type = new IngestTypeBean(results.getInt(1));
			type.setName(results.getString(2));
			
			// The type has been defined, so add it to the map.
			mapping.put(type.getTypeId(), type);
		}
		
		// Close the open database streams.
		try { results.close(); } catch (SQLException e) {}
		try { stmt.close(); } catch (SQLException e) {}			
		
		return mapping;
	}
	
	/**
	 * Get the name of the ingest type.
	 * @return The ingest type's name.
	 */
	public String getName() { return name; }
	
	/**
	 * Get the database ID for the ingest type.
	 * @return The ingest type's database ID.
	 */
	public int getTypeId() { return typeId; }

	/**
	 * Set the name for the ingest type.
	 * @param name The ingest type's name.
	 */
	public void setName(String name) { this.name = name; }

	/**
	 * Get the String representation of the ingest type.
	 * @return The ingest type's String representation.
	 */
	public String toString() { return getName(); }
}
