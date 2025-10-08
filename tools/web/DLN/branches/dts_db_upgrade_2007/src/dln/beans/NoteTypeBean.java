package dln.beans;

import java.sql.*;
import java.util.*;

/**
 * The NoteTypeBean class is the representation of a note type for a DTS note.
 * 
 * @author jclawson
 */
public class NoteTypeBean extends DefaultBean implements Comparable<NoteTypeBean> {

	private static final long serialVersionUID = -2019173065444893140L;
	
	private int sortIndex, typeId;
	private String name;
	
	/**
	 * Create a new instance of a NoteTypeBean.  This is private because it should only
	 * be created when reading the note types from the database in this class.
	 * @param id The database ID for the note type.
	 */
	private NoteTypeBean(int id) { typeId = id; }
	
	/**
	 * Compare this NoteTypeBean to the specified NoteTypeBean for sort order.  This uses
	 * the sort index defined in the database for each note type.
	 * @param type The NoteTypeBean to compare to <code>this</code> NoteTypeBean.
	 * @return A negative integer, zero, or positive integer if <code>this</code> 
	 * NoteTypeBean is less than, equal to, or greater than the specified NoteTypeBean.
	 */
	public int compareTo(NoteTypeBean type) {
		return getSortIndex().compareTo(type.getSortIndex());
	}
	
	/**
	 * Get the name of the note type.
	 * @return The note type's name.
	 */
	public String getName() { return name; }

	/**
	 * Get the note types in the database as a map of the note types by their database IDs.
	 * @return The mapping of the note types by their IDs.
	 * @throws SQLException if there is a problem loading the note types from the database.
	 */
	public static Map<Integer, NoteTypeBean> getNoteTypesMap() throws SQLException {
		Connection connection = getConnection();
		Map<Integer, NoteTypeBean> mapping = getNoteTypesMap(connection);
		try { connection.close(); } catch (SQLException e) {}
		return mapping;
	}
	
	/**
	 * Get the note types in the database as a map of the note types by their database IDs.
	 * @param connection the connection to use to load the note types.
	 * @return The mapping of the note types by their IDs.
	 * @throws SQLException if there is a problem loading the note types from the database.
	 */
	public static Map<Integer, NoteTypeBean> getNoteTypesMap(Connection connection) throws SQLException {
		TreeMap<Integer, NoteTypeBean> mapping = new TreeMap<Integer, NoteTypeBean>();
		
		// Define the SQL to pull the type information from the database.
		String sql = "SELECT note_type_id, type_name, sort_index FROM note_type";
		
		// Define the statement to be executed on the database.
		PreparedStatement stmt = connection.prepareStatement(sql);
		
		// Execute the query on the database and obtain the results.
		ResultSet results = stmt.executeQuery();
		
		// Loop through the rows of the results.
		while (results.next()) {
			NoteTypeBean type = new NoteTypeBean(results.getInt(1));
			type.setName(results.getString(2));
			type.setSortIndex(results.getInt(3));
			
			// The type has been defined, so add it to the map.
			mapping.put(type.getTypeId(), type);
		}
		
		// Close the open database streams.
		try { results.close(); } catch (SQLException e) {}
		try { stmt.close(); } catch (SQLException e) {}			
		
		return mapping;
	}
	
	/**
	 * Get the index used for sorting the note type.
	 * @return The note type's sort index.
	 */
	public Integer getSortIndex() { return sortIndex; }
	
	/**
	 * Get the database ID for the note type.
	 * @return The note type's database ID.
	 */
	public int getTypeId() { return typeId; }

	/**
	 * Set the name for the note type.
	 * @param name The note type's name.
	 */
	public void setName(String name) { this.name = name; }
	
	/**
	 * Set the index used for sorting the note type.
	 * @param sortIndex The note type's sort index.
	 */
	public void setSortIndex(int index) { sortIndex = index; }

	/**
	 * Get the String representation of the note type.
	 * @return The note type's String representation.
	 */
	public String toString() { return getName(); }
}
