package dln.beans;

import java.util.*;
import java.sql.*;

/**
 * The NoteBean class is the representation of a note in the DTS.
 * 
 * @author jclawson
 */
public class NoteBean extends DefaultBean implements Comparable<NoteBean> {

	private static final long serialVersionUID = -6151513139560762487L;
	
	private Timestamp entryDate, reviseTime;
	private int noteId;
	private Set<Integer> types;
	private String text;
	private int author;
	
	/**
	 * Create a new instance of a NoteBean.
	 */
	public NoteBean() { this(-1); }

	/**
	 * Create a new instance of a NoteBean.
	 * @param id The database ID of the note.
	 */
	public NoteBean(int id) {
		noteId = id;
		types = new TreeSet<Integer>();
	}
	
	/**
	 * Add a note type to the note.
	 * @param typeId The database ID for the note type.
	 */
	public void addNoteType(Integer typeId) { types.add(typeId); }
	
	/**
	 * Add a note type to the note.
	 * @param type The note type to add to the note.
	 */
	public void addNoteType(NoteTypeBean type) { types.add(type.getTypeId()); }
	
	/**
	 * Compare this NoteBean to the specified NoteBean for sort order.  This uses the
	 * entry date for the notes.
	 * @param type The NoteBean to compare to <code>this</code> NoteBean.
	 * @return A negative integer, zero, or positive integer if <code>this</code> 
	 * NoteBean is less than, equal to, or greater than the specified NoteBean.
	 */
	public int compareTo(NoteBean note) {
		return getEntryDate().compareTo(note.getEntryDate());
	}
	
	/**
	 * Delete the notes in the database that are no longer attached to a data set or project.
	 * @param connection The connection to the database to execute the commands on.
	 * @throws SQLException if there is a problem executing the command.
	 */
	public static void deleteUnattachedNotes(Connection connection) throws SQLException {
		// Define the SQL to delete the notes.
		String sql = "DELETE FROM note WHERE note_id NOT IN (SELECT note_id FROM dataset_note UNION SELECT note_id FROM project_note)";
		// Create the statement to delete the notes.
		PreparedStatement stmt = connection.prepareStatement(sql);
		// Perform the delete.
		stmt.execute();
		// Close the open statement stream.
		try { stmt.close(); } catch (SQLException e) {}
	}
	
	/**
	 * Get the database ID of the User that authored the note.
	 * @return The ID of the note's author.
	 */
	public int getAuthor() { return author; }
	
	/**
	 * Get the author of the note.
	 * @param users The mapping of the UserBeans that may have authored the note.
	 * @return The UserBean author of the note.
	 */
	public UserBean getAuthor(Map<Integer, UserBean> users) { return users.get(getAuthor()); }
	
	/**
	 * Get the Timestamp when the note was created.
	 * @return The creation timestamp of the note.
	 */
	public Timestamp getEntryDate() { return entryDate; }
	
	/**
	 * Get the database ID for the note.
	 * @return The note's database ID.
	 */
	public int getNoteId() { return noteId; }
	
	/**
	 * Get the text of the note.
	 * @return The note's text.
	 */
	public String getNoteText() { return text == null ? "" : text.trim(); }
	
	/**
	 * Get an array of the IDs of the note types associated with the note.
	 * @return The array of note type IDs for the note.
	 */
	public Integer[] getNoteTypes() {
		Integer[] array = new Integer[types.size()];
		int index = 0;
		for (Integer typeId: types) {
			array[index++] = typeId;
		}
		return array;
	}
	
	/**
	 * Get the timestamp when the note was last changed.
	 * @return The revise time of the note.
	 */
	public Timestamp getReviseTime() { return reviseTime; }
	
	/**
	 * Get the note types associated with the note as a comma delimited list.
	 * @param types A mapping of the available note types.
	 * @return The comma delimited list of note types.
	 */
	public String getTypesAsString(Map<Integer, NoteTypeBean> types) {
		StringBuffer sb = new StringBuffer();
		for (Integer typeId: this.types) {
			sb.append(types.get(typeId).getName()).append(", ");
		}
		return sb.length() == 0 ? "" : sb.substring(0, sb.length() - 2);
	}
	
	/**
	 * Insert this note into the database.
	 * @param datasetId The data set the note was written for.
	 * @return <code>true</code> if the note was inserted into the database,
	 * <code>false</code> if it was not.
	 * @throws SQLException if there was a problem inserting the note into the database.
	 */
	public boolean insert(String datasetId) throws SQLException {
		// Only enter a note that has text.
		if (getNoteText() == null || getNoteText().trim().equals("")) {
			return false;
		}
		
		// Define the connection to the database.
		Connection connection = getConnection();
		// Make sure to turn off auto commit so all of the commands will execute as a single transaction.
		connection.setAutoCommit(false);

		DatasetBean dataset = new DatasetBean();
		dataset.setDatasetId(datasetId);
		
		insert(connection, dataset);
		
		connection.commit();
		
		try { connection.close(); } catch (SQLException e) {}
		// There were no errors, so the insert was a success.
		return true;
	}
	
	/**
	 * Insert the note into the database.
	 * @param connection The connection to use to execute the statement.
	 * @param dataset The data set the note was written for.
	 * @throws SQLException if there was a problem inserting the note into the database.
	 */
	public void insert(Connection connection, DatasetBean dataset) throws SQLException {
		// Insert the note into the database.
		String sql = "INSERT INTO note(author_id, entry_date, note_text) VALUES(?, NOW(), ?)";
		PreparedStatement noteStmt = connection.prepareStatement(sql, PreparedStatement.RETURN_GENERATED_KEYS);
		noteStmt.setInt(1, getAuthor());
		noteStmt.setString(2, getNoteText());
		noteStmt.execute();
		// Get the auto-generated note id from the database.
		ResultSet keys = noteStmt.getGeneratedKeys();
		if (keys.next()) { setNoteId(keys.getInt(1)); }
		// Close the open streams.
		try { keys.close(); } catch (SQLException e) {}
		try { noteStmt.close(); } catch (SQLException e) {};
		
		// Associate the note with the data set for each note type.
		sql = "INSERT INTO dataset_note(dataset_id, note_id, note_type_id) VALUES(?, ?, ?)";
		PreparedStatement dsNoteStmt = connection.prepareStatement(sql);
		dsNoteStmt.setString(1, dataset.getDatasetId());
		dsNoteStmt.setInt(2, getNoteId());
		for (Integer typeId: getNoteTypes()) {
			dsNoteStmt.setInt(3, typeId);
			dsNoteStmt.execute();
		}
		// Close the open streams.
		try { dsNoteStmt.close(); } catch (SQLException e) {}
	}
	
	/**
	 * Determine if the note is associated with the specified note type.
	 * @param typeId The database ID for the note type.
	 * @return <code>true</code> if the note type is associated with the note,
	 * <code>false</code> if it is not.
	 */
	public boolean isNoteType(int typeId) {
		return this.types.contains(typeId);
	}

	/**
	 * Determine if the note is associated with the specified note type.
	 * @param type The note type to be tested.
	 * @return <code>true</code> if the note type is associated with the note,
	 * <code>false</code> if it is not.
	 */
	public boolean isNoteType(NoteTypeBean type) { return isNoteType(type.getTypeId()); }
	
	/**
	 * Determine if the note has been updated since it was created.
	 * @return <code>true</code> if the note has been changed, <code>false</code>
	 * if it has not.
	 */
	public boolean isRevised() { return getReviseTime() != null && getReviseTime().compareTo(getEntryDate()) != 0; }
	
	/**
	 * Load the note information for the specified data set.
	 * @param datasetId The data set ID to load the note for.
	 * @throws SQLException if there is a problem reading the information from the database.
	 */
	public void load(String datasetId) throws SQLException {
		// Get the connection to the database.
		Connection connection = getConnection();
		// Define the SQL to read the specific note.
		String noteSQL = "SELECT author_id, note_text, entry_date FROM note WHERE note_id=?";
		// Define the statement to be executed.
		PreparedStatement noteStmt = connection.prepareStatement(noteSQL);
		noteStmt.setInt(1, getNoteId());
		// Get the results of the query.
		ResultSet noteResults = noteStmt.executeQuery();
		// Load the note information into this note.
		if (noteResults.next()) {
			this.setAuthor(noteResults.getInt(1));
			this.setNoteText(noteResults.getString(2));
			this.setEntryDate(noteResults.getTimestamp(3));
		}
		// Close the open database streams.
		try { noteResults.close(); } catch (SQLException e) {}
		try { noteStmt.close(); } catch (SQLException e) {}
		
		// Define the SQL to read the note type information for the data set.
		String typeSQL = "SELECT note_type_id FROM dataset_note WHERE note_id=? AND dataset_id=?";
		// Define the statement to be executed.
		PreparedStatement typeStmt = connection.prepareStatement(typeSQL);
		typeStmt.setInt(1, getNoteId());
		typeStmt.setString(2, datasetId);
		// Get the results of the query.
		ResultSet typeResults = typeStmt.executeQuery();
		// Assign the note types to the note.
		while (typeResults.next()) {
			this.addNoteType(typeResults.getInt(1));
		}
		// Close the open database streams.
		try { typeResults.close(); } catch (SQLException e) {}
		try { typeStmt.close(); } catch (SQLException e) {}
		
		// Close the connection to the database.
		try { connection.close(); } catch (SQLException e) {}
	}
	
	/**
	 * Set the author of the note.
	 * @param author The database ID of the note's author.
	 */
	public void setAuthor(int author) { this.author = author; }
	
	/**
	 * Set the timestamp when the note was created.
	 * @param date The time the note was created.
	 */
	public void setEntryDate(Timestamp date) { entryDate = date; }
	
	/**
	 * Set the database ID for the note.
	 * @param id The note's database ID.
	 */
	public void setNoteId(int id) { noteId = id; }
	
	/**
	 * Set the text for the note.
	 * @param text The note's text.
	 */
	public void setNoteText(String text) { this.text = text; }
	
	/**
	 * Set the note types for the note.
	 * @param types The array of note type database IDs to associate with the note.
	 */
	public void setNoteTypes(Integer[] types) {
		for (Integer type: types) { this.types.add(type); }
	}
	
	/**
	 * Set the time the note was last updated.
	 * @param reviseTime The note's most recent update time.
	 */
	public void setReviseTime(Timestamp reviseTime) { this.reviseTime = reviseTime; }

	/**
	 * Update the note in the database for the specified data set.
	 * @param datasetId The data set ID for the data set assigned to this note.
	 * @return <code>true</code> if the note was updated, <code>false</code> if it
	 * was note.
	 * @throws SQLException if there was a problem updating the note in the database.
	 */
	public boolean update(String datasetId) throws SQLException {
		// Only update the note if there is text to update.
		if (getNoteText() == null || getNoteText().trim().equals("")) {
			return false;
		}
		
		// Define the connection to the database.
		Connection connection = getConnection();
		// Make sure to turn off auto commit so all of the commands will execute as a single transaction.
		connection.setAutoCommit(false);
		
		// Define the SQL to update the note.
		String sql = "UPDATE note SET author_id=?, note_text=? WHERE note_id=?";
		// Define the statement to be executed.
		PreparedStatement stmt = connection.prepareStatement(sql);
		stmt.setInt(1, getAuthor());
		stmt.setString(2, getNoteText());
		stmt.setInt(3, getNoteId());
		// Update the note.
		stmt.execute();
		// Close the open database stream.
		try { stmt.close(); } catch (SQLException e) {}
		
		// Define the SQL to remove all of the original note types for the data set.
		sql = "DELETE FROM dataset_note WHERE dataset_id=? AND note_id=?";
		// Define the statement to be executed.
		PreparedStatement delStmt = connection.prepareStatement(sql);
		delStmt.setString(1, datasetId);
		delStmt.setInt(2, getNoteId());
		// Delete the data set note types.
		delStmt.execute();
		// Close the open database stream.
		try { delStmt.close(); } catch (SQLException e) {}
		
		// Define the SQL to insert the new note types with the data set.
		sql = "INSERT INTO dataset_note(dataset_id, note_id, note_type_id) VALUES(?, ?, ?)";
		// Define the statement to perform the insert.
		PreparedStatement dsNoteStmt = connection.prepareStatement(sql);
		dsNoteStmt.setString(1, datasetId);
		dsNoteStmt.setInt(2, getNoteId());
		// Insert each note type with the data set.
		for (Integer typeId: getNoteTypes()) {
			dsNoteStmt.setInt(3, typeId);
			dsNoteStmt.execute();
		}
		// Close the open database stream.
		try { dsNoteStmt.close(); } catch (SQLException e) {}
		
		// Save all of the changes to the database.
		connection.commit();
		
		// Close the connection to the database.
		try { connection.close(); } catch (SQLException e) {}
		// There were no errors, so the insert was a success.
		return true;
	}
}
