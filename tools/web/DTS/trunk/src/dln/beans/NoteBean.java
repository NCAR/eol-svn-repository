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
	 * @param note The NoteBean to compare to <code>this</code> NoteBean.
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
		/***************************************************
		 * Doing 2 separate statements (1 to find the notes and another to delete) is
		 * faster than doing a single delete statement (DELETE ... WHERE note_id IN (SELECT ...))
		 **************************************************/
		// Define the SQL to delete the notes.
		String sql = "SELECT note_id FROM note WHERE note_id NOT IN (SELECT note_id FROM dataset_note UNION SELECT note_id FROM project_note UNION SELECT collection_time_note_id FROM dataset_source_info UNION SELECT source_info_note_id FROM dataset_source_info)";
		// Create the statement to delete the notes.
		PreparedStatement stmt = connection.prepareStatement(sql);
		// Perform the delete.
		ResultSet results = stmt.executeQuery();
		
		// Define the statement to delete a specific note.
		PreparedStatement delStmt = connection.prepareStatement("DELETE FROM note WHERE note_id=?");
		
		// Delete the notes that aren't attached, one by one.
		while (results.next()) {
			delStmt.setInt(1, results.getInt(1));
			delStmt.execute();
		}
		
		// Close the open statement stream.
		try { delStmt.close(); } catch (SQLException e) {}
		try { results.close(); } catch (SQLException e) {}
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
	 * Get the time when the note was created.
	 * @return The creation time of the note.
	 */
	public String getEntryDateString() { return getEntryDate().toString().substring(0, 19); }
	
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
	 * Get the time when the note was last changed.
	 * @return The revise time of the notes.
	 */
	public String getReviseTimeString() { return getReviseTime().toString().substring(0, 19); }
	
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
	 * Insert the note and attach it to the specified project.
	 * @param projectId The ID of the project to attach the note to.
	 * @return <code>true</code> if the insert was successful, <code>false</code> if it was not.
	 * @throws SQLException if there is a problem inserting the note.
	 */
	public boolean insertProject(String projectId) throws SQLException {
		// Only enter a note that has text.
		if (getNoteText() == null || getNoteText().trim().equals("")) {
			return false;
		}
		
		// Define the connection to the database.
		Connection connection = getConnection();
		// Make sure to turn off auto commit so all of the commands will execute as a single transaction.
		connection.setAutoCommit(false);

		ProjectBean project = new ProjectBean();
		project.setProjectId(projectId);
		
		insert(connection, project);
		
		connection.commit();
		
		try { connection.close(); } catch (SQLException e) {}
		// There were no errors, so the insert was a success.
		return true;
	}
	
	/**
	 * Insert the note and attach it to the specified project.
	 * @param connection The connection to use to insert the note.
	 * @param project The project the note is to be assigned to.
	 * @throws SQLException if there is a problem inserting the note.
	 */
	public void insert(Connection connection, ProjectBean project) throws SQLException {
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
		
		PreparedStatement noteType = connection.prepareStatement("SELECT note_type_id FROM note_type WHERE type_name=?");
		noteType.setString(1, "General");
		ResultSet resultType = noteType.executeQuery();
		int noteTypeId = 0;
		if (resultType.next()) { noteTypeId = resultType.getInt(1); }
		try { resultType.close(); } catch (SQLException e) {}
		try { noteType.close(); } catch (SQLException e) {}
		
		// Associate the note with the data set for each note type.
		sql = "INSERT INTO project_note(project_id, note_id, note_type_id) VALUES(?, ?, ?)";
		PreparedStatement projNoteStmt = connection.prepareStatement(sql);
		projNoteStmt.setString(1, project.getProjectId());
		projNoteStmt.setInt(2, getNoteId());
		projNoteStmt.setInt(3, noteTypeId);
		projNoteStmt.execute();
		// Close the open streams.
		try { projNoteStmt.close(); } catch (SQLException e) {}
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
	 * Determine if the note is a process note.
	 * @param types The mapping of potential types of notes.
	 * @return <code>true</code> if the note is a general or processing note, <code>false</code> if it is not.
	 */
	public boolean isProcessNote(Map<Integer, NoteTypeBean> types) {
		for (Integer typeId: getNoteTypes()) {
			if (types.get(typeId).getName().contains("Process") || types.get(typeId).getName().contains("General")) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Determine if the note has been updated since it was created.
	 * @return <code>true</code> if the note has been changed, <code>false</code>
	 * if it has not.
	 */
	public boolean isRevised() { return getReviseTime() != null && getReviseTime().compareTo(getEntryDate()) != 0; }
	
	/**
	 * Load a note from the database.
	 * @param connection The connection to use to connect to the database.
	 * @param noteId The DTS ID of the note to be loaded.
	 * @return The note loaded from the database.
	 * @throws SQLException when there is a problem loading the note from the database.
	 */
	public static NoteBean load(Connection connection, Integer noteId) throws SQLException {
		String sql = "SELECT author_id, entry_date, note_text, row_revise_time FROM note WHERE note_id=?";
		PreparedStatement stmt = connection.prepareStatement(sql);
		stmt.setInt(1, noteId);
		ResultSet results = stmt.executeQuery();
		
		// Load the note with the info from the database.
		NoteBean note = new NoteBean();
		if (results.next()) {
			note.setNoteId(noteId);
			note.setAuthor(results.getInt(1));
			note.setEntryDate(results.getTimestamp(2));
			note.setNoteText(results.getString(3));
			note.setReviseTime(results.getTimestamp(4));
		}
		
		// Close the open streams.
		try { results.close(); } catch (SQLException e) {}
		try { stmt.close(); } catch (SQLException e) {}
		
		return note;
	}
	
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
	 * Load the note for the specified project.
	 * @param projectId The ID of the project to be loaded.
	 * @throws SQLException if there is a problem loading the note from the database.
	 */
	public void loadProject(String projectId) throws SQLException {
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
	 * Update this note in the database.
	 * @param connection The connection to use to update the note.
	 * @throws SQLException if there is a problem updating the note.
	 */
	public void update(Connection connection) throws SQLException {
		String sql = "UPDATE note SET author_id=?, note_text=? WHERE note_id=?";
		PreparedStatement stmt = connection.prepareStatement(sql);
		stmt.setInt(1, getAuthor());
		stmt.setString(2, getNoteText());
		stmt.setInt(3, getNoteId());
		stmt.execute();
		try { stmt.close(); } catch (SQLException e) {}
	}
	
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
	
	/**
	 * Update the note for the specified project.
	 * @param projectId The ID of the project the note is being updated for.
	 * @return <code>true</code> if the note update was successful, <code>false</code> otherwise.
	 * @throws SQLException when there is a problem updating the note.
	 */
	public boolean updateProject(String projectId) throws SQLException {
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
				
		// Save all of the changes to the database.
		connection.commit();
		
		// Close the connection to the database.
		try { connection.close(); } catch (SQLException e) {}
		// There were no errors, so the insert was a success.
		return true;
	}
}
