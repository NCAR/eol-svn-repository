package dts.port;

import java.sql.*;

/**
 * The Note class is a container for a Note in the DTS.  It can be for either a
 * Dataset or a Project.
 * 
 * @author jclawson
 */
public class Note {

	private Timestamp entryDate;
	private int id;
	private String content;
	private User author;
	
	/**
	 * Create a new instance of a Note.
	 * @param author The author of the note.
	 * @param entryDate The date the note was created.
	 * @param content The text of the note.
	 */
	public Note(User author, Timestamp entryDate, String content) {
		this.author = author;
		this.entryDate = entryDate;
		this.content = content;
	}
	
	/**
	 * Get the User who wrote the Note.
	 * @return The note's author.
	 */
	public User getAuthor() { return author; }
	
	/**
	 * Get the date the note was created.
	 * @return The creation date of the note.
	 */
	public Timestamp getEntryDate() { return entryDate; }
	
	/**
	 * Get the DTS ID number for the note.
	 * @return The note's DTS ID number.
	 */
	public int getId() { return id; }
	
	/**
	 * Get the note's contents.
	 * @return The note's text.
	 */
	public String getNoteText() { return content; }
	
	/**
	 * Set the DTS ID for the Note.
	 * @param id The Note's DTS ID number.
	 */
	private void setId(int id) { this.id = id; }
	
	/**
	 * Insert the Note into the DTS database.
	 * @param connection The connection to the DTS database.
	 * @return <code>true</code> if the Note was inserted successfully, <code>false</code>
	 * if it was not.
	 * @throws SQLException if there was a problem executing the statements on the database.
	 */
	public boolean storeInDataTrackingSystem(Connection connection) throws SQLException {
		// Define the success flag used to return the success of storing the note information into the database.
		boolean success = true;
		// Define the SQL to store the note information into the database.
		String sql = "INSERT INTO note(author_id, entry_date, note_text, row_revise_time) VALUES(?, ?, ?, ?)";
		// Define the statement to execute on the database.
		PreparedStatement stmt = connection.prepareStatement(sql, PreparedStatement.RETURN_GENERATED_KEYS);

		// Assign the note information to the statement.
		stmt.setInt(1, getAuthor().getDtsId());
		stmt.setTimestamp(2, getEntryDate());
		stmt.setString(3, getNoteText());
		stmt.setTimestamp(4, getEntryDate());
		// Execute the statement.
		stmt.execute(); 
		
		// Get the auto-generated key for the User
		ResultSet keys = stmt.getGeneratedKeys();
		// Assign the DTS ID if the key was returned.
		if (keys.next()) { setId(keys.getInt(1)); }
		// There is some problem, since the key was not returned.
		else { success = false; }
		// Properly close the results stream.
		try { keys.close(); } catch (SQLException e) {}

		// Close the open statement stream.
		try { stmt.close(); } catch (SQLException e) {}
		
		// Return the success status of the insert.
		return success;
	}

	/**
	 * Associate the note to a data set.
	 * @param connection The connection to the DTS database.
	 * @param dataset The data set to be assigned the note.
	 * @param type The type of the note.
	 * @return <code>true</code> if the note was assigned successfully, <code>false</code> if it was not.
	 * @throws SQLException if there is a problem with the statement.
	 */
	public boolean storeInDataTrackingSystem(Connection connection, Dataset dataset, NoteType type) throws SQLException {
		String sql = "INSERT INTO dataset_note(dataset_id, note_id, note_type_id) VALUES(?, ?, ?)";
		PreparedStatement stmt = connection.prepareStatement(sql);
		// Assign the information to the statement.
		stmt.setString(1, dataset.getDatasetId());
		stmt.setInt(2, getId());
		stmt.setInt(3, type.getId());
		// Execute the statement.
		stmt.execute();
		
		// Properly close the open statement stream.
		try { stmt.close(); } catch (SQLException e) {}
		return true;
	}
}
