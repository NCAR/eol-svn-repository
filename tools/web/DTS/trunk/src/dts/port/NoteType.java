package dts.port;

import java.sql.*;
import java.util.*;

/**
 * The NoteType class is the container for a type of a Note.  This is for specifying
 * what task (if any) a note was for.  It includes ingest, processing, loading, and
 * checking task types along with a general type for notes that don't apply to a
 * specific task.
 * 
 * @author jclawson
 */
public class NoteType {

	/** Constant for a Checking Task Note. */
	public static final String CHECK_NOTE = "Check";
	/** Constant for a General Data set/Project note. */
	public static final String GENERAL_NOTE = "General";
	/** Constant for an Ingest Task Note. */
	public static final String INGEST_NOTE = "Ingest";
	/** Constant for a Loading Task Note. */
	public static final String LOAD_NOTE = "Load";
	/** Constant for a Processing Task Note. */
	public static final String PROCESS_NOTE = "Process";
	
	private static TreeMap typeMap;
	
	private int id, sortIndex;
	private String name;

	/**
	 * Create a new instance of a NoteType.
	 * @param name The name of the note type.
	 */
	private NoteType(String name, int sortIndex) {
		this.name = name;
		this.sortIndex = sortIndex;
	}
	
	/**
	 * Create the mapping of the NoteTypes.
	 */
	private static void buildTypeMap() {
		typeMap = new TreeMap();
		
		typeMap.put(GENERAL_NOTE, new NoteType(GENERAL_NOTE, 1));
		typeMap.put(INGEST_NOTE, new NoteType(INGEST_NOTE, 2));
		typeMap.put(PROCESS_NOTE, new NoteType(PROCESS_NOTE, 3));
		typeMap.put(LOAD_NOTE, new NoteType(LOAD_NOTE, 4));
		typeMap.put(CHECK_NOTE, new NoteType(CHECK_NOTE, 5));
	}
	
	/**
	 * Get the name of the Note Type.
	 * @return The Note Type's name.
	 */
	public String getName() { return name; }
	
	/**
	 * Get the DTS ID number for the Note Type.
	 * @return The Note Type's DTS ID number.
	 */
	public int getId() { return id; }
	
	/**
	 * Get the mapping of the Note Types by the name of the type.
	 * @return The note type map.
	 */
	public static TreeMap getNoteTypeMap() {
		if (typeMap == null) { buildTypeMap(); }
		return typeMap;
	}
	
	/**
	 * Get the index to use to sort this note type.
	 * @return The sort index of the note type.
	 */
	public int getSortIndex() { return sortIndex; }
	
	/**
	 * Set the DTS ID number for the Note Type.
	 * @param id The Note Type's DTS ID number.
	 */
	private void setId(int id) { this.id = id; }
	
	/**
	 * Insert the known Note Types into the DTS database.
	 * @param porter The controlling object for the porting process.
	 * @param connection The connection to the DTS database.
	 * @return <code>true</code> if the types were inserted successfully, <code>false</code>
	 * if there was at least one error during the insert.
	 * @throws SQLException if there is a problem executing the insert statements on the database.
	 */
	public static boolean storeDataTrackingSystemNoteTypes(ToDmgDts porter, Connection connection) throws SQLException {
		// Define the success flag used to return the success of storing the note type information into the database.
		boolean success = true;
		// Define the SQL to store the note type information into the database.
		String sql = "INSERT INTO note_type(type_name, sort_index) VALUES(?, ?)";
		// Define the statement to execute on the database.
		PreparedStatement stmt = connection.prepareStatement(sql);
		
		// Define the iterator over the known note types
		Iterator itr = getNoteTypeMap().values().iterator();
		// Loop through each note type in the iterator.
		while (itr.hasNext()) {
			// Pull out the current note type from the iterator.
			NoteType type = (NoteType)itr.next();
			
			boolean localSuccess = true;
			
			// Insert the type into the database.
			try {
				// Assign the current type's information to the statement.
				stmt.setString(1, type.getName());
				stmt.setInt(2, type.getSortIndex());
				// Execute the statement on the database.
				stmt.execute();
			}
			// The statement didn't execute, so it needs to fail properly.
			catch (SQLException e) {
				localSuccess = false;
				success = false;
				porter.appendMergeException(new MergeException("Note Type "+type.getName()+" could not be inserted into the DTS database:  "+e.getMessage()));
			}

			// Obtain the auto-generated DTS ID keys after a successful insert.
			if (localSuccess) {
				// Define an exception holder for a later display.
				SQLException sqlEx = null;
				try {
					// Get the auto-generated key for the User
					ResultSet keys = stmt.getGeneratedKeys();
					// Assign the DTS ID if the key was returned.
					if (keys.next()) { type.setId(keys.getInt(1)); }
					// There is some problem, since the key was not returned.
					else { localSuccess = false; }
					// Properly close the results stream.
					try { keys.close(); } catch (SQLException e) {}
				} catch (SQLException e) {
					localSuccess = false;
					sqlEx = e;
				}
				// There was a local failure for getting the key, so notify the user.
				if (!localSuccess) {
					success = false;
					porter.appendMergeException(new MergeException("Note Type "+type.getName()+" was unable to obtain an auto-generated DTS ID."+(sqlEx == null ? "" : "  "+sqlEx.getMessage())));
				}
			}
		}

		// Properly close the open statement stream.
		try { stmt.close(); } catch (SQLException e) {};

		// Return the status of the execution of the User inserts.
		return success;
	}
}
