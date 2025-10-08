package dts.port;

import java.sql.*;
import java.util.Iterator;
import java.util.TreeMap;

/**
 * The IngestType class is a container for a manner in which a Dataset was ingested
 * into EOL.  This could be by Email, FTP, scripts, CD/DVD, hard drives, etc.
 * 
 * @author jclawson
 */
public class IngestType {

	private static TreeMap typeMap;
	
	private int id;
	private String name;
	
	/**
	 * Create a new instance of an IngestType.
	 * @param name The name of the ingest type.
	 */
	private IngestType(String name) {
		this.name = name;
	}
	
	/**
	 * Create the mapping of the IngestTypes by name.
	 */
	private static void buildIngestTypeMap() {
		typeMap = new TreeMap();
		
		typeMap.put("CD", new IngestType("CD"));
		typeMap.put("DVD", new IngestType("DVD"));
		typeMap.put("Email", new IngestType("Email"));
		typeMap.put("Floppy", new IngestType("Floppy Disc"));
		typeMap.put("Hard Drive", new IngestType("Hard Drive"));
		typeMap.put("FTP", new IngestType("FTP"));
		typeMap.put("Jump Drive", new IngestType("Flash/Jump/Thumb Drive"));
		typeMap.put("MSS", new IngestType("Mass Store"));
		typeMap.put("Paper", new IngestType("Paper Copy"));
		typeMap.put("Tapes", new IngestType("Data Tapes"));
		typeMap.put("VHS", new IngestType("VHS Tapes"));
		typeMap.put("Web", new IngestType("Web"));
	}

	/**
	 * Get the DTS ID number for the IngestType.
	 * @return The IngestType's DTS ID number.
	 */
	public int getId() { return id; }
	
	/**
	 * Get the name of the IngestType.
	 * @return The IngestType's name.
	 */
	public String getName() { return name; }
	
	/**
	 * Get the mapping of all of the IngestTypes by name.
	 * @return The mapping of the IngestTypes.
	 */
	public static TreeMap getIngestTypeMap() {
		if (typeMap == null) { buildIngestTypeMap(); }
		return typeMap;
	}
	
	/**
	 * Set the DTS ID number for the IngestType.
	 * @param id The IngestType's DTS ID number.
	 */
	private void setId(int id) { this.id = id; }
	
	/**
	 * Insert all of the known IngestTypes into the DTS database.
	 * @param porter The controlling object for the porting process.
	 * @param connection The connection to the DTS database.
	 * @return <code>true</code> if all of the IngestTypes were inserted into the database successfully,
	 * <code>false</code> if there was at least one problem with the insert.
	 * @throws SQLException if there was problem with the exection of the statements to the database.
	 */
	public static boolean storeDataTrackingSystemIngestTypes(ToDmgDts porter, Connection connection) throws SQLException {
		// Define the success flag used to return the success of storing the ingest type information into the database.
		boolean success = true;
		// Define the SQL to store the ingest type information into the database.
		String sql = "INSERT INTO ingest_type(name) VALUES(?)";
		// Define the statement to execute on the database.
		PreparedStatement stmt = connection.prepareStatement(sql);
		
		// Define the iterator over the known ingest types.
		Iterator itr = getIngestTypeMap().values().iterator();
		// Loop through each ingest type in the iterator.
		while (itr.hasNext()) {
			// Pull out the current ingest type from the iterator.
			IngestType type = (IngestType)itr.next();

		
			boolean localSuccess = true;
			
			// Insert the ingest type into the database.
			try {
				// Assign the current ingest type's information to the statement.
				stmt.setString(1, type.getName());
				// Execute the statement on the database.
				stmt.execute();
			}
			// The statement didn't execute, so it needs to fail properly.
			catch (SQLException e) {
				localSuccess = false;
				success = false;
				porter.appendMergeException(new MergeException("Ingest Type "+type.getName()+" could not be inserted into the DTS database:  "+e.getMessage()));
			}

			// Obtain the auto-generated DTS ID keys after a successful insert.
			if (localSuccess) {
				// Define an exception holder for a later display.
				SQLException sqlEx = null;
				try {
					// Get the auto-generated key.
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
					porter.appendMergeException(new MergeException("Ingest Type "+type.getName()+" was unable to obtain an auto-generated DTS ID."+(sqlEx == null ? "" : "  "+sqlEx.getMessage())));
				}
			}
		}

		// Properly close the open statement stream.
		try { stmt.close(); } catch (SQLException e) {};

		// Return the status of the execution of the ingest type inserts.
		return success;
	}

}
