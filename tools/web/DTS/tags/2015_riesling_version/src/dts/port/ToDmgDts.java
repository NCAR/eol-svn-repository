package dts.port;

import java.sql.*;
import java.util.*;

/**
 * The ToDmgDts is the controlling class for porting the DMG DLN and IVEN databases
 * to the DMG DTS database.  It loads the information from the DLN and IVEN databases along
 * with the data sets in EMDAC/ZITH for the projects in the DTS and combines the information into
 * a single set.  The information is also compared with the DTS single set as well.  Finally,
 * the single set of DTS information is inserted into the new DTS database.
 * 
 * @author jclawson
 */
public class ToDmgDts {

	// Define some never changing constants for database information.
	private static final String DLN_URL = "jdbc:mysql://localhost/dmg_dln";
	private static final String ML_URL = "jdbc:mysql://localhost/dmg_merged_ml";
	private static final String IVEN_URL = "jdbc:mysql://localhost/dmg_iven";
//	private static final String ZEDI_URL = "jdbc:mysql://tsunami.eol.ucar.edu/zedi";
	private static final String ZITH_URL = "jdbc:mysql://sferic.eol.ucar.edu/zith9";
	private static final String DMG_PASS = "l@micbc";
	private static final String DMG_USER = "dts-full";
//	private static final String ZEDI_USER = "XXX";  // NO! don't use guimaint here. use zediupdate or dts-specific user. -jja
//	private static final String ZEDI_PASS = "XXX";
	private static final String ZITH_USER = "zithview"; //"zithupdate";
	private static final String ZITH_PASS = "look-999"; //"change-999";
	
	private boolean successStatus;
	private PortFrame frame;		
	private TreeMap datasets, projects, users;
	
	/**
	 * Create a new instance of the ToDmgDts class.
	 * @throws ClassNotFoundException if the database driver cannot be found.
	 */
	public ToDmgDts() throws ClassNotFoundException {
		// Try to find the MySQL database driver.
		Class.forName("com.mysql.jdbc.Driver");
		// Initialize the single set maps for the DTS information.
		projects = new TreeMap();
		users = new TreeMap();
		datasets = new TreeMap();
		
		successStatus = true;
	}
	
	/**
	 * Add an exception to the display running the port.
	 * @param e The exception to be displayed.
	 */
	public void appendMergeException(MergeException e) { 
		frame.appendExceptionMessage(e.getMessage());
		successStatus = false;
	}
	
	/**
	 * Change to the specified status for the port.
	 * @param msg The status message to be displayed.
	 */
	private void changeStatus(String msg) {	
		frame.setStatus(msg);
		frame.appendMessage(msg);
	}
	
	/**
	 * Clear all of the data that has been done on a previous port.
	 */
	public void clearAllData() {
		projects.clear();
		users.clear();
		datasets.clear();
		successStatus = true;
	}
	
	/**
	 * Get the current date and time.
	 * @return The current date and time.
	 */
	public java.sql.Date getCurrentTime() {
		return new java.sql.Date(new java.util.Date().getTime());
	}
	
	/**
	 * Load the project, user, and data set information from the Data Loading Notes into
	 * the single set DTS mappings.
	 * @throws SQLException if there is a problem accessing or reading the DLN database.
	 */
	public void loadDataLoadingNotes() throws SQLException {
		// Get the connection to the DLN database.
		Connection connection = DriverManager.getConnection(DLN_URL, DMG_USER, DMG_PASS);
		// Create an object to hold an exception to allow the connection to be properly terminated.
		SQLException sqlException = null;
		
		try {
			/* NOTE:  The success variable is to prevent the import of information past a failure that would
			 * be required by the later import.  (i.e.  A data set can only be loaded if all of the users and
			 * projects have already been loaded correctly.  It also allows the porting to do as much as it
			 * can before ending to allow the user to make multiple corrections.
			 */
			
			// 1.  Load the projects.
			changeStatus("Loading the Projects from the DLN...");
			boolean success = Project.loadDlnProjects(this, connection, projects);
			// 2.  Load the users.
			changeStatus("Loading the Users from the DLN...");
			success = User.loadDlnUsers(this, connection, users) && success;
			// 3.  Load the data sets.  This requires the users and projects to have loaded correctly.
			if (success) { 
				changeStatus("Loading the Data Sets from the DLN...");
				success = Dataset.loadDlnDatasets(this, connection, projects, users, datasets);
			} else {
				changeStatus("Loading of DLN datasets was skipped due to a previous failure.");
			}
		} catch (SQLException e) {
			// Store the exception to be handled later.
			sqlException = e;
		}
		
		// Close the DLN connection since it is no longer needed.
		try { connection.close(); } catch (SQLException e) {}
		
		// The connection should be closed now, so throw the exception that was to be handled later (if there is one).
		if (sqlException != null) { throw sqlException; }
	}
	
	/**
	 * Load the data sets from EMDAC that are not already in the DTS merged mappings and compare the values for
	 * the ones that are already in them.  Also compare the projects and users with the information in EMDAC.
	 * @throws SQLException if there is a problem accessing or reading the ZEDI database.
	 */
	public void loadEMDAC() throws SQLException {
		// Get the connection to the ZEDI database.
//		Connection connection = DriverManager.getConnection(ZEDI_URL, ZEDI_USER, ZEDI_PASS);
		Connection connection = DriverManager.getConnection(ZITH_URL, ZITH_USER, ZITH_PASS);
		// Create an object to hold an exception to allow the connection to be properly terminated.
		SQLException sqlException = null;
		
		try {
			/* NOTE:  The success variable is to prevent the import of information past a failure that would
			 * be required by the later import.  (i.e.  A data set can only be loaded if all of the users and
			 * projects have already been loaded correctly.  It also allows the porting to do as much as it
			 * can before ending to allow the user to make multiple corrections.
			 */
			
			// 1.  Load the projects.
			changeStatus("Comparing the Projects from EMDAC...");
			boolean success = Project.compareEmdacProjects(this, connection, projects);
			// 2.  Load the users.
			changeStatus("Comparing the Users from EMDAC...");
			success = User.compareEmdacUsers(this, connection, users) && success;
			// 3.  Load the data sets.  This requires the users, projects, and products to have loaded correctly.
			if (success) {
				changeStatus("Loading the Datasets from EMDAC...");
				success = Dataset.loadEmdacDatasets(this, connection, projects, users, datasets);
			} else {
				changeStatus("Loading of EMDAC data sets was skipped due to a previous failure.");
			}
		} catch (SQLException e) {
			// Store the exception to be handled later.
			sqlException = e;
		}
		
		// Close the ZEDI connection since it is no longer needed.
		try { connection.close(); } catch (SQLException e) {}
		
		// The connection should be closed now, so throw the exception that was to be handled later (if there is one).
		if (sqlException != null) { throw sqlException; }
	}
	
	/**
	 * Load the project, user, and data set (including products) information from the processing Inventory into
	 * the single set DTS mappings.
	 * @throws SQLException if there is a problem accessing or reading the IVEN database.
	 */
	public void loadIVEN() throws SQLException {
		// Get the connection to the IVEN database.
		Connection connection = DriverManager.getConnection(IVEN_URL, DMG_USER, DMG_PASS);
		// Create an object to hold an exception to allow the connection to be properly terminated.
		SQLException sqlException = null;
		
		try {
			/* NOTE:  The success variable is to prevent the import of information past a failure that would
			 * be required by the later import.  (i.e.  A data set can only be loaded if all of the users and
			 * projects have already been loaded correctly.  It also allows the porting to do as much as it
			 * can before ending to allow the user to make multiple corrections.
			 */
			
			// 1.  Load the projects.
			changeStatus("Loading the Projects from IVEN...");
			boolean success = Project.loadIvenProjects(this, connection, projects);
			// 2.  Load the users.
			changeStatus("Loading the Users from the IVEN...");
			success = User.loadIvenUsers(this, connection, users) && success;
			// 3.  Load the product.  This requires the users and projects to have loaded correctly.
			if (success) { 
				changeStatus("Loading the Products from the IVEN...");
				success = Dataset.loadIvenProducts(this, connection, projects, users, datasets);
			} else {
				changeStatus("Loading of IVEN products was skipped due to a previous failure.");
			}
			// 4.  Load the data sets.  This requires the users, projects, and products to have loaded correctly.
			if (success) {
				changeStatus("Loading the Datasets from IVEN...");
				success = Dataset.loadIvenDatasets(this, connection, projects, users, datasets);
			} else {
				changeStatus("Loading of IVEN data sets was skipped due to a previous failure.");
			}
		} catch (SQLException e) {
			// Store the exception to be handled later.
			sqlException = e;
		}
		
		// Close the IVEN connection since it is no longer needed.
		try { connection.close(); } catch (SQLException e) {}
		
		// The connection should be closed now, so throw the exception that was to be handled later (if there is one).
		if (sqlException != null) { throw sqlException; }
	}
	
	/**
	 * Compare the projects and data set information from the Master Lists with the DTS
	 * information
	 * @throws SQLException if there is a problem accessing or reading the ML database.
	 */
	public void loadMasterList() throws SQLException {
		// Get the connection to the ML database.
		Connection connection = DriverManager.getConnection(ML_URL, DMG_USER, DMG_PASS);
		// Create an object to hold an exception to allow the connection to be properly terminated.
		SQLException sqlException = null;
		
		try {
			/* NOTE:  The success variable is to prevent the import of information past a failure that would
			 * be required by the later import.  (i.e.  A data set can only be loaded if all of the users and
			 * projects have already been loaded correctly.  It also allows the porting to do as much as it
			 * can before ending to allow the user to make multiple corrections.
			 */
			
			// 1.  Load the projects.
			changeStatus("Comparing the Projects from the ML...");
			boolean success = Project.compareMasterListProjects(this, connection, projects);
			// 2.  Load the data sets.
			if (success) {
				changeStatus("Comparing the Datasets from the ML...");
				success = Dataset.compareMasterListDatasets(this, connection, projects, datasets);
			} else {
				changeStatus("Loading of the ML data sets was skipped due to a previous failure.");
			}
		} catch (SQLException e) {
			// Store the exception to be handled later.
			sqlException = e;
		}
		
		// Close the Master List connection since it is no longer needed.
		try { connection.close(); } catch (SQLException e) {}
		
		// The connection should be closed now, so throw the exception that was to be handled later (if there is one).
		if (sqlException != null) { throw sqlException; }
	}
	
	/**
	 * Perform the process of porting the DLN and IVEN databases to the DTS database.
	 * @throws SQLException if there is a problem with any of the connections, commands, results, etc from any of the
	 * DTS/EMDAC databases.
	 */
	public void port() throws SQLException {
		// 1.  Load the information from the DLN.
		loadDataLoadingNotes();
		// 2.  Load the information from IVEN.
		loadIVEN();
		// 3.  Load the information from EMDAC.
		loadEMDAC();
		// 4.  Load the information from the Master Lists.
		loadMasterList();

		// 5.  Everything has been merged correctly, so inquire what database should be loaded.
		if (successStatus) {
			// Get the name of the machine from the User.
			String machineName = frame.machineInquire();
			changeStatus("User entered machine: "+machineName);
			// Store the information to the DTS database.
			storeDTS(machineName);
		} 

		// Output the final results of the port.
		changeStatus(successStatus ? "Success!!!" : "Failure(s)!");
	}
	
	/**
	 * Set the GUI frame used to run this port object.
	 * @param frame The frame running the port.
	 */
	public void setDisplayFrame(PortFrame frame) { this.frame = frame; }

	/**
	 * Store the information that has been loaded into the DTS database on the specified machine.
	 * @param machine The name of the machine containing the the DTS database.
	 * @throws SQLException if there is a problem storing the DTS information to the database.
	 */
	public void storeDTS(String machine) throws SQLException {
		// Get the connection to the DTS database.
		Connection connection = DriverManager.getConnection("jdbc:mysql://"+machine+"/dmg_dts", DMG_USER, DMG_PASS);
		// Turn off auto commit so all of the information is loaded at once.
		connection.setAutoCommit(false);
		// Create an object to hold an exception to allow the connection to be properly terminated.
		SQLException sqlException = null;
		
		// Output an empty line to separate the load from the store.
		changeStatus("");
		
		try {
			/* NOTE:  The success variable is to prevent the import of information past a failure that would
			 * be required by the later import.  (i.e.  A data set can only be loaded if all of the users and
			 * projects have already been loaded correctly.  It also allows the porting to do as much as it
			 * can before ending to allow the user to make multiple corrections.
			 */
			
			// 1.  Insert all of the stuff not in the DLN or IVEN that will be needed to
			// be able to insert the rest of the information.
			changeStatus("Storing the Statuses in the DTS...");
			boolean success = Status.storeDataTrackignSystemStatuses(this, connection);
			changeStatus("Storing the Ingest Types in the DTS...");
			success = IngestType.storeDataTrackingSystemIngestTypes(this, connection) && success;
			changeStatus("Storing the Note Types in the DTS...");
			success = NoteType.storeDataTrackingSystemNoteTypes(this, connection) && success;
			
			// 2.  Store the Users.
			changeStatus("Storing the Users in the DTS...");
			success = User.storeDataTrackingSystemUsers(this, connection, users) && success;
			
			// 3.  Store the projects.
			if (success) {
				changeStatus("Storing the Projects in the DTS...");
				success = Project.storeDataTrackingSystemProjects(this, connection, projects, users, NoteType.getNoteTypeMap());
			} else {
				changeStatus("Storing of the DTS projects was skipped due to a previous failure.");
			}
			
			// 4.  Store the data sets.
			if (success) {
				changeStatus("Storing the Datasets in the DTS...");
				success = Dataset.storeDataTrackingSystemDatasets(this, connection, projects, users, datasets, NoteType.getNoteTypeMap(), Status.getStatusMap());
			} else {
				changeStatus("Storing of the DTS datasets was skipped due to a previous failure.");
			}
			
			// 5.  Store the source data set information.  This must be done following the data sets to
			// ensure that the source data sets are in the database.
			if (success) {
				changeStatus("Storing the Source Dataset information in the DTS.");
				success = DatasetSource.storeDataTrackingSystemSourceDatasets(this, connection, datasets, users);
			} else {
				changeStatus("Storing of the DTS source dataset information was skipped due to a previous failure.");
			}
			
			// 6.  Everything is now inserted into the database without any problems, so it can be 
			// finalized with a commit.
			if (success) {
				changeStatus("Finalizing Transaction to DTS...");
				connection.commit();
			} else {
				changeStatus("Unable to finalize the DTS transaction due to previous failures... Rolling back!");
				// Just try to roll back; don't care if there is a problem.
				try { connection.rollback(); } catch (SQLException e) {}
			}
			
		} catch (SQLException e) {
			// Undo all of the commands already sent. 
			try {
				changeStatus("Rolling back the DTS database!");
				connection.rollback();
			}
			// Don't care about an exception in the roll back, there is a more important issue at hand.
			catch (SQLException ex) {}
			
			// Store the exception to be handled later.
			sqlException = e;
		}
		
		// Close the DTS connection since it is no longer needed.
		try { connection.close(); } catch (SQLException e) {}
		
		// The connection should be closed now, so throw the exception that was to be handled later (if there is one).
		if (sqlException != null) { throw sqlException; }
	}

	/**
	 * Execute the program.
	 * @param args NONE.
	 * @throws Exception if there is a problem finding the driver.
	 */
	public static void main(String[] args) throws Exception {
		(new PortFrame(new ToDmgDts())).setVisible(true);
	}
}
