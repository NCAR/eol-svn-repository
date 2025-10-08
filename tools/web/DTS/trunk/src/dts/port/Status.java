package dts.port;

import java.sql.*;
import java.util.*;

/**
 * The Status class is a container for a Status of a task for a Dataset.  This includes
 * To Be Done, In Progress, Done, N/A, etc.
 * 
 * @author jclawson
 */
public class Status {
	
	/** Constant for the Status:  Do Not Do */
	public static final String DND = "Do Not Do";
	/** Constant for the Status:  Done */
	public static final String DONE = "Done";
	/** Constant for the Status:  In Progress */
	public static final String IN_PROGRESS = "In Progress";
	/** Constant for the Status:  Not Available */
	public static final String NA = "N/A";
	/** Constant for the Status:  On Going */
	public static final String ON_GOING = "On Going";
	/** Constant for the Status:  To Be Done */
	public static final String TBD = "To Be Done";
	
	private static TreeMap statusMap;
	
	private boolean done;
	private int id, sortIndex;
	private String name, style;
	
	/**
	 * Create a new instance of a Status.
	 * @param name The name of the Status.
	 * @param style The CSS style for the Status.
	 * @param sortIndex The sorting index of the Status.
	 * @param done The flag that marks the status as being completed
	 */
	private Status(String name, String style, int sortIndex, boolean done) {
		this.name = name;
		this.style = style;
		this.sortIndex = sortIndex;
		this.done = done;
	}
	
	/**
	 * Create the mapping of the Statuses by their name.
	 */
	private static void buildStatusMap() {
		statusMap = new TreeMap();
		
		statusMap.put(TBD, new Status(TBD, "color: #F00; font-weight: bold;", 1, false));
		statusMap.put(IN_PROGRESS, new Status(IN_PROGRESS, "color: #FC0; font-weight: bold;", 2, false));
		statusMap.put(DONE, new Status(DONE, "color: #0C0; font-weight: bold;", 3, true));
		statusMap.put(ON_GOING, new Status(ON_GOING, "color: #00F; font-weight: bold;", 4, true));
		statusMap.put(DND, new Status(DND, "color: #000; font-weight: bold;", 5, true));
		statusMap.put(NA, new Status(NA, "color: #999; font-weight: bold;", 6, true));
	}

	/**
	 * Convert a boolean checked status to one of the known Statuses from the status map.
	 * @param checked The flag marking a checked task as being completed or not.
	 * @return The corresponding status for the flag.
	 */
	public static Status determineCheckingStatus(boolean checked) {
		return (Status)(checked ? statusMap.get(DONE) : statusMap.get(TBD));
	}
	
	/**
	 * Convert a boolean loaded status to one of the known Statuses from the status map.
	 * @param loaded The flag marking a loading task as being completed or not.
	 * @return The corresponding status for the flag.
	 */
	public static Status determineLoadingStatus(boolean loaded) {
		return (Status)(loaded ? statusMap.get(DONE) : statusMap.get(TBD));
	}
	
	/**
	 * Convert a String processed status to one of the known Statuses from the status map.
	 * @param status The processing status name.
	 * @return The corresponding status for the name or <code>null</code> if there is not a Status for the name.
	 */
	public static Status determineProcessingStatus(String status) {
		if (status.equalsIgnoreCase("tbd")) { return (Status)statusMap.get(TBD); }
		else if (status.equalsIgnoreCase("in_progress")) { return (Status)statusMap.get(IN_PROGRESS); }
		else if (status.equalsIgnoreCase("done")) { return (Status)statusMap.get(DONE); }
		else if (status.equalsIgnoreCase("dnd")) { return (Status)statusMap.get(DND); }
		else if (status.equalsIgnoreCase("na")) { return (Status)statusMap.get(NA); }
		else { return null; }
	}
	
	/**
	 * Set the DTS ID number for the Status.
	 * @return The Status' DTS ID number.
	 */
	public int getId() { return id; }
	
	/**
	 * Get the name of the Status.
	 * @return The Status' name.
	 */
	public String getName() { return name; }
	
	/**
	 * Get the sort index of the Status.
	 * @return The Status' sort index.
	 */
	public int getSortIndex() { return sortIndex; }
	
	/**
	 * Get the mapping of all known Statuses by their names.
	 * @return The mapping of Statuses by name.
	 */
	public static TreeMap getStatusMap() {
		if (statusMap == null) { buildStatusMap(); }
		return statusMap;
	}
	
	/**
	 * Get the CSS style for the Status.
	 * @return The Status' CSS style.
	 */
	public String getStyle() { return style; }
	
	/**
	 * Determine if this status is resolved and requires no further work.
	 * @return <code>true</code> if the status is finished, <code>false</code> if it is not.
	 */
	public boolean isResolved() { return done; }
	
	/**
	 * Set the DTS ID number for the Status.
	 * @param id The Status' DTS ID number.
	 */
	private void setId(int id) { this.id = id; }
	
	/**
	 * Insert all of the known Statues into the DTS database.
	 * @param porter The controller object for the porting process.
	 * @param connection The connection to the DTS database.
	 * @return <code>true</code> if all of the Statuses were inserted successfully, <code>false</code>
	 * if there is any error during the insert.
	 * @throws SQLException if there is a problem executing the statements on the database.
	 */
	public static boolean storeDataTrackignSystemStatuses(ToDmgDts porter, Connection connection) throws SQLException {
		// Define the success flag used to return the success of storing the status information into the database.
		boolean success = true;
		// Define the SQL to store the status information into the database.
		String sql = "INSERT INTO status(name, css_style, sort_index, is_resolved) VALUES(?, ?, ?, ?)";
		// Define the statement to execute on the database.
		PreparedStatement stmt = connection.prepareStatement(sql);
		
		// Define the iterator over the known status.
		Iterator itr = getStatusMap().values().iterator();
		// Loop through each status in the iterator.
		while (itr.hasNext()) {
			// Pull out the current status from the iterator.
			Status status = (Status)itr.next();

		
			boolean localSuccess = true;
			
			// Insert the status into the database.
			try {
				// Assign the current status' information to the statement.
				stmt.setString(1, status.getName());
				stmt.setString(2, status.getStyle());
				stmt.setInt(3, status.getSortIndex());
				stmt.setBoolean(4, status.isResolved());
				// Execute the statement on the database.
				stmt.execute();
			}
			// The statement didn't execute, so it needs to fail properly.
			catch (SQLException e) {
				localSuccess = false;
				success = false;
				porter.appendMergeException(new MergeException("Status "+status.getName()+" could not be inserted into the DTS database:  "+e.getMessage()));
			}

			// Obtain the auto-generated DTS ID keys after a successful insert.
			if (localSuccess) {
				// Define an exception holder for a later display.
				SQLException sqlEx = null;
				try {
					// Get the auto-generated key.
					ResultSet keys = stmt.getGeneratedKeys();
					// Assign the DTS ID if the key was returned.
					if (keys.next()) { status.setId(keys.getInt(1)); }
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
					porter.appendMergeException(new MergeException("Status "+status.getName()+" was unable to obtain an auto-generated DTS ID."+(sqlEx == null ? "" : "  "+sqlEx.getMessage())));
				}
			}
		}

		// Properly close the open statement stream.
		try { stmt.close(); } catch (SQLException e) {};

		// Return the status of the execution of the User inserts.
		return success;
	}
}
