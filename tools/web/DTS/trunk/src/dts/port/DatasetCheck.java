package dts.port;

import java.sql.*;

/**
 * The DatasetCheck class is a container and servicer for a specific instance of a checking
 * task.  This includes the status and contact for the task.
 * 
 * @author jclawson
 */
public class DatasetCheck {

	private Boolean checked;
	private Dataset dataset;
	private Timestamp date;
	private User checker;
	
	/**
	 * Create a new instance of a DatasetCheck object.
	 * @param dataset The Dataset assigned this checking task.
	 * @param date The entry date of the task.
	 */
	public DatasetCheck(Dataset dataset, Timestamp date) {
		this.dataset = dataset;
		this.date = date;
	}
	
	/**
	 * Get the User tasked with checking this version of the data set.
	 * @return The User tasked with checking the data set.
	 */
	public User getChecker() { return checker; }
	
	/**
	 * Get the Dataset assigned to this checking task.
	 * @return The Dataset.
	 */
	public Dataset getDataset() { return dataset; }
	
	/**
	 * Get the Date the checking task was created.
	 * @return The checking task's entry date.
	 */
	public Timestamp getEntryDate() { return date; }
	
	/**
	 * Determine if this task has been checked.
	 * @return <code>true</code> if the checking has been completed, <code>false</code> if it has not been done.
	 */
	public boolean isChecked() {
		return checked == null ? false : checked.booleanValue();
	}
	
	/**
	 * Set the flag that marks the checking task as being completed.
	 * @param flag The checking task's status flag.
	 * @throws MergeException if the flag does not match a previously set value.
	 */
	public void setChecked(boolean flag) throws MergeException {
		// Set the flag for the first time.
		if (checked == null) { checked = new Boolean(flag); }
		// Make sure the flag matches a previously set value.
		else if (flag != checked.booleanValue()) {
			throw new MergeException("Dataset "+getDataset().getDatasetId()+" has a mismatched checked flag.");
		}
	}
	
	/**
	 * Set the User tasked with checking this Dataset.
	 * @param checker The checker of the Dataset.
	 * @throws MergeException if the checker does not match a previouly set value.
	 */
	public void setChecker(User checker) throws MergeException {
		// Set the checker for the first time.
		if (this.checker == null) { this.checker = checker; }
		// Make sure the checker matches a previously set value.
		else if (this.checker.compareTo(checker) != 0) {
			throw new MergeException("Dataset "+getDataset().getDatasetId()+" has a mismatched checker: "+this.checker.getUserId()+" vs "+checker.getUserId());
		}
	}

	/**
	 * Insert the data set information for this specific checking task into the DTS database.
	 * @param connection The connection to the DTS database.
	 * @return <code>true</code> if the insert was successful.
	 * @throws SQLException if there was a problem inserting the entry.
	 */
	public boolean storeDataTrackingSystemInformation(Connection connection) throws SQLException {
		// Define the SQL to insert the task into the database.
		String sql = "INSERT INTO dataset_approve(dataset_id, entry_date, approve_contact_id, approve_status_id, row_revise_time) VALUES(?, ?, ?, ?, ?)";
		// Define the statement that will execute the insert query.
		PreparedStatement stmt = connection.prepareStatement(sql);
		// Assign the values to the statement.
		stmt.setString(1, getDataset().getDatasetId());
		stmt.setTimestamp(2, getEntryDate());
		if (getChecker() != null) {
			stmt.setInt(3, getChecker().getDtsId());
		} else {
			stmt.setNull(3, Types.INTEGER);
		}
		Status status = Status.determineCheckingStatus(isChecked());
		if (status != null) {
			stmt.setInt(4, status.getId());
		} else {
			stmt.setNull(4, Types.INTEGER);
		}
		stmt.setTimestamp(5, getEntryDate());
		// Execute the statement.
		stmt.execute();
		
		// Properly close the open statement stream.
		try { stmt.close(); } catch (SQLException e) {}
		
		// If there was a problem, an exception would have been thrown.
		return true;
	}
}
