package dts.port;

import java.sql.*;

/**
 * The DatasetLoad class is the container and servicer for a specific instance of a
 * Dataset's load task.
 * 
 * @author jclawson
 */
public class DatasetLoad {

	private Boolean loaded, readme;
	private Dataset dataset;
	private Timestamp date;
	private String archive, dataToArchive;
	private User loader;
	
	/**
	 * Create a new instance of a DatasetLoad task.
	 * @param dataset The Dataset being assigned to this load task.
	 * @param date The entry date of this load task.
	 */
	public DatasetLoad(Dataset dataset, Timestamp date) {
		this.dataset = dataset;
		this.date = date;
	}
	
	/**
	 * Get the location of where the data is archived.
	 * @return The Dataset's archive location.
	 */
	public String getArchiveDirectory() {
		return archive == null ? "" : archive.trim();
	}
	
	/**
	 * Get the location of the data to be archived.
	 * @return The data to be archive location.
	 */
	public String getDataToArchiveDirectory() {
		return dataToArchive == null ? "" : dataToArchive.trim();
	}
	
	/**
	 * Get the Dataset associated to this load task.
	 * @return The Dataset.
	 */
	public Dataset getDataset() { return dataset; }
	
	/**
	 * Get the Date the loading task was created.
	 * @return The loading task's entry date.
	 */
	public Timestamp getEntryDate() { return date; }
	
	/**
	 * Get the User tasked with loading this version of the data set.
	 * @return The loader of the data set.
	 */
	public User getLoader() { return loader; }
	
	/**
	 * Determine if this version of the loading has at least one documentation file to be loaded with it.
	 * @return <code>true</code> if there is a document to be loaded, <code>false</code> if there is not.
	 */
	public boolean hasDocumentation() {
		return readme == null ? false : readme.booleanValue();
	}
	
	/**
	 * Determine if this loaded task has been completed.
	 * @return <code>true</code> if the data set has been loaded, <code>false</code> if it hasn't.
	 */
	public boolean isLoaded() {
		return loaded == null ? false : loaded.booleanValue();
	}
	
	/**
	 * Set the directory where the data archived for this load task.
	 * @param directory The archive directory location.
	 * @throws MergeException if the directory does not match a previously set value.
	 */
	public void setArchiveDirectory(String directory) throws MergeException {
		// Set the directory for the first time.
		if (archive == null && directory != null && !directory.trim().equals("")) { archive = directory.trim(); }
		// Make sure the directory matches the previously set value.
		else if (archive != null && archive.compareTo(directory) != 0) {
			throw new MergeException("Dataset "+getDataset().getDatasetId()+" has a mismatched archive directory:  "+archive+" vs "+directory);
		}
	}
	
	/**
	 * Set the directory where the data to be archived can be found for this load task.
	 * @param directory The data to be archived directory location.
	 * @throws MergeException if the directory does not match a previously set value.
	 */
	public void setDataToArchiveDirectory(String directory) throws MergeException {
		// Set the directory for the first time.
		if (dataToArchive == null && directory != null && !directory.trim().equals("")) { dataToArchive = directory.trim(); }
		// Make sure the directory matches the previously set value.
		else if (dataToArchive != null && dataToArchive.compareTo(directory) != 0) {
			throw new MergeException("Dataset "+getDataset().getDatasetId()+" has a mismatched data to archive directory:  "+dataToArchive+" vs "+directory);
		}
	}
	
	/**
	 * Set the flag that marks this load task's status.
	 * @param flag The status of this load task.
	 * @throws MergeException if the flag does not match a previously set value.
	 */
	public void setLoaded(boolean flag) throws MergeException {
		// Set the flag for the first time.
		if (loaded == null) { loaded = new Boolean(flag); }
		// Make sure the flag matches a previously set value.
		else if (flag != loaded.booleanValue()) {
			throw new MergeException("Dataset "+getDataset().getDatasetId()+" has a mismatched loaded flag.");
		}

	}
	
	/**
	 * Set the User tasked with loading this Dataset.
	 * @param loader The User tasked to load this Dataset.
	 * @throws MergeException if the loader does not match a previously set value.
	 */
	public void setLoader(User loader) throws MergeException {
		// Set the loader for the first time.
		if (this.loader == null) { this.loader = loader; }
		// Make sure the loader matches a previously set value.
		else if (this.loader.compareTo(loader) != 0) {
			throw new MergeException("Dataset "+getDataset().getDatasetId()+" has a mismatched loader: "+this.loader.getUserId()+" vs "+loader.getUserId());
		}
	}
	
	/**
	 * Set the flag that marks the data as having a documentation file.
	 * @param flag The documentation flag value.
	 * @throws MergeException if the flag does not match a previously set value.
	 */
	public void setReadmeFlag(boolean flag) throws MergeException {
		// Set the flag for the first time.
		if (readme == null) { readme = new Boolean(flag); }
		// Make sure the flag matches a previously set value.
		else if (flag != readme.booleanValue()) {
			throw new MergeException("Dataset "+getDataset().getDatasetId()+" has a mismatched readme flag.");
		}
	}
	
	/**
	 * Insert the data set information for this specific loading task into the DTS database.
	 * @param connection The connection to the DTS database.
	 * @return <code>true</code> if the insert was successful.
	 * @throws SQLException if there was a problem inserting the entry.
	 */
	public boolean storeDataTrackingSystemInformation(Connection connection) throws SQLException {
		// Define the SQL to insert the task into the database.
		String sql = "INSERT INTO dataset_load(dataset_id, entry_date, load_contact_id, load_status_id, load_data_location, archive_location, documentation_flag, row_revise_time) VALUES(?, ?, ?, ?, ?, ?, ?, ?)";
		// Define the statement that will execute the insert query.
		PreparedStatement stmt = connection.prepareStatement(sql);
		// Assign the values to the statement.
		stmt.setString(1, getDataset().getDatasetId());
		stmt.setTimestamp(2, getEntryDate());
		if (getLoader() != null) {
			stmt.setInt(3, getLoader().getDtsId());
		} else {
			stmt.setNull(3, Types.INTEGER);
		}
		Status status = Status.determineLoadingStatus(isLoaded());
		if (status != null) {
			stmt.setInt(4, status.getId());
		} else {
			stmt.setNull(4, Types.INTEGER);
		}
		stmt.setString(5, getDataToArchiveDirectory());
		stmt.setString(6, getArchiveDirectory());
		stmt.setBoolean(7, hasDocumentation());
		stmt.setTimestamp(8, getEntryDate());
		// Execute the statement.
		stmt.execute();
		
		// Properly close the open statement stream.
		try { stmt.close(); } catch (SQLException e) {}
		
		// If there was a problem, an exception would have been thrown.
		return true;
	}
}
