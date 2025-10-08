package dts.port;

import java.sql.*;

/**
 * The DatasetIngest class is the container and servicer for an individual ingest
 * task for a Dataset.
 * 
 * @author jclawson
 */
public class DatasetIngest {

	private Dataset dataset;
	private Timestamp date;
	private String ingest;
	
	/**
	 * Create a new instance of a DatasetIngest object.
	 * @param dataset The Dataset being assigned this ingest task.
	 * @param date The entry date of this task.
	 */
	public DatasetIngest(Dataset dataset, Timestamp date) {
		this.dataset = dataset;
		this.date = date;
	}
	
	/**
	 * Get the Dataset assigned this ingest task.
	 * @return The Dataset.
	 */
	public Dataset getDataset() { return dataset; }
	
	/**
	 * Get the Date the checking task was created.
	 * @return The checking task's entry date.
	 */
	public Timestamp getEntryDate() { return date; }
	
	/**
	 * Get the location where the data was ingested for this task.
	 * @return The Dataset's ingest location for this task.
	 */
	public String getIngestDirectory() {
		return ingest == null ? "" : ingest.trim();
	}
	
	/**
	 * Set the directory where the data was ingested for this task.
	 * @param directory The ingest directory location.
	 * @throws MergeException if the directory does not match a previously set value.
	 */
	public void setIngestDirectory(String directory) throws MergeException {
		// Set the ingest directory for the first time.
		if (ingest == null && directory != null && !directory.trim().equals("")) { ingest = directory.trim(); }
		// Make sure the directory matches a previously set value.
		else if (ingest != null && ingest.compareTo(directory) != 0) {
			throw new MergeException("Dataset "+getDataset().getDatasetId()+" has a mismatched ingest directory:  "+ingest+" vs "+directory);
		}
	}

	/**
	 * Insert the data set information for this specific ingest task into the DTS database.
	 * @param connection The connection to the DTS database.
	 * @return <code>true</code> if the insert was successful.
	 * @throws SQLException if there was a problem inserting the entry.
	 */
	public boolean storeDataTrackingSystemInformation(Connection connection) throws SQLException {
		// Define the SQL to insert the task into the database.
		String sql = "INSERT INTO dataset_ingest(dataset_id, entry_date, ingest_location, row_revise_time) VALUES(?, ?, ?, ?)";
		// Define the statement that will execute the insert query.
		PreparedStatement stmt = connection.prepareStatement(sql);
		// Assign the values to the statement.
		stmt.setString(1, getDataset().getDatasetId());
		stmt.setTimestamp(2, getEntryDate());
		stmt.setString(3, getIngestDirectory());
		stmt.setTimestamp(4, getEntryDate());
		// Execute the statement.
		stmt.execute();
		
		// Properly close the open statement stream.
		try { stmt.close(); } catch (SQLException e) {}
		
		// If there was a problem, an exception would have been thrown.
		return true;
	}
}
