package dts.port;

import java.sql.*;

/**
 * The DatasetProject class is a container for associating Dataset information to a specific
 * Project.
 * 
 * @author jclawson
 */
public class DatasetProject {

	private Boolean mlFlag;
	private Dataset dataset;
	private Project project;
	
	/**
	 * Create a new instance of a DatasetProject.
	 * @param dataset The Dataset being associated to a Project.
	 * @param project The Project being associated to a Dataset.
	 */
	public DatasetProject(Dataset dataset, Project project) {
		this.dataset = dataset;
		this.project = project;
	}
	
	/**
	 * Get the Dataset associated in this DatasetProject.
	 * @return The Dataset.
	 */
	public Dataset getDataset() { return dataset; }

	/**
	 * Get the Project associated in this DatasetProject.
	 * @return The Project.
	 */
	public Project getProject() { return project; }
	
	/**
	 * Determine if the data set is in the Master List for this project.
	 * @return <code>true</code> if the data set is in the ML for the project, <code>false</code>
	 * if it is not.
	 */
	public boolean isInMasterList() {
		return mlFlag == null ? false : mlFlag.booleanValue();
	}
	
	/**
	 * Set the flag that marks this Dataset as being in the Master List for this Project.
	 * @param flag The Master List flag value.
	 * @throws MergeException if the flag does not match a previously set value.
	 */
	public void setMasterListFlag(boolean flag) throws MergeException {
		// Set the flag for the first time.
		if (mlFlag == null) { mlFlag = new Boolean(flag); }
		// Make sure the flag matches the previously set value.
		else if (flag != mlFlag.booleanValue()) {
			throw new MergeException("Dataset "+getDataset().getDatasetId()+" has a mismatched master list flag for project "+getProject().getProjectId());
		}
	}
	
	/**
	 * Insert the data set information for this specific project into the DTS database.
	 * @param connection The connection to the DTS database.
	 * @return <code>true</code> if the insert was successful.
	 * @throws SQLException if there was a problem inserting the entry.
	 */
	public boolean storeDataTrackingSystemInformation(Connection connection) throws SQLException {
		// Define the SQL to insert the base data set into the database.
		String sql = "INSERT INTO dataset_project(dataset_id, project_id, master_list_flag) VALUES(?, ?, ?)";
		// Define the statement that will execute the insert query.
		PreparedStatement stmt = connection.prepareStatement(sql);
		// Assign the values to the statement.
		stmt.setString(1, getDataset().getDatasetId());
		stmt.setString(2, getProject().getProjectId());
		stmt.setBoolean(3, isInMasterList());
		// Execute the statement.
		stmt.execute();
		
		// Properly close the open statement stream.
		try { stmt.close(); } catch (SQLException e) {}
		
		// If there was a problem, an exception would have been thrown.
		return true;
	}
}
