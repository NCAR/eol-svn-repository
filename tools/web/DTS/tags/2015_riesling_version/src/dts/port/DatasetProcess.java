package dts.port;

import java.util.*;
import java.sql.*;

/**
 * The DatasetProcess class is the container and servicer for a specific instance of a
 * Dataset's processing task.
 * 
 * @author jclawson
 */
public class DatasetProcess {

	private Dataset dataset;
	private Project project;
	private String howToFile, finalData, note, plots, repository, stationFile, status;
	private User processor;
	
	/**
	 * Create a new instance of a DatasetProcess task.
	 * @param dataset The Dataset being assigned to this processing task.
	 * @param project The Project the processing is being done for.
	 */
	public DatasetProcess(Dataset dataset, Project project) {
		this.dataset = dataset;
		this.project = project;
	}
	
	/**
	 * Get the Dataset associated to this process task.
	 * @return The Dataset.
	 */
	public Dataset getDataset() { return dataset; }
	
	/**
	 * Get the entry date for the processing task.  This is the begin date of the project
	 * since the versions of the processing task were done by project.
	 * @return The entry date for the data set.
	 */
	public Timestamp getEntryDate() {
		return new Timestamp(getProject().getBeginDate().getTime());
	}
	
	/**
	 * Get the location of the final processed data.
	 * @return The final processed data location.
	 */
	public String getFinalDataDirectory() {
		return finalData == null ? "" : finalData.trim();
	}
	
	/**
	 * Get the location of the how to file for the processing of the data set.
	 * @return The how to file location.
	 */
	public String getHowToFile() {
		return howToFile == null ? "" : howToFile.trim();
	}
	
	/**
	 * Get the location of the plots generated from this data set.
	 * @return The plots directory for the data set.
	 */
	public String getPlotsDirectory() {
		return plots == null ? "" : plots.trim();
	}
	
	/**
	 * Get the processing notes for this task.
	 * @return The tasks processing notes.
	 */
	public String getProcessingNotes() {
		return note == null ? "" : note.trim();
	}
	
	/**
	 * Get the status of the processing of the data set.
	 * @return The data set's processing status.
	 */
	public String getProcessingStatus() { return status; }
	
	/**
	 * Get the User tasked with processing the data set.
	 * @return The User in charge of processing the data set.
	 */
	public User getProcessor() { return processor; }
	
	/**
	 * Get the Project the processing task is for.
	 * @return The processing task's Project.
	 */
	public Project getProject() { return project; }
	
	/**
	 * Get the location of the repository where the software for the processing can be found.
	 * @return The data set's software repository.
	 */
	public String getRepository() {
		return repository == null ? "" : repository.trim();
	}
	
	/**
	 * Get the location of the station list file for the processed data.
	 * @return The station list file location.
	 */
	public String getStationListFile() {
		return stationFile == null ? "" : stationFile.trim();
	}
	
	/**
	 * Set the how to file of this processing task.
	 * @param file The how to file of the processing.
	 * @throws MergeException if the file does not match a previously set value.
	 */
	public void setHowToFile(String file) throws MergeException {
		// Set the file for the first time.
		if (howToFile == null && file != null && !file.trim().equals("")) { howToFile = file.trim(); }
		// Make sure the directory matches the previously set value.
		else if (howToFile != null && howToFile.compareTo(file) != 0) {
			throw new MergeException("Dataset "+getDataset().getDatasetId()+" has a mismatched how to file:  "+howToFile+" vs "+file);
		}
	}
	
	/**
	 * Set the final data directory of this processing task.
	 * @param directory The final data directory of the processing.
	 * @throws MergeException if the directory does not match a previously set value.
	 */
	public void setFinalDataDirectory(String directory) throws MergeException {
		// Set the directory for the first time.
		if (finalData == null && directory != null && !directory.trim().equals("")) { finalData = directory.trim(); }
		// Make sure the directory matches the previously set value.
		else if (finalData != null && finalData.compareTo(directory) != 0) {
			throw new MergeException("Dataset "+getDataset().getDatasetId()+" has a mismatched final data directory:  "+finalData+" vs "+directory);
		}
	}
	
	/**
	 * Set the plots directory of this processing task.
	 * @param directory The plots directory of the processing.
	 * @throws MergeException if the directory does not match a previously set value.
	 */
	public void setPlotsDirectory(String directory) throws MergeException {
		// Set the directory for the first time.
		if (plots == null && directory != null && !directory.trim().equals("")) { plots = directory.trim(); }
		// Make sure the directory matches the previously set value.
		else if (plots != null && plots.compareTo(directory) != 0) {
			throw new MergeException("Dataset "+getDataset().getDatasetId()+" has a mismatched plots directory:  "+plots+" vs "+directory);
		}
	}
	
	/**
	 * Set the processing notes of this processing task.
	 * @param text The notes for the processing.
	 * @throws MergeException if the text does not match a previously set value.
	 */
	public void setProcessingNotes(String text) throws MergeException {
		// Handle a null note.
		text = text == null ? "" : text.trim();
		// Set the notes for the first time.
		if (note == null && !text.equals("")) { note = text; }
		// Make sure the note matches the previously set value.
		else if (note != null && !text.equals("") && note.compareTo(text.trim()) != 0) {
			throw new MergeException("Dataset "+getDataset().getDatasetId()+" has a mismatched processing note:  '"+note+"' vs '"+text+"'");
		}
	}
	
	/**
	 * Set the status of this processing task.
	 * @param status The status of the processing.
	 * @throws MergeException if the status does not match a previously set value.
	 */
	public void setProcessingStatus(String status) throws MergeException {
		// Set the status for the first time.
		if (this.status == null && status != null && !status.trim().equals("")) { this.status = status.trim(); }
		// Make sure the status matches the previously set value.
		else if (this.status != null && this.status.compareTo(status) != 0) {
			throw new MergeException("Dataset "+getDataset().getDatasetId()+" has a mismatched processing status:  "+this.status+" vs "+status);
		}
	}
	
	/**
	 * Set the processor of this processing task.
	 * @param processor The User tasked with the processing.
	 * @throws MergeException if the processor does not match a previously set value.
	 */
	public void setProcessor(User processor) throws MergeException {
		// Set the processor for the first time.
		if (this.processor == null && processor != null) { this.processor = processor; }
		// Make sure the processor matches the previously set value.
		else if (this.processor != null && this.processor.compareTo(processor) != 0) {
			throw new MergeException("Dataset "+getDataset().getDatasetId()+" has a mismatched processing processor:  "+this.processor+" vs "+processor);
		}
	}
	
	/**
	 * Set the repository directory of this processing task.
	 * @param directory The repository directory of the processing.
	 * @throws MergeException if the repository does not match a previously set value.
	 */
	public void setRepository(String directory) throws MergeException {
		// Set the directory for the first time.
		if (repository == null && directory != null && !directory.trim().equals("")) { repository = directory.trim(); }
		// Make sure the directory matches the previously set value.
		else if (repository != null && repository.compareTo(directory) != 0) {
			throw new MergeException("Dataset "+getDataset().getDatasetId()+" has a mismatched repository:  "+repository+" vs "+directory);
		}
	}

	/**
	 * Set the station list file of this processing task.
	 * @param file The station file of the processing.
	 * @throws MergeException if the file does not match a previously set value.
	 */
	public void setStationFile(String file) throws MergeException {
		// Set the file for the first time.
		if (stationFile == null && file != null && !file.trim().equals("")) { stationFile = file.trim(); }
		// Make sure the directory matches the previously set value.
		else if (stationFile != null && stationFile.compareTo(file) != 0) {
			throw new MergeException("Dataset "+getDataset().getDatasetId()+" has a mismatched station file:  "+stationFile+" vs "+file);
		}
	}
	
	/**
	 * Insert the data set information for this specific processing task into the DTS database.
	 * @param porter The class controlling the porting of the databases.
	 * @param connection The connection to the DTS database.
	 * @param software The mapping of the known software packages in the DTS.
	 * @return <code>true</code> if the insert was successful.
	 * @throws SQLException if there was a problem inserting the entry.
	 */
	public boolean storeDataTrackingSystemInformation(ToDmgDts porter, Connection connection, Map users, Map software) throws SQLException {
		boolean success  = true;
		
		// Define the SQL to insert the process into the database.
		String sql = "INSERT INTO dataset_process(dataset_id, entry_date, process_contact_id, process_status_id, work_location, final_data_location, station_list_location, howto_url, plot_url, row_revise_time) VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";
		// Define the statement that will execute the insert query.
		PreparedStatement stmt = connection.prepareStatement(sql);
		// Assign the values to the statement.
		stmt.setString(1, getDataset().getDatasetId());
		stmt.setTimestamp(2, getEntryDate());
		if (getProcessor() != null) {
			stmt.setInt(3, getProcessor().getDtsId());
		} else {
			stmt.setNull(3, Types.INTEGER);
		}
		Status status = Status.determineProcessingStatus(getProcessingStatus());
		if (status != null) {
			stmt.setInt(4, status.getId());
		} else {
			stmt.setNull(4, Types.INTEGER);
		}
		stmt.setString(5, "");
		stmt.setString(6, getFinalDataDirectory());
		stmt.setString(7, getStationListFile());
		stmt.setString(8, getHowToFile());
		stmt.setString(9, getPlotsDirectory());
		stmt.setTimestamp(10, getEntryDate());
		// Execute the statement.
		stmt.execute();
		
		// Properly close the open statement stream.
		try { stmt.close(); } catch (SQLException e) {}
		
		// Insert the processing note into the database if one exists.
		if (success && !getProcessingNotes().equals("")) {
			// Create a new Note object for the note.
			Note note = new Note(getProcessor() == null ? (User)users.get("local") : getProcessor(), getEntryDate(), getProcessingNotes());
			// Insert the note into the database.
			try { success = note.storeInDataTrackingSystem(connection); }
			catch (SQLException e) {
				success = false;
				porter.appendMergeException(new MergeException("Processing Note for project "+getProject().getProjectId()+" and dataset "+getDataset().getDatasetId()+" could not be inserted.  "+e.getMessage()));
			}
			// Only try to associate the note to the data set if it was inserted successfully.
			if (success) {
				try { success = note.storeInDataTrackingSystem(connection, getDataset(), (NoteType)NoteType.getNoteTypeMap().get(NoteType.PROCESS_NOTE)); }
				catch (SQLException e) {
					success = false;
					porter.appendMergeException(new MergeException("Unable to assign the note to data set "+getDataset().getDatasetId()+"  "+e.getMessage()));
				}
			}
		}
		
		// Insert the repository/software information into the DTS.
		if (!getRepository().equals("")) {
			success = Software.storeDataTrackingSystemSoftware(porter, connection, software, this);
		}
		
		// Return the success status of the data insert.
		return success;
	}
}
