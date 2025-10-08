package dln.beans;

import dln.display.*;

import java.sql.*;
import java.util.*;

/**
 * The DatasetBean is the representation of a data set in the DTS.  The actual content
 * of the bean is dependent on the current page being displayed in the DTS.  For example,
 * the ingest, load, and check tasks are only available when viewing or editing the
 * specific data set and the processed information is only available through the IVEN
 * parts of the tools. 
 * 
 * @author jclawson
 */
public class DatasetBean extends DefaultBean implements Comparable<DatasetBean> {
	
	private static final long serialVersionUID = -7027315789557748781L;
	
	private String archiveLocation, datasetId, finalDataLocation, howtoURL, name, ingestLocation, loadDataLocation, originalId, readmeURL, remoteURL, plotURL, selectedProductId, stationListFile, utcOffset, workLocation;
	private int checker, extContact, ingester, intContact, loader, processer;
	private int checkStatus, ingestStatus, loadStatus, processStatus;
	private NoteBean collectionTimeNote, sourceInfoNote;
	private List<String> deletedProjects, masterListFlags, projectList;
	private boolean docFlag, questions, isProcessed, isSource;
	private Boolean dstFlag;
	private Timestamp entryDate;
	private Set<String> excludedSources;
	private int ingestType;
	private int listIndex;
	private DatasetBean next, previous;
	
	private List<NoteBean> notes, processNotes;
	
	private Set<Integer> platforms;
	private Map<String, DatasetBean> productMap;
	private TreeSet<SoftwareBean> softwareSet;
	private Map<String, String> sourceDataDirectory;
	private List<DatasetBean> sourceDatasets;
	
	/**
	 * Create a new instance of a DatasetBean.
	 */
	public DatasetBean() {
		projectList = new ArrayList<String>();
		masterListFlags = new ArrayList<String>();
		deletedProjects = new ArrayList<String>();
		notes = new ArrayList<NoteBean>();
		processNotes = new ArrayList<NoteBean>();
		sourceDatasets = new ArrayList<DatasetBean>();
		excludedSources = new TreeSet<String>();
		sourceDataDirectory = new TreeMap<String, String>();
		productMap = new TreeMap<String, DatasetBean>();
		softwareSet = new TreeSet<SoftwareBean>();
		platforms = new TreeSet<Integer>();
		
		isSource = false;
	}
		
	/**
	 * Create a new instance of a DatasetBean.
	 * @param datasetId The data set ID.
	 */
	public DatasetBean(String datasetId) {
		this();
		setDatasetId(datasetId);
		setOriginalDatasetId(datasetId);
	}
	
	/**
	 * Add a new platform to the data set.  If the platform is already attached to the data set,
	 * it will just be ignored.
	 * @param platformId The EMDAC id for the platform.
	 */
	public void addPlatform(Integer platformId) {
		platforms.add(platformId);
	}

	/**
	 * Add a product that this data set helps create.
	 * @param dataset The product data set which uses this data set to be created. 
	 */
	public void addProduct(DatasetBean dataset) {
		if (!productMap.containsKey(dataset.getDatasetId()) && !dataset.getDatasetId().equals(getDatasetId())) {
			productMap.put(dataset.getDatasetId(), dataset);
		}
	}
	
	/**
	 * Associate a project with the data set.
	 * @param projectId The project ID to associate to the data set.
	 */
	public void addProject(String projectId) {
		if (!projectList.contains(projectId)) { projectList.add(projectId); }
	}
	
	/**
	 * Add a software package which was used to help generate this data set.
	 * @param software The software package used on this data set.
	 */
	public void addSoftware(SoftwareBean software) {
		if (!softwareSet.contains(software)) { softwareSet.add(software); }
	}
	
	/**
	 * Add a data set as a source for this data set.
	 * @param dataset The source data set for this data set.
	 */
	public void addSourceDataset(DatasetBean dataset) {
		if (!sourceDatasets.contains(dataset)) { sourceDatasets.add(dataset); }
		dataset.setIsSource(true);
	}
	
	/**
	 * Assign the specified data set as a source for this data set.  This updates the database.
	 * @param datasetId The DTS/EMDAC ID of the source data set.
	 * @throws SQLException if there is a problem associating the source data set to this data set.
	 */
	public void assignSourceDataset(String datasetId) throws SQLException {
		Connection connection = getConnection();
		
		try {
			// Turn on transaction processing.
			connection.setAutoCommit(false);
			
			// Create a statement to see if the source already defines source information in the database.
			PreparedStatement findStmt = connection.prepareStatement("SELECT COUNT(*) FROM dataset_source_info WHERE dataset_id=?");
			findStmt.setString(1, datasetId);
			ResultSet findResults = findStmt.executeQuery();
			
			// Only add source information for the source data set if it doesn't already exist.
			if (findResults.next() && findResults.getInt(1) == 0) {
				int authorId = 0, collectionTimeNoteId = 0, sourceInfoNoteId = 0;
				
				// Find the "local" contact in the database to be the default author of the new notes.
				PreparedStatement authorStmt = connection.prepareStatement("SELECT contact_id FROM contact WHERE contact_short_name=?");
				authorStmt.setString(1, "local");
				ResultSet authorResults = authorStmt.executeQuery();
				if (authorResults.next()) {
					authorId = authorResults.getInt(1);
				}
				try { authorResults.close(); } catch (SQLException e) {}
				try { authorStmt.close(); } catch (SQLException e) {}
				
				// Define the statement to insert the source notes into the database.
				PreparedStatement noteStmt = connection.prepareStatement("INSERT INTO note(entry_date, author_id, note_text) VALUES(NOW(), ?, '<p>Placeholder for a future note.</p>')", Statement.RETURN_GENERATED_KEYS);
				noteStmt.setInt(1, authorId);
				noteStmt.execute();
				// Obtain the note ID key generated by the database for the collection time note.
				ResultSet noteId = noteStmt.getGeneratedKeys();
				if (noteId.next()) { collectionTimeNoteId = noteId.getInt(1); }
				try { noteId.close(); } catch (SQLException e) {}
				noteStmt.execute();
				// Obtain the note ID key generated by the database for the source info note.
				noteId = noteStmt.getGeneratedKeys();
				if (noteId.next()) { sourceInfoNoteId = noteId.getInt(1); }
				try { noteId.close(); } catch (SQLException e) {}
				try { noteStmt.close(); } catch (SQLException e) {}
				
				// Insert the source information for the source data set into the database.
				PreparedStatement srcStmt = connection.prepareStatement("INSERT INTO dataset_source_info(dataset_id, utc_offset, dst_flag, collection_time_note_id, source_info_note_id) VALUES(?, ?, ?, ?, ?)");
				srcStmt.setString(1, datasetId);
				srcStmt.setString(2, "");
				srcStmt.setNull(3, Types.BOOLEAN);
				srcStmt.setInt(4, collectionTimeNoteId);
				srcStmt.setInt(5, sourceInfoNoteId);
				srcStmt.execute();
				try { srcStmt.close(); } catch (SQLException e) {}
			}
			// Close the open database streams.
			try { findResults.close(); } catch (SQLException e) {}
			try { findStmt.close(); } catch (SQLException e) {}

			// Assign the source data set to this data set in the database.
			String sql = "INSERT INTO dataset_source_dataset(dataset_id, source_dataset_id, data_directory, exclude_flag) VALUES(?, ?, ?, ?)";
			PreparedStatement stmt = connection.prepareStatement(sql);
			stmt.setString(1, getDatasetId());
			stmt.setString(2, datasetId);
			stmt.setString(3, "");
			stmt.setBoolean(4, false);
			stmt.execute();
			try { stmt.close(); } catch (SQLException e) {}
			
			// Update this data set's platform list in the database now that a new source has been added to it.
			updatePlatforms(connection);
			
			// Save the commands sent to the database.
			try { connection.commit(); }
			catch (SQLException e) {
				try { connection.rollback(); }
				catch (SQLException e1) {}
				throw e;
			}
		} finally {
			// Properly close the open database connection, no matter what happens.
			try { connection.close(); } catch (SQLException e) {}
		}
	}
	
	/**
	 * Compare this DatasetBean to the specified DatasetBean for sort order.  This uses
	 * the natural sort order of the data set ID for each data set.
	 * @param dataset The DatasetBean to compare to <code>this</code> DatasetBean.
	 * @return A negative integer, zero, or positive integer if <code>this</code> DatasetBean
	 * is less than, equal to, or greater than the specified DatasetBean.
	 */
	public int compareTo(DatasetBean dataset) {
		return getDatasetId().compareTo(dataset.getDatasetId());
	}
	
	/**
	 * Create a new version of the process task for this data set.
	 * @throws SQLException if there is a problem creating the new process task in the database.
	 */
	public void createNewProcessTask() throws SQLException {
		Connection connection = getConnection();
		try {
			// Define the statement for the new version using the current timestamp.
			String sql = "INSERT INTO dataset_process(dataset_id, entry_date) VALUES(?, NOW())";
			PreparedStatement stmt = connection.prepareStatement(sql);
			stmt.setString(1, getDatasetId());
			stmt.execute();
			try { stmt.close(); } catch (SQLException e) {}
		} finally {
			// Properly close the open database connection, no matter what happens.
			try { connection.close(); } catch (SQLException e) {}
		}
	}
	
	/**
	 * Delete this data set and all associations from the database.
	 * @throws SQLException if there is a problem executing the delete.
	 */
	public void delete() throws SQLException {
		// Define the connection to the database.
		Connection connection = getConnection();

		try {
			connection.setAutoCommit(false);
			
			// Define the SQL to delete the data set from the database.
			String sql = "DELETE FROM dataset WHERE dataset_id=?";
			PreparedStatement datasetStmt = connection.prepareStatement(sql);
			// Want to use the original ID in case the user changed the ID in the edit form.
			datasetStmt.setString(1, getOriginalDatasetId());
			// Delete the data set and all associations.  The database has a on delete cascade
			// that will remove all necessary associations as part of this delete.
			datasetStmt.execute();
			// Close the open statement stream.
			try { datasetStmt.close(); } catch (SQLException e) {}
			
			// Delete any alienated notes caused by this delete.
			NoteBean.deleteUnattachedNotes(connection);
			
			// Save the commands sent to the database.
			try { connection.commit(); }
			catch (SQLException e) {
				try { connection.rollback(); }
				catch (SQLException e1) {}
				throw e;
			}
		} finally {
			// Properly close the open database connection, no matter what happens.
			try { connection.close(); } catch (SQLException e) {}
		}
	}
	
	/**
	 * Delete the processed part of the data set.  This should only be done when all of the following
	 * are true.  1.  There are no source data sets associated to this data set.  2.  There are not any
	 * software packages assigned to the data set.  3.  All of the processing information notes for the
	 * current version are empty/null.  4.  There is only one version of the process task for this data
	 * set.
	 * @throws SQLException if there is a problem deleting the process task for the data set.
	 */
	public void deleteProcessed() throws SQLException {
		// Define the connection to the database.
		Connection connection = getConnection();

		try {
			// Turn on transaction processing.
			connection.setAutoCommit(false);
			
			// Define the SQL to delete the data set from the database.
			String sql = "DELETE FROM dataset_process WHERE dataset_id=?";
			PreparedStatement datasetStmt = connection.prepareStatement(sql);
			// Want to use the original ID in case the user changed the ID in the edit form.
			datasetStmt.setString(1, getDatasetId());
			// Delete the data set and all associations.  The database has a on delete cascade
			// that will remove all necessary associations as part of this delete.
			datasetStmt.execute();
			// Close the open statement stream.
			try { datasetStmt.close(); } catch (SQLException e) {}
			
			// Delete any alienated notes caused by this delete.
			NoteBean.deleteUnattachedNotes(connection);
			
			// Save the commands sent to the database.
			try { connection.commit(); }
			catch (SQLException e) {
				try { connection.rollback(); }
				catch (SQLException e1) {}
				throw e;
			}
		} finally {
			// Properly close the open database connection, no matter what happens.
			try { connection.close(); } catch (SQLException e) {}
		}
	}
	
	/**
	 * Update the data set ID if it is not defined with the next available DTS number.
	 * @param connection A connection to the DTS database.
	 * @throws SQLException if there is a problem finding the next available DTS number.
	 */
	private void determineDatasetId(Connection connection) throws SQLException {
		if (getDatasetId() == null || getDatasetId().equals("")) {
			PreparedStatement stmt = connection.prepareStatement("SELECT MAX(number) FROM (SELECT (SUBSTRING(dataset_id,5))+1 AS number FROM dataset WHERE dataset_id LIKE 'DTS.%') as dts_ids");
			ResultSet results = stmt.executeQuery();
			if (results.next()) { setDatasetId("DTS." + results.getInt(1)); }
			try { results.close(); } catch (SQLException e) {}
			try { stmt.close(); } catch (SQLException e) {}
		}
	}
	
	/**
	 * Search for the data set in the list.
	 * @param list The list to find the data set in.
	 * @param datasetId The data set ID for the data set.
	 * @param date The version date of the data set to be found.
	 * @return The index of the data set in the list or -1 if it is not found.
	 */
	private static int findDatasetInList(List<DatasetBean> list, String datasetId, String date) {
		for (int index = 0; index < list.size(); index++) {
			if (list.get(index).getDatasetId().equals(datasetId) && list.get(index).getEntryDate().toString().equals(date)) {
				return index;
			}
		}
		return -1;
	}

	/**
	 * Find the most recent timestamp of the three tasks' timestamps.
	 * @param ingest The ingest task's timestamp.
	 * @param load The load task's timestamp.
	 * @param check The check task's timestamp.
	 * @return The most recent timestamp of the three tasks.
	 */
	private Timestamp findMaxTimestamp(Timestamp ingest, Timestamp load, Timestamp check) {
		if (ingest.compareTo(load) < 0) {
			return ingest.compareTo(check) < 0 ? ingest : check;
		} else {
			return load.compareTo(check) < 0 ? load : check;
		}
	}
	
	/**
	 * Get the directory where the data is archived for the data set.
	 * @return The data set's archive location.
	 */
	public String getArchiveLocation() { return archiveLocation == null ? "" : archiveLocation.trim(); }
	
	/**
	 * Get the database ID of the contact tasked with checking this data set.
	 * @return The checking contact's database ID.
	 */
	public int getChecker() { return checker; }
	
	/**
	 * Get the contact tasked with checking this data set.
	 * @param users The mapping of possible checking contacts.
	 * @return The checking contact or <code>null</code> if a check contact is not assigned.
	 */
	public UserBean getChecker(Map<Integer, UserBean> users) { return users.get(getChecker()); }
	
	/**
	 * Get the database ID of the status of the check task for the data set.
	 * @return The data set's check status ID.
	 */
	public int getCheckStatus() { return checkStatus; }
	
	/**
	 * Get the status of the checking task for this data set.
	 * @param statuses The mapping of possible checking statuses.
	 * @return The checking status or <code>null</code> if the status is not assigned.
	 */
	public StatusBean getCheckStatus(Map<Integer, StatusBean> statuses) {
		return statuses.get(getCheckStatus());
	}
	
	/**
	 * Get the note that contains the collection time information for this source data set.
	 * @return The collection time note for this source data set.
	 */
	public NoteBean getCollectionTimeNote() { return collectionTimeNote; }

	/**
	 * Get the number of processed source data sets that are completed that are associated to this product data set.
	 * This is a recursive count, counting all source data sets down the tree from this product.
	 * @param statuses The mapping of statuses used to determine if a source is completed.
	 * @return The number of completed processed data sets.
	 */
	public int getCompletedProgressBarCount(Map<Integer, StatusBean> statuses) {
		Set<String> resolvedDatasets = new TreeSet<String>();
		Set<String> seenDatasets = new TreeSet<String>();

		getCompletedProgressBarCount(resolvedDatasets, this, statuses, seenDatasets);

		resolvedDatasets.remove(getDatasetId());

		return resolvedDatasets.size();
	}

	/**
	 * This is the main part of the recursion for the public function with the same name.  This makes
	 * sure that data sets only get counted once, even if they are sources to multiple data sets in the
	 * recursion tree.  It also does not include any excluded data sets, sources of excluded data sets, or
	 * source data sets that are not processed.
	 * @param resolved The set of data set IDs for resolved datasets (to produce the final count).
	 * @param root The current root of the tree being recursed.
	 * @param statuses The mapping of statuses used to determine if a source is completed.
	 * @param known The set of known data set IDs (to prevent multiple passes down the same branch of the
	 * product source tree).
	 */
	private void getCompletedProgressBarCount(Set<String> resolved, DatasetBean root, Map<Integer, StatusBean> statuses, Set<String> known) {
		for (DatasetBean source: root.getSourceDatasets()) {
			// Only care about source data sets that have not yet been seen are included in the processing of the product.
			if ( !known.contains(source.getDatasetId()) &&
                 !root.isExcluded(source.getDatasetId())
               ) {
				known.add(source.getDatasetId());

				    // Count the source if it has a resolved/completed status
				if (source.getProcessStatus(statuses) != null && source.getProcessStatus(statuses).isResolved()) {
					resolved.add(source.getDatasetId());
				}

				    // Recursively continue down the tree if the current source has source data sets.
				if (source.getSourceDatasets().size() > 0) {
					getCompletedProgressBarCount(resolved, source, statuses, known);
				}
			}
		}
	}

	/**
	 * This is the main part of the recursion for the public function with the same name.  This makes
	 * sure that data sets only get counted once, even if they are sources to multiple data sets in the
	 * recursion tree.  It also does not include any excluded data sets, sources of excluded data sets, or
	 * source data sets that are not processed.
	 * @param known The set of known data set IDs (to prevent multiple passes down the same branch of the
	 * product source tree).
	 * @param root The current root of the tree being recursed.
	 * @param statuses The mapping of statuses used to determine if a source is completed.
	 */
	private void getCompletedProgressBarCount(Set<String> known, DatasetBean root, Map<Integer, StatusBean> statuses) {
		for (DatasetBean source: root.getSourceDatasets()) {
			// Only care about source data sets that have not yet been seen are included in the processing of the product.
			if (!known.contains(source.getDatasetId()) && !root.isExcluded(source.getDatasetId())) {
				// Add the source if it has a resolved/completed status
				if (source.getProcessStatus(statuses) != null && source.getProcessStatus(statuses).isResolved()) {
					known.add(source.getDatasetId());
				}
				// Recursively continue down the tree if the current source has source data sets.
				if (source.getSourceDatasets().size() > 0) {
					getCompletedProgressBarCount(known, source, statuses);
				}
			}
		}
	}

	/**
     * Get the current time from the database.
     * @param connection The connection to the database.
     * @return The current time from the database.
     * @throws SQLException if there is a problem reading the time from the database.
     */
    private Timestamp getCurrentTime(Connection connection) throws SQLException {
		PreparedStatement stmt = connection.prepareStatement("SELECT NOW()");
		ResultSet results = stmt.executeQuery();
		Timestamp now = null;
		if (results.next()) { now = results.getTimestamp(1); }
		try { results.close(); } catch (SQLException e) {}
		try { stmt.close(); } catch (SQLException e) {}
		return now;
    }
	
    /**
	 * Get the location of the source data for the currently selected product data set.
	 * @return The location of the source data.
	 */
	public String getDataLocation() { return getDataLocation(getSelectedProduct()); }
	
	/**
	 * Get the location of the source data for the specified product.
	 * @param productId The DTS/EMDAC data set ID for the product.
	 * @return The location of the source data.
	 */
	public String getDataLocation(String productId) {
		return sourceDataDirectory.get(productId);
	}
	
	/**
	 * Get the entry date of this version of the data set as a String.
	 * @return The String of the data set version's entry date.
	 */
	public String getDatasetEntryDate() { return getEntryDate().toString(); }
	
	/**
	 * Get the unique ID for the data set.
	 * @return The data set's ID.
	 */
	public String getDatasetId() { return datasetId; }
	
	/**
	 * Get the entry date of this version of the data set.
	 * @return The data set version's entry date.
	 */
	public Timestamp getEntryDate() { return entryDate; }
	
	/**
	 * Get the number of source data sets excluded from this product data set.
	 * @return The number of excluded data sets.
	 */
	public int getExcludedDatasetCount() {
		return excludedSources.contains(getDatasetId()) ? excludedSources.size() - 1 : excludedSources.size();
	}
	
	/**
	 * Get the database ID of the source contact of this data set.
	 * @return The source contact's database ID.
	 */
	public int getExternalContact() { return extContact; }
	
	/**
	 * Get the source contact of this data set.
	 * @param users The mapping of possible source contacts.
	 * @return The source contact or <code>null</code> if a source contact is not assigned.
	 */
	public UserBean getExternalContact(Map<Integer, UserBean> users) { return users.get(getExternalContact()); }
	
	/**
	 * Get the directory where the final processed data can be found for this data set.
	 * @return The final processed data directory.
	 */
	public String getFinalProcessedDataLocation() { return finalDataLocation == null ? "" : finalDataLocation; }
	
	/**
	 * Get the URL to the How To for processing this data set.
	 * @return The How To URL for this data set.
	 */
	public String getHowtoURL() { return howtoURL == null ? "" : howtoURL; }
	
	/**
	 * Get the database ID of the contact tasked with ingesting this data set.
	 * @return The ingesting contact's database ID.
	 */
	public int getIngester() { return ingester; }
	
	/**
	 * Get the contact tasked with ingesting this data set.
	 * @param users The mapping of possible ingesting contacts.
	 * @return The ingesting contact or <code>null</code> if an ingest contact is not assigned.
	 */
	public UserBean getIngester(Map<Integer, UserBean> users) { return users.get(getIngester()); }
	
	/**
	 * Get the directory where the data was ingested for the data set.
	 * @return The data set's ingest directory.
	 */
	public String getIngestLocation() { return ingestLocation == null ? "" : ingestLocation.trim(); }
	
	/**
	 * Get the database ID of the status of the ingest task for the data set.
	 * @return The data set's ingest status ID.
	 */
	public int getIngestStatus() { return ingestStatus; }
	
	/**
	 * Get the status of the ingesting task for this data set.
	 * @param statuses The mapping of possible ingesting statuses.
	 * @return The ingesting status or <code>null</code> if the status is not assigned.
	 */
	public StatusBean getIngestStatus(Map<Integer, StatusBean> statuses) {
		return statuses.get(getIngestStatus());
	}
	
	/**
	 * Get the database ID of the ingest type of the data acquired for this data set.
	 * @return The data set's ingest type ID.
	 */
	public int getIngestType() { return ingestType; }
	
	/**
	 * Get the ingest type of the data acquired for this data set.
	 * @param types The mapping of possible ingest types.
	 * @return The ingest type or <code>null</code> if the ingest type is not assigned.
	 */
	public IngestTypeBean getIngestType(Map<Integer, IngestTypeBean> types) {
		return types.get(getIngestType());
	}
	
	/**
	 * Get the database ID of the internal contact for this data set.
	 * @return The internal contact's database ID.
	 */
	public int getInternalContact() { return intContact; }
	
	/**
	 * Get the internal contact for this data set.
	 * @param users The mapping of possible internal contacts.
	 * @return The internal contact or <code>null</code> if an internal contact is not assigned.
	 */
	public UserBean getInternalContact(Map<Integer, UserBean> users) { return users.get(getInternalContact()); }
	
	/**
	 * Get the index of this data set in the displayed data set list.
	 * @return The 1 based list index of the data set.
	 */
	public int getListIndex() { return listIndex + 1; }
	
	/**
	 * Get the directory where data to be loaded for this data set is located.
	 * @return The data to be loaded directory.
	 */
	public String getLoadDataLocation() { return loadDataLocation == null ? "" : loadDataLocation.trim(); }
	
	/**
	 * Get the database ID of the contact tasked with loading this data set.
	 * @return The loading contact's database ID.
	 */
	public int getLoader() { return loader; }
	
	/**
	 * Get the contact tasked with loading this data set.
	 * @param users The mapping of possible loading contacts.
	 * @return The loading contact or <code>null</code> if a load contact is not assigned.
	 */
	public UserBean getLoader(Map<Integer, UserBean> users) { return users.get(getLoader()); }
	
	/**
	 * Get the database ID of the status of the load task for the data set.
	 * @return The data set's load status ID.
	 */
	public int getLoadStatus() { return loadStatus; }
	
	/**
	 * Get the status of the loading task for this data set.
	 * @param statuses The mapping of possible loading statuses.
	 * @return The loading status or <code>null</code> if the status is not assigned.
	 */
	public StatusBean getLoadStatus(Map<Integer, StatusBean> statuses) {
		return statuses.get(getLoadStatus());
	}
	
	/**
	 * Get the name/title of this data set.
	 * @return The name of the data set.
	 */
	public String getName() { return name == null ? "" : name.trim(); }
	
	/**
	 * Get the next data set in the displayed data set list.
	 * @return The next data set in the list or <code>null</code> if this is the last data
	 * set in the list.
	 */
	public DatasetBean getNextDataset() { return next; }
	
	/**
	 * Get the collection of unique authors of data set notes.
	 * @param users The mapping of possible note authors.
	 * @return A collection of authors of data set notes.
	 */
	public Collection<UserBean> getNoteAuthors(Map<Integer, UserBean> users) {
		Set<UserBean> authors = new TreeSet<UserBean>();
		
		for (NoteBean note: notes) {
			UserBean author = note.getAuthor(users);
			if (author != null) { authors.add(author); }
		}
		
		return authors;
	}
	
	/**
	 * Get the set of notes associated to this data set.
	 * @return The notes associated the data set.
	 */
	public List<NoteBean> getNotes() {
		Collections.sort(notes);
		return notes;
	}
	
	/**
	 * Get the data set ID as it is in the database whether it was changed in a form
	 * or not.
	 * @return The original data set ID as it is in the database.
	 */
	public String getOriginalDatasetId() { return originalId; }

	/**
	 * Get the set of EMDAC platform IDs associated to this data set.
	 * @return The set of platform IDs.
	 */
	public Set<Integer> getPlatforms() { return platforms; }
	
	/**
	 * Get the set of EMDAC platform IDs associated to this data set as a semicolon delimited list.
	 * @return The set of platform IDs as a String.
	 */
	public String getPlatformString() {
		StringBuffer str = new StringBuffer();
		for (Integer id: getPlatforms()) {
			str.append(id).append("; ");
		}
		return str.length() == 0 ? "" : str.substring(0, str.length() - 2).toString();
	}
	
	/**
	 * Get the URL to the plots generated from the data in this data set.
	 * @return The plots URL for this data set.
	 */
	public String getPlotsURL() { return plotURL == null ? "" : plotURL; }
	
	/**
	 * Get the previous data set in the displayed data set list.
	 * @return The previous data set in the list or <code>null</code> if this is the first data
	 * set in the list.
	 */
	public DatasetBean getPreviousDataset() { return previous; }
	
	/**
	 * Get the database ID of the contact tasked with processing this data set.
	 * @return The processing contact's database ID.
	 */
	public int getProcesser() { return processer; }
	
	/**
	 * Get the contact tasked with processing this data set.
	 * @param users The mapping of possible processing contacts.
	 * @return The processing contact or <code>null</code> if a process contact is not assigned.
	 */
	public UserBean getProcesser(Map<Integer, UserBean> users) { return users.get(getProcesser()); }
	
	/**
	 * Get the database ID of the status of the process task for the data set.
	 * @return The data set's process status ID.
	 */
	public int getProcessStatus() { return processStatus; }
	
	/**
	 * Get the status of the processing task for this data set.
	 * @param statuses The mapping of possible processing statuses.
	 * @return The processing status or <code>null</code> if the status is not assigned.
	 */
	public StatusBean getProcessStatus(Map<Integer, StatusBean> statuses) {
		return statuses.get(getProcessStatus());
	}
	
	/**
	 * Get the list of product data sets this source data set is associated to as a source.
	 * @return The list of product data sets for this source data set.
	 */
	public List<DatasetBean> getProducts() {
		List<DatasetBean> products = new ArrayList<DatasetBean>(productMap.values());
		Collections.sort(products, new dln.util.DatasetNameComparator(false));
		return products;
	}
	
	/**
	 * Get the list of project IDs associated to the data set as a comma delimited String.
	 * @return The projects as a comma delimited list.
	 */
	public String getProjects() {
		StringBuffer sb = new StringBuffer();
		List<String> sortedIds = new ArrayList<String>(getProjectsAsList());
		Collections.sort(sortedIds);
		for (String projectId: sortedIds) {
			sb.append(projectId).append(", ");
		}
		return sb.substring(0, sb.length() - 2);
	}
	
	/**
	 * Get the projects associated to the data set as a list of project IDs.
	 * @return The list of project IDs associated to the data set.
	 */
	public List<String> getProjectsAsList() { return projectList; }
	
	/**
	 * Get the readme URL for the data set.
	 * @return The data set's readme URL.
	 */
	public String getReadmeURL() { return readmeURL == null ? "" : readmeURL.trim(); }

	/**
	 * Get the remote URL for the data set.
	 * @return The data set's remote URL.
	 */
	public String getRemoteURL() { return remoteURL == null ? "" : remoteURL.trim(); }

	/**
	 * Get the notes associated to the data set in reverse time order (most recent notes
	 * first).
	 * @param noteTypeFilterId The database ID of the note type to be included in the note list.
	 * @param authorId The database ID of the author to be included in the note list.
	 * @return The list of filtered notes in most recent time creation order.
	 */
	public List<NoteBean> getReverseNoteList(int noteTypeFilterId, int authorId) { 
		List<NoteBean> list = new ArrayList<NoteBean>();
		for (NoteBean note: notes) {
			if ((noteTypeFilterId == -1 || note.isNoteType(noteTypeFilterId) || (noteTypeFilterId == 0 && note.getAuthor() == 0)) &&
				(authorId == -1 || note.getAuthor() == authorId)) {
				list.add(0, note);
			}
		}
		Collections.sort(list);
		Collections.reverse(list);
		return list;
	}
	
	/**
	 * Get the process/general notes associated to the data set in reverse time order. 
	 * @return The list of notes in most recent time creation order.
	 */
	public List<NoteBean> getReverseProcessNotes() {
		List<NoteBean> list = new ArrayList<NoteBean>(processNotes);
		Collections.sort(list);
		Collections.reverse(list);
		return list;
	}
	
	/**
	 * Get the DTS/EMDAC data set ID of the currently selected product for this source data set.
	 * @return The data set ID for the selected product.
	 */
	public String getSelectedProduct() { return selectedProductId; }
	
	/**
	 * Get the set of software packages used to generate this data set.
	 * @return The software used to generate this data set.
	 */
	public Set<SoftwareBean> getSoftware() { return softwareSet; }
	
	/**
	 * Get the list of source data sets assigned to this processed data sets.
	 * @return The list of source data sets.
	 */
	public List<DatasetBean> getSourceDatasets() { return sourceDatasets; }
	
	/**
	 * Get the note containing the source information about this source data set.
	 * @return The source information note.
	 */
	public NoteBean getSourceInfoNote() { return sourceInfoNote; }
	
	/**
	 * Get the location of the station list file generated by this processed data set.
	 * @return The station list file location.
	 */
	public String getStationListFile() { return stationListFile == null ? "" : stationListFile; }
	
	/**
	 * Get the total number of source data sets associated to this product data set.
	 * @return The total number of source data sets.
	 */
	public int getTotalDatasetCount() {
		return getSourceDatasets().contains(getDatasetId()) ? getSourceDatasets().size() - 1 : getSourceDatasets().size();
	}
	
	/**
	 * Get the total number of processed data sets that are associated to this product data set.
	 * This is a recursive function that counts all of the processed source data sets down the product tree.
	 * @return The number of processed source data sets for this product.
	 */
	public int getTotalProgressBarCount() {
		Set<String> datasets = new TreeSet<String>();
		
		getTotalProgressBarCount(datasets, this);

		// Make sure this data set isn't counted as part of the progress bar.
		datasets.remove(getDatasetId());
		
		return datasets.size();
	}
	
	/**
	 * This is the main part of the recursion for the public function with the same name.  This makes
	 * sure that data sets only get counted once, even if they are sources to multiple data sets in the
	 * recursion tree.  It also does not include any excluded data sets, sources of excluded data sets, or
	 * source data sets that are not processed.
	 * @param known The set of known data set IDs (to prevent multiple passes down the same branch of the
	 * product source tree).
	 * @param root The current root of the tree being recursed.
	 */
	private void getTotalProgressBarCount(Set<String> known, DatasetBean root) {
		for (DatasetBean source: root.getSourceDatasets()) {
			if (!known.contains(source.getDatasetId()) && source.getTotalDatasetCount() > 0 && !root.isExcluded(source.getDatasetId())) {
				known.add(source.getDatasetId());
				getTotalProgressBarCount(known, source);
			}
		}
	}
	
	/**
	 * Get the time offset from UTC of the data.
	 * @return The data's UTC time offset.
	 */
	public String getUtcOffset() { return utcOffset; }
	
	/**
	 * Get the directory where the processing work was done for this data set.
	 * @return The processed data set's work directory.
	 */
	public String getWorkLocation() { return workLocation == null ? "" : workLocation; }
	
	/**
	 * Determine if this data set has been flags with questions/issues.
	 * @return <code>true</code> if the data set has outstanding questions,
	 * <code>false</code> if it does not.
	 */
	public boolean hasQuestions() { return questions; }
	
	/**
	 * Determine if the specified data set is a source of this data set.
	 * @param datasetId The DTS data set ID for the source data set.
	 * @return <code>true</code> if the specified data set is a source of this data set, <code>false</code>
	 * if it is not.
	 */
	public boolean hasSource(String datasetId) {
		for (DatasetBean source: getSourceDatasets()) {
			if (datasetId.equals(source.getDatasetId())) { return true; }
		}
		return false;
	}
	
	/**
	 * Insert the data set into the database with the specified note.
	 * @param note The note to include with the data set.
	 * @throws SQLException if there is an issues inserting the data set into the database.
	 */
	public void insert(NoteBean note) throws SQLException {
		// Define the connection to the database.
		Connection connection = getConnection();
		connection.setAutoCommit(false);
		
		try {
			// Update the data set ID with a DTS number if it is not defined.
			determineDatasetId(connection);
					
			// This will be the new date for the version.
			setEntryDate(getCurrentTime(connection));
			
			// Insert the data set into the database.
			String sql = "INSERT INTO dataset(dataset_id, name, source_contact_id, internal_contact_id, question_flag, remote_url, readme_url) VALUES(?, ?, ?, ?, ?, ?, ?)";
			PreparedStatement datasetStmt = connection.prepareStatement(sql);
			datasetStmt.setString(1, getDatasetId());
			datasetStmt.setString(2, getName());
			if (getExternalContact() <= 0) {
				datasetStmt.setNull(3, Types.INTEGER);
			} else {
				datasetStmt.setInt(3, getExternalContact());
			}
			datasetStmt.setInt(4, getInternalContact());
			datasetStmt.setBoolean(5, hasQuestions());
			datasetStmt.setString(6, getRemoteURL());
			datasetStmt.setString(7, getReadmeURL());
			datasetStmt.execute();
			try { datasetStmt.close(); } catch (SQLException e) {}
		
			// Insert the insert task into the database.
			sql = "INSERT INTO dataset_ingest(ingest_type_id, ingest_location, ingest_contact_id, ingest_status_id, dataset_id, entry_date) VALUES(?, ?, ?, ?, ?, ?)";
			PreparedStatement ingestStmt = connection.prepareStatement(sql);
			if (getIngestType() <= 0) {
				ingestStmt.setNull(1, Types.INTEGER);
			} else {
				ingestStmt.setInt(1, getIngestType());
			}
			ingestStmt.setString(2, getIngestLocation());
			if (getIngester() <= 0) {
				ingestStmt.setNull(3, Types.INTEGER);
			} else {
				ingestStmt.setInt(3, getIngester());
			}
			if (getIngestStatus() <= 0) {
				ingestStmt.setNull(4, Types.INTEGER);
			} else {
				ingestStmt.setInt(4, getIngestStatus());
			}
			ingestStmt.setString(5, getDatasetId());
			ingestStmt.setTimestamp(6, getEntryDate());
			ingestStmt.execute();
			try { ingestStmt.close(); } catch (SQLException e) {}

			// Insert the load task into the database.
			sql = "INSERT INTO dataset_load(load_data_location, archive_location, load_contact_id, load_status_id, documentation_flag, dataset_id, entry_date) VALUES (?, ?, ?, ?, ?, ?, ?)";
			PreparedStatement loadStmt = connection.prepareStatement(sql);
			loadStmt.setString(1, getLoadDataLocation());
			loadStmt.setString(2, getArchiveLocation());
			if (getLoader() <= 0) {
				loadStmt.setNull(3, Types.INTEGER);
			} else {
				loadStmt.setInt(3, getLoader());
			}
			if (getLoadStatus() <= 0) {
				loadStmt.setNull(4, Types.INTEGER);
			} else {
				loadStmt.setInt(4, getLoadStatus());
			}
			loadStmt.setBoolean(5, isDocumented());
			loadStmt.setString(6, getDatasetId());
			loadStmt.setTimestamp(7, getEntryDate());
			loadStmt.execute();
			try { loadStmt.close(); } catch (SQLException e) {}

			// Insert the check task into the database.
			sql = "INSERT INTO dataset_approve(approve_contact_id, approve_status_id, dataset_id, entry_date) VALUES(?, ?, ?, ?)";
			PreparedStatement approveStmt = connection.prepareStatement(sql);
			if (getChecker() <= 0) {
				approveStmt.setNull(1, Types.INTEGER);
			} else {
				approveStmt.setInt(1, getChecker());
			}
			if (getCheckStatus() <= 0) {
				approveStmt.setNull(2, Types.INTEGER);
			} else {
				approveStmt.setInt(2, getCheckStatus());
			}
			approveStmt.setString(3, getDatasetId());
			approveStmt.setTimestamp(4, getEntryDate());
			approveStmt.execute();
			try { approveStmt.close(); } catch (SQLException e) {}

			// Insert the project associations into the database.
			sql = "INSERT INTO dataset_project(dataset_id, project_id, master_list_flag) VALUES(?,?,?)";
			PreparedStatement projStmt = connection.prepareStatement(sql);
			projStmt.setString(1, getDatasetId());
			for (String projectId: getProjectsAsList()) {
				if (!isDeletedProject(projectId)) {
					projStmt.setString(2, projectId);
					projStmt.setBoolean(3, isInMasterList(projectId));
					projStmt.execute();
				}
			}
			try { projStmt.close(); } catch (SQLException e) {}

			// Only try to add the note if there is a note to add.
			if (note.getNoteText() != null && !note.getNoteText().trim().equals("")) {
				note.insert(connection, this);
			}
		
			// Save the commands to the data base.
			try { connection.commit(); }
			catch (SQLException e) {
				try { connection.rollback(); }
				catch (SQLException e1) {}
				throw e;
			}
		} 
		catch (SQLException e) { throw e; }
		
		// Close the connection to the database no matter what happened.
		finally {
			try { connection.close(); } catch (SQLException e) {}
		}
	}
	
	/**
	 * Determine if this data set has been checked.
	 * @param statuses The mapping of possible checked statuses.
	 * @return <code>true</code> if this data set has been marked as resolved in its check
	 * status, <code>false</code> if it has not.
	 */
	public boolean isChecked(Map<Integer, StatusBean> statuses) {
		return statuses.containsKey(checkStatus) ? statuses.get(checkStatus).isResolved() : false;
	}
	
	/**
	 * Determine if this processed data set can delete it's processed task from the database.
	 * @return <code>true</code> if the data sets has 1. no attached software, 2. no attached
	 * source data sets, 3. a single version of the processed task, and 4. no/empty current
	 * task data; <code>false</code> otherwise.
	 */
	public boolean isDeletable() { 
		// Count the number of versions for the data set.
		int versions = 0;
		for (NoteBean note: getReverseProcessNotes()) {
			if (note.getNoteTypes().length == 0) { versions++; }
		}
		
		// Determine if the data set has any information so it can't be deleted.
		if (getSoftware().size() == 0 && getSourceDatasets().size() == 0 && versions == 1 &&
			getHowtoURL().equals("") && getWorkLocation().equals("") && getFinalProcessedDataLocation().equals("") &&
			getStationListFile().equals("") && getPlotsURL().equals("") && getProcesser() < 1 &&
			getProcessStatus() < 1) {
			return true;
		}
		
		return false;
	}
	
	/**
	 * Determine if the specified project has been marked to be disassociated with the data set.
	 * @param projectId The ID of the project to be tested for deletion.
	 * @return <code>true</code> if the project is to be disassociated with the data set,
	 * <code>false</code> if it is not.
	 */
	public boolean isDeletedProject(String projectId) { return deletedProjects.contains(projectId); }
	
	/**
	 * Determine if the data set has a documentation file.
	 * @return <code>true</code> if the data set has at least one documentation file,
	 * <code>false</code> if it does not.
	 */
	public boolean isDocumented() { return docFlag; }	
	
	/**
	 * Determine if this source data set's data uses DST.
	 * @return <code>true</code> if the data uses DST, <code>false</code> if it does not, or
	 * <code>null</code> if it is unknown.
	 */
	public Boolean isDSTFlag() { return dstFlag; }

	/**
	 * Determine if this data set is excluded from the creation of the currently selected product.
	 * @return <code>true</code> if this data set is excluded from the creation of the product,
	 * <code>false</code> if it is not.
	 */
	public boolean isExcluded() { return isExcluded(getSelectedProduct()); }
	
	/**
	 * Determine if this data set is excluded from the creation of the specified product.
	 * @param datasetId The DTS/EMDAC data set ID of the specified product.
	 * @return <code>true</code> if this data set is excluded from the creation of the product,
	 * <code>false</code> if it is not.
	 */
	public boolean isExcluded(String datasetId) {
		return excludedSources.contains(datasetId);
	}
	
	/**
	 * Determine if the data set has been marked to be included in the project's master list..
	 * @param projectId The ID of the project to be tested for master list inclusion.
	 * @return <code>true</code> if the data set is in the project's master list,
	 * <code>false</code> if it is not.
	 */
	public boolean isInMasterList(String projectId) { return masterListFlags.contains(projectId); }
	
	/**
	 * Determine if this data set has been loaded.
	 * @param statuses The mapping of possible load statuses.
	 * @return <code>true</code> if this data set has been marked as resolved in its load
	 * status, <code>false</code> if it has not.
	 */
	public boolean isLoaded(Map<Integer, StatusBean> statuses) {
		return statuses.containsKey(loadStatus) ? statuses.get(loadStatus).isResolved() : false;
	}
	
	/**
	 * Determine if this data set is a processed data set.
	 * @return <code>true</code> if this is a processed data set, <code>false</code> otherwise.
	 */
	public boolean isProcessedDataset() { return isProcessed; }
	
	/**
	 * Determine if this data set is a product data set.
	 * @return <code>true</code> if this data set is a product data set, <code>false</code>
	 * otherwise.
	 */
	public boolean isProduct() { 		
		// Check if it is not a source, but a processed data set.
		if (getProducts().size() == 0 && isProcessedDataset()) { return true; }
		
		// If a data set does not have any source data sets, it is only a source data set and not a product.
		if (getSourceDatasets().size() == 0) { return false; }

		// Only has a single source, but it is itself.
		if (getSourceDatasets().size() == 1 && getSourceDatasets().get(0).getDatasetId().equals(getDatasetId())) {
			return true;
		}
		
		// Count the number of source data sets
		int processedDatasetCount = 0;
		for (DatasetBean source: getSourceDatasets()) {
			if (source.getSourceDatasets().size() > 0) { processedDatasetCount++; }
		}
		if (processedDatasetCount > 1) { return true; }
		
		// This data set has source data sets and is not a source for some product.
		if (getSourceDatasets().size() > 0 && !isSource()) { return true; }
		
		// Default must be false.
		return false;
	}
	
	/**
	 * Determine if this data set is a source for another data set.
	 * @return <code>true</code> if the data set is a source for another data set,
	 * <code>false</code> if it is not.
	 */
	public boolean isSource() { return isSource; }

	/**
	 * Load this DatasetBean with the values from the database for the specified data set ID.
	 * @param datasetId The ID of the data set to be loaded.
	 * @throws SQLException if there is a problem loading the data set from the database.
	 */
	public void loadDataset(String datasetId) throws SQLException {
		// Define the connection to the database.
		Connection connection = getConnection();

		try {
			// Define the statement to load the data set.
			String sql = "SELECT name, source_contact_id, internal_contact_id, question_flag, remote_url, MAX(entry_date), readme_url FROM dataset JOIN dataset_approve ON (dataset.dataset_id=dataset_approve.dataset_id) WHERE dataset.dataset_id=? GROUP BY dataset.dataset_id";
			PreparedStatement stmt = connection.prepareStatement(sql);
			stmt.setString(1, datasetId);
			ResultSet results = stmt.executeQuery();
			// Load the results into the bean.
			if (results.next()) {
				setDatasetId(datasetId);
				setOriginalDatasetId(datasetId);
				setName(results.getString(1));
				setExternalContact(results.getInt(2));
				setInternalContact(results.getInt(3));
				setQuestions(results.getBoolean(4));
				setRemoteURL(results.getString(5));
				setEntryDate(results.getTimestamp(6));
				setReadmeURL(results.getString(7));
			}
			// Close the open database streams.
			try { results.close(); } catch (SQLException e) {}
			try { stmt.close(); } catch (SQLException e) {}

			// Define the statement to read in the task information
			sql = "SELECT ingest_type_id, ingest_location, ingest_contact_id, ingest_status_id, load_data_location, archive_location, load_contact_id, load_status_id, documentation_flag, approve_contact_id, approve_status_id FROM dataset_ingest JOIN dataset_load ON (dataset_ingest.dataset_id=dataset_load.dataset_id AND dataset_ingest.entry_date=dataset_load.entry_date) JOIN dataset_approve ON (dataset_ingest.dataset_id=dataset_approve.dataset_id AND dataset_ingest.entry_date=dataset_approve.entry_date) WHERE dataset_ingest.dataset_id=? AND dataset_ingest.entry_date=?";
			stmt = connection.prepareStatement(sql);
			stmt.setString(1, getDatasetId());
			stmt.setTimestamp(2, getEntryDate());
			results = stmt.executeQuery();
			// Load the task results into the bean.
			if (results.next()) {
				setIngestType(results.getInt(1));
				setIngestLocation(results.getString(2));
				setIngester(results.getInt(3));
				setIngestStatus(results.getInt(4));
				setLoadDataLocation(results.getString(5));
				setArchiveLocation(results.getString(6));
				setLoader(results.getInt(7));
				setLoadStatus(results.getInt(8));
				setDocumented(results.getBoolean(9));
				setChecker(results.getInt(10));
				setCheckStatus(results.getInt(11));
			}
			// Close the open database streams.
			try { results.close(); } catch (SQLException e) {}
			try { stmt.close(); } catch (SQLException e) {}			
		
			// Determine if the data set is a processed data set.
			sql = "SELECT COUNT(*) FROM dataset_process WHERE dataset_id=?";
			stmt = connection.prepareStatement(sql);
			stmt.setString(1, getDatasetId());
			results = stmt.executeQuery();
			if (results.next()) { setProcessedDataset(results.getInt(1) > 0); }
			// Close the open database streams.
			try { results.close(); } catch (SQLException e) {}
			try { stmt.close(); } catch (SQLException e) {}
			
			// Define the statement to load the project information.
			sql = "SELECT project_id, master_list_flag FROM dataset_project WHERE dataset_id=?";
			stmt = connection.prepareStatement(sql);
			stmt.setString(1, getDatasetId());
			results = stmt.executeQuery();
			// Load the project results into the bean.
			while (results.next()) {
				addProject(results.getString(1));
				setInMasterList(results.getString(1), results.getBoolean(2));
			}
			// Close the open database streams.
			try { results.close(); } catch (SQLException e) {}
			try { stmt.close(); } catch (SQLException e) {}
		}
		catch (SQLException e) { throw e; }
		// Close the database connection no matter what happens.
		finally {
			try { connection.close(); } catch (SQLException e) {}
		}
	}
	
	/**
	 * Load the specified data set version into the bean. 
	 * @param datasetId The ID of the data set to be loaded.
	 * @param date The version date of the data set to be loaded.
	 * @param display The display bean containing load restrictions.
	 * @return The DatasetBean loaded from the database.
	 * @throws SQLException if there is a problem loading the data set.
	 */
	public static DatasetBean loadDataset(String datasetId, String date, DatasetDisplayBean display) throws SQLException {
		// Define the connection to the database.
		Connection connection = getConnection();
		
		try {
			// Define the maps required for the loading.
			Map<Integer, UserBean> users = (new UserBean()).getAllUsersMap(connection);
			Map<Integer, StatusBean> statuses = StatusBean.getAllStatusesMap(connection);
			Map<Integer, IngestTypeBean> ingestTypes = IngestTypeBean.getAllIngestTypesMap(connection);
		
			// Get the list of data set that will be displayed in the view.
			List<DatasetBean> datasets = display.update(loadDatasets(connection, display), users, statuses);
			// Determine where this data set is in the list.
			int index = findDatasetInList(datasets, datasetId, date);
	
			// Find the current data set and assign neighbors
			DatasetBean dataset = datasets.get(index);
			dataset.setListIndex(index);
			if (index - 1 >= 0) { dataset.setPreviousDataset(datasets.get(index - 1)); }
			if (index + 1 < datasets.size()) { dataset.setNextDataset(datasets.get(index + 1)); }
		
			// Define the statement to load the data set details.
			String sql = "SELECT source_contact_id, internal_contact_id, remote_url, ingest_type_id, ingest_location, load_data_location, archive_location, readme_url FROM dataset JOIN dataset_ingest ON (dataset.dataset_id=dataset_ingest.dataset_id) JOIN dataset_load ON (dataset.dataset_id=dataset_load.dataset_id AND dataset_ingest.entry_date=dataset_load.entry_date) JOIN dataset_approve ON (dataset.dataset_id=dataset_approve.dataset_id AND dataset_ingest.entry_date=dataset_approve.entry_date) WHERE dataset.dataset_id=? AND dataset_ingest.entry_date=?";
			PreparedStatement stmt = connection.prepareStatement(sql);
			stmt.setString(1, datasetId);
			stmt.setString(2, date);
			ResultSet results = stmt.executeQuery();
			// Load the results into the data set bean.
			if (results.next()) {
				dataset.setExternalContact(results.getInt(1));
				dataset.setInternalContact(results.getInt(2));
				dataset.setRemoteURL(results.getString(3));
				dataset.setIngestType(results.getInt(4));
				dataset.setIngestLocation(results.getString(5));
				dataset.setLoadDataLocation(results.getString(6));
				dataset.setArchiveLocation(results.getString(7));
				dataset.setReadmeURL(results.getString(8));
			}
			// Close the open database streams.
			try { results.close(); } catch (SQLException e) {}
			try { stmt.close(); } catch (SQLException e) {}
	
			// Load the project information from the database.
			sql = "SELECT project_id, master_list_flag FROM dataset_project WHERE dataset_id=?";
			stmt = connection.prepareStatement(sql);
			stmt.setString(1, dataset.getDatasetId());
			results = stmt.executeQuery();
			while (results.next()) {
				dataset.addProject(results.getString(1));
				dataset.setInMasterList(results.getString(1), results.getBoolean(2));
			}
			// Close the open database streams.
			try { results.close(); } catch (SQLException e) {}
			try { stmt.close(); } catch (SQLException e) {}
		
			// Load the notes for the data set.
			dataset.loadNotes(connection, users, statuses, ingestTypes);
			
			// The data set was loaded, so it can be returned.
			return dataset;
		}
		catch (SQLException e) { throw e; }
		// Close the connection to the database no matter what happens.
		finally {
			try { connection.close(); } catch (SQLException e) {}
		}
	}

	/**
	 * Get the list of data sets to be displayed in the view.
	 * @param connection The connection to use to load the data sets.
	 * @param display The restrictions on the list to be displayed.
	 * @return The list of data sets to be displayed in the view.
	 * @throws SQLException if there is a problem loading the data sets.
	 */
	private static List<DatasetBean> loadDatasets(Connection connection, DatasetDisplayBean display) throws SQLException {
		// Define the list to store the list from the data sets.
		List<DatasetBean> datasets = new ArrayList<DatasetBean>();
		
		// Define the statement to be executed on the database.
		PreparedStatement stmt = null;
		
		// Limit the list based on the project ID
		if (display.isDisplayProjectList()) {
			String sql = "SELECT dataset.dataset_id, name, dataset_approve.entry_date, ingest_contact_id, load_contact_id, approve_contact_id, ingest_status_id, load_status_id, approve_status_id, documentation_flag, question_flag, master_list_flag FROM dataset JOIN dataset_project ON (dataset.dataset_id=dataset_project.dataset_id) JOIN dataset_approve ON (dataset.dataset_id=dataset_approve.dataset_id) JOIN dataset_load ON (dataset.dataset_id=dataset_load.dataset_id AND dataset_approve.entry_date=dataset_load.entry_date) JOIN dataset_ingest ON (dataset.dataset_id=dataset_ingest.dataset_id AND dataset_approve.entry_date = dataset_ingest.entry_date) WHERE project_id=?";
			stmt = connection.prepareStatement(sql);
			stmt.setString(1, display.getDisplayId().trim());
		}
		// Limit the list based on the ingest contact ID
		else if (display.isDisplayIngesterList()) {
			String sql = "SELECT dataset.dataset_id, name, dataset_approve.entry_date, ingest_contact_id, load_contact_id, approve_contact_id, ingest_status_id, load_status_id, approve_status_id, documentation_flag, question_flag, NULL FROM dataset JOIN dataset_approve ON (dataset.dataset_id=dataset_approve.dataset_id) JOIN dataset_load ON (dataset.dataset_id=dataset_load.dataset_id AND dataset_approve.entry_date=dataset_load.entry_date) JOIN dataset_ingest ON (dataset.dataset_id=dataset_ingest.dataset_id AND dataset_approve.entry_date = dataset_ingest.entry_date) WHERE ingest_contact_id=?";
			stmt = connection.prepareStatement(sql);
			stmt.setInt(1, Integer.parseInt(display.getDisplayId()));
		}
		// Limit the list based on the load contact ID
		else if (display.isDisplayLoaderList()) {
			String sql = "SELECT dataset.dataset_id, name, dataset_approve.entry_date, ingest_contact_id, load_contact_id, approve_contact_id, ingest_status_id, load_status_id, approve_status_id, documentation_flag, question_flag, NULL FROM dataset JOIN dataset_approve ON (dataset.dataset_id=dataset_approve.dataset_id) JOIN dataset_load ON (dataset.dataset_id=dataset_load.dataset_id AND dataset_approve.entry_date=dataset_load.entry_date) JOIN dataset_ingest ON (dataset.dataset_id=dataset_ingest.dataset_id AND dataset_approve.entry_date = dataset_ingest.entry_date) WHERE load_contact_id=?";
			stmt = connection.prepareStatement(sql);
			stmt.setInt(1, Integer.parseInt(display.getDisplayId()));
		}
		// Limit the list based on the check contact ID
		else if (display.isDisplayCheckerList()) {
			String sql = "SELECT dataset.dataset_id, name, dataset_approve.entry_date, ingest_contact_id, load_contact_id, approve_contact_id, ingest_status_id, load_status_id, approve_status_id, documentation_flag, question_flag, NULL FROM dataset JOIN dataset_approve ON (dataset.dataset_id=dataset_approve.dataset_id) JOIN dataset_load ON (dataset.dataset_id=dataset_load.dataset_id AND dataset_approve.entry_date=dataset_load.entry_date) JOIN dataset_ingest ON (dataset.dataset_id=dataset_ingest.dataset_id AND dataset_approve.entry_date = dataset_ingest.entry_date) WHERE approve_contact_id=?";
			stmt = connection.prepareStatement(sql);
			stmt.setInt(1, Integer.parseInt(display.getDisplayId()));
		}
		
		
		// Execute the statement and get the results.
		ResultSet results = stmt.executeQuery();
		while (results.next()) {
			DatasetBean dataset = new DatasetBean(results.getString(1));
			dataset.setName(results.getString(2));
			dataset.setEntryDate(results.getTimestamp(3));
			dataset.setIngester(results.getInt(4));
			dataset.setLoader(results.getInt(5));
			dataset.setChecker(results.getInt(6));
			dataset.setIngestStatus(results.getInt(7));
			dataset.setLoadStatus(results.getInt(8));
			dataset.setCheckStatus(results.getInt(9));
			dataset.setDocumented(results.getBoolean(10));
			dataset.setQuestions(results.getBoolean(11));
			// Only assign the master list flag when in project view as it has
			// no meaning in the task view.
			if (display.getDisplayView() == DatasetDisplayBean.PROJECT_DISPLAY_VIEW) {
				dataset.addProject(display.getDisplayId());
				dataset.setInMasterList(display.getDisplayId(), results.getBoolean(12));
			}
			
			// Add the data set to the result list.
			datasets.add(dataset);
		}
		
		// Close the open database streams.
		try { results.close(); } catch (SQLException e) {}
		try { stmt.close(); } catch (SQLException e) {}
		
		// Return the list of data sets loaded from the database.
		return datasets;
	}

	/**
	 * Get the list of data sets to be displayed in the view.
	 * @param display The restrictions on the list to be displayed.
	 * @return The list of data sets to be displayed in the view.
	 * @throws SQLException if there is a problem loading the data sets.
	 */
	public static List<DatasetBean> loadDatasets(DatasetDisplayBean display) throws SQLException {
		// Define the connection to the database.
		Connection connection = getConnection();
		try {
			return loadDatasets(connection, display);
		} catch (SQLException e) { throw e; }
		// Close the connection no matter what happens.
		finally { 
			try { connection.close(); } catch (SQLException e) {}
		}
	}

	/**
	 * Load the notes for the data set from the database.
	 * @param connection The connection to user to load the notes.
	 * @param users The mapping of users for the authors and versions.
	 * @param statuses The mapping of statuses for version notes.
	 * @param ingestTypes The mapping of ingest types for version notes.
	 * @throws SQLException if there is a problem loading the data set's notes.
	 */
	private void loadNotes(Connection connection, Map<Integer, UserBean> users, Map<Integer, StatusBean> statuses, Map<Integer, IngestTypeBean> ingestTypes) throws SQLException {
		Map<Integer, NoteBean> noteMap = new TreeMap<Integer, NoteBean>();
		// Load the note types from the database.
		Map<Integer, NoteTypeBean> typeMap = NoteTypeBean.getNoteTypesMap(connection);
		
		// Define the statement to load the notes for the data set.
		String sql = "SELECT note.note_id, note_type_id, author_id, entry_date, note_text, note.row_revise_time FROM note JOIN dataset_note ON (note.note_id=dataset_note.note_id) WHERE dataset_id=?";
		PreparedStatement stmt = connection.prepareStatement(sql);
		stmt.setString(1, getDatasetId());
		ResultSet results = stmt.executeQuery();
		// Loop through each note/note_type pair.
		while (results.next()) {
			Integer noteId = results.getInt(1);
			// If the note already exists, only need to add the new note type.
			if (noteMap.containsKey(noteId)) {
				noteMap.get(noteId).addNoteType(typeMap.get(results.getInt(2)));
			} 
			// Note doesn't exist, so create it an add it to the map.
			else {
				NoteBean note = new NoteBean(noteId);
				note.addNoteType(typeMap.get(results.getInt(2)));
				note.setAuthor(results.getInt(3));
				note.setEntryDate(results.getTimestamp(4));
				note.setNoteText(results.getString(5));
				note.setReviseTime(results.getTimestamp(6));
				noteMap.put(note.getNoteId(), note);
			}
		}
		// Close the open database streams.
		try { results.close(); } catch (SQLException e) {}
		try { stmt.close(); } catch (SQLException e) {}
		
		// Defein the statement to read the version information from the database.
		sql = "SELECT dataset_ingest.entry_date, ingest_type_id, ingest_location, ingest_contact_id, ingest_status_id, load_data_location, archive_location, load_contact_id, load_status_id, documentation_flag, approve_contact_id, approve_status_id, MAX(dataset_ingest.row_revise_time), MAX(dataset_load.row_revise_time), MAX(dataset_approve.row_revise_time) FROM dataset_ingest JOIN dataset_load ON (dataset_ingest.dataset_id=dataset_load.dataset_id AND dataset_ingest.entry_date=dataset_load.entry_date) JOIN dataset_approve ON (dataset_ingest.dataset_id=dataset_approve.dataset_id AND dataset_ingest.entry_date=dataset_approve.entry_date) WHERE dataset_ingest.dataset_id=? GROUP BY (dataset_ingest.entry_date)";
		stmt = connection.prepareStatement(sql);
		stmt.setString(1, getDatasetId());
		results = stmt.executeQuery();
		
		while (results.next()) {
			NoteBean note = new NoteBean();
			// Need to subtract a second so the version will not collide with a note that may
			// have been entered when the version was created.  This will place the version note
			// earlier than the actual note so the note will be displayed in the correct relation
			// to the version.
			note.setEntryDate(new Timestamp(results.getTimestamp(1).getTime() - 1000));
			
			StringBuffer text = new StringBuffer();
			
			// Build the table to display the ingest information.
			text.append("<table class=\"info\">\n");
			text.append("<tr><td><div class=\"floatLeft\"><b>Ingest Location:</b> ");
			text.append(results.getString(3));
			text.append("</div></td></tr>\n");
			text.append("<tr><td><div class=\"floatLeft\"><b>Ingest Type:</b> ");
			IngestTypeBean ingestType = ingestTypes.get(results.getInt(2));
			if (ingestType == null) { text.append("Unknown"); }
			else { text.append(ingestType.getName()); }
			text.append("</div></td></tr>\n");
			text.append("<tr><td><div class=\"floatRight\"><b>Ingest Status:</b> ");
			StatusBean ingestStatus = statuses.get(results.getInt(5));
			if (ingestStatus == null) { text.append("No Status"); }
			else { text.append("<span style=\"").append(ingestStatus.getStyle()).append("\">").append(ingestStatus.getName()).append("</span>"); }
			text.append("</div><div class=\"floatLeft\"><b>Ingest Contact:</b> ");
			UserBean ingestContact = users.get(results.getInt(4));
			if (ingestContact != null) { text.append(ingestContact.getPersonName()); }
			text.append("</div></td></tr>\n</table>\n");

			// Build the table to display the load information.
			text.append("<table class=\"info\">\n");
			text.append("<tr><td><div class=\"floatLeft\"><b>Data to be Archived:</b> ");
			text.append(results.getString(6));
			text.append("</div></td></tr>\n");
			text.append("<tr><td><div class=\"floatLeft\"><b>Archive Location:</b> ");
			text.append(results.getString(7));
			text.append("</div></td></tr>\n");
			text.append("<tr><td><div class=\"floatRight\"><b>Readme Complete:</b> ");
			text.append("<span class=\"").append(results.getBoolean(10) ? "yes" : "no").append("\">");
			text.append(results.getBoolean(10) ? "YES" : "NO");
			text.append("</span></div></td></tr>");
			text.append("<tr><td><div class=\"floatRight\"><b>Load Status:</b> ");
			StatusBean loadStatus = statuses.get(results.getInt(9));
			if (loadStatus == null) { text.append("No Status"); }
			else { text.append("<span style=\"").append(loadStatus.getStyle()).append("\">").append(loadStatus.getName()).append("</span>"); }
			text.append("</div><div class=\"floatLeft\"><b>Load Contact:</b> ");
			UserBean loadContact = users.get(results.getInt(8));
			if (loadContact != null) { text.append(loadContact.getPersonName()); }
			text.append("</div></td></tr>\n</table>\n");

			// Build the table to display the check task information.
			text.append("<table class=\"info\">\n");
			text.append("<tr><td><div class=\"floatRight\"><b>Check Status:</b> ");
			StatusBean checkStatus = statuses.get(results.getInt(12));
			if (checkStatus == null) { text.append("No Status"); }
			else { text.append("<span style=\"").append(checkStatus.getStyle()).append("\">").append(checkStatus.getName()).append("</span>"); }
			text.append("</div><div class=\"floatLeft\"><b>Check Contact:</b> ");
			UserBean checkContact = users.get(results.getInt(11));
			if (checkContact != null) { text.append(checkContact.getPersonName()); }
			text.append("</div></td></tr>\n</table>\n");

			
			note.setNoteText(text.toString());
			note.setReviseTime(new Timestamp(findMaxTimestamp(results.getTimestamp(13), results.getTimestamp(14), results.getTimestamp(14)).getTime() - 1000));
			
			notes.add(note);
		}
		
		// Put the notes in the map into the data set beans note store.
		notes.addAll(noteMap.values());
	}
	
	/**
	 * Load the platforms associated to this data set from the database.
	 * @param connection The connection to use to load the platforms.
	 * @throws SQLException if there is a problem reading the platforms from the database.
	 */
	private void loadPlatforms(Connection connection) throws SQLException {
		String sql = "SELECT platform_id FROM dataset_platform WHERE dataset_id=?";
		PreparedStatement stmt = connection.prepareStatement(sql);
		stmt.setString(1, getDatasetId());
		ResultSet results = stmt.executeQuery();
		while (results.next()) {
			addPlatform(results.getInt(1));
		}
		try { results.close(); } catch (SQLException e) {}
		try { stmt.close(); } catch (SQLException e) {}
	}
	
	/**
	 * Load the process task information for the specified data set.
	 * @param connection The connection to use to read from the database.
	 * @param datasetId The ID of the data set to be loaded.
	 * @param project The project the processed data set is being loaded for.
	 * @throws SQLException if there is a problem reading from the database.
	 */
	private void loadProcessDataset(Connection connection, String datasetId, ProjectBean project) throws SQLException {
		String sql = "SELECT name, question_flag, MAX(entry_date), internal_contact_id, readme_url FROM dataset JOIN dataset_process ON dataset.dataset_id=dataset_process.dataset_id WHERE dataset.dataset_id=? GROUP BY (dataset.dataset_id)";
		PreparedStatement stmt = connection.prepareStatement(sql);
		stmt.setString(1, datasetId);
		ResultSet results = stmt.executeQuery();

		// Load the basic data set information.
		if (results.next()) {
			setDatasetId(datasetId);
			setOriginalDatasetId(datasetId);
			setName(results.getString(1));
			setQuestions(results.getBoolean(2));
			setEntryDate(results.getTimestamp(3));
			setInternalContact(results.getInt(4));
			setReadmeURL(results.getString(5));
		}

		try { results.close(); } catch (SQLException e) {}
		try { stmt.close(); } catch (SQLException e) {}

		// Load the most recent process task information for the dataset.
		sql = "SELECT process_contact_id, process_status_id, work_location, final_data_location, station_list_location, howto_url, plot_url FROM dataset_process WHERE dataset_id=? AND entry_date=?";
		stmt = connection.prepareStatement(sql);
		stmt.setString(1, getDatasetId());
		stmt.setTimestamp(2, getEntryDate());
		results = stmt.executeQuery();

		if (results.next()) {
			setProcesser(results.getInt(1));
			setProcessStatus(results.getInt(2));
			setWorkLocation(results.getString(3));
			setFinalProcessedDataLocation(results.getString(4));
			setStationListFile(results.getString(5));
			setHowtoURL(results.getString(6));
			setPlotsURL(results.getString(7));
		}

		try { results.close(); } catch (SQLException e) {}
		try { stmt.close(); } catch (SQLException e) {}

		// Load the rest of the process data set information.
		SoftwareBean.load(connection, this);
		if (project == null) { loadProducts(connection); }
		else { loadProducts(connection, project); }
		loadSourceDatasets(connection, new TreeMap<String, DatasetBean>());
		loadProcessNotes(connection);
		loadPlatforms(connection);
	}
			
	/**
	 * Load the process task information for the specified data set.
	 * @param datasetId The ID of the data set to be loaded.
	 * @throws SQLException if there is a problem reading the database.
	 */
	public void loadProcessDataset(String datasetId) throws SQLException {
		Connection connection = getConnection();
		
		try { loadProcessDataset(connection, datasetId, null); }
		finally {
			try { connection.close(); } catch (SQLException e) {}
		}
	}

	/**
	 * Load the process task information for the specified data set and project.
	 * @param datasetId The id of the data set to be loaded.
         * @param project The project the data set is being loaded for.
	 * @throws SQLException if there is a problem reading the database.
	 */
	public void loadProcessDataset(String datasetId, ProjectBean project) throws SQLException {
		Connection connection = getConnection();
		try { loadProcessDataset(connection, datasetId, project); }
		finally {
			try { connection.close(); } catch (SQLException e) {}
		}
	}
	
	/**
	 * Get a list of processed data sets assigned to the specified project.
	 * @param project The project searching for its processed data set lists.
	 * @return The list of processed data sets in the specified project.
	 * @throws SQLException if there is a problem reading from the database.
	 */
	public static List<DatasetBean> loadProcessedDatasets(ProjectBean project) throws SQLException {
		Map<String, DatasetBean> datasets = new TreeMap<String, DatasetBean>();
		
		Connection connection = getConnection();
		try {
			// Determine which datasets are processed data sets for the project.
			String sql = "SELECT DISTINCT(dataset_process.dataset_id), MAX(entry_date) FROM dataset_process JOIN dataset_project ON dataset_process.dataset_id=dataset_project.dataset_id WHERE project_id=? GROUP BY dataset_process.dataset_id";
			PreparedStatement stmt = connection.prepareStatement(sql);
			stmt.setString(1, project.getProjectId());
			ResultSet results = stmt.executeQuery();

			// Define the statement to find the most recent information for the data set.
			String sql2 = "SELECT dataset.dataset_id, name, question_flag, process_contact_id, process_status_id FROM dataset JOIN dataset_process ON dataset.dataset_id=dataset_process.dataset_id WHERE dataset.dataset_id=? AND entry_date=?";
                        PreparedStatement stmt2 = connection.prepareStatement(sql2);

			while (results.next()) {
				// Load the most recent process task information for the data set.
				stmt2.setString(1, results.getString(1));
				stmt2.setTimestamp(2, results.getTimestamp(2));
				ResultSet results2 = stmt2.executeQuery();
				if (results2.next()) {

					DatasetBean dataset = new DatasetBean();
					dataset.setDatasetId(results2.getString(1));
					dataset.setName(results2.getString(2));
				
					if (!datasets.containsKey(dataset.getDatasetId())){
						datasets.put(dataset.getDatasetId(), dataset);
					}
								
					// Assign the values to the entry in the map.
					datasets.get(dataset.getDatasetId()).setQuestions(results2.getBoolean(3));
					datasets.get(dataset.getDatasetId()).setProcesser(results2.getInt(4));
					datasets.get(dataset.getDatasetId()).setProcessStatus(results2.getInt(5));
					datasets.get(dataset.getDatasetId()).loadSourceDatasets(connection, datasets);
				
					// Load the products of this data set.
					datasets.get(dataset.getDatasetId()).loadProducts(connection, datasets);
					dataset.setProcessedDataset(true);
				}
			
				try { results2.close(); } catch (SQLException e) {}
			}			
			try { stmt2.close(); } catch (SQLException e) {}
			try { results.close(); } catch (SQLException e) {}
			try { stmt.close(); } catch (SQLException e) {}
		} finally {
			try { connection.close(); } catch (SQLException e) {}
		}
		
		return new ArrayList<DatasetBean>(datasets.values());
	}

	/**
	 * Get the list of notes in the database related to the process task.
	 * @param connection The connection to use to read the notes.
	 * @throws SQLException if there is a problem reading from the database.
	 */
	private void loadProcessNotes(Connection connection) throws SQLException {
		Map<Integer, NoteBean> noteMap = new TreeMap<Integer, NoteBean>();
		// Load the note types from the database.
		Map<Integer, NoteTypeBean> typeMap = NoteTypeBean.getNoteTypesMap(connection);
		Map<Integer, UserBean> users = (new UserBean()).getAllUsersMap(connection);
		Map<Integer, StatusBean> statuses = StatusBean.getAllStatusesMap(connection);
		
		// Define the statement to load the notes for the data set.
		String sql = "SELECT note.note_id, note_type_id, author_id, entry_date, note_text, note.row_revise_time FROM note JOIN dataset_note ON (note.note_id=dataset_note.note_id) WHERE dataset_id=?";
		PreparedStatement stmt = connection.prepareStatement(sql);
		stmt.setString(1, getDatasetId());
		ResultSet results = stmt.executeQuery();
		// Loop through each note/note_type pair.
		while (results.next()) {
			Integer noteId = results.getInt(1);
			// If the note already exists, only need to add the new note type.
			if (noteMap.containsKey(noteId)) {
				noteMap.get(noteId).addNoteType(typeMap.get(results.getInt(2)));
			} 
			// Note doesn't exist, so create it an add it to the map.
			else {
				NoteBean note = new NoteBean(noteId);
				note.addNoteType(typeMap.get(results.getInt(2)));
				note.setAuthor(results.getInt(3));
				note.setEntryDate(results.getTimestamp(4));
				note.setNoteText(results.getString(5));
				note.setReviseTime(results.getTimestamp(6));
				noteMap.put(note.getNoteId(), note);
			}
		}
		// Close the open database streams.
		try { results.close(); } catch (SQLException e) {}
		try { stmt.close(); } catch (SQLException e) {}

		// Put the notes in the map into the data set beans note store.
		for (NoteBean note: noteMap.values()) {
			if (note.isProcessNote(typeMap)) {
				processNotes.add(note);
			}
		}
		
		
		// Define the statement to read the version information from the database.
		sql = "SELECT entry_date, process_contact_id, process_status_id, work_location, final_data_location, station_list_location, plot_url, howto_url, row_revise_time FROM dataset_process WHERE dataset_id=?";
		stmt = connection.prepareStatement(sql);
		stmt.setString(1, getDatasetId());
		results = stmt.executeQuery();
		
		while (results.next()) {
			NoteBean note = new NoteBean();
			// Need to subtract a second so the version will not collide with a note that may
			// have been entered when the version was created.  This will place the version note
			// earlier than the actual note so the note will be displayed in the correct relation
			// to the version.
			note.setEntryDate(new Timestamp(results.getTimestamp(1).getTime() - 1000));
			
			StringBuffer text = new StringBuffer();
			

			// Build the table to display the load information.
			text.append("<table class=\"info\">\n");
			text.append("<tr><td><div class=\"floatRight\"><b>Process Status:</b> ");
			StatusBean loadStatus = statuses.get(results.getInt(3));
			if (loadStatus == null) { text.append("No Status"); }
			else { text.append("<span style=\"").append(loadStatus.getStyle()).append("\">").append(loadStatus.getName()).append("</span>"); }
			text.append("</div><div class=\"floatLeft\"><b>Process Contact:</b> ");
			UserBean procContact = users.get(results.getInt(2));
			if (procContact != null) { text.append(procContact.getPersonName()); }
			text.append("<tr><td><div class=\"floatLeft\"><b>Work Location:</b> ");
			text.append(results.getString(4));
			text.append("</div></td></tr>\n");
			text.append("<tr><td><div class=\"floatLeft\"><b>Final Data Location:</b> ");
			text.append(results.getString(5));
			text.append("</div></td></tr>\n");
			text.append("<tr><td><div class=\"floatLeft\"><b>Station List Location:</b> ");
			text.append(results.getString(6));
			text.append("</div></td></tr>\n");
			text.append("<tr><td><div class=\"floatLeft\"><b>How To:</b> ");
			text.append(results.getString(8));
			text.append("</div></td></tr>\n");
			text.append("<tr><td><div class=\"floatLeft\"><b>Plots:</b> ");
			text.append(results.getString(7));
			text.append("</div></td></tr>\n");
			text.append("</div></td></tr>\n</table>\n");

			
			note.setNoteText(text.toString());
			note.setReviseTime(new Timestamp(results.getTimestamp(9).getTime() - 1000));
			
			processNotes.add(note);
		}		
	}
	
	/**
	 * Load the product data sets this processed data set is a source data set for.
	 * @param connection The connection to use to read from the database.
	 * @throws SQLException if there is a problem reading from the database.
	 */
	private void loadProducts(Connection connection) throws SQLException {
		String sql = "SELECT dataset.dataset_id, name FROM dataset JOIN dataset_source_dataset ON dataset.dataset_id=dataset_source_dataset.dataset_id WHERE source_dataset_id=?";
		PreparedStatement stmt = connection.prepareStatement(sql);
		stmt.setString(1, getDatasetId());
		ResultSet results = stmt.executeQuery();
		while (results.next()) {
			DatasetBean dataset = new DatasetBean();
			dataset.setDatasetId(results.getString(1));
			dataset.setName(results.getString(2));
			addProduct(dataset);
		}
		try { results.close(); } catch (SQLException e) {}
		try { stmt.close(); } catch (SQLException e) {}
	}

	private void loadProducts(Connection connection, ProjectBean project) throws SQLException {
		String sql = "SELECT dataset.dataset_id, name FROM dataset JOIN dataset_source_dataset ON dataset.dataset_id=dataset_source_dataset.dataset_id JOIN dataset_project ON dataset.dataset_id=dataset_project.dataset_id WHERE source_dataset_id=? AND project_id=?";
		PreparedStatement stmt = connection.prepareStatement(sql);
		stmt.setString(1, getDatasetId());
		stmt.setString(2, project.getProjectId());
		ResultSet results = stmt.executeQuery();
		while (results.next()) {
			DatasetBean dataset = new DatasetBean();
			dataset.setDatasetId(results.getString(1));
			dataset.setName(results.getString(2));
			addProduct(dataset);
		}
		try { results.close(); } catch (SQLException e) {}
		try { stmt.close(); } catch (SQLException e) {}
	}
	
	/**
	 * Load the product data sets this processed data set is a source data set for.
	 * @param connection The connection to use to read from the database.
	 * @param datasets A mapping of known product data sets.
	 * @throws SQLException if there is a problem reading from the database.
	 */
	private void loadProducts(Connection connection, Map<String, DatasetBean> datasets) throws SQLException {
		String sql = "SELECT dataset.dataset_id, name FROM dataset JOIN dataset_source_dataset ON dataset.dataset_id=dataset_source_dataset.dataset_id WHERE source_dataset_id=?";
		PreparedStatement stmt = connection.prepareStatement(sql);
		stmt.setString(1, getDatasetId());
		ResultSet results = stmt.executeQuery();
		while (results.next()) {
			if (!datasets.containsKey(results.getString(1))) {
				DatasetBean dataset = new DatasetBean();
				dataset.setDatasetId(results.getString(1));
				dataset.setName(results.getString(2));
				datasets.put(dataset.getDatasetId(), dataset);
			}
			addProduct(datasets.get(results.getString(1)));
		}
		try { results.close(); } catch (SQLException e) {}
		try { stmt.close(); } catch (SQLException e) {}
	}
	
	/**
	 * Load the source information for this data set unique to the specified product.
	 * @param datasetId The data set ID of the source data set.
	 * @param productDatasetId The data set ID of the product data set.
	 * @throws SQLException if there is a problem reading from the database.
	 */
	public void loadSourceDataset(String datasetId, String productDatasetId) throws SQLException {
		Connection connection = getConnection();
		String sql = "SELECT name, readme_url, data_directory, exclude_flag, utc_offset, dst_flag, collection_time_note_id, source_info_note_id FROM dataset_source_dataset JOIN dataset_source_info ON dataset_source_dataset.source_dataset_id=dataset_source_info.dataset_id JOIN dataset ON dataset_source_dataset.source_dataset_id=dataset.dataset_id WHERE dataset_source_dataset.dataset_id=? AND dataset_source_dataset.source_dataset_id=?";
		try {
			PreparedStatement stmt = connection.prepareStatement(sql);
			stmt.setString(1, productDatasetId);
			stmt.setString(2, datasetId);
			ResultSet results = stmt.executeQuery();
			if (results.next()) {
				setDatasetId(datasetId);
				setSelectedProduct(productDatasetId);
				setName(results.getString(1));
				setReadmeURL(results.getString(2));
				setDataDirectory(productDatasetId, results.getString(3));
				setExcluded(productDatasetId, results.getBoolean(4));
				setUtcOffset(results.getString(5));
				if (results.getString(6) != null) {
					setDSTFlag(results.getBoolean(6));
				}
				setCollectionTimeNote(NoteBean.load(connection, results.getInt(7)));
				setSourceInfoNote(NoteBean.load(connection, results.getInt(8)));
				
				loadPlatforms(connection);
			}
			
			try { results.close(); } catch (SQLException e) {}
			try { stmt.close(); } catch (SQLException e) {}
		} finally {
			try { connection.close(); } catch (SQLException e) {}
		}
	}
	
	/**
	 * Load the source data sets for this data set and update the values in the specified data set mapping.
	 * This is recursive and will load all source information down the product tree.
	 * @param connection The connection to use to read from the database.
	 * @param datasets The mapping containing the source data set information.
	 * @throws SQLException if there is a problem reading from the database.
	 */
	private void loadSourceDatasets(Connection connection, Map<String, DatasetBean> datasets) throws SQLException {
		String sql = "SELECT source_dataset_id, name, readme_url, data_directory, exclude_flag, utc_offset, dst_flag, collection_time_note_id, source_info_note_id, question_flag FROM dataset_source_dataset JOIN dataset ON dataset_source_dataset.source_dataset_id=dataset.dataset_id JOIN dataset_source_info ON dataset_source_dataset.source_dataset_id=dataset_source_info.dataset_id WHERE dataset_source_dataset.dataset_id=?";
		PreparedStatement stmt = connection.prepareStatement(sql);
		stmt.setString(1, getDatasetId());
		ResultSet results = stmt.executeQuery();

		while (results.next()) {
			if (!datasets.containsKey(results.getString(1))) {
				DatasetBean dataset = new DatasetBean();
				dataset.setDatasetId(results.getString(1));
				dataset.setOriginalDatasetId(results.getString(1));
				dataset.setName(results.getString(2));
				datasets.put(dataset.getDatasetId(), dataset);
				
				dataset.loadSourceDatasets(connection, datasets);
			}
			
			DatasetBean dataset = datasets.get(results.getString(1));
			addSourceDataset(dataset);
			dataset.setReadmeURL(results.getString(3));
			setDataDirectory(dataset.getDatasetId(), results.getString(4));
			setExcluded(dataset.getDatasetId(), results.getBoolean(5));
			dataset.setUtcOffset(results.getString(6));
			if (results.getString(7) != null) {
				dataset.setDSTFlag(results.getBoolean(7));
			}
			dataset.setCollectionTimeNote(NoteBean.load(connection, results.getInt(8)));
			dataset.setSourceInfoNote(NoteBean.load(connection, results.getInt(9)));
			dataset.setQuestions(results.getBoolean(10));
			
			dataset.loadPlatforms(connection);
		}
		
		try { results.close(); } catch (SQLException e) {}
		try { stmt.close(); } catch (SQLException e) {}
	}
	
	/**
	 * Regenerate the list of platforms for the specified product.
	 * @param connection The connection to use to update the database.
	 * @param productId The data set ID of the product to be updated.
	 * @throws SQLException when there is a problem updating the database.
	 */
	private void regeneratePlatforms(Connection connection, String productId) throws SQLException {
		// Load the product from the database and clear its current platforms.
		DatasetBean product = new DatasetBean();
		product.loadProcessDataset(connection, productId, null);
		product.setPlatformString("");

		// Assign the platforms from the platforms of it's source data set(s).
		for (DatasetBean source: product.getSourceDatasets()) {
			if (!product.isExcluded(source.getDatasetId())) {
				for (Integer platform: source.getPlatforms()) {
					product.addPlatform(platform);
				}
			}
		}
		
		// Update the database.
		product.updatePlatforms(connection);
	}
	
	/**
	 * Remove this data set as a source of the currently selected product data set.  This updates the
	 * database.
	 * @throws SQLException if there is a problem updating the database.
	 */
	public void removeSource() throws SQLException {
		Connection connection = getConnection();
		try {
			// Turn on transaction processing.
			connection.setAutoCommit(false);
			
			// Delete the source association from the product.
			String sql = "DELETE FROM dataset_source_dataset WHERE dataset_id=? AND source_dataset_id=?";
			PreparedStatement stmt = connection.prepareStatement(sql);
			stmt.setString(1, getSelectedProduct());
			stmt.setString(2, getDatasetId());
			stmt.execute();
			try { stmt.close(); } catch (SQLException e) {}

			// Update the product's platforms now that the source is no longer a source.
			regeneratePlatforms(connection, getSelectedProduct());

			// Save the commands to the database.
			try { connection.commit(); }
			catch (SQLException e) {
				try { connection.rollback(); }
				catch (SQLException e1) {}
				throw e;
			}
		} finally {
			try { connection.close(); } catch (SQLException e) {}
		}
	}
	
	/**
	 * Set the location where the data is archived for the data set.
	 * @param archive The data set's archive location.
	 */
	public void setArchiveLocation(String archive) { archiveLocation = archive; }
	
	/**
	 * Set the contact tasked with checking this data set.
	 * @param checker The database ID of the check contact.
	 */
	public void setChecker(int checker) { this.checker = checker; }
	
	/**
	 * Set the status of the check task.
	 * @param status The database ID of the check status.
	 */
	public void setCheckStatus(int status) { checkStatus = status; }
	
	/**
	 * Set the note that contains the information on the data set's collection time.
	 * @param note The data set's collection time note.
	 */
	public void setCollectionTimeNote(NoteBean note) { collectionTimeNote = note; }
	
	/**
	 * Set the author of the collection time note.
	 * @param author The DTS ID of the author.
	 */
	public void setCollectionTimeNoteAuthor(int author) { getCollectionTimeNote().setAuthor(author); }
	
	/**
	 * Set the text of the collection time note.
	 * @param text The content of the note.
	 */
	public void setCollectionTimeNoteText(String text) { getCollectionTimeNote().setNoteText(text.trim()); }
	
	/**
	 * Set the location of the source data for the source data set.
	 * @param datasetId The data set ID for the source data set.
	 * @param directory The source data set's data directory.
	 */
	public void setDataDirectory(String datasetId, String directory) {
		sourceDataDirectory.put(datasetId, directory);
	}
	
	/**
	 * Set the directory where the source data can be found for the currently selected product.
	 * @param directory The directory of the source data.
	 */
	public void setDataLocation(String directory) { setDataDirectory(getSelectedProduct(), directory); }
	
	/**
	 * Set the time the data set was entered into the database.
	 * @param date The creation time of the data set.
	 */
	public void setDatasetEntryDate(String date) {
		setEntryDate(Timestamp.valueOf(date));
	}
	
	/**
	 * Set the data set ID for the data set.
	 * @param datasetId The data set's new data set ID.
	 */
	public void setDatasetId(String datasetId) { this.datasetId = datasetId; }
	
	/**
	 * Set a single project to be disassociated with the data set.
	 * @param project The ID of the project to be disassociated with the data set.
	 */
	public void setDeletedProjects(String project) {
		deletedProjects.clear();
		deletedProjects.add(project);
	}

	/**
	 * Set the projects to be disassociated with the data set.
	 * @param projects The IDs of the projects to be disassociated with the data set.
	 */
	public void setDeletedProjects(String[] projects) {
		deletedProjects.clear();
		deletedProjects.addAll(Arrays.asList(projects));
	}
	
	/**
	 * Mark the data set as having documentation.
	 * @param flag <code>true</code> if the data set has documentation to be loaded,
	 * <code>false</code> if it does not.
	 */
	public void setDocumented(boolean flag) { docFlag = flag; }

	/**
	 * Set the flag marking the data set as using Daylight Savings Time.
	 * @param flag The Daylight Savings Time flag.
	 */
	public void setDSTFlag(Boolean flag) { dstFlag = flag; }

	/**
	 * Set the flag marking the data set as using Daylight Savings Time.
	 * @param flag The Daylight Saving Time flag as a string.
	 */
	public void setDstFlagString(String flag) {
		if (flag.trim().equals("null")) { setDSTFlag(null); }
		else { setDSTFlag(Boolean.parseBoolean(flag)); }
	}
	
	/**
	 * Set the time the data set was entered into the database.
	 * @param date The creation time of the data set.
	 */
	public void setEntryDate(Timestamp date) { this.entryDate = date; }
	
	/**
	 * Set the flag for marking this source data set as excluded from the currently selected product.
	 * @param flag The exclusion flag.
	 */
	public void setExcluded(boolean flag) { setExcluded(getSelectedProduct(), flag); }
	
	/**
	 * Set the exclusion flag for a source data set of this data set.
	 * @param datasetId The id of the source data set.
	 * @param flag The exclusion flag for the source data set.
	 */
	public void setExcluded(String datasetId, boolean flag) {
		if (flag) { excludedSources.add(datasetId); }
		else { excludedSources.remove(datasetId); }
	}
	
	/**
	 * Set the source contact for this data set.
	 * @param extContact The database ID of the source contact.
	 */
	public void setExternalContact(int extContact) { this.extContact = extContact; }
	
	/**
	 * Set the location where the final processed data can be found.
	 * @param dir The final data directory.
	 */
	public void setFinalProcessedDataLocation(String dir) { finalDataLocation = dir; }
	
	/**
	 * Set the location of the how to document for the data set.
	 * @param url The how to URL.
	 */
	public void setHowtoURL(String url) { howtoURL = url; }
	
	/**
	 * Set the contact tasked with ingesting this data set.
	 * @param ingester The database ID of the ingest contact.
	 */
	public void setIngester(int ingester) { this.ingester = ingester; }
	
	/**
	 * Set the directory where the data is to be ingested for this data set.
	 * @param ingest The data set's ingest directory.
	 */
	public void setIngestLocation(String ingest) { ingestLocation = ingest; }
	
	/**
	 * Set the status of the ingest task.
	 * @param status The database ID of the ingest status.
	 */
	public void setIngestStatus(int status) { ingestStatus = status; }
	
	/**
	 * Set the ingest type for the data set.
	 * @param type The database ID of the ingest type.
	 */
	public void setIngestType(int type) { ingestType = type; }
	
	/**
	 * Mark the data set as being included in a project's master list.
	 * @param projectId The ID of the project associated to the data set.
	 * @param flag The flag to mark the data set as being in the project's master list.
	 */
	public void setInMasterList(String projectId, boolean flag) {
		if (flag && !masterListFlags.contains(projectId)) { masterListFlags.add(projectId); }
		else if (!flag) { masterListFlags.remove(projectId); }
	}
	
	/**
	 * Set the internal contact for this data set.
	 * @param intContact The database ID of the internal contact.
	 */
	public void setInternalContact(int intContact) { this.intContact = intContact; }
	
	/**
	 * Set the flag that marks this data set as a source data set.
	 * @param flag The source flag for the data set.
	 */
	public void setIsSource(boolean flag) { isSource = flag; }
	
	/**
	 * Set the index of this data set in the display view.
	 * @param index The index of the data set in the display list.
	 */
	private void setListIndex(int index) { this.listIndex = index; }
	
	/**
	 * Set the directory where the data to be loaded for the data set is located.
	 * @param load The directory of the data to be loaded.
	 */
	public void setLoadDataLocation(String load) { loadDataLocation = load; }
	
	/**
	 * Set the contact tasked with loading this data set.
	 * @param loader The database ID of the load contact.
	 */
	public void setLoader(int loader) { this.loader = loader; }
	
	/**
	 * Set the status of the load task.
	 * @param status The database ID of the load status.
	 */
	public void setLoadStatus(int status) { loadStatus = status; }

	/**
	 * Set the data set as included in the specified projects.
	 * @param projects The IDs of the projects that have the data set in their master lists.
	 */
	public void setMasterListFlags(String[] projects) {
		masterListFlags.clear();
		masterListFlags.addAll(Arrays.asList(projects));
	}
	
	/**
	 * Set the name/title of the data set.
	 * @param name The data set's name.
	 */
	public void setName(String name) { this.name = name; }
	
	/**
	 * Set the data set that is next in the display list.
	 * @param next The next data set in the display list.
	 */
	private void setNextDataset(DatasetBean next) { this.next = next; }
	
	/**
	 * Set the unchanged data set ID from the database.
	 * @param datasetId The data set ID.
	 */
	public void setOriginalDatasetId(String datasetId) { originalId = datasetId; }
	
	/**
	 * Set the platforms attached to attach to this data set as a semicolon delimited list.
	 * @param items The list of semicolon delimited platform IDs.
	 */
	public void setPlatformString(String items) {
		platforms.clear();
		if (items.trim().length() > 0) {
			for (String item: items.split(";")) {
				addPlatform(Integer.parseInt(item.trim()));
			}
		}
	}
	
	/**
	 * Set the URL where the plots for the data set can be found.
	 * @param url The data set plot URL.
	 */
	public void setPlotsURL(String url) { plotURL = url; }

	/**
	 * Set the data set that is before this data set in the display list.
	 * @param previous The previous data set in the list.
	 */
	private void setPreviousDataset(DatasetBean previous) { this.previous = previous; }
	
	/**
	 * Mark this data set as a processed data set.
	 * @param flag <code>true</code> if the data set is a processed data set, <code>false</code>
	 * if is not.
	 */
	private void setProcessedDataset(boolean flag) { isProcessed = flag; }
	
	/**
	 * Set the contact tasked with processing this data set.
	 * @param processer The database ID of the process contact.
	 */
	public void setProcesser(int processer) { this.processer = processer; }
	
	/**
	 * Set the status of the process task.
	 * @param status The database ID of the process status.
	 */
	public void setProcessStatus(int status) { processStatus = status; }
	
	/**
	 * Set the specified projects as being associated with the data set.
	 * @param projects The projects to associate with the data set.
	 */
	public void setProject(String[] projects) {
		for (String project: projects) { 
			if (project != null && !project.trim().equalsIgnoreCase("null")) {
				addProject(project.trim());
			}
		}
	}
	
	/**
	 * Mark the data set as having outstanding questions/issues.
	 * @param questions The flag to mark the data set as having issues.
	 */
	public void setQuestions(boolean questions) { this.questions = questions; }
	
	/**
	 * Set the readme URL for the data set.
	 * @param url The data set's readme URL.
	 */
	public void setReadmeURL(String url) { readmeURL = url; }

	/**
	 * Set the remote URL for the data set.
	 * @param url The data set's remote URL.
	 */
	public void setRemoteURL(String url) { remoteURL = url; }
	
	/**
	 * Set the product currently being worked with for this source data set.
	 * @param productId The data set ID for the selected product.
	 */
	public void setSelectedProduct(String productId) { selectedProductId = productId; }
	
	/**
	 * Set the note that contains information on the source of this data set.
	 * @param note The source information note for the data set.
	 */
	public void setSourceInfoNote(NoteBean note) { sourceInfoNote = note; }
	
	/**
	 * Set the author of the source info note.
	 * @param author The DTS ID of the author.
	 */
	public void setSourceInfoNoteAuthor(int author) { getSourceInfoNote().setAuthor(author); }
	
	/**
	 * Set the text of the source info note.
	 * @param text The content of the note.
	 */
	public void setSourceInfoNoteText(String text) { getSourceInfoNote().setNoteText(text.trim()); }
	
	/**
	 * Set the location of the station list file for the data set.
	 * @param file The station list file location.
	 */
	public void setStationListFile(String file) { stationListFile = file; }
	
	/**
	 * Set the offset from UTC for the data in this data set.
	 * @param offset The UTC offset for the data set's data.
	 */
	public void setUtcOffset(String offset) { utcOffset = offset; }
	
	/**
	 * Set the directory where the processing work was done.
	 * @param dir The processing work directory.
	 */
	public void setWorkLocation(String dir) { workLocation = dir; }
	
	/**
	 * Update the data set in the database with the new note.
	 * @param note The note to be added to the data set.
	 * @throws SQLException if there is a problem updating the data set.
	 */
	public void update(NoteBean note) throws SQLException {
		// Define the connection to the database.
		Connection connection = getConnection();
		
		try {
			connection.setAutoCommit(false);

			// Determine if the data set ID needs to be converted to a DTS ID.
			determineDatasetId(connection);

			// Define the statement to update the data set.
			String sql = "UPDATE dataset SET dataset_id=?, name=?, source_contact_id=?, internal_contact_id=?, question_flag=?, remote_url=?, readme_url=? WHERE dataset_id=?";
			PreparedStatement datasetStmt = connection.prepareStatement(sql);
			datasetStmt.setString(1, getDatasetId());
			datasetStmt.setString(2, getName());
			if (getExternalContact() <= 0) {
				datasetStmt.setNull(3, Types.INTEGER);
			} else {
				datasetStmt.setInt(3, getExternalContact());
			}
			datasetStmt.setInt(4, getInternalContact());
			datasetStmt.setBoolean(5, hasQuestions());
			datasetStmt.setString(6, getRemoteURL());
			datasetStmt.setString(7, getReadmeURL());
			datasetStmt.setString(8, getOriginalDatasetId());
			datasetStmt.execute();
			// Close the open database streams.
			try { datasetStmt.close(); } catch (SQLException e) {}
		
			// Update the ingest task data.
			sql = "UPDATE dataset_ingest SET ingest_type_id=?, ingest_location=?, ingest_contact_id=?, ingest_status_id=? WHERE dataset_id=? AND entry_date=?";
			PreparedStatement ingestStmt = connection.prepareStatement(sql);
			if (getIngestType() <= 0) {
				ingestStmt.setNull(1, Types.INTEGER);
			} else {
				ingestStmt.setInt(1, getIngestType());
			}
			ingestStmt.setString(2, getIngestLocation());
			if (getIngester() <= 0) {
				ingestStmt.setNull(3, Types.INTEGER);
			} else {
				ingestStmt.setInt(3, getIngester());
			}
			if (getIngestStatus() <= 0) {
				ingestStmt.setNull(4, Types.INTEGER);
			} else {
				ingestStmt.setInt(4, getIngestStatus());
			}
			ingestStmt.setString(5, getDatasetId());
			ingestStmt.setTimestamp(6, getEntryDate());
			ingestStmt.execute();
			// Close the open database streams.
			try { ingestStmt.close(); } catch (SQLException e) {}

			// Update the load task data.
			sql = "UPDATE dataset_load SET load_data_location=?, archive_location=?, load_contact_id=?, load_status_id=?, documentation_flag=? WHERE dataset_id=? AND entry_date=?";
			PreparedStatement loadStmt = connection.prepareStatement(sql);
			loadStmt.setString(1, getLoadDataLocation());
			loadStmt.setString(2, getArchiveLocation());
			if (getLoader() <= 0) {
				loadStmt.setNull(3, Types.INTEGER);
			} else {
				loadStmt.setInt(3, getLoader());
			}
			if (getLoadStatus() <= 0) {
				loadStmt.setNull(4, Types.INTEGER);
			} else {
				loadStmt.setInt(4, getLoadStatus());
			}
			loadStmt.setBoolean(5, isDocumented());
			loadStmt.setString(6, getDatasetId());
			loadStmt.setTimestamp(7, getEntryDate());
			loadStmt.execute();
			// Close the open database streams.
			try { loadStmt.close(); } catch (SQLException e) {}
		
			// Update the check task streams.
			sql = "UPDATE dataset_approve SET approve_contact_id=?, approve_status_id=? WHERE dataset_id=? AND entry_date=?";
			PreparedStatement approveStmt = connection.prepareStatement(sql);
			if (getChecker() <= 0) {
				approveStmt.setNull(1, Types.INTEGER);
			} else {
				approveStmt.setInt(1, getChecker());
			}
			if (getCheckStatus() <= 0) {
				approveStmt.setNull(2, Types.INTEGER);
			} else {
				approveStmt.setInt(2, getCheckStatus());
			}
			approveStmt.setString(3, getDatasetId());
			approveStmt.setTimestamp(4, getEntryDate());
			approveStmt.execute();
			// Close the open database streams.
			try { approveStmt.close(); } catch (SQLException e) {}

			// Delete all project associations.
			sql = "DELETE FROM dataset_project WHERE dataset_id=?";
			PreparedStatement delProjStmt = connection.prepareStatement(sql);
			delProjStmt.setString(1, getDatasetId());
			delProjStmt.execute();
			try { delProjStmt.close(); } catch (SQLException e) {}

			// Now insert all of the associations as new
			sql = "INSERT INTO dataset_project(dataset_id, project_id, master_list_flag) VALUES(?,?,?)";
			PreparedStatement projStmt = connection.prepareStatement(sql);
			projStmt.setString(1, getDatasetId());
			for (String projectId: getProjectsAsList()) {
				if (!isDeletedProject(projectId)) {
					projStmt.setString(2, projectId);
					projStmt.setBoolean(3, isInMasterList(projectId));
					projStmt.execute();
				}
			}
			try { projStmt.close(); } catch (SQLException e) {}

			// Add the note if there is a note to be added.
			if (note.getNoteText() != null && !note.getNoteText().trim().equals("")) {
				note.insert(connection, this);
			}
		
			// Save the commands to the database.
			try { connection.commit(); }
			catch (SQLException e) {
				try { connection.rollback(); }
				catch (SQLException e1) {}
				throw e;
			}
		} finally {
			try { connection.close(); } catch (SQLException e) {}
		}
	}
	
	/**
	 * Update the set of platforms associated with this data set.
	 * @param connection The connection to use to update the database.
	 * @throws SQLException when there is a problem updating the database.
	 */
	private void updatePlatforms(Connection connection) throws SQLException {
		// Remove all current values to ensure old values are removed.
		String sql = "DELETE FROM dataset_platform WHERE dataset_id=?";
		PreparedStatement stmt = connection.prepareStatement(sql);
		stmt.setString(1, getDatasetId());
		stmt.execute();
		try { stmt.close(); } catch (SQLException e) {}
		
		// Insert all values into the database.
		sql = "INSERT INTO dataset_platform(dataset_id, platform_id) VALUES(?, ?)";
		stmt = connection.prepareStatement(sql);
		stmt.setString(1, getDatasetId());
		for (Integer platform: getPlatforms()) {
			stmt.setInt(2, platform);
			stmt.execute();
		}
		try { stmt.close(); } catch (SQLException e) {}
		
		// Update the product platforms.
		updateProductPlatforms(connection, this);
	}
	
	/**
	 * Update the process task of the data set.
	 * @param note The note to include as part of the update.
	 * @throws SQLException when there is a problem trying to update the database.
	 */
	public void updateProcess(NoteBean note) throws SQLException {
		Connection connection = getConnection();
		
		try {
			// Turn on transaction processing.
			connection.setAutoCommit(false);
			
			// Update the base data set information.
			PreparedStatement dsStmt = connection.prepareStatement("UPDATE dataset SET question_flag=? WHERE dataset_id=?");
			dsStmt.setBoolean(1, hasQuestions());
			dsStmt.setString(2, getOriginalDatasetId());
			dsStmt.execute();
			try { dsStmt.close(); } catch (SQLException e) {}
			
			// Update the process task specific information.
			PreparedStatement procStmt = connection.prepareStatement("UPDATE dataset_process SET process_contact_id=?, process_status_id=?, work_location=?, final_data_location=?, station_list_location=?, howto_url=?, plot_url=? WHERE dataset_id=? AND entry_date=?");
			if (getProcesser() <= 0) { procStmt.setNull(1, Types.INTEGER); } else { procStmt.setInt(1, getProcesser()); }
			if (getProcessStatus() <= 0) { procStmt.setNull(2, Types.INTEGER); } else { procStmt.setInt(2, getProcessStatus()); }
			procStmt.setString(3, getWorkLocation());
			procStmt.setString(4, getFinalProcessedDataLocation());
			procStmt.setString(5, getStationListFile());
			procStmt.setString(6, getHowtoURL());
			procStmt.setString(7, getPlotsURL());
			procStmt.setString(8, getDatasetId());
			procStmt.setTimestamp(9, getEntryDate());
			procStmt.execute();
			try { procStmt.close(); } catch (SQLException e) {}
			
			// Add the note if there is a note to be added.
			if (note.getNoteText() != null && !note.getNoteText().trim().equals("")) {
				note.insert(connection, this);
			}
		
			// Save the commands to the database.
			try { connection.commit(); }
			catch (SQLException e) {
				try { connection.rollback(); } catch (SQLException e1) {}
				throw e;
			}
		} finally {
			try { connection.close(); } catch (SQLException e) {}
		}
	}
	
	/**
	 * Add a new process task version for the data set.
	 * @param note The note to be included as part of the updated.
	 * @throws SQLException when there is a problem updating the database.
	 */
	public void updateProcessWithNewVersion(NoteBean note) throws SQLException {
		Connection connection = getConnection();
		
		try {
			// Turn on transaction processing.
			connection.setAutoCommit(false);
			
			// Update the base data set information.
			PreparedStatement dsStmt = connection.prepareStatement("UPDATE dataset SET question_flag=? WHERE dataset_id=?");
			dsStmt.setBoolean(1, hasQuestions());
			dsStmt.setString(2, getOriginalDatasetId());
			dsStmt.execute();
			try { dsStmt.close(); } catch (SQLException e) {}
			
			// Create a new Timestamp.
			setEntryDate(getCurrentTime(connection));
			
			// Insert the new task information.
			PreparedStatement procStmt = connection.prepareStatement("INSERT INTO dataset_process(process_contact_id, process_status_id, work_location, final_data_location, station_list_location, howto_url, plot_url, dataset_id, entry_date) VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?)");
			if (getProcesser() <= 0) { procStmt.setNull(1, Types.INTEGER); } else { procStmt.setInt(1, getProcesser()); }
			if (getProcessStatus() <= 0) { procStmt.setNull(2, Types.INTEGER); } else { procStmt.setInt(2, getProcessStatus()); }
			procStmt.setString(3, getWorkLocation());
			procStmt.setString(4, getFinalProcessedDataLocation());
			procStmt.setString(5, getStationListFile());
			procStmt.setString(6, getHowtoURL());
			procStmt.setString(7, getPlotsURL());
			procStmt.setString(8, getDatasetId());
			procStmt.setTimestamp(9, getEntryDate());
			procStmt.execute();
			try { procStmt.close(); } catch (SQLException e) {}
			
			// Add the note if there is a note to be added.
			if (note.getNoteText() != null && !note.getNoteText().trim().equals("")) {
				note.insert(connection, this);
			}
		
			// Save the commands to the database.
			try { connection.commit(); }
			catch (SQLException e) {
				try { connection.rollback(); } catch (SQLException e1) {}
				throw e;
			}
		} finally {
			try { connection.close(); } catch (SQLException e) {}
		}
	}
	
	/**
	 * Update the platforms for the products of the specified data set.
	 * @param connection The connection to use to update the database.
	 * @param dataset The data set which products are to be updated.
	 * @throws SQLException if there is a problem updating the database.
	 */
	private void updateProductPlatforms(Connection connection, DatasetBean dataset) throws SQLException {
		// First find the products of this data set.
		dataset.loadProducts(connection);
		
		for (DatasetBean product: dataset.getProducts()) {
			// Need to get the information for the product first.
			product.loadProcessDataset(connection, product.getDatasetId(), null);
			// Clear the platforms for the product.
			product.setPlatformString("");
			
			// Add the platforms for all of its source data sets.
			for (DatasetBean source: product.getSourceDatasets()) {
				if (!product.isExcluded(source.getDatasetId())) {
					for (Integer platform: source.getPlatforms()) {
						product.addPlatform(platform);
					}
				}
			}
			
			// Update the database.
			product.updatePlatforms(connection);
		}
	}
	
	/**
	 * Update the source information in the database for this data set.
	 * @throws SQLException when there is a problem updating the database.
	 */
	public void updateSourceInfo() throws SQLException {
		Connection connection = getConnection();
		
		try {
			// Turn on transaction processing.
			connection.setAutoCommit(false);

			// Update the general source information.
			String sql = "UPDATE dataset_source_dataset SET data_directory=?, exclude_flag=? WHERE dataset_id=? AND source_dataset_id=?";
			PreparedStatement stmt = connection.prepareStatement(sql);
			stmt.setString(1, getDataLocation());
			stmt.setBoolean(2, isExcluded());
			stmt.setString(3, getSelectedProduct());
			stmt.setString(4, getDatasetId());
			stmt.execute();
			try { stmt.close(); } catch (SQLException e) {}
			
			// Update the product specific information.
			sql = "UPDATE dataset_source_info SET utc_offset=?, dst_flag=? WHERE dataset_id=?";
			stmt = connection.prepareStatement(sql);
			stmt.setString(1, getUtcOffset());
			if (isDSTFlag() == null) { stmt.setNull(2, Types.BOOLEAN); }	
			else { stmt.setBoolean(2, isDSTFlag()); }
			stmt.setString(3, getDatasetId());
			stmt.execute();
			try { stmt.close(); } catch (SQLException e) {}
			
			// Update the platforms.
			updatePlatforms(connection);
			
			// Update the contents of the notes.
			getCollectionTimeNote().update(connection);
			getSourceInfoNote().update(connection);
			
			// Save the commands to the database.
			try { connection.commit(); }
			catch (SQLException e) {
				try { connection.rollback(); } catch (SQLException e1) {}
				throw e;
			}	
		} finally {
			try { connection.close(); } catch (SQLException e) {}
		}
	}
	
	
	/**
	 * Update the data set in the database, but add the task information as a new version.
	 * @param note A note to associate with the data set.
	 * @throws SQLException if there is a problem updating the data set.
	 */
	public void updateWithNewVersion(NoteBean note) throws SQLException {
		// Define the connection to the database.
		Connection connection = getConnection();
		
		try {
			// Turn on transaction processing.
			connection.setAutoCommit(false);
		

			// Determine if the data set ID needs to be converted to a DTS ID.
			determineDatasetId(connection);
		
			// This will be the new date for the version.
			setEntryDate(getCurrentTime(connection));

			// Define the statement to update the data set.
			String sql = "UPDATE dataset SET dataset_id=?, name=?, source_contact_id=?, internal_contact_id=?, question_flag=?, remote_url=?, readme_url=? WHERE dataset_id=?";
			PreparedStatement datasetStmt = connection.prepareStatement(sql);
			datasetStmt.setString(1, getDatasetId());
			datasetStmt.setString(2, getName());
			if (getExternalContact() <= 0) {
				datasetStmt.setNull(3, Types.INTEGER);
			} else {
				datasetStmt.setInt(3, getExternalContact());
			}
			datasetStmt.setInt(4, getInternalContact());
			datasetStmt.setBoolean(5, hasQuestions());
			datasetStmt.setString(6, getRemoteURL());
			datasetStmt.setString(7, getReadmeURL());
			datasetStmt.setString(8, getOriginalDatasetId());
			datasetStmt.execute();
			try { datasetStmt.close(); } catch (SQLException e) {}

			// Insert the ingest task as a new version.
			sql = "INSERT INTO dataset_ingest(ingest_type_id, ingest_location, ingest_contact_id, ingest_status_id, dataset_id, entry_date) VALUES(?, ?, ?, ?, ?, ?)";
			PreparedStatement ingestStmt = connection.prepareStatement(sql);
			if (getIngestType() <= 0) {
				ingestStmt.setNull(1, Types.INTEGER);
			} else {
				ingestStmt.setInt(1, getIngestType());
			}
			ingestStmt.setString(2, getIngestLocation());
			if (getIngester() <= 0) {
				ingestStmt.setNull(3, Types.INTEGER);
			} else {
				ingestStmt.setInt(3, getIngester());
			}
			if (getIngestStatus() <= 0) {
				ingestStmt.setNull(4, Types.INTEGER);
			} else {
				ingestStmt.setInt(4, getIngestStatus());
			}
			ingestStmt.setString(5, getDatasetId());
			ingestStmt.setTimestamp(6, getEntryDate());
			ingestStmt.execute();
			try { ingestStmt.close(); } catch (SQLException e) {}

			// Insert the load task as a new version.
			sql = "INSERT INTO dataset_load(load_data_location, archive_location, load_contact_id, load_status_id, documentation_flag, dataset_id, entry_date) VALUES (?, ?, ?, ?, ?, ?, ?)";
			PreparedStatement loadStmt = connection.prepareStatement(sql);
			loadStmt.setString(1, getLoadDataLocation());
			loadStmt.setString(2, getArchiveLocation());
			if (getLoader() <= 0) {
				loadStmt.setNull(3, Types.INTEGER);
			} else {
				loadStmt.setInt(3, getLoader());
			}
			if (getLoadStatus() <= 0) {
				loadStmt.setNull(4, Types.INTEGER);
			} else {
				loadStmt.setInt(4, getLoadStatus());
			}
			loadStmt.setBoolean(5, isDocumented());
			loadStmt.setString(6, getDatasetId());
			loadStmt.setTimestamp(7, getEntryDate());
			loadStmt.execute();
			try { loadStmt.close(); } catch (SQLException e) {}

			// Insert the check task as a new version.
			sql = "INSERT INTO dataset_approve(approve_contact_id, approve_status_id, dataset_id, entry_date) VALUES(?, ?, ?, ?)";
			PreparedStatement approveStmt = connection.prepareStatement(sql);
			if (getChecker() <= 0) {
				approveStmt.setNull(1, Types.INTEGER);
			} else {
				approveStmt.setInt(1, getChecker());
			}
			if (getCheckStatus() <= 0) {
				approveStmt.setNull(2, Types.INTEGER);
			} else {
				approveStmt.setInt(2, getCheckStatus());
			}
			approveStmt.setString(3, getDatasetId());
			approveStmt.setTimestamp(4, getEntryDate());
			approveStmt.execute();
			try { approveStmt.close(); } catch (SQLException e) {}

			// Remove all project associations.
			sql = "DELETE FROM dataset_project WHERE dataset_id=?";
			PreparedStatement delProjStmt = connection.prepareStatement(sql);
			delProjStmt.setString(1, getDatasetId());
			delProjStmt.execute();
			try { delProjStmt.close(); } catch (SQLException e) {}

			// Add all project assocations as new.
			sql = "INSERT INTO dataset_project(dataset_id, project_id, master_list_flag) VALUES(?,?,?)";
			PreparedStatement projStmt = connection.prepareStatement(sql);
			projStmt.setString(1, getDatasetId());
			for (String projectId: getProjectsAsList()) {
				if (!isDeletedProject(projectId)) {
					projStmt.setString(2, projectId);
					projStmt.setBoolean(3, isInMasterList(projectId));
					projStmt.execute();
				}
			}
			try { projStmt.close(); } catch (SQLException e) {}

			// Only add a note if there is one to add.
			if (note.getNoteText() != null && !note.getNoteText().trim().equals("")) {
				note.insert(connection, this);
			}
		
			// Save the commands to the database.
			try { connection.commit(); }
			catch (SQLException e) { 
				try { connection.rollback(); }
				catch (SQLException e1) {}
				throw e;
			}
		} catch (SQLException e) {}
		// Close the database connection no matter what happens.
		finally {
			try { connection.close(); } catch (SQLException e) {}
		}
	}
}
