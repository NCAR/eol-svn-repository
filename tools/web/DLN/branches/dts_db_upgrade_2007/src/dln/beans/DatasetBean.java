package dln.beans;

import dln.display.*;
import java.sql.*;
import java.util.*;

/**
 * The DatasetBean is the representation of a data set in the DTS.
 * 
 * @author jclawson
 */
public class DatasetBean extends DefaultBean implements Comparable<DatasetBean> {
	
	private static final long serialVersionUID = -7027315789557748781L;
	
	private boolean docFlag, questions;
	private DatasetBean next, previous;
	private Timestamp entryDate;
	private int ingestType;
	private int listIndex;
	private List<String> deletedProjects, masterListFlags, projectList;
	private int checkStatus, ingestStatus, loadStatus;
	private String archiveLocation, datasetId, name, ingestLocation, loadDataLocation, originalId, remoteURL;
	private TreeSet<NoteBean> notes;
	private int checker, extContact, ingester, intContact, loader;
	
	/**
	 * Create a new instance of a DatasetBean.
	 */
	public DatasetBean() {
		projectList = new ArrayList<String>();
		masterListFlags = new ArrayList<String>();
		deletedProjects = new ArrayList<String>();
		notes = new TreeSet<NoteBean>();		
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
	 * Associate a project with the data set.
	 * @param projectId The project ID to associate to the data set.
	 */
	public void addProject(String projectId) {
		if (!projectList.contains(projectId)) { projectList.add(projectId); }
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
		} catch (SQLException e) { throw e; }
		// Close the connection to the database.
		finally {
			try { connection.close(); } catch (SQLException e) {}
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
	 * Get the entry date of this version of the data set.
	 * @return The data set version's entry date.
	 */
	public Timestamp getEntryDate() { return entryDate; }
	
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
	 * Get the set of notes associated to this data set.
	 * @return The notes associated the data set.
	 */
	public Set<NoteBean> getNotes() { return notes; }
	
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
	 * Get the data set ID as it is in the database whether it was changed in a form
	 * or not.
	 * @return The original data set ID as it is in the database.
	 */
	public String getOriginalDatasetId() { return originalId; }
	
	/**
	 * Get the previous data set in the displayed data set list.
	 * @return The previous data set in the list or <code>null</code> if this is the first data
	 * set in the list.
	 */
	public DatasetBean getPreviousDataset() { return previous; }

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
		return list;
	}

	/**
	 * Determine if this data set has been flags with questions/issues.
	 * @return <code>true</code> if the data set has outstanding questions,
	 * <code>false</code> if it does not.
	 */
	public boolean hasQuestions() { return questions; }
	
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
			String sql = "INSERT INTO dataset(dataset_id, name, source_contact_id, internal_contact_id, question_flag, remote_url) VALUES(?, ?, ?, ?, ?, ?)";
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
	 * Load this DatasetBean with the values from the database for the specified data set ID.
	 * @param datasetId The ID of the data set to be loaded.
	 * @throws SQLException if there is a problem loading the data set from the database.
	 */
	public void loadDataset(String datasetId) throws SQLException {
		// Define the connection to the database.
		Connection connection = getConnection();

		try {
			// Define the statement to load the data set.
			String sql = "SELECT name, source_contact_id, internal_contact_id, question_flag, remote_url, MAX(entry_date) FROM dataset JOIN dataset_approve ON (dataset.dataset_id=dataset_approve.dataset_id) WHERE dataset.dataset_id=? GROUP BY dataset.dataset_id";
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
			String sql = "SELECT source_contact_id, internal_contact_id, remote_url, ingest_type_id, ingest_location, load_data_location, archive_location FROM dataset JOIN dataset_ingest ON (dataset.dataset_id=dataset_ingest.dataset_id) JOIN dataset_load ON (dataset.dataset_id=dataset_load.dataset_id AND dataset_ingest.entry_date=dataset_load.entry_date) JOIN dataset_approve ON (dataset.dataset_id=dataset_approve.dataset_id AND dataset_ingest.entry_date=dataset_approve.entry_date) WHERE dataset.dataset_id=? AND dataset_ingest.entry_date=?";
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
			String sql = "SELECT dataset.dataset_id, name, dataset_approve.entry_date, ingest_contact_id, load_contact_id, approve_contact_id, load_status_id, approve_status_id, documentation_flag, question_flag, master_list_flag FROM dataset JOIN dataset_project ON (dataset.dataset_id=dataset_project.dataset_id) JOIN dataset_approve ON (dataset.dataset_id=dataset_approve.dataset_id) JOIN dataset_load ON (dataset.dataset_id=dataset_load.dataset_id AND dataset_approve.entry_date=dataset_load.entry_date) JOIN dataset_ingest ON (dataset.dataset_id=dataset_ingest.dataset_id AND dataset_approve.entry_date = dataset_ingest.entry_date) WHERE project_id=?";
			stmt = connection.prepareStatement(sql);
			stmt.setString(1, display.getDisplayId().trim());
		}
		// Limit the list based on the ingest contact ID
		else if (display.isDisplayIngesterList()) {
			String sql = "SELECT dataset.dataset_id, name, dataset_approve.entry_date, ingest_contact_id, load_contact_id, approve_contact_id, load_status_id, approve_status_id, documentation_flag, question_flag, NULL FROM dataset JOIN dataset_approve ON (dataset.dataset_id=dataset_approve.dataset_id) JOIN dataset_load ON (dataset.dataset_id=dataset_load.dataset_id AND dataset_approve.entry_date=dataset_load.entry_date) JOIN dataset_ingest ON (dataset.dataset_id=dataset_ingest.dataset_id AND dataset_approve.entry_date = dataset_ingest.entry_date) WHERE ingest_contact_id=?";
			stmt = connection.prepareStatement(sql);
			stmt.setInt(1, Integer.parseInt(display.getDisplayId()));
		}
		// Limit the list based on the load contact ID
		else if (display.isDisplayLoaderList()) {
			String sql = "SELECT dataset.dataset_id, name, dataset_approve.entry_date, ingest_contact_id, load_contact_id, approve_contact_id, load_status_id, approve_status_id, documentation_flag, question_flag, NULL FROM dataset JOIN dataset_approve ON (dataset.dataset_id=dataset_approve.dataset_id) JOIN dataset_load ON (dataset.dataset_id=dataset_load.dataset_id AND dataset_approve.entry_date=dataset_load.entry_date) JOIN dataset_ingest ON (dataset.dataset_id=dataset_ingest.dataset_id AND dataset_approve.entry_date = dataset_ingest.entry_date) WHERE load_contact_id=?";
			stmt = connection.prepareStatement(sql);
			stmt.setInt(1, Integer.parseInt(display.getDisplayId()));
		}
		// Limit the list based on the check contact ID
		else if (display.isDisplayCheckerList()) {
			String sql = "SELECT dataset.dataset_id, name, dataset_approve.entry_date, ingest_contact_id, load_contact_id, approve_contact_id, load_status_id, approve_status_id, documentation_flag, question_flag, NULL FROM dataset JOIN dataset_approve ON (dataset.dataset_id=dataset_approve.dataset_id) JOIN dataset_load ON (dataset.dataset_id=dataset_load.dataset_id AND dataset_approve.entry_date=dataset_load.entry_date) JOIN dataset_ingest ON (dataset.dataset_id=dataset_ingest.dataset_id AND dataset_approve.entry_date = dataset_ingest.entry_date) WHERE approve_contact_id=?";
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
			dataset.setLoadStatus(results.getInt(7));
			dataset.setCheckStatus(results.getInt(8));
			dataset.setDocumented(results.getBoolean(9));
			dataset.setQuestions(results.getBoolean(10));
			// Only assign the master list flag when in project view as it has
			// no meaning in the task view.
			if (display.getDisplayView() == DatasetDisplayBean.PROJECT_DISPLAY_VIEW) {
				dataset.addProject(display.getDisplayId());
				dataset.setInMasterList(display.getDisplayId(), results.getBoolean(11));
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
	 * Set the time the data set was entered into the database.
	 * @param date The creation time of the data set.
	 */
	public void setEntryDate(Timestamp date) { this.entryDate = date; }
	
	/**
	 * Set the source contact for this data set.
	 * @param extContact The database ID of the source contact.
	 */
	public void setExternalContact(int extContact) { this.extContact = extContact; }
	
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
	 * Set the data set that is before this data set in the display list.
	 * @param previous The previous data set in the list.
	 */
	private void setPreviousDataset(DatasetBean previous) { this.previous = previous; }
	
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
	 * Set the remote URL for the data set.
	 * @param url The data set's remote URL.
	 */
	public void setRemoteURL(String url) { remoteURL = url; }
	
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
			String sql = "UPDATE dataset SET dataset_id=?, name=?, source_contact_id=?, internal_contact_id=?, question_flag=?, remote_url=? WHERE dataset_id=?";
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
			datasetStmt.setString(7, getOriginalDatasetId());
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
		} catch (SQLException e) { throw e; }
		// Close the database connection no matter what happens.
		finally {
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
			connection.setAutoCommit(false);
		

			// Determine if the data set ID needs to be converted to a DTS ID.
			determineDatasetId(connection);
		
			// This will be the new date for the version.
			setEntryDate(getCurrentTime(connection));

			// Define the statement to update the data set.
			String sql = "UPDATE dataset SET dataset_id=?, name=?, source_contact_id=?, internal_contact_id=?, question_flag=?, remote_url=? WHERE dataset_id=?";
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
			datasetStmt.setString(7, getOriginalDatasetId());
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
