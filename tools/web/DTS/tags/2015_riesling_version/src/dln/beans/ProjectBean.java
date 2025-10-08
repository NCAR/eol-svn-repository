package dln.beans;

import java.sql.*;
import java.text.*;
import java.util.*;

/**
 * The ProjectBean is a container for a Project.  It also contains functions on how
 * to load the project from the database, insert new projects, and update an existing
 * one.  It can also generate lists of projects as well.
 * 
 * @author jclawson
 * @author Dan Sullivan
 */
public class ProjectBean extends DefaultBean {

	private static final long serialVersionUID = -7373059486168169215L;
	
	private Double minLat, minLon, maxLat, maxLon;
	private String beginDate, chargeNumber, endDate, name, originalId, prefixes, projectId;
	private TreeMap<String, DatasetBean> processedDatasets;
	private TreeSet<NoteBean> notes;
	private boolean active;

	/**
	 * Create a new instance of a ProjectBean.
	 */
	public ProjectBean() { this(""); }
	
	/**
	 * Create a new instance of a ProjectBean.
	 * @param projectId The project ID for the bean.
	 */
	public ProjectBean(String projectId) {
		this.originalId = projectId;
		this.projectId = projectId;
		notes = new TreeSet<NoteBean>();
	}
	
	/**
	 * Get the list of projects from the database that are flagged as active.
	 * @return The list of active projects.
	 * @throws SQLException if there is a problem accessing the projects from the database.
	 */
	public List<ProjectBean> getActiveProjects() throws SQLException { 
		return getProjectList(Boolean.TRUE);
	}
	
	/**
	 * Get the list of all of the projects in the database.
	 * @return The list of all projects.
	 * @throws SQLException if there is a problem accessing the projects from the database.
	 */
	public List<ProjectBean> getAllProjects() throws SQLException {
		return getProjectList(null);
	}
	
	/**
	 * Get the start date of the project's time of interest.
	 * @return Returns the projects begin date or '0001-01-01' as a default value.
	 */
	public String getBeginDate() { 
		return beginDate == null || beginDate.equals("") ? "0001-01-01" : beginDate;
	}
	
	/**
	 * Get the begin date as a java.sql.Date object.
	 * @return The begin date as a date object.
	 * @throws ParseException if there is a problem parsing the String to a Date.
	 */
	private java.sql.Date getBeginDateAsDate() throws ParseException {
		if (getBeginDate().equals("0000-00-00")) {
			setBeginDate("0001-01-01");
		}
		return convertDate(getBeginDate());
	}
	
	/**
	 * Get the account charge number for work on this project.
	 * @return The project's account charge number.
	 */
	public String getChargeNumber() { return chargeNumber == null ? "" : chargeNumber.trim(); }
	
	/**
	 * Get the list of dataset id prefixes assigned to the project.  The IDs are separated by
	 * semicolons.
	 * @return The list of data set ID prefixes.
	 */
	public String getDatasetPrefixes() { return prefixes == null ? "" : prefixes.trim(); }
	
	/**
	 * Get the end date of the project's time of interest.
	 * @return Returns the projects end date or '9999-12-31' as a default value.
	 */
	public String getEndDate() {
		return endDate == null || endDate.equals("") ? "9999-12-31" : endDate;
	}
	
	/**
	 * Get the end date as a java.sql.Date object.
	 * @return The end date as a date object.
	 * @throws ParseException if there is a problem parsing the String to a Date.
	 */
	private java.sql.Date getEndDateAsDate() throws ParseException {
		if (getEndDate().equals("0000-00-00")) {
			setEndDate("9999-12-31");
		}
		return convertDate(getEndDate());
	}
	
	/**
	 * Get the northern most latitude for the project's area of interest.
	 * @return The project's maximum latitude.
	 */
	public Double getMaxLatitude() { return maxLat == null ? 90.0 : maxLat; }
	
	/**
	 * Get the western most longitude for the project's area of interest.
	 * @return The project's maximum longitude.
	 */
	public Double getMaxLongitude() { return maxLon == null ? 180.0 : maxLon; }
	
	/**
	 * Get the southern most latitude for the project's area of interest.
	 * @return The project's minimum latitude.
	 */
	public Double getMinLatitude() { return minLat == null ? -90.0 : minLat; }

	/**
	 * Get the eastern most longitude for the project's area of interest.
	 * @return The project's minimum longitude.
	 */
	public Double getMinLongitude() { return minLon == null ? -180.0 : minLon; }
	
	/**
	 * Get the full name of the project.
	 * @return The project's full name.
	 */
	public String getName() { return name == null ? "" : name.trim(); }
	
	/**
	 * Get the project's original project ID before it was updated.
	 * @return The project's original project ID.
	 */
	public String getOriginalProjectId() { return originalId; }

	/**
	 * Get the set of processed data sets associated with this project.
	 * @return The set of processed data sets mapped by the data set ID.
	 * @throws SQLException if there is a problem loading the data sets from the database.
	 */
	private Map<String, DatasetBean> getProcessedDatasets() throws SQLException {
		if (processedDatasets == null) {
			processedDatasets = new TreeMap<String, DatasetBean>();
			for (DatasetBean dataset: DatasetBean.loadProcessedDatasets(this)) {
				processedDatasets.put(dataset.getDatasetId(), dataset);
			}
		}
		return processedDatasets;
	}
	
	/**
	 * Get the list of product data sets associated to this project.
	 * @return The list of this project's product data sets.
	 * @throws SQLException if there is a problem reading the products from the database.
	 */
	public List<DatasetBean> getProductDatasets() throws SQLException {
		List<DatasetBean> products = new ArrayList<DatasetBean>();
		
		for (DatasetBean dataset: getProcessedDatasets().values()) {
			if (dataset.isProduct()) { products.add(dataset); }
		}
		
		Collections.sort(products, new dln.util.DatasetNameComparator(false));
		return products;
	}
	
	/**
	 * Get the project's ID.
	 * @return The project's ID.
	 */
	public String getProjectId() { return projectId == null ? "" : projectId.trim(); }
	
	/**
	 * Get a list of projects from the database.
	 * @param active A flag to get active/inactive projects only.  <code>null</code> will get all projects.
	 * @return The list of projects from the database.
	 * @throws SQLException if there is a problem accessing the database.
	 */
	private List<ProjectBean> getProjectList(Boolean active) throws SQLException {
		// Define the list to store the list from the projects.
		List<ProjectBean> projects = new ArrayList<ProjectBean>();
		
		// Define the connection to the database.
		Connection connection = getConnection();

		try {
			// Define the SQL to pull the project information from the database.
			String sql = "SELECT project_id, full_name, begin_date, end_date, minlat, maxlat, minlon, maxlon, charge_number, active_flag FROM project";
			// Handle the case where the active flag is not NULL
			if (active != null) { sql += " WHERE active_flag=?"; }
			
			// Define the statement to be executed on the database.
			PreparedStatement stmt = connection.prepareStatement(sql);
			// Set the value for the active flag if it is not NULL.
			if (active != null) { stmt.setBoolean(1, active); }
			
			// Execute the query on the database and obtain the results.
			ResultSet results = stmt.executeQuery();
			
			// Loop through the rows of the results.
			while (results.next()) {
				ProjectBean project = new ProjectBean(results.getString(1));
				project.setProjectId(results.getString(1));
				project.setName(results.getString(2));
				project.setBeginDate(results.getDate(3));
				project.setEndDate(results.getDate(4));
				project.setMinLatitude(results.getDouble(5));
				project.setMaxLatitude(results.getDouble(6));
				project.setMinLongitude(results.getDouble(7));
				project.setMaxLongitude(results.getDouble(8));
				project.setChargeNumber(results.getString(9));
				project.setActive(results.getBoolean(10));
				
				// The project also needs to extract the data set id prefixes from another table.
				project.loadDatasetPrefixes(connection);
				
				// The project has been defined, so add it to the list.
				projects.add(project);
			}
			
			// Close the open database streams.
			try { results.close(); } catch (SQLException e) {}
			try { stmt.close(); } catch (SQLException e) {}		
		} catch (SQLException e) {
			throw e;
		}
		// Need to use the finally to close the connection cleanly and not leave the DLN in a bad state.
		finally { 
			// Close the connection to the database.
			try { connection.close(); } catch (SQLException e) {}
		}

		// Return the list of projects loaded from the database.
		return projects;
	}
	
	/**
	 * Get the notes associated to the project in reverse time order (most recent notes
	 * first).
	 * @return The list of filtered notes in most recent time creation order.
	 */
	public List<NoteBean> getReverseNoteList() { 
		List<NoteBean> list = new ArrayList<NoteBean>(notes);
		Collections.reverse(list);
		return list;
	}

	/**
	 * Insert this project into the database.
	 * @throws ParseException if there was a problem converting the begin or end date String to a Date.
	 * @throws SQLException if there was a problem inserting the project into the database.
	 */
	public void insert() throws ParseException, SQLException {
		// Define the connection to the database.
		Connection connection = getConnection();
		
		try {
			// Make sure to turn off auto commit so all of the commands will execute as a single transaction.
			connection.setAutoCommit(false);
	
			// Define the SQL to update the project information it the database.
			String sql = "INSERT INTO project(project_id, full_name, active_flag, begin_date, end_date, minlat, maxlat, minlon, maxlon, charge_number) VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";
			// Define the statement to be executed on the database.
			PreparedStatement stmt = connection.prepareStatement(sql);
			stmt.setString(1, getProjectId());
			stmt.setString(2, getName() == null ? "" : getName());
			stmt.setBoolean(3, isActive());
			stmt.setDate(4, getBeginDateAsDate());
			stmt.setDate(5, getEndDateAsDate());
			stmt.setDouble(6, getMinLatitude());
			stmt.setDouble(7, getMaxLatitude());
			stmt.setDouble(8, getMinLongitude());
			stmt.setDouble(9, getMaxLongitude());
			stmt.setString(10, getChargeNumber());
			// Perform the update in the database.
			stmt.execute();
			// Close the open database streams.
			try { stmt.close(); } catch (SQLException e) {}
			
			// Now that the project has been inserted, insert the data set ID prefixes.
			updateDatasetPrefixes(connection);
	
			// Save all of the executed statements to the database permanently.
			connection.commit();
		} catch (SQLException e) {
			throw e;
		}
		// Need to use the finally to close the connection cleanly and not leave the DLN in a bad state.
		finally { 
			// Close the connection to the database.
			try { connection.close(); } catch (SQLException e) {}
		}
	}
	
	/**
	 * Determine if the project is considered to be active.
	 * @return <code>true</code> if the project is active, <code>false</code> if it is not.
	 */
	public boolean isActive() { return active; }
	
	/**
	 * Extract the data set ID prefixes for the project from the database.
	 * @param connection The connection to use to execute the statement.
	 * @throws SQLException if there is a problem reading the database.
	 */
	private void loadDatasetPrefixes(Connection connection) throws SQLException {
		// Define the SQL to read the prefixes from the database.
		String prefixSql = "SELECT dataset_id_prefix FROM dataset_prefix_project WHERE project_id=?";
		// Define the statement to execute on the database.
		PreparedStatement prefixStmt = connection.prepareStatement(prefixSql);		
		prefixStmt.setString(1, getProjectId());
		// Execute the statement and get the results.
		ResultSet prefixResults = prefixStmt.executeQuery();
		
		// Define the container for accumulating the prefixes.
		StringBuffer prefixes = new StringBuffer();
		// Put the first result into the prefix accumulator.
		if (prefixResults.next()) { prefixes.append(prefixResults.getString(1)); }
		// Add all remaining prefixes to the accumulator adding a ; between entries.
		while (prefixResults.next()) {
			prefixes.append(";").append(prefixResults.getString(1));
		}
		// Assign the prefix string to the project.
		setDatasetPrefixes(prefixes.toString());
		
		// Close the open database streams.
		try { prefixResults.close(); } catch (SQLException e) {}
		try { prefixStmt.close(); } catch (SQLException e) {}
	}
	
	/**
	 * Load the project defined by the specified project ID.
	 * @param projectId The ID of the project to be loaded from the database.
	 * @throws SQLException if there is a problem reading the project from the database.
	 */
	public void loadProject(String projectId) throws SQLException {
		// Define the connection to the database.
		Connection connection = getConnection();

		try {
			// Define the SQL to pull the project information from the database.
			String sql = "SELECT project_id, full_name, begin_date, end_date, minlat, maxlat, minlon, maxlon, charge_number, active_flag FROM project WHERE project_id=?";
			// Define the statement to be executed on the database.
			PreparedStatement stmt = connection.prepareStatement(sql);
			stmt.setString(1, projectId);
			
			// Execute the statement and get the results.
			ResultSet results = stmt.executeQuery();
			if (results.next()) {
				setProjectId(results.getString(1));
				setOriginalProjectId(results.getString(1));
				setName(results.getString(2));
				setBeginDate(results.getDate(3));
				setEndDate(results.getDate(4));
				setMinLatitude(results.getDouble(5));
				setMaxLatitude(results.getDouble(6));
				setMinLongitude(results.getDouble(7));
				setMaxLongitude(results.getDouble(8));
				setChargeNumber(results.getString(9));
				setActive(results.getBoolean(10));
				
				// The project is now loaded, so the information is set to load the prefixes.
				loadDatasetPrefixes(connection);

				loadNotes(connection);
			}
			
			// Close the open database streams.
			try { results.close(); } catch (SQLException e) {}
			try { stmt.close(); } catch (SQLException e) {}
		} catch (SQLException e) {
			throw e;
		}
		// Need to use the finally to close the connection cleanly and not leave the DLN in a bad state.
		finally { 
			// Close the connection to the database.
			try { connection.close(); } catch (SQLException e) {}
		}
	}
	
	/**
	 * Load the notes attached to the project.
	 * @param connection The connection to use to load the notes.
	 * @throws SQLException if there is a problem loading the notes.
	 */
	private void loadNotes(Connection connection) throws SQLException {
		Map<Integer, NoteBean> noteMap = new TreeMap<Integer, NoteBean>();
		// Load the note types from the database.
		Map<Integer, NoteTypeBean> typeMap = NoteTypeBean.getNoteTypesMap(connection);
		
		// Define the statement to load the notes for the data set.
		String sql = "SELECT note.note_id, note_type_id, author_id, entry_date, note_text, note.row_revise_time FROM note JOIN project_note ON (note.note_id=project_note.note_id) WHERE project_id=?";
		PreparedStatement stmt = connection.prepareStatement(sql);
		stmt.setString(1, getProjectId());
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
		notes.addAll(noteMap.values());
	}
	
	/**
	 * Set the active flag for this project.
	 * @param active The flag to mark the project as active or not.
	 */
	public void setActive(boolean active) { this.active = active; }

	/**
	 * Set the begin date of the project's time of interest.
	 * @param beginDate The project's begin date.
	 */
	private void setBeginDate(java.sql.Date beginDate) { setBeginDate(beginDate.toString()); }
	
	/**
	 * Set the begin date of the project's time of interest.
	 * @param beginDate The project's begin date.
	 */
	public void setBeginDate(String beginDate) { this.beginDate = beginDate; }
	
	/**
	 * Set the account charge number for the work done on this project.
	 * @param number The project's charge number.
	 */
	public void setChargeNumber(String number) { this.chargeNumber = number; }
	
	/**
	 * Set the list of data set ID prefixes allowed for this project.  The list is separated by
	 * semicolons.
	 * @param prefixes The semicolon delimited list of data set ID prefixes.
	 */
	public void setDatasetPrefixes(String prefixes) { this.prefixes = prefixes; }

	/**
	 * Set the end date of the project's time of interest.
	 * @param endDate The project's end date.
	 */
	private void setEndDate(java.sql.Date endDate) { setEndDate(endDate.toString()); }
	
	/**
	 * Set the end date of the project's time of interest.
	 * @param endDate The project's end date.
	 */
	public void setEndDate(String endDate) { this.endDate = endDate; }
	
	/**
	 * Set the northern most latitude of the project's area of interest.
	 * @param lat The maximum latitude for the project.
	 */
	public void setMaxLatitude(Double lat) { maxLat = lat; }
	
	/**
	 * Set the western most longitude of the project's area of interest.
	 * @param lon The maximum longitude for the project.
	 */
	public void setMaxLongitude(Double lon) { maxLon = lon; }
	
	/**
	 * Set the southern most latitude of the project's area of interest.
	 * @param lat The minimum latitude for the project.
	 */
	public void setMinLatitude(Double lat) { minLat = lat; }
	
	/**
	 * Set the eastern most longitude of the project's area of interest.
	 * @param lon The minimum longitude for the project.
	 */
	public void setMinLongitude(Double lon) { minLon = lon; }
	
	/**
	 * Set the full name for the project.
	 * @param name The project's full name.
	 */
	public void setName(String name) { this.name = name; }
	
	/**
	 * Set the original project ID that is the current value in the database.
	 * @param originalId The original project ID for the project.
	 */
	public void setOriginalProjectId(String originalId) { this.originalId = originalId; }
	
	/**
	 * Set the project ID for the project.
	 * @param projectId The project's identifier.
	 */
	public void setProjectId(String projectId) { this.projectId = projectId; }
	
	/**
	 * Update the project in the database with the new values stored in the project.
	 * @throws ParseException if there is a problem converting the begin or end date String to a Date.
	 * @throws SQLException if there is a problem executing a statement on the database.
	 */
	public void update() throws ParseException, SQLException {
		// Define the connection to the database.
		Connection connection = getConnection();
		try {
			// Need to turn off auto commit so all of the update statements will be executed as a single transaction.
			connection.setAutoCommit(false);
	
			// Define the SQL to update the project information it the database.
			String sql = "UPDATE project SET project_id=?, full_name=?, active_flag=?, begin_date=?, end_date=?, minlat=?, maxlat=?, minlon=?, maxlon=?, charge_number=? WHERE project_id=?";
			// Define the statement to be executed on the database.
			PreparedStatement stmt = connection.prepareStatement(sql);
			stmt.setString(1, getProjectId());
			stmt.setString(2, getName() == null ? "" : getName());
			stmt.setBoolean(3, isActive());
			stmt.setDate(4, getBeginDateAsDate());
			stmt.setDate(5, getEndDateAsDate());
			stmt.setDouble(6, getMinLatitude());
			stmt.setDouble(7, getMaxLatitude());
			stmt.setDouble(8, getMinLongitude());
			stmt.setDouble(9, getMaxLongitude());
			stmt.setString(10, getChargeNumber());
			stmt.setString(11, getOriginalProjectId());
			
			// Perform the update in the database.
			stmt.execute();
			// Close the open database streams.
			try { stmt.close(); } catch (SQLException e) {}
			
			// The project has been updated, so now update the prefixes.
			updateDatasetPrefixes(connection);
			
			// All of the commands executed, so save them to the database permanently.
			connection.commit();
		} catch (SQLException e) {
			throw e;
		} 
		// Need to use the finally to close the connection cleanly and not leave the DLN in a bad state.
		finally { 
			// Close the open connection stream.
			try { connection.close(); } catch (SQLException e) {}
		}
	}
	
	/**
	 * Update the data set ID prefixes for the project by removing any IDs no longer associated to
	 * the project and adding new ones.
	 * @param connection The connection to use to execute the commands.
	 * @throws SQLException if there is a problem updating the database.
	 */
	private void updateDatasetPrefixes(Connection connection) throws SQLException {
		// Define the statement to delete all existing prefixes.
		PreparedStatement delStmt = connection.prepareStatement("DELETE FROM dataset_prefix_project WHERE project_id=?");
		delStmt.setString(1, getProjectId());
		// Delete all the prefixes.
		delStmt.execute();
		// Properly close the open statement stream.
		try { delStmt.close(); } catch (SQLException e) {}
		
		// Only try to add prefixes if there are some to add.
		if (getDatasetPrefixes() != null && !getDatasetPrefixes().equals("")) {
			// Define the SQL to insert the prefixes for the project.
			String prefixSql = "INSERT INTO dataset_prefix_project(project_id, dataset_id_prefix) VALUES(?, ?)";
			// Define the statement to insert the prefixes for the project.
			PreparedStatement prefixStmt = connection.prepareStatement(prefixSql);
			prefixStmt.setString(1, getProjectId());
			// Loop through each prefix.
			for (String prefix: Arrays.asList(getDatasetPrefixes().split(";"))) {
				// Assign the prefix to the statement.
				prefixStmt.setString(2, prefix.trim());
				// Execute the statement.
				prefixStmt.execute();
			}

			// Properly close the open statement stream.
			try { prefixStmt.close(); } catch (SQLException e) {}
		}
	}
}
