package dts.port;

import java.sql.*;
import java.util.*;

/**
 * The Project class is the container and database servicer for projects.  It contains
 * general meta data for projects in the DTS.
 * 
 * @author jclawson
 */
public class Project implements Comparable {

	private Boolean active;
	private Double maxLat, maxLon, minLat, minLon;
	private java.sql.Date beginDate, endDate;
	private Set prefixes;
	private String chargeNumber, fullName, note, projectId;
	
	/**
	 * Create a new instance of a Project.
	 * @param projectId The project identifier.
	 */
	public Project(String projectId) {
		this.projectId = projectId;
		
		prefixes = new TreeSet();
	}
	
	/**
	 * Add a prefix to use for data sets for this project.
	 * @param prefix A data set prefix.
	 */
	public void addDatasetPrefix(String prefix) {
		prefixes.add(prefix);
	}
	
	/**
	 * Read in the Project information from the EMDAC database and compare the meta data with the values in
	 * the provided container.
	 * @param porter The porting controller class running the porting process.
	 * @param connection The ZEDI database connection to use to read the Project meta data.
	 * @param projects The mapping of Projects by project ID used to store and merge the DTS information.
	 * @return <code>true</code> if the porting process proceeded successfully without any database read errors
	 * or conflicts between meta data.
	 * @throws SQLException if there is a problem reading the Project information from the DLN database.
	 */
	public static boolean compareEmdacProjects(ToDmgDts porter, Connection connection, TreeMap projects) throws SQLException {
		// Define the success flag used to return the success of reading the Project information from the database.
		boolean success = true;
		// Define the SQL to read the Project information from the database.
//		String sql = "SELECT full_name, begin_date, end_date, minlat, minlon, maxlat, maxlon FROM project WHERE project_id=?";
		String sql = "SELECT title, begin_date, end_date, minimum_latitude, minimum_longitude, maximum_latitude, maximum_longitude FROM project WHERE name=?";
		// Define the statement to execute on the database.
		PreparedStatement stmt = connection.prepareStatement(sql);
		// Define the SQL to read the data set prefixed for the project.
//		String prefixSQL = "SELECT 1 FROM dataset_prefix_project WHERE project_id=? AND dataset_id_prefix=?";
		String prefixSQL = "SELECT 1 FROM dataset_prefix_project WHERE project_name=? AND dataset_id_prefix=?";
		// Define the prefix selections statement to execute on the database.
		PreparedStatement prefixStmt = connection.prepareStatement(prefixSQL);
		
		// Define an iterator to loop over all of the projects from the mapping.
		Iterator itr = projects.values().iterator();
		while (itr.hasNext()) {
			// Get the project that is currently being tested.
			Project project = (Project)itr.next();
			// Assign the project ID to the statement so it can read in its information.
			stmt.setString(1, project.getProjectId());
			// Get the results of the query.
			ResultSet results = stmt.executeQuery();
			
			if (results.next()) {
				// Set the full name of the project.
				try { project.setFullName(results.getString(1)); }
				// Handle the exception when the name does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}
				
				// Set the begin date of the project.
				try { project.setBeginDate(results.getDate(2)); }
				// Handle the exception when the date does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}
				
				// Set the end date of the project.
				try { project.setEndDate(results.getDate(3)); }
				// Handle the exception when the date does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}

				// Handle a null minimum latitude
				if (results.getString(4) == null) {
					// Update the porting class with the new exception.
					porter.appendMergeException(new MergeException("Project "+project.getProjectId()+" has a null min latitude."));
					// This is a conflict, so the entire port failed.
					success = false;
				} else {
					// Set the minimum latitude of the project's area of interest.
					try { project.setMinLatitude(results.getDouble(4)); }
					// Handle the exception when the latitude does not match a previously set value.
					catch (MergeException e) {
						// Update the porting class with the exception.
						porter.appendMergeException(e);
						// There is a conflict, so the entire port failed.
						success = false;
					}
				}
				
				// Handle a null minimum longitude
				if (results.getString(5) == null) {
					// Update the porting class with the new exception.
					porter.appendMergeException(new MergeException("Project "+project.getProjectId()+" has a null min lonitude."));
					// This is a conflict, so the entire port failed.
					success = false;
				} else {
					// Set the minimum longitude of the project's area of interest.
					try { project.setMinLongitude(results.getDouble(5)); }
					// Handle the exception when the longitude does not match a previously set value.
					catch (MergeException e) {
						// Update the porting class with the exception.
						porter.appendMergeException(e);
						// There is a conflict, so the entire port failed.
						success = false;
					}
				}

				// Handle a null maximum latitude
				if (results.getString(6) == null) {
					// Update the porting class with the new exception.
					porter.appendMergeException(new MergeException("Project "+project.getProjectId()+" has a null max latitude."));
					// This is a conflict, so the entire port failed.
					success = false;
				} else {
					// Set the maximum latitude of the project's area of interest.
					try { project.setMaxLatitude(results.getDouble(6)); }
					// Handle the exception when the latitude does not match a previously set value.
					catch (MergeException e) {
						// Update the porting class with the exception.
						porter.appendMergeException(e);
						// There is a conflict, so the entire port failed.
						success = false;
					}
				}
				
				// Handle a null maximum longitude
				if (results.getString(7) == null) {
					// Update the porting class with the new exception.
					porter.appendMergeException(new MergeException("Project "+project.getProjectId()+" has a null max lonitude."));
					// This is a conflict, so the entire port failed.
					success = false;
				} else {
					// Set the maximum longitude of the project's area of interest.
					try { project.setMaxLongitude(results.getDouble(7)); }
					// Handle the exception when the longitude does not match a previously set value.
					catch (MergeException e) {
						// Update the porting class with the exception.
						porter.appendMergeException(e);
						// There is a conflict, so the entire port failed.
						success = false;
					}
				}
			}
			// There were not any results found, so the project isn't in the database
			else {
				porter.appendMergeException(new MergeException("Project "+project.getProjectId()+" was not found in the EMDAC database."));
				success = false;
			}
			
			// Cleanly close the open results stream.
			try { results.close(); } catch (SQLException e) {}
			
			
			// Assign the current project to the prefix statement.
			prefixStmt.setString(1, project.getProjectId());
			// Define an iterator over the data set prefixed for the current project.
			Iterator iter = project.getDatasetPrefixes().iterator();
			while (iter.hasNext()) {
				// Pull the current prefix from the iterator.
				String prefix = (String)iter.next();
				// Assign the prefix to the statement.
				prefixStmt.setString(2, prefix);
				// Get the results of the query.
				results = prefixStmt.executeQuery();
				
				// An empty result set means the current prefix is not in EMDAC.
				if (!results.next()) {
					porter.appendMergeException(new MergeException("Project "+project.getProjectId()+" does not have dataset prefix "+prefix+" in the EMDAC database"));
					success = false;
				}
				
				// Cleanly close the results and ignore any exceptions.
				try { results.close(); } catch (SQLException e) {}
			}
		}
		
		// Cleanly close the statements and ignore any exceptions.
		try { prefixStmt.close(); } catch (SQLException e) {}
		try { stmt.close(); } catch (SQLException e) {}
		
		// All of the projects have been processed, so we can return the success flag now.
		return success;
	}
	
	/**
	 * Read in the Project information from the Master List database and compare the meta data with the values in
	 * the provided container.
	 * @param porter The porting controller class running the porting process.
	 * @param connection The ML database connection to use to read the Project meta data.
	 * @param projects The mapping of Projects by project ID used to store and merge the DTS information.
	 * @return <code>true</code> if the porting process proceeded successfully without any database read errors
	 * or conflicts between meta data.
	 * @throws SQLException if there is a problem reading the Project information from the ML database.
	 */
	public static boolean compareMasterListProjects(ToDmgDts porter, Connection connection, TreeMap projects) throws SQLException {
		// Define the success flag used to return the success of reading the Project information from the database.
		boolean success = true;
		// Define the SQL to read the Project information from the database.
		String sql = "SELECT project_id FROM project";
		// Define the statement to execute on the database.
		PreparedStatement stmt = connection.prepareStatement(sql);
		// Get the results of the query.
		ResultSet results = stmt.executeQuery();
		
		// Loop through each row of the results.
		while (results.next()) {
			// Extract the project ID from the current rows of the results.
			String projectId = results.getString(1);
			// Make sure the project exists in the DTS.
			if (!projects.containsKey(projectId)) {
				porter.appendMergeException(new MergeException("Project "+projectId+" is not in the DTS."));
				success = false;
			}
		}
		
		// Close the open database streams.
		try { results.close(); } catch (SQLException e) {}
		try { stmt.close(); } catch (SQLException e) {}
		
		// All of the projects have been processed, so we can return the success flag now.
		return success;		
	}
	
	/**
	 * Compare this Project to the specified Project for sort order.  This uses the natural sort
	 * order of the Project identifiers.
	 * @param project The Project to compare to this one.
	 * @return A negative integer, zero, or positive integer if this Project is less than,
	 * equal to, or greater than the specified Project.
	 */
	public int compareTo(Object project) {
		return getProjectId().compareTo(((Project)project).getProjectId());
	}
	
	/**
	 * Get the begin date of the Project's time of interest.
	 * @return The Project's begin date.
	 */
	public java.sql.Date getBeginDate() { return beginDate; }
	
	/**
	 * Get the charge number for work done on this Project.
	 * @return The Project's charge number.
	 */
	public String getChargeNumber() { return chargeNumber == null ? "000000" : chargeNumber; }
	
	/**
	 * Get the collection of data set prefixes that have been assigned to this project.
	 * @return The collection of this Project's data set prefixes.
	 */
	public Collection getDatasetPrefixes() { return prefixes; }
	
	/**
	 * Get the end date of the Project's time of interest.
	 * @return The Project's end date.
	 */
	public java.sql.Date getEndDate() { return endDate; }
	
	/**
	 * Get the northern latitude of the Project's area of interest.
	 * @return The Project's maximum latitude.
	 */
	public double getMaxLatitude() {
		return maxLat == null ? 90.0 : maxLat.doubleValue();
	}
	
	/**
	 * Get the eastern longitude of the Project's area of interest.
	 * @return The Project's maximum longitude.
	 */
	public double getMaxLongitude() {
		return maxLon == null ? 180.0 : maxLon.doubleValue();
	}
	
	/**
	 * Get the southern latitude of the Project's area of interest.
	 * @return The Project's minimum latitude.
	 */
	public double getMinLatitude() {
		return minLat == null ? -90.0 : minLat.doubleValue();
	}
	
	/**
	 * Get the western longitude of the Project's area of interest.
	 * @return The Project's minimum longitude.
	 */
	public double getMinLongitude() {
		return minLon == null ? -180.0 : minLon.doubleValue();
	}
	
	/**
	 * Get the full name of the Project.
	 * @return The Project's full name.
	 */
	public String getName() { return fullName; }
	
	/**
	 * Get the Project's identifier.
	 * @return The Project's identifier.
	 */
	public String getProjectId() { return projectId; }
	
	/**
	 * Get the note for the Project.
	 * @return The project's note.
	 */
	public String getProjectNote() {
		return note == null ? "" : note.trim();
	}
	
	/**
	 * Determine if this Project is active within the DTS.
	 * @return <code>true</code> if the Project is active, <code>false</code> if it is not.
	 */
	public boolean isActive() {
		return active == null ? false : active.booleanValue();
	}

	/**
	 * Read in the Project information from the Data Loading Notes database and store the meta data in
	 * the provided container.
	 * @param porter The porting controller class running the porting process.
	 * @param connection The DLN database connection to use to read the Project meta data.
	 * @param projects The mapping of Projects by project ID used to store and merge the DTS information.
	 * @return <code>true</code> if the porting process proceeded successfully without any database read errors
	 * or conflicts between meta data.
	 * @throws SQLException if there is a problem reading the Project information from the DLN database.
	 */
	public static boolean loadDlnProjects(ToDmgDts porter, Connection connection, TreeMap projects) throws SQLException {
		// Define the success flag used to return the success of reading the Project information from the database.
		boolean success = true;
		// Define the SQL to read the Project information from the database.
		String sql = "SELECT pname, storm_id_prefix, active FROM project";
		// Define the statement to execute on the database.
		PreparedStatement stmt = connection.prepareStatement(sql);
		// Execute the statement and obtain the results of the query.
		ResultSet results = stmt.executeQuery();
		
		// Loop through each row of the query results.
		while (results.next()) {
			// Pull out the project ID from the current row of the results.
			String projectId = results.getString(1);
			// Create a new Project if it is not already in the mapping.
			if (!projects.containsKey(projectId)) {
				projects.put(projectId, new Project(projectId));
			}
			// Load the Project from the mapping with the current row's project ID.
			Project project = (Project)projects.get(projectId);
			// Add the data set prefix to the Project.  This does not need to test to 
			// see if it already exists since a Project may have multiple prefixes.
			project.addDatasetPrefix(results.getString(2));
			// Set the active flag for the Project.  (Only worry if the value is not null.)
			if (results.getString(3) != null) {
				// Try to set the active flag for the Project.
				try { project.setActive(results.getBoolean(3)); }
				// Handle the exception when the flag does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}
			}
		}
		
		// Close the open database streams and ignore any exceptions.
		try { results.close(); } catch (SQLException e) {}
		try { stmt.close();  } catch (SQLException e) {}
		
		// All of the projects have been processed, so we can return the success flag now.
		return success;
	}

	/**
	 * Read in the Project information from the IVEN database and store the meta data in
	 * the provided container.
	 * @param porter The porting controller class running the porting process.
	 * @param connection The IVEN database connection to use to read the Project meta data.
	 * @param projects The mapping of Projects by project ID used to store and merge the DTS information.
	 * @return <code>true</code> if the porting process proceeded successfully without any database read errors
	 * or conflicts between meta data.
	 * @throws SQLException if there is a problem reading the Project information from the IVEN database.
	 */
	public static boolean loadIvenProjects(ToDmgDts porter, Connection connection, TreeMap projects) throws SQLException {
		// Define the success flag used to return the success of reading the Project information from the database.
		boolean success = true;
		// Define the SQL to read the Project information from the database.
		String sql = "SELECT pjname, full_name, storm_id_prefix, begin_date, end_date, minlat, maxlat, minlon, maxlon, admin_notes, charge_num FROM projects";
		// Define the statement to execute on the database.
		PreparedStatement stmt = connection.prepareStatement(sql);
		// Execute the statement and obtain the results of the query.
		ResultSet results = stmt.executeQuery();
		
		// Loop through each row of the query results.
		while (results.next()) {
			// Pull out the project ID from the current row of the results.
			String projectId = results.getString(1);
			// Create a new Project if it is not already in the mapping.
			if (!projects.containsKey(projectId)) {
				projects.put(projectId, new Project(projectId));
			}
			// Load the Project from the mapping with the current row's project ID.
			Project project = (Project)projects.get(projectId);

			// Set the full name of the project.
			try { project.setFullName(results.getString(2)); }
			// Handle the exception when the name does not match a previously set value.
			catch (MergeException e) {
				// Update the porting class with the exception.
				porter.appendMergeException(e);
				// There is a conflict, so the entire port failed.
				success = false;
			}
			
			// Add the data set prefix to the Project.  This does not need to test to 
			// see if it already exists since a Project may have multiple prefixes.
			project.addDatasetPrefix(results.getString(3));

			// Set the begin date of the project.
			try { project.setBeginDate(results.getDate(4)); }
			// Handle the exception when the date does not match a previously set value.
			catch (MergeException e) {
				// Update the porting class with the exception.
				porter.appendMergeException(e);
				// There is a conflict, so the entire port failed.
				success = false;
			}
			
			// Set the end date of the project.
			try { project.setEndDate(results.getDate(5)); }
			// Handle the exception when the date does not match a previously set value.
			catch (MergeException e) {
				// Update the porting class with the exception.
				porter.appendMergeException(e);
				// There is a conflict, so the entire port failed.
				success = false;
			}

			// Handle a null minimum latitude
			if (results.getString(6) == null) {
				// Update the porting class with the new exception.
				porter.appendMergeException(new MergeException("Project "+project.getProjectId()+" has a null min latitude."));
				// This is a conflict, so the entire port failed.
				success = false;
			} else {
				// Set the minimum latitude of the project's area of interest.
				try { project.setMinLatitude(results.getDouble(6)); }
				// Handle the exception when the latitude does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}
			}

			// Handle a null maximum latitude
			if (results.getString(7) == null) {
				// Update the porting class with the new exception.
				porter.appendMergeException(new MergeException("Project "+project.getProjectId()+" has a null max latitude."));
				// This is a conflict, so the entire port failed.
				success = false;
			} else {
				// Set the maximum latitude of the project's area of interest.
				try { project.setMaxLatitude(results.getDouble(7)); }
				// Handle the exception when the latitude does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}
			}
			
			// Handle a null minimum longitude
			if (results.getString(8) == null) {
				// Update the porting class with the new exception.
				porter.appendMergeException(new MergeException("Project "+project.getProjectId()+" has a null min lonitude."));
				// This is a conflict, so the entire port failed.
				success = false;
			} else {
				// Set the minimum longitude of the project's area of interest.
				try { project.setMinLongitude(results.getDouble(8)); }
				// Handle the exception when the longitude does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}
			}

			// Handle a null maximum longitude
			if (results.getString(9) == null) {
				// Update the porting class with the new exception.
				porter.appendMergeException(new MergeException("Project "+project.getProjectId()+" has a null max lonitude."));
				// This is a conflict, so the entire port failed.
				success = false;
			} else {
				// Set the maximum longitude of the project's area of interest.
				try { project.setMaxLongitude(results.getDouble(9)); }
				// Handle the exception when the longitude does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}
			}

			// Set the project note.
			try { project.setProjectNote(results.getString(10)); }
			// Handle the exception when the note does not match a previously set value.
			catch (MergeException e) {
				// Update the porting class with the exception.
				porter.appendMergeException(e);
				// There is a conflict, so the entire port failed.
				success = false;
			}

			// Set the project's charge number
			try { project.setChargeNumber(results.getString(11)); }
			// Handle the exception when the charge number does not match a previously set value.
			catch (MergeException e) {
				// Update the porting class with the exception.
				porter.appendMergeException(e);
				// There is a conflict, so the entire port failed.
				success = false;
			}
		}
		
		// Close the open database streams and ignore any exceptions.
		try { results.close(); } catch (SQLException e) {}
		try { stmt.close(); } catch (SQLException e) {}
		
		// All of the projects have been processed, so we can return the success flag now.
		return success;
	}

	/**
	 * Set the flag that marks this Project as an active Project.
	 * @param flag The active Project flag.
	 * @throws MergeException if the specified flag does not match a previously set value.
	 */
	public void setActive(boolean flag) throws MergeException {
		// Set the flag if it has never been set before. 
		if (active == null) { active = new Boolean(flag); }
		// Make sure the specified flag is the same as the previously set flag.
		else if (flag != active.booleanValue()) {
			throw new MergeException("Project "+getProjectId()+" has a mismatched active flag.");
		}
	}
	
	/**
	 * Set the begin date of the project's time of interest.
	 * @param date The Project's begin date.
	 * @throws MergeException if the date is <code>null</code> or does not match a previously set value.
	 */
	public void setBeginDate(java.sql.Date date) throws MergeException {
		// Handle a null date
		if (date == null) {
			throw new MergeException("Project "+getProjectId()+" has a null begin date.");
		}
		// Set the begin date for the first time.
		else if (beginDate == null) { beginDate = date; }
		// Make sure the date matches a previously set value.
		else if (beginDate.compareTo(date) != 0) {
			throw new MergeException("Project "+getProjectId()+" has a mismatched begin date: "+beginDate+" vs "+date);
		}
	}
	
	/**
	 * Set the charge number for the Project.
	 * @param charge The Project's charge number.
	 * @throws MergeException if the change number does not match a previously set value.
	 */
	public void setChargeNumber(String charge) throws MergeException {
		// Handle a null charge number.
		charge = charge == null ? "" : charge.trim();
		// Set the charge number for the first time.
		if (chargeNumber == null) { chargeNumber = charge; }
		// Make sure the charge number matches a previously set value.
		else if (chargeNumber.compareTo(charge) != 0) {
			throw new MergeException("Project "+getProjectId()+" has a mismatched charge number: "+chargeNumber+" vs "+charge);
		}
	}
	
	/**
	 * Set the end date of the project's time of interest.
	 * @param date The Project's end date.
	 * @throws MergeException if the date is <code>null</code> or does not match a previously set value.
	 */
	public void setEndDate(java.sql.Date date) throws MergeException {
		// Handle a null date
		if (date == null) {
			throw new MergeException("Project "+getProjectId()+" has a null end date.");
		}
		// Set the end date for the first time.
		else if (endDate == null) { endDate = date; }
		// Make sure the date matches a previously set value.
		else if (endDate.compareTo(date) != 0) {
			throw new MergeException("Project "+getProjectId()+" has a mismatched end date: "+beginDate+" vs "+date);
		}
	}
	
	/**
	 * Set the full name for the Project.
	 * @param name The Project's full name.
	 * @throws MergeException if the name is <code>null</code>, the empty String, or if the
	 * name does not match a previously set value.
	 */
	public void setFullName(String name) throws MergeException {
		// Handle an unacceptable name value.
		if (name == null || name.trim().equals("")) {
			throw new MergeException("Project "+getProjectId()+" has a null or empty full name.");
		}
		// Set the name for the first time.
		else if (fullName == null) { fullName = name.trim(); }
		// Make sure the name matches a previously set value.
		else if (fullName.compareTo(name.trim()) != 0) {
			throw new MergeException("Project "+getProjectId()+" has a mismatched name: "+fullName+" vs "+name);
		}
	}
	
	/**
	 * Set the maximum latitude of the Project's area of interest.
	 * @param lat The Project's maximum latitude.
	 * @throws MergeException if the latitude does not match a previously set value.
	 */
	public void setMaxLatitude(double lat) throws MergeException {
		// Set the latitude for the first time.
		if (maxLat == null) { maxLat = new Double(lat); }
		// Make sure the latitude matches the previously set value.
		else if (maxLat.doubleValue() != lat) {
			throw new MergeException("Project "+getProjectId()+" has a mismatched max latitude: "+maxLat+" vs "+lat);
		}
	}
	
	/**
	 * Set the maximum longitude of the Project's area of interest.
	 * @param lon The Project's maximum longitude.
	 * @throws MergeException if the longitude does not match a previously set value.
	 */
	public void setMaxLongitude(double lon) throws MergeException {
		// Set the longitude for the first time.
		if (maxLon == null) { maxLon = new Double(lon); }
		// Make sure the longitude matches the previously set value.
		else if (maxLon.doubleValue() != lon) {
			throw new MergeException("Project "+getProjectId()+" has a mismatched max longitude: "+maxLon+" vs "+lon);
		}
	}
	
	/**
	 * Set the minimum latitude of the Project's area of interest.
	 * @param lat The Project's minimum latitude.
	 * @throws MergeException if the latitude does not match a previously set value.
	 */
	public void setMinLatitude(double lat) throws MergeException {
		// Set the latitude for the first time.
		if (minLat == null) { minLat = new Double(lat); }
		// Make sure the latitude matches the previously set value.
		else if (minLat.doubleValue() != lat) {
			throw new MergeException("Project "+getProjectId()+" has a mismatched min latitude: "+minLat+" vs "+lat);
		}
	}
	
	/**
	 * Set the minimum longitude of the Project's area of interest.
	 * @param lon The Project's minimum longitude.
	 * @throws MergeException if the longitude does not match a previously set value.
	 */
	public void setMinLongitude(double lon) throws MergeException {
		// Set the longitude for the first time.
		if (minLon == null) { minLon = new Double(lon); }
		// Make sure the longitude matches the previously set value.
		else if (minLon.doubleValue() != lon) {
			throw new MergeException("Project "+getProjectId()+" has a mismatched min longitude: "+minLon+" vs "+lon);
		}
	}
	
	/**
	 * Set the note for the project.
	 * @param text The note text.
	 * @throws MergeException if the note does not matche a previously set value.
	 */
	public void setProjectNote(String text) throws MergeException {
		// Ignore any null or empty notes.
		if (text == null || text.trim().equals("")) { return; }
		// Set the note for the first time.
		else if (note == null) { note = text.trim(); }
		// Make sure the note matches the previously set value.
		else if (note.compareTo(text.trim()) != 0) {
			throw new MergeException("Project "+getProjectId()+" has a mismatched note: '"+note+"' vs '"+text+"'");
		}
	}
	
	/**
	 * Insert the provided projects into the DTS database.
	 * @param porter The controlling object of the porting process.
	 * @param connection The connection to the DTS database.
	 * @param projects The mapping of projects to be inserted into the DTS database.
	 * @param users The mapping of user already in the DTS database.
	 * @param noteTypes The mapping of note types already in the DTS database.
	 * @return <code>true</code> if all of the projects (and its pieces) were successfully added to
	 * the database, <code>false</code> if there were one or more problems.
	 * @throws SQLException if there is a problem executing the statements on the database.
	 */
	public static boolean storeDataTrackingSystemProjects(ToDmgDts porter, Connection connection, Map projects, Map users, Map noteTypes) throws SQLException {
		// Define the success flag used to return the success of storing the Project information into the database.
		boolean success = true;
		// Define the SQL to store the Project information into the database.
		String projectSql = "INSERT INTO project(project_id, full_name, begin_date, end_date, minlat, maxlat, minlon, maxlon, charge_number, active_flag) VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";
		// Define the statement to execute on the database.
		PreparedStatement projectStmt = connection.prepareStatement(projectSql);
		// Define the SQL to store the prefix information into the database.
		String prefixSql = "INSERT INTO dataset_prefix_project(project_id, dataset_id_prefix) VALUES(?, ?)";
		// Define the statement to execute on the database.
		PreparedStatement prefixStmt = connection.prepareStatement(prefixSql);
		// Define the SQL to attach a note to a project.
		String noteSql = "INSERT INTO project_note(note_id, project_id, note_type_id) VALUES(?, ?, ?)";
		// Define the statement to execute on the database.
		PreparedStatement noteStmt = connection.prepareStatement(noteSql);
		
		// Define an iterator over all of the projects in the mapping.
		Iterator itr = projects.values().iterator();
		// Loop through each project in the iterator.
		while (itr.hasNext()) {
			// Pull out the current project from the iterator.
			Project project = (Project)itr.next();
			
			// Load the statement with the values specific for this project.
			projectStmt.setString(1, project.getProjectId());
			projectStmt.setString(2, project.getName());
			projectStmt.setDate(3, project.getBeginDate());
			projectStmt.setDate(4, project.getEndDate());
			projectStmt.setDouble(5, project.getMinLatitude());
			projectStmt.setDouble(6, project.getMaxLatitude());
			projectStmt.setDouble(7, project.getMinLongitude());
			projectStmt.setDouble(8, project.getMaxLongitude());
			projectStmt.setString(9, project.getChargeNumber());
			projectStmt.setBoolean(10, project.isActive());

			
			boolean currentSuccess = true;
			
			// Execute the statement to put it into the database.
			try { projectStmt.execute(); }
			catch (SQLException e) {
				currentSuccess = false;
				success = false;
				porter.appendMergeException(new MergeException("Unable to insert project "+project.getProjectId()+":  "+e.getMessage()));
			}

			
			// Only try to associated other information with the project if the project
			// was successfully added to the database.
			if (currentSuccess) {
				// Store the data set prefixes into the database.
				
				// Set the project for the prefixes.  This only needs to be done once since it will be the
				// same for all of the prefixes for the project.
				prefixStmt.setString(1, project.getProjectId());
				// Loop through each prefix for the project.
				Iterator iter = project.getDatasetPrefixes().iterator();
				while (iter.hasNext()) {
					String prefix = (String)iter.next();
					// Load the statement with the values specific to this prefix.
					prefixStmt.setString(2, prefix);
					// Execute the statement to put it into the database.
					try { prefixStmt.execute(); }
					catch (SQLException e) {
						success = false;
						porter.appendMergeException(new MergeException("Unable to insert prefix "+prefix+" for project "+project.getProjectId()+":  "+e.getMessage()));
					}
				}
				
				
				// Store the project note into the database.
				
				// Create a new Note object for the note text.
				if (project.getProjectNote() != null && !project.getProjectNote().trim().equals("")) {
					Note note = new Note((User)users.get("local"), new Timestamp(project.getBeginDate().getTime()), project.getProjectNote());
					// Insert the note into the database.
					try { currentSuccess = note.storeInDataTrackingSystem(connection); }
					catch (SQLException e) {
						currentSuccess = false;
						success = false;
						porter.appendMergeException(new MergeException("Note for project "+project.getProjectId()+" could not be inserted.  "+e.getMessage()));
					}
					
					// The note successfully entered, so attach it to the project.
					if (currentSuccess) {
						try {
							// Assign the information to the statement.
							noteStmt.setInt(1, note.getId());
							noteStmt.setString(2, project.getProjectId());
							noteStmt.setInt(3, ((NoteType)noteTypes.get(NoteType.GENERAL_NOTE)).getId());
							// Execute the statement.
							noteStmt.execute();
						} catch (SQLException e) {
							// Handle any problems with the assignment or execution.
							success = false;
							porter.appendMergeException(new MergeException("Unable to assign the note to project "+project.getProjectId()+"  "+e.getMessage()));
						}
					}
				}
			}
		}
		
		// Close the open statement streams.
		try { noteStmt.close(); } catch (SQLException e) {}
		try { prefixStmt.close(); } catch (SQLException e) {}
		try { projectStmt.close(); } catch (SQLException e) {}
		
		// Return the status of all of the insert commands.
		return success;
	}
}
