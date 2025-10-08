package dts.port;

import java.sql.*;
import java.util.*;

/**
 * The Dataset class is the container and database servicer for Datasets.
 * 
 * @author jclawson
 */
public class Dataset implements Comparable {
	
	private Boolean questions;
	private String datasetId, name, readmeFile, remoteURL;
	private TreeMap checkTasks, datasetProjects, generalNotes, ingestTasks, loadTasks, processTasks, sourceDatasets;
	private TreeSet platforms;
	private User externalContact, internalContact;
	
	/**
	 * Create a new instance of a Dataset.
	 * @param datasetId The unique data set identifier.
	 */
	public Dataset(String datasetId) {
		this.datasetId = datasetId;
		
		datasetProjects = new TreeMap();
		sourceDatasets = new TreeMap();
		
		generalNotes = new TreeMap();
		
		ingestTasks = new TreeMap();
		processTasks = new TreeMap();
		loadTasks = new TreeMap();
		checkTasks = new TreeMap();
		
		platforms = new TreeSet();
	}
	
	/**
	 * Add a general note to the data set.
	 * @param date The entry date for the note.
	 * @param note The note text.
	 * @throws MergeException if the note already exists for the specified date and the text does not match.
	 */
	public void addGeneralNote(Timestamp date, String note) throws MergeException {
		// Only care about notes with data.
		if (note != null && !note.trim().equals("")) {			
			// Clean up the note text.
			note = note.trim();
			// Handle the case where the note is on a previously existing date and the text does not match between them.
			if (generalNotes.containsKey(date)) {
				if (!note.equals(generalNotes.get(date))) {
					throw new MergeException("Dataset "+getDatasetId()+" has different notes for entry date: "+date.toString());
				}
			}
			// It is a new note
			else {
				generalNotes.put(date, note);
			}
		}
	}
	
	/**
	 * Add a general note to the data set.
	 * @param project The project for the note.
	 * @param note The note text.
	 * @throws MergeException if the note already exists for the specified project's begin date and the text does not match.
	 */
	public void addGeneralNote(Project project, String note) throws MergeException {
		addGeneralNote(new Timestamp(project.getBeginDate().getTime()), note);
	}
	
	/**
	 * Associate a platform to the data set.
	 * @param platformId The identifier of the platform.
	 */
	public void addPlatform(int platformId) {
		Integer id = new Integer(platformId);
		// Only add the platform if it isn't already associated to the data set.
		if (platformId > 0 && platformId != 999 && !platforms.contains(id)) { platforms.add(id); }
	}
	
	/**
	 * Associate this data set to the specified Project.
	 * @param project The project to be associated to this data set.
	 */
	public void addProject(Project project) {
		// Only add the project if it isn't already associated to the data set.
		if (!datasetProjects.containsKey(project.getProjectId())) {
			datasetProjects.put(project.getProjectId(), new DatasetProject(this, project));
		}
	}
	
	/**
	 * Associate a source data set to this data set.
	 * @param source The source data set to be associated to this data set.
	 */
	public void addSourceDataset(Dataset source) {
		// Only add the source if it isn't already a source of the data set.
		if (!sourceDatasets.containsKey(source.getDatasetId())) {
			sourceDatasets.put(source.getDatasetId(), new DatasetSource(this, source));
		}
	}
	
	/**
	 * Perform a series of operations on a data set name to make it follow common naming conventions.
	 * @param name The data set name to be cleaned.
	 * @return The cleaned up data set name.
	 */
	private String cleanUpName(String name) {
		// Clean up the spacing so it is consistent.  Make multiple spaces a single space and remove extraneous whitespace.
		name = name.replaceAll("\\s+", " ").trim();
		// Replace old organizational names with the new ones.
		name = name.replaceAll("\\[(UCAR/JOSS|JOSS|NCAR/ATD|ATD|EOL)\\]$", "[NCAR/EOL]");
		// Remove categorizations from the start of the name.
		name = name.replaceFirst("^(Aircraft|Aircraft\\s+Radar|Ancillary|GPS|Lidar|Mesonet|Model|Profiler/Sodar|Radar|Radiation|Satellite|Soundings?|Sub\\-?[Ss]urface|Surface|Upper[\\s\\-]?Air):\\s*", "");
		// Remove the orange stars from the NAME IVEN data sets.
		name = name.replaceFirst("\\s*<font.+/font>\\s*$", "");
		// Make any formats in the data set name be consistent.
		name = name.replaceFirst("\\([Aa][Ss][Cc][Ii][Ii]\\)", "(ASCII)");
		name = name.replaceFirst("\\([Ee][Xx][Cc][Ee][Ll]\\)", "(Excel)");
		// Remove the years from the start of the data set name.
		
		return name.trim();
	}
	
	/**
	 * Read in the data set information from the Master Lists and compare the meta data with the meta data in the
	 * provided container.
	 * @param porter The porting controller class running the porting process.
	 * @param connection The IVEN database connection to use to read the User meta data.
	 * @param projects The mapping of Projects by Project ID.
	 * @param datasets The mapping of Datasets by Dataset ID used to store and merge the DTS information.
	 * @return <code>true</code> if the porting process proceeded successfully without any database read errors
	 * or conflicts between meta data.
	 * @throws SQLException if there is a problem reading the Dataset information from the IVEN database.
	 */
	public static boolean compareMasterListDatasets(ToDmgDts porter, Connection connection, TreeMap projects, TreeMap datasets) throws SQLException {
		// Define the success flag used to return the success of reading the Dataset information from the database.
		boolean success = true;
		// Define the SQL to read the Dataset information from the database.
		String sql = "SELECT dataset.dataset_id, name, author_pi, project_id FROM dataset JOIN dataset_project ON (dataset.dataset_id=dataset_project.dataset_id) WHERE dataset.dataset_id NOT LIKE 'ML.%'";
		// Define the statement to execute on the database.
		PreparedStatement stmt = connection.prepareStatement(sql);
		// Get the results of the query.
		ResultSet results = stmt.executeQuery();
		
		// Loop through each row of the results.
		while (results.next()) {
			// Extract the data set ID from the current row.
			String datasetId = results.getString(1);
			// Make sure the data set is in the DTS.
			if (!datasets.containsKey(datasetId)) {
				porter.appendMergeException(new MergeException("Dataset "+datasetId+" is not in the DTS."));
				success = false;
			}
			// The data set is in the DTS.
			else {
				// Get the data set out of the DTS mapping.
				Dataset dataset = (Dataset)datasets.get(datasetId);
				// Handle the data set name when there is not a PI/author.
				if (results.getString(3) == null || results.getString(3).equals("")) {
					try { dataset.setName(results.getString(2)); }
					// Handle the exception when the name does not match a previously set value.
					catch (MergeException e) {
						// Update the porting class with the exception.
						porter.appendMergeException(e);
						// There is a conflict, so the entire port failed.
						success = false;
					}
				}
				// Handle the data set name when there is a PI/author.
				else {
					try { dataset.setName(results.getString(2)+" ["+results.getString(3)+"]"); }
					// Handle the exception when the name does not match a previously set value.
					catch (MergeException e) {
						// Update the porting class with the exception.
						porter.appendMergeException(e);
						// There is a conflict, so the entire port failed.
						success = false;
					}
				}

				// Make sure the data set is associated to the project.
				dataset.addProject((Project)projects.get(results.getString(4)));

				// Make sure the ML flag is set for the data set in the DTS.
				try { dataset.setMasterListFlag(results.getString(4), true); }
				// Handle the exception when the name does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}
			}
		}
		
		// Close the open database streams and ignore any thrown exceptions.
		try { results.close(); } catch (SQLException e) {}
		try { stmt.close(); } catch (SQLException e) {}
		
		// All of the data sets have been processed, so the success flag can now be returned.
		return success;
	}
		
	/**
	 * Compare this Dataset with the specified Dataset for sort order.  This uses the natural sort
	 * order for the Dataset identifiers.
	 * @param dataset The Dataset to be compared with this Dataset.
	 * @return A negative integer, zero, or positive integer if this Dataset is less than, equal to,
	 * or greater than the specified Dataset.
	 */
	public int compareTo(Object dataset) {
		return getDatasetId().compareTo(((Dataset)dataset).getDatasetId());
	}

	/**
	 * Get the collection of approving/checking tasks for this data set.
	 * @return The Dataset's approve tasks.
	 */
	public Collection getApproveTasks() { return checkTasks.values(); }
	
	/**
	 * Get the Dataset identifier.
	 * @return The Dataset's identifier.
	 */
	public String getDatasetId() { return datasetId; }
	
	/**
	 * Get the mapping of general notes for the data set by the date they were entered
	 * @return The general notes mapping.
	 */
	public Map getGeneralNotes() { return generalNotes; }
	
	/**
	 * Get the User that is the external/source contact for the data set.
	 * @return The Dataset's external contact.
	 */
	public User getExternalContact() { return externalContact; }
	
	/**
	 * Get the collection of ingest tasks for this data set.
	 * @return The Dataset's ingest task set.
	 */
	public Collection getIngestTasks() { return ingestTasks.values(); }
	
	/**
	 * Get the User that is the internal contact for the data set.
	 * @return The data set's internal contact.
	 */
	public User getInternalContact() { return internalContact; }
	
	/**
	 * Get the collection of load tasks for this data set.
	 * @return The Dataset's load task set.
	 */
	public Collection getLoadTasks() { return loadTasks.values(); }
	
	/**
	 * Get the name of the data set.
	 * @return The data set's name.
	 */
	public String getName() { return name; }
	
	/**
	 * Get the collection of platform identifiers associated to the data set.
	 * @return The collection of platforms.
	 */
	public Collection getPlatforms() { return platforms; }
	
	/**
	 * Get the collection of processing tasks for this data set.
	 * @return The Dataset's processing tasks.
	 */
	public Collection getProcessTasks() { return processTasks.values(); }
	
	/**
	 * Get the collection of project associations for this data set.
	 * @return The associated project information for the data set.
	 */
	public Collection getProjectAssociations() { return datasetProjects.values(); }
	
	/**
	 * Get the location of the data set's readme/documentation file.
	 * @return The data set's readme file.
	 */
	public String getReadmeFile() {
		return readmeFile == null ? "" : readmeFile.trim()	;
	}
	
	/**
	 * Get the URL of the data set's remote URL.
	 * @return The data set's remote URL.
	 */
	public String getRemoteURL() {
		return remoteURL == null ? "" : remoteURL.trim();
	}
	
	/**
	 * Get the collection of source data sets for this data set.
	 * @return The collection of source DatasetSources.
	 */
	public Collection getSourceDatasets() { return sourceDatasets.values(); }

	/**
	 * Determine if there are outstanding questions/issues with the data set.
	 * @return <code>true</code> if there are outstanding questions, <code>false</code> if there is not.
	 */
	public boolean hasQuestions() { 
		return questions == null ? false : questions.booleanValue();
	}
	
	/**
	 * Read in the Dataset information from the Data Loading Notes and store the meta data in the
	 * provided container.
	 * @param porter The porting controller class running the porting process.
	 * @param connection The DLN database connection to use to read the User meta data.
	 * @param projects The mapping of Projects by Project ID.
	 * @param users The mapping of Users by short name ID.
	 * @param datasets The mapping of Datasets by Dataset ID used to store and merge the DTS information.
	 * @return <code>true</code> if the porting process proceeded successfully without any database read errors
	 * or conflicts between meta data.
	 * @throws SQLException if there is a problem reading the Dataset information from the DLN database.
	 */
	public static boolean loadDlnDatasets(ToDmgDts porter, Connection connection, TreeMap projects, TreeMap users, TreeMap datasets) throws SQLException {
		// Define the success flag used to return the success of reading the Dataset information from the database.
		boolean success = true;
		// Define the SQL to read the Dataset information from the database.
		String sql = "SELECT storm_id, title, readme, master, checked, loaded, date, notes, project, loader, checker, int_contact, ext_contact, ext_email, ingest, archive, remote_url FROM dataset";
		// Define the statement to execute on the database.
		PreparedStatement stmt = connection.prepareStatement(sql);
		// Execute the statement and obtain the results fo the query.
		ResultSet results = stmt.executeQuery();
		
		// Loop through each row of the query results.
		while (results.next()) {
			// Pull out the data set ID from the current row of the results.
			String datasetId = results.getString(1);
			
			// Make sure the data set ID is an acceptable value.
			if (datasetId == null || datasetId.trim().equals("99.999") || datasetId.trim().equals("XX.xxx")) {
				porter.appendMergeException(new MergeException("Dataset has an invalid dataset id of: "+datasetId));
			}
			// The ID is acceptable, so continue reading the results.
			else {			
				// Create a new Dataset if it is not already in the mapping.
				if (!datasets.containsKey(datasetId)) {
					datasets.put(datasetId, new Dataset(datasetId));
				}
				
				// Load the Dataset from the mapping with the current row's data set ID.
				Dataset dataset = (Dataset)datasets.get(datasetId);
				
				// Read in the project ID for the data set.
				String projectId = results.getString(9);
				// Make sure the Project exists in the mapping.
				if (!projects.containsKey(projectId)) {
					porter.appendMergeException(new MergeException("Dataset "+dataset.getDatasetId()+" has an unknown project:  "+projectId));
					success = false;
				}
				// Associate the Project to the Dataset.
				else {
					dataset.addProject((Project)projects.get(projectId));
				}
				
				// Read in the entry date for the data set.
				Timestamp date = results.getTimestamp(7);
				// Make sure the date is an acceptable value.
				if (date == null) {
					porter.appendMergeException(new MergeException("Dataset "+dataset.getDatasetId()+" has a null entry date for project: "+projectId));
					success = false;
				}
				
				// Set the name/title of the data set.
				try { dataset.setName(results.getString(2)); }
				// Handle the exception when the name has an illegal value or does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}
				
				// Set the readme/documentation flag for the Dataset/date pair.  (Only worry if the value is not null.)				
				if (results.getString(3) != null) {
					try { dataset.setReadmeFlag(date, results.getBoolean(3)); }
					// Handle the exception when the flag does not match a previously set value.
					catch (MergeException e) {
						// Update the porting class with the exception.
						porter.appendMergeException(e);
						// There is a conflict, so the entire port failed.
						success = false;
					}
				}
				
				// Set the master list flag for the Dataset/Project pair.  (Only worry if the value is not null.)
				if (results.getString(4) != null) {
					try { dataset.setMasterListFlag(projectId, results.getBoolean(4)); }
					// Handle the exception when the flag does not match a previously set value.
					catch (MergeException e) {
						// Update the porting class with the exception.
						porter.appendMergeException(e);
						// There is a conflict, so the entire port failed.
						success = false;
					}
				}

				// Set the checked flag for the Dataset/Date pair.  (Only worry if the value is not null.)
				if (results.getString(5) != null) {
					try { dataset.setChecked(date, results.getBoolean(5)); }
					// Handle the exception when the flag does not match a previously set value.
					catch (MergeException e) {
						// Update the porting class with the exception.
						porter.appendMergeException(e);
						// There is a conflict, so the entire port failed.
						success = false;
					}
				}

				// Set the loaded flag for the Dataset/Date pair.  (Only worry if the value is not null.)
				if (results.getString(6) != null) {
					try { dataset.setLoaded(date, results.getBoolean(6)); }
					// Handle the exception when the flag does not match a previously set value.
					catch (MergeException e) {
						// Update the porting class with the exception.
						porter.appendMergeException(e);
						// There is a conflict, so the entire port failed.
						success = false;
					}
				}
				
				// Add a general note to the Dataset for the specified Date.
				try { dataset.addGeneralNote(date, results.getString(8)); }
				// Handle the exception when the note does not match a previously set value for that date.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}
				
				// Set the User tasked to loading the Dataset for the current Date.
				String loaderId = results.getString(10);
				// Ignore the User if they are not a real User.
				if (!loaderId.equalsIgnoreCase("Unassigned") && !loaderId.equalsIgnoreCase("Other")) {
					try { dataset.setLoader(date, (User)users.get(loaderId.toLowerCase())); }
					// Handle the exception when the loader does not match a previously set value for the date.
					catch (MergeException e) {
						// Update the porting class with the exception.
						porter.appendMergeException(e);
						// There is a conflict, so the entire port failed.
						success = false;
					}
				}
				
				// Set the User tasked to checking the Dataset for the current Date.
				String checkerId = results.getString(11);
				// Ignore the User if they are not a real User.
				if (!checkerId.equalsIgnoreCase("Unassigned") && !checkerId.equalsIgnoreCase("Other")) {
					try { dataset.setChecker(date, (User)users.get(checkerId.toLowerCase())); }
					// Handle the exception when the checker does not match a previously set value for the date.
					catch (MergeException e) {
						// Update the porting class with the exception.
						porter.appendMergeException(e);
						// There is a conflict, so the entire port failed.
						success = false;
					}
				}
				
				// Set the internal contact for the Dataset.
				String intId = results.getString(12);
				// Ignore the User if they are not a real User.
				if (!intId.equalsIgnoreCase("Unassigned") && !intId.equalsIgnoreCase("Other")) {
					try { dataset.setInternalContact((User)users.get(intId.toLowerCase())); }
					// Handle the exception when the internal contact is an illegal value or does not match a previously set value.
					catch (MergeException e) {
						// Update the porting class with the exception.
						porter.appendMergeException(e);
						// There is a conflict, so the entire port failed.
						success = false;
					}
				}
				
				// Set the external contact for the Dataset.
				try { dataset.setExternalContact(User.findExternalContact(users, results.getString(13), results.getString(14))); }
				// Handle the exception when the external contact could not be created or does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}
				
				// Set the directory where the data was ingested for the Dataset.
				try { dataset.setIngestDirectory(date, results.getString(15)); }
				// Handle the exception when the ingest directory does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}
				
				// Set the directory where the data to be archived can be found.
				try { dataset.setDataToArchiveDirectory(date, results.getString(15)); }
				// Handle the exception when the directory does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}
				
				// Set the directory where the data is to be archived.
				try { dataset.setArchiveDirectory(date, results.getString(16)); }
				// Handle the exception when the directory does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}
				
				// Set the remote URL for the Dataset.
				try { dataset.setRemoteURL(results.getString(17)); }
				// Handle the exception when the URL does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}
			}
		}
		
		// Close the open database stream and ignore any exceptions.
		try { results.close(); } catch (SQLException e) {}
		try { stmt.close(); } catch (SQLException e) {}
		
		// All of the data sets have been processed, so the success flag can now be returned.
		return success;
	}
	
	/**
	 * Read in the data set information from the EMDAC database and store the meta data in the
	 * provided container.
	 * @param porter The porting controller class running the porting process.
	 * @param connection The EMDAC database connection to use to read the Dataset meta data.
	 * @param projects The mapping of Projects by Project ID.
	 * @param users The mapping of Users by short name ID.
	 * @param datasets The mapping of Datasets by data set ID used to store and merge the DTS information.
	 * @return <code>true</code> if the porting process proceeded successfully without any database read errors
	 * or conflicts between meta data.
	 * @throws SQLException if there is a problem reading the Dataset information from the EMDAC database.
	 */
	public static boolean loadEmdacDatasets(ToDmgDts porter, Connection connection, TreeMap projects, TreeMap users, TreeMap datasets) throws SQLException {
		// Define the success flag used to return the success of reading the Dataset information from the database.
		boolean success = true;
		// Define the SQL to read the data set information from the database.
//		String sql = "SELECT dataset.dataset_id, name, source.contact_short_name, internal.contact_short_name FROM dataset LEFT JOIN contact AS source ON (dataset.source_contact_id=source.contact_id) LEFT JOIN contact AS internal ON (dataset.internal_contact_id=internal.contact_id) LEFT JOIN dataset_project ON (dataset.dataset_id=dataset_project.dataset_id) WHERE project_id=?";
		String sql = "SELECT dataset.archive_ident, dataset.title, source.short_name, internal.short_name FROM dataset LEFT JOIN contact AS source ON (dataset.point_of_contact_id=source.id) LEFT JOIN contact AS internal ON (dataset.internal_contact_id=internal.id) LEFT JOIN dataset_project ON (dataset.id=dataset_project.dataset_id) LEFT JOIN project ON (project.id=dataset_project.project_id) WHERE project.name=?";
		// Define the statement to execute on the database.
		PreparedStatement stmt = connection.prepareStatement(sql);
		
		// Define an iterator to loop through all of the known projects.
		Iterator itr = projects.values().iterator();
		// Loop through the known projects.
		while (itr.hasNext()) {
			// Pull out the current project from the iterator.
			Project project = (Project)itr.next();
			
			// Assign the project to the statement to pull out the data sets.
			stmt.setString(1, project.getProjectId());
		
			// Execute the statement and obtain the results for the query.
			ResultSet results = stmt.executeQuery();
			
			// Loop through each row of the query results.
			while (results.next()) {
				// Pull out the data set ID from the current row of the results.
				String datasetId = results.getString(1);
				
				// Make sure the data set ID is an acceptable value.
				// Does this also account for datasets in ARCSS and BSIERP (e.g. 106.ARCSS400 or 245.B99-001)? - orin
				if (datasetId == null || datasetId.trim().equals("") || datasetId.trim().equals("99.999") || datasetId.trim().equals("XX.xxx")) {
					porter.appendMergeException(new MergeException("Dataset "+results.getString(1)+" has an invalid dataset id of: "+datasetId));
				}
				// The ID is acceptable, so continue reading the results.
				else {
					// The EMDAC data set isn't in the DTS, so add it.
					if (!datasets.containsKey(datasetId)) {
						datasets.put(datasetId, new Dataset(datasetId));
					}
					
					// Load the Dataset from the mapping with the current row's data set ID.
					Dataset dataset = (Dataset)datasets.get(datasetId);
					
					dataset.addProject(project);
					
					// Set the name/title of the data set.
					if (dataset.getName() == null || !dataset.getName().equalsIgnoreCase("unassigned")) {
						try { dataset.setName(results.getString(2)); }
						// Handle the exception when the name has an illegal value or does not match a previously set value.
						catch (MergeException e) {
							// Update the porting class with the exception.
							porter.appendMergeException(e);
							// There is a conflict, so the entire port failed.
							success = false;
						}
					}
					
					// Set the external contact for the data set.
					String externalId = results.getString(3);
					if (externalId != null) {
						try { dataset.setExternalContact((User)users.get(externalId.toLowerCase())); }
						// Handle the exception when the processor does not match a previously set value for the date.
						catch (MergeException e) {
							// Update the porting class with the exception.
							porter.appendMergeException(e);
							// There is a conflict, so the entire port failed.
							success = false;
						}
					}
					
					// Set the internal contact for the data set.
					String internalId = results.getString(4).trim();
					if (internalId != null && !internalId.equals("local")) {
						try { dataset.setInternalContact((User)users.get(internalId.toLowerCase())); }
						// Handle the exception when the processor does not match a previously set value for the date.
						catch (MergeException e) {
							// Update the porting class with the exception.
							porter.appendMergeException(e);
							// There is a conflict, so the entire port failed.
							success = false;
						}
					}
				}
			}
		
			// Close the streams cleanly and ignore any exceptions.
			try { results.close(); } catch (SQLException e) {}
		}
		
		// Close the streams cleanly and ignore any exceptions.
		try { stmt.close(); } catch (SQLException e) {
			
		}
		// All of the data sets have been processed, so the success flag can now be returned.
		return success;		
	}
	
	/**
	 * Read in the data set information from the processing Inventory and store the meta data in the
	 * provided container.
	 * @param porter The porting controller class running the porting process.
	 * @param connection The IVEN database connection to use to read the User meta data.
	 * @param projects The mapping of Projects by Project ID.
	 * @param users The mapping of Users by short name ID.
	 * @param datasets The mapping of Datasets by Dataset ID used to store and merge the DTS information.
	 * @return <code>true</code> if the porting process proceeded successfully without any database read errors
	 * or conflicts between meta data.
	 * @throws SQLException if there is a problem reading the Dataset information from the IVEN database.
	 */
	public static boolean loadIvenDatasets(ToDmgDts porter, Connection connection, TreeMap projects, TreeMap users, TreeMap datasets) throws SQLException {
		// Define the success flag used to return the success of reading the Dataset information from the database.
		boolean success = true;
		// Define the SQL to read the Dataset information from the database.
		String sql = "SELECT dataset_id.dataset_id, datasets.pjname, datasets.pdname, product_id.dataset_id, datasets.dsname, user_processing, raw_data, final_data, station_info, software, plots, platform_type, status_notes, admin_notes, status, exclude, questions, how_to, readme FROM datasets LEFT JOIN dataset_id ON (datasets.pjname=dataset_id.pjname AND datasets.pdname=dataset_id.pdname AND datasets.dsname=dataset_id.dsname) LEFT JOIN product_id ON (datasets.pjname=product_id.pjname AND datasets.pdname=product_id.pdname)";
		// Define the statement to execute on the database.
		PreparedStatement stmt = connection.prepareStatement(sql);
		// Execute the statement and obtain the results for the query.
		ResultSet results = stmt.executeQuery();
		
		// Define a set of data sets that will require at least one source data set.
		TreeSet needSourceDatasets = new TreeSet();
		
		// Loop through each row of the query results.
		while (results.next()) {
			// Pull out the data set ID from the current row of the results.
			String datasetId = results.getString(1);
			
			// Make sure the data set ID is an acceptable value.
			if (datasetId == null || datasetId.trim().equals("") || datasetId.trim().equals("99.999") || datasetId.trim().equals("XX.xxx")) {
				porter.appendMergeException(new MergeException("Dataset "+results.getString(2)+":"+results.getString(3)+":"+results.getString(5)+" has an invalid dataset id of: "+datasetId));
			}
			// The ID is acceptable, so continue reading the results.
			else {			
				// The data set needs to exist in the mapping before it gets here.
				if (!datasets.containsKey(datasetId)) {
					porter.appendMergeException(new MergeException("Dataset "+datasetId+" is not in the DLN."));
				}
				
				// Load the Dataset from the mapping with the current row's data set ID.
				Dataset dataset = (Dataset)datasets.get(datasetId);
				
				// Add this data set as one that requires a source data set.
				needSourceDatasets.add(dataset);
				
				// Read in the project ID for the data set.
				Project project = (Project)projects.get(results.getString(2));
				// Make sure the Project exists in the mapping.
				if (project == null) {
					porter.appendMergeException(new MergeException("Dataset "+dataset.getDatasetId()+" has an unknown project:  "+results.getString(2)));
					success = false;
				}
				// Associate the Project to the Dataset.
				else {
					dataset.addProject(project);
				}
				
				// Read in the product's dataset ID for the data set.
				String productId = results.getString(4);
				// Make sure the product exists as a data set.
				if (!datasets.containsKey(productId)) {
					porter.appendMergeException(new MergeException("Dataset "+dataset.getDatasetId()+" has an unknown product:  "+productId));
					success = false;
				}
				// Associate this data set as a source for the product.
				else {
					((Dataset)datasets.get(productId)).addSourceDataset(dataset);
				}
				
				// Set the name/title of the data set.
				try { dataset.setName(results.getString(5)); }
				// Handle the exception when the name has an illegal value or does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}

				// Set the user processing the data set.
				String processorId = results.getString(6);
				// Ignore the User if they are not a real User.
				if (!processorId.equalsIgnoreCase("Unassigned") && !processorId.equalsIgnoreCase("Other")) {
					try { dataset.setProcessor(project, (User)users.get(processorId.toLowerCase())); }
					// Handle the exception when the processor does not match a previously set value for the date.
					catch (MergeException e) {
						// Update the porting class with the exception.
						porter.appendMergeException(e);
						// There is a conflict, so the entire port failed.
						success = false;
					}
				}
				
				// Set the final data directory for the processed data.
				try { dataset.setFinalDataDirectory(project, results.getString(8)); }
				// Handle the exception when the directory does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}
				
				// Set the location of the station file for the processed data.
				try { dataset.setStationFile(project, results.getString(9)); }
				// Handle the exception when the file does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}

				// Set the location of the software repository used for processing the data.
				try { dataset.setRepository(project, results.getString(10)); }
				// Handle the exception when the repository does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}

				// Set the location of the plots directory for the processed data.
				try { dataset.setPlotsDirectory(project, results.getString(11)); }
				// Handle the exception when the directory does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}

				// Add the platform ID to this data set and the product that this is a source for.
				dataset.addPlatform(results.getInt(12));
				((Dataset)datasets.get(productId)).addPlatform(results.getInt(12));
				
				// Set the processing notes for the data set.
				try { dataset.setProcessingNotes(project, results.getString(13)); }
				// Handle the exception when the note does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}
				
				// Add general notes to the data set.
				try { dataset.addGeneralNote(project, results.getString(14)); }
				// Handle the exception when the note does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}
				
				// Set the processing status of the data set.
				try { dataset.setProcessingStatus(project, results.getString(15)); }
				// Handle the exception when the status does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}
				
				// Set the exclusion flag for the data set in the product.
				try { ((Dataset)datasets.get(productId)).setExcluded(dataset, results.getBoolean(16)); }
				// Handle the exception when the flag does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}

				// Set the questions flag for the data set.
				try { dataset.setHasQuestions(results.getBoolean(17)); }
				// Handle the exception when the flag does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}
				
				// Set the location of the how to file for the processing of the data.
				try { dataset.setHowToFile(project, results.getString(18)); }
				// Handle the exception when the file does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}

				// Set the location of the readme file for this data set.
				try { dataset.setReadmeFile(results.getString(19)); }
				// Handle the exception when the file does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}
			}
		}
		
		// Close the streams cleanly and ignore any exceptions.
		try { results.close(); } catch (SQLException e) {}
		try { stmt.close(); } catch (SQLException e) {}
		
		// Define the SQL to read the source information for a specific data set.
		sql = "SELECT source_id FROM dataset_source WHERE dataset_id=?";
		// Define the statement to read the information from the database.
		stmt = connection.prepareStatement(sql);
		// Define an iterator to loop through the set of data sets that require a source data set.
		Iterator itr = needSourceDatasets.iterator();
		while (itr.hasNext()) {
			// Get the data set that is searching for source data sets.
			Dataset dataset = (Dataset)itr.next();
			// Assign the data set ID to the statement so it knows to search for this data sets sources.
			stmt.setString(1, dataset.getDatasetId());
			// Get the results of the query.
			ResultSet result = stmt.executeQuery();
			// Loop through all of the rows from the results of the query.
			while (result.next()) {
				// Pull out the source data set ID from the current row of the results.
				String sourceId = result.getString(1);
				// Make sure the source data set actually exists.
				if (!datasets.containsKey(sourceId)) {
					porter.appendMergeException(new MergeException("Dataset "+dataset.getDatasetId()+" has an unknown source dataset "+sourceId));
					success = false;
				}
				// Add the source data set as a source to its parent data set.
				else {
					dataset.addSourceDataset((Dataset)datasets.get(sourceId));
				}
			}
			
			// Make sure that the data set has at least one source data set.
			if (dataset.getSourceDatasets().size() == 0) {
				porter.appendMergeException(new MergeException("Dataset "+dataset.getDatasetId()+" does not have any source datasets."));
				success = false;
			}
			
			// Close the open results stream.
			try { result.close(); } catch (SQLException e) {}
		}
		
		// Close the open statement stream.
		try { stmt.close(); } catch (SQLException e) {}
		
		// All of the data sets have been processed, so the success flag can now be returned.
		return success;
	}
	
	/**
	 * Read in the product information from the processing Inventory and store the meta data in the
	 * provided container.
	 * @param porter The porting controller class running the porting process.
	 * @param connection The IVEN database connection to use to read the User meta data.
	 * @param projects The mapping of Projects by Project ID.
	 * @param users The mapping of Users by short name ID.
	 * @param datasets The mapping of Datasets by Dataset ID used to store and merge the DTS information.
	 * @return <code>true</code> if the porting process proceeded successfully without any database read errors
	 * or conflicts between meta data.
	 * @throws SQLException if there is a problem reading the Dataset information from the IVEN database.
	 */
	public static boolean loadIvenProducts(ToDmgDts porter, Connection connection, TreeMap projects, TreeMap users, TreeMap datasets) throws SQLException {
		// Define the success flag used to return the success of reading the Dataset information from the database.
		boolean success = true;
		// Define the SQL to read the Dataset information from the database.
		String sql = "SELECT dataset_id, products.pjname, products.pdname, status FROM products LEFT JOIN product_id ON (products.pjname=product_id.pjname AND products.pdname=product_id.pdname)";
		// Define the statement to execute on the database.
		PreparedStatement stmt = connection.prepareStatement(sql);
		// Execute the statement and obtain the results for the query.
		ResultSet results = stmt.executeQuery();
		
		// Loop through each row of the query results.
		while (results.next()) {
			// Pull out the data set ID from the current row of the results.
			String datasetId = results.getString(1);
			
			// Make sure the data set ID is an acceptable value.
			if (datasetId == null || datasetId.trim().equals("") || datasetId.trim().equals("99.999") || datasetId.trim().equals("XX.xxx")) {
				porter.appendMergeException(new MergeException("Product "+results.getString(2)+":"+results.getString(3)+" has an invalid dataset id of: "+datasetId));
			}
			// The ID is acceptable, so continue reading the results.
			else {			
				// The data set needs to exist in the mapping before it gets here.
				if (!datasets.containsKey(datasetId)) {
					porter.appendMergeException(new MergeException("Product "+datasetId+" is not in the DLN."));
				}
				
				// Load the Dataset from the mapping with the current row's data set ID.
				Dataset dataset = (Dataset)datasets.get(datasetId);
				
				// Read in the project ID for the data set.
				String projectId = results.getString(2);
				// Make sure the Project exists in the mapping.
				if (!projects.containsKey(projectId)) {
					porter.appendMergeException(new MergeException("Product "+dataset.getDatasetId()+" has an unknown project:  "+projectId));
					success = false;
				}
				// Associate the Project to the Dataset.
				else {
					dataset.addProject((Project)projects.get(projectId));
				}
				
				// Set the name/title of the data set.
				try { dataset.setName(results.getString(3)); }
				// Handle the exception when the name has an illegal value or does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}

				// Set the processing status of the data set.
				try { dataset.setProcessingStatus((Project)projects.get(projectId), results.getString(4)); }
				// Handle the exception when the status does not match a previously set value.
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
		try { stmt.close(); } catch (SQLException e) {}
		
		// All of the data sets have been processed, so the success flag can now be returned.
		return success;
	}
	
	/**
	 * Set the directory where the data is to be archived for the Dataset.
	 * @param date The date of the load task.
	 * @param directory The directory where the data is to be archived.
	 * @throws MergeException if the date is <code>null</code> or if the directory does not match a previously set value.
	 * @see dts.port.DatasetLoad#setArchiveDirectory(String)
	 */
	public void setArchiveDirectory(Timestamp date, String directory) throws MergeException {
		// Make sure that the date is not null.
		if (date == null) {
			throw new MergeException("Dataset "+getDatasetId()+" is trying to set the archive directory with a null date.");
		}
		// Create a new load task if it doesn't already exist.
		if (!loadTasks.containsKey(date)) {
			loadTasks.put(date, new DatasetLoad(this, date));
		}
		// Set the archive directory in the specific load task.
		((DatasetLoad)loadTasks.get(date)).setArchiveDirectory(directory);
	}
	
	/**
	 * Set the flag that marks the data set as checked.
	 * @param date The date of the checking task.
	 * @param flag The checked flag value.
	 * @throws MergeException if the date is <code>null</code> or if the flag does not match a previously set value.
	 * @see dts.port.DatasetCheck#setChecked(boolean)
	 */
	public void setChecked(Timestamp date, boolean flag) throws MergeException {
		// Make sure that the date is not null.
		if (date == null) {
			throw new MergeException("Dataset "+getDatasetId()+" is trying to set the archive directory with a null date.");
		}
		// Create a new check task if it doesn't already exist.
		if (!checkTasks.containsKey(date)) {
			checkTasks.put(date, new DatasetCheck(this, date));
		}
		// Set the flag in the specific check task.
		((DatasetCheck)checkTasks.get(date)).setChecked(flag);

	}
	
	/**
	 * Set the User tasked with checking this Dataset.
	 * @param date The date of the checking task.
	 * @param checker The checking User.
	 * @throws MergeException if the date is <code>null</code> or if the checker does not match a previously set value.
	 * @see dts.port.DatasetCheck#setChecker(User)
	 */
	public void setChecker(Timestamp date, User checker) throws MergeException {
		// Make sure that the date is not null.
		if (date == null) {
			throw new MergeException("Dataset "+getDatasetId()+" is trying to set the archive directory with a null date.");
		}
		// Create a new check task if it doesn't already exist.
		if (!checkTasks.containsKey(date)) {
			checkTasks.put(date, new DatasetCheck(this, date));
		}
		// Set the checker in the specific check task.
		((DatasetCheck)checkTasks.get(date)).setChecker(checker);
	}
	
	/**
	 * Set the directory where the data to be archived can be found for the Dataset.
	 * @param date The date of the loading task.
	 * @param directory The data to archive directory location.
	 * @throws MergeException if the date is <code>null</code> or if the directory does not match a previously set value.
	 * @see dts.port.DatasetLoad#setDataToArchiveDirectory(String)
	 */
	public void setDataToArchiveDirectory(Timestamp date, String directory) throws MergeException {
		// Make sure that the date is not null.
		if (date == null) {
			throw new MergeException("Dataset "+getDatasetId()+" is trying to set the archive directory with a null date.");
		}
		// Create a new load task if it doesn't already exist.
		if (!loadTasks.containsKey(date)) {
			loadTasks.put(date, new DatasetLoad(this, date));
		}
		// Set the data to archive directory in the specific load task.
		((DatasetLoad)loadTasks.get(date)).setDataToArchiveDirectory(directory);
	}

	/**
	 * Mark a source data set to be excluded from the processed data set.
	 * @param source The source data set to be marked.
	 * @param flag The flag to mark the source data set as excluded.
	 * @throws MergeException if the flag does not match a previously set value for the source data set.
	 */
	public void setExcluded(Dataset source, boolean flag) throws MergeException {
		// Make sure the source is not null.
		if (source == null) {
			throw new MergeException("Dataset "+getDatasetId()+" has a null source data set for setting the excluded flag.");
		}
		// Make sure the source is associated with the data set.
		else if (!sourceDatasets.containsKey(source.getDatasetId())) {
			throw new MergeException("Dataset "+getDatasetId()+" is trying to set the excluded flag for source data set "+source.getDatasetId()+" that is not know as a source.");
		}
		// Set the excluded flag for the source data set.
		((DatasetSource)sourceDatasets.get(source.getDatasetId())).setExcluded(flag);
	}
	
	/**
	 * Set the external contact for the Dataset.
	 * @param contact The Dataset's external contact.
	 * @throws MergeException if the external contact does not match a previously set value.
	 */
	public void setExternalContact(User contact) throws MergeException {
		// Don't do anything if the specified contact is null.
		if (contact == null) { return; }
		// Set the contact for the first time.
		else if (externalContact == null) { externalContact = contact; }
		// Make sure the contact matches a previously set value.
		else if (externalContact.compareTo(contact) != 0) {
			throw new MergeException("Dataset "+getDatasetId()+" has a mismatched external contact:  "+externalContact.getUserId()+" vs "+contact.getUserId());
		}
	}
	
	/**
	 * Set the final data directory for the processing of the Dataset.
	 * @param project The project for the processing task.
	 * @param directory The final data directory for processing the data set.
	 * @throws MergeException if the project is <code>null</code> or if the directory does not match a previously set value.
	 * @see dts.port.DatasetProcess#setFinalDataDirectory(String)
	 */
	public void setFinalDataDirectory(Project project, String directory) throws MergeException {
		// Make sure the project isn't null.
		if (project == null) {
			throw new MergeException("Dataset "+getDatasetId()+" is trying to set the final data directory with a null Project.");
		}
		// Create a new processing task if it doesn't already exist.
		if (!processTasks.containsKey(project.getProjectId())) {
			processTasks.put(project.getProjectId(), new DatasetProcess(this, project));
		}
		// Set the final data directory for the specific processing task.
		((DatasetProcess)processTasks.get(project.getProjectId())).setFinalDataDirectory(directory);
	}
	
	/**
	 * Set the flag that marks if there are outstanding questions on this data set.
	 * @param flag The questions flag value.
	 * @throws MergeException if the flag does not match a previously set value.
	 */
	public void setHasQuestions(boolean flag) throws MergeException {
		// Set the flag for the first time.
		if (questions == null) { questions = new Boolean(flag); }
		// Make sure the flag matches a previously set value.
		else if (flag != questions.booleanValue()) {
			throw new MergeException("Dataset "+getDatasetId()+" has a mismatched questions flag.");
		}
	}
	
	/**
	 * Set the how to file for the processing of the Dataset.
	 * @param project The project for the processing task.
	 * @param file The how to file for processing the data set.
	 * @throws MergeException if the project is <code>null</code> or if the file does not match a previously set value.
	 * @see dts.port.DatasetProcess#setHowToFile(String)
	 */
	public void setHowToFile(Project project, String file) throws MergeException {
		// Make sure the project isn't null.
		if (project == null) {
			throw new MergeException("Dataset "+getDatasetId()+" is trying to set the how to file with a null Project.");
		}
		// Create a new processing task if it doesn't already exist.
		if (!processTasks.containsKey(project.getProjectId())) {
			processTasks.put(project.getProjectId(), new DatasetProcess(this, project));
		}
		// Set the how to file for the specific processing task.
		((DatasetProcess)processTasks.get(project.getProjectId())).setHowToFile(file);
	}
	
	/**
	 * Set the directory where the data was ingested for the Dataset.
	 * @param date The date of the ingest task.
	 * @param directory The Dataset's ingest directory.
	 * @throws MergeException if the date is <code>null</code> or if the directory does not match a previously set value.
	 * @see dts.port.DatasetIngest#setIngestDirectory(String)
	 */
	public void setIngestDirectory(Timestamp date, String directory) throws MergeException {
		// Make sure that the date is not null.
		if (date == null) {
			throw new MergeException("Dataset "+getDatasetId()+" is trying to set the archive directory with a null date.");
		}
		// Create a new ingest task if it doesn't already exist.
		if (!ingestTasks.containsKey(date)) {
			ingestTasks.put(date, new DatasetIngest(this, date));
		}
		// Set the ingest directory in the specific ingest task.
		((DatasetIngest)ingestTasks.get(date)).setIngestDirectory(directory);
	}
	
	/**
	 * Set the internal contact for the Dataset.
	 * @param contact The Dataset's internal contact.
	 * @throws MergeException if the contact is <code>null</code> or if the contact does not match a previously set value.
	 */
	public void setInternalContact(User contact) throws MergeException {
		// Handle a null internal contact.
		if (contact == null) {
			throw new MergeException("Dataset "+getDatasetId()+" has a null internal contact.");
		}
		// Set the internal contact for the first time.
		else if (internalContact == null) { internalContact = contact; }
		// Make sure the contact matches a previously set value.
		else if (internalContact.compareTo(contact) != 0) {
			throw new MergeException("Dataset "+getDatasetId()+" has a mismatched internal contact:  "+internalContact.getUserId()+" vs "+contact.getUserId());
		}
	}

	/**
	 * Set the flag that marks the Dataset as loaded.
	 * @param date The date of the load task.
	 * @param flag The loaded flag value.
	 * @throws MergeException if the date is <code>null</code> or if the flag does not match a previously set value.
	 * @see dts.port.DatasetLoad#setLoaded(boolean)
	 */
	public void setLoaded(Timestamp date, boolean flag) throws MergeException {
		// Make sure that the date is not null.
		if (date == null) {
			throw new MergeException("Dataset "+getDatasetId()+" is trying to set the archive directory with a null date.");
		}
		// Create a new load task, if it doesn't already exist.
		if (!loadTasks.containsKey(date)) {
			loadTasks.put(date, new DatasetLoad(this, date));
		}
		// Set the loaded flag for the specific load task.
		((DatasetLoad)loadTasks.get(date)).setLoaded(flag);
	}
	
	/**
	 * Set the User tasked to loading the Dataset at the specified date.
	 * @param date The date of the load task.
	 * @param loader The User tasked to loading the Dataset.
	 * @throws MergeException if the date is <code>null</code> or if the loader does not match a previously set value.
	 * @see dts.port.DatasetLoad#setLoader(User)
	 */
	public void setLoader(Timestamp date, User loader) throws MergeException {
		// Make sure that the date is not null.
		if (date == null) {
			throw new MergeException("Dataset "+getDatasetId()+" is trying to set the archive directory with a null date.");
		}
		// Create a new load task if it doesn't already exist.
		if (!loadTasks.containsKey(date)) {
			loadTasks.put(date, new DatasetLoad(this, date));
		}
		// Set the loader for the specific load task.
		((DatasetLoad)loadTasks.get(date)).setLoader(loader);
	}
	
	/**
	 * Set the flag that marks the Dataset as being in the Master List for the specified Project.
	 * @param projectId The identifier for the Project.
	 * @param flag The Master List flag value.
	 * @throws MergeException if the Project is not associated to the Dataset or if the flag does not match a previously set value.
	 * @see dts.port.DatasetProject#setMasterListFlag(boolean)
	 */
	public void setMasterListFlag(String projectId, boolean flag) throws MergeException {
		// Make sure the project is associated to the data set.
		if (!datasetProjects.containsKey(projectId)) {
			throw new MergeException("Dataset "+getDatasetId()+" cannot find project "+projectId+" for setting the master list flag.");
		}
		// Set the ML flag for the specified project.
		else { ((DatasetProject)datasetProjects.get(projectId)).setMasterListFlag(flag); }
	}

	/**
	 * Set the name/title of the Dataset.
	 * @param name The Dataset's name.
	 * @throws MergeException if the name is <code>null</code> or if the name does not match a previously set value.
	 */
	public void setName(String name) throws MergeException {
		// Handle a null data set name.
		if (name == null) {
			throw new MergeException("Dataset "+getDatasetId()+" has a null name.");
		}

		// Clean up the name to follow conventions.
		name = cleanUpName(name);
		
		// Set the name for the first time.
		if (this.name == null) { this.name = name; }
		// Make sure the name matches a previously set value.
		else if (!this.name.equals(name)) {
			throw new MergeException("Dataset "+getDatasetId()+" has a mismatched name:  '"+this.name+"' vs '"+name+"'");
		}
	}
	
	/**
	 * Set the plots directory for the processing of the Dataset.
	 * @param project The project for the processing task.
	 * @param directory The plots directory for processing the data set.
	 * @throws MergeException if the project is <code>null</code> or if the directory does not match a previously set value.
	 * @see dts.port.DatasetProcess#setPlotsDirectory(String)
	 */
	public void setPlotsDirectory(Project project, String directory) throws MergeException {
		// Make sure the project isn't null.
		if (project == null) {
			throw new MergeException("Dataset "+getDatasetId()+" is trying to set the plots directory with a null Project.");
		}
		// Create a new processing task if it doesn't already exist.
		if (!processTasks.containsKey(project.getProjectId())) {
			processTasks.put(project.getProjectId(), new DatasetProcess(this, project));
		}
		// Set the plots directory for the specific processing task.
		((DatasetProcess)processTasks.get(project.getProjectId())).setPlotsDirectory(directory);
	}
	
	/**
	 * Set the notes for the processing of the Dataset.
	 * @param project The project for the processing task.
	 * @param note The notes for the processing.
	 * @throws MergeException if the project is <code>null</code> or if the note does not match a previously set value.
	 * @see dts.port.DatasetProcess#setProcessingNotes(String)
	 */
	public void setProcessingNotes(Project project, String note) throws MergeException {
		// Make sure the project is not null.
		if (project == null) {
			throw new MergeException("Dataset "+getDatasetId()+" is trying to set the processing status with a null Project.");
		}
		// Create a new processing task if it doesn't already exist.
		if (!processTasks.containsKey(project.getProjectId())) {
			processTasks.put(project.getProjectId(), new DatasetProcess(this, project));
		}
		// Set the status for the specific processing task.
		((DatasetProcess)processTasks.get(project.getProjectId())).setProcessingNotes(note);
	}
	
	/**
	 * Set the status for the processing of the Dataset.
	 * @param project The project for the processing task.
	 * @param status The status of the processing.
	 * @throws MergeException if the project is <code>null</code> or if the status does not match a previously set value.
	 * @see dts.port.DatasetProcess#setProcessingStatus(String)
	 */
	public void setProcessingStatus(Project project, String status) throws MergeException {
		// Make sure the project is not null.
		if (project == null) {
			throw new MergeException("Dataset "+getDatasetId()+" is trying to set the processing status with a null Project.");
		}
		// Create a new processing task if it doesn't already exist.
		if (!processTasks.containsKey(project.getProjectId())) {
			processTasks.put(project.getProjectId(), new DatasetProcess(this, project));
		}
		// Set the status for the specific processing task.
		((DatasetProcess)processTasks.get(project.getProjectId())).setProcessingStatus(status);
	}
	
	/**
	 * Set the User tasked with processing the Dataset.
	 * @param project The project for the processing task.
	 * @param processor The User tasked with processing the data set.
	 * @throws MergeException if the project is <code>null</code> or if the processor does not match a previously set value.
	 * @see dts.port.DatasetProcess#setProcessor(User)
	 */
	public void setProcessor(Project project, User processor) throws MergeException {
		// Make sure the project isn't null.
		if (project == null) {
			throw new MergeException("Dataset "+getDatasetId()+" is trying to set the processor with a null Project.");
		}
		// Create a new processing task if it doesn't already exist.
		if (!processTasks.containsKey(project.getProjectId())) {
			processTasks.put(project.getProjectId(), new DatasetProcess(this, project));
		}
		// Set the processor for the specific processing task.
		((DatasetProcess)processTasks.get(project.getProjectId())).setProcessor(processor);
	}
	
	/**
	 * Set the flag that marks the Dataset as having documentation for the specified date.
	 * @param date The date of the load task.
	 * @param flag The documentation flag value.
	 * @throws MergeException if the date is <code>null</code> or if the flag does not match a previously set value.
	 * @see dts.port.DatasetLoad#setReadmeFlag(boolean)
	 */
	public void setReadmeFlag(Timestamp date, boolean flag) throws MergeException {
		// Make sure that the date is not null.
		if (date == null) {
			throw new MergeException("Dataset "+getDatasetId()+" is trying to set the archive directory with a null date.");
		}
		// Create a new load task if it doesn't already exist.
		if (!loadTasks.containsKey(date)) {
			loadTasks.put(date, new DatasetLoad(this, date));
		}
		// Set the readme flag for the specific load task.
		((DatasetLoad)loadTasks.get(date)).setReadmeFlag(flag);
	}
	
	/**
	 * Set the readme file for the Dataset.
	 * @param file The Dataset's readme file.
	 * @throws MergeException if the file does not match a previously set value.
	 */
	public void setReadmeFile(String file) throws MergeException {
		// Handle a null or empty file
		if (file == null || file.trim().equals("")) { return; }
		
		// Set the file for the first time.
		if (readmeFile == null) { readmeFile = file; }
		// Make sure the file matches a previously set value.
		else if (readmeFile.compareTo(file) != 0) {
			throw new MergeException("Dataset "+getDatasetId()+" has a mismatched readme file:  "+readmeFile+" vs "+file);			
		}
	}
	
	/**
	 * Set the remote URL for the Dataset.
	 * @param url The Dataset's remote URL.
	 * @throws MergeException if the URL does not match a previously set value.
	 */
	public void setRemoteURL(String url) throws MergeException {
		// Handle a null URL.
		url = url == null ? "" : url.trim();
		// Don't bother with empty URLs.
		if (url.equals("")) { return; }
		// Set the URL for the first time.
		else if (remoteURL == null) { remoteURL = url; }
		// Make sure the URL matches a previously set value.
		else if (remoteURL.compareTo(url) != 0) {
			throw new MergeException("Dataset "+getDatasetId()+" has a mismatched remote URL:  "+remoteURL+" vs "+url);
		}
	}

	/**
	 * Set the repository for the processing of the Dataset.
	 * @param project The project for the processing task.
	 * @param directory The repository directory for processing the data set.
	 * @throws MergeException if the project is <code>null</code> or if the directory does not match a previously set value.
	 * @see dts.port.DatasetProcess#setRepository(String)
	 */
	public void setRepository(Project project, String directory) throws MergeException {
		// Make sure the project isn't null.
		if (project == null) {
			throw new MergeException("Dataset "+getDatasetId()+" is trying to set the repository with a null Project.");
		}
		// Create a new processing task if it doesn't already exist.
		if (!processTasks.containsKey(project.getProjectId())) {
			processTasks.put(project.getProjectId(), new DatasetProcess(this, project));
		}
		// Set the repository for the specific processing task.
		((DatasetProcess)processTasks.get(project.getProjectId())).setRepository(directory);
	}
	
	/**
	 * Set the station file for the processing of the Dataset.
	 * @param project The project for the processing task.
	 * @param file The station list file for processing the data set.
	 * @throws MergeException if the project is <code>null</code> or if the file does not match a previously set value.
	 * @see dts.port.DatasetProcess#setStationFile(String)
	 */
	public void setStationFile(Project project, String file) throws MergeException {
		// Make sure the project isn't null.
		if (project == null) {
			throw new MergeException("Dataset "+getDatasetId()+" is trying to set the station file with a null Project.");
		}
		// Create a new processing task if it doesn't already exist.
		if (!processTasks.containsKey(project.getProjectId())) {
			processTasks.put(project.getProjectId(), new DatasetProcess(this, project));
		}
		// Set the file for the specific processing task.
		((DatasetProcess)processTasks.get(project.getProjectId())).setStationFile(file);
	}	
	
	/**
	 * Store the data set information into the DTS database.
	 * @param porter The class controlling the porting of the database.
	 * @param connection The connection to the DTS database.
	 * @param projects The mapping of the projects in the DTS database.
	 * @param users The mapping of the users in the DTS database.
	 * @param datasets The mapping of the data sets to put into the database.
	 * @param noteTypes The mapping of the note types in the DTS database.
	 * @param statuses The mapping of the statuses in the DTS database.
	 * @return <code>true</code> if all of the data sets and their associated information was 
	 * inserted correctly, <code>false</code> if there was at least one problem with an insert.
	 * @throws SQLException if there is a problem in executing the statements on the database.
	 */
	public static boolean storeDataTrackingSystemDatasets(ToDmgDts porter, Connection connection, Map projects, Map users, Map datasets, Map noteTypes, Map statuses) throws SQLException {
		// Define the success flag to mark if there were any errors during the insert.
		boolean success = true;
		// Define the SQL to insert the base data set into the database.
		String sql = "INSERT INTO dataset(dataset_id, name, source_contact_id, internal_contact_id, question_flag, readme_url, remote_url) VALUES(?, ?, ?, ?, ?, ?, ?)";
		// Define the statement that will execute the insert query.
		PreparedStatement stmt = connection.prepareStatement(sql);
		
		// Define a container for holding software packages for DTS.  This is only used by processed
		// data sets and does not to be a more widely container since all of the software will be
		// handled during the processing task inserts.
		TreeMap software = new TreeMap();
		
		// Define an iterator to loop through all of the data sets in the mapping.
		Iterator dsItr = datasets.values().iterator();
		// Loop through the data sets in the iterator.
		while (dsItr.hasNext()) {
			// Pull out the current data set from the iterator
			Dataset dataset = (Dataset)dsItr.next();

			// Define a flag that marks the success of inserting the base data set into the database to
			// prevent the rest of the data set from being inserted.
			boolean currentSuccess = true;
			
			try {
				// Assign the data set specific information to the statement.
				stmt.setString(1, dataset.getDatasetId());
				stmt.setString(2, dataset.getName());
				if (dataset.getExternalContact() != null) {
					stmt.setInt(3, dataset.getExternalContact().getDtsId());
				} else {
					stmt.setNull(3, Types.INTEGER);
				}
				if (dataset.getInternalContact() != null) {
					stmt.setInt(4, dataset.getInternalContact().getDtsId());
				} else {
					stmt.setNull(4, Types.INTEGER);
				}
				stmt.setBoolean(5, dataset.hasQuestions());
				stmt.setString(6, dataset.getReadmeFile());
				stmt.setString(7, dataset.getRemoteURL());
				// Execute the insert statement.
				stmt.execute();
			} catch (SQLException e) {
				currentSuccess = false;
				success = false;
				porter.appendMergeException(new MergeException("Dataset "+dataset.getDatasetId()+" was unable to be inserted into the DTS database.  "+e.getMessage()));
			}
		
			// Only try to insert the rest of the data set information if the base data set was inserted correctly.
			if (currentSuccess) {
				// Define an Iterator for the projects associated with the data set.
				Iterator projectItr = dataset.getProjectAssociations().iterator();
				// Loop through each project associated with the data set
				while (projectItr.hasNext()) {
					DatasetProject dsProj = (DatasetProject)projectItr.next();
					try {
						success = dsProj.storeDataTrackingSystemInformation(connection) && success;
					} catch (SQLException e) {
						success = false;
						porter.appendMergeException(new MergeException("Unable to associate Dataset "+dataset.getDatasetId()+" to project "+dsProj.getProject().getProjectId()+":  "+e.getMessage()));
					}
				}
				
				// Define an Iterator for the ingest tasks for this data set.
				Iterator ingestItr = dataset.getIngestTasks().iterator();
				// Loop through each ingest task.
				while (ingestItr.hasNext()) {
					// Pull out the current ingest task.
					DatasetIngest ingest = (DatasetIngest)ingestItr.next();
					try {
						success = ingest.storeDataTrackingSystemInformation(connection) && success;
					} catch (SQLException e) {
						success = false;
						porter.appendMergeException(new MergeException("Unable to insert ingest task "+ingest.getEntryDate()+" for data set "+dataset.getDatasetId()+":  "+e.getMessage()));
					}
				}
				
				// Define an Iterator for the processing tasks for this data set.
				Iterator procItr = dataset.getProcessTasks().iterator();
				// Loop through each processing task.
				while (procItr.hasNext()) {
					// Pull out the current processing task.
					DatasetProcess process = (DatasetProcess)procItr.next();
					try {
						success = process.storeDataTrackingSystemInformation(porter, connection, users, software) && success;
					} catch (SQLException e) {
						success = false;
						porter.appendMergeException(new MergeException("Unable to insert the processing task "+process.getProject()+" for data set "+dataset.getDatasetId()+":  "+e.getMessage()));
					}
				}
				
				// Define an Iterator for the loading tasks for this data set.
				Iterator loadItr = dataset.getLoadTasks().iterator();
				// Loop through each load task.
				while (loadItr.hasNext()) {
					// Pull out the current load task.
					DatasetLoad load = (DatasetLoad)loadItr.next();
					try {
						success = load.storeDataTrackingSystemInformation(connection) && success;
					} catch (SQLException e) {
						success = false;
						porter.appendMergeException(new MergeException("Unable to insert load task "+load.getEntryDate()+" for data set "+dataset.getDatasetId()+":  "+e.getMessage()));						
					}
				}
				
				// Define an Iterator for the checking tasks for the data set.
				Iterator checkItr = dataset.getApproveTasks().iterator();
				// Loop through each checking task.
				while (checkItr.hasNext()) {
					// Pull out the current check task.
					DatasetCheck check = (DatasetCheck)checkItr.next();
					try {
						success = check.storeDataTrackingSystemInformation(connection) && success;
					} catch (SQLException e) {
						success = false;
						porter.appendMergeException(new MergeException("Unable to insert check task "+check.getEntryDate()+" for data set "+dataset.getDatasetId()+":  "+e.getMessage()));
					}
				}
				
				// Define an Iterator for the general notes for the data set.
				Iterator noteItr = dataset.getGeneralNotes().keySet().iterator();
				// Loop through each note.
				while (noteItr.hasNext()) {
					// Pull out the entry date from the iterator.
					Timestamp entryDate = (Timestamp)noteItr.next();
					// Create a new Note object for the note.
					Note note = new Note((User)users.get("local"), entryDate, (String)dataset.getGeneralNotes().get(entryDate));
					// Insert the note into the database.
					try { success = note.storeInDataTrackingSystem(connection); }
					catch (SQLException e) {
						success = false;
						porter.appendMergeException(new MergeException("General Note for dataset "+dataset.getDatasetId()+" could not be inserted.  "+e.getMessage()));
					}
					// Only try to associate the note to the data set if it was inserted successfully.
					if (success) {
						try { success = note.storeInDataTrackingSystem(connection, dataset, (NoteType)NoteType.getNoteTypeMap().get(NoteType.GENERAL_NOTE)); }
						catch (SQLException e) {
							success = false;
							porter.appendMergeException(new MergeException("Unable to assign the note to data set "+dataset.getDatasetId()+"  "+e.getMessage()));
						}
					}
				}

				// Define the platform insert statement.
				PreparedStatement platStmt = connection.prepareStatement("INSERT INTO dataset_platform(dataset_id, platform_id) VALUES(?, ?)");
				platStmt.setString(1, dataset.getDatasetId());
				// Define an Iterator for the platforms for the data set.
				Iterator platItr = dataset.getPlatforms().iterator();
				// Loop through all of the platforms.
				while (platItr.hasNext()) {
					// Get the platform ID from the iterator.
					Integer platform = (Integer)platItr.next();
					// Assign the platform to the statement.
					platStmt.setInt(2, platform.intValue());
					// Execute the statement.
					platStmt.execute();
				}
				// Properly close the open statement stream.
				try { platStmt.close(); } catch (SQLException e) {}
			}
		}
		
		// Properly close the open statement stream.
		try { stmt.close(); } catch (SQLException e) {}
		
		// Return the success status of the inserts.
		return success;
	}
}
