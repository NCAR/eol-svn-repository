package dts.port;

import java.sql.*;
import java.util.*;

/**
 * The Software class is the container for a software package in the DTS.
 * 
 * @author jclawson
 */
public class Software {

	private int id;
	private String name, repository;
	
	/**
	 * Create a new instance of a Software package.
	 * @param id The DTS ID number for the software.
	 * @param name The name of the software package.
	 * @param repository The location of the software in the repository.
	 */
	public Software(int id, String name, String repository) {
		this.id = id;
		this.name = name;
		this.repository = repository;
	}
	
	/**
	 * Get the DTS ID number for the software.
	 * @return The software's DTS ID number.
	 */
	public int getId() { return id; }
	
	/**
	 * Get the name of the software package.
	 * @return The software package's name.
	 */
	public String getName() { return name; }
	
	/**
	 * Get the location of the software package's repository.
	 * @return The repository for the software.
	 */
	public String getRepository() { return repository; }
	
	/**
	 * Insert the software and associate it to the data set.
	 * @param porter The controlling class for the database porting.
	 * @param connection The connection to the DTS database.
	 * @param software The mapping of the software already in the DTS database.
	 * @param process The processing task that the software was used for.
	 * @return <code>true</code> if all of the inserts were performed successfully, <code>false</code>
	 * if there was at least one failure.
	 * @throws SQLException if there is a problem with the execution of the statements.
	 */
	public static boolean storeDataTrackingSystemSoftware(ToDmgDts porter, Connection connection, Map software, DatasetProcess process) throws SQLException {
		boolean success = true;
		
		// Get the repository that needs to be put into the DTS.
		String repos = process.getRepository();
		
		// Handle when there isn't a repository defined.
		if (repos.equals("")) { return true; }
		
		// Define the default values for the piece of software.
		String name = repos;
		String tag = process.getProject().getProjectId();
		String repository = "";

		// Check to see if the repository is actually a valid 
		if (repos.startsWith("http://svn.eol.ucar.edu") && repos.contains("/tags/")) {
			name = repos.substring(0, repos.lastIndexOf("/tags/"));
			tag = repos.substring(repos.lastIndexOf("/tags/") + 6);
			repository = name;
		}
		
		// Check to see if the software already exists in the mapping and add it if it isn't.
		if (!software.containsKey(name)) {
			// Define the SQL to insert the software package into the database.
			String sql = "INSERT INTO software(name, repository) VALUES(?, ?)";
			// Define the statement to execute the software SQL.
			PreparedStatement stmt = connection.prepareStatement(sql, PreparedStatement.RETURN_GENERATED_KEYS);
			try {
				// Assign the specific software values to the statement.
				stmt.setString(1, name);
				stmt.setString(2, repository);
				// Execute the statement.
				stmt.execute();
			} catch (SQLException e) {
				success = false;
				porter.appendMergeException(new MergeException("Software "+name+" could not inserted into the database.  "+e.getMessage()));
			}
			
			// Obtain the auto-generated DTS ID keys after a successful insert.
			if (success) {
				try {
					// Get the auto-generated key for the software
					ResultSet keys = stmt.getGeneratedKeys();
					// Assign the DTS ID if the key was returned.
					if (keys.next()) { 
						Software sw = new Software(keys.getInt(1), name, repository);
						software.put(sw.getName(), sw);
					}
					// There is some problem, since the key was not returned.
					else { success = false; }
					// Properly close the results stream.
					try { keys.close(); } catch (SQLException e) {}
				} catch (SQLException e) {
					success = false;
					throw e;
				}
			}
			
			// Properly close the open statement stream.
			try { stmt.close(); } catch (SQLException e) {}
		}
		
		// Only try to associate the software to the data set if there hasn't been any problems yet.
		if (success) {
			// Pull the software set out of the map.
			Software softwarePackage = (Software)software.get(name);
			// Define the SQL to associate the software to the data set.
			String sql = "INSERT INTO dataset_software(dataset_id, software_id, tag_name) VALUES(?, ?, ?)";
			// Create the statement to execute the SQL.
			PreparedStatement stmt = connection.prepareStatement(sql);
			// Assign the specific values to the statement.
			stmt.setString(1, process.getDataset().getDatasetId());
			stmt.setInt(2, softwarePackage.getId());
			stmt.setString(3, tag);
			// Execute the statement.
			stmt.execute();
			
			// Properly close the open statement stream.
			try { stmt.close(); } catch (SQLException e) {}			
		}
		
		// Return the final status for all of the inserts.
		return success;
	}
}
