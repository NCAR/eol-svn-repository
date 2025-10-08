package dln.beans;

import java.sql.*;
import java.util.*;

/**
 * The SoftwareBean class is the representation of a software package in the DTS.
 * 
 * @author jclawson
 */
public class SoftwareBean extends DefaultBean implements Comparable<SoftwareBean> {

	private static final long serialVersionUID = 2094948320535375086L;

	private Integer softwareId;
	private String deployLocation, description, language, name, repository, tagName;
	
	/**
	 * Create a new instance of a SoftwareBean.
	 */
	public SoftwareBean() {}
	
	/**
	 * Compare this software package to the specified software package.
	 * @param software The software to compare to this software package.
	 * @return A negative, zero, or positive integer if this software is less than,
	 * equal to, or greater than the specified software using the name and tag.
	 */
	public int compareTo(SoftwareBean software) {
		int result = getName().compareTo(software.getName());
		if (result == 0) {
			result = getTagName().compareTo(software.getTagName());
		}
		return result;
	}
	
	/**
	 * Delete the software package from the specified data set.
	 * @param datasetId The ID of the data set to have the software removed.
	 * @throws SQLException if there is a problem deleting the entry from the database.
	 */
	public void delete(String datasetId) throws SQLException {
		Connection connection = getConnection();
		try {
			String sql = "DELETE FROM dataset_software WHERE dataset_id=? AND software_id=?";
			PreparedStatement stmt = connection.prepareStatement(sql);
			stmt.setString(1, datasetId);
			stmt.setInt(2, getSoftwareId());
			stmt.execute();
			try { stmt.close(); } catch (SQLException e) {}
		} finally {
			try { connection.close(); } catch (SQLException e) {}
		}
	}
	
	/**
	 * Get the list of all of the software packages from the database.
	 * @return The list of software packages in the database.
	 * @throws SQLException if there is a problem reading the database.
	 */
	public static List<SoftwareBean> getAllSoftwarePackages() throws SQLException {
		List<SoftwareBean> list = new ArrayList<SoftwareBean>();
		
		Connection connection = getConnection();
		
		try {
			String sql = "SELECT software_id, name, repository, language, deploy_location, description FROM software";
			PreparedStatement stmt = connection.prepareStatement(sql);
			ResultSet results = stmt.executeQuery();
			while (results.next()) {
				SoftwareBean software = new SoftwareBean();
				software.setSoftwareId(results.getInt(1));
				software.setName(results.getString(2));
				software.setRepository(results.getString(3));
				software.setLanguage(results.getString(4));
				software.setDeployLocation(results.getString(5));
				software.setDescription(results.getString(6));
				list.add(software);
			}
			try { results.close(); } catch (SQLException e) {}
			try { stmt.close(); } catch (SQLException e) {}
		} finally {
			try { connection.close(); } catch (SQLException e) {}
		}
		
		
		return list;
	}
	
	/**
	 * Get the directory where the software is deployed.
	 * @return The deployed software location.
	 */
	public String getDeployLocation() { return deployLocation == null ? "" : deployLocation.trim(); }
	
	/**
	 * Get the description of the software package.
	 * @return The software package's description
	 */
	public String getDescription() { return description == null ? "" : description.trim(); }
	
	/**
	 * Get the language(s) the software is written in.
	 * @return The software programming languages.
	 */
	public String getLanguage() { return language == null ? "" : language.trim(); }
	
	/**
	 * Get the name of the software package.
	 * @return The software package's name.
	 */
	public String getName() { return name == null ? "" : name.trim(); }
	
	/**
	 * Get the URL to the software package's repository location.
	 * @return The repository for the software.
	 */
	public String getRepository() { return repository == null ? "" : repository.trim(); }
	
	/**
	 * Get the DTS ID for the software package.
	 * @return The software's DTS ID.
	 */
	public Integer getSoftwareId() { return softwareId; }
	
	/**
	 * Get the repository tag name for this software package.
	 * @return The software package's tag name.
	 */
	public String getTagName() { return tagName == null ? "" : tagName.trim(); }
	
	/**
	 * Insert the software into the database.
	 * @param datasetId The ID of the data set to be associated to the software.
	 * @throws SQLException if there is a problem updating the database.
	 */
	public void insert(String datasetId) throws SQLException {
		Connection connection = getConnection();
		try {
			// Turn on transaction processing.
			connection.setAutoCommit(false);
			
			// Update the existing software package.
			if (getSoftwareId() != null && getSoftwareId() > 0) {
				String sql = "UPDATE software SET name=?, repository=?, deploy_location=?, language=?, description=? WHERE software_id=?";
				PreparedStatement stmt = connection.prepareStatement(sql);
				stmt.setString(1, getName());
				stmt.setString(2, getRepository());
				stmt.setString(3, getDeployLocation());
				stmt.setString(4, getLanguage());
				stmt.setString(5, getDescription());
				stmt.setInt(6, getSoftwareId());
				stmt.execute();
				try { stmt.close(); } catch (SQLException e) {}
			}
			// Insert the software as a new entry.
			else {
				String sql = "INSERT INTO software(name, repository, deploy_location, language, description) VALUES(?, ?, ?, ?, ?)";
				PreparedStatement stmt = connection.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS);
				stmt.setString(1, getName());
				stmt.setString(2, getRepository());
				stmt.setString(3, getDeployLocation());
				stmt.setString(4, getLanguage());
				stmt.setString(5, getDescription());
				stmt.execute();
				ResultSet keys = stmt.getGeneratedKeys();
				if (keys.next()) { setSoftwareId(keys.getInt(1)); }
				try { keys.close(); } catch (SQLException e) {}
				try { stmt.close(); } catch (SQLException e) {}
			}
			
			// Attach the software to the data set.
			PreparedStatement stmt = connection.prepareStatement("INSERT INTO dataset_software(tag_name, software_id, dataset_id) VALUES(?, ?, ?)");
			stmt.setString(1, getTagName());
			stmt.setInt(2, getSoftwareId());
			stmt.setString(3, datasetId);
			stmt.execute();
			try { stmt.close(); } catch (SQLException e) {}
			
			// Save the commands sent to the database.
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
	 * Load the specified software package for the data set.
	 * @param softwareId The DTS ID of the software package.
	 * @param dataset The data set the software is being loaded for.
	 * @throws SQLException when there is a problem reading from the database.
	 */
	public void load(Integer softwareId, DatasetBean dataset) throws SQLException {
		Connection connection = getConnection();
		try {
			// Load the software information.
			String sql = "SELECT name, repository, language, deploy_location, description FROM software WHERE software_id=?";
			PreparedStatement stmt = connection.prepareStatement(sql);
			stmt.setInt(1, softwareId);
			ResultSet results = stmt.executeQuery();
			setSoftwareId(softwareId);
			if (results.next()) {
				setName(results.getString(1));
				setRepository(results.getString(2));
				setLanguage(results.getString(3));
				setDeployLocation(results.getString(4));
				setDescription(results.getString(5));
			}
			try { results.close(); } catch (SQLException e) {}
			try { stmt.close(); } catch (SQLException e) {}
			
			// Load the data set specific software information.
			stmt = connection.prepareStatement("SELECT tag_name FROM dataset_software WHERE dataset_id=? AND software_id=?");
			stmt.setString(1, dataset.getDatasetId());
			stmt.setInt(2, softwareId);
			results = stmt.executeQuery();
			if (results.next()) {
				setTagName(results.getString(1));
			}
			try { results.close(); } catch (SQLException e) {}
			try { stmt.close(); } catch (SQLException e) {}			
		}
		finally {
			try { connection.close(); } catch (SQLException e) {}
		}
	}
	
	/**
	 * Load all of the software packages attached to the specified data set.
	 * @param connection The connection to use to read from the database.
	 * @param dataset The data set the software is to be loaded for.
	 * @throws SQLException when there is a problem reading the database.
	 */
	public static void load(Connection connection, DatasetBean dataset) throws SQLException {
		String sql = "SELECT software.software_id, name, tag_name, repository, language, deploy_location, description FROM software JOIN dataset_software ON software.software_id=dataset_software.software_id WHERE dataset_id=?";
		PreparedStatement stmt = connection.prepareStatement(sql);
		stmt.setString(1, dataset.getDatasetId());
		ResultSet results = stmt.executeQuery();
		while (results.next()) {
			SoftwareBean software = new SoftwareBean();
			software.setSoftwareId(results.getInt(1));
			software.setName(results.getString(2));
			software.setTagName(results.getString(3));
			software.setRepository(results.getString(4));
			software.setLanguage(results.getString(5));
			software.setDeployLocation(results.getString(6));
			software.setDescription(results.getString(7));
			dataset.addSoftware(software);
		}
		try { results.close(); } catch (SQLException e) {}
		try { stmt.close(); } catch (SQLException e) {}
	}
	
	/**
	 * Set the directory where the software is deployed for use.
	 * @param dir The software's deploy directory.
	 */
	public void setDeployLocation(String dir) { deployLocation = dir; }
	
	/**
	 * Set the description of the software package.
	 * @param desc The software's description.
	 */
	public void setDescription(String desc) { description = desc; }
	
	/**
	 * Set the language(s) the software was written in.
	 * @param language The software's programming language(s).
	 */
	public void setLanguage(String language) { this.language = language; }
	
	/**
	 * Set the name of the software package.
	 * @param name The name of the software package.
	 */
	public void setName(String name) { this.name = name; }
	
	/**
	 * Set the URL of the software's location in the repository.
	 * @param repos The repository for the software.
	 */
	public void setRepository(String repos) { repository = repos; }
	
	/**
	 * Set the DTS ID for the software package.
	 * @param id The DTS ID for the software package.
	 */
	public void setSoftwareId(Integer id) { softwareId = id; }
	
	/**
	 * Set the name of the tag for this software package.
	 * @param tag The software package's repository tag.
	 */
	public void setTagName(String tag) { tagName = tag; }
	
	/**
	 * Update the software package in the database.
	 * @param datasetId The ID of the data set the software is being updated for.
	 * @throws SQLException if there is a problem updating the database.
	 */
	public void update(String datasetId) throws SQLException {
		Connection connection = getConnection();
		try {
			// Turn on transaction processing.
			connection.setAutoCommit(false);
			
			// Update the general software information.
			String sql = "UPDATE software SET name=?, repository=?, deploy_location=?, language=?, description=? WHERE software_id=?";
			PreparedStatement stmt = connection.prepareStatement(sql);
			stmt.setString(1, getName());
			stmt.setString(2, getRepository());
			stmt.setString(3, getDeployLocation());
			stmt.setString(4, getLanguage());
			stmt.setString(5, getDescription());
			stmt.setInt(6, getSoftwareId());
			stmt.execute();
			try { stmt.close(); } catch (SQLException e) {}
			
			// Insert/Update the data set specific software information.
			stmt = connection.prepareStatement("REPLACE INTO dataset_software SET tag_name=?, software_id=?, dataset_id=?");
			stmt.setString(1, getTagName());
			stmt.setInt(2, getSoftwareId());
			stmt.setString(3, datasetId);
			stmt.execute();
			try { stmt.close(); } catch (SQLException e) {}
			
			// Save the commands sent to the database.
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
}
