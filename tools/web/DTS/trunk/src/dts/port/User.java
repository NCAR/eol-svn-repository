package dts.port;

import java.sql.*;
import java.util.*;

/**
 * The User class is the container and database servicer for Users/Contacts.  It contains
 * general meta data for a User including their name and email.
 * 
 * @author jclawson
 */
public class User implements Comparable {

	private Boolean active;
	private int dtsId;
	private String email, fullName, shortName;
	
	/**
	 * Create a new instance of a User.
	 * @param shortName The short name identifier for the User.
	 */
	public User(String shortName) {
		this.shortName = shortName;
	}
	
	/**
	 * Read in the User information from the EMDAC database and compare the meta data with the information in
	 * the provided container.
	 * @param porter The porting controller class running the porting process.
	 * @param connection The IVEN database connection to use to read the User meta data.
	 * @param users The mapping of Users by short name ID used to store and merge the DTS information.
	 * @return <code>true</code> if the porting process proceeded successfully without any database read errors
	 * or conflicts between meta data.
	 * @throws SQLException if there is a problem reading the User information from the ZEDI database.
	 */
	public static boolean compareEmdacUsers(ToDmgDts porter, Connection connection, TreeMap users) throws SQLException {
		// Define the success flag used to return the success of reading the User information from the database.
		boolean success = true;
		// Define the SQL to read the User information from the database.		
//		String sql = "SELECT contact_short_name, email, active_editor FROM contact WHERE person_name=?";
		String sql = "SELECT short_name, email, active_editor FROM contact WHERE person_name=?";
		// Define the statement to execute on the database.
		PreparedStatement stmt = connection.prepareStatement(sql);
		
		// Define an Iterator to loop through all of the known users.
		Iterator itr = users.values().iterator();		
		while (itr.hasNext()) {
			// Get the current User from the iterator.
			User user = (User)itr.next();
			// Assign the current User's name to the statement so it can be searched.
			stmt.setString(1, user.getName());
			// Get the results of the query.
			ResultSet results = stmt.executeQuery();
			
			// The name was found, so it exists and we can compare it to the currently known info.
			if (results.next()) {
				// Overwrite the ID with the EMDAC short name.  This is because it is the official short name and
				// many of the IDs in the User mapping may actually be full person names.
				user.overwriteId(results.getString(1));

				// Set the email for the User.
				try { user.setEmail(results.getString(2)); } 
				// Handle the exception when the email has an illegal value or does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}
				
				// Set the active flag for the User.  (Only worry if the value is not null.)
				if (results.getString(3) != null) {
					// Try to set the active flag for the Project.
					try { user.setActive(results.getBoolean(3)); }
					// Handle the exception when the flag does not match a previously set value.
					catch (MergeException e) {
						// Update the porting class with the exception.
						porter.appendMergeException(e);
						// There is a conflict, so the entire port failed.
						success = false;
					}
				}
			}
			// The name was not found which is an error.
			else {
				porter.appendMergeException(new MergeException("User "+user.getName()+" was not found in EMDAC."));
				success = false;
			}
			
			// Properly close the result stream and ignore any exceptions.
			try { results.close(); } catch (SQLException e) {}
		}
		
		// Properly close the statement stream and ignore any exceptions.
		try { stmt.close(); } catch (SQLException e) {}
		
		// All of the Users have been processed, so we can return the success flag now.
		return success;
	}
	
	/**
	 * Compare this User to the specified User for sort order.  It uses the natural sort order
	 * of the User identifiers.
	 * @param user The User to compare with this User.
	 * @return A negative integer, zero, or a positive integer if this User is less than, equal to,
	 * or greater than the specified User.
	 */
	public int compareTo(Object user) {
		return getUserId().compareTo(((User)user).getUserId());
	}
	
	/**
	 * Search through the User mapping for the specified external contact or create a new User for
	 * the contact if it is not in the map.
	 * @param users The mapping of Users by their short name identifier.
	 * @param name The external contact's full name.
	 * @param email The external contact's email address.
	 * @return The external contact's User from the User mapping.
	 * @throws MergeException if there is a conflict in User meta data from the new external contact information.
	 */
	public static User findExternalContact(TreeMap users, String name, String email) throws MergeException {
		// Clean up the name and email address to handle null values and extra white space.
		name = name == null || name.equalsIgnoreCase("none") || name.equalsIgnoreCase("n/a") ? "" : name.trim();
		email = email == null || email.equalsIgnoreCase("none") || email.equalsIgnoreCase("n/a") ? "" : email.trim();
		
		// If the name and email are both empty, there is not an external contact.
		if (name.equals("") && email.equals("")) { return null; }
		// There is a problem if there is an email without a name. (Note: There can be a name without an email.)
		else if (name.equals("") && !email.equals("")) {
			throw new MergeException("External Contact has a missing name with a non-missing email: "+email);
		}
		
		// Define a mapping for full names to Users.  This needs to be done because the external contact's
		// short name identifier is not known by any of the DTS databases.  Therefore, a new map is created
		// using the full name as the key so existing external contact's can be found.
		TreeMap remap = new TreeMap();
		// Define an Iterator of the currently mapped values to be remapped by full name.
		Iterator itr = users.values().iterator();
		while (itr.hasNext()) {
			User user = (User)itr.next();
			remap.put(user.getName(), user);
		}
		// Search the remapped Users for the current external contact.
		if (remap.containsKey(name)) {
			// Get the external contact from the map.
			User user = (User)remap.get(name);
			// Set the email address to make sure that it matches any previously set values.
			user.setEmail(email);
			// Everything matched, so the User can be returned.
			return user;
		}
		// The external contact is not in the current mapping, so a new one needs to be created.
		else {
			// Create the new User.  (The full name is used as a short name since the DTS doesn't know the
			// short name.
			User user = new User(name);
			// Set the name of the User.  
			user.setName(name);
			// Set the email of the User.
			user.setEmail(email);
			// Add the new User to the main User mapping.
			users.put(user.getUserId(), user);
			// The User now exists, so it can be returned.
			return user;
		}
	}
	
	/**
	 * Get the ID number for the User in the DTS database.
	 * @return The User's DTS ID.
	 */
	public int getDtsId() { return dtsId; }
	
	/**
	 * Get the email address for the User.
	 * @return The User's email address.
	 */
	public String getEmail() { return email; }
	
	/**
	 * Get the name of the User.
	 * @return The User's full name.
	 */
	public String getName() { return fullName; }
	
	/**
	 * Get the short name identifier for the User.
	 * @return The User's short name identifier.
	 */
	public String getUserId() { return shortName; }
	
	/**
	 * Determine if this user is an active editor.
	 * @return <code>true</code> if the User is an active editor, <code>false</code> if the User is not.
	 */
	public boolean isActive() {
		return active == null ? false : active.booleanValue();
	}

	/**
	 * Read in the User information from the Data Loading Notes database and store the meta data in
	 * the provided container.
	 * @param porter The porting controller class running the porting process.
	 * @param connection The DLN database connection to use to read the User meta data.
	 * @param users The mapping of Users by short name ID used to store and merge the DTS information.
	 * @return <code>true</code> if the porting process proceeded successfully without any database read errors
	 * or conflicts between meta data.
	 * @throws SQLException if there is a problem reading the User information from the DLN database.
	 */
	public static boolean loadDlnUsers(ToDmgDts porter, Connection connection, TreeMap users) throws SQLException {
		// Define the success flag used to return the success of reading the User information from the database.
		boolean success = true;
		// Define the SQL to read the User information from the database.		
		String sql = "SELECT uid, first_name, last_name, email, active FROM user WHERE uid != 'Other' AND uid != 'Unassigned'";
		// Define the statement to execute on the database.
		PreparedStatement stmt = connection.prepareStatement(sql);
		// Execute the statement and obtain the results of the query.
		ResultSet results = stmt.executeQuery();

		// Loop through each row of the query results.
		while (results.next()) {
			// Pull out the short name ID from the current row of the results.
			String shortName = results.getString(1).toLowerCase();
			
			// Create a new User if it is not already in the mapping.
			if (!users.containsKey(shortName)) {
				users.put(shortName, new User(shortName));
			}

			// Load the User from the mapping with the current row's short name ID.
			User user = (User)users.get(shortName);
			
			// Set the name of the User.
			try { user.setName(results.getString(2), results.getString(3)); }
			// Handle the exception when the name has an illegal value or does not match a previously set value.
			catch (MergeException e) {
				// Update the porting class with the exception.
				porter.appendMergeException(e);
				// There is a conflict, so the entire port failed.
				success = false;
			}
			
			// Set the email for the User.
			try { user.setEmail(results.getString(4)); } 
			// Handle the exception when the email has an illegal value or does not match a previously set value.
			catch (MergeException e) {
				// Update the porting class with the exception.
				porter.appendMergeException(e);
				// There is a conflict, so the entire port failed.
				success = false;
			}
			
			// Set the active flag for the User.  (Only worry if the value is not null.)
			if (results.getString(5) != null) {
				// Try to set the active flag for the Project.
				try { user.setActive(results.getBoolean(5)); }
				// Handle the exception when the flag does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}
			}
		}
		
		// Properly close the open streams and ignore any exceptions that may occur while being closed.
		try { results.close(); } catch (SQLException e) {}
		try { stmt.close(); } catch (SQLException e) {}
		
		// All of the Users have been processed, so we can return the success flag now.
		return success;
	}
	
	/**
	 * Read in the User information from the processing Inventory database and store the meta data in
	 * the provided container.
	 * @param porter The porting controller class running the porting process.
	 * @param connection The IVEN database connection to use to read the User meta data.
	 * @param users The mapping of Users by short name ID used to store and merge the DTS information.
	 * @return <code>true</code> if the porting process proceeded successfully without any database read errors
	 * or conflicts between meta data.
	 * @throws SQLException if there is a problem reading the User information from the IVEN database.
	 */
	public static boolean loadIvenUsers(ToDmgDts porter, Connection connection, TreeMap users) throws SQLException {
		// Define the success flag used to return the success of reading the User information from the database.
		boolean success = true;
		// Define the SQL to read the User information from the database.		
		String sql = "SELECT uid, fname, lname, email, active FROM users WHERE uid != 'Other' AND uid != 'Unassigned'";
		// Define the statement to execute on the database.
		PreparedStatement stmt = connection.prepareStatement(sql);
		// Execute the statement and obtain the results of the query.
		ResultSet results = stmt.executeQuery();

		// Loop through each row of the query results.
		while (results.next()) {
			// Pull out the short name ID from the current row of the results.
			String shortName = results.getString(1).toLowerCase();
			
			// Create a new User if it is not already in the mapping.
			if (!users.containsKey(shortName)) {
				users.put(shortName, new User(shortName));
			}

			// Load the User from the mapping with the current row's short name ID.
			User user = (User)users.get(shortName);
			
			// Set the name of the User.
			try { user.setName(results.getString(2), results.getString(3)); }
			// Handle the exception when the name has an illegal value or does not match a previously set value.
			catch (MergeException e) {
				// Update the porting class with the exception.
				porter.appendMergeException(e);
				// There is a conflict, so the entire port failed.
				success = false;
			}
			
			// Set the email for the User.
			try { user.setEmail(results.getString(4)); } 
			// Handle the exception when the email has an illegal value or does not match a previously set value.
			catch (MergeException e) {
				// Update the porting class with the exception.
				porter.appendMergeException(e);
				// There is a conflict, so the entire port failed.
				success = false;
			}
			
			// Set the active flag for the User.  (Only worry if the value is not null.)
			if (results.getString(5) != null) {
				// Try to set the active flag for the Project.
				try { user.setActive(results.getBoolean(5)); }
				// Handle the exception when the flag does not match a previously set value.
				catch (MergeException e) {
					// Update the porting class with the exception.
					porter.appendMergeException(e);
					// There is a conflict, so the entire port failed.
					success = false;
				}
			}
		}
		
		// Properly close the open streams and ignore any exceptions that may occur while being closed.
		try { results.close(); } catch (SQLException e) {}
		try { stmt.close(); } catch (SQLException e) {}
		
		// All of the Users have been processed, so we can return the success flag now.
		return success;
	}
	
	/**
	 * Overwrite the short name identifier without caring what it is overwriting.
	 * @param id The new short name identifier.
	 */
	private void overwriteId(String id) { shortName = id; }
	
	/**
	 * Set the flag that marks this User as an active User.
	 * @param flag The active User flag.
	 * @throws MergeException if the specified flag does not match a previously set value.
	 */
	public void setActive(boolean flag) throws MergeException {
		// Set the flag if it has never been set before. 
		if (active == null) { active = new Boolean(flag); }
		// Make sure the specified flag is the same as the previously set flag.
		else if (flag != active.booleanValue()) {
			throw new MergeException("User "+getUserId()+" has a mismatched active flag.");
		}
	}
	
	/**
	 * Set the ID value generated by the DTS for the User.
	 * @param id The DTS ID for the User.
	 */
	private void setDtsId(int id) { dtsId = id; }
	
	/**
	 * Set the email address for this User.
	 * @param email The User's email address.
	 * @throws MergeException if the specified email does not match a previously set value.
	 */
	public void setEmail(String email) throws MergeException {
		// Handle a null email address.
		email = email == null ? "" : email.trim();
		// Clean up an email address of the form <email@address> by removing the '<' and '>' characters.
		if (email.startsWith("<") && email.endsWith(">")) {
			email = email.substring(1, email.length() - 1);
		}
		
		// Replace eol, joss, and atd emails with just ucar.edu.
		email = email.replaceAll("@(atd|eol|joss)\\.", "@");
		
		// Set the email if it has not been set before.
		if (this.email == null && !email.equals("")) { this.email = email; }
		// make sure the specified email is the previously set value.
		else if (!email.equals("") && !this.email.equals(email)) {
			throw new MergeException("User "+getUserId()+" has a mismatched email:  "+this.email+" vs "+email);
		}
	}
	
	/**
	 * Set the name for this User.
	 * @param name The User's full name.
	 * @throws MergeException if the specified name is <code>null</code> or if the name does not
	 * match a previously set value.
	 */
	public void setName(String name) throws MergeException {
		// Handle a null name by throwing an exception.
		if (name == null) {
			throw new MergeException("User "+getUserId()+" has a null name.");
		}
		// Set the name.
		setName(name, "");
	}
	
	/**
	 * Set the name for this User.
	 * @param firstName The User's first name.
	 * @param lastName The User's last name.
	 * @throws MergeException if the first or last name are <code>null</code> or if the combined
	 * full name does not match a previously set value.
	 */
	public void setName(String firstName, String lastName) throws MergeException {
		// Handle a null first name by throwing an exception.
		if (firstName == null) {
			throw new MergeException("User "+getUserId()+" has a null first name.");
		}
		// Handle a null last name by throwing an exception.
		else if (lastName == null) {
			throw new MergeException("User "+getUserId()+" has a null last name.");
		}
		// Combine the first and last names into a full name.
		String name = firstName.trim()+" "+lastName.trim();
		// Convert multiple space sections into a single space and remove any leading and trailing whitespace.
		name = name.replaceAll("\\s+", " ").trim();
		
		// Set the name if it has not been set before.
		if (fullName == null) { fullName = name; }
		// Make sure the name matches a previously set value.
		else if (!fullName.equals(name)) {
			throw new MergeException("User "+getUserId()+" has a mismatched name:  "+fullName+" vs "+name);
		}
	}
	
	/**
	 * Store the User information in the provided mapping into the DTS database.
	 * @param porter The porting class controlling the porting process.
	 * @param connection The connection that connects to the DTS database.
	 * @param users The mapping of the User information to be inserted into the database.
	 * @return <code>true</code> if all of the User information was inserted successfully, <code>false</code>
	 * if there was any problem inserting the User information.
	 * @throws SQLException if there was a problem executing the insert statements.
	 */
	public static boolean storeDataTrackingSystemUsers(ToDmgDts porter, Connection connection, Map users) throws SQLException {
		// Define the success flag used to return the success of storing the User information into the database.
		boolean success = true;
		// Define the SQL to store the User information into the database.
		String userSql = "INSERT INTO contact(contact_short_name, person_name, email, active_editor) VALUES(?, ?, ?, ?)";
		// Define the statement to execute on the database.
		PreparedStatement userStmt = connection.prepareStatement(userSql, PreparedStatement.RETURN_GENERATED_KEYS);
		
		// Define an iterator over all of the Users in the mapping.
		Iterator itr = users.values().iterator();
		// Loop through the Users in the iterator.
		while (itr.hasNext()) {
			// Extract the current User in the Iterator.
			User user = (User)itr.next();
			// Define a flag for the success of the current User's insert process
			boolean localSuccess = true;
			
			try {
				// Assign the values for the User.
				userStmt.setString(1, user.getUserId());
				userStmt.setString(2, user.getName());
				userStmt.setString(3, user.getEmail());
				userStmt.setBoolean(4, user.isActive());
				// Execute the statement on the database.
				userStmt.execute(); 
			}
			// The statement didn't execute, so it needs to fail properly.
			catch (SQLException e) {
				localSuccess = false;
				success = false;
				porter.appendMergeException(new MergeException("User "+user.getName()+" could not be inserted into the DTS database:  "+e.getMessage()));
			}

			// Obtain the auto-generated DTS ID keys after a successful insert.
			if (localSuccess) {
				// Define an exception holder for a later display.
				SQLException sqlEx = null;
				try {
					// Get the auto-generated key for the User
					ResultSet keys = userStmt.getGeneratedKeys();
					// Assign the DTS ID if the key was returned.
					if (keys.next()) { user.setDtsId(keys.getInt(1)); }
					// There is some problem, since the key was not returned.
					else { localSuccess = false; }
					// Properly close the results stream.
					try { keys.close(); } catch (SQLException e) {}
				} catch (SQLException e) {
					localSuccess = false;
					sqlEx = e;
				}
				// There was a local failure for getting the key, so notify the user.
				if (!localSuccess) {
					success = false;
					porter.appendMergeException(new MergeException("User "+user.getName()+" was unable to obtain an auto-generated DTS ID."+(sqlEx == null ? "" : "  "+sqlEx.getMessage())));
				}
			}
		}
		
		// Properly close the open statement stream.
		try { userStmt.close(); } catch (SQLException e) {};
		
		// Return the status of the execution of the User inserts.
		return success;
	}
}
