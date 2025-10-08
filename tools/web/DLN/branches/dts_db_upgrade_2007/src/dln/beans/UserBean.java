package dln.beans;

import java.sql.*;
import java.util.*;

/**
 * The UserBean is a container for a User.  It also contains functions on how
 * to load the user from the database, insert new users, and update an existing
 * one.  It can also generate lists of users as well.
 * 
 * @author jclawson
 * @author Dan Sullivan
 */
public class UserBean extends DefaultBean implements Comparable<UserBean> {
	
	private static final long serialVersionUID = -7617543535745360918L;
	
	private boolean active;
	private Integer contactId;
	private String email, personName, shortName;

	/**
	 * Create a new instance of a UserBean.
	 */
	public UserBean() {}
	
	/**
	 * Create a new instance of a UserBean.
	 * @param id The DTS id for the user.
	 */
	public UserBean(int id) {
		contactId = id;
	}
	
	/**
	 * Compare this user to the specified user for sort order.  This is the
	 * natural sort order of the short name of the user.
	 * @param user The user to compare to this user.
	 * @return A negative integer, zero, or a positive integer if this user is
	 * less than, equal to, or greater than the specified user.
	 */
	public int compareTo(UserBean user) {
		return getShortName().compareTo(user.getShortName());
	}
	
	/**
	 * Get the list of active users in the database.
	 * @return The currently active users for the DLN.
	 * @throws SQLException if there is a problem reading the database.
	 */
	public List<UserBean> getActiveUsers() throws SQLException {
		List<UserBean> list = new ArrayList<UserBean>();
		// Define the connection to the database.
		Connection connection = getConnection();
		try {
			// Get the list of users.
			list.addAll(getUserList(connection, true));
			// Close the open connection stream.
		} catch (SQLException e) {
			throw e;
		} finally {
			try { connection.close(); } catch (SQLException e) {}
		}
		// Now return the list that the connection has been closed.
		return list;
	}
	
	/**
	 * Get a list of all known users/contacts in the database.
	 * @return The list of all users/contacts.
	 * @throws SQLException if there is a problem reading the database.
	 */
	public List<UserBean> getAllUsers() throws SQLException {
		List<UserBean> list = new ArrayList<UserBean>();
		// Define the connection to the database.
		Connection connection = getConnection();
		// Get the list of users.
		try {
			list.addAll(getUserList(connection, null));
		} catch (SQLException e) {
			throw e;
		} finally {
			// Close the open connection stream.
			try { connection.close(); } catch (SQLException e) {}
		}
		// Now return the list that the connection has been closed.
		return list;
	}
	
	/**
	 * Get the set of all users as a mapping by their contact IDs.
	 * @return The mapping of the Users by their contact IDs.
	 * @throws SQLException if there is a problem reading the users from the database.
	 */
	public Map<Integer, UserBean> getAllUsersMap() throws SQLException {
		Map<Integer, UserBean> mapping;
		// Define the connection to the database.
		Connection connection = getConnection();
		try {
			// Get the mapping of users.
			mapping = getAllUsersMap(connection);
		} catch (SQLException e) {
			throw e;
		} finally {
			// Close the open connection stream.
			try { connection.close(); } catch (SQLException e) {}
		}
		// Now return the list that the connection has been closed.
		return mapping;
	}
	
	/**
	 * Get the set of all users as a mapping by their contact IDs.
	 * @param connection The connection to use to access the database.
	 * @return The mapping of the Users by their contact IDs.
	 * @throws SQLException if there is a problem reading the users from the database.
	 */
	public Map<Integer, UserBean> getAllUsersMap(Connection connection) throws SQLException {
		// Define the container map.
		TreeMap<Integer, UserBean> mapping = new TreeMap<Integer, UserBean>();
		// Loop through all of the users and add them to the map.
		for (UserBean user: getUserList(connection, null)) {
			mapping.put(user.getContactId(), user);
		}
		// Return the new mapping.
		return mapping;
	}
	
	/**
	 * Get the unique database identifier for the user.
	 * @return The User's internal ID number.
	 */
	public Integer getContactId() { return contactId; }
	
	/**
	 * Get the unique database identifier for the user as a String.
	 * @return The User's internal ID as a String.
	 */
	public String getContactIdString() { return contactId.toString(); }

	/**
	 * Get the user's email address.
	 * @return The user's email address.
	 */
	public String getEmail() { return email == null ? "" : email.trim(); }

	/**
	 * Get the user's full name.
	 * @return The user's full name.
	 */
	public String getPersonName() { return personName == null ? "" : personName.trim(); }
	
	/**
	 * Get the short name identifier for the user.
	 * @return The user's short name ID.
	 */
	public String getShortName() { return shortName == null ? "" : shortName.trim(); }
	
	/**
	 * Get a list of users from the database.
	 * @param active <code>true</code> to get only the active users, <code>false</code> to get
	 * only the inactive users, or <code>null</code> to get all users.
	 * @return The list of users based on the active flag.
	 * @throws SQLException if there is a problem accessing or reading the database.
	 */
	private List<UserBean> getUserList(Connection connection, Boolean active) throws SQLException {
		// Define the list to store the list of Users.
		List<UserBean> users = new ArrayList<UserBean>();
		
		// Define the SQL to pull the user information from the database.
		String sql = "SELECT contact_id, contact_short_name, person_name, email, active_editor FROM contact";
		// Handle the case where the active flag is not NULL
		if (active != null) { sql += " WHERE active_editor=?"; }
		
		// Add the order by clause to the SQL to have the users ordered by their name.
		sql += " ORDER BY active_editor DESC, person_name ASC";
		
		// Define the statement to be executed on the database.
		PreparedStatement stmt = connection.prepareStatement(sql);
		// Set the value for the active flag if it is not NULL.
		if (active != null) { stmt.setBoolean(1, active); }
		
		// Execute the query on the database and obtain the results.
		ResultSet results = stmt.executeQuery();
		
		// Loop through the rows of the results.
		while (results.next()) {
			UserBean user = new UserBean(results.getInt(1));
			user.setShortName(results.getString(2));
			user.setPersonName(results.getString(3));
			user.setEmail(results.getString(4));
			user.setActive(results.getBoolean(5));
			
			// The user has been defined, so add it to the list.
			users.add(user);
		}
		
		// Close the open database streams.
		try { results.close(); } catch (SQLException e) {}
		try { stmt.close(); } catch (SQLException e) {}	
		
		// Return the list of users loaded from the database.
		return users;
	}
	
	/**
	 * Insert the current user into the database.
	 * @throws SQLException if there is a problem accessing the database or inserting the User.
	 */
	public void insert() throws  SQLException {
		// Define the connection to the database.
		Connection connection = getConnection();
		try {
			// Make sure to turn off auto commit so all of the commands will execute as a single transaction.
			connection.setAutoCommit(false);
	
			// Define the SQL to insert the user information into the database.
			String sql = "INSERT INTO contact(contact_short_name, person_name, email, active_editor) VALUES(?, ?, ?, ?)";
			// Define the statement to be executed on the database.
			PreparedStatement stmt = connection.prepareStatement(sql, PreparedStatement.RETURN_GENERATED_KEYS);
			stmt.setString(1, getShortName());
			stmt.setString(2, getPersonName());
			stmt.setString(3, getEmail());
			stmt.setBoolean(4, isActive());
			// Perform the update in the database.
			stmt.execute();
			
			// Get the auto-generated key for the user.
			ResultSet keys = stmt.getGeneratedKeys();
			if (keys.next()) {
				setContactId(keys.getInt(1));
			} else {
				throw new SQLException("The database did not generate a key for the user.");
			}
			
			// Close the open database streams.
			try { keys.close(); } catch (SQLException e) {}
			try { stmt.close(); } catch (SQLException e) {}

			// Save all of the executed statements to the database permanently.
			connection.commit();
		} catch (SQLException e) {
			throw e;
		} finally {	
			try { connection.close(); } catch (SQLException e) {}
		}
	}
	
	/**
	 * Determine if the user is an active editor/user.
	 * @return <code>true</code> if the user is an active editor, <code>false</code> if it is not.
	 */
	public boolean isActive() { return active; }
	
	/**
	 * Load the specified user from the database.
	 * @param contactId The database ID of the user to be loaded.
	 * @throws SQLException if there is a problem accessing or reading the user from the database.
	 */
	public void loadUser(int contactId) throws SQLException {
		// Define the connection to the database.
		Connection connection = getConnection();
		try {
			// Define the SQL to pull the user information from the database.
			String sql = "SELECT contact_id, contact_short_name, person_name, email, active_editor FROM contact WHERE contact_id=?";
			// Define the statement to be executed on the database.
			PreparedStatement stmt = connection.prepareStatement(sql);
			stmt.setInt(1, contactId);
			
			// Execute the statement and get the results.
			ResultSet results = stmt.executeQuery();
			if (results.next()) {
				setContactId(results.getInt(1));
				setShortName(results.getString(2));
				setPersonName(results.getString(3));
				setEmail(results.getString(4));
				setActive(results.getBoolean(5));
			}
			
			// Close the open database streams.
			try { results.close(); } catch (SQLException e) {}
			try { stmt.close(); } catch (SQLException e) {}
		} catch (SQLException e) {
			throw e;
		} finally {
			try { connection.close(); } catch (SQLException e) {}
		}
	}

	/**
	 * Set the active editor flag for the user.
	 * @param active <code>true</code> if the user is an active editor, <code>false</code>
	 * if it is not.
	 */
	public void setActive(boolean active) { this.active = active; }
	
	/**
	 * Set the unique internal database ID for the user. 
	 * @param id The database ID of the user.
	 */
	public void setContactId(Integer id) { contactId = id; }

	/**
	 * Set the email address for the user.
	 * @param email The user's email address.
	 */
	public void setEmail(String email) { this.email = email; }

	/**
	 * Set the full name of the user in the database.
	 * @param name The user's full name.
	 */
	public void setPersonName(String name) { personName = name; }
	
	/**
	 * Set the unique short name identifier for the user.
	 * @param name The user's short name ID.
	 */
	public void setShortName(String name) { shortName = name; }
	
	/**
	 * Update the database with the new information for this user.
	 * @throws SQLException if there was a problem accessing or updating the database.
	 */
	public void update() throws SQLException {
		// Define the connection to the database.
		Connection connection = getConnection();
		try {
			// Need to turn off auto commit so all of the update statements will be executed as a single transaction.
			connection.setAutoCommit(false);
	
			// Define the SQL to update the user information it the database.
			String sql = "UPDATE contact SET contact_short_name=?, person_name=?, email=?, active_editor=? WHERE contact_id=?";
			// Define the statement to be executed on the database.
			PreparedStatement stmt = connection.prepareStatement(sql);
			stmt.setString(1, getShortName());
			stmt.setString(2, getPersonName());
			stmt.setString(3, getEmail());
			stmt.setBoolean(4, isActive());
			stmt.setInt(5, getContactId());
			
			// Perform the update in the database.
			stmt.execute();
			// Close the open database streams.
			try { stmt.close(); } catch (SQLException e) {}
					
			// All of the commands executed, so save them to the database permanently.
			connection.commit();
		} catch (SQLException e) {
			throw e;
		} finally {
			// Close the open connection stream.
			try { connection.close(); } catch (SQLException e) {}
		}
	}
}
