package eventupdate;



import java.sql.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map.Entry;
import java.util.Set;

/**
 * The <code>Database</code> class is used to establish a connection to a MySQL database.
 * Select, update, and insert statements can be executed.
 * Changes to the database are not finalized immediately and can be committed or rolledback.
 * @author Pierce Martin
 */
public class Database {

    private Connection conn = null;

    /** The address of the MySQL host database. */
    String host;
    /** The name of the host location that is defined for this Database object. */
    String db;
    /** The username used in the connection to the database. */
    String user;
    /** The password for the user. */
    String pass;

    /**
     * Creates a connection to the MySQL server.
     * @param Host the address of the MySQL host database
     * @param Db the name of the database to use
     * @param User the username to be used in the connection
     * @param Pass the password for the user
     * @throws java.sql.SQLException if any errors occur while connecting to the database.
     *
     */
    public Database(String Host, String Db, String User, String Pass) throws SQLException {
        host = Host;
        db = Db;
        user = User;
        pass = Pass;

        //create connection statement ex: "jdbc:mysql://localhost/test"
        String statement = "jdbc:mysql://" + host + "/" + db;
        conn = DriverManager.getConnection(statement, user, pass);

        //turn off autocommit so that rollback and commit can be used
        conn.setAutoCommit(false);

    }

    /**
     * Creates a connection to the MySQL server at the specified host.
     * Used when specifying a database to use is not necessary.
     * @param Host the address of the MySQL host database
     * @param User the username to be used in the connection
     * @param Pass the password for the user
     * @throws java.sql.SQLException if any errors occur while connecting to the MySQL server.
     *
     */
    public Database(String Host, String User, String Pass) throws SQLException {
        host = Host;
        user = User;
        pass = Pass;

        //create connection statement ex: "jdbc:mysql://localhost"
        String statement = "jdbc:mysql://" + host;
        conn = DriverManager.getConnection(statement, user, pass);

        //turn off autocommit so that rollback and commit can be used
        conn.setAutoCommit(false);
    }

    /**
     * Changes the working database.  Issues a MySQL <code>USE database</code> command.
     * @param database the name of the database to use
     * @throws java.sql.SQLException if any errors occur while changing database.
     */
    public void changeDatabase(String database) throws SQLException {
        db = database;
        Statement stmt = null;
        stmt = conn.createStatement();
        stmt.execute("USE " + database);
    }

    /**
     * Issues a select query to the database and return the results as a <code>String</code>.
     * The select command is issued as a normal MySQL select: <code>SELECT attributes FROM table WHERE conditions</code>
     * @param attributes the attributes to display from the table.  Multiple attributes can be separated by commas.
     * @param table the table to perform the select upon
     * @param conditions the set of conditions that must be met.  Follow MySQL syntax when passing multiple conditions.
     * @return a <code>String</code> containing all of the rows that matched the select query.
     * One row is printed per line and each attribute is separated by \t
     * @throws SQLException if any errors occurred during the select.
     */
    public String printSelect(String attributes, String table, String conditions) throws SQLException {
        ArrayList<HashMap> results = select(attributes, table, conditions);
        String output = "";


        //loop through each row
        for(HashMap data : results){
            Set<Entry> s = data.entrySet();
            //loop through each column
            for(Entry entry : s){
                if (entry.getValue() != null) {
                    output += entry.getKey().toString() + ": " + entry.getValue().toString() + "\t";
                } else {
                    output += entry.getKey().toString() + ": null\t";
                }
            }
            output += "\n";
        }
        return output;
    }

    /**
     * Performs an update to a MySQL table.
     * The update command is issued as a normal MySQL update: <code>UPDATE table SET attributes WHERE conditions</code>
     * @param table the table that will be updated
     * @param attributes the attributes that will be changed in this table
     * @param conditions the set of conditions that must be met for the update to occur.
     * @return the number of rows that were updated
     * @throws SQLException if any errors occur during the update.
     */
    public int update(String table, String attributes, String conditions) throws SQLException {
        int result = 0;

        Statement stmt = conn.createStatement();
        result = stmt.executeUpdate("UPDATE " + table + " SET " + attributes + " WHERE " + conditions);
        //System.out.println("UPDATE " + table + " SET " + attributes + " WHERE " + conditions);
        // release resources if they are no-longer needed
        if (stmt != null) {
            stmt.close();
            stmt = null;
        }
        return result;
    }

    /**
     * Inserts a set of values into the database
     * @param table the table which the data will be inserted into
     * @param attributes the column names of the insert
     * @param values the actual data to be inserted
     * @return the number of rows that were inserted
     * @throws SQLException if any errors occur during the insert.
     */
    public int insert(String table, String attributes, String values) throws SQLException {
        Statement stmt = null;
        int result = 0;

        stmt = conn.createStatement();
        result = stmt.executeUpdate("INSERT INTO " + table + "(" + attributes + ") VALUES (" + values + ")");

        // release resources if they are no-longer needed
        if (stmt != null) {
            stmt.close();
            stmt = null;
        }

        return result;
    }

    /**
     * Issues a select query to the database and return the results as an <code>ArrayList</code>.
     * The select command is issued as a normal MySQL select: <code>SELECT attributes FROM table WHERE conditions</code>
     * @param attributes The attributes to display from the table.  Multiple attributes can be separated by commas.
     * @param table the name of the database table
     * @param conditions The set of conditions that must be met.  Follow MySQL syntax when passing multiple conditions.
     * @return an ArrayList with each entry representing one row from the select query.
     * Each entry in the <code>ArrayList</code> contains a <code>HashMap</code> with each attribute as the key and the result as the value.
     * The values of the columns in each row can be referenced by <code>HashMap.get("attribute")</code>
     * @throws SQLException if any errors occur during the select
     */
    public ArrayList select(String attributes, String table, String conditions) throws SQLException {
        ArrayList<HashMap> results = new ArrayList();
        Statement stmt = null;
        ResultSet rs = null;


        stmt = conn.createStatement();
        rs = stmt.executeQuery("SELECT " + attributes + " FROM " + table + " WHERE " + conditions);
        //gets the column information
        ResultSetMetaData rsmd = rs.getMetaData();
        int numCols = rsmd.getColumnCount();


        while (rs.next()) {
            HashMap data = new HashMap();
            //Adds each attribute to a HashMap
            for (int i = 1; i <= numCols; i++) {
                data.put(rsmd.getColumnName(i), rs.getObject(i));
            }
            //Adds the HashMap to the ArrayList
            results.add(data);
        }

        // release resources
        // in reverse-order of their creation
        // if they are no-longer needed
        if (rs != null) {
            rs.close();
            rs = null;
        }

        if (stmt != null) {
            stmt.close();
            stmt = null;
        }

        return results;
    }

    /**
     * Finalizes the changes that have been made to the database since the last commit.
     * @throws SQLException if any errors occur during the commit.
     */
    public void commit() throws SQLException {
        conn.commit();
    }

    /**
     * Undoes the changes that have been made since the last commit.
     * @throws SQLException if any errors occur during the rollback.
     */
    public void rollback() throws SQLException {
            conn.rollback();
    }

    /**
     * Returns the <code>Connection</code> object associated with this database
     * @return the Connection
     * @see java.sql.Connection
     */
    public Connection getConnection() {
        return conn;
    }
}
