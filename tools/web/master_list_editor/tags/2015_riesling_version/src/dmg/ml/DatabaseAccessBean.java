package dmg.ml;

import java.sql.Connection;
import java.sql.SQLException;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.sql.DataSource;

/**
 * <p>The DatabaseAccessBean class is a container for accessing the 
 * Master List database.  It keeps track of the connection pool and generates
 * new connections to the database on request.</p>
 * <p><b>It requires that their is a java.sql.DataSource called
 * <i>connection</i> that contains the information necessary to connect to the
 * <i>dmg_general_ml</i> database.</b></p>
 *
 * @author Joel Clawson
 */
public class DatabaseAccessBean {

    private static Connection connection;
    
    /**
     * Prevent the constructor from being called by other classes.
     **/
    private DatabaseAccessBean() {}
    
    /**
     * Get a connection to the Master List database.
     * @return A connection to the database.
     * @throws MasterListException when there is a problem accessing the 
     * database connection pool or establishing a connection to the database.
     **/
    public static Connection getConnection() throws MasterListException {
        
        try {
            if (connection == null || connection.isClosed()) {
                try {
                    DataSource dataSource = (DataSource)(new InitialContext()).lookup("java:comp/env/connection");
                    connection = dataSource.getConnection();
                } catch (NamingException e) {
                    throw new MasterListException("Cannot find the connection "+
                                                  "variable in the initial context.",e);
                } catch (SQLException e) {
                    throw new MasterListException("Cannot establish a new connection "+
                                                  "to the database.",e);
                }
            }
        } catch (SQLException e) { throw new MasterListException("Unable to test connection status.",e); }
        return connection;
    }
}
