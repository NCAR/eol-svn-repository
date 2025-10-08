package dmg.ml;

import java.sql.Connection;
import java.sql.SQLException;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.sql.DataSource;

/**
 * <p>The GeneralDatabaseAccessBean class is a container for accessing the 
 * Master List database.  It keeps track of the connection pool and generates
 * new connections to the database on request.</p>
 * <p><b>It requires that their is a java.sql.DataSource called
 * <i>connection</i> that contains the information necessary to connect to the
 * <i>dmg_general_ml</i> database.</b></p>
 *
 * @author Joel Clawson
 */
public class GeneralDatabaseAccessBean {
    
    private static GeneralDatabaseAccessBean instance;
    
    private DataSource dataSource;
    
    /**
     * Prevent the constructor from being called by other classes.
     **/
    private GeneralDatabaseAccessBean() {}
    
    /**
     * Get a connection to the Master List database.
     * @return A connection to the database.
     * @throws MasterListException when there is a problem accessing the 
     * database connection pool or establishing a connection to the database.
     **/
    public Connection getConnection() throws MasterListException {
        // Make sure that the the data connection pool exists.
        if (dataSource == null) {
            try {
                dataSource = (DataSource)(new InitialContext()).lookup(
                        "java:comp/env/connection");
            } catch (NamingException e) {
                throw new MasterListException("Cannot find the connection "+
                        "variable in the initial context.",e);
            }
        }
        
        // Get a connection from the connection pool.
        try { return dataSource.getConnection(); }
        catch (SQLException e) {
            throw new MasterListException("Cannot establish a new connection "+
                    "to the database.",e);
        }
    }
    
    /**
     * Get the singleton instance of the GeneralDatabaseAccessBean.
     * @return The instance of the GeneralDatabaseAccessBean.
     **/
    public static GeneralDatabaseAccessBean getInstance() {
        if (instance == null) { instance = new GeneralDatabaseAccessBean(); }
        return instance;
    }
}
