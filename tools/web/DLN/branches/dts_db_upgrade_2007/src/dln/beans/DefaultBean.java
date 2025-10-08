package dln.beans;

import java.io.*;
import java.sql.*;
import java.text.*;

/**
 * The DefaultBean class is a generalized bean for the DLN.  It is a container
 * for common functions that are used by all of the beans.
 * 
 * @author jclawson
 */
public abstract class DefaultBean implements Serializable {

    protected SimpleDateFormat dateFormat;
	
    /**
     * Create a new DefaultBean.
     */
	public DefaultBean() {
        dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        dateFormat.setLenient(false);
	}
	
    /**
     * Convert a date String in YYYY-MM-DD format to a Date object used by SQL.
     * @param date The date String in YYYY-MM-DD format.
     * @return The date String as a Date object or <code>null</code> if the 
     * date String is <code>null</code> or is the empty String.
     * @throws ParseException when the date is not in YYYY-MM-DD format or
     * if the date is not a valid date.
     **/
    protected java.sql.Date convertDate(String date) throws ParseException {
        if (date == null || date.equals("")) { return null; }
        else {
        	return new java.sql.Date(dateFormat.parse(date).getTime());
        }
    }
    
    /**
     * Get a connection to the DTS database.
     * @return A connection to the database.
     * @throws SQLException if a connection could not be established or if the driver
     * to the database could not be found.
     */
	public static Connection getConnection() throws SQLException {
		try { Class.forName("com.mysql.jdbc.Driver"); }
		catch(ClassNotFoundException e) {
			System.out.println( "Unable to load driver." );
			e.printStackTrace();
		}
		return DriverManager.getConnection("jdbc:mysql://localhost/dmg_dts","dts-full","l@micbc");
	}
}