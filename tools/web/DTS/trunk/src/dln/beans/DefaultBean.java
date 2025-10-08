package dln.beans;

import java.io.*;
import java.sql.*;
import java.text.*;
import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.sql.DataSource;

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
        Context ctx = null;
        DataSource ds = null;
        try {
          ctx = new InitialContext();
          ds = (DataSource) ctx.lookup("java:/comp/env/jdbc/dmg_dts");
          return ((null == ds) ? null : ds.getConnection());
        } catch (NamingException e) {
          return null;
        }
	}
}
