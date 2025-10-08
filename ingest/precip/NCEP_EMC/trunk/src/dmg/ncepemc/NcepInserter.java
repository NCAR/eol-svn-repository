package dmg.ncepemc;

import java.io.*;
import java.sql.*;
import java.util.*;
import java.util.regex.*;

/**
 * <p>The NcepInserter is a class that inserts the NCEP/EMC precipitation data
 * (Sid Katz precip) into the database.  There are several datasets that are
 * updated.  Each dataset will have the new files added and the begin and end
 * dates will be updated to include the new files.</p>
 * <p>The inserter treats all datasets as a single database transaction.  It
 * will either insert all of the files for all of the datasets or it will not
 * change the database at all.</p>
 *
 * @author Joel Clawson
 **/

/** <p>Added file format 55 (GNU_ZIP_FORMAT), changed hourly and daily
 *  precip from 125 (GNU_COMPRESS_FILE_FORMAT) to 55, and changed all 
 *  others (.Z) from 66 (COMPRESS_FILE_FORMAT) to 125.</p>
 *
 *  @author Linda Echo-Hawk (13 Feb 2019)
 **/

/** <p>Added code to ensure that file size for DB would not be zero and
 *  commented out testing-only println statements.</p>
 *
 *  @author Linda Echo-Hawk (25 Mar 2019)
 **/

/** <p> Changed default time from one MINUTE to one HOUR, as all files contain
 *  one hour of data.</p>
 *
 *  @author Linda Echo-Hawk (29 June 2020)
 **/

/** <p> Added GRIB-2 file format to account for file format changes
 *  from NOAA starting 20 July 2020, and added check for zero size files.</p>
 *
 *  @author Linda Echo-Hawk (3 Sept 2020)
 **/

/** <p> Modified the pattern matcher for file names to pick up all files
 *  including the new grb2 files, and added checks for file names the matcher
 *  might have missed.</p>
 *
 *  @author Linda Echo-Hawk (7 Oct 2020)
 **/


/** <p> Modified the NcepInserter to require two more arguments, one for
 *  database user name and one for database password.</p>
 *
 *  @author Linda Echo-Hawk (9 Mar 2021)
 **/

public class NcepInserter {

    // Constants used for connecting to the database.
    // private static final String URL = "jdbc:mysql://emdac.eol.ucar.edu/zith9";
    private static final String URL = "jdbc:mysql://emdac.eol.ucar.edu/zith9?useSSL=false&allowPublicKeyRetrieval=true";
    // private static final String URL = "jdbc:mysql://farskol.eol.ucar.edu/zith9";
    // private static final String USER = "zithupdate";
    // private static final String PASS = "change-999";

    // Constants used for defining data file formats.
    public static final int GNU_COMPRESS_FILE_FORMAT = 125;
    public static final int COMPRESS_FILE_FORMAT = 66;
    public static final int GIF_FILE_FORMAT = 34;
	public static final int GNU_ZIP_FORMAT = 55;
	public static final int GRIDDED_BINARY_EDITION_2_FORMAT = 103;

    private int month, year;

	private String pass;
	private String user;
    private Connection connection;
    private PreparedStatement datasetStmt, fileStmt, idStmt;
    private ResultSet rs;

    /**
     * Create a new instance of a NcepInserter.
     * @param month The month of the data files being inserted.
     * @param year The year of the data files being inserted.
	 * @param user Your database user name.
	 * @param pass Your database password.
     * @throws ClassNotFoundException if the Driver used to connect to the database
     * cannot be found.
     * @throws SQLException if the connection to the database cannot be established or
     * the statements to insert the files or update the dataset cannot be prepared.
     **/
    public NcepInserter(int month, int year, String user, String pass) throws ClassNotFoundException, SQLException {
        this.month = month;
        this.year = year;
		this.user = user;
		this.pass = pass;

	// Establish the connection to the database.
        Class.forName("com.mysql.jdbc.Driver");
        // connection = DriverManager.getConnection(URL, USER, PASS);
        connection = DriverManager.getConnection(URL, user, pass);
        connection.setAutoCommit(false);

	// Create the statements that will insert the files and update the dataset
	// in the database.
	idStmt = connection.prepareStatement("SELECT id FROM dataset WHERE archive_ident=?");
        fileStmt = connection.prepareStatement("INSERT INTO file(dataset_id,host,directory,filename," +
                                               "begin_date,end_date,format_id,size_kb,purpose,data_archive_date) "+
                                               "VALUES((SELECT id FROM dataset WHERE archive_ident=?),'localhost',?,?,?,?,?,?,'data',?)");
	datasetStmt = connection.prepareStatement("UPDATE dataset SET " +
		                                  "begin_date=(SELECT MIN(begin_date) FROM file WHERE " +
						  "dataset_id=? AND " +
					          "purpose='data' AND filename!='nwsli.uniq.new'),"+
					          "end_date=(SELECT MAX(end_date) FROM file WHERE " +
					          "dataset_id=? AND " +
					          "purpose='data' and filename!='nwsli.uniq.new') " +
						  "WHERE archive_ident=?");
    }

    /**
     * Properly close down the connection to the database.
     * @throws SQLException if there is a problem closing the connection.
     **/
    public void close() throws SQLException {
        connection.close();
    }

    /**
     * Save all of the statmenets sent to the database.
     * @throws SQLException if there is a problem executing the commit.
     **/
    public void commit() throws SQLException {
        connection.commit();
    }

    /**
     * Insert the *.gz files found in the specified directory in the database
     * for the specified dataset.
     * @param directory The directory containing the data files.
     * @param datasetId The dataset id the files are attached to.
     * @param defaultLength The default length of a file if it is not defined from
     * the file name.  It should be something like Calendar.HOUR.
     * @throws SQLException if there is a problem executing the file inserts.
     * @see java.util.Calendar
     **/
    public void insertCompressFiles(File directory, String datasetId, int defaultLength) throws SQLException {
	// Define the compress file pattern used to match file names against.
        // Pattern filePattern = Pattern.compile("(\\d{4})(\\d{2})(\\d{2})(\\d{2})(\\..{3})?(\\.Z)?");
		// EXAMPLE: st4_conus.2020073100.01h.grb2
		// grp1=year,grp2=mon,grp3=day,grp4=hour,group5=.01h,grp6=grb2
        Pattern filePattern = Pattern.compile("(\\d{4})(\\d{2})(\\d{2})(\\d{2})(\\..{3})?(\\..+)?");

	// Loop through all of the files found in the directory.
        for (File file: Arrays.asList(directory.listFiles())) {
	    // Create the matcher used to match the file name and file pattern.
            Matcher fileMatcher = filePattern.matcher(file.getName());

			
	    // Only try to insert if the file name matches the pattern.
			if (fileMatcher.find()) {

		// Create the date from the pattern groups.
                Calendar date = Calendar.getInstance();
                date.clear();
                date.set(Calendar.YEAR, Integer.parseInt(fileMatcher.group(1)));
                date.set(Calendar.MONTH, Integer.parseInt(fileMatcher.group(2)) - 1);
                date.set(Calendar.DAY_OF_MONTH, Integer.parseInt(fileMatcher.group(3)));
                date.set(Calendar.HOUR, Integer.parseInt(fileMatcher.group(4)));

		// Set the end date to the filename date.
                fileStmt.setString(5, String.format("%1$tY-%1$tm-%1$td %1$tH:%1$tM:%1$tS", date));

		// Adjust the date to the begin date depending on the information in the filename.
                if (fileMatcher.group(5) == null || fileMatcher.group(5).equalsIgnoreCase(".Grb")) {
                    date.add(defaultLength, -1);
                } else {
                    date.add(Calendar.HOUR, -1 * Integer.parseInt(fileMatcher.group(5).substring(1, 3)));
                }
                date.add(Calendar.SECOND, 1);
		// Set the begin date to the adjusted date.
                fileStmt.setString(4, String.format("%1$tY-%1$tm-%1$td %1$tH:%1$tM:%1$tS", date));

		// Set the remaining parts of the statement.
                fileStmt.setString(1, datasetId);
                fileStmt.setString(2, directory.getAbsolutePath());
                fileStmt.setString(3, file.getName());


				// EXAMPLE: st4_conus.2020073100.01h.grb2
				// if (fileMatcher.group(6) == fileMatcher.group(6).equalsIgnoreCase(".grb2")) {
				if (fileMatcher.group(6).equalsIgnoreCase(".grb2")) {
				  	fileStmt.setInt(6, GRIDDED_BINARY_EDITION_2_FORMAT);
				}
				else
				{
				   	fileStmt.setInt(6, GNU_COMPRESS_FILE_FORMAT);
				}
					
				// make sure that the size isn't zero in the database (CODIAC)
				if (file.length() == 0)
				{
					System.out.println("File is empty: " + file.getName());
				}
				else
				{
					if (((int)Math.ceil((double)(file.length() / 1024))) > 0)
					{
						fileStmt.setInt(7, (int)Math.ceil((double)(file.length() / 1024)));
					}
					else
					{
						fileStmt.setInt(7, 1);
					}
				}

                fileStmt.setDate(8, new java.sql.Date(Calendar.getInstance().getTime().getTime()));
		// Make sure to execute the statement so it gets applied to the database.
            // System.out.println(fileStmt);
                fileStmt.execute();
            }
			else
			{
				System.out.println("File pattern not matched: " + file.getName());

			}
        }

	// Update the dataset begin and end dates.
	idStmt.setString(1,datasetId);
	idStmt.execute();
	rs = idStmt.getResultSet();
	rs.next ();
	  int idVal = rs.getInt ("id");
	  // System.out.println ( "id = " + idVal );
	rs.close ();
        datasetStmt.setString(1, String.format("%d", idVal));
        datasetStmt.setString(2, String.format("%d", idVal));
        datasetStmt.setString(3, datasetId);
	// Make sure to execute the statement so it gets applied to the database.
            // System.out.println(datasetStmt);
        datasetStmt.execute();
    }

    /**
     * Insert the daily precipitation files into the database for the daily preciptation
     * dataset.
     * @param archive The base directory for all of the NCEP/EMC data.
     * @throws SQLException if there is a problem inserting the files or updating the
     * daily precipitation dataset.
     **/
    public void insertDailyFiles(String archive) throws SQLException {
	// Define the directory where the daily precip files are stored.
        File directory = new File(String.format("%s/dly_gage/%04d%02d", archive, year, month));
        String datasetId = "21.005";

	// Loop through all of the files in the directory.
        for (File file: Arrays.asList(directory.listFiles())) {

	    // Only process daily precipitation files.
            if (file.getName().startsWith("gage.dly.prcp.")) {
                String yr = file.getName().substring(14, 18);
                String mo = file.getName().substring(18, 20);
                String dy = file.getName().substring(20, 22);

                fileStmt.setString(1, datasetId);
                fileStmt.setString(2, directory.getAbsolutePath());
                fileStmt.setString(3, file.getName());
                fileStmt.setString(4, String.format("%s-%s-%s 00:00:00", yr, mo, dy));
                fileStmt.setString(5, String.format("%s-%s-%s 23:59:59", yr, mo, dy));
                fileStmt.setInt(6, GNU_ZIP_FORMAT);
				
				// make sure that the size isn't zero in the database (CODIAC)	
				if (file.length() == 0)
				{
					System.out.println("File is empty: " + file.getName());
				}
				else
				{
					if (((int)Math.ceil((double)(file.length() / 1024))) > 0)
					{
						fileStmt.setInt(7, (int)Math.ceil((double)(file.length() / 1024)));
					}
					else
					{
						fileStmt.setInt(7, 1);
					}
				}

                fileStmt.setDate(8, new java.sql.Date(Calendar.getInstance().getTime().getTime()));
  		// Make sure to execute the statement so it gets applied to the database.
            // System.out.println(fileStmt);
                fileStmt.execute();
            }
			else
			{
				System.out.println("File pattern not matched: " + file.getName());

			}
        }

	// Update the dataset to use the new begin and end dates of the files.
	idStmt.setString(1,datasetId);
	idStmt.execute();
	rs = idStmt.getResultSet();
	rs.next ();
	  int idVal = rs.getInt ("id");
	  // System.out.println ( "id = " + idVal );
	rs.close ();
        datasetStmt.setString(1, String.format("%d", idVal));
        datasetStmt.setString(2, String.format("%d", idVal));
        datasetStmt.setString(3, datasetId);
	// Make sure to execute the statement so it gets applied to the database.
            // System.out.println(datasetStmt);
        datasetStmt.execute();
    }

    /**
     * Insert the gag4/ST2gg* files into the database.
     * @param archive The base directory for all of the NCEP/EMC data.
     * @throws SQLException if there is a problem inserting the files or updating the
     * dataset.
     * @see #insertCompressFiles(java.io.File, java.lang.String, int)
     **/
    public void insertGag4Files(String archive) throws SQLException {
        File directory = new File(String.format("%s/grib4km/gag4/%04d%02d", archive, year, month));
        String datasetId = "21.088";
        insertCompressFiles(directory, datasetId, Calendar.HOUR);
    }

    /**
     * Insert the *.gif files into the database.
     * @param archive The base directory for all of the NCEP/EMC data.
     * @throws SQLException if there is a problem inserting the files or updating the dataset.
     **/
    public void insertGifFiles(String archive) throws SQLException {
	// Define the directory that contains the gif files.
        File directory = new File(String.format("%s/preview_gifs/%04d%02d",archive,year,month));
        String datasetId = "21.087";

	// Define the file pattern to match the gif files for to pull out date information.
        Pattern filePattern = Pattern.compile("\\.(\\d{4})(\\d{2})(\\d{2})(\\d{2})\\.(.{3}\\.)?gif");

	// Loop through the files found in the data directory.
        for (File file: Arrays.asList(directory.listFiles())) {

	    // Define the matcher that compares the file name to the file pattern.
            Matcher fileMatcher = filePattern.matcher(file.getName());

	    // Only try to insert the file if it matched the pattern.
            if (fileMatcher.find()) {

		// Pull out the end date from the filename pattern match.
                Calendar date = Calendar.getInstance();
                date.clear();
                date.set(Calendar.YEAR, Integer.parseInt(fileMatcher.group(1)));
                date.set(Calendar.MONTH, Integer.parseInt(fileMatcher.group(2)) - 1);
                date.set(Calendar.DAY_OF_MONTH, Integer.parseInt(fileMatcher.group(3)));
                date.set(Calendar.HOUR, Integer.parseInt(fileMatcher.group(4)));
		// Set the end date for the file in the statement.
                fileStmt.setString(5, String.format("%1$tY-%1$tm-%1$td %1$tH:%1$tM:%1$tS", date));

		// Adjust the begin date from the end date based on the information found in
		// the file pattern.
		// What this set of statements does is pull the 06h, 24h, or 01h from the file name.  
		// Then it takes the numeric portion and multiples that by 1 to get the number of 
		// hours to subtract from the time.  If there is no 06h, etc.  then it uses the 
		// default of one HOUR (which was formerly incorrectly set to one MINUTE).  At the 
		// end, it adds a second to the time, so that it is correct. (note from Janet Scannell May 2020)
                if (fileMatcher.group(5) == null) {
                    date.add(Calendar.HOUR, -1);
                } else {
                    date.add(Calendar.HOUR, -1 * Integer.parseInt(fileMatcher.group(5).substring(0, 2)));
                }
                date.add(Calendar.SECOND, 1);
		// Set the begin date for the file in the statement.
                fileStmt.setString(4, String.format("%1$tY-%1$tm-%1$td %1$tH:%1$tM:%1$tS", date));

		// Set the remaining parts of the statement for the file.
                fileStmt.setString(1, datasetId);
                fileStmt.setString(2, directory.getAbsolutePath());
                fileStmt.setString(3, file.getName());
                fileStmt.setInt(6, GIF_FILE_FORMAT);


				// make sure that the size isn't zero in the database (CODIAC)
				if (file.length() == 0)
				{
					System.out.println("File is empty: " + file.getName());
				}
				else
				{
					if (((int)Math.ceil((double)(file.length() / 1024))) > 0)
					{
						fileStmt.setInt(7, (int)Math.ceil((double)(file.length() / 1024)));
					}
					else
					{
						fileStmt.setInt(7, 1);
					}
				}

                fileStmt.setDate(8, new java.sql.Date(Calendar.getInstance().getTime().getTime()));
		// Make sure to execute the statement so it gets applied to the database.
            // System.out.println(fileStmt);
		        fileStmt.execute();
            }
			else
			{
				System.out.println("File pattern not matched: " + file.getName());

			}
        }

	// Update the dataset begin and end dates to include the new file dates.
	idStmt.setString(1,datasetId);
	idStmt.execute();
	rs = idStmt.getResultSet();
	rs.next ();
	  int idVal = rs.getInt ("id");
	  // System.out.println ( "id = " + idVal );
	rs.close ();
        datasetStmt.setString(1, String.format("%d", idVal));
        datasetStmt.setString(2, String.format("%d", idVal));
        datasetStmt.setString(3, datasetId);
	// Make sure to execute the statement so it gets applied to the database.
            // System.out.println(datasetStmt);
        datasetStmt.execute();
    }

    /**
     * Insert the hourly precipitation files into the database for the hourly preciptation
     * dataset.
     * @param archive The base directory for all of the NCEP/EMC data.
     * @throws SQLException if there is a problem inserting the files or updating the
     * hourly precipitation dataset.
     **/
    public void insertHourlyFiles(String archive) throws SQLException {
	// Define the directory that contains the hourly precip files.
        File directory = new File(String.format("%s/hrly_gage/%04d%02d",archive,year,month));
        String datasetId = "21.004";

	// Loop through all of the files in the directory.
        for (File file: Arrays.asList(directory.listFiles())) {

	    // Only process file that match the hourly file pattern.
            if (file.getName().startsWith("gage.hrly.prcp.")) {
                String yr = file.getName().substring(15,19);
                String mo = file.getName().substring(19,21);
                String dy = file.getName().substring(21,23);

		// Setup the statement with the file details.
                fileStmt.setString(1, datasetId);
                fileStmt.setString(2, directory.getAbsolutePath());
                fileStmt.setString(3, file.getName());
                fileStmt.setString(4, String.format("%s-%s-%s 00:00:00", yr, mo, dy));
                fileStmt.setString(5, String.format("%s-%s-%s 23:59:59", yr, mo, dy));
                fileStmt.setInt(6, GNU_ZIP_FORMAT);

				// make sure that the size isn't zero in the database (CODIAC)
				if (file.length() == 0)
				{
					System.out.println("File is empty: " + file.getName());
				}
				else
				{
					if (((int)Math.ceil((double)(file.length() / 1024))) > 0)
					{
						fileStmt.setInt(7, (int)Math.ceil((double)(file.length() / 1024)));
					}
					else
					{
						fileStmt.setInt(7, 1);
					}
				}

                fileStmt.setDate(8, new java.sql.Date(Calendar.getInstance().getTime().getTime()));
		// Make sure to execute the statement so it gets applied to the database.
            // System.out.println(fileStmt);
                fileStmt.execute();
            }
			else
			{
				System.out.println("File pattern not matched: " + file.getName());

			}
        }

	// Update the dataset to include the dates from the new files.
	idStmt.setString(1,datasetId);
	idStmt.execute();
	rs = idStmt.getResultSet();
	rs.next ();
	  int idVal = rs.getInt ("id");
	  // System.out.println ( "id = " + idVal );
	rs.close ();
        datasetStmt.setString(1, String.format("%d", idVal));
        datasetStmt.setString(2, String.format("%d", idVal));
        datasetStmt.setString(3, datasetId);
	// Make sure to execute the statement so it gets applied to the database.
            // System.out.println(datasetStmt);
        datasetStmt.execute();
    }

    /**
     * Insert the mul4/ST2ml* files into the database.
     * @param archive The base directory for all of the NCEP/EMC data.
     * @throws SQLException if there is a problem inserting the files or updating the
     * dataset.
     * @see #insertCompressFiles(java.io.File, java.lang.String, int)
     **/
    public void insertMul4Files(String archive) throws SQLException {
        File directory = new File(String.format("%s/grib4km/mul4/%04d%02d", archive, year, month));
        String datasetId = "21.089";
        insertCompressFiles(directory, datasetId, Calendar.HOUR);
    }

    /**
     * Insert the rad4/ST2rd* files into the database.
     * @param archive The base directory for all of the NCEP/EMC data.
     * @throws SQLException if there is a problem inserting the files or updating the
     * dataset.
     * @see #insertCompressFiles(java.io.File, java.lang.String, int)
     **/
    public void insertRad4Files(String archive) throws SQLException {
        File directory = new File(String.format("%s/grib4km/rad4/%04d%02d", archive, year, month));
        String datasetId = "21.090";
        insertCompressFiles(directory, datasetId, Calendar.HOUR);
    }

    /**
     * Insert the rfc8 files into the database.
     * @param archive The base directory for all of the NCEP/EMC data.
     * @throws SQLException if there is a problem inserting the files or updating the
     * dataset.
     * @see #insertCompressFiles(java.io.File, java.lang.String, int)
     **/
    public void insertRfc8Files(String archive) throws SQLException {
        File directory = new File(String.format("%s/grib4km/rfc8/%04d%02d", archive, year, month));
        String datasetId = "21.095";
        insertCompressFiles(directory, datasetId, Calendar.DAY_OF_YEAR);
    }

    /**
     * Insert the ST4* files into the database.
     * @param archive The base directory for all of the NCEP/EMC data.
     * @throws SQLException if there is a problem inserting the files or updating the
     * dataset.
     * @see #insertCompressFiles(java.io.File, java.lang.String, int)
     **/
    public void insertStage4Files(String archive) throws SQLException {
        File directory = new File(String.format("%s/stage4/%04d%02d", archive, year, month));
        String datasetId = "21.093";
        insertCompressFiles(directory, datasetId, Calendar.HOUR);
    }

    /**
     * Insert the ubr4/ST2un* files into the database.
     * @param archive The base directory for all of the NCEP/EMC data.
     * @throws SQLException if there is a problem inserting the files or updating the
     * dataset.
     * @see #insertCompressFiles(java.io.File, java.lang.String, int)
     **/
    public void insertUbr4Files(String archive) throws SQLException {
        File directory = new File(String.format("%s/grib4km/ubr4/%04d%02d", archive, year, month));
        String datasetId = "21.092";
        insertCompressFiles(directory, datasetId, Calendar.HOUR);
    }

    /**
     * Cancel out all statements sent to the database since the last commit.
     * @throws SQLException if there is a problem rolling back the database.
     **/
    public void rollback() throws SQLException {
        connection.rollback();
    }

    /**
     * Run the inserter.
     * @param args The month, year, and archive directory for the files to be inserted
     * into the database.
     * @throws ClassNotFoundException when the Driver used to connect to the database
     * cannot be found.
     * @throws SQLException if there is a problem inserting the files or updating the
     * datasets in the database.
     **/
    public static void main(String[] args) throws ClassNotFoundException, SQLException {
	// Parse the command line arguments.
        int month = Integer.parseInt(args[0]);
        int year = Integer.parseInt(args[1]);
        String user = args[2]; // declared here - correctly?
        String pass = args[3];
        String archive = args[4];

	// Define a new inserter (and set up the connection to the database).
        // NcepInserter inserter = new NcepInserter(month, year);
        NcepInserter inserter = new NcepInserter(month, year, user, pass);

	// Insert the files for each dataset.  Order is important here.
        try {
            inserter.insertHourlyFiles(archive);
            inserter.insertDailyFiles(archive);
            inserter.insertGifFiles(archive);
            // inserter.insertGag4Files(archive); Stage 2 files not available after 28 July 2020
            // inserter.insertMul4Files(archive);
            // inserter.insertRad4Files(archive);
            // inserter.insertUbr4Files(archive);
            inserter.insertStage4Files(archive);
	    //last received RFC8 files 31-May-2008
            //inserter.insertRfc8Files(archive); 
        } catch (SQLException e) {
	    // Catch the exception to attempt a proper rollback before quitting.
            System.out.println(e.getMessage());
            inserter.rollback();
            inserter.close();
	    System.exit(1);
        }

	// Save the transaction to the database.
        try {
	    inserter.commit();
        } catch (SQLException e) {
	    // Catch the exception to attempt a proper rollback before quitting.
            System.out.println(e.getMessage());
            inserter.rollback();
	    inserter.close();
	    System.exit(1);
        }

	// Properly close down the connection to the database.
        inserter.close();
    }
}                                                           
