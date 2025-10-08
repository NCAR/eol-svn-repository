package dmg.ncepemc;

import com.enterprisedt.net.ftp.*;
import java.io.*;
import java.util.*;

/**
 * The NcepFTP class is used to download the NCEP/EMC precipitation data
 * from the NCEP FTP server.
 *
 * @author Joel Clawson
 **/
public class NcepFTP {

    // Constants used to connect to the FTP server.
    private static final String DATA_DIRECTORY = "/mmb/gcp/precip/JOSS";
    private static final String HOST = "ftp.emc.ncep.noaa.gov";
    private static final String USERNAME = "anonymous";
    private static final String PASSWORD = "joss@ingest.eol.ucar.edu";

    private int month, year;

    // The FTP connection.
    private FTPClient ftp;

    /**
     * Create a new instance of a NcepFTP.
     * @param month The month of the data to be downloaded.
     * @param year The year of the data to be downloaded.
     * @throws FTPException if there is a problem setting up the FTP connection.
     * @throws IOException if there is a problem setting up the FTP connection.
     **/
    public NcepFTP(int month, int year) throws FTPException, IOException {
	this.month = month;
	this.year = year;

	// Establish the connection to the FTP server.
	ftp = new FTPClient();
	ftp.setRemoteHost(HOST);
	ftp.connect();
    }

    /**
     * Download the ceopgfs files and place them in the specified directory.
     * @param directory The directory where the files should be placed.
     * @throws NcepException if there is a problem downloading any of the files.
     **/
    public void downloadCeopFiles(String directory) throws NcepException {
	// Create a container for exception messages.
	StringBuffer msgs = new StringBuffer();

	// Loop through each day
	for (int day = 1; day <= getDaysInMonth(); day++) {
	    // Loop through each 12 hour period
	    for (int hour = 0; hour <= 12; hour += 12) {
		// Attempt to download the 12 hourly file.
		try { 
		    downloadFile(new File(directory, 
					  String.format("ceopgfs.%04d%02d%02d%02d",
							year, month, day, hour)));
		} catch (NcepException e) {
		    // Add the problem downloading the file to the exception message
		    if (msgs.length() == 0) {
			msgs.append(e.getMessage());
		    } else {
			msgs.append("\n\t").append(e.getMessage());
		    }
		}
	    }
	}

	// Throw an exception if at least one exception was caught
	if (msgs.length() > 0) {
	    throw new NcepException(msgs.toString());
	}
    }

    /**
     * Download the daily precipitation files.
     * @param directory The directory where the files should be placed.
     * @throws NcepException if there is a problem downloading any of the files.
     **/
    public void downloadDailyFiles(String directory) throws NcepException {
	downloadFile(new File(directory, String.format("prcp.dly.%s",(new File(directory)).getName())));
    }

    /**
     * Download the specified file from the FTP server to the local system.
     * @param file The file to be downloaded.
     * @throws NcepException if the file does not exist on the server or if some
     * other problem occurs during the download.
     **/
    private void downloadFile(File file) throws NcepException {

	// Make sure the file exists on the server before downloading.
	boolean exists = false;
	try { exists = ftp.exists(file.getName()); }
	catch (Exception e) {
	    throw new NcepException(e.getMessage());
	}

	// Download the file if it exists.
	if (exists) {
	    System.out.printf("Downloading file: %s\n",file.getName());
	    try {
		ftp.get(file.getAbsolutePath(), file.getName());
	    } catch (Exception e) {
		throw new NcepException(e.getMessage());
	    }
	} else {
	    throw new NcepException(String.format("Could not find %s on the server.",
						  file.getName()));
	}
    }

    /**
     * Download the hourly precipitation files.
     * @param directory The directory where the files should be placed.
     * @throws NcepException if there is a problem downloading any of the files.
     **/
    public void downloadHourlyFiles(String directory) throws NcepException {
	downloadFile(new File(directory, String.format("prcp.hrly.%s",(new File(directory)).getName())));
    }

    /**
     * Download the shapshot files.
     * @param directory The directory where the files should be placed.
     * @throws NcepException if there is a problem downloading any of the files.
     **/
    public void downloadSnapshotFiles(String directory) throws NcepException {
	downloadFile(new File(directory, String.format("snapshot.%s",(new File(directory)).getName())));
    }
    
    /**
     * Download the ST2 4km files.
     * @param directory The directory where the files should be placed.
     * @throws NcepException if there is a problem downloading any of the files.
     **/
    public void downloadST2_4kmFiles(String directory) throws NcepException {
	// Create a container for holding exception messages.
	StringBuffer msgs = new StringBuffer();

	// Loop through each day of the month.
	for (int day = 1; day <= getDaysInMonth(); day++) {
	    // Attempt to download the day file.
	    try { 
		downloadFile(new File(directory, String.format("ST2_4km.%04d%02d%02d",
							       year, month, day)));
	    } catch (NcepException e) {
		// Add the exception message to the container.
		if (msgs.length() == 0) {
		    msgs.append(e.getMessage());
		} else {
		    msgs.append("\n\t").append(e.getMessage());
		}
	    }
	}

	// Throw an exception if at least one was caught earlier.
	if (msgs.length() > 0) {
	    throw new NcepException(msgs.toString());
	}
    }

    /**
     * Download the stage 4 files.
     * @param directory The directory where the files should be placed.
     * @throws NcepException if there is a problem downloading any of the files.
     **/
    public void downloadStage4Files(String directory) throws NcepException {
	downloadFile(new File(directory, String.format("stage4.%s",(new File(directory)).getName())));
    }
    
    /**
     * Get the number of days in the month being processed.
     * @return The number of days in the month.
     **/
    private int getDaysInMonth() {
	Calendar date = Calendar.getInstance();
	date.clear();
	date.set(Calendar.YEAR, year);
	date.set(Calendar.MONTH, month - 1);
	return date.getActualMaximum(Calendar.DAY_OF_MONTH);
    }

    /**
     * Login to the server with the appropriate username and password.
     * @throws FTPException if there is a problem logging into the FTP server.
     * @throws IOException if there is a problem logging into the FTP server.
     **/
    public void login() throws FTPException, IOException {
	ftp.login(USERNAME, PASSWORD);
	ftp.setConnectMode(FTPConnectMode.PASV);
	ftp.setType(FTPTransferType.BINARY);
	ftp.chdir(DATA_DIRECTORY);
    }

    /**
     * Properly close the connection to the FTP server.
     * @throws FTPException if there is a problem closing the connection.
     * @throws IOException if there is a problem closing the connection.
     **/
    public void quit() throws FTPException, IOException {
	ftp.quit();
	System.out.println("Goodbye!");
    }

    /**
     * Download the files for the month.
     * @param args The month and year to download, the directory for the month files,
     * the directory for the ceop files, and optionally a flag (noceop, ceoponly) to
     * specify which data to download.
     * @throws NcepException if there is any problem during the FTPing of the files.
     **/
    public static void main(String[] args) throws NcepException {
	// Parse the command line arguments.
	int month = Integer.parseInt(args[0]);
	int year = Integer.parseInt(args[1]);
	String katzDir = args[2];
	String ceopDir = args[3];

	// Determine which data files to download.
	Boolean dwndCeop = Boolean.TRUE;
	Boolean dwndKatz = Boolean.TRUE;
	try {
	    if (args[4].equals("ceoponly")) { dwndKatz = Boolean.FALSE; }
	    else if (args[4].equals("noceop")) { dwndCeop = Boolean.FALSE; }
	} catch (ArrayIndexOutOfBoundsException e) {}

	NcepFTP dwnd = null;
        boolean failure = false;

	try {
	    // Establish the connection to the server and login.
	    dwnd = new NcepFTP(month, year);
	    dwnd.login();

	    // Download the specified data.
	    if (dwndKatz) {
		try {
		    dwnd.downloadHourlyFiles(katzDir);
		} catch (NcepException e) {
		    System.out.println("\t" + e.getMessage());
                    failure = true;
		}
		
		try {
		    dwnd.downloadDailyFiles(katzDir);
		} catch (NcepException e) {
		    System.out.println("\t" + e.getMessage());
                    failure = true;
		}
		
		try {
		    dwnd.downloadSnapshotFiles(katzDir);
		} catch (NcepException e) {
		    System.out.println("\t" + e.getMessage());
                    failure = true;
		}
		
		try {
		    dwnd.downloadStage4Files(katzDir);
		} catch (NcepException e) {
		    System.out.println("\t" + e.getMessage());
                    failure = true;
		}
		
		try {
		    dwnd.downloadST2_4kmFiles(katzDir);
		} catch (NcepException e) {
		    System.out.println("\t" + e.getMessage());
                    failure = true;
		}
	    }
	    
	    if (dwndCeop) {
		try {
		    dwnd.downloadCeopFiles(ceopDir);
		} catch (NcepException e) {
		    System.out.println("\t" + e.getMessage());
                    failure = true;
		}
	    }
	    
	} catch (FTPException e) {
            e.printStackTrace();
	    failure = true;
	} catch (IOException e) {
            e.printStackTrace();
	    failure = true;
	}

	// Need to check for null for the case where the FTP client
	// could not be created.
	if (dwnd != null) {
	    try { dwnd.quit(); }
            catch (Exception e) {
                e.printStackTrace();
            }
	}

	// Throw an exception if there were any problems during the file transfer.
        if (failure) { throw new NcepException("There were problems downloading the data."); }
    }
}
