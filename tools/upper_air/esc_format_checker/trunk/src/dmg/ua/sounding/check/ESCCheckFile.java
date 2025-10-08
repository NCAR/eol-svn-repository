package dmg.ua.sounding.check;

import java.io.*;
import java.util.*;
import java.util.regex.*;
import java.util.zip.*;

import dmg.util.*;

/**
 * <p>The ESCCheckFile class is a program that tests a set of files to see if they
 * are properly formatted EOL Sounding Composite (ESC) formatted sounding files.  The
 * main function requires in input directory (where the files to be tested are located) and
 * a log file (the file where the failures are to be written).  It may optionally include
 * a file pattern to select the correct files in the input directory (defaults to .cls
 * or .cls.gz).</p>
 * <p>The program searches for specific formats of lines (in any order) and tests each line
 * for a its specific format.  If a line does not match one of the expected lines, it is
 * assumed to be a data line and goes through the data line tests.  The following are the
 * known lines to the program.</p>
 * <ul>
 *    <li>Data Type</li>
 *    <li>Project ID</li>
 *    <li>Release Site</li>
 *    <li>Release Location</li>
 *    <li>UTC Release Time</li>
 *    <li>Nominal Release Time</li>
 *    <li>/</li>
 *    <li>anything with a colon (:)</li>
 *    <li>A line consisting all of hyphens (-) and whitespace</li>
 *    <li>A line that only contains characters in A-z, /, %, and whitespace</li>
 * </ul>
 * <p>Each data line has its own specific checks.  These checks include properly defined
 * header labels with the correct capitalization and padding to 35 characters, header data
 * beginning on the correct index for a header line, no extranseous whitespace, properly
 * formatted dates, expected text, unexpected data values (including negative zeros),  
 * numerical values for numbers, and correctly positioned values and spacing.</p>
 * 
 * @author Joel Clawson
 */
public class ESCCheckFile {

    private boolean isDropsonde;
    private CheckState state;
    private File INPUT_DIR, LOG_FILE;
    private String pattern;
    
    /**
     * Create a new instance of an ESCCheckFile.
     */
    public ESCCheckFile() {}
    
    /**
     * Perform all of the checks on the specified ESC formatted sounding.
     * @param file The file containing the ESC formatted sounding.
     * @param log The output stream where errors are to be logged.
     * @throws IOException when there is a problem reading the sounding or
     * writing to the output stream.
     */
    public void checkFile(File file, PrintWriter log) throws IOException {
	// Write the file being processed to the log.
	log.printf("File:  %s\n", file.getName());
	log.flush();
	
	
	// Open the file into a readable stream.
	BufferedReader in = openToRead(file);
	
	// Create a new DataStore for this sounding.
	DataStore store = new DataStore();
	/* Initialize the state with the initial check state.  Don't want to
	 * assume that the first line is the Data Type line as it should be in
	 * the case where the sounding is not formatted correctly.
	 */
	state = new InitialCheckState(in);
	
	// Continue to check the line of data until it can no longer be checked.
	while (state.canCheckLine()) {
	    state = state.checkLine(store, log);
	}
	// Properly close the input stream.
	in.close();
	
	// Now that all of the data lines have been read and tested, execute
	// the tests that require multiple data lines.
	store.compareHeaderDates(log);
	// Do not compare the header and surface locations in dropsondes.
	if (!isDropsonde()) {
	    store.compareLocations(log);
	}
	store.compareDataTimeSequence(log);
    }
    
	/**
	 * Excecute the ESC formatting checks on all of the files foundi within the
	 * input directory specified from the main arguements.
	 */
	public void checkFiles() {
		// Find the soundings that are to be extracted and loop through them.
		List<File> files = findInputFiles();
		
		PrintWriter log = null;
		try {
			// Create the directories where the log file will be stored.
			if (LOG_FILE.getParentFile() != null) { LOG_FILE.getParentFile().mkdirs(); }
			// Open the log file.
			log = new PrintWriter(new FileWriter(LOG_FILE));
		} catch (IOException e) {
			System.err.printf("Unable to open the log file: %s\n.", LOG_FILE.getName());
			System.exit(1);
		}
	
                Collections.sort(files);	
		for (File file: files) {
			try {
				checkFile(file, log);
			} catch (IOException e) {
				System.err.printf("Unable to check file: %s.\n", file.toString());
				System.err.printf("\tIOException: %s\n", e.getMessage());
			}
		}
		
		log.close();
	}
	
	/**
	 * Search the input directory recursively for soundings that match the
	 * file pattern provided from the command line {default \.cls(\.gz)?}.
	 * @return The list of sounding files to be qc'ed.
	 */
	public List<File> findInputFiles() {
		List<File> files = new ArrayList<File>();
		
		// Generate the pattern to use to find the sounding files.
		Pattern pattern = Pattern.compile(this.pattern);
		
		// Create a list of files to check for sounding files.
		List<File> cycle = new ArrayList<File>();
		cycle.add(INPUT_DIR);
		
		while (!cycle.isEmpty()) {
			// Remove the file so the list will eventually be empty.
			File current = cycle.remove(0);
			
			if (current.isDirectory()) {
				// Add all of the directory's files to the cycle list.
				cycle.addAll(Arrays.asList(current.listFiles()));
			} else if (pattern.matcher(current.getName()).find()) {
				// The file name matched the pattern, so add it to the sounding
				// list.
				files.add(current);
			}
		}
		
		return files;
	}

    /**
     * Determine if the checker is checking dropsonde data.
     * @return <code>true</code> if the data are dropsondes, <code>false</code> if they are not.
     **/
    private boolean isDropsonde() { return isDropsonde; }
    
	/**
	 * Open the specified file as a readable stream.
	 * @param file The file to be read.
	 * @return The readable stream over the file.
	 * @throws IOException if there is a problem opening the stream.
	 */
	public BufferedReader openToRead(File file) throws IOException {
		if (file.getName().toLowerCase().endsWith(".gz")) {
			return new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(file))));
		} else {
			return new BufferedReader(new FileReader(file));
		}
	}
	
	/**
	 * Parse the command line arguments for the qc.
	 * @param args The list of arguments passed to the program.
	 * @throws InvalidParameterException if there is a problem parsing any of
	 * the arguments.
	 */
	public void parseArguments(String[] args) throws InvalidParameterException {
		List<String> params = new ArrayList<String>(Arrays.asList(args));
		
		// Search for flags at the start of the list
		while (params.size() > 0 && params.get(0).startsWith("-")) {
		    String arg = params.remove(0);
		    if (arg.equals("-DROP")) { setIsDropsonde(true); }
		}
		
		// Make sure the argument list is the expected size.
		if (params.size() < 2 || params.size() > 3) {
			throw new InvalidParameterException("parseArguments","args",
					"Invalid arugment list.  See the usage.");
		}

		// Set up the variables
		INPUT_DIR = new File(params.get(0));
		LOG_FILE = new File(params.get(1));
		// Set the default pattern if it didn't come through the command line.
		pattern = params.size() == 3 ? params.get(2) : "\\.cls(\\.gz)?$";
	}
	
	/**
	 * Print out the usage instructions to the user on how to run this program.
	 */
	public void printUsage() {
		System.out.println();
		System.out.printf("Usage: java %s <dataDirectory> <logFile>\n", getClass().getName());
		System.out.printf("Usage: java %s <dataDirectory> <logFile> <filePattern>\n", getClass().getName());
		System.out.printf("Usage: java %s <flag> <dataDirectory> <logFile> <filePattern>\n", getClass().getName());
		System.out.println();
		
		System.out.printf("Flags:\n");
		System.out.printf("\t-DROP : Check dropsonde data.\n");
		System.out.println();
		
		System.out.printf("Example: java %s output output/checkfile.log\n", getClass().getName());
		System.out.printf("Example: java %s output output/checkfile.log \\.cls\\.qc(\\.gz)?\n", getClass().getName());
		//System.out.printf("Example: java %s -Z final dayfiles logs \\.cls\\.qc(\\.gz)?\n", getClass().getName());
		System.out.println();
	}

    /**
     * Set the flag to mark if the data being checked are dropsondes.
     * @param flag <code>true</code> if the data are dropsondes, <code>false</code> if they are not.
     */
    private void setIsDropsonde(boolean flag) { isDropsonde = flag; }

	/**
	 * Execute the ESCCheckFile program.
	 * @param args The list of arguments sent to the program.
	 */
	public static void main(String[] args) {
		ESCCheckFile checker = new ESCCheckFile();
		try { 
			checker.parseArguments(args);
		}
		catch (InvalidParameterException e) {
			System.out.println(e.getMessage());
			checker.printUsage();
			System.exit(1);
		}
		
		checker.checkFiles();
	}
}
