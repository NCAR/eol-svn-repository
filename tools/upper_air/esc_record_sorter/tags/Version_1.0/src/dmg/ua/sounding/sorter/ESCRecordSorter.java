package dmg.ua.sounding.sorter;

import dmg.ua.sounding.*;
import dmg.ua.sounding.esc.*;
import dmg.util.*;
import java.io.*;
import java.util.*;
import java.util.regex.*;
import java.util.zip.*;

/**
 * The ESCRecordSorter is a sounding tool for sorting the records of an ESCSounding.  This
 * tool is able to sort the records by Time and Pressure in either an ascending or descending
 * order.
 * 
 * @author jclawson
 */
public class ESCRecordSorter {
	
	private File INPUT_DIR, LOG_FILE;
	private SoundingRecordComparator<ESCSoundingRecord> COMPARATOR;
	private String PATTERN;

	/**
	 * Create a new instance of an ESCRecordSorter.
	 */
	public ESCRecordSorter() {}
	
    /**
     * Search the input directory recursively for soundings that match the
     * file pattern provided from the command line {default \.cls(\.gz)?}.
     * @return The list of sounding files to be qc'ed.
     */
    public List<File> findInputFiles() {
        List<File> files = new ArrayList<File>();
        
        // Generate the pattern to use to find the sounding files.
        Pattern pattern = Pattern.compile(PATTERN);
        
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
     * Parse the command line arguments for the sorter.
     * @param args The list of arguments passed to the program.
     * @throws InvalidParameterException if there is a problem parsing any of
     * the arguments.
     */
    public void parseArguments(String[] args) throws InvalidParameterException {
        List<String> params = new ArrayList<String>(Arrays.asList(args));
        
        // Search for flags at the start of the list
        while (params.size() > 0 && params.get(0).startsWith("-")) {
            String flag = params.remove(0);
            
            // Look for known flags for sorting by time and pressure.
            if (flag.equals("-T") || flag.equals("-P")) {
            	// Extract the direction that must be defined for sort order.
	            String direction = params.remove(0);
	            // Make sure the direction is a valid value.
	            if (!direction.equals("ASC") && !direction.equals("DESC")) {
	            	throw new InvalidParameterException("parseArguments", "args", String.format("The direction %s for the flag %s is not ASC or DESC.", direction, flag));
	            }
	            // Define the comparison algorithm comparator for the sort.
	            if (flag.equals("-T")) {
	            	COMPARATOR = new ESCSoundingRecordTimeComparator(direction.equals("DESC"));
	            } else if (flag.equals("-P")) {
	            	COMPARATOR = new ESCSoundingRecordPressureComparator(direction.equals("DESC"));
	            }
            }
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
        PATTERN = params.size() == 3 ? params.get(2) : "\\.cls(\\.gz)?$";
    }
    
    /**
     * Print out the usage instructions to the user on how to run this program.
     */
    public void printUsage() {
        System.out.println();
        System.out.printf("Usage: java %s <flag> <dir> <dataDirectory> <logFile>\n", getClass().getName());
        System.out.printf("Usage: java %s <flag> <dir> <dataDirectory> <logFile> <filePattern>\n", getClass().getName());
        //System.out.printf("Usage: java %s <flag> <dataDirectory> <logFile> <filePattern>\n", getClass().getName());
        System.out.println();
        /*
        System.out.printf("Flags:\n");
        System.out.printf("\t-Z : Turn off the compression of the final " +
                "data.\n");
        System.out.println();
        */
        System.out.printf("Example: java %s -T ASC output output/sorter.log\n", getClass().getName());
        System.out.printf("Example: java %s -T DESC output output/sorter.log \\.cls(\\.gz)?\n", getClass().getName());
        System.out.printf("Example: java %s -P DESC output output/sorter.log \\.cls(\\.gz)?\n", getClass().getName());
        System.out.println();
    }
    
    /**
     * Sort the records in the soundings contained within the file.
     * @param parser The parser to use to read the soundings from the file.
     * @param file The file containing the soundings to have the records sorted.
     * @param log An output stream where errors and warnings are to be written.
     * @throws IOException if there is a problem writing to the log stream.
     */
    public void sortFile(ESCSoundingParser parser, File file, PrintWriter log) throws IOException {
        // Write the file being processed to the log.
        log.printf("File:  %s\n", file.getName());
        log.flush();

        // Pull out the soundings in the file.  Any exception thrown by the parsing is the 
        List<ESCSounding> soundings = new ArrayList<ESCSounding>();
        try { soundings.addAll(parser.parseFile(file)); }
        catch (Exception e) {
        	log.printf("An error occurred while parsing the file: %s.  The file is being skipped.\n\t%s\n", file.getName(), e.getMessage());
        }
        
        // Loop through the parsed soundings and sort their records.
        for (ESCSounding sounding: soundings) { sounding.sortRecord(COMPARATOR); }

        // Define the parts of the new file name from the original.
        String parent = file.getParent();
        String filename = file.getName();
        // Rename the original file to be "unsorted".
        file.renameTo(new File(parent, String.format("%s.unsort", filename)));
        
        // Write the sorted soundings to the output file.
        PrintWriter out = new PrintWriter(new FileWriter(new File(parent, filename)));
        for (ESCSounding sounding: soundings) {
        	out.println(sounding.toString());
        }
        out.close();
    }
    
    /**
     * Sort the files defined from the parameters to the program.
     */
    public void sortFiles() {
    	// Acquire the list of files that match the file pattern to be sorted.
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

        // Sort the list of files to process them in an expected order.
        Collections.sort(files);

        // Define the parser to use to read the soundings from the file.  It sets the
        // computation flag to false to prevent any data changes.
        ESCSoundingParser parser = new ESCSoundingParser(false);
        // Loop through the files to be sorted.
        for (File file: files) {
            try {
                sortFile(parser, file, log);
            } catch (IOException e) {
                System.err.printf("Unable to sort file: %s.\n", file.toString());
                System.err.printf("\tIOException: %s\n", e.getMessage());
            }
        }
        
        // Close the open log output stream cleanly.
        log.close();    
    }
    
    /**
     * Run the sorting program.
     * @param args The command line parameters to the program.
     */
    public static void main(String[] args) {
		ESCRecordSorter sorter = new ESCRecordSorter();
		try {
			sorter.parseArguments(args);
		} catch (InvalidParameterException e) {
            System.out.println(e.getMessage());
            sorter.printUsage();
            System.exit(1);
        }
		sorter.sortFiles();
	}
}
