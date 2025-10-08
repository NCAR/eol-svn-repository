package dmg.ua.sounding.dayfile;

import static dmg.util.ComparisonUtils.*;
import static dmg.util.TimeUtils.*;
import dmg.ua.sounding.*;
import dmg.util.*;

import java.io.*;
import java.util.*;
import java.util.regex.*;
import java.util.zip.*;

/**
 * <p>The DayFileCreator is a class that reads in sounding files, parses them 
 * into appropriate Sounding instances, and writes them into day files (using 
 * the nominal release date) sorted by their natural sort order.  It is abstract
 * to allow multiple extensions of the class to handle different formats 
 * properly.</p>
 * <p>Most of the work is done through a SoundingParser.  The parse reads in the
 * data file and converts it into the proper Sounding instance.  The sorting is 
 * done through the natural sort order of the Sounding using 
 * Collections.sort.</p>
 * 
 * @see dmg.ua.sounding.SoundingParser
 * @see java.util.Collections#sort(List)
 * @see dmg.ua.sounding.Sounding#compareTo(Sounding)
 *
 * @author Joel Clawson
 */
public abstract class DayFileCreator<T extends Sounding<?>> {

    private SoundingParser<T> parser;
    
    /**
     * Create a new instance of a DayFileCreator.
     * @param parser The parser to use to read the data.
     */
    public DayFileCreator(SoundingParser<T> parser) {
	this.parser = parser;
    }
    
    /**
     * Parse the data files and reorganize them into day files.
     * @throws ConversionException if there is a problem converting a value between units
     * in a sounding.
     * @throws DateTimeException if there is a problem generating/assigning a date/time
     * to a sounding.
     * @throws InvalidValueException if an invalid value is being assigned to a sounding.
     * @throws InvalidValueWarning if an invalid value is being assigned to a sounding.
     * @throws IOException if there is a problem parsing a sounding.
     */
    public void createDayFiles() 
	throws ConversionException, DateTimeException, InvalidValueException, 
	       InvalidValueWarning, IOException 
    {
	List<SoundingFileAssociator<T>> allSoundings = parseInputFiles();
	Collections.sort(allSoundings);
	generateDayFiles(allSoundings);
    }
    
    /**
     * Find the set of files that are to be used to generate the day files.
     * @return The list of sounding files to use to generate the day files.
     */
    public List<File> findDataFiles() {
	List<File> files = new ArrayList<File>();
	
	// Generate the pattern to use to find the sounding files.
	Pattern pattern = Pattern.compile(getDataFilePattern());
		
	// Create a list of files to check for sounding files.
	List<File> cycle = new ArrayList<File>();
	cycle.add(getDataDirectory());
	
	while (!cycle.isEmpty()) {
	    // Remove the file so the list will eventually be empty.
	    File current = cycle.remove(0);
	    
	    if (current.isDirectory()) {
		// Add all of the directory's files to the cycle list.
		cycle.addAll(Arrays.asList(current.listFiles()));
	    } else if (pattern.matcher(current.getName()).find()) {
		// The file name matched the pattern, so add it to the sounding list.
		files.add(current);
	    }
	}
	
	return files;
    }
    
    /**
     * Generate the day files using the soundings in the specified list.
     * @param sortedSoundings The list of soundings (sorted) used to generate 
     * the day files.
     * @throws DateTimeException if there is a problem creating the date for the dayfile.
     * @throws FileNotFoundException if an input sounding or output file cannot be found.
     * @throws IOException if there is a problem reading an input file or writing to the
     * output file.
     */
    public void generateDayFiles(List<SoundingFileAssociator<T>> sortedSoundings) 
	throws DateTimeException, FileNotFoundException, IOException 
    {
	// Make sure the output directory exists before trying to make the day files.
	if (!getOutputDirectory().exists()) {
	    getOutputDirectory().mkdir();
	}
	
	// Initialize variables that are required for multiple soundings.
	Calendar dayfile = null;
	PrintWriter out = null;
	
	for (SoundingFileAssociator<T> association: sortedSoundings) {
	    T sounding = loadFullSounding(association);
			
	    Calendar current = sounding.getNominalDate();
	    if (compare(dayfile, buildDate(current.get(Calendar.YEAR), 
					   current.get(Calendar.MONTH) + 1, 
					   current.get(Calendar.DAY_OF_MONTH),UTC)) != 0) {
		// Close down the file since the sounding is in a different day. 
		if (out != null) { out.close(); }
		// Create the new day file date (want to make sure there are 
		// no time values for later comparisons)
		dayfile = buildDate(current.get(Calendar.YEAR), 
				    current.get(Calendar.MONTH) + 1, 
				    current.get(Calendar.DAY_OF_MONTH), UTC);
		
		System.out.printf("Creating day file: %1$tY/%1$tm/%1$td\n", dayfile);
		
		// Create the new file output stream.
		File outFile = getOutputFile(dayfile);
		if (outFile.getName().toLowerCase().endsWith(".gz")) {
		    out = new PrintWriter(new OutputStreamWriter(new GZIPOutputStream(new FileOutputStream(outFile))));
		} else {
		    out = new PrintWriter(new FileWriter(outFile));
		}
	    }
	    // Add the sounding to the file now that it is properly set up.
	    out.println(sounding);
	}
	
	// Need to close down the final file if it was created.
	if (out != null) { out.close(); }
    }
    
    /**
     * Get the directory where the data files used to generate the day files are located.
     * @return The data directory of the sounding files.
     */
    public abstract File getDataDirectory();
    
    /**
     * Get the file pattern used to find soundings to include in the day files.
     * @return The sounding file pattern.
     */
    public abstract String getDataFilePattern();
    
    /**
     * Get the directory where the day files are to be stored.
     * @return The day file directory.
     */
    public abstract File getOutputDirectory();
    
    /**
     * Get the full path day file for the specified date.
     * @param date The date the day file is for.
     * @return The file for the specified date.
     */
    public abstract File getOutputFile(Calendar date);
    
    /**
     * Load the full sounding specified in the associator from the source file.
     * @param association The association containing the information about the sounding
     * to be loaded.
     * @return The sounding loaded from the file.
     **/
    public T loadFullSounding(SoundingFileAssociator<T> association) {
	try {
	    List<T> soundings = parser.parseFile(association.getFile());
	    return soundings.get(association.getFileIndex());
	}
	// None of these should ever happen.  This is because the files have
	// already been parsed once to load the headers into the associators.
	// The data files should never change in the middle of the run.
	catch (DefaultException e) {
	    e.printStackTrace();
	    System.exit(1);
	}
	catch (DefaultWarning e) {
	    e.printStackTrace();
	    System.exit(1);
	}
	catch (IOException e) {
	    e.printStackTrace();
	    System.exit(1);
	}
	
	// This should never be reached.
	return null;
    }
    
    /**
     * Parse the sounding files into a list of Sounding instances.
     * @return The list of Soundings found in the data directory.
     * @throws ConversionException if there is a problem converting a value between units.
     * @throws DateTimeException if there is a problem creating or assigning a date
     * to a sounding.
     * @throws InvalidValueException if there is a problem setting a value to a sounding.
     * @throws InvalidValueWarning of there is a problem setting a value to a sounding.
     * @throws IOException if there is a problem reading the soundings from the files.
     * @see dmg.ua.sounding.SoundingParser#parseFile(File)
     */
    public List<SoundingFileAssociator<T>> parseInputFiles() 
	throws ConversionException, DateTimeException, InvalidValueException, 
	       InvalidValueWarning, IOException 
    {
	List<SoundingFileAssociator<T>> associations = 
	    new ArrayList<SoundingFileAssociator<T>>();
	
	for (File file: findDataFiles()) {
	    int index = 0;
	    for (T sounding: parser.parseFile(file)) {
		sounding.getRecords().clear();
		associations.add(new SoundingFileAssociator<T>(sounding, file, index++));
	    }
	}
	return associations;
    }
    
    
    /**
     * The SoundingFileAssociator is a class to hold the minimal amount of sounding
     * information so they can be sorted without having to know the entire sounding.
     **/
    public class SoundingFileAssociator<S extends T> 
	implements Comparable<SoundingFileAssociator<S>> 
    {
	
	private S sounding;
	private File file;
	private int index;
	
	/**
	 * Create a new instance of a SoundingFileAssociator.
	 * @param sounding The sounding to be associated.
	 * @param file The file the sounding is in.
	 * @param index The index of the sounding in the file.
	 **/
	public SoundingFileAssociator(S sounding, File file, int index) {
	    this.sounding = sounding;
	    this.file = file;
	    this.index = index;
	}
		
	/**
	 * Compare this associator to another one for sort order.
	 * @param item The associator to compare to this one.
	 * @return The result of the comparisons between the soundings stored in
	 * the associators.
	 * @see dmg.ua.sounding.Sounding#compareTo(Sounding)
	 **/
	public int compareTo(SoundingFileAssociator<S> item) {
	    return getSounding().compareTo(item.getSounding());
	}
	
	/**
	 * Get the file the sounding is in.
	 * @return The file that contains the sounding.
	 **/
	public File getFile() { return file; }
		
	/**
	 * Get the index of the sounding in the file.
	 * @return The index of the sounding in the file.
	 **/
	public int getFileIndex() { return index; }
	
	/**
	 * Get the sounding being associated with the file.
	 * @return The sounding associated with the file.
	 **/
	public S getSounding() { return sounding; }
    }
}
