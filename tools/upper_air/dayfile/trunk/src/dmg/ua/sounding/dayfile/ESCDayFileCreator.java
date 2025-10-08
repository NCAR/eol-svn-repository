package dmg.ua.sounding.dayfile;

import dmg.ua.sounding.*;
import dmg.ua.sounding.esc.*;
import dmg.util.*;

import java.io.*;
import java.util.*;

/**
 * <p>The ESCDayFileCreator is a class that creates EOL Sounding Composite
 * Format (ESC) day files.  It uses an ESCSoundingParser for reading in the
 * ESCSoundings and uses the natural sort order of the ESCSounding for the
 * sort function.</p>
 * @see dmg.ua.sounding.esc.ESCSoundingParser
 * @see dmg.ua.sounding.esc.ESCSounding#compareTo(Sounding)
 *
 * @author Joel Clawson
 */
public class ESCDayFileCreator extends DayFileCreator<ESCSounding> {

	private boolean gzip;
	private File dataDirectory, outputDirectory;
	private String filePrefix, pattern;
	
	/**
	 * Create a new instance of an ESCDayFileCreator.
	 */
	public ESCDayFileCreator() {
		super(new ESCSoundingParser(false));
		gzip = true;
	}
	
	@Override
	public File getDataDirectory() { return dataDirectory; }

	@Override
	public String getDataFilePattern() { return pattern; }

	@Override
	public File getOutputDirectory() { return outputDirectory; }

	@Override
	public File getOutputFile(Calendar date) {
		return new File(outputDirectory, 
				String.format("%3$s%1$tY%1$tm%1$td.cls%2$s", date, 
						gzip ? ".gz" : "", filePrefix));
	}
	
	/**
	 * Read in the arguments from the command line and setup the variables
	 * needed to create the day files.
	 * @param args The list of command line arguments.
	 * @throws InvalidParameterException if the argument list length is less 
	 * than 2 or more than 3.
	 */
	public void parseArguments(String[] args) throws InvalidParameterException {
		List<String> params = new ArrayList<String>(Arrays.asList(args));
		
		// Search for flags at the start of the list
		while (params.size() > 0 && params.get(0).startsWith("-")) {
			if (params.get(0).equals("-Z")) { gzip = false; }
			else {
				throw new InvalidParameterException("parseArguments","args",
						params.get(0), "Invalid flag of "+params.get(0));
			}
			params.remove(0);
		}
		
		// Make sure the argument list is the expected size.
		if (params.size() < 3 || params.size() > 4) {
			throw new InvalidParameterException("parseArguments","args",
					"Invalid arugment list.  See the usage.");
		}
		
		// Set up the variables
                filePrefix = params.get(0);
		dataDirectory = new File(params.get(1));
		outputDirectory = new File(params.get(2));
		// Set the default pattern if it didn't come through the command line.
		pattern = params.size() == 4 ? params.get(3) : "\\.cls(\\.gz)?$";
	}
	
	/**
	 * Print the usage of the day file creator to standard out.
	 */
	public void printUsage() {
		System.out.println();
		System.out.printf("Usage: java dmg.ua.sounding.dayfile.ESCDayFileCreator <prefix> <dataDirectory> <outputDirectory>\n");
		System.out.printf("Usage: java dmg.ua.sounding.dayfile.ESCDayFileCreator <prefix> <dataDirectory> <outputDirectory> <filePattern>\n");
		System.out.printf("Usage: java dmg.ua.sounding.dayfile.ESCDayFileCreator <flag> <prefix> <dataDirectory> <outputDirectory> <filePattern>\n");
		System.out.println();
		System.out.printf("Flags:\n");
		System.out.printf("\t-Z : Turn off the compression of the final data.\n");
		System.out.println();
		System.out.printf("Example: java dmg.ua.sounding.dayfile.ESCDayFileCreator NWS_ final dayfiles\n");
		System.out.printf("Example: java dmg.ua.sounding.dayfile.ESCDayFileCreator NWS_ final dayfiles \\.cls\\.qc(\\.gz)?\n");
		System.out.printf("Example: java dmg.ua.sounding.dayfile.ESCDayFileCreator -Z NWS_ final dayfiles \\.cls\\.qc(\\.gz)?\n");
		System.out.println();
	}

	/**
	 * Run the ESC sounding day file creator.
	 * @param args The command line arguments sent to the script.
	 * @throws IOException 
	 * @throws InvalidValueWarning 
	 * @throws InvalidValueException 
	 * @throws DateTimeException 
	 * @throws ConversionException 
	 */
	public static void main(String[] args) throws ConversionException,
	DateTimeException, InvalidValueException, InvalidValueWarning, IOException {
		ESCDayFileCreator creator = new ESCDayFileCreator();
		try { 
			creator.parseArguments(args);
		}
		catch (InvalidParameterException e) {
			System.out.println(e.getMessage());
			creator.printUsage();
			System.exit(1);
		}
		
		System.out.printf("Creating the day files from data in %s and " +
				"putting the files into %s using file pattern %s\n",
				creator.getDataDirectory(), creator.getOutputDirectory(), 
				creator.getDataFilePattern());
		
		creator.createDayFiles();
	}
}
