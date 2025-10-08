package dmg.ua.sounding.convert.nws;

import static dmg.ua.sounding.esc.ESCSoundingRecord.*;
import static dmg.util.LengthUtils.*;
import static dmg.util.PressureUtils.*;
import static dmg.util.TemperatureUtils.*;
import static dmg.util.TimeUtils.*;
import static dmg.util.VelocityUtils.*;

import java.io.*;
import java.text.*;
import java.util.*;
import java.util.regex.*;
import java.util.zip.*;

import dmg.station.*;
import dmg.ua.sounding.esc.*;
import dmg.ua.sounding.esc.ESCSoundingRecord.*;
import dmg.util.*;

/******************************************************************************
 * <p>The RRSSoundingConverter class is a conversion of the NWS RRS formatted
 * text soundings into the ESC format.  It uses 3 of the 7 files generated
 * from the buffer extraction:  the 1Meta.txt file for the header information; 
 * the 5pPTU file for the pressure, temperature, relative humidity and altitude;
 * and the 6pGPS file for the winds and location of the record.</p>
 * 
 * @author Linda Cully - Feb/Mar 2022
 * @version 1.03 Updated translation of MQ data flags to accept and translate
 * input values of 8 and 13. Per S.Loehrer, the MQ=8 flag indicates a bad value
 * and the MQ=13 flag indicates a good value. The MQ=13 value indicates a data
 * value that has been modified but is good. Search for "HERE" for a description
 * of other changes made to this code. 
 *
 * @author Linda Cully
 * @version 1.02 Added code to set lat/lon on first data record to default of
 * the sounding release lat/lon if 6pGPS "wind" file does not exist. This is 
 * done because occasionally there is no GPS file for a sounding. This ensures 
 * that even without GPS data, the first record indicates start location of 
 * sounding.
 *
 * @author Joel Clawson
 * @version 1.01 Added the RH Sennsor Type to the sounding header.  Programmed
 * in the Code Tables for the header information to display the actual value
 * instead of the code and table identifier.  This also checks to see if the
 * code is in the expected table and that the parameter is coming from the 
 * expected table.  The Code Table information comes from the 
 * <a href="http://dmg.eol.ucar.edu/software/tools/upper_air/NWS_RRS_bufr_extractor/NWS_RRS_reference.doc">
 * NWS_RRS_reference.doc</a>.
 *
 * @author Joel Clawson
 * @version 1.00 The original creation of the conversion.
 ******************************************************************************/
public class RRSSoundingConverter {

    // Constants defining directories where data is to be stored or can be found.
    public static final File FINAL_DIRECTORY = new File("rrs_final");
    public static final File OUTPUT_DIRECTORY = new File("rrs_output");
    public static final File RAW_DIRECTORY = new File("rrs_raw_data");
    
    // Constants defining names of files generated during the conversion.
    public static final File STATION_SUMMARY = new File(OUTPUT_DIRECTORY, "station_summary.log");
    public static final File WARNING_FILE = new File(OUTPUT_DIRECTORY, "warning.log");
	
    // Constants unique to this conversion.
    public static final String NETWORK = "NWS";
    public static final int PLATFORM_ID = 54;
    public static final String PROJECT_ID = "WINTRE-MIX_2022";

    public static final boolean DEBUG = false; // HARDCODED  (true or false) HERE
    public String public_sondeType = "XXX"; // Initialize a Public Radiosonde Type Variable HERE

    
    // Constant to define the source data files to be parsed.
    public static final Pattern FILE_PATTERN = Pattern.compile("1Meta\\.txt(\\.gz)?$");
    
    // Constants containing the code table information for the sounding headers.
    private static final RRSCodeTable sondeTypes = new RRSRadiosondeTypeTable();
    private static final RRSCodeTable balloonMfcrs = new RRSBalloonManufacturerTable();
    private static final RRSCodeTable balloonTypes = new RRSBalloonTypeTable();
    private static final RRSCodeTable rhSensorTypes = new RRSRHSensorTypeTable();

    // List for the source files to be parsed.
    private List<File> metadataFiles;

    // Hold the station information of the soundings being processed.
    private ElevatedStationList stations;
    
    // Output stream for warning messages.
    private PrintWriter log;

    // Flag to determine if the output file names should use nominal instead of actual release times.
    private boolean useNominal;
	
    // Flag to determine if the GPS wind file exists.
    private boolean windFileExists = false;


    /*****************************************************************************
     * Create a new instance of a RRSSoundingConverter.
     * @param useNominal A flag to specify if the filename should use the nominal
     * date instead of the actual date.
     *****************************************************************************/
    public RRSSoundingConverter(boolean useNominal) {
	this.useNominal = useNominal;
	
	// Create the directories (recursively) needed for the conversion.
	if (!FINAL_DIRECTORY.exists()) { FINAL_DIRECTORY.mkdirs(); }
	if (!OUTPUT_DIRECTORY.exists()) { OUTPUT_DIRECTORY.mkdirs(); }
	
	// Instantiate the variables for the conversion.
	metadataFiles = new ArrayList<File>();
	stations = new ElevatedStationList();
    }
    
    /*****************************************************************
     * Get the file name for the specified sounding in the 
     * StationId_ActualDate.cls format.
     * @param sounding The sounding which needs its file name created.
     * @return The file name for the sounding.
     ******************************************************************/
    public String buildFileName(ESCSounding sounding) {
	return String.format("%1$s_%2$tY%2$tm%2$td%2$tH%2$tM%2$tS.cls", sounding.getStationId(), 
			     useNominal ? sounding.getNominalDate() : sounding.getActualDate());
    }
    
    /*******************************************************
     * Print out the station list and station summary files.
     ********************************************************/
    public void buildStationList() {
	// Print out the station CD file.
	try {
	    stations.writeStationCDout(FINAL_DIRECTORY, NETWORK, PROJECT_ID, "sounding");
	} catch (IOException e) {
	    log.printf("Unable to generate the station list.  %s:  %s\n", "IOException", e.getMessage());
	}
	
	// Print out the station summary file.
	try {
	    stations.writeStationSummary(STATION_SUMMARY);
	} catch (IOException e) {
	    log.printf("Unable to generate the station summary.  %s:  %s\n", "IOException", e.getMessage());
	}
    }

    /*****************************************************************
     * Convert the RRS raw soundings into ESC formatted soundings and 
     * generate the associated station list.
     *****************************************************************/
    public void convert() {
	// Define the warning log output stream.
	try {
	    log = new PrintWriter(new FileWriter(WARNING_FILE));
	} catch (IOException e) {
	    e.printStackTrace();
	    System.exit(1);
	}

	// Determine which soundings are to be converted.
	findMetadataFiles();

	// Convert the soundings
	parseSoundings();

	// Create the station files.
	buildStationList();
	
	// Close down the warning log stream.
	log.close();
    }
    
    /****************************************************************
     * Determine the list of soundings to be converted by finding the 
     * source metadata files for the headers.
     *****************************************************************/
    public void findMetadataFiles() {
	// Initialize the list with all of the files in the raw directory.
	List<File> files = new ArrayList<File>(Arrays.asList(RAW_DIRECTORY.listFiles()));
		
	// Continue to loop through all of the files in the list until there
	// are no more files to find.
	while (!files.isEmpty()) {
	    File current = files.remove(0);
	    // The current file is a directory, so read all of it's files and 
	    // add them to the processing list.
	    if (current.isDirectory()) {
		files.addAll(Arrays.asList(current.listFiles()));
	    }
	    // Add files that match the meta data file pattern to the metadata 
	    // file list.
	    else if (FILE_PATTERN.matcher(current.getName()).find()) {
		metadataFiles.add(current);
	    }
	}
    }
    
    /******************************************************
     * Map the source QC flag to its appropriate ESC flag.
     * @param value The data value the flag is assigned to.
     * @param srcFlag The source QC flag to be mapped.
     * @return The ESC flag mapped from the source flag.
     ******************************************************/
    public ESCFlag mapFlag(Double value, int srcFlag) {
	// Don't care what the NWS flag is if the value is missing.
	if (value == null) { return null; }

	// --------------------------------------------------------------
	// Handle source flags that have a value.
	//
	// Feb 2022 - Add in values of 8 and 13 for MQ flags translated
	// and changed how other flags are also translated to match
	// BUFR code tables.
	// --------------------------------------------------------------
	if (srcFlag == 0) { return GOOD_FLAG; }
	else if (srcFlag == 1) { return GOOD_FLAG; }
	else if (srcFlag == 2) { return BAD_FLAG; }
	else if (srcFlag == 3) { return QUESTIONABLE_FLAG; } 
	else if (srcFlag == 4) { return BAD_FLAG; }
	else if (srcFlag == 5) { return GOOD_FLAG; }
	else if (srcFlag == 6) { return QUESTIONABLE_FLAG; } 
	else if (srcFlag == 7) { return BAD_FLAG; }
	else if (srcFlag == 8) { return BAD_FLAG; }
	else if (srcFlag == 13) { return GOOD_FLAG; }

	// -----------------------------------------------------------------
	// Source QC flags 3-6 appear to be missing values in the pure raw
	// data (not the smoothed that are being used) and were replaced in
	// the smoothed data with smoothed over values which is why they are
	// mapped to the estimate flag. 
	// -----------------------------------------------------------------
//	else if (srcFlag == 3 || srcFlag == 4 || srcFlag == 5 || srcFlag == 6) {
//	    return value == null ? null : ESTIMATE_FLAG;
//	}

	else {
	    // -----------------------------------------------------------------
	    // Don't know how to deal with the flag.           HERE
	    // -----------------------------------------------------------------
	    // In Feb 2022, S. Loehrer requested that new flags be added.  
	    // L. Cully added in those new flags and updated the previous flag
	    // translation per S.L.'s request. At that time the following
	    // system exit command was also commented out so that all possible
	    // output would be written to the output Class file. Having the
	    // exit in place, prevented considerable amounts of data that could
	    // be written out to the Class file to not be written. It's better
	    // to see all data for verification purposes, so that line below
	    // has been commented out.
	    // -----------------------------------------------------------------
	    log.printf("mapFlag(): Unknown source flag: %d\n", srcFlag);

	    // System.exit(1);   - Do NOT stop processing! Only issue the warning above! 
	}
	return null;
    }
    
    /***************************************************************
     * Open the specified file to be read.
     * @param file The file to be read.
     * @return The read stream for the file.
     * @throws IOException if there is a problem opening the stream.
     ****************************************************************/
    public BufferedReader open(File file) throws IOException {
	// Open a stream for a gzip file.
	if (file.getName().endsWith(".gz")) {
	    return new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(file))));
	} 
	// Open a stream for an ASCII text file.
	else {
	    return new BufferedReader(new FileReader(file));
	}
    }
    
    /**********************************************************************
     * Parse out the sounding header information from the metadata file.
     * @param file The metadata file for the sounding.
     * @param sounding The ESC formatted sounding for the raw data.
     * @throws IOException if there is a problem reading the metadata file.
     * @throws ParseException if there is a problem parsing out the info
     * from the metadata file.
     ***********************************************************************/
    public void parseMetadataFile(File file, ESCSounding sounding) throws IOException, ParseException {
	BufferedReader reader = open(file);
	
	// Define a container for accumulating exceptions for the metadata
	StringBuffer exceptions = new StringBuffer();
	
	// Initialize the sounding with the basic NWS RRS metadata
	sounding.setDataType("National Weather Service Sounding");
	sounding.setReleaseDirection(ESCSounding.ASCENDING);
	sounding.setProjectId(PROJECT_ID);
	
	// Set the nominal release of the sounding from the file name.
	try {
	    sounding.setNominalRelease(Integer.parseInt(file.getName().substring(6, 10)),
				       Integer.parseInt(file.getName().substring(10, 12)),
				       Integer.parseInt(file.getName().substring(12, 14)),
				       Integer.parseInt(file.getName().substring(14, 16)),
				       0, 0, UTC);
	} catch (DateTimeException e) {
	    exceptions.append("Unable to set the nominal release from a ")
		.append("DateTimeException:  ").append(e.getMessage())
		.append("\n");
	} catch (NumberFormatException e) {
	    exceptions.append("Unable to set the nominal release from a ")
		.append("NumberFormatException:  ").append(e.getMessage())
		.append("\n");
	}
	
	
	
	/*
	 * Define and initialize variables needed to parse out the values when 
	 * they cannot be added to the sounding directly. 
	 */
	String balloonMfcr  = null, balloonType = null, sondeType = null, rhSensorType = null;
	Double sfcPressure  = null, sfcRH       = null, sfcTemp   = null;
	Double sfcWindSpd   = null, sfcWindDir  = null, balloonWeight = null;
	Calendar actualDate = null;

	int dataSignificanceSection = -1, wmoNumber = 0, balloonLotNumber = 0;

	try { 
	    actualDate = buildDate(0, 1, 1, UTC);
	} catch (DateTimeException e) {
	    // This should never occur unless there is a programming error or
	    // if the definitions for the buildDate function change.  So this
	    // should bomb if there is a problem. 
	    e.printStackTrace();
	    System.exit(1);
	}
	
	// Loop through all of the lines in the metadata file.
	String line = null;
	while ((line = reader.readLine()) != null) {
	    // Define the Matcher objects for the different lines being read in
	    // from the metadata file.
	    Matcher altitudeMatcher = Pattern.compile("(\\d+)\\s+HEIGHT\\s*$").matcher(line);
	    Matcher ascensionNumberMatcher = Pattern.compile("(\\d+)\\s+RADIOSONDE ASCENSION NUMBER").matcher(line);
	    Matcher balloonLotNumberMatcher = Pattern.compile("(\\d+)\\s+BALLOON LOT NUMBER").matcher(line);
	    Matcher balloonMfcrMatcher = Pattern.compile("(\\d+)\\s+BALLOON MANUFACTURER,\\s+(Code Table.+)$").matcher(line);
	    Matcher balloonTypeMatcher = Pattern.compile("(\\d+)\\s+BALLOON TYPE,\\s+(Code Table.+)$").matcher(line);
	    Matcher balloonWeightMatcher = Pattern.compile("(\\d+\\.\\d+)\\s+BALLOON WEIGHT").matcher(line);
	    Matcher dataSignificanceMatcher = Pattern.compile("(\\d)\\s+DATA SIGNIFICANCE").matcher(line);
	    Matcher dayMatcher = Pattern.compile("(\\d+)\\s+DAY\\s*$").matcher(line);
	    Matcher hourMatcher = Pattern.compile("(\\d+)\\s+HOUR\\s*$").matcher(line);
	    Matcher latitudeMatcher = Pattern.compile("(\\-?\\d+(\\.\\d+)?)\\s+LATITUDE").matcher(line);
	    Matcher longitudeMatcher = Pattern.compile("(\\-?\\d+(\\.\\d+)?)\\s+LONGITUDE").matcher(line);
	    Matcher minuteMatcher = Pattern.compile("(\\d+)\\s+MINUTE\\s*$").matcher(line);
	    Matcher monthMatcher = Pattern.compile("(\\d+)\\s+MONTH\\s*$").matcher(line);
	    Matcher rhSensorTypeMatcher = Pattern.compile("(\\d+)\\s+HUMIDITY SENSOR TYPE,?\\s+(Code Table.+)$").matcher(line);
	    Matcher secondMatcher = Pattern.compile("(\\d+(\\.\\d+)?)\\s+SECOND\\s*$").matcher(line);
	    Matcher sfcPressMatcher = Pattern.compile("(\\-?\\d+)\\s+PRESSURE\\s*$").matcher(line);
	    Matcher sfcRHMatcher = Pattern.compile("(\\-?\\d+(\\.\\d+)?)\\s+RELATIVE HUMIDITY\\s*$").matcher(line);
	    Matcher sfcTempMatcher = Pattern.compile("(\\-?\\d+(\\.\\d+)?)\\s+TEMPERATURE/DRY BULB").matcher(line);
	    Matcher sfcWindDirMatcher = Pattern.compile("(\\-?\\d+(\\.\\d+)?)\\s+WIND DIRECTION\\s*$").matcher(line);
	    Matcher sfcWindSpdMatcher = Pattern.compile("(\\-?\\d+(\\.\\d+)?)\\s+WIND SPEED\\s*$").matcher(line);
	    Matcher sondeSerialNumberMatcher = Pattern.compile("(\\w+)\\s+RADIOSONDE SERIAL NUMBER").matcher(line);
	    Matcher sondeTypeMatcher = Pattern.compile("(\\d+)\\s+RADIOSONDE TYPE,\\s+(Code Table .+)$").matcher(line);
	    Matcher stationDescriptionMatcher = Pattern.compile("Message Type .+ \\-\\- Metadata, (.+)$").matcher(line);			
	    Matcher stationIdMatcher = Pattern.compile("(\\w+)\\s+SHORT ICAO LOCATION IDENTIFIER").matcher(line);
	    Matcher wmoMatcher = Pattern.compile("(\\d+)\\s+WMO BLOCK AND STATION NUMBER").matcher(line);
	    Matcher yearMatcher = Pattern.compile("(\\d+)\\s+YEAR\\s*$").matcher(line);
	    
	    // Look for metadata lines that are not in a significance section.
	    if (dataSignificanceMatcher.find()) {
		dataSignificanceSection = Integer.parseInt(dataSignificanceMatcher.group(1).trim());
	    }
	    else if (stationDescriptionMatcher.find()) {
		sounding.setStationDescription(stationDescriptionMatcher.group(1).trim());
	    }
	    
	    // This block only cares if the line is in the 1 data significance section
			
	    // Look for the ascension number.
	    else if (dataSignificanceSection == 1 && ascensionNumberMatcher.find()) {
		try {
		    sounding.setHeaderLine(6, "Ascension Number:", 
					   ascensionNumberMatcher.group(1).trim());
		} catch (InvalidValueException e) {
		    exceptions.append("Unable to set the sounding's ascension ")
			.append(" number from an InvalidValueException: ")
			.append(e.getMessage()).append("\n");
		}
	    }
	    // Look for the balloon lot number.
	    else if (dataSignificanceSection == 1 && balloonLotNumberMatcher.find()) {
		try {
		    balloonLotNumber = Integer.parseInt(balloonLotNumberMatcher.group(1));
		} catch (NumberFormatException e) {
		    exceptions.append("Unable to parse the balloon lot number ")
			.append("from ").append(file.getName()).append(".\n");
		}
	    }
	    // Look for the balloon manufacturer 
	    else if (dataSignificanceSection == 1 && balloonMfcrMatcher.find()) {
		if (!balloonMfcrMatcher.group(2).trim().equals("Code Table 9-9")) {
		    exceptions.append("Balloon Manufacturer is not in Code Table 9-9 (")
			.append(balloonMfcrMatcher.group(2).trim()).append(").\n");
		} else {
		    balloonMfcr = balloonMfcrs.get(Integer.parseInt(balloonMfcrMatcher.group(1).trim()));

       //             if (DEBUG) {log.printf("balloonMfcr: %s \n", balloonMfcr);} 
		}
	    }
	    // Look for the balloon type 
	    else if (dataSignificanceSection == 1 && balloonTypeMatcher.find()) {
		if (!balloonTypeMatcher.group(2).trim().equals("Code Table 9-10a")) {
		    exceptions.append("Balloon Type is not in Code Table 9-10a (")
			.append(balloonTypeMatcher.group(2).trim()).append(").\n");
		} else {
		    balloonType = balloonTypes.get(Integer.parseInt(balloonTypeMatcher.group(1).trim()));

      //            if (DEBUG) {log.printf("balloonType: %s \n", balloonType);}

		}
	    }
	    // Look for the balloon weight
	    else if (dataSignificanceSection == 1 && balloonWeightMatcher.find()) {
		try {
		    balloonWeight = Double.parseDouble(balloonWeightMatcher.group(1));
		} catch (NumberFormatException e) {
		    exceptions.append("Unable to parse the balloon weight ")
			.append("from ").append(file.getName()).append(".\n");
		}
	    }
	    // Look for the RH sensor type
	    else if (dataSignificanceSection == 1 && rhSensorTypeMatcher.find()) {
		if (!rhSensorTypeMatcher.group(2).trim().equals("Code Table 9-4")) {
		    exceptions.append("RH Sensor Type is not in Code Table 9-4 (").
			append(rhSensorTypeMatcher.group(2).trim()).append(").\n");
		} else {
		    rhSensorType = rhSensorTypes.get(Integer.parseInt(rhSensorTypeMatcher.group(1).trim()));
		}
	    }
	    // Look for the sonde serial number
	    else if (dataSignificanceSection == 1 && sondeSerialNumberMatcher.find()) {
		try {
		    sounding.setHeaderLine(7, "Radiosonde Serial Number:", 
					   sondeSerialNumberMatcher.group(1).trim());
		} catch (InvalidValueException e) {
		    exceptions.append("Unable to assign the radiosonde serial ")
			.append("number because of an InvalidValueException:  ")
			.append(e.getMessage()).append("\n");
		}
	    }
	    // Look for the sonde type
	    else if (dataSignificanceSection == 1 && sondeTypeMatcher.find()) {
		if (!sondeTypeMatcher.group(2).trim().equals("Code Table 9-1")) {
		    exceptions.append("Radiosonde Type is not in code table 9-1 (").
			append(sondeTypeMatcher.group(2).trim()).append(").\n");
		} else {
		    sondeType = sondeTypes.get(Integer.parseInt(sondeTypeMatcher.group(1).trim()));

                    public_sondeType = sondeType;  // Added HERE
		}
	    }
	    // Look for the station id
	    else if (dataSignificanceSection == 1 && stationIdMatcher.find()) {
		sounding.setStationId(stationIdMatcher.group(1).trim());
	    }
	    // Look for the WMO number
	    else if (dataSignificanceSection == 1 && wmoMatcher.find()) {
		wmoNumber = Integer.parseInt(wmoMatcher.group(1));
	    }
	    
	    
	    // Handle the 3 data significance section
	    
	    // Look for the release altitude
	    else if (dataSignificanceSection == 3 && altitudeMatcher.find()) {
		try {
		    sounding.setAltitude(Double.parseDouble(altitudeMatcher.group(1)), METERS);
		} catch (ConversionException e) {
		    // This should only occur if there is a programming error
		    // or a change in one of the libraries.  So, this should
		    // bomb if this occurs.
		    e.printStackTrace();
		    System.exit(1);
		} catch (NumberFormatException e) {
		    exceptions.append("Unable to assign the altitude because " )
			.append(" of a NumberFormatException:  ")
			.append(e.getMessage()).append("\n");
		}
	    }
	    // Look for the day of the actual release time.
	    else if (dataSignificanceSection == 3 && dayMatcher.find()) {
		try {
		    actualDate.set(Calendar.DAY_OF_MONTH, Integer.parseInt(dayMatcher.group(1)));
		} catch (NumberFormatException e) {
		    exceptions.append("Unable to assign the actual day of the ")
			.append("month for the actual date because of a ")
			.append("NumberFormatException.  ")
			.append(e.getMessage()).append("\n");
		}
	    }
	    // Look for the hour of the actual release time.
	    else if (dataSignificanceSection == 3 && hourMatcher.find()) {
		try {
		    actualDate.set(Calendar.HOUR_OF_DAY, Integer.parseInt(hourMatcher.group(1)));
		} catch (NumberFormatException e) {
		    exceptions.append("Unable to assign the hour for the ")
			.append("actual date because of a ")
			.append("NumberFormatException.  ")
			.append(e.getMessage()).append("\n");
		}
	    }
	    // Look for the release latitude
	    else if (dataSignificanceSection == 3 && latitudeMatcher.find()) {
		try {
		    sounding.setLatitude(Double.parseDouble(latitudeMatcher.group(1)));
		} catch (InvalidValueWarning e) {
		    exceptions.append("Unable to assign the latitude because ")
			.append(" of a InvalidValueWarning:  ")
			.append(e.getMessage()).append("\n");
		} catch (NumberFormatException e) {
		    exceptions.append("Unable to assign the latitude because ")
			.append(" of a NumberFormatException:  ")
			.append(e.getMessage()).append("\n");
		}
	    }
	    // Look for the release longitude
	    else if (dataSignificanceSection == 3 && longitudeMatcher.find()) {
		try {
		    sounding.setLongitude(Double.parseDouble(longitudeMatcher.group(1)));
		} catch (InvalidValueWarning e) {
		    exceptions.append("Unable to assign the longitude because ")
			.append("of a InvalidValueWarning:  ")
			.append(e.getMessage()).append("\n");
		} catch (NumberFormatException e) {
		    exceptions.append("Unable to assign the longitude because ")
			.append("of a NumberFormatException:  ")
			.append(e.getMessage()).append("\n");
		}
	    }
	    // Look for the actual minute of release
	    else if (dataSignificanceSection == 3 && minuteMatcher.find()) {
		try {
		    actualDate.set(Calendar.MINUTE, Integer.parseInt(minuteMatcher.group(1)));
		} catch (NumberFormatException e) {
		    exceptions.append("Unable to assign the minute to the ")
			.append("actual release date because of a")
			.append("NumberFormatException.  ")
			.append(e.getMessage()).append("\n");
		}
	    }
	    // Look for the actual month of the release
	    else if (dataSignificanceSection == 3 && monthMatcher.find()) {
		try {
		    actualDate.set(Calendar.MONTH, Integer.parseInt(monthMatcher.group(1)) - 1);
		} catch (NumberFormatException e) {
		    exceptions.append("Unable to assign the month to the ")
			.append("actual release date because of a ")
			.append("NumberFormatException.  ")
			.append(e.getMessage()).append("\n");
		}
	    }
	    // Look for the actual second of the release
	    else if (dataSignificanceSection == 3 && secondMatcher.find()) {
		try {
		    actualDate.set(Calendar.SECOND, (int)Math.round(Double.parseDouble(secondMatcher.group(1))));
		} catch (NumberFormatException e) {
		    exceptions.append("Unable to assign the seconds to the ")
			.append("actual release date because of a ")
			.append("NumberFormatException.  ")
			.append(e.getMessage()).append("\n");
		}
	    }
	    // Look for the actual year of the release
	    else if (dataSignificanceSection == 3 && yearMatcher.find()) {
		try {
		    actualDate.set(Calendar.YEAR, Integer.parseInt(yearMatcher.group(1)));
		} catch (NumberFormatException e) {
		    exceptions.append("Unable to assign the year to the ")
			.append("actual release date because of a ")
			.append("NumberFormatException.  ")
			.append(e.getMessage()).append("\n");
		}
	    }
	    
	    // Handle the 4 data significance section
	    
	    // Look for the surface pressure
	    else if (dataSignificanceSection == 4 && sfcPressMatcher.find()) {
		try {
		    sfcPressure = Integer.parseInt(sfcPressMatcher.group(1)) / 100.0;
		} catch (NumberFormatException e) {
		    exceptions.append("Unable to parse the surface pressure (")
			.append(sfcPressMatcher.group(1)).append(").  ")
			.append(e.getMessage()).append("\n");
		}
	    }
	    // Look for the surface rh
	    else if (dataSignificanceSection == 4 && sfcRHMatcher.find()) {
		try {
		    sfcRH = Double.parseDouble(sfcRHMatcher.group(1));
		} catch (NumberFormatException e) {
		    exceptions.append("Unable to parse the surface rh (")
			.append(sfcRHMatcher.group(1)).append(").  ")
			.append(e.getMessage()).append("\n");
		}
	    }
	    // Look for the surface temp
	    else if (dataSignificanceSection == 4 && sfcTempMatcher.find()) {
		try {
		    Matcher offsetMatcher = Pattern.compile("0\\s+TIME PERIOD OR DISPLACEMENT").matcher(reader.readLine());
		    if (offsetMatcher.find()) {
			sfcTemp = Double.parseDouble(sfcTempMatcher.group(1));
		    }
		} catch (NumberFormatException e) {
		    exceptions.append("Unable to parse the surface temp (")
			.append(sfcTempMatcher.group(1)).append(").  ")
			.append(e.getMessage()).append("\n");
		}
	    }
	    // Look for the surface wind direction
	    else if (dataSignificanceSection == 4 && sfcWindDirMatcher.find()) {
		try {
		    sfcWindDir = Double.parseDouble(sfcWindDirMatcher.group(1));

                    if (DEBUG) {log.printf("BEFORE: SURFACE ONLY: sondeType: xxx %s xxx, sfcWindDir: %f\n", sondeType, sfcWindDir);}

		} catch (NumberFormatException e) {
		    exceptions.append("Unable to parse the surface wind dir (")
			.append(sfcWindDirMatcher.group(1)).append(").  ")
			.append(e.getMessage()).append("\n");
		}
	    }
         
	    // -------------------------------------------------------------------------------------------
	    // Look for the surface wind speed
	    // -------------------------------------------------------------------------------------------
	    // Note that the surface wind speed is pulled from the Metadata file (*_2022010700_1Meta.txt)
	    // and that in some cases the surface wind speed does NOT match the value found in the first
	    // line of data. This was found in at least one Lockheed Martin Sippican LMS-6 GPS Radiosonde.
	    // -------------------------------------------------------------------------------------------
	    else if (dataSignificanceSection == 4 && sfcWindSpdMatcher.find()) {
		try {

		    sfcWindSpd = Double.parseDouble(sfcWindSpdMatcher.group(1));

                    if (DEBUG) {log.printf("SURFACE ONLY: sondeType: xxx %s xxx, sfcWindSpd: %f\n", sondeType, sfcWindSpd);}

		} catch (NumberFormatException e) {
		    exceptions.append("Unable to parse the SURFACE wind spd (")
			.append(sfcWindSpdMatcher.group(1)).append(").  ")
			.append(e.getMessage()).append("\n");
		}
	    }
	}
	
	// Don't care if there is an exception closing the stream.
	try { reader.close(); }
	catch (IOException e) {}
		
	// Now that everything has been read in, update the sounding with the 
	// data that is on multiple metadata lines.
	sounding.setStationDescription(String.format("%s %s / %d", sounding.getStationId(), 
						     sounding.getStationDescription(), wmoNumber));
	sounding.setActualRelease(actualDate);
		
	// Generate the entire surface observation line.
	StringBuffer sfcObs = new StringBuffer();
	if (sfcPressure != null) {
	    sfcObs.append(String.format(", P: %.1f", sfcPressure));
	}
	if (sfcTemp != null) {
	    sfcObs.append(String.format(", T: %.1f", sfcTemp));
	}
	if (sfcRH != null) {
	    sfcObs.append(String.format(", RH: %.1f", sfcRH));
	}

        if (DEBUG) {log.printf("Before append: sfcWindSpd: %.1f\n", sfcWindSpd);}

	if (sfcWindSpd != null) {  
            if (DEBUG) {log.printf("Set sfcWindSpd on Line: sondeType: %s, sfcWindSpd: %.1f\n", sondeType, sfcWindSpd);} 

	    sfcObs.append(String.format(", WS: %.1f", sfcWindSpd));

            if (DEBUG) {log.printf("AFTER Set sfcWindSpd on Line: sondeType: %s, sfcWindSpd: %.1f\n", sondeType, sfcWindSpd);}
	}

	if (sfcWindDir != null) {
            if (DEBUG) {log.printf("BEFORE DIRECTION Set sfcWindDir on Line: sondeType: %s, sfcWindDir: %.1f\n", sondeType, sfcWindDir);}

	    sfcObs.append(String.format(", WD: %.1f", sfcWindDir));

            if (DEBUG) {log.printf("AFTER DIRECTION: Set sfcWindDir on Line: sondeType: %s, sfcWindDir: %.1f\n", sondeType, sfcWindDir);}
	}

	// Only set the line if there was a surface observation found.
	if (sfcObs.length() > 0) {
	    try {
		sounding.setHeaderLine(11, "Surface Observations:", sfcObs.toString().substring(2).trim());
	    } catch (InvalidValueException e) {
		exceptions.append("Unable to assign the surface observations ")
		    .append("because of an InvalidValueException:  ")
		    .append(e.getMessage()).append("\n");
	    }
	}
	
	// Set the manufacturer/type of the balloon based on what values exist 
	try {
	    if (balloonMfcr != null && balloonType != null) {
		sounding.setHeaderLine(8, "Balloon Manufacturer/Type:", String.format("%s / %s", balloonMfcr, balloonType));
	    } else if (balloonMfcr != null) {
		sounding.setHeaderLine(8, "Balloon Manufacturer:", balloonMfcr);
	    } else if (balloonType != null) {
		sounding.setHeaderLine(8, "Balloon Type:", balloonType);
	    }
	} catch (InvalidValueException e) {
	    exceptions.append("Unable to assign the balloon type/manufacturer ")
		.append("because of an InvalidValueException:  ")
		.append(e.getMessage()).append("\n");			
	}
	
	// Set the lot number/weight of the balloon based on what values exist.
	try {
	    if (balloonLotNumber != 0 && balloonWeight != null) {
		sounding.setHeaderLine(9, "Balloon Lot Number/Weight:",
				       String.format("%d / %.3f", balloonLotNumber, balloonWeight));
	    } else if (balloonLotNumber != 0) {
		sounding.setHeaderLine(9, "Balloon Lot Number:", Integer.toString(balloonLotNumber));
	    } else if (balloonWeight != null) {
		sounding.setHeaderLine(9, "Balloon Weight:", String.format("%.3f", balloonWeight));
	    }
	} catch (InvalidValueException e) {
	    exceptions.append("Unable to assign the baloon lot number/weight ")
		.append("because of an InvalidValueException:  ")
		.append(e.getMessage()).append("\n");
	}

	// Set the sonde type and rh sensor type.
	try {
	    if (sondeType != null && rhSensorType != null) {
		sounding.setHeaderLine(10, "Radiosonde Type/RH Sensor Type:",
				       String.format("%s / %s", sondeType, rhSensorType));
	    } else if (sondeType != null) {
		sounding.setHeaderLine(10, "Radiosonde Type:", sondeType);
	    } else if (rhSensorType != null) {
		sounding.setHeaderLine(10, "RH Sensor Type:", rhSensorType);
	    }
	} catch (InvalidValueException e) {
	    exceptions.append("Unable to assign the radiosonde type ")
		.append("because of an InvalidValueException:  ")
		.append(e.getMessage()).append("\n");
	}
	
	// If any exceptions were thrown, wrap it in a ParseException.
	if (exceptions.length() > 0) {
	    throw new ParseException(exceptions.toString(), -1);
	}
    }
	
    /*********************************************************************
     * Parse the PTU file for the sounding.
     * @param ptuFile The PTU file to be parsed.
     * @param sounding The sounding to put the data in.
     * @throws IOException if there is a problem reading the file.
     * @throws ParseException if there is a problem parsing the raw data.
     *********************************************************************/
    public void parsePTUFile(File ptuFile, ESCSounding sounding) throws IOException, ParseException {
	BufferedReader reader = open(ptuFile);

        if (DEBUG) {log.printf("ENTER parsePTUFile.\n");}
	
	// Setup the pattern to match the lines of the file that contain data.
	String wban = ptuFile.getName().substring(0, 5);
	Pattern linePattern = Pattern.compile("^\\s*\\d{5}\\s+" + wban);
	
	// Setup a container for accumulating exceptions
	StringBuffer exceptions = new StringBuffer();
	
	String line = null;
	int lineCount = 0;
	int recordCount = 0;      // HERE

	while ((line = reader.readLine()) != null) {
	    lineCount++;

            if (DEBUG) {log.printf("Working on PTU line: %d \n", lineCount);}
	    
	    // Look for the data lines.
	    if (linePattern.matcher(line).find()) {
		String[] lineData = line.trim().split("\\s+");

		// Determine the previous record in the sounding
		ESCSoundingRecord previous = sounding.getRecords().isEmpty() ? null : 
		    sounding.getRecords().get(sounding.getRecords().size() - 1);
		
		// Create a new record for the current data line.
//                if (DEBUG) {log.printf("Create new rec for current line \n"); }

		ESCSoundingRecord record = new ESCSoundingRecord(previous);
		sounding.add(record);
		
		// Set the time after release of the record
		try {
//                    if (DEBUG) {log.printf("Set Release Time. \n"); }

		    long releaseDate = buildDate(Integer.parseInt(lineData[4]),
						 Integer.parseInt(lineData[5]),
						 Integer.parseInt(lineData[6]),
						 Integer.parseInt(lineData[7]),
						 Integer.parseInt(lineData[8]),
						 (int)Math.round(Double.parseDouble(lineData[9])),UTC).getTimeInMillis();
		    long recordDate = buildDate(Integer.parseInt(lineData[15]),
						Integer.parseInt(lineData[16]),
						Integer.parseInt(lineData[17]),
						Integer.parseInt(lineData[18]),
						Integer.parseInt(lineData[19]),
						(int)Math.round(Double.parseDouble(lineData[20])),UTC).getTimeInMillis();
		    record.setTime(new Double((recordDate-releaseDate)/1000));

                    // Record has time, so count as data record. 
                    recordCount++;

		} catch (DateTimeException e) {
                    log.printf("Unable to set the time to record. \n");

		    exceptions.append("Unable to set the time to record at ")
			.append("line #").append(lineCount)
			.append(" from a DateTimeException: ")
			.append(e.getMessage()).append("\n");
		} catch (CalculationWarning e) {
		    // This can't occur at this time since the altitude has not been set.
		    e.printStackTrace();
		    System.exit(1);
		} catch (InvalidValueWarning e) {
                    log.printf("Time could not be set. \n");

		    exceptions.append("The time (").append(e.getValue())
			.append(") could not be set.  ")
			.append(e.getMessage()).append("\n");
		}
		
		// Set the pressure value for the record.
		try {
                    if (DEBUG) {log.printf("Try to set Press. Now checking for 999999 also! Added Feb 2022.\n");}

		    Integer press = Integer.parseInt(lineData[26]);
		    if (press != -999999 && press != 999999) {
                    if (DEBUG) {log.printf("GOOD Press! Divide by 100.0. Press = %d \n", press);}

			record.setPressure(press / 100.0, MILLIBARS);
		    }
                    else {
                    if (DEBUG) {log.printf("****BAD Press!  SKIP Press = %d \n", press); }
                    }

		} catch (CalculationWarning e) {
		    // The only calculation involving pressure is sea level
		    // pressure and ESC doesn't use it.
		    e.printStackTrace();

                    log.printf("ERROR: Press Calc Warning \n");
		    System.exit(1);
		} catch (ConversionException e) {
		    // This should only occur on a programming error or if the libraries change.
		    e.printStackTrace();
                    log.printf("ERROR: Press Conversion Exception. Exiting. \n");

		    System.exit(1);
		} catch (InvalidValueWarning e) {
                    log.printf("ERROR: Press values could not be set \n");

		    exceptions.append("The pressure value could not be set ")
			.append("from an illegal value (").append(lineData[26])
			.append(").  ").append(e.getMessage()).append("\n");
		} catch (NumberFormatException e) {
                    log.printf("ERROR: NumberFormat Exception. Unable to parse Press value \n");

		    exceptions.append("Unable to parse the pressure value (")
			.append(lineData[26]).append(") from file ")
			.append(ptuFile.getName()).append("\n");
		}
		
		// Set the pressure flag for the record.
		if (DEBUG) {log.printf("Set Pressure Flag = %s Flag\n", lineData[27]);}

		ESCFlag pressFlag = mapFlag(record.getPressure(), Integer.parseInt(lineData[27]));

		if (DEBUG) {log.printf("After Set Pressure Flag = %s Flag/n", pressFlag);}


		try {
		    record.setPressureFlag(pressFlag);
		} catch (InvalidFlagException e) {
                    log.printf("Failed to Set Pressure Flag \n");
		    System.out.println("Press: " + e.getMessage());
		    System.out.println(record);
		    System.exit(1);
		}
		
		// Set the relative humidity value for the record.
		try {
                    if (DEBUG) {log.printf("Set RH value \n"); }
		    Double rh = Double.parseDouble(lineData[29]);
		    if (rh != -99.0) {
			record.setRelativeHumidity(rh);
		    }
		} catch (CalculationWarning e) {
		    // This shouldn't happen because the temperature hasn't been set at this time.
		    e.printStackTrace();
		    System.exit(1);
		} catch (InvalidValueWarning e) {
		    exceptions.append("The rh value could not be set from an")
			.append(" illegal value (").append(lineData[29])
			.append(").  ").append(e.getMessage()).append("\n");
		} catch (NumberFormatException e) {
		    exceptions.append("Unable to parse the rh value (")
			.append(lineData[29]).append(") from file ")
			.append(ptuFile.getName()).append("\n");
		}
		
		// Set the relative humidity flag for the record.
                if (DEBUG) {log.printf("Set RH Flag;");}

		ESCFlag rhFlag = mapFlag(record.getRelativeHumidity(),
					 Integer.parseInt(lineData[30]));
		try {
		    record.setRelativeHumidityFlag(rhFlag);
		} catch (InvalidFlagException e) {
		    System.out.println("RH: " + e.getMessage());
		    System.out.println(record);
		    System.exit(1);
		}
		
		
		// Set the temperature value for the record.
		try {
                    if (DEBUG) {log.printf("Set Temp value;");}

		    Double temp = Double.parseDouble(lineData[37]);
		    if (temp != -999.0) {
			record.setTemperature(temp, CELCIUS);
		    }
		} catch (CalculationWarning e) {
		    exceptions.append("CalculationException:  ")
			.append(e.getMessage()).append("\n");
		} catch (ConversionException e) {
		    // This should only happen if there is a programming error
		    // or if the libraries change.
		    e.printStackTrace();
		    System.exit(1);
		} catch (InvalidValueWarning e) {
		    exceptions.append("The temperature value could not be set ")
			.append("from an illegal value (")
			.append(lineData[37]).append(").  ")
			.append(e.getMessage()).append("\n");
		} catch (NumberFormatException e) {
		    exceptions.append("Unable to parse the temperature value (")
			.append(lineData[37]).append(") from file ")
			.append(ptuFile.getName()).append("\n");
		}
		
		// Set the temperature flag for the record.
               if (DEBUG) {log.printf("Set Temp flag; ");}

		ESCFlag tempFlag = mapFlag(record.getTemperature(), Integer.parseInt(lineData[38]));

		try {
		    record.setTemperatureFlag(tempFlag);
		} catch (InvalidFlagException e) {
		    System.out.println("Temp: " + e.getMessage());
		    System.out.println(record);
		    System.exit(1);
		}
		
		
                //---------------------------------------------------------
		// Set the altitude value for the record.
                //---------------------------------------------------------
		try {
                    if (DEBUG) {log.printf("Set Alt = %s ; ", lineData[43]);}

		    record.setAltitude(Double.parseDouble(lineData[43]),METERS);

		} catch (CalculationWarning e) {
		    exceptions.append("Calculation Warning:  ")
			.append(e.getMessage()).append("\n");
                    log.printf("Calc warning on ALT; \n");

		} catch (ConversionException e) {
		    // This should only occur if there is a programming error or
		    // if the librarires change.
		    e.printStackTrace();
		    System.exit(1);
		} catch (InvalidValueWarning e) {
                    log.printf("ALT value could not be set;\n");
		    exceptions.append("The altitude value could not be set ")
			.append("from an illegal value (").append(lineData[43])
			.append(").  ").append(e.getMessage()).append("\n");
		} catch (NumberFormatException e) {
                    log.printf("ALT Unable to parse ; \n");
		    exceptions.append("Unable to parse the altitude value (")
			.append(lineData[43]).append(") from file ")
			.append(e.getMessage()).append("\n");
		}

                //---------------------------------------------------------
                // Also preset the Lat/Lon for first record only 
                // so that if the GPS data file is missing, the sounding
                // will at least have the release lat/lon in first record.
                // Only do this if the GPS file is missing! 
                //---------------------------------------------------------
                if (recordCount == 1 && !windFileExists)  {
                   try {
                       if (DEBUG) {log.printf("PreSet lat/lon if wind file exists.\n");}
                       record.setLatitude(sounding.getLatitude()); 
                       log.printf("WARNING: Set the lat for first record (%d) to %f in %s. Flag windFileExists= DNE. \n", recordCount, record.getLatitude(), ptuFile.getName());
                   } catch (InvalidValueWarning e) {
                       exceptions.append("Unable to assign the latitude because ")
                           .append(" of a InvalidValueWarning:  ")
                           .append(e.getMessage()).append("\n");
                   } catch (NumberFormatException e) {
                       exceptions.append("Unable to assign the latitude because ")
                           .append(" of a NumberFormatException:  ")
                           .append(e.getMessage()).append("\n");
                   } // preset lat/lon in case no GPS file

                   try {
                       record.setLongitude(sounding.getLongitude());
                       log.printf("WARNING: Set the lon for first record (%d) to %f in %s\n", recordCount, record.getLongitude(), ptuFile.getName());
                   } catch (InvalidValueWarning e) {
                       exceptions.append("Unable to assign the longitude because ")
                           .append(" of a InvalidValueWarning:  ")
                           .append(e.getMessage()).append("\n");
                   } catch (NumberFormatException e) {
                       exceptions.append("Unable to assign the longitude because ")
                           .append(" of a NumberFormatException:  ")
                           .append(e.getMessage()).append("\n");
                   } // preset lat/lon in case no GPS file
                } // Only set lat/lon for first record

	    }
	}

        if (DEBUG) {log.printf("Close read stream. \n");}
	
	// Close down the open read stream.
	try { reader.close(); }
	catch (IOException e) {}
	
	// Convert any thrown exceptions into a ParseException
	if (exceptions.length() > 0) {
	    throw new ParseException(exceptions.toString(), -1);

	}
     if (DEBUG) {log.printf("Exit parse Press \n"); }
    }
    
    /************************************************************
     * Parse the raw data soundings into ESC formatted soundings.
     ************************************************************/
    public void parseSoundings() {
        Collections.sort(metadataFiles);

        System.out.printf("-------Begin Processing------\n");

        //----------------------------------------
 	// Loop through all of the metadata files.
        //----------------------------------------
	for (File file: metadataFiles) {
	    log.printf("\n-------parseSoundings(): Processing sounding: %s...-----------\n", file.getName());
            System.out.printf("Processing Sounding: %s \n", file); 
	    
            //-----------------------
	    // Create a new sounding.
            //-----------------------
	    ESCSounding sounding = new ESCSounding();
	    
	    boolean passed = true;
	    try {
                //-----------------------------------------------------
		// Parse the header information from the metadata file.
                // If you can not parse the header info, then you can
                // not convert this sounding!
                //-----------------------------------------------------
		try { 
		    parseMetadataFile(file, sounding);
		} catch (ParseException e) {
		    log.printf("ERROR: Unable to successfully parse the metadata file %s.  ParseException:\n%s\n", 
			       file.getName(), e.getMessage());
		    passed = false;
		}
		
                //------------------------------------------------------------
		// Successfully parsed the header info from the metadata file,
                // so Parse the PTU data from the PTU file. Determine if Wind
                // file exists. If not, then need to set the lat/lon on the
                // first records to the header lat/lon. If wind file exists,
                // then location will be set while processing the wind file.
                // The wind file is of the 6pGPS file type.
                //------------------------------------------------------------
		if (passed) {
                    if (DEBUG) {log.printf("Successfully parsed MetadataFile! See if wind file exists.  Then parsePTUFile.\n");}

                    File windfile = new File(file.getParentFile(), file.getName().replace("1Meta", "6pGPS"));
                    windFileExists = windfile.exists();

		    try {
                        if (DEBUG) {log.printf("BEFORE ParsePTUFile. \n"); }
			parsePTUFile(new File(file.getParentFile(), file.getName().replace("1Meta", "5pPTU")),sounding);
                        if (DEBUG) {log.printf("After ParsePTUFile. \n");}

		    } catch (ParseException e) {

                    //------------------------------------------------------------
                    // The original warning on this exception was:
                    // "Catch ParseException: Unable to successfully parse the 
                    // PTU file %s." This did not indicate where in this s/w
                    // the parse exception occurred plus it was followed with
                    // the following statement:  
                    //
                    // passed = false;  //HERE - WHY set to false. It prevents valid data from output
	            //
		    // Setting the "passed" to false prevented valid data from
		    // being put to the output, so that statement has been commented
		    // out below. This was done Feb 2022 L. Cully where the
		    // statement was replaced with the following log print instead
		    // of a system print to the screen while executing. 
                    //------------------------------------------------------------
		    log.printf("WARNING: parseSoundings(): Catch ParseException: Found parse/calculation issues in PTU file %s. All data put to output. ParseException Messages: %s\n", 
			   file.getName().replace("1Meta", "5pPTU"), e.getMessage());

                    if (DEBUG) {log.printf("WARNING: parseSoundings(): NOT Setting passed PTU Processing Flag to FALSE even with ParseException. Put all data to output!\n\n"); }
		    }
		}
	
                //-----------------------------------------------------------------------
                // If successfully parsed header and PTU file, then try to 
		// parse the wind/location data from the GPS file. If don't have GPS file,
                // convert sounding anyway. The GPS file adds wind info, only. 
                // If no GPS data, set the first record's lat/lon to that in the header.
                //-----------------------------------------------------------------------
                if (DEBUG) {log.printf("IMPORTANT: PTU file parsed file passed? passed= %s  If false, skip parseWindFile\n", passed);}
 
		if (passed) {
		    try {
                        File windfile = new File(file.getParentFile(), file.getName().replace("1Meta", "6pGPS"));

                        windFileExists = windfile.exists();
                        if (DEBUG) {log.printf("if windFileExists, parseWindFile() %s \n", file.getName()); }

                        if (windFileExists) {
                            if (DEBUG) {log.printf("wind file exists:: CALLING parseWindFile(): 6pGPS windFile EXISTS for %s. Call parseWindFile to output winds. \n", file.getName()); }

			    parseWindFile(windfile, sounding);
                            }
                        else {
                            // Was println
                            log.printf("WARNING: The 6pGPS wind file does NOT EXIST for %s. Output will NOT contain WINDS.\n", file.getName());
                        }

		    } catch (ParseException e) {
			log.printf("WARNING: Unable to successfully parse the GPS file %s.  ParseException:\n%s\n",
				   file.getName().replace("1Meta", "6pGPS"), e.getMessage());
                     passed = false; 
		    }
		}
	    } catch (IOException e) {
		e.printStackTrace();
		System.exit(1);
	    }
	    
            //--------------------------------
	    // Print the sounding to its file.
            //--------------------------------
	    try {
		File outFile = new File(OUTPUT_DIRECTORY, buildFileName(sounding));

		log.printf("Generating Output File: %s.\n", buildFileName(sounding));

		PrintWriter out = new PrintWriter(new FileWriter(outFile));
		out.println(sounding);
		out.close();
	    } catch (IOException e) {
		log.printf("Unable to successfully print out the sounding for %s.  IOException:  %s\n", 
			   buildFileName(sounding), e.getMessage());
	    }
	    
            //----------------------------------------------------------------
	    // Update the station list with the information from the sounding.
            //----------------------------------------------------------------
	    updateStationList(sounding);
	}
        log.printf("\n-------End parseSoundings()------\n");
        System.out.printf("-------End Processing------\n");

    } // ---- parseSoundings() -----
    


    /*************************************************************************
     * Parse the wind and location data from the GPS file.
     * @param windFile The file to be parsed.
     * @param sounding The sounding to store the data.
     * @throws IOException If there is a problem reading the file.
     * @throws ParseException if there is a problem parsing the data from the
     * file.
     **************************************************************************/
    public void parseWindFile(File windFile, ESCSounding sounding) throws IOException, ParseException {

        if (DEBUG) {log.printf("Enter parseWindFile(): Parse: %s \n", windFile);}

	BufferedReader reader = open(windFile);
	
	// Define the pattern for the lines that contain the data.
	String wban = windFile.getName().substring(0, 5);
	Pattern linePattern = Pattern.compile("^\\s*\\d{5}\\s+" + wban);
	
	// Define the container for accumulating exceptions.
	StringBuffer exceptions = new StringBuffer();
	
	String line = null;
	int lineCount = 0, recordCounter = 0;
	while ((line = reader.readLine()) != null) {
	    lineCount++;

            if (DEBUG) {log.printf("parseWindFile(): Processing Input line #: %d \n", lineCount);}
            if (DEBUG) {log.printf("parseWindFile():    Input line: %s \n", line);}
	    
	    // Search for the data lines.
	    if (linePattern.matcher(line).find()) {
		String[] lineData = line.trim().split("\\s+");
		ESCSoundingRecord record = null;

		// Look for the record at the time after release of the data line.
		try {
		    long releaseDate = buildDate(Integer.parseInt(lineData[4]),
						 Integer.parseInt(lineData[5]),
						 Integer.parseInt(lineData[6]),
						 Integer.parseInt(lineData[7]),
						 Integer.parseInt(lineData[8]),
						 (int)Math.round(Double.parseDouble(lineData[9])),UTC).getTimeInMillis();
		    long recordDate = buildDate(Integer.parseInt(lineData[15]),
						Integer.parseInt(lineData[16]),
						Integer.parseInt(lineData[17]),
						Integer.parseInt(lineData[18]),
						Integer.parseInt(lineData[19]),
						(int)Math.round(Double.parseDouble(lineData[20])),UTC).getTimeInMillis();
		    
		    Double time =  new Double((recordDate-releaseDate) / 1000);
		    
		    // Perform the search of the record.
                    recordCounter = 0;
		    while (record == null && recordCounter < sounding.getRecords().size()) {
			if (time.equals(sounding.getRecords().get(recordCounter).getTime())) {
                            if (sounding.getRecords().get(recordCounter).getUComponent() == null && sounding.getRecords().get(recordCounter).getVComponent() == null) {
			        record = sounding.getRecords().get(recordCounter);
                            } else {
                                log.printf("Record at time %s already has a wind value defined.  Skipping record at %d.\n", time, recordCounter);
                            }
			}
			recordCounter++;
		    }
		} catch (DateTimeException e) {
		    exceptions.append("Unable to find the time of the record ")
			.append("at line #").append(lineCount)
			.append(" from a DateTimeException: ")
			.append(e.getMessage()).append("\n");
		}
		
		// Only add data if a record was found for the time.
                if (DEBUG) { log.printf("  ParseWindFile():: Process record: %s \n", record); }

		if (record != null) {
                    if (DEBUG) {log.printf("    ParseWindFile():: Record NOT null. \n", record); }		

		    // Assign the latitude for the record.
		    try {
			Double lat = Double.parseDouble(lineData[21]);
                        if (DEBUG) {log.printf("    ParseWindFile():: lat: %f \n", lat ); }

			if (lat != -99.0) {
			    record.setLatitude(lat);
			}
		    } catch (InvalidValueWarning e) {
			exceptions.append("The latitude value ")
			    .append(e.getValue()).append(" is not an allowed ")
			    .append("value.  ").append(e.getMessage())
			    .append("\n");
		    } catch (NumberFormatException e) {
			exceptions.append("Unable to parse the latitude value")
			    .append(" (").append(lineData[21])
			    .append(") from file ").append(e.getMessage())
			    .append("\n");
		    }
		    
		    // Assign the longitude for the record.
		    try {
			Double lon = Double.parseDouble(lineData[24]);
                        if (DEBUG) {log.printf("    ParseWindFile():: lon: %f \n", lon ); }

			if (lon != -999.0) {
			    record.setLongitude(lon);
			}
		    } catch (InvalidValueWarning e) {
			exceptions.append("The longitude value ")
			    .append(e.getValue()).append(" is not an allowed ")
			    .append("value.  ").append(e.getMessage())
			    .append("\n");
		    } catch (NumberFormatException e) {
			exceptions.append("Unable to parse the longitude value")
			    .append(" (").append(lineData[24])
			    .append(") from file ").append(windFile.getName())
			    .append("\n");
		    }

	    
		    // Assign the U component for the record.
		    try {
			Double uComp = Double.parseDouble(lineData[30]);

                        if (DEBUG) {log.printf("    parseWindFile(): uComp: %f\n", uComp);} 

                        if ( (public_sondeType.equalsIgnoreCase("Graw DFM-17")) && (uComp != -999.0)) {   
                             record.setUComponent(uComp*0.514444, METERS_PER_SECOND);    // Graw DFM-17 winds are in knots => Convert to m/s!
                        } // Else Ucomp is in m/s so not unit change required
                        else  {
                         record.setUComponent(uComp, METERS_PER_SECOND);
                        }
                        if (DEBUG) {log.printf("    parseWindFile(): IMMEDIATELY AFTER uComp: %f \n", record.getUComponent()); }

		    } catch (CalculationWarning e) {
			// This cannot occurs at this time because the V 
			// component nor the wind speed and direction are set.
			e.printStackTrace();
			System.exit(1);
		    } catch (ConversionException e) {
			// This will only occur if there is a programming error
			// or if the libraries changed.
			e.printStackTrace();
			System.exit(1);
		    } catch (InvalidValueWarning e) {
			exceptions.append("The u component value (")
			    .append(e.getValue()).append(") is not an allowed ")
			    .append(" value.  ").append(e.getMessage())
			    .append("\n");
		    } catch (NumberFormatException e) {
			exceptions.append("Unable to parse the U component ")
			    .append("value (").append(lineData[30])
			    .append(") from file ").append(windFile.getName())
			    .append("\n");
		    }
		    
		    // Assign the U Component flag for the record.
		    ESCFlag uFlag = mapFlag(record.getUComponent(), Integer.parseInt(lineData[31]));
                    if (DEBUG) {log.printf("    parseWindFile(): uComp Flag to print next\n");} 

		    try {
			record.setUComponentFlag(uFlag);
		    } catch (InvalidFlagException e) {
			System.out.println("UComp: " + e.getMessage());
			System.out.println(record);
			System.exit(1);
		    }
		    

		    // Assign the V Component value for the record.
		    try {
			Double vComp = Double.parseDouble(lineData[33]);

                        if (DEBUG) {log.printf("    parseWindFile(): vComp: %f\n", vComp);} 

                        if ( (public_sondeType.equalsIgnoreCase("Graw DFM-17")) && (vComp != -999.0)) {
                             record.setVComponent(vComp*0.514444, METERS_PER_SECOND);    // Graw DFM-17 winds are in knots => Convert to m/s!
                        } // Else Vcomp is in m/s so not unit change required
                        else  {
                         record.setVComponent(vComp, METERS_PER_SECOND);
                        }
                        if (DEBUG) {log.printf("    parseWindFile(): IMMEDIATELY AFTER vComp: %f \n", record.getVComponent()); }

		    } catch (CalculationWarning e) {
			exceptions.append("Calculation Warning:  ")
			    .append(e.getMessage()).append("\n");
		    } catch (ConversionException e) {
			// This will only occur if there is a programming error
			// or if the libraries changed.
			e.printStackTrace();
			System.exit(1);
		    } catch (InvalidValueWarning e) {
			exceptions.append("The V component value (")
			    .append(e.getValue()).append(") is not an allowed ")
			    .append(" value.  ").append(e.getMessage())
			    .append("\n");
		    } catch (NumberFormatException e) {
			exceptions.append("Unable to parse the V component ")
			    .append("value (").append(lineData[30])
			    .append(") from file ").append(windFile.getName())
			    .append("\n");
		    }
		    
		    // Assign the V Component flag for the record.
		    ESCFlag vFlag = mapFlag(record.getVComponent(), Integer.parseInt(lineData[34]));

                    if (DEBUG) {log.printf("    parseWindFile(): vComp Flag to print next. \n");}

		    try {
			record.setVComponentFlag(vFlag);
		    } catch (InvalidFlagException e) {
			System.out.println("VComp: " + e.getMessage());
			System.out.println(record);
			System.exit(1);
		    }

                 //---------------------------------------------------------------   
                 //   For the Graw DFM-17 soundings that began in Jan/Feb
                 //   2022, the wind speed (computed from the U and V components
                 //   resulted in Knots instead of the required m/s for the output
                 //   sounding.  The next section of code was put in place to 
                 //   fix that problem for these particular Graw soundings only. 
                 //   To convert from Knots to m/s, multiply knots by 0.5144.
                 //
                 //  HERE WARNING: When the setWindSpeed()
                 //  function was designed, in that code it nulls out the wind 
                 //  direction!  It says it also attempts to recalc the U/V
                 //  components presumable to make the U/V comps match
                 //  the Wind Speed and Wind Direction. All must be in m/s. 
                 //  Be careful to note that these resets to null/missing Happen
                 //  deep with the libraries/utilities!!!!!
                 //  he setWindSpeed should do exactly that and only that. 
                 //  To see that code, go to link
                 //  http://svn.eol.ucar.edu/websvn/filedetails.php?repname=dmg&path=%2Flibrary%2Fjava%2Futilities%2Ftrunk%2Fsrc%2Fdmg%2Frecord%2FWindData.java
                 //  Search for setWindSpeed and not how the wind dir is reset 
                 //  to null. 
                 //
                 //  WARNING: The same thing will happen with the setWindDirection
                 //  code. It will reset the windSpeed to null! BEWARE!
                 //
                 //---------------------------------------------------------------
                if (DEBUG) {log.printf("    parseWindFile(): After U/V set:: WindSpeed, WindDirection: %f %f \n", record.getWindSpeed(), record.getWindDirection()); }

		} // If Record not null 

	    } // End Search for data line with linePattern.matcher()

	} // End While Loop to read lines from input 6pGPS Wind file
	
	// Close down the open file stream.
	try { reader.close(); }
	catch (IOException e) {}
	
	// Wrap any thrown exceptions into a single ParseException.
	if (exceptions.length() > 0) {
	    throw new ParseException(exceptions.toString(), -1);
	}

    if (DEBUG) {log.printf("Exit parseWindFile()\n");}

    } // parseWindFile()
    
    /*****************************************************************
     * Update the station list with the station information from the
     * specified sounding.
     * @param sounding The sounding to use to update the station list.
     ******************************************************************/
    public void updateStationList(ESCSounding sounding) {
	// Check to see if the station is already in the list.
	if (!stations.contains(sounding.getStationId(), NETWORK, 
			       sounding.getLatitude(), sounding.getLongitude(),
			       sounding.getAltitude())) {
	    
	    // Create a new station.
	    Station station = new Station();
	    
	    try {
		station.setDescription(sounding.getStationDescription());
		station.setElevation(sounding.getAltitude(), METERS);
		station.setFrequency("12 hourly");
		station.setLatitude(sounding.getLatitude());
		station.setLongitude(sounding.getLongitude());
		station.setNetworkName(NETWORK);
		station.setPlatform(PLATFORM_ID);
		
		Matcher stateMatcher = 
		    Pattern.compile(", (..) \\/ \\d{5}").matcher(sounding.getStationDescription());
		stateMatcher.find();
		
		station.setState("US", stateMatcher.group(1)); //Used for US stations - HERE stationList File
//		station.setState("PR", "XX");                  //Used for San Juan, PR only
		station.setStationId(sounding.getStationId());
		
		
		stations.add(station);
	    } catch (ConversionException e) {
		// This should only occur on a programming error or if a change
		// has been made to one of the libraries.
		e.printStackTrace();
		System.exit(1);
	    } catch (InvalidValueException e) {
		// This will occur when a bad value is sent to one of the 
		// functions.
		e.printStackTrace();
		System.exit(1);
	    } catch (InvalidValueWarning e) {
		// This should never occur.  It would happen if a bad value is
		// begin set for a latitude/longitude/elevation.  Since the
		// values are already valid in the sounding, they should be
		// valid here.
		e.printStackTrace();
		System.exit(1);
	    } catch (IOException e) {
		// This should only occur if the state code file has an issue.
		e.printStackTrace();
		System.exit(1);
	    } catch (RestrictedOperationException e) {
		// This will only occur on a change of value.  Since the values
		// of the station should not be changing, this should never occur.
		e.printStackTrace();
		System.exit(1);
	    }
	}
	
	// Get the station from the list and update the begin and end dates
	// with the nominal date of the sounding.
	try {
	    stations.get(sounding.getStationId(), NETWORK, 
			 sounding.getLatitude(), sounding.getLongitude(), 
			 sounding.getAltitude()).insertDate(sounding.getNominalDate());
	} catch (DateTimeException e) {
	    log.printf("Unable to update the date (%1$tY%1$tm%1$td%1$t) for "+
		       "station %2$s:%3$s:%4$.5f:%5$.5f:%6$.1f\n", 
		       sounding.getNominalDate(), NETWORK, sounding.getStationId(),
		       sounding.getLatitude(), sounding.getLongitude(),
		       sounding.getAltitude());
	}
    }
    
    /**************************************************
     * Run the RRS sounding conversion.
     * @param args This does not require any arguments.
     **************************************************/
    public static void main(String[] args) {
	if (args != null && args.length > 0 && args[0] != null && args[0].equals("-Nominal")) {
	    (new RRSSoundingConverter(true)).convert();
	} else {
	    (new RRSSoundingConverter(false)).convert();
	}
    }
}
