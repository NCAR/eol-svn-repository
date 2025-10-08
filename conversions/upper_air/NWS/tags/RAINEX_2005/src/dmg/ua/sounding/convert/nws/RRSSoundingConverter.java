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

/**
 * <p>The RRSSoundingConverter class is a conversion of the NWS RRS formatted
 * text soundings into the ESC format.  It uses 3 of the 7 files generated
 * from the buffer extraction:  the 1Meta.txt file for the header information; 
 * the 5pPTU file for the pressure, temperature, relative humidity and altitude;
 * and the 6pGPS file for the winds and location of the record.</p>
 * 
 * @author Joel Clawson
 * @version 1.0 The original creation of the conversion.
 */
public class RRSSoundingConverter {

	public static final File FINAL_DIRECTORY = new File("rrs_final");
	public static final File OUTPUT_DIRECTORY = new File("rrs_output");
	public static final File RAW_DIRECTORY = new File("rrs_raw_data");

	public static final File STATION_SUMMARY =
		new File(OUTPUT_DIRECTORY, "station_summary.log");
	public static final File WARNING_FILE =
		new File(OUTPUT_DIRECTORY, "warning.log");
	
	public static final String NETWORK = "NWS";
	public static final int PLATFORM_ID = 54;
	public static final String PROJECT_ID = "RAINEX";
	
	public static final Pattern FILE_PATTERN = Pattern.compile("1Meta\\.txt$");
	
	private List<File> metadataFiles;
	private ElevatedStationList stations;

	private PrintWriter log;
        private boolean useNominal;
	
	/**
	 * Create a new instance of a RRSSoundingConverter.
         * @param useNominal A flag to specify if the filename should use the nominal
         * date instead of the actual date.
	 */
	public RRSSoundingConverter(boolean useNominal) {
                this.useNominal = useNominal;

		// Create the directories (recursively) needed for the conversion.
		if (!FINAL_DIRECTORY.exists()) { FINAL_DIRECTORY.mkdirs(); }
		if (!OUTPUT_DIRECTORY.exists()) { OUTPUT_DIRECTORY.mkdirs(); }
		
		// Instantiate the variables for the conversion.
		metadataFiles = new ArrayList<File>();
		stations = new ElevatedStationList();
	}
	
	/**
	 * Get the file name for the specified sounding in the 
	 * StationId_ActualDate.cls format.
	 * @param sounding The sounding which needs its file name created.
	 * @return The file name for the sounding.
	 */
	public String buildFileName(ESCSounding sounding) {
		return String.format("%1$s_%2$tY%2$tm%2$td%2$tH%2$tM%2$tS.cls",
				sounding.getStationId(), 
				useNominal ? sounding.getNominalDate() : sounding.getActualDate());
	}
	
	/**
	 * Print out the station list and station summary files.
	 */
	public void buildStationList() {
		// Print out the station CD file.
		try {
			stations.writeStationCDout(FINAL_DIRECTORY, NETWORK, PROJECT_ID, 
					"sounding");
		} catch (IOException e) {
			log.printf("Unable to generate the station list.  %s:  %s\n",
					"IOException", e.getMessage());
		}
		
		// Print out the station summary file.
		try {
			stations.writeStationSummary(STATION_SUMMARY);
		} catch (IOException e) {
			log.printf("Unable to generate the station summary.  %s:  %s\n",
					"IOException", e.getMessage());
		}
	}

	/**
	 * Convert the RRS raw soundings into ESC formatted soundings and 
	 * generate the associated station list.
	 */
	public void convert() {
		// Define the warning log output stream.
		try {
			log = new PrintWriter(new FileWriter(WARNING_FILE));
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(1);
		}
		
		// Determine which soundings are to be converted/
		findMetadataFiles();
		// Convert the soundings
		parseSoundings();
		// Create the station files.
		buildStationList();

		// Close down the warning log stream.
		log.close();
	}
	
	/**
	 * Determine the list of soundings to be converted by finding the 
	 * source metadata files for the headers.
	 */
	public void findMetadataFiles() {
		// Initialize the list with all of the files in the raw directory.
		List<File> files = new ArrayList<File>(
				Arrays.asList(RAW_DIRECTORY.listFiles()));
		
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
	
	/**
	 * Map the source QC flag to its appropriate ESC flag.
	 * @param value The data value the flag is assigned to.
	 * @param srcFlag The source QC flag to be mapped.
	 * @return The ESC flag mapped from the source flag.
	 */
	public ESCFlag mapFlag(Double value, int srcFlag) {
		if (srcFlag == 0) { return GOOD_FLAG; }
		else if (srcFlag == 1) { return QUESTIONABLE_FLAG; }
		else if (srcFlag == 2) { return BAD_FLAG; }
		// Source QC flags 3-6 appear to be missing values in the pure raw
		// data (not the smoothed that are being used) and were replaced in
		// the smoothed data with smoothed over values which is why they are
		// mapped to the estimate flag.
		else if (srcFlag == 3 || srcFlag == 4 || srcFlag == 5 || srcFlag == 6) {
			return value == null ? null : ESTIMATE_FLAG;
		}
		else {
			// Don't know how to deal with the flag.	
			System.out.printf("Unknown source flag: %d\n", srcFlag);
			System.exit(1);
		}
		return null;
	}

	/**
	 * Open the specified file to be read.
	 * @param file The file to be read.
	 * @return The read stream for the file.
	 * @throws IOException if there is a problem opening the stream.
	 */
	public BufferedReader open(File file) throws IOException {
		// Open a stream for a gzip file.
		if (file.getName().endsWith(".gz")) {
			return new BufferedReader(new InputStreamReader(
					new GZIPInputStream(new FileInputStream(file))));
		} 
		// Open a stream for an ASCII text file.
		else {
			return new BufferedReader(new FileReader(file));
		}
	}
	
	/**
	 * Parse out the sounding header information from the metadata file.
	 * @param file The metadata file for the sounding.
	 * @param sounding The ESC formatted sounding for the raw data.
	 * @throws IOException if there is a problem reading the metadata file.
	 * @throws ParseException if there is a problem parsing out the info
	 * from the metadata file.
	 */
	public void parseMetadataFile(File file, ESCSounding sounding) throws
		IOException, ParseException {
		BufferedReader reader = open(file);

		// Define a container for accumulating exceptions for the metadata
		StringBuffer exceptions = new StringBuffer();
		
		// Initialize the sounding with the basic NWS RRS metadata
		sounding.setDataType("National Weather Service Sounding");
		sounding.setReleaseDirection(ESCSounding.ASCENDING);
		sounding.setProjectId(PROJECT_ID);
				
		// Set the nominal release of the sounding from the file name.
		try {
			sounding.setNominalRelease(
						Integer.parseInt(file.getName().substring(6, 10)),
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
		String balloonMfcr = null, balloonType = null;
		int dataSignificanceSection = -1, wmoNumber = 0, balloonLotNumber = 0;
		Double sfcPressure = null, sfcRH = null, sfcTemp = null;
		Double sfcWindSpd = null, sfcWindDir = null, balloonWeight = null;
		Calendar actualDate = null;
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
				dataSignificanceSection = 
					Integer.parseInt(dataSignificanceMatcher.group(1).trim());
			}
			else if (stationDescriptionMatcher.find()) {
				sounding.setStationDescription(
						stationDescriptionMatcher.group(1).trim());
			}

			// This block only cares if the line is in the 1 data significance 
			// section
			
			// Look for the ascension number.
			else if (dataSignificanceSection == 1 && 
					ascensionNumberMatcher.find()) {
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
			else if (dataSignificanceSection == 1 && 
					balloonLotNumberMatcher.find()) {
				try {
					balloonLotNumber = 
						Integer.parseInt(balloonLotNumberMatcher.group(1));
				} catch (NumberFormatException e) {
					exceptions.append("Unable to parse the balloon lot number ")
						.append("from ").append(file.getName()).append(".\n");
				}
			}
			// Look for the balloon manufacturer
			else if (dataSignificanceSection == 1 && balloonMfcrMatcher.find()){
				balloonMfcr = String.format("%s (%s)", 
						balloonMfcrMatcher.group(1).trim(), 
						balloonMfcrMatcher.group(2).trim());
			}
			// Look for the balloon type
			else if (dataSignificanceSection == 1 && balloonTypeMatcher.find()){
				balloonType = String.format("%s (%s)", 
						balloonTypeMatcher.group(1).trim(),
						balloonTypeMatcher.group(2).trim());
			}
			// Look for the balloon weight
			else if (dataSignificanceSection == 1 && 
					balloonWeightMatcher.find()) {
				try {
					balloonWeight = 
						Double.parseDouble(balloonWeightMatcher.group(1));
				} catch (NumberFormatException e) {
					exceptions.append("Unable to parse the balloon weight ")
						.append("from ").append(file.getName()).append(".\n");
				}
			}
			// Look for the sonde serial number
			else if (dataSignificanceSection == 1 && 
					sondeSerialNumberMatcher.find()) {
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
				try {
					sounding.setHeaderLine(10, "Radiosonde Type:", 
							String.format("%s (%s)", 
									sondeTypeMatcher.group(1).trim(), 
									sondeTypeMatcher.group(2).trim()));
				} catch (InvalidValueException e) {
					exceptions.append("Unable to assign the radiosonde type ")
					.append("because of an InvalidValueException:  ")
					.append(e.getMessage()).append("\n");
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
					sounding.setAltitude(Double.parseDouble(
							altitudeMatcher.group(1)), METERS);
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
					actualDate.set(Calendar.DAY_OF_MONTH, 
							Integer.parseInt(dayMatcher.group(1)));
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
					actualDate.set(Calendar.HOUR_OF_DAY, 
							Integer.parseInt(hourMatcher.group(1)));
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
					sounding.setLatitude(Double.parseDouble(
							latitudeMatcher.group(1)));
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
					sounding.setLongitude(Double.parseDouble(
							longitudeMatcher.group(1)));
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
					actualDate.set(Calendar.MINUTE, 
							Integer.parseInt(minuteMatcher.group(1)));
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
					actualDate.set(Calendar.MONTH, 
							Integer.parseInt(monthMatcher.group(1)) - 1);
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
					actualDate.set(Calendar.SECOND, 
							(int)Math.round(Double.parseDouble(
									secondMatcher.group(1))));
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
					actualDate.set(Calendar.YEAR, 
							Integer.parseInt(yearMatcher.group(1)));
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
					sfcPressure = 
						Integer.parseInt(sfcPressMatcher.group(1)) / 100.0;
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
					Matcher offsetMatcher = 
						Pattern.compile("0\\s+TIME PERIOD OR DISPLACEMENT").
								matcher(reader.readLine());
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
				} catch (NumberFormatException e) {
					exceptions.append("Unable to parse the surface wind dir (")
						.append(sfcWindDirMatcher.group(1)).append(").  ")
						.append(e.getMessage()).append("\n");
				}
			}
			// Look for the surface wind speed
			else if (dataSignificanceSection == 4 && sfcWindSpdMatcher.find()) {
				try {
					sfcWindSpd = Double.parseDouble(sfcWindSpdMatcher.group(1));
				} catch (NumberFormatException e) {
					exceptions.append("Unable to parse the surface wind spd (")
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
		sounding.setStationDescription(String.format("%s %s / %d", 
				sounding.getStationId(), sounding.getStationDescription(), 
				wmoNumber));
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
		if (sfcWindSpd != null) {
			sfcObs.append(String.format(", WS: %.1f", sfcWindSpd));
		}
		if (sfcWindDir != null) {
			sfcObs.append(String.format(", WD: %.1f", sfcWindDir));
		}
		// Only set the line if there was a surface observation found.
		if (sfcObs.length() > 0) {
			try {
				sounding.setHeaderLine(11, "Surface Observations:", 
						sfcObs.toString().substring(2).trim());
			} catch (InvalidValueException e) {
				exceptions.append("Unable to assign the surface observations ")
					.append("because of an InvalidValueException:  ")
					.append(e.getMessage()).append("\n");
			}
		}

		// Set the manufacturer/type of the balloon based on what values exist
		try {
			if (balloonMfcr != null && balloonType != null) {
				sounding.setHeaderLine(8, "Balloon Manufacturer/Type:", 
						String.format("%s / %s", balloonMfcr, balloonType));
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
						String.format("%d / %.3f", balloonLotNumber, 
								balloonWeight));
			} else if (balloonLotNumber != 0) {
				sounding.setHeaderLine(9, "Balloon Lot Number:",
						Integer.toString(balloonLotNumber));
			} else if (balloonWeight != null) {
				sounding.setHeaderLine(9, "Balloon Weight:",
						String.format("%.3f", balloonWeight));
			}
		} catch (InvalidValueException e) {
			exceptions.append("Unable to assign the baloon lot number/weight ")
				.append("because of an InvalidValueException:  ")
				.append(e.getMessage()).append("\n");
		}

		// If any exceptions were thrown, wrap it in a ParseException.
		if (exceptions.length() > 0) {
			throw new ParseException(exceptions.toString(), -1);
		}
	}
	
	/**
	 * Parse the PTU file for the sounding.
	 * @param ptuFile The PTU file to be parsed.
	 * @param sounding The sounding to put the data in.
	 * @throws IOException if there is a problem reading the file.
	 * @throws ParseException if there is a problem parsing the raw data.
	 */
	public void parsePTUFile(File ptuFile, ESCSounding sounding) throws
	IOException, ParseException {
		BufferedReader reader = open(ptuFile);
		
		// Setup the pattern to match the lines of the file that contain data.
		String wban = ptuFile.getName().substring(0, 5);
		Pattern linePattern = Pattern.compile("^\\s*\\d{5}\\s+" + wban);
		
		// Setup a container for accumulating exceptions
		StringBuffer exceptions = new StringBuffer();
		
		String line = null;
		int lineCount = 0;
		while ((line = reader.readLine()) != null) {
			lineCount++;
			
			// Look for the data lines.
			if (linePattern.matcher(line).find()) {
				String[] lineData = line.trim().split("\\s+");
				// Determine the previous record in the sounding
				ESCSoundingRecord previous = sounding.getRecords().isEmpty() ?
						null : 
					sounding.getRecords().get(sounding.getRecords().size() - 1);
				
				// Create a new record for the current data line.
				ESCSoundingRecord record = new ESCSoundingRecord(previous);
				sounding.add(record);
				
				// Set the time after release of the record
				try {
					long releaseDate = buildDate(Integer.parseInt(lineData[4]),
							Integer.parseInt(lineData[5]),
							Integer.parseInt(lineData[6]),
							Integer.parseInt(lineData[7]),
							Integer.parseInt(lineData[8]),
							(int)Math.round(
									Double.parseDouble(lineData[9])),UTC).
											getTimeInMillis();
					long recordDate = buildDate(Integer.parseInt(lineData[15]),
							Integer.parseInt(lineData[16]),
							Integer.parseInt(lineData[17]),
							Integer.parseInt(lineData[18]),
							Integer.parseInt(lineData[19]),
							(int)Math.round(
									Double.parseDouble(lineData[20])),UTC).
										getTimeInMillis();
					record.setTime(new Double((recordDate-releaseDate)/1000));
				} catch (DateTimeException e) {
					exceptions.append("Unable to set the time to record at ")
						.append("line #").append(lineCount)
						.append(" from a DateTimeException: ")
						.append(e.getMessage()).append("\n");
				} catch (CalculationWarning e) {
					// This can't occur at this time since the altitude has
					// not been set.
					e.printStackTrace();
					System.exit(1);
				} catch (InvalidValueWarning e) {
					exceptions.append("The time (").append(e.getValue())
						.append(") could not be set.  ")
						.append(e.getMessage()).append("\n");
				}
				
				// Set the pressure value for the record.
				try {
					Integer press = Integer.parseInt(lineData[26]);
					if (press != -999999) {
						record.setPressure(press / 100.0, MILLIBARS);
					}
				} catch (CalculationWarning e) {
					// The only calculation involving pressure is sea level
					// pressure and ESC doesn't use it.
					e.printStackTrace();
					System.exit(1);
				} catch (ConversionException e) {
					// This should only occur on a programming error or if
					// the libraries change.
					e.printStackTrace();
					System.exit(1);
				} catch (InvalidValueWarning e) {
					exceptions.append("The pressure value could not be set ")
						.append("from an illegal value (").append(lineData[26])
						.append(").  ").append(e.getMessage()).append("\n");
				} catch (NumberFormatException e) {
					exceptions.append("Unable to parse the pressure value (")
						.append(lineData[26]).append(") from file ")
						.append(ptuFile.getName()).append("\n");
				}
				
				// Set the pressure flag for the record.
				ESCFlag pressFlag = mapFlag(record.getPressure(),
						Integer.parseInt(lineData[27]));
				try {
					record.setPressureFlag(pressFlag);
				} catch (InvalidFlagException e) {
					System.out.println("Press: " + e.getMessage());
					System.out.println(record);
					System.exit(1);
				}
				
				// Set the relative humidity value for the record.
				try {
					Double rh = Double.parseDouble(lineData[29]);
					if (rh != -99.0) {
						record.setRelativeHumidity(rh);
					}
				} catch (CalculationWarning e) {
					// This shouldn't happen because the temperature hasn't been
					// set at this time.
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
				ESCFlag tempFlag = mapFlag(record.getTemperature(),
						Integer.parseInt(lineData[38]));
				try {
					record.setTemperatureFlag(tempFlag);
				} catch (InvalidFlagException e) {
					System.out.println("Temp: " + e.getMessage());
					System.out.println(record);
					System.exit(1);
				}

				
				// Set the altitude value for the record.
				try {
					record.setAltitude(Double.parseDouble(lineData[43]),METERS);
				} catch (CalculationWarning e) {
					exceptions.append("Calculation Warning:  ")
						.append(e.getMessage()).append("\n");
				} catch (ConversionException e) {
					// This should only occur if there is a programming error or
					// if the librarires change.
					e.printStackTrace();
					System.exit(1);
				} catch (InvalidValueWarning e) {
					exceptions.append("The altitude value could not be set ")
						.append("from an illegal value (").append(lineData[43])
						.append(").  ").append(e.getMessage()).append("\n");
				} catch (NumberFormatException e) {
					exceptions.append("Unable to parse the altitude value (")
						.append(lineData[43]).append(") from file ")
						.append(e.getMessage()).append("\n");
				}
			}
		}
		
		// Close down the open read stream.
		try { reader.close(); }
		catch (IOException e) {}

		// Convert any thrown exceptions into a ParseException
		if (exceptions.length() > 0) {
			throw new ParseException(exceptions.toString(), -1);
		}
	}
	
	/**
	 * Parse the raw data soundings into ESC formatted soundings.
	 */
	public void parseSoundings() {
		// Loop through all of the metadata files.
		for (File file: metadataFiles) {
			System.out.printf("Processing sounding: %s...\n", file.getName());

			// Create a new sounding.
			ESCSounding sounding = new ESCSounding();

			boolean passed = true;
			try {
				// Parse the header information from the metadata file.
				try { 
					parseMetadataFile(file, sounding);
				} catch (ParseException e) {
					log.printf("Unable to successfully parse the metadata file "
							+ "%s.  ParseException:\n%s\n", file.getName(), 
							e.getMessage());
					passed = false;
				}
				
				// Parse the PTU data from the PTU file.
				if (passed) {
					try {
						parsePTUFile(new File(file.getParentFile(), 
								file.getName().replace("1Meta", "5pPTU")),
								sounding);
					} catch (ParseException e) {
						log.printf("Unable to successfully parse the PTU file "+
								"%s.  ParseException:\n%s\n", 
								file.getName().replace("1Meta", "5pPTU"),
								e.getMessage());
						passed = false;
					}
				}
				
				// Parse the wind/location data from the GPS file.
				if (passed) {
					try {
						parseWindFile(new File(file.getParentFile(),
								file.getName().replace("1Meta", "6pGPS")),
								sounding);
					} catch (ParseException e) {
						log.printf("Unable to successfully parse the GPS file "+
								"%s.  ParseException:\n%s\n",
								file.getName().replace("1Meta", "6pGPS"),
								e.getMessage());
						passed = false;
					}
				}
			} catch (IOException e) {
				e.printStackTrace();
				System.exit(1);
			}
			
			// Print the sounding to its file.
			try {
				File outFile = new File(OUTPUT_DIRECTORY, 
						buildFileName(sounding));
				PrintWriter out = new PrintWriter(new FileWriter(outFile));
				out.println(sounding);
				out.close();
			} catch (IOException e) {
				log.printf("Unable to successfully print out the sounding "+
						"for %s.  IOException:  %s\n", 
						buildFileName(sounding), e.getMessage());
			}
	
			// Update the station list with the information from the sounding.
			updateStationList(sounding);
		}
	}

	/**
	 * Parse the wind and location data from the GPS file.
	 * @param windFile The file to be parsed.
	 * @param sounding The sounding to store the data.
	 * @throws IOException If there is a problem reading the file.
	 * @throws ParseException if there is a problem parsing the data from the
	 * file.
	 */
	public void parseWindFile(File windFile, ESCSounding sounding) throws
	IOException, ParseException {
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
			
			// Search for the data lines.
			if (linePattern.matcher(line).find()) {
				String[] lineData = line.trim().split("\\s+");
				ESCSoundingRecord record = null;
				
				// Look for the record at the time after release of the data
				// line.
				try {
					long releaseDate = buildDate(Integer.parseInt(lineData[4]),
							Integer.parseInt(lineData[5]),
							Integer.parseInt(lineData[6]),
							Integer.parseInt(lineData[7]),
							Integer.parseInt(lineData[8]),
							(int)Math.round(
									Double.parseDouble(lineData[9])),UTC).
											getTimeInMillis();
					long recordDate = buildDate(Integer.parseInt(lineData[15]),
							Integer.parseInt(lineData[16]),
							Integer.parseInt(lineData[17]),
							Integer.parseInt(lineData[18]),
							Integer.parseInt(lineData[19]),
							(int)Math.round(
									Double.parseDouble(lineData[20])),UTC).
										getTimeInMillis();
					
					Double time =  new Double((recordDate-releaseDate) / 1000);
					
					// Perform the search of the record.
					while (record == null && 
							recordCounter < sounding.getRecords().size()) {
						
						if (time.equals(sounding.getRecords().
								get(recordCounter).getTime())) {
							record = sounding.getRecords().get(recordCounter);
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
				if (record != null) {
					
					// Assign the latitude for the record.
					try {
						Double lat = Double.parseDouble(lineData[21]);
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
						if (uComp != -999.0) {
							record.setUComponent(uComp, METERS_PER_SECOND);
						}
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
					ESCFlag uFlag = mapFlag(record.getUComponent(),
							Integer.parseInt(lineData[31]));
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
						if (vComp != -999.0) {
							record.setVComponent(vComp, METERS_PER_SECOND);
						}
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
					ESCFlag vFlag = mapFlag(record.getVComponent(),
							Integer.parseInt(lineData[34]));
					try {
						record.setVComponentFlag(vFlag);
					} catch (InvalidFlagException e) {
						System.out.println("VComp: " + e.getMessage());
						System.out.println(record);
						System.exit(1);
					}
				}
			}
		}
		
		// Close down the open file stream.
		try { reader.close(); }
		catch (IOException e) {}

		// Wrap any thrown exceptions into a single ParseException.
		if (exceptions.length() > 0) {
			throw new ParseException(exceptions.toString(), -1);
		}
	}
	
	/**
	 * Update the station list with the station information from the
	 * specified sounding.
	 * @param sounding The sounding to use to update the station list.
	 */
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
					Pattern.compile(", (..) \\/ \\d{5}").matcher(
							sounding.getStationDescription());
				stateMatcher.find();
			
				station.setState("US", stateMatcher.group(1));
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
				// of the station should not be changing, this should never
				// occur.
				e.printStackTrace();
				System.exit(1);
			}
		}
		
		// Get the station from the list and update the begin and end dates
		// with the nominal date of the sounding.
		try {
			stations.get(sounding.getStationId(), NETWORK, 
					sounding.getLatitude(), sounding.getLongitude(), 
					sounding.getAltitude()).
					insertDate(sounding.getNominalDate());
		} catch (DateTimeException e) {
			log.printf("Unable to update the date (%1$tY%1$tm%1$td%1$t) for "+
					"station %2$s:%3$s:%4$.5f:%5$.5f:%6$.1f\n", 
					sounding.getNominalDate(), NETWORK, sounding.getStationId(),
					sounding.getLatitude(), sounding.getLongitude(),
					sounding.getAltitude());
		}
	}
	
	/**
	 * Run the RRS sounding conversion.
	 * @param args This does not require any arguments.
	 */
	public static void main(String[] args) {
            if (args != null && args[0] != null && args[0].equals("-Nominal")) {
                (new RRSSoundingConverter(true)).convert();
            } else {
		(new RRSSoundingConverter(false)).convert();
            }
	}
}
