package dmg.ua.sounding.esc;

import static dmg.util.LengthUtils.*;
import static dmg.util.PositionUtils.*;
import static dmg.util.TimeUtils.*;

import java.io.*;
import java.util.*;
import java.util.zip.*;

import dmg.ua.sounding.*;
import dmg.util.*;

/**
 * <p>The ESCSoundingParser class is a SoundingParser for parsing 
 * ESCSoundings.</p>
 * 
 * @author Joel Clawson
 */
public class ESCSoundingParser extends SoundingParser<ESCSounding> {

    private boolean calculationsAllowed;

    /**
     * Create a new instance of an ESCSoundingParser.
     */
    public ESCSoundingParser(boolean calculationsAllowed) {
	this.calculationsAllowed = calculationsAllowed;
    }
    
    /**
     * Parse the specified line for the data type information.
     * @param sounding The sounding to contain the data type information.
     * @param line The line to be parsed.
     */
    private void parseDataTypeLine(ESCSounding sounding, String line) {
	line = line.substring(35);
	int lastSlash = line.lastIndexOf("/");
	if (lastSlash != -1) {
	    sounding.setDataType(line.substring(0, lastSlash).trim());
	    sounding.setReleaseDirection(line.substring(lastSlash + 1).trim());
	} else {
	    sounding.setDataType(line.trim());
	    //sounding.setReleaseDirection(Sounding.UNSPECIFIED);
	}
    }
    
	/**
	 * Parse the specified line for the specified date information.
	 * @param sounding The sounding to contain the date information.
	 * @param line The line to be parsed.
	 * @param dateType A type to specify if the date is the actual or nominal 
	 * release.
	 * @throws DateTimeException if there is a problem setting the date 
	 * in the sounding.
	 * @throws NumberFormatException if an expected numeric value cannot be 
	 * converted to a number.
	 */
	private void parseDateLine(ESCSounding sounding, String line, 
			String dateType) throws DateTimeException, NumberFormatException {
		line = line.substring(35).trim();
		
		if (dateType.equals(ACTUAL_DATE) && 
				!line.substring(0, 22).equals("0000, 00, 00, 00:00:00")) {
			sounding.setActualRelease(Integer.parseInt(line.substring( 0,  4)),
									  Integer.parseInt(line.substring( 6,  8)),
									  Integer.parseInt(line.substring(10, 12)),
									  Integer.parseInt(line.substring(14, 16)),
									  Integer.parseInt(line.substring(17, 19)),
									  Integer.parseInt(line.substring(20, 22)), 
									  UTC);
		} else if (dateType.equals(NOMINAL_DATE) && 
				!line.substring(0, 22).equals("0000, 00, 00, 00:00:00")) {
			sounding.setNominalRelease(Integer.parseInt(line.substring( 0,  4)),
									   Integer.parseInt(line.substring( 6,  8)),
									   Integer.parseInt(line.substring(10, 12)),
									   Integer.parseInt(line.substring(14, 16)),
									   Integer.parseInt(line.substring(17, 19)),
									   Integer.parseInt(line.substring(20, 22)),
									   UTC);
		}
	}
	
	/**
	 * Set one of the variable header lines for the sounding.
	 * @param sounding The sounding to hold the header line.
	 * @param line The line to be parsed.
	 * @param lineIndex The index of the line in the sounding header.
	 * @throws InvalidArgumentException if there is a problem setting the
	 * heading in the sounding.
	 */
	private void parseGeneralHeaderLine(ESCSounding sounding, String line, 
			int lineIndex) throws InvalidValueException {
		if (!line.equals("/")) {
			sounding.setHeaderLine(lineIndex, line.substring(0, 35).trim(), 
					line.substring(35).trim());
		}
	}
	
	/**
	 * Set the location of the sounding from the specified line.
	 * @param sounding The sounding to hold the location information.
	 * @param line The line to be parsed.
	 * @throws NumberFormatException if an expected numeric value cannot be 
	 * converted into a number.
	 * @throws InvalidArgumentException if there is a problem setting the 
	 * latitude, longitude, or altitude for the stounding.
	 * @throws InvalidValueException if there is a problem setting the latitude 
	 * or longitude for the sounding.
	 */
	private void parseLocationLine(ESCSounding sounding, String line) throws 
	ConversionException, InvalidValueException, InvalidValueWarning {
		String[] parts = line.substring(35).split(",");
		sounding.setAltitude(Double.parseDouble(parts[parts.length - 1].trim()),
				METERS);
		
		String[] longitude = parts[0].trim().split("\\s+");
		longitude[0] = longitude[0].trim();
		longitude[1] = longitude[1].trim();
		if (Double.parseDouble(longitude[0]) != 9999.0) {
			sounding.setLongitude(Double.parseDouble(longitude[0]), 
					Double.parseDouble(longitude[1].substring(0, 
							longitude[1].length() - 2)), 
							longitude[1].substring(longitude[1].length() - 1).
							equalsIgnoreCase("W") ? WEST : EAST);
		}

		String[] latitude = parts[1].trim().split("\\s+");
		latitude[0] = latitude[0].trim();
		latitude[1] = latitude[1].trim();
		if (Double.parseDouble(latitude[0]) != 999.0) {
			sounding.setLatitude(Double.parseDouble(latitude[0]), 
					Double.parseDouble(latitude[1].substring(0, 
							latitude[1].length() - 2)), 
							latitude[1].substring(latitude[1].length() - 1).
							equalsIgnoreCase("N") ? NORTH : SOUTH);
		}
	}
	
	/**
	 * Set the variable measurements for the sounding.
	 * @param sounding The sounding to store the information.
	 * @param line The line to be parsed.
	 * @throws InvalidArgumentException if there is a problem setting the 
	 * measurement names in the sounding.
	 */
	private void parseMeasurementLine(ESCSounding sounding, String line) throws 
	InvalidValueException {
		String[] measurements = line.trim().split("\\s+");
		sounding.setVariableMeasurement1(measurements[12]);
		sounding.setVariableMeasurement2(measurements[13]);
	}
	
	/**
	 * Set the project for the sounding.
	 * @param sounding The sounding to store the project id.
	 * @param line The line to be parsed.
	 */
	private void parseProjectLine(ESCSounding sounding, String line) {
		sounding.setProjectId(line.substring(35).trim());
	}
	
	/**
	 * Set the site the sounding was released from.
	 * @param sounding The sounding to store the information.
	 * @param line The line to be parsed.
	 */
	private void parseSiteLine(ESCSounding sounding, String line) {
		sounding.setStationDescription(line.substring(35).trim());
	}
	
    @Override public List<ESCSounding> parseFile(File file) 
	throws ConversionException, DateTimeException, InvalidValueException, 
	       InvalidValueWarning, IOException
    {
		List<ESCSounding> list = new ArrayList<ESCSounding>();
		
		BufferedReader reader = null;
		if (file.getName().toLowerCase().endsWith(".gz")) {
			reader = new BufferedReader(new InputStreamReader(
					new GZIPInputStream(new FileInputStream(file))));
		} else {
			reader = new BufferedReader(new FileReader(file));
		}
		String line = null;
		ESCSounding sounding = null;
		int records = 0;
		while ((line = reader.readLine()) != null) {
			// Start of a new sounding
			if (line.startsWith("Data Type")) {
			    // Parse the entire header.
			    sounding = new ESCSounding();
			    list.add(sounding);
			    parseDataTypeLine(sounding, line);
			    parseProjectLine(sounding, reader.readLine());
			    parseSiteLine(sounding, reader.readLine());
			    parseLocationLine(sounding, reader.readLine());
			    parseDateLine(sounding, reader.readLine(), ACTUAL_DATE);
			    for (int i = 6; i < 12; i++) {
				parseGeneralHeaderLine(sounding, reader.readLine(), i);
			    }
			    parseDateLine(sounding, reader.readLine(), NOMINAL_DATE);
			    parseMeasurementLine(sounding, reader.readLine());
			    parseUnitLine(sounding, reader.readLine());
			    reader.readLine(); // Parse off the hyphen line.
			} else {
			    // Everything else must be data for the previously defined 
			    // sounding.
			    sounding.add(new ESCSoundingRecord(line, calculationsAllowed, ++records));
			}
		}		
		reader.close();
		
		return list;
	}

	/**
	 * Set the variable units for the sounding.
	 * @param sounding The sounding to store the information.
	 * @param line The line to be parsed.
	 * @throws InvalidArgumentException if there is a problem setting the
	 * units in the sounding.
	 */
	private void parseUnitLine(ESCSounding sounding, String line) throws 
	InvalidValueException {
		String[] units = line.trim().split("\\s+");
		sounding.setVariableUnit1(units[12]);
		sounding.setVariableUnit2(units[13]);
	}
}
