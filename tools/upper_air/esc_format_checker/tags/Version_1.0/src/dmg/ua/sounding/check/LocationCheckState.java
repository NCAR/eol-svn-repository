package dmg.ua.sounding.check;

import static dmg.util.PositionUtils.*;

import dmg.util.*;
import java.io.*;
import java.util.regex.*;

/**
 * <p>The LocationCheckState class is a CheckState that checks the ESC header
 * line that contains the release location for the sounding.  The checks make sure 
 * that the header label is the expected label and padded correctly.  It also performs 
 * checks on the latitude and longitude to ensure that they are in the correct 
 * format and match between the degree and degree/minute values.  This also checks 
 * that the line itself is on the expected line of the sounding header.</p>
 * <p>This class is a Concrete State of the <i>State</i> pattern described in
 * <u>Design Patterns</u> by Gamma, Helm, Johnson, and Vlissides.</p>
 * 
 * @author Joel Clawson
 */
public class LocationCheckState extends CheckState {

	/**
	 * Create a new instance of a LocationCheckState.
	 * @param in The input stream that contains the sounding being checked.
	 * @param currentLine The current line being checked by the state.
	 * @param lineNumber The number of the line in the file.
	 */
	public LocationCheckState(BufferedReader in, String currentLine, int lineNumber) {
		super(in, currentLine, lineNumber);
	}

	/**
	 * Check the elevation/altitude value to see if it is a valid number.
	 * @param elevation The String containing the elevation value.
	 * @param lineNumber The line number the elevation is on.
	 * @param log The output stream where errors are to be written.
	 * @return The decimal elevation value.
	 */
	private Double checkElevation(String elevation, int lineNumber, PrintWriter log) {
		try {
			return Double.parseDouble(elevation);
		} catch (NumberFormatException e) {
			log.printf("NumberFormatException:  Unable to convert the elevation value %s to a double on line %d.\n", elevation, lineNumber);
		}
		return null;
	}
	
	/**
	 * Check the latitude values to see if they are properly formatted, valid
	 * latitude values, and that they are the same value between the degree and
	 * degree/minute values.
	 * @param dmLat The String containing the degree/minute latitude value.
	 * @param degLat The String containing the degree latitude value.
	 * @param lineNumber The line number the latitude is on.
	 * @param log The output stream were errors are to be written.
	 * @return The decimal degree latitude value.
	 */
	private Double checkLatitudes(String dmLat, String degLat, int lineNumber, PrintWriter log) {
		Double dmLatDeg = null, degLatDeg = null;
		
		// 5.2.1  Make sure the DM latitude is in the correct format.
		if (!Pattern.matches("^\\d{1,2}\\s\\d{2}\\.\\d{2}\'[NS]$", dmLat)) {
			log.printf("DM Latitude is not in the format DD MM.MM'[NS].  It was %s on line %d.\n", dmLat, lineNumber);
		} else {
			// The value is valid, so parse it and convert to its decimal value.
			String[] parts = dmLat.trim().split("\\s");
			try {
				dmLatDeg = toDegrees(Double.parseDouble(parts[0]), 
				                     Double.parseDouble(parts[1].substring(0,5)), 
				                     0.0, 
				                     parts[1].substring(6,7).equals("N") ? NORTH : SOUTH);
			} catch (ConversionException e) {
				log.println(String.format("ConversionException: %s on line %d.\n", e.getMessage(), lineNumber));
			} catch (InvalidValueWarning e) {
				log.printf("The DM latitude %s is not within the valid range of -90 to 90 on line %d.\n", e.getValue(), lineNumber);
			} catch (NumberFormatException e) {
				log.printf("NumberFormatException: Unable to convert the latitude value %s to a decimal degree on line %d.\n", dmLat, lineNumber);
			}
		}

		try { degLatDeg = Double.parseDouble(degLat); }
		catch (NumberFormatException e) {
			log.printf("NumberFormatException:  Unable to convert the latitude value %s to a double on line %d.\n", degLat, lineNumber);
		}
		
		// 5.2.2  Compare the two longitude values to make sure they have the save value.
		if (dmLatDeg != null && degLatDeg != null) {
			//int dmInt = (int)Math.round(1000.0 * dmLatDeg);
			//int degInt = (int)Math.round(1000.0 * degLatDeg);
                        int dmInt = (int)(Double.parseDouble(String.format("%.3f", dmLatDeg)) * 1000.0);
                        int degInt = (int)(Double.parseDouble(String.format("%.3f", degLatDeg)) * 1000.0);		

			if (dmInt != degInt) {
				log.printf("The two latitude values do not match on line %d.  %s (%s), %s\n", lineNumber, dmLat, dmLatDeg, degLat);
			}
		}
		
		return degLatDeg;
	}
	
	/**
	 * Check the longitude values to see if they are properly formatted, valid
	 * longitude values, and that they are the same value between the degree and
	 * degree/minute values.
	 * @param dmLon The String containing the degree/minute longitude value.
	 * @param degLon The String containing the degree longitude value.
	 * @param lineNumber The line number the longitude is on.
	 * @param log The output stream were errors are to be written.
	 * @return The decimal degree longitude value.
	 */
	private Double checkLongitudes(String dmLon, String degLon, int lineNumber, PrintWriter log) {
		Double dmLonDeg = null, degLonDeg = null;
		
		// 5.1.1  Make sure the DM longitude is in the correct format.
		if (!Pattern.matches("^\\d{1,3}\\s\\d{2}\\.\\d{2}\'[EW]$", dmLon)) {
			log.printf("DM Longitude is not in the format DDD MM.MM'[EW].  It was %s on line %d.\n", dmLon, lineNumber);
		} else {
			// The value is valid, so parse it and convert to its decimal value.
			String[] parts = dmLon.trim().split("\\s");
			try {
				dmLonDeg = toDegrees(Double.parseDouble(parts[0]), 
				                     Double.parseDouble(parts[1].substring(0,5)), 
				                     0.0, 
				                     parts[1].substring(6,7).equals("E") ? EAST : WEST);
			} catch (ConversionException e) {
				log.println(String.format("ConversionException: %s on line %d.\n", e.getMessage(), lineNumber));
			} catch (InvalidValueWarning e) {
				log.printf("The DM longitude value %s is not within the valid range of -180 to 180 on line %d.\n", e.getValue(), lineNumber);
			} catch (NumberFormatException e) {
				log.printf("NumberFormatException: Unable to convert the longitude value %s to a decimal degree on line %d.\n", dmLon, lineNumber);
			}
		}

		try { degLonDeg = Double.parseDouble(degLon); }
		catch (NumberFormatException e) {
			log.printf("NumberFormatException:  Unable to convert the longitude value %s to a double on line %d.\n", degLon, lineNumber);
		}
		
		// 5.1.2  Compare the two longitude values to make sure they have the save value.
		if (dmLonDeg != null && degLonDeg != null) {
			//int dmInt = (int)Math.round(1000.0 * dmLonDeg);
			//int degInt = (int)Math.round(1000.0 * degLonDeg);
			int dmInt = (int)(Double.parseDouble(String.format("%.3f", dmLonDeg)) * 1000.0);
                        int degInt = (int)(Double.parseDouble(String.format("%.3f", degLonDeg)) * 1000.0);


			if (dmInt != degInt) {
				log.printf("The two longitude values do not match on line %d.  %s (%s), %s\n", lineNumber, dmLon, dmLonDeg, degLon);
			}
		}
		
		return degLonDeg;
	}
	
	/**
	 * Perform the checks on the data line.  This ensures the line has the correct
	 * label and has valid text after the label in the correct location.  It also
	 * makes sure the latitude and longitude values are real values and match between
	 * the DM and degree values.
	 * @param line The line containing the label and location being checked.
	 * @param lineNumber The number of the line in the data file.
	 * @param store The store for data information needed beyond the current line for later checks.
	 * @param log The output stream where failures are to be written.
	 */
	@Override public void executeLineCheck(String line, int lineNumber, DataStore store, PrintWriter log) {
		// 1.  Make sure the label contains the expected content.
		if (!line.startsWith("Release Location (lon,lat,alt):    ")) {
			log.printf("The 'Release Location' label is not a 35 char string starting with 'Release Location (lon,lat,alt):' on line %d.\n", lineNumber);
		}

		if (line.length() > 35) {
			// 2.  Make sure the text after the label begins at the correct spot on the line.
			if (line.substring(35).startsWith(" ")) {
				log.printf("The 'Release Location' line does not have any text after the label begin in the correct spot on line %d.\n", lineNumber);
			}
			// 3.  Make sure there isn't any trailing whitespace.
			if (line.substring(35).endsWith(" ")) {
				log.printf("The 'Release Location' header line has extra whitespace at the end of the line on line %d.\n", lineNumber);
			}

			
			String[] sections = line.substring(35).split(",\\s");
			// 4.  Make sure the number of parts in the content after the label is 5.
			if (sections.length != 5) {
				log.printf("The 'Release Location' data does not have the 5 parts for DM Lon, DM Lat, Deg Lon, Deg Lat, Elev '%s' on line %d.\n", line.substring(35), lineNumber);					
			}
			// 5.  The content has the correct number of parts, so perform the checks.
			else {
				// 5.1 Test the longitude values
				store.setHeaderLongitude(checkLongitudes(sections[0], sections[2], lineNumber, log));
				// 5.2 Test the latitude values
				store.setHeaderLatitude(checkLatitudes(sections[1], sections[3], lineNumber, log));
				// 5.3 Test the elevation value
				store.setHeaderAltitude(checkElevation(sections[4], lineNumber, log));
			}
		}
		log.flush();
	}

	/**
	 * Perform the check to ensure that the location header line is on the 14th 
	 * line of the file.
	 * @param lineNumber The number of the line in the file.
	 * @param log The output stream where failures are to be written.
	 */
	@Override public void executeLineNumberCheck(int lineNumber, PrintWriter log) {
		if (lineNumber != 4) {
			log.printf("The 'Release Location' header line is expected on line 4.  It is on line %d.\n", lineNumber);
			log.flush();
		}
	}
}
