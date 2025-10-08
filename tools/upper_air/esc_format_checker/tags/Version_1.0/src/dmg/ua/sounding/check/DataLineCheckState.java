package dmg.ua.sounding.check;

import java.io.*;

/**
 * <p>The DataLineCheckState class is a CheckState that checks an ESC data
 * line that contains the actual measurement values in the sounding.  The checks
 * make sure that each value has the proper format in the correct location on the
 * line while making sure that the spaces are also in the correct spot.  It includes
 * consistancy checks between the data and the flags, making sure that there are
 * values for all of the wind components or that they are all missing, and that
 * the values/missing values for the dew point, temperature, and relative 
 * humidity value match.  This also checks that the line itself is on a line 
 * after the header of the sounding.</p>
 * <p>This class is a Concrete State of the <i>State</i> pattern described in
 * <u>Design Patterns</u> by Gamma, Helm, Johnson, and Vlissides.</p>
 * 
 * @author Joel Clawson
 */
public class DataLineCheckState extends CheckState {

	/**
	 * Create a new instance of a DataLineCheckState.
	 * @param in The input stream that contains the sounding being checked.
	 * @param currentLine The current line being checked by the state.
	 * @param lineNumber The number of the line in the file.
	 */
	public DataLineCheckState(BufferedReader in, String currentLine, int lineNumber) {
		super(in, currentLine, lineNumber);
	}

	/**
	 * Perform the checks on the data line.  This ensures the line has the correct
	 * number of characters and will continue to test for the value being in
	 * the correct positions along with having spaces in the correct indecies. It
	 * also checks for unexpected values and consistancy between data values and
	 * data values with their flags.
	 * @param line The line containing the values being checked.
	 * @param lineNumber The number of the line in the data file.
	 * @param store The store for data information needed beyond the current line for later checks.
	 * @param log The output stream where failures are to be written.
	 */
	@Override public void executeLineCheck(String line, int lineNumber, DataStore store, PrintWriter log) {
		// 1.  Make sure the line is the correct length.
		if (line.length() != 130) {
			log.printf("The 'Data' line is expected to be 130 characters long, but was %d on line %d.\n", line.length(), lineNumber);
		}
		
		else {
			// A data object to hold data values that need to have data line
			// consistancy checks performed.
			RecordStore record = new RecordStore();			
			
			// 2.  Test the time value
			String timeStr = line.substring(0, 6);
			if (timeStr.endsWith(" ") || !timeStr.substring(4, 5).equals(".")) {
				logBadPositioning(log, "Time", timeStr, lineNumber);
			}
			try { 
				Double time = Double.parseDouble(timeStr);
				// Sanity Check - Shouldn't ever happen since the problem will be caught elsewhere
				if (time < -999.9 || time > 9999.9) {
					logIllegalValue(log, "Time", time, -999.9, 9999.9, lineNumber);
				}
				if (time.compareTo(-0.0) == 0) { logNegativeZero(log, "Time", lineNumber); }
				store.addTime(time);
			} catch (NumberFormatException e) {
				logNumberFormatException(log, "Time", timeStr, lineNumber);
			}
			// 3.  Check to see if there is a space between the Time and Pressure measurements
			if (!line.substring(6, 7).equals(" ")) {
				printToLog(log, 6, "Time", "Press", lineNumber);
			}
			// 4.  Test the pressure value
			String pressStr = line.substring(7, 13);
			if (pressStr.endsWith(" ") || !pressStr.substring(4, 5).equals(".")) {
				logBadPositioning(log, "Press", pressStr, lineNumber);
			}
			try { 
				Double press = Double.parseDouble(pressStr);
				// Sanity Check - Shouldn't ever happen since the problem will be caught elsewhere
				if (press < -999.9 || press > 9999.9) {
					logIllegalValue(log, "Press", press, -999.9, 9999.9, lineNumber);
				}
				if (press.compareTo(-0.0) == 0) { logNegativeZero(log, "Press", lineNumber); }
				record.setPressure(press);
			} catch (NumberFormatException e) {
				logNumberFormatException(log, "Press", pressStr, lineNumber);
			}
			// 5.  Check to see if there is a space between the Presure and Temperature measurements.
			if (!line.substring(13, 14).equals(" ")) {
				printToLog(log, 13, "Press", "Temp", lineNumber);
			}
			// 6.  Test the temperature value
			String tempStr = line.substring(14, 19);
			if (tempStr.endsWith(" ") || !tempStr.substring(3, 4).equals(".")) {
				logBadPositioning(log, "Temp", tempStr, lineNumber);
			}
			try { 
				Double temp = Double.parseDouble(tempStr);
				// Sanity Check - Shouldn't ever happen since the problem will be caught elsewhere
				if (temp < -99.9 || temp > 999.9) {
					logIllegalValue(log, "Temp", temp, -99.9, 999.9, lineNumber);
				}
				if (temp.compareTo(-0.0) == 0) { logNegativeZero(log, "Temp", lineNumber); }
				record.setTemperature(temp);
			} catch (NumberFormatException e) {
				logNumberFormatException(log, "Temp", tempStr, lineNumber);
			}
			// 7.  Check to see if there is a space between the temperature and dew point measurements.
			if (!line.substring(19, 20).equals(" ")) {
				printToLog(log, 19, "Temp", "Dewpt", lineNumber);
			}
			// 8.  Test the dew point value
			String dpStr = line.substring(20, 25);
			if (dpStr.endsWith(" ") || !dpStr.substring(3, 4).equals(".")) {
				logBadPositioning(log, "Dewpt", dpStr, lineNumber);
			}
			try { 
				Double dp = Double.parseDouble(dpStr);
				// Sanity Check - Shouldn't ever happen since the problem will be caught elsewhere
				if (dp < -99.9 || dp > 999.9) {
					logIllegalValue(log, "Dewpt", dp, -99.9, 999.9, lineNumber);
				}
				if (dp.compareTo(-0.0) == 0) { logNegativeZero(log, "Dewpt", lineNumber); }
				record.setDewPoint(dp);
			} catch (NumberFormatException e) {
				logNumberFormatException(log, "Dewpt", dpStr, lineNumber);
			}
			// 9.  Check to see if there is a space between the dew point and rh.
			if (!line.substring(25, 26).equals(" ")) {
				printToLog(log, 25, "Dewpt", "RH", lineNumber);
			}
			// 10. Test the relative humidity value
			String rhStr = line.substring(26, 31);
			if (rhStr.endsWith(" ") || !rhStr.substring(3, 4).equals(".")) {
				logBadPositioning(log, "RH", rhStr, lineNumber);
			}
			try { 
				Double rh = Double.parseDouble(rhStr);
				if (rh != 999.0 && (rh < -0.5 || rh > 104.0)) {
					logIllegalValue(log, "RH", rh, -0.5, 104.0, lineNumber);
				}
				if (rh.compareTo(-0.0) == 0) { logNegativeZero(log, "RH", lineNumber); }
				record.setRelativeHumidity(rh);
			} catch (NumberFormatException e) {
				logNumberFormatException(log, "RH", rhStr, lineNumber);
			}
			// 11. Check to see if there is a space between the RH and Ucmp
			if (!line.substring(31, 32).equals(" ")) {
				printToLog(log, 31, "RH", "Ucmp", lineNumber);
			}
			// 12. Test the u component value
			String ucompStr = line.substring(32, 38);
			if (ucompStr.endsWith(" ") || !ucompStr.substring(4, 5).equals(".")) {
				logBadPositioning(log, "Ucmp", ucompStr, lineNumber);
			}
			try { 
				Double ucomp = Double.parseDouble(ucompStr);
				// Sanity Check - Shouldn't ever happen since the problem will be caught elsewhere
				if (ucomp < -999.9 || ucomp > 9999.9) {
					logIllegalValue(log, "Ucmp", ucomp, -999.9, 9999.9, lineNumber);
				}
				if (ucomp.compareTo(-0.0) == 0) { logNegativeZero(log, "Ucmp", lineNumber); }
				record.setUComponent(ucomp);
			} catch (NumberFormatException e) {
				logNumberFormatException(log, "Ucmp", ucompStr, lineNumber);
			}
			// 13. Check to see if there is a space between the Ucmp and Vcmp
			if (!line.substring(38, 39).equals(" ")) {
				printToLog(log, 38, "Ucmp", "Vcmp", lineNumber);
			}
			// 14. Test the v component value
			String vcompStr = line.substring(39, 45);
			if (vcompStr.endsWith(" ") || !vcompStr.substring(4, 5).equals(".")) {
				logBadPositioning(log, "Vcmp", vcompStr, lineNumber);
			}
			try { 
				Double vcomp = Double.parseDouble(vcompStr);
				// Sanity Check - Shouldn't ever happen since the problem will be caught elsewhere
				if (vcomp < -999.9 || vcomp > 9999.9) {
					logIllegalValue(log, "Vcmp", vcomp, -999.9, 9999.9, lineNumber);
				}
				if (vcomp.compareTo(-0.0) == 0) { logNegativeZero(log, "Vcmp", lineNumber); }
				record.setVComponent(vcomp);
			} catch (NumberFormatException e) {
				logNumberFormatException(log, "Vcmp", vcompStr, lineNumber);
			}
			// 15. Check to see if there is a space between the Vcmp and spd
			if (!line.substring(45, 46).equals(" ")) {
				printToLog(log, 45, "Vcmp", "spd", lineNumber);
			}
			// 16. Test the wind speed value
			String spdStr = line.substring(46, 51);
			if (spdStr.endsWith(" ") || !spdStr.substring(3, 4).equals(".")) {
				logBadPositioning(log, "spd", spdStr, lineNumber);
			}
			try { 
				Double spd = Double.parseDouble(spdStr);
				// Sanity Check - Shouldn't ever happen since the problem will be caught elsewhere
				if (spd < 0 || spd > 999.9) {
					logIllegalValue(log, "spd", spd, 0.0, 999.9, lineNumber);
				}
				if (spd.compareTo(-0.0) == 0) { logNegativeZero(log, "spd", lineNumber); }
				record.setWindSpeed(spd);
			} catch (NumberFormatException e) {
				logNumberFormatException(log, "spd", spdStr, lineNumber);
			}
			// 17. Check to see if there is a space between the spd and dir
			if (!line.substring(51, 52).equals(" ")) {
				printToLog(log, 51, "spd", "dir", lineNumber);
			}
			// 18. Test the wind dir value
			String dirStr = line.substring(52, 57);
			if (dirStr.endsWith(" ") || !dirStr.substring(3, 4).equals(".")) {
				logBadPositioning(log, "dir", dirStr, lineNumber);
			}
			try { 
				Double dir = Double.parseDouble(dirStr);
				if (dir != 999.0 && (dir < 0 || dir > 360.0)) {
					logIllegalValue(log, "dir", dir, 0.0, 360.0, lineNumber);
				}
				if (dir.compareTo(-0.0) == 0) { logNegativeZero(log, "dir", lineNumber); }
				record.setWindDirection(dir);
			} catch (NumberFormatException e) {
				logNumberFormatException(log, "dir", dirStr, lineNumber);
			}
			// 19. Check to see if there is a space between dir and Wcmp
			if (!line.substring(57, 58).equals(" ")) {
				printToLog(log, 57, "dir", "Wcmp", lineNumber);
			}
			// 20. Test the ascent rate value
			String rateStr = line.substring(58, 63);
			if (rateStr.endsWith(" ") || !rateStr.substring(3, 4).equals(".")) {
				logBadPositioning(log, "Wcmp", rateStr, lineNumber);
			}
			try { 
				Double rate = Double.parseDouble(rateStr);
				if (rate < -99.9 || rate > 999.9) {
					logIllegalValue(log, "Wcmp", rate, -99.9, 999.9, lineNumber);
				}
				if (rate.compareTo(-0.0) == 0) { logNegativeZero(log, "Wcmp", lineNumber); }
				record.setAscentRate(rate);
			} catch (NumberFormatException e) {
				logNumberFormatException(log, "Wcmp", rateStr, lineNumber);
			}
			// 21. Check to see if there is a space between Wcmp and Lon.
			if (!line.substring(63, 64).equals(" ")) {
				printToLog(log, 63, "Wcmp", "Lon", lineNumber);
			}
			// 22. Test the longitude value
			String lonStr = line.substring(64, 72);
			if (lonStr.endsWith(" ") || !lonStr.substring(4, 5).equals(".")) {
				logBadPositioning(log, "Lon", lonStr, lineNumber);
			}
			try { 
				Double lon = Double.parseDouble(lonStr);
				if (lon != 9999.0 && Math.abs(lon) > 180.0) {
					logIllegalValue(log, "Lon", lon, -180.0, 180.0, lineNumber);
				}
				if (lon.compareTo(-0.0) == 0) { logNegativeZero(log, "Lon", lineNumber); }
				store.setSurfaceLongitude(lon, lineNumber);
			} catch (NumberFormatException e) {
				logNumberFormatException(log, "Lon", lonStr, lineNumber);
			}
			// 23. Check to see if there is a space between Lon and Lat.
			if (!line.substring(72, 73).equals(" ")) {
				printToLog(log, 72, "Lon", "Lat", lineNumber);
			}
			// 24. Test the latitude value
			String latStr = line.substring(73, 80);
			if (latStr.endsWith(" ") || !latStr.substring(3, 4).equals(".")) {
				logBadPositioning(log, "Lat", latStr, lineNumber);
			}
			try { 
				Double lat = Double.parseDouble(latStr);
				if (lat != 999.0 && Math.abs(lat) > 90.0) {
					logIllegalValue(log, "Lat", lat, -90.0, 90.0, lineNumber);
				}
				if (lat.compareTo(-0.0) == 0) { logNegativeZero(log, "Lat", lineNumber); }
				store.setSurfaceLatitude(lat, lineNumber);
			} catch (NumberFormatException e) {
				logNumberFormatException(log, "Lat", latStr, lineNumber);
			}
			// 25. Check to see if there is a space between Lat and Var 1.
			if (!line.substring(80, 81).equals(" ")) {
				printToLog(log, 80, "Lat", "Variable Measurement 1", lineNumber);
			}
			// 26. Test the first variable measurement value
			String var1Str = line.substring(81, 86);
			if (var1Str.endsWith(" ") || !var1Str.substring(3, 4).equals(".")) {
				logBadPositioning(log, "Variable Measurement 1", var1Str, lineNumber);
			}
			try { 
				Double var1 = Double.parseDouble(var1Str);
				// Sanity Check - Shouldn't ever happen since the problem will be caught elsewhere
				if (var1 < -99.9 || var1 > 999.9) {
					logIllegalValue(log, "Variable Measurement 1", var1, -99.9, 999.9, lineNumber);
				}
				if (var1.compareTo(-0.0) == 0) { logNegativeZero(log, "Variable Measurement 1", lineNumber); }
			} catch (NumberFormatException e) {
				logNumberFormatException(log, "Variable Measurement 1", var1Str, lineNumber);
			}
			// 27. Check to see if there is a space between Var 1 and Var 2.
			if (!line.substring(86, 87).equals(" ")) {
				printToLog(log, 86, "Variable Measurement 1", "Variable Measurement 2", lineNumber);
			}
			// 28. Test the second variable measurement value
			String var2Str = line.substring(87, 92);
			if (var2Str.endsWith(" ") || !var2Str.substring(3, 4).equals(".")) {
				logBadPositioning(log, "Variable Measurement 2", var2Str, lineNumber);
			}
			try { 
				Double var2 = Double.parseDouble(var2Str);
				// Sanity Check - Shouldn't ever happen since the problem will be caught elsewhere
				if (var2 < -99.9 || var2 > 999.9) {
					logIllegalValue(log, "Variable Measurement 2", var2, -99.9, 999.9, lineNumber);
				}
				if (var2.compareTo(-0.0) == 0) { logNegativeZero(log, "Variable Measurement 2", lineNumber); }
			} catch (NumberFormatException e) {
				logNumberFormatException(log, "Variable Measurement 2", var2Str, lineNumber);
			}
			// 29. Check to see if there is a space between Var 2 and Alt
			if (!line.substring(92, 93).equals(" ")) {
				printToLog(log, 92, "Variable Measurement 2", "Alt", lineNumber);
			}
			// 30. Test the altitude measurement value
			String altStr = line.substring(93, 100);
			if (altStr.endsWith(" ") || !altStr.substring(5, 6).equals(".")) {
				logBadPositioning(log, "Alt", altStr, lineNumber);
			}
			try { 
				Double alt = Double.parseDouble(altStr);
				// Sanity Check - Shouldn't ever happen since the problem will be caught elsewhere
				if (alt < -9999.9 || alt > 99999.9) {
					logIllegalValue(log, "Alt", alt, -9999.9, 99999.9, lineNumber);
				}
				if (alt.compareTo(-0.0) == 0) { logNegativeZero(log, "Alt", lineNumber); }
				store.setSurfaceAltitude(alt, lineNumber);
			} catch (NumberFormatException e) {
				logNumberFormatException(log, "Alt", altStr, lineNumber);
			}
			// 31. Check to see if there is a space between Alt and Qp.
			if (!line.substring(100, 101).equals(" ")) {
				printToLog(log, 100, "Alt", "Qp", lineNumber);
			}
			// 32. Test the pressure flag value
			String qpStr = line.substring(101, 105);
			if (qpStr.endsWith(" ") || !qpStr.substring(2, 3).equals(".")) {
				logBadPositioning(log, "Qp", qpStr, lineNumber);
			}
			try { 
				Double qp = Double.parseDouble(qpStr.trim());
				if (!isValidFlag(qp)) {
					logIllegalFlag(log, "Qp", qp, lineNumber);
				} else {
					record.setPressureFlag(qp);
				}
			} catch (NumberFormatException e) {
				logNumberFormatException(log, "Qp", qpStr, lineNumber);
			}
			// 33. Check to see if there is a space between Qp and Qt.
			if (!line.substring(105, 106).equals(" ")) {
				printToLog(log, 105, "Qp", "Qt", lineNumber);
			}
			// 34. Test the temperature flag value
			String qtStr = line.substring(106, 110);
			if (qtStr.endsWith(" ") || !qtStr.substring(2, 3).equals(".")) {
				logBadPositioning(log, "Qt", qtStr, lineNumber);
			}
			try { 
				Double qt = Double.parseDouble(qtStr.trim());
				if (!isValidFlag(qt)) {
					logIllegalFlag(log, "Qt", qt, lineNumber);
				} else {
					record.setTemperatureFlag(qt);
				}
			} catch (NumberFormatException e) {
				logNumberFormatException(log, "Qt", qtStr, lineNumber);
			}
			// 35. Check to see if there is a space between Qt and Qrh.
			if (!line.substring(110, 111).equals(" ")) {
				printToLog(log, 110, "Qt", "Qrh", lineNumber);
			}
			// 36. Test the relative humidity flag value.
			String qrhStr = line.substring(111, 115);
			if (qrhStr.endsWith(" ") || !qrhStr.substring(2, 3).equals(".")) {
				logBadPositioning(log, "Qrh", qrhStr, lineNumber);
			}
			try { 
				Double qrh = Double.parseDouble(qrhStr.trim());
				if (!isValidFlag(qrh)) {
					logIllegalFlag(log, "Qrh", qrh, lineNumber);
				} else {
					record.setRelativeHumidityFlag(qrh);
				}
			} catch (NumberFormatException e) {
				logNumberFormatException(log, "Qrh", qrhStr, lineNumber);
			}
			// 37. Check to see if there is a space between Qrh and Qu.
			if (!line.substring(115, 116).equals(" ")) {
				printToLog(log, 115, "Qrh", "Qu", lineNumber);
			}
			// 38. Test the u component flag value.
			String quStr = line.substring(116, 120);
			if (quStr.endsWith(" ") || !quStr.substring(2, 3).equals(".")) {
				logBadPositioning(log, "Qu", quStr, lineNumber);
			}
			try { 
				Double qu = Double.parseDouble(quStr.trim());
				if (!isValidFlag(qu)) {
					logIllegalFlag(log, "Qu", qu, lineNumber);
				} else {
					record.setUComponentFlag(qu);
				}
			} catch (NumberFormatException e) {
				logNumberFormatException(log, "Qu", quStr, lineNumber);
			}
			// 39. Check to see if there is a space between Qu and Qv.
			if (!line.substring(120, 121).equals(" ")) {
				printToLog(log, 120, "Qu", "Qv", lineNumber);
			}
			// 40. Test the v component flag value.
			String qvStr = line.substring(121, 125);
			if (qvStr.endsWith(" ") || !qvStr.substring(2, 3).equals(".")) {
				logBadPositioning(log, "Qv", qvStr, lineNumber);
			}
			try { 
				Double qv = Double.parseDouble(qvStr.trim());
				if (!isValidFlag(qv)) {
					logIllegalFlag(log, "Qv", qv, lineNumber);
				} else {
					record.setVComponentFlag(qv);
				}
			} catch (NumberFormatException e) {
				logNumberFormatException(log, "Qv", qvStr, lineNumber);
			}
			// 41. Check to see if there is a space between Qu and QdZ.
			if (!line.substring(125, 126).equals(" ")) {
				printToLog(log, 125, "Qv", "QdZ", lineNumber);
			}
			// 42. Test the ascent rate flag value.
			String qdzStr = line.substring(126, 130);
			if (qdzStr.endsWith(" ") || !qdzStr.substring(2, 3).equals(".")) {
				logBadPositioning(log, "QdZ", qdzStr, lineNumber);
			}
			try { 
				Double qdz = Double.parseDouble(qdzStr.trim());
				if (!isValidFlag(qdz)) {
					logIllegalFlag(log, "QdZ", qdz, lineNumber);
				} else {
					record.setAscentRateFlag(qdz);
				}
			} catch (NumberFormatException e) {
				logNumberFormatException(log, "QdZ", qdzStr, lineNumber);
			}
			
			record.executeConsistancyChecks(log, lineNumber);
		}
		
		log.flush();
	}

	/**
	 * Perform the check to ensure that the data line is past the 15th 
	 * line of the file.
	 * @param lineNumber The number of the line in the file.
	 * @param log The output stream where failures are to be written.
	 */
	@Override public void executeLineNumberCheck(int lineNumber, PrintWriter log) {
		if (lineNumber < 16) {
			log.printf("The 'Data' line is expected on a line after 15.  It is on line %d.\n", lineNumber);
			log.flush();
		}
	}
	
	/**
	 * Determine if the specified flag is a valid flag for the ESC format.
	 * @param flag The flag to be tested.
	 * @return <code>true</code> if the flag is a valid flag, <code>false</code>
	 * if it is not.
	 */
	private boolean isValidFlag(Double flag) {
		return (flag.equals(1.0) || flag.equals(2.0) || flag.equals(3.0) || flag.equals(4.0) || flag.equals(9.0) || flag.equals(99.0));
	}
	
	/**
	 * Print the failure of finding a value in an unexpected location on the data line.
	 * @param log The output stream where the failure is to be written.
	 * @param name The name of the measurement that has a badly positioned value.
	 * @param value The value that is badly positioned.
	 * @param lineNumber The line number of the data line where the failure was found.
	 */
	private void logBadPositioning(PrintWriter log, String name, String value, int lineNumber) {
		log.printf("The 'Data' line has a badly positioned value '%s' of %s on line %d.\n", name, value, lineNumber);
	}
	
	/**
	 * Print the finding of an illegal flag value to an output stream.
	 * @param log The output stream where the failure is to be written.
	 * @param name The name of the measurement that has an illegal flag.
	 * @param value The illegal flag value.
	 * @param lineNumber The line number of the data line where the failure was found.
	 */
	private void logIllegalFlag(PrintWriter log, String name, Double value, int lineNumber) {
		log.printf("The 'Data' line has an illegal value %.1f for flag '%s' on line %d.\n", value, name, lineNumber);
	}
	
	/**
	 * Print the finding of an illegal value to an output stream.
	 * @param log The output stream where the failure is to be written.
	 * @param name The name of the measurment that has an illegal value.
	 * @param value The illegal value.
	 * @param low The minimum allowed value for the measurement.
	 * @param high The maximum allowed value for the measurement.
	 * @param lineNumber The line number of the data line where the failure was found.
	 */
	private void logIllegalValue(PrintWriter log, String name, Double value, Double low, Double high, int lineNumber) {
		log.printf("The 'Data' line has an illegal value %.1f (%.1f, %.1f) for '%s' on line %d.\n.", value, low, high, name, lineNumber);
	}
	
	/**
	 * Print the finding of a negative zero to an output stream.
	 * @param log The output stream where the failure is to be written.
	 * @param name The name of the measurment that has a negative zero value.
	 * @param lineNumber The line number of the data line where the failure was found.
	 */
	private void logNegativeZero(PrintWriter log, String name, int lineNumber) {
		log.printf("The 'Data' line has a negative zero value for '%s' on line %d.\n", name, lineNumber);
	}
	
	/**
	 * Print the failure to parse a number to an output stream.
	 * @param log The output stream where the failure is to be written.
	 * @param name The name of the measurment that failed.
	 * @param value The value of the measurement that caused the failure.
	 * @param lineNumber The line number of the data line where the failure was found.
	 */
	private void logNumberFormatException(PrintWriter log, String name, String value, int lineNumber) {
		log.printf("The 'Data' line has a non-numerical value of %s for '%s' on line %d.\n", value, name, lineNumber);
	}
	
	/**
	 * Print the failure to find a space in the expected location to an output stream.
	 * @param log The output stream where the failure is to be written.
	 * @param index The index where the space was expected.
	 * @param leftMeasure The measurement to the left of the expected space on the data line.
	 * @param rightMeasure The measurement to the right of the expected space on the data line.
	 * @param lineNumber The line number of the data line where the failure was found.
	 */
	private void printToLog(PrintWriter log, int index, String leftMeasure, String rightMeasure, int lineNumber) {
		log.printf("The 'Data' line does not contain a space at character %d between the '%s' and '%s' records on line %d.\n", index, leftMeasure, rightMeasure, lineNumber);
	}
}
