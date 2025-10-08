package dmg.ua.sounding.check;

import java.io.*;

/**
 * <p>The MeasurementsCheckState class is a CheckState that checks the ESC header
 * line that contains the measurements in the sounding.  The checks makes sure
 * that each measurement has the proper format in the correct location on the
 * line while making sure that the spaces are also in the correct spot.  This
 * also checks that the line itself is on the expected line of the sounding
 * header.</p>
 * <p>This class is a Concrete State of the <i>State</i> pattern described in
 * <u>Design Patterns</u> by Gamma, Helm, Johnson, and Vlissides.</p>
 * 
 * @author Joel Clawson
 */
public class MeasurementsCheckState extends CheckState {

	/**
	 * Create a new instance of a MeasurementsCheckState.
	 * @param in The input stream that contains the sounding being checked.
	 * @param currentLine The current line being checked by the state.
	 * @param lineNumber The number of the line in the file.
	 */
	public MeasurementsCheckState(BufferedReader in, String currentLine, int lineNumber) {
		super(in, currentLine, lineNumber);
	}

	/**
	 * Perform the checks on the data line.  This ensures the line has the correct
	 * number of characters and will continue to test for the measurements being in
	 * the correct positions along with having spaces in the correct indecies.
	 * @param line The line containing the measurements being checked.
	 * @param lineNumber The number of the line in the data file.
	 * @param store The store for data information needed beyond the current line for later checks.
	 * @param log The output stream where failures are to be written.
	 */
	@Override public void executeLineCheck(String line, int lineNumber, DataStore store, PrintWriter log) {
		// 1.  Make sure the line is the correct length.
		if (line.length() != 130) {
			log.printf("The 'Measurement' line is expected to be 130 characters long, but was %d on line %d.\n", line.length(), lineNumber);
		}
		
		else {
			
			// 2.  Check to see if the first measurement is time.
			if (!line.substring(0, 6).trim().equals("Time")) {
				printToLog(log, "Time", 0, 6, lineNumber);
			}
			// 3.  Check to see if there is a space between the Time and Pressure measurements
			if (!line.substring(6, 7).equals(" ")) {
				printToLog(log, 6, "Time", "Press", lineNumber);
			}
			// 4.  Check to see if the second measurement is pressure.
			if (!line.substring(7, 13).trim().equals("Press")) {
				printToLog(log, "Press", 7, 13, lineNumber);
			}
			// 5.  Check to see if there is a space between the Presure and Temperature measurements.
			if (!line.substring(13, 14).equals(" ")) {
				printToLog(log, 13, "Press", "Temp", lineNumber);
			}
			// 6.  Check to see if the third measurements is temperature.
			if (!line.substring(14, 19).trim().equals("Temp")) {
				printToLog(log, "Temp", 14, 19, lineNumber);
			}
			// 7.  Check to see if there is a space between the temperature and dew point measurements.
			if (!line.substring(19, 20).equals(" ")) {
				printToLog(log, 19, "Temp", "Dewpt", lineNumber);
			}
			// 8.  Check to see if the fourth measurement is dew point.
			if (!line.substring(20, 25).equals("Dewpt")) {
				printToLog(log, "Dewpt", 20, 25, lineNumber);
			}
			// 9.  Check to see if there is a space between the dew point and rh.
			if (!line.substring(25, 26).equals(" ")) {
				printToLog(log, 25, "Dewpt", "RH", lineNumber);
			}
			// 10. Check to see if the fifth measurement is rh.
			if (!line.substring(26, 31).trim().equals("RH")) {
				printToLog(log, "RH", 26, 31, lineNumber);
			}
			// 11. Check to see if there is a space between the RH and Ucmp
			if (!line.substring(31, 32).equals(" ")) {
				printToLog(log, 31, "RH", "Ucmp", lineNumber);
			}
			// 12. Check to see if the sixth measurement is Ucmp
			if (!line.substring(32, 38).trim().equals("Ucmp")) {
				printToLog(log, "Ucmp", 32, 38, lineNumber);
			}
			// 13. Check to see if there is a space between the Ucmp and Vcmp
			if (!line.substring(38, 39).equals(" ")) {
				printToLog(log, 38, "Ucmp", "Vcmp", lineNumber);
			}
			// 14. Check to see if the seventh measurement is Vcmp
			if (!line.substring(39, 45).trim().equals("Vcmp")) {
				printToLog(log, "Vcmp", 39, 45, lineNumber);
			}
			// 15. Check to see if there is a space between the Vcmp and spd
			if (!line.substring(45, 46).equals(" ")) {
				printToLog(log, 45, "Vcmp", "spd", lineNumber);
			}
			// 16. Check to see if the eighth measurement is spd.
			if (!line.substring(46, 51).trim().equals("spd")) {
				printToLog(log, "spd", 46, 51, lineNumber);
			}
			// 17. Check to see if there is a space between the spd and dir
			if (!line.substring(51, 52).equals(" ")) {
				printToLog(log, 51, "spd", "dir", lineNumber);
			}
			// 18. Check to see if the ninth measurement is the wind direction.
			if (!line.substring(52, 57).trim().equals("dir")) {
				printToLog(log, "dir", 52, 57, lineNumber);
			}
			// 19. Check to see if there is a space between dir and Wcmp
			if (!line.substring(57, 58).equals(" ")) {
				printToLog(log, 57, "dir", "Wcmp", lineNumber);
			}
			// 20. Check to see if the tenth measurement is ascent rate.
			if (!line.substring(58, 63).trim().equals("Wcmp")) {
				printToLog(log, "Wcmp", 58, 63, lineNumber);
			}
			// 21. Check to see if there is a space between Wcmp and Lon.
			if (!line.substring(63, 64).equals(" ")) {
				printToLog(log, 63, "Wcmp", "Lon", lineNumber);
			}
			// 22. Check to see if the eleventh measurement is the longitude
			if (!line.substring(64, 72).trim().equals("Lon")) {
				printToLog(log, "Lon", 64, 72, lineNumber);
			}
			// 23. Check to see if there is a space between Lon and Lat.
			if (!line.substring(72, 73).equals(" ")) {
				printToLog(log, 72, "Lon", "Lat", lineNumber);
			}
			// 24. Check to see if the twelfth measurement is the latitude.
			if (!line.substring(73, 80).trim().equals("Lat")) {
				printToLog(log, "Lat", 73, 80, lineNumber);
			}
			// 25. Check to see if there is a space between Lat and Var 1.
			if (!line.substring(80, 81).equals(" ")) {
				printToLog(log, 80, "Lat", "variable measurement 1", lineNumber);
			}
			// 26. Check to see if the thirteenth measurement is the 1st var measurement.
			if (line.substring(81, 86).trim().equals("")) {
				log.printf("The 'Measurement' line does not contain the first variable measurement between characters %d and %d on line %d.\n", 81, 86, lineNumber);
			}
			// 27. Check to see if there is a space between Var 1 and Var 2.
			if (!line.substring(86, 87).equals(" ")) {
				printToLog(log, 86, "variable measurement 1", "variable measurement 2", lineNumber);
			}
			// 28. Check to see if the fourteenth measurement is the 2nd var measurement.
			if (line.substring(87, 92).trim().equals("")) {
				log.printf("The 'Measurement' line does not contain the second variable measurement between characters %d and %d on line %d.\n", 87, 92, lineNumber);
			}
			// 29. Check to see if there is a space between Var 2 and Alt
			if (!line.substring(92, 93).equals(" ")) {
				printToLog(log, 92, "variable measurement 2", "Alt", lineNumber);
			}
			// 30. Check to see if the fifteenth measurement is altitude.
			if (!line.substring(93, 100).trim().equals("Alt")) {
				printToLog(log, "Alt", 93, 100, lineNumber);
			}
			// 31. Check to see if there is a space between Alt and Qp.
			if (!line.substring(100, 101).equals(" ")) {
				printToLog(log, 100, "Alt", "Qp", lineNumber);
			}
			// 32. Check to see if the sixteenth measurement is the pressure flag.
			if (!line.substring(101, 105).trim().equals("Qp")) {
				printToLog(log, "Qp", 101, 105, lineNumber);
			}
			// 33. Check to see if there is a space between Qp and Qt.
			if (!line.substring(105, 106).equals(" ")) {
				printToLog(log, 105, "Qp", "Qt", lineNumber);
			}
			// 34. Check to see if the seventeenth measurement is the temperature flag.
			if (!line.substring(106, 110).trim().equals("Qt")) {
				printToLog(log, "Qt", 106, 110, lineNumber);
			}
			// 35. Check to see if there is a space between Qt and Qrh.
			if (!line.substring(110, 111).equals(" ")) {
				printToLog(log, 110, "Qt", "Qrh", lineNumber);
			}
			// 36. Check to see if the eighteenth measurement is the rh flag.
			if (!line.substring(111, 115).trim().equals("Qrh")) {
				printToLog(log, "Qrh", 111, 115, lineNumber);
			}
			// 37. Check to see if there is a space between Qrh and Qu.
			if (!line.substring(115, 116).equals(" ")) {
				printToLog(log, 115, "Qrh", "Qu", lineNumber);
			}
			// 38. Check to see if the nineteenth measurement is the u comp flag.
			if (!line.substring(116, 120).trim().equals("Qu")) {
				printToLog(log, "Qu", 116, 120, lineNumber);
			}
			// 39. Check to see if there is a space between Qu and Qv.
			if (!line.substring(120, 121).equals(" ")) {
				printToLog(log, 120, "Qu", "Qv", lineNumber);
			}
			// 40. Check to see if the twentieth measurement is the v comp flag.
			if (!line.substring(121, 125).trim().equals("Qv")) {
				printToLog(log, "Qv", 121, 125, lineNumber);
			}
			// 41. Check to see if there is a space between Qu and QdZ.
			if (!line.substring(125, 126).equals(" ")) {
				printToLog(log, 125, "Qv", "QdZ", lineNumber);
			}
			// 42. Check to see if the twenty-first measurement is the ascent rate flag.
			if (!line.substring(126, 130).trim().equals("QdZ")) {
				printToLog(log, "QdZ", 126, 130, lineNumber);
			}
		}
		
		log.flush();
	}

	/**
	 * Perform the check to ensure that the measurement data line is on the 13th 
	 * line of the file.
	 * @param lineNumber The number of the line in the file.
	 * @param log The output stream where failures are to be written.
	 */
	@Override public void executeLineNumberCheck(int lineNumber, PrintWriter log) {
		if (lineNumber != 13) {
			log.printf("The 'Measurement' header line is expected on line 13.  It is on line %d.\n", lineNumber);
			log.flush();
		}
	}
	
	/**
	 * Print the failure to find a space in the expected location to an output stream.
	 * @param log The output stream where the failure is to be written.
	 * @param index The index where the space was expected.
	 * @param leftMeasure The measurement to the left of the expected space on the data line.
	 * @param rightMeasure The measurement to the right of the expected space on the data line.
	 * @param lineNumber The line number of the measurement line where the failure was found.
	 */
	private void printToLog(PrintWriter log, int index, String leftMeasure, String rightMeasure, int lineNumber) {
		log.printf("The 'Measurement' line does not contain a space at character %d between the '%s' and '%s' measurements on line %d.\n", index, leftMeasure, rightMeasure, lineNumber);
	}
	
	/**
	 * Print the failure to find the expected measurement to an output stream.
	 * @param log The output stream where the failure is to be written.
	 * @param measurement The measurement that caused the failure.
	 * @param lowIndex The index of the data line where the measurement was expected to begin.
	 * @param highIndex The index of the data line where the measurement was expected to end.
	 * @param lineNumber The line number of the measurement line where the failure was found.
	 */
	private void printToLog(PrintWriter log, String measurement, int lowIndex, int highIndex, int lineNumber) {
		log.printf("The 'Measurement' line does not contain '%s' between characters %d and %d on line %d.\n", measurement, lowIndex, highIndex, lineNumber);
	}
}
