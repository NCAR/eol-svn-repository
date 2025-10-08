package dmg.ua.sounding.check;

import java.io.*;
import java.util.*;

/**
 * <p>The ActualTimeCheckState class is a CheckState that checks the ESC header
 * line that contains the actual time the sounding was released.  The checks 
 * make sure that the header label is the expected label and padded correctly.
 * It also performs checks on the date to ensure that it is formatted correctly
 * and is a real date.  This also checks that the line itself is on the expected 
 * line of the sounding header.</p>
 * <p>This class is a Concrete State of the <i>State</i> pattern described in
 * <u>Design Patterns</u> by Gamma, Helm, Johnson, and Vlissides.</p>
 * 
 * @author Joel Clawson
 */
public class ActualTimeCheckState extends DateCheckState {

	/**
	 * Create a new instance of a ActualTimeCheckState.
	 * @param in The input stream that contains the sounding being checked.
	 * @param currentLine The current line being checked by the state.
	 * @param lineNumber The number of the line in the file.
	 */
	public ActualTimeCheckState(BufferedReader in, String currentLine, int lineNumber) {
		super(in, currentLine, lineNumber);
	}

	/**
	 * Perform the checks on the data line.  This ensures the line has the correct
	 * label and a valid date in the expected format.
	 * @param line The line containing the label and date being checked.
	 * @param lineNumber The number of the line in the data file.
	 * @param store The store for data information needed beyond the current line for later checks.
	 * @param log The output stream where failures are to be written.
	 */
	@Override public void executeLineCheck(String line, int lineNumber, DataStore store, PrintWriter log) {
		// 1.  Make sure the label contains the expected content.
		if (!line.startsWith("UTC Release Time (y,m,d,h,m,s):    ")) {
			log.printf("The 'Actual Release' label is not a 35 char string starting with 'UTC Release Time (y,m,d,h,m,s):' on line %d.\n", lineNumber);
		}
		
		// 2.  Make sure there is a valid date after the label.
		Calendar actual = parseDate(line.substring(35));
		if (actual == null) {
			log.printf("The 'Actual Release' line does not have a valid date '%s' after the label on line %d.\n", line.substring(35), lineNumber);
		} else {
			store.setActualDate(actual);
		}
		log.flush();
	}

	/**
	 * Perform the check to ensure that the actual time header line is on the 5th 
	 * line of the file.
	 * @param lineNumber The number of the line in the file.
	 * @param log The output stream where failures are to be written.
	 */
	@Override public void executeLineNumberCheck(int lineNumber, PrintWriter log) {
		if (lineNumber != 5) {
			log.printf("The 'Actual Release' header line is expected on line 5.  It is on line %d.\n", lineNumber);
			log.flush();
		}
	}
}
