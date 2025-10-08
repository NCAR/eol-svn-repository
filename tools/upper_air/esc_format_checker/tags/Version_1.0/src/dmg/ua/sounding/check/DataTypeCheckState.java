package dmg.ua.sounding.check;

import java.io.*;

/**
 * <p>The DataTypeCheckState class is a CheckState that checks the ESC header
 * line that contains the data type for the sounding.  The checks make sure that 
 * the header label is the expected label and padded correctly.  It also performs 
 * checks on the text to ensure that it is in the correct location and exists
 * without any extra whitespace.  This also checks that the line itself is on 
 * the expected line of the sounding header.</p>
 * <p>This class is a Concrete State of the <i>State</i> pattern described in
 * <u>Design Patterns</u> by Gamma, Helm, Johnson, and Vlissides.</p>
 * 
 * @author Joel Clawson
 */
public class DataTypeCheckState extends CheckState {

	/**
	 * Create a new instance of a DataTypeCheckState.
	 * @param in The input stream that contains the sounding being checked.
	 * @param currentLine The current line being checked by the state.
	 * @param lineNumber The number of the line in the file.
	 */
	public DataTypeCheckState(BufferedReader in, String currentLine, int lineNumber) {
		super(in, currentLine, lineNumber);
	}

	/**
	 * Perform the checks on the data line.  This ensures the line has the correct
	 * label and has valid text after the label in the correct location.
	 * @param line The line containing the label and type being checked.
	 * @param lineNumber The number of the line in the data file.
	 * @param store The store for data information needed beyond the current line for later checks.
	 * @param log The output stream where failures are to be written.
	 */
	@Override public void executeLineCheck(String line, int lineNumber, DataStore store, PrintWriter log) {
		// 1.  Make sure the label contains the expected content.
		if (!line.startsWith("Data Type:                         ")) {
			log.printf("The 'Data Type' label is not a 35 char string starting with 'Data Type:' on line %d.\n", lineNumber);
		}
		
		// Need to make sure the data section can actually be parsed.
		if (line.length() > 35) {
			// 2.  Make sure there is some text after the label.  It could be anything, so only make sure it is not empty.
			if (line.substring(35).trim().equals("")) {
				log.printf("The 'Data Type' line does not have any text after the label on line %d.\n", lineNumber);
			}
			else {
				// 3.  Make sure the text begins on the correct column.
				if (line.substring(35).startsWith(" ")) {
					log.printf("The 'Data Type' header line does not have the text after the label begin in the correct spot on line %d.\n", lineNumber);
				}
				// 4.  Make sure there isn't any trailing whitespace.
				if (line.substring(35).endsWith(" ")) {
					log.printf("The 'Data Type' header line has extra whitespace at the end of the line on line %d.\n", lineNumber);
				}
			}
		}
		
		log.flush();
	}

	/**
	 * Perform the check to ensure that the data type header line is on the 1st 
	 * line of the file.
	 * @param lineNumber The number of the line in the file.
	 * @param log The output stream where failures are to be written.
	 */
	@Override public void executeLineNumberCheck(int lineNumber, PrintWriter log) {
		if (lineNumber != 1) {
			log.printf("The 'Data Type' header line is expected on line 1.  It is on line %d.\n", lineNumber);
			log.flush();
		}
	}
}
