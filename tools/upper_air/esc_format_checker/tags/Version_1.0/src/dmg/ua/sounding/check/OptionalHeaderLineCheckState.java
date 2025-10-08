package dmg.ua.sounding.check;

import java.io.*;
import java.util.regex.*;

/**
 * <p>The OptionalHeaderLineCheckState class is a CheckState that checks an ESC header
 * line that contains optional header information of a filler line for the sounding.
 * The checks make sure that the header label is a valid label and padded correctly.  
 * It also performs checks on the text to ensure that it is in the correct location 
 * and exists without any extra whitespace.  If the line is a filler line it ensures
 * that it only contains the / and nothing else.  This also checks that the line itself
 * is on the expected line of the sounding header.</p>
 * <p>This class is a Concrete State of the <i>State</i> pattern described in
 * <u>Design Patterns</u> by Gamma, Helm, Johnson, and Vlissides.</p>
 * 
 * @author Joel Clawson
 */
public class OptionalHeaderLineCheckState extends CheckState {

	/**
	 * Create a new instance of a OptionalHeaderLineCheckState.
	 * @param in The input stream that contains the sounding being checked.
	 * @param currentLine The current line being checked by the state.
	 * @param lineNumber The number of the line in the file.
	 */
	public OptionalHeaderLineCheckState(BufferedReader in, String currentLine, int lineNumber) {
		super(in, currentLine, lineNumber);
	}

	/**
	 * Perform the checks on the optional header line.  This ensures the line has a
	 * valid label and has valid text after the label in the correct location if it is
	 * not a filler line.  If the header line is a filler line it makes sure that it
	 * is only a / and nothing else.
	 * @param line The line containing the label and type being checked.
	 * @param lineNumber The number of the line in the data file.
	 * @param store The store for data information needed beyond the current line for later checks.
	 * @param log The output stream where failures are to be written.
	 */
	@Override public void executeLineCheck(String line, int lineNumber, DataStore store, PrintWriter log) {
		// 1.  Check to see if it is a filler header line.
		if (line.startsWith("/")) {
			// 1.1  Make sure the filler header line only contains the slash.
			if (!line.equals("/")) {
				log.printf("The 'Filler' header line has more than just the '/' on line %d.\n", lineNumber);
			}
		}
		
		// 2.  Handle optional headers with real data.
		else {
			if (line.length() > 35) {
				// 2.1  Make sure the header label is valid.
				if (!Pattern.matches("^[^:]+:\\s*$", line.substring(0, 35))) {
					log.printf("The 'Optional' header line has an illegal label of '%s' on line %d.\n", line.substring(0, 35), lineNumber);
				}
				// 2.2  Make sure there is some text after the label.
				if (line.substring(35).trim().equals("")) {
					log.printf("The 'Optional' header line does not have any text after the label on line %d.\n", lineNumber);
				}
				else { 
					// 2.3  Make sure that the text that exists starts on the correct column
					if (line.substring(35).startsWith(" ")) {
						log.printf("The 'Optional' header line does not have the text after the label begin in the correct spot on line %d.\n", lineNumber);
					}
					// 2.4  Make sure there isn't any trailing whitespace.
					if (line.substring(35).endsWith(" ")) {
						log.printf("The 'Optional' header line has extra whitespace at the end of the line on line %d.\n", lineNumber);
					}
				}
			} else {
				log.printf("The 'Optional' header line has a format problem '%s' on line %d.\n", line, lineNumber);
			}
		}
		
		log.flush();
	}

	/**
	 * Perform the check to ensure that the optional header line is between the 6th and 
	 * 11th line of the file.
	 * @param lineNumber The number of the line in the file.
	 * @param log The output stream where failures are to be written.
	 */
	@Override public void executeLineNumberCheck(int lineNumber, PrintWriter log) {
		if (lineNumber < 6 || lineNumber > 11) {
			log.printf("The 'Optional' header line is expected on lines 6-11.  It is on line %d.\n", lineNumber);
			log.flush();
		}
	}
}
