package dmg.ua.sounding.check;

import java.io.*;

/**
 * <p>The HeaderSeparatorCheckState class is a CheckState that checks the ESC header
 * line that separates the header and data in the sounding.  The checks makes sure
 * that each sequence of hyphens has the proper number in the correct location on the
 * line while making sure that the spaces are also in the correct spot.  This
 * also checks that the line itself is on the expected line of the sounding
 * header.</p>
 * <p>This class is a Concrete State of the <i>State</i> pattern described in
 * <u>Design Patterns</u> by Gamma, Helm, Johnson, and Vlissides.</p>
 * 
 * @author Joel Clawson
 */
public class HeaderSeparatorCheckState extends CheckState {

	/**
	 * Create a new instance of a HeaderSeparatorCheckState.
	 * @param in The input stream that contains the sounding being checked.
	 * @param currentLine The current line being checked by the state.
	 * @param lineNumber The number of the line in the file.
	 */
	public HeaderSeparatorCheckState(BufferedReader in, String currentLine, int lineNumber) {
		super(in, currentLine, lineNumber);
	}

	/**
	 * Perform the checks on the data line.  This ensures the line has the correct
	 * number of characters and will continue to test for the hyphen sequences are in
	 * the correct positions along with having spaces in the correct indecies.
	 * @param line The line containing the separators being checked.
	 * @param lineNumber The number of the line in the data file.
	 * @param store The store for data information needed beyond the current line for later checks.
	 * @param log The output stream where failures are to be written.
	 */
	@Override public void executeLineCheck(String line, int lineNumber, DataStore store, PrintWriter log) {
		// This line must be exactly in this format to be correct.
		if (!line.equals("------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----")) {
			log.printf("The 'Separator' header line does not match the required format on line %d.\n", lineNumber);
			log.flush();
		}
	}

	/**
	 * Perform the check to ensure that the header separator line is on the 15th 
	 * line of the file.
	 * @param lineNumber The number of the line in the file.
	 * @param log The output stream where failures are to be written.
	 */
	@Override public void executeLineNumberCheck(int lineNumber, PrintWriter log) {
		if (lineNumber != 15) {
			log.printf("The 'Separator' header line is expected on line 15.  It is on line %d.\n", lineNumber);
			log.flush();
		}
	}
}
