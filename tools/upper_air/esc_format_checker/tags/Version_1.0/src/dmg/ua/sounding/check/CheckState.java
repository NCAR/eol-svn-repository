package dmg.ua.sounding.check;

import java.io.*;
import java.util.regex.*;

/**
 * <p>The CheckState class is an abstract state that checks a line in an ESC
 * formatted sounding.  It performs the required checks on the specific line
 * along with testing to see if the information found is on the expected 
 * sounding line.</p>
 * <p>This class is the State of the <i>State</i> pattern described in
 * <u>Design Patterns</u> by Gamma, Helm, Johnson, and Vlissides.</p>
 * 
 * @author Joel Clawson
 */
public abstract class CheckState {
	
	private BufferedReader in;
	private int lineNumber;
	private String currentLine;
	
	/**
	 * Create a new instance of a CheckState.
	 * @param in The input stream that contains the sounding being checked.
	 * @param currentLine The current line being checked by the state.
	 * @param lineNumber The number of the line in the file.
	 */
	public CheckState(BufferedReader in, String currentLine, int lineNumber) {
		this.in = in;
		this.currentLine = currentLine;
		this.lineNumber = lineNumber;
	}
	
	/**
	 * Determine if the line contained within the state can be checked.
	 * @return <code>true</code> if the current line is defined or the input
	 * stream is ready to be read, <code>false</code> it the current line is
	 * <code>null</code> and the input stream cannot be read.
	 * @throws IOException if there is a problem testing the state of the
	 * input stream.
	 */
	public boolean canCheckLine() throws IOException { 
		return currentLine != null || in.ready();
	}

	/**
	 * Check the current line within the state.  This checks both the data and the
	 * expected line number for the current line.
	 * @param store The DataStore that maintains general sounding information that is
	 * contained beyond the current line.
	 * @param log The output stream where errors are to be written.
	 * @return The next state to be tested after the current line has finsished.
	 * @throws IOException if there is a problem writing to the log stream or if
	 * the next state cannot be determined.
	 */
	public CheckState checkLine(DataStore store, PrintWriter log) throws IOException {
		executeLineNumberCheck(lineNumber, log);
		executeLineCheck(currentLine, lineNumber, store, log);
		return determineNextState();
	}
	
	/**
	 * Determine the next state that should be tested.
	 * @return The next state to be tested.
	 * @throws IOException when there is a problem reading the input stream to
	 * determine the next state.
	 */
	private CheckState determineNextState() throws IOException {
		// Read in the next line from the input stream.
		String line = in.readLine();
		
		// Handle the case where the input stream has no more data to be read
		if (line == null) {
			this.currentLine = line;
			return this;
		}

		// Create an all lower case line to make comparisons easier.
		String lcLine = line.toLowerCase();
		// Look for a data type header line.
		if (lcLine.startsWith("data type")) {
			return new DataTypeCheckState(in, line, lineNumber + 1);
		}
		// Look for a project id header line.
		else if (lcLine.startsWith("project id")) {
			return new ProjectCheckState(in, line, lineNumber + 1);
		}
		// Look for a release site line.
		else if (lcLine.startsWith("release site")) {
			return new SiteCheckState(in, line, lineNumber + 1);
		}
		// Look for a release location line
		else if (lcLine.startsWith("release location")) {
			return new LocationCheckState(in, line, lineNumber + 1);
		}
		// Look for a actual release time line.
		else if (lcLine.startsWith("utc release time")) {
			return new ActualTimeCheckState(in, line, lineNumber + 1);
		}
		// Look for a nominal release time line.
		else if (lcLine.startsWith("nominal release time")) {
			return new NominalTimeCheckState(in, line, lineNumber + 1);
		}
		// Look for an empty optional header line
		else if (lcLine.startsWith("/") || lcLine.contains(":")) {
			return new OptionalHeaderLineCheckState(in, line, lineNumber + 1);
		}
		// Look for a line that separates the header from the data
		else if (Pattern.matches("^[\\-\\s]+$", lcLine)) {
			return new HeaderSeparatorCheckState(in, line, lineNumber + 1);
		}
		// Look for a measurement/units header line
		else if (Pattern.matches("^[\\sa-z\\%\\/]+$", lcLine)) {
			if (lcLine.contains("code")) {
				return new UnitsCheckState(in, line, lineNumber + 1);
			} else {
				return new MeasurementsCheckState(in, line, lineNumber + 1);
			}
		}
		// Assume anything else is a data line.
		else {
			return new DataLineCheckState(in, line, lineNumber + 1);
		}
	}
	
	/**
	 * Perform the checks on the specified line.
	 * @param line The line containing the values being checked.
	 * @param lineNumber The number of the line in the data file.
	 * @param store The store for data information needed beyond the current line for later checks.
	 * @param log The output stream where failures are to be written.
	 */
	public abstract void executeLineCheck(String line, int lineNumber, DataStore store, PrintWriter log);
	
	/**
	 * Perform the check to ensure that the line occured on the expected line of the file.
	 * @param lineNumber The number of the line in the file.
	 * @param log The output stream where failures are to be written.
	 */
	public abstract void executeLineNumberCheck(int lineNumber, PrintWriter log);
}
