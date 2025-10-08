package dmg.ua.sounding.check;

import java.io.*;

/**
 * <p>The InitialCheckState class is a specialized CheckState that is only used
 * as the initial check state before any checking of the sounding is performed.  It
 * does not perform any checks as it can not be associated to any data line.  It's
 * only purpose is to determine the correct state for the first line in the
 * sounding file (in the chance that it is not the expected 'Data Type' line).</p>
 * <p>This class is a Concrete State of the <i>State</i> pattern described in
 * <u>Design Patterns</u> by Gamma, Helm, Johnson, and Vlissides.</p>
 * 
 * @author Joel Clawson
 */
public class InitialCheckState extends CheckState {

	/**
	 * Create a new instance of a InitialCheckState.
	 * @param in The input stream that contains the sounding being checked.
	 */
	public InitialCheckState(BufferedReader in) { 
		super(in, null, 0);
	}

	/**
	 * This does not do any checks since there is not a line associated with this state.
	 * @param line The line to be checked.
	 * @param lineNumber The number of the line in the data file.
	 * @param store The store for data information needed beyond the current line for later checks.
	 * @param log The output stream where failures are to be written.
	 */
	@Override public void executeLineCheck(String line, int lineNumber, DataStore store, PrintWriter log) {}

	/**
	 * This does not do any checks since there is not a line associated with this state.
	 * @param lineNumber The number of the line in the file.
	 * @param log The output stream where failures are to be written.
	 */
	@Override public void executeLineNumberCheck(int lineNumber, PrintWriter log) {}
}
