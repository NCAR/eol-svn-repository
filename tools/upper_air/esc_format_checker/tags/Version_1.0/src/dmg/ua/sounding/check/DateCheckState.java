package dmg.ua.sounding.check;

import static dmg.util.TimeUtils.*;

import dmg.util.*;

import java.io.*;
import java.util.*;
import java.util.regex.*;

/**
 * <p>The DateCheckState class is a CheckState that checks an ESC header
 * line that contains a time for the sounding.  It provides a set of common
 * utilities needed by any date state.</p>
 * 
 * @author Joel Clawson
 */
public abstract class DateCheckState extends CheckState {

	/**
	 * Create a new instance of a DateCheckState.
	 * @param in The input stream that contains the sounding being checked.
	 * @param currentLine The current line being checked by the state.
	 * @param lineNumber The number of the line in the file.
	 */
	public DateCheckState(BufferedReader in, String currentLine, int lineNumber) {
		super(in, currentLine, lineNumber);
	}

	/**
	 * Parse the date from the specified String.
	 * @param dateString The date in the String format.
	 * @return The date from the String or <code>null</code> if the date could
	 * not be determined from the String.
	 */
	protected Calendar parseDate(String dateString) {
		// Define the matcher that will do the regex stuff on the string.
		Matcher matcher = Pattern.compile("^(\\d{4}), (\\d{2}), (\\d{2}), (\\d{2}):(\\d{2}):(\\d{2})$").matcher(dateString);
		
		// Need to execute the find to actually assign the groups.
		if (!matcher.find()) { return null; }
		
		try {
			return buildDate(Integer.parseInt(matcher.group(1)), 
					         Integer.parseInt(matcher.group(2)),
					         Integer.parseInt(matcher.group(3)),
					         Integer.parseInt(matcher.group(4)),
					         Integer.parseInt(matcher.group(5)),
					         Integer.parseInt(matcher.group(6)), UTC);
		} 
		// Can't create the date so just return null.
		catch (NumberFormatException e) {
			return null;
		} catch (DateTimeException e) {
			return null;
		}
	}
}
