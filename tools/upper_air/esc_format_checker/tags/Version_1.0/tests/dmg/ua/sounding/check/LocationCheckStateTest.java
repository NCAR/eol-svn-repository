package dmg.ua.sounding.check;

import static org.junit.Assert.*;

import java.io.*;
import org.junit.*;

public class LocationCheckStateTest {

	private LocationCheckState state;
	private DataStore store;
	
	private BufferedReader reader;
	private PrintWriter log;
	
	@Before public void setUp() throws IOException {
    	PipedInputStream in = new PipedInputStream();
    	PipedOutputStream out = new PipedOutputStream(in);
    	
    	reader = new BufferedReader(new InputStreamReader(in));
    	log = new PrintWriter(out);
	
		state = new LocationCheckState(reader, "", 4);
		store = new DataStore();
	}
	
	@Test public void correctLocationLine() throws IOException {
		state.executeLineCheck("Release Location (lon,lat,alt):    117 59.76'W, 36 53.26'N, -117.996, 36.888, 13225.0", 4, store, log);
		assertFalse("Correct Release Location Line: ready", reader.ready());
	}
	
	@Test public void incorrectLabel() throws IOException {
		state.executeLineCheck("Data TYPE:                         117 59.76'W, 36 53.26'N, -117.996, 36.888, 13225.0", 4, store, log);
		assertTrue("Incorrect Label: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Incorrect Label: release location label", line.contains("'Release Location' label"));
		assertTrue("Incorrect Label: expected output", line.contains("Release Location (lon,lat,alt):"));
	}
	
	@Test public void missingColon() throws IOException {
		state.executeLineCheck("Release Location (lon,lat,alt)     117 59.76'W, 36 53.26'N, -117.996, 36.888, 13225.0", 4, store, log);
		assertTrue("Missing Colon: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Missing Colon: release location label", line.contains("'Release Location' label"));
		assertTrue("Missing Colon: expected output", line.contains("Release Location (lon,lat,alt):"));
	}

	@Test public void noPaddingAfterLabel() throws IOException {
		state.executeLineCheck("Release Location (lon,lat,alt):117 59.76'W, 36 53.26'N, -117.996, 36.888, 13225.0", 4, store, log);
		assertTrue("No Padding: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("No Padding: release location label", line.contains("'Release Location' label"));
		assertTrue("No Padding: expected output", line.contains("Release Location (lon,lat,alt):"));
	}

	@Test public void tooLittlePaddingAfterLabel() throws IOException {
		state.executeLineCheck("Release Location (lon,lat,alt):   117 59.76'W, 36 53.26'N, -117.996, 36.888, 13225.0", 4, store, log);
		assertTrue("Too Little Padding: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Too Little Padding: release location label", line.contains("'Release Location' label"));
		assertTrue("Too Little Padding: expected output", line.contains("Release Location (lon,lat,alt):"));
	}

	@Test public void tooMuchPaddingAfterLabel() throws IOException {
		state.executeLineCheck("Release Location (lon,lat,alt):     117 59.76'W, 36 53.26'N, -117.996, 36.888, 13225.0", 4, store, log);
		assertTrue("Too Much Padding: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Too Much Padding: release location", line.contains("'Release Location'"));
		assertTrue("Too Much Padding: expected output", line.contains("does not have any text after the label begin in the correct spot"));
	}
	
	@Test public void noTextAfterLabel() throws IOException {
		state.executeLineCheck("Release Location (lon,lat,alt):     ", 4, store, log);
		assertTrue("No Text: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("No Text: release location label", line.contains("'Release Location'"));
		assertTrue("No Text: expected output", line.contains("does not have any text after the label"));
	}
	
	@Test public void trailingWhiteSpace() throws IOException {
		state.executeLineCheck("Release Location (lon,lat,alt):    117 59.76'W, 36 53.26'N, -117.996, 36.888, 13225.0 ", 4, store, log);
		assertTrue("Trailing Whitespace: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Trailing Whitespace: release location", line.contains("'Release Location'"));
		assertTrue("Trailing Whitespace: extra whitespace", line.contains("extra whitespace at the end of the line"));				
	}

	@Test public void lineNumberCorrect() throws IOException {
		state.executeLineNumberCheck(4, log);
		assertFalse("Correct Line Number: ready", reader.ready());
	}
	
	@Test public void lineNumberTooSmall() throws IOException {
		state.executeLineNumberCheck(3, log);
		assertTrue("Too Small Line Number: ready", reader.ready());
		assertTrue("Too Small Line Number: log read", reader.readLine().contains(" expected on line 4"));
	}
	
	@Test public void lineNumberTooBig() throws IOException {
		state.executeLineNumberCheck(5, log);
		assertTrue("Too Big Line Number: ready", reader.ready());
		assertTrue("Too Big Line Number: log read", reader.readLine().contains(" expected on line 4"));
	}
	
	
	
	@Test public void oneDigitLongitude() throws IOException {
		state.executeLineCheck("Release Location (lon,lat,alt):    1 59.76'W, 36 53.26'N, -1.996, 36.888, 13225.0", 4, store, log);
		assertFalse("1 Digit Lon: ready", reader.ready());
	}
	
	@Test public void twoDigitLongitude() throws IOException {
		state.executeLineCheck("Release Location (lon,lat,alt):    17 59.76'W, 36 53.26'N, -17.996, 36.888, 13225.0", 4, store, log);
		assertFalse("2 Digit Lon: ready", reader.ready());
	}
	
	@Test public void badLongitudeFormat() throws IOException {
		state.executeLineCheck("Release Location (lon,lat,alt):    -117 59.76, 36 53.26'N, -117.996, 36.888, 13225.0", 4, store, log);
		assertTrue("Bad Lon Format: ready", reader.ready());
		assertTrue("Bad Lon Format: content", reader.readLine().contains("DM Longitude is not in the format DDD MM.MM'[EW]"));
	}
	
	@Test public void illegalDMLongitudeDegree() throws IOException {
		state.executeLineCheck("Release Location (lon,lat,alt):    181 59.76'W, 36 53.26'N, -117.996, 36.888, 13225.0", 4, store, log);
		assertTrue("Illegal DM Lon Degree: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Illegal DM Lon Degree: type", line.contains("DM longitude"));
		assertTrue("Illegal DM Lon Degree: content", line.contains("not within the valid range of -180 to 180"));
	}
	
	@Test public void illegalDMLongitudeMinute() throws IOException {
		state.executeLineCheck("Release Location (lon,lat,alt):    117 60.76'W, 36 53.26'N, -117.996, 36.888, 13225.0", 4, store, log);
		assertTrue("Illegal DM Lon Minute: ready", reader.ready());
		assertTrue("Illegal DM Lon Minute: content", reader.readLine().contains("ConversionException"));
	}
	
	@Test public void illegalDMLongitudeDirection() throws IOException {
		state.executeLineCheck("Release Location (lon,lat,alt):    117 59.76'N, 36 53.26'N, -117.996, 36.888, 13225.0", 4, store, log);
		assertTrue("Illegal DM Lon Dir: ready", reader.ready());
		assertTrue("Illegal DM Lon Dir: content", reader.readLine().contains("DM Longitude is not in the format"));
	}
	
	@Test public void nonNumericDMLongitudeDegree() throws IOException {
		state.executeLineCheck("Release Location (lon,lat,alt):    1x7 59.76'W, 36 53.26'N, -117.996, 36.888, 13225.0", 4, store, log);
		assertTrue("Non Numeric DM Lon Degree: ready", reader.ready());
		assertTrue("Non Numeric DM Lon Degree: content", reader.readLine().contains("DM Longitude is not in the format"));
	}
	
	@Test public void nonNumericDMLongitudeMinute() throws IOException {
		state.executeLineCheck("Release Location (lon,lat,alt):    117 59.x6'W, 36 53.26'N, -117.996, 36.888, 13225.0", 4, store, log);
		assertTrue("Non Numeric DM Lon Minute: ready", reader.ready());
		assertTrue("Non Numeric DM Lon Minute: content", reader.readLine().contains("DM Longitude is not in the format"));
	}

	@Test public void nonNumericDegLongitude() throws IOException {
		state.executeLineCheck("Release Location (lon,lat,alt):    117 59.76'W, 36 53.26'N, -11X.996, 36.888, 13225.0", 4, store, log);
		assertTrue("Non Numeric Deg Lon: ready", reader.ready());
		assertTrue("Non Numeric Deg Lon: content", reader.readLine().contains("Unable to convert the longitude value"));
	}
	
	@Test public void longitudeValuesDontMatch() throws IOException {
		state.executeLineCheck("Release Location (lon,lat,alt):    117 59.76'W, 36 53.26'N, -117.995, 36.888, 13225.0", 4, store, log);
		assertTrue("Lon Don't Match: ready", reader.ready());
		assertTrue("Lon Don't Match: content", reader.readLine().contains("The two longitude values do not match"));
	}
	
	@Test public void oneDigitLatitude() throws IOException {
		state.executeLineCheck("Release Location (lon,lat,alt):    1 59.76'W, 6 53.26'N, -1.996, 6.888, 13225.0", 4, store, log);
		assertFalse("1 Digit Lat: ready", reader.ready());
	}
	
	@Test public void badLatitudeFormat() throws IOException {
		state.executeLineCheck("Release Location (lon,lat,alt):    -117 59.76'W, 36 53.26', -117.996, 36.888, 13225.0", 4, store, log);
		assertTrue("Bad Lat Format: ready", reader.ready());
		assertTrue("Bad Lat Format: content", reader.readLine().contains("DM Longitude is not in the format DDD MM.MM'[EW]"));
	}
	
	@Test public void illegalDMLatitudeDegree() throws IOException {
		state.executeLineCheck("Release Location (lon,lat,alt):    117 59.76'W, 90 53.26'N, -117.996, 36.888, 13225.0", 4, store, log);
		assertTrue("Illegal DM Lat Degree: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Illegal DM Lat Degree: type", line.contains("DM latitude"));
		assertTrue("Illegal DM Lat Degree: content", line.contains("not within the valid range of -90 to 90"));
	}
	
	@Test public void illegalDMLatitudeMinute() throws IOException {
		state.executeLineCheck("Release Location (lon,lat,alt):    117 59.76'W, 36 60.26'N, -117.996, 36.888, 13225.0", 4, store, log);
		assertTrue("Illegal DM Lat Minute: ready", reader.ready());
		assertTrue("Illegal DM Lat Minute: content", reader.readLine().contains("ConversionException"));
	}
	
	@Test public void illegalDMLatitudeDirection() throws IOException {
		state.executeLineCheck("Release Location (lon,lat,alt):    117 59.76'W, 36 53.26'W, -117.996, 36.888, 13225.0", 4, store, log);
		assertTrue("Illegal DM Lat Dir: ready", reader.ready());
		assertTrue("Illegal DM Lat Dir: content", reader.readLine().contains("DM Latitude is not in the format"));
	}
	
	@Test public void nonNumericDMLatitudeDegree() throws IOException {
		state.executeLineCheck("Release Location (lon,lat,alt):    117 59.76'W, 3X 53.26'N, -117.996, 36.888, 13225.0", 4, store, log);
		assertTrue("Non Numeric DM Lat Degree: ready", reader.ready());
		assertTrue("Non Numeric DM Lat Degree: content", reader.readLine().contains("DM Latitude is not in the format"));
	}
	
	@Test public void nonNumericDMLatitudeMinute() throws IOException {
		state.executeLineCheck("Release Location (lon,lat,alt):    117 59.76'W, 36 5X.26'N, -117.996, 36.888, 13225.0", 4, store, log);
		assertTrue("Non Numeric DM Lat Minute: ready", reader.ready());
		assertTrue("Non Numeric DM Lat Minute: content", reader.readLine().contains("DM Latitude is not in the format"));
	}

	@Test public void nonNumericDegLatitude() throws IOException {
		state.executeLineCheck("Release Location (lon,lat,alt):    117 59.76'W, 36 53.26'N, -117.996, X6.888, 13225.0", 4, store, log);
		assertTrue("Non Numeric Deg Lat: ready", reader.ready());
		assertTrue("Non Numeric Deg Lat: content", reader.readLine().contains("Unable to convert the latitude value"));
	}
	
	@Test public void latitudeValuesDontMatch() throws IOException {
		state.executeLineCheck("Release Location (lon,lat,alt):    117 59.76'W, 36 53.26'N, -117.996, 36.889, 13225.0", 4, store, log);
		assertTrue("Lat Don't Match: ready", reader.ready());
		assertTrue("Lat Don't Match: content", reader.readLine().contains("The two latitude values do not match"));
	}
	
	@Test public void nonNumericElevation() throws IOException {
		state.executeLineCheck("Release Location (lon,lat,alt):    117 59.76'W, 36 53.26'N, -117.996, 36.888, 1X225.0", 4, store, log);
		assertTrue("Non Numeric Alt: ready", reader.ready());
		assertTrue("Non Numeric Alt: content", reader.readLine().contains("Unable to convert the elevation value"));
	}
}
