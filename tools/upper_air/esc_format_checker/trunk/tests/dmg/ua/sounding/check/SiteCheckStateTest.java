package dmg.ua.sounding.check;

import static org.junit.Assert.*;

import java.io.*;
import org.junit.*;

public class SiteCheckStateTest {

	private SiteCheckState state;
	private DataStore store;
	
	private BufferedReader reader;
	private PrintWriter log;
	
	@Before public void setUp() throws IOException {
    	PipedInputStream in = new PipedInputStream();
    	PipedOutputStream out = new PipedOutputStream(in);
    	
    	reader = new BufferedReader(new InputStreamReader(in));
    	log = new PrintWriter(out);
	
		state = new SiteCheckState(reader, "", 3);
		store = new DataStore();
	}
	
	@Test public void correctDataTypeLine() throws IOException {
		state.executeLineCheck("Release Site Type/Site ID:         Site Name", 3, store, log);
		assertFalse("Correct Release Site Line: ready", reader.ready());
	}
	
	@Test public void incorrectLabel() throws IOException {
		state.executeLineCheck("Data TYPE:                         Site Name", 3, store, log);
		assertTrue("Incorrect Label: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Incorrect Label: site label", line.contains("'Release Site' label"));
		assertTrue("Incorrect Label: expected output", line.contains("Release Site Type/Site ID:"));
	}
	
	@Test public void missingColon() throws IOException {
		state.executeLineCheck("Release Site Type/Site ID          Site Name", 3, store, log);
		assertTrue("Missing Colon: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Missing Colon: site label", line.contains("'Release Site' label"));
		assertTrue("Missing Colon: expected output", line.contains("Release Site Type/Site ID:"));
	}

	@Test public void noPaddingAfterLabel() throws IOException {
		state.executeLineCheck("Release Site Type/Site ID:Site Name", 3, store, log);
		assertTrue("No Padding: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("No Padding: site label", line.contains("'Release Site' label"));
		assertTrue("No Padding: expected output", line.contains("Release Site Type/Site ID:"));
	}

	@Test public void tooLittlePaddingAfterLabel() throws IOException {
		state.executeLineCheck("Release Site Type/Site ID:        Site Name", 3, store, log);
		assertTrue("Too Little Padding: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Too Little Padding: site label", line.contains("'Release Site' label"));
		assertTrue("Too Little Padding: expected output", line.contains("Release Site Type/Site ID:"));
	}

	@Test public void tooMuchPaddingAfterLabel() throws IOException {
		state.executeLineCheck("Release Site Type/Site ID:          Site Name", 3, store, log);
		assertTrue("Too Much Padding: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Too Much Padding: site", line.contains("'Release Site'"));
		assertTrue("Too Much Padding: expected output", line.contains("does not have the text after the label begin in the correct spot"));
	}
	
	@Test public void noTextAfterLabel() throws IOException {
		state.executeLineCheck("Release Site Type/Site ID:          ", 3, store, log);
		assertTrue("No Text: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("No Text: site label", line.contains("'Release Site'"));
		assertTrue("No Text: expected output", line.contains("does not have any text after the label"));
	}
	
	@Test public void trailingWhiteSpace() throws IOException {
		state.executeLineCheck("Release Site Type/Site ID:         Site Name ", 3, store, log);
		assertTrue("Trailing Whitespace: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Trailing Whitespace: site", line.contains("'Release Site'"));
		assertTrue("Trailing Whitespace: extra whitespace", line.contains("extra whitespace at the end of the line"));				
	}

	@Test public void lineNumberCorrect() throws IOException {
		state.executeLineNumberCheck(3, log);
		assertFalse("Correct Line Number: ready", reader.ready());
	}
	
	@Test public void lineNumberTooSmall() throws IOException {
		state.executeLineNumberCheck(2, log);
		assertTrue("Too Small Line Number: ready", reader.ready());
		assertTrue("Too Small Line Number: log read", reader.readLine().contains(" expected on line 3"));
	}
	
	@Test public void lineNumberTooBig() throws IOException {
		state.executeLineNumberCheck(4, log);
		assertTrue("Too Big Line Number: ready", reader.ready());
		assertTrue("Too Big Line Number: log read", reader.readLine().contains(" expected on line 3"));
	}
}