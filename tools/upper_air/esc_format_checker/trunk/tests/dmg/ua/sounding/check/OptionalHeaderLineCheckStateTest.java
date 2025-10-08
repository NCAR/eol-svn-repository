package dmg.ua.sounding.check;

import static org.junit.Assert.*;

import java.io.*;
import org.junit.*;

public class OptionalHeaderLineCheckStateTest {

	private OptionalHeaderLineCheckState state;
	private DataStore store;
	
	private BufferedReader reader;
	private PrintWriter log;
	
	@Before public void setUp() throws IOException {
    	PipedInputStream in = new PipedInputStream();
    	PipedOutputStream out = new PipedOutputStream(in);
    	
    	reader = new BufferedReader(new InputStreamReader(in));
    	log = new PrintWriter(out);
	
		state = new OptionalHeaderLineCheckState(reader, "", 6);
		store = new DataStore();
	}
	
	@Test public void correctHeaderLine() throws IOException {
		state.executeLineCheck("Sonde Type:                        Sounding Type", 6, store, log);
		assertFalse("Correct Header Line: ready", reader.ready());
	}
	
	@Test public void correctFillerLine() throws IOException {
		state.executeLineCheck("/", 6, store, log);
		assertFalse("Correct Filler Line: ready", reader.ready());
	}
	
	@Test public void invalidFillerLine() throws IOException {
		state.executeLineCheck("/ ", 6, store, log);
		assertTrue("Invalid Filler Line: reader", reader.ready());
		assertTrue("Invalid Filler Line: content", reader.readLine().contains("The 'Filler' header line has more than just the '/'"));
	}
	
	@Test public void invalidLabel() throws IOException {
		state.executeLineCheck("Extra:Colon:                       Sounding Type", 6, store, log);
		assertTrue("Invalid Label: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Invalid Label: expected output", line.contains("The 'Optional' header line has an illegal label"));
	}
	
	@Test public void missingColon() throws IOException {
		state.executeLineCheck("No Colon                           Sounding Type", 6, store, log);
		assertTrue("Missing Colon: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Missing Colon: expected output", line.contains("The 'Optional' header line has an illegal label"));
	}

	@Test public void noPaddingAfterLabel() throws IOException {
		state.executeLineCheck("Data Type:Sounding Type", 6, store, log);
		assertTrue("No Padding: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("No Padding: optional label", line.contains("The 'Optional' header line has a format problem"));
	}

	@Test public void tooLittlePaddingAfterLabel() throws IOException {
		state.executeLineCheck("Data Type:                        Sounding Type", 6, store, log);
		assertTrue("Too Little Padding: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Too Little Padding: optional label", line.contains("'Optional'"));
		assertTrue("Too Little Padding: expected output", line.contains("Data Type:"));
	}

	@Test public void tooMuchPaddingAfterLabel() throws IOException {
		state.executeLineCheck("Data Type:                          Sounding Type", 6, store, log);
		assertTrue("Too Much Padding: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Too Much Padding: optional", line.contains("'Optional'"));
		assertTrue("Too Much Padding: expected output", line.contains("does not have the text after the label begin in the correct spot"));
	}
	
	@Test public void noTextAfterLabel() throws IOException {
		state.executeLineCheck("Data Type:                          ", 6, store, log);
		assertTrue("No Text: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("No Text: optional label", line.contains("'Optional'"));
		assertTrue("No Text: expected output", line.contains("does not have any text after the label"));
	}
	
	@Test public void trailingWhiteSpace() throws IOException {
		state.executeLineCheck("Data Type:                         Sounding Type ", 6, store, log);
		assertTrue("Trailing Whitespace: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Trailing Whitespace: optional", line.contains("'Optional'"));
		assertTrue("Trailing Whitespace: extra whitespace", line.contains("extra whitespace at the end of the line"));				
	}

	@Test public void lineNumberCorrect() throws IOException {
		state.executeLineNumberCheck(6, log);
		assertFalse("Correct Line Number: ready", reader.ready());
	}
	
	@Test public void lineNumberTooSmall() throws IOException {
		state.executeLineNumberCheck(5, log);
		assertTrue("Too Small Line Number: ready", reader.ready());
		assertTrue("Too Small Line Number: log read", reader.readLine().contains(" expected on lines 6-11"));
	}
	
	@Test public void lineNumberTooBig() throws IOException {
		state.executeLineNumberCheck(12, log);
		assertTrue("Too Big Line Number: ready", reader.ready());
		assertTrue("Too Big Line Number: log read", reader.readLine().contains(" expected on lines 6-11"));
	}
}
