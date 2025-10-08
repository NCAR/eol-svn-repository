package dmg.ua.sounding.check;

import static org.junit.Assert.*;

import java.io.*;
import org.junit.*;

public class DataTypeCheckStateTest {

	private DataTypeCheckState state;
	private DataStore store;
	
	private BufferedReader reader;
	private PrintWriter log;
	
	@Before public void setUp() throws IOException {
    	PipedInputStream in = new PipedInputStream();
    	PipedOutputStream out = new PipedOutputStream(in);
    	
    	reader = new BufferedReader(new InputStreamReader(in));
    	log = new PrintWriter(out);
	
		state = new DataTypeCheckState(reader, "", 1);
		store = new DataStore();
	}
	
	@Test public void correctDataTypeLine() throws IOException {
		state.executeLineCheck("Data Type:                         Sounding Type", 5, store, log);
		assertFalse("Correct Data Type Line: ready", reader.ready());
	}
	
	@Test public void incorrectLabel() throws IOException {
		state.executeLineCheck("Data TYPE:                         Sounding Type", 5, store, log);
		assertTrue("Incorrect Label: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Incorrect Label: data type label", line.contains("'Data Type' label"));
		assertTrue("Incorrect Label: expected output", line.contains("Data Type:"));
	}
	
	@Test public void missingColon() throws IOException {
		state.executeLineCheck("Data Type                          Sounding Type", 5, store, log);
		assertTrue("Missing Colon: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Missing Colon: data type label", line.contains("'Data Type' label"));
		assertTrue("Missing Colon: expected output", line.contains("Data Type:"));
	}

	@Test public void noPaddingAfterLabel() throws IOException {
		state.executeLineCheck("Data Type:Sounding Type", 5, store, log);
		assertTrue("No Padding: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("No Padding: data type label", line.contains("'Data Type' label"));
		assertTrue("No Padding: expected output", line.contains("Data Type:"));
	}

	@Test public void tooLittlePaddingAfterLabel() throws IOException {
		state.executeLineCheck("Data Type:                        Sounding Type", 5, store, log);
		assertTrue("Too Little Padding: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Too Little Padding: data type label", line.contains("'Data Type' label"));
		assertTrue("Too Little Padding: expected output", line.contains("Data Type:"));
	}

	@Test public void tooMuchPaddingAfterLabel() throws IOException {
		state.executeLineCheck("Data Type:                          Sounding Type", 5, store, log);
		assertTrue("Too Much Padding: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Too Much Padding: data type", line.contains("'Data Type'"));
		assertTrue("Too Much Padding: expected output", line.contains("does not have the text after the label begin in the correct spot"));
	}
	
	@Test public void noTextAfterLabel() throws IOException {
		state.executeLineCheck("Data Type:                          ", 5, store, log);
		assertTrue("No Text: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("No Text: data type label", line.contains("'Data Type'"));
		assertTrue("No Text: expected output", line.contains("does not have any text after the label"));
	}
	
	@Test public void trailingWhiteSpace() throws IOException {
		state.executeLineCheck("Data Type:                         Sounding Type ", 5, store, log);
		assertTrue("Trailing Whitespace: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Trailing Whitespace: data type", line.contains("'Data Type'"));
		assertTrue("Trailing Whitespace: extra whitespace", line.contains("extra whitespace at the end of the line"));				
	}

	@Test public void lineNumberCorrect() throws IOException {
		state.executeLineNumberCheck(1, log);
		assertFalse("Correct Line Number: ready", reader.ready());
	}
	
	@Test public void lineNumberTooSmall() throws IOException {
		state.executeLineNumberCheck(0, log);
		assertTrue("Too Small Line Number: ready", reader.ready());
		assertTrue("Too Small Line Number: log read", reader.readLine().contains(" expected on line 1"));
	}
	
	@Test public void lineNumberTooBig() throws IOException {
		state.executeLineNumberCheck(2, log);
		assertTrue("Too Big Line Number: ready", reader.ready());
		assertTrue("Too Big Line Number: log read", reader.readLine().contains(" expected on line 1"));
	}
}
