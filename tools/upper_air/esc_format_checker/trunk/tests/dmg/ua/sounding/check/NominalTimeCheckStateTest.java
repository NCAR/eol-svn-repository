package dmg.ua.sounding.check;

import static org.junit.Assert.*;

import java.io.*;
import org.junit.*;


public class NominalTimeCheckStateTest {

	private NominalTimeCheckState state;
	private DataStore store;
	
	private BufferedReader reader;
	private PrintWriter log;
	
	@Before public void setUp() throws IOException {
    	PipedInputStream in = new PipedInputStream();
    	PipedOutputStream out = new PipedOutputStream(in);
    	
    	reader = new BufferedReader(new InputStreamReader(in));
    	log = new PrintWriter(out);
	
		state = new NominalTimeCheckState(reader, "", 12);
		store = new DataStore();
	}
	
	@Test public void correctNominalTimeLine() throws IOException {
		state.executeLineCheck("Nominal Release Time (y,m,d,h,m,s):2007, 07, 09, 13:31:54", 12, store, log);
		assertFalse("Correct Nominal Time Line: ready", reader.ready());
	}
	
	@Test public void incorrectLabel() throws IOException {
		state.executeLineCheck("UTC Release Time (y,m,d,h,m,s):    2007, 07, 09, 13:31:54", 12, store, log);
		assertTrue("Incorrect Label: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Incorrect Label: nominal release label", line.contains("'Nominal Release' label"));
		assertTrue("Incorrect Label: expected output", line.contains("Nominal Release Time (y,m,d,h,m,s):"));
	}
	
	@Test public void missingColon() throws IOException {
		state.executeLineCheck("Nominal Release Time (y,m,d,h,m,s) 2007, 07, 09, 13:31:54", 12, store, log);
		assertTrue("Missing Colon: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Missing Colon: nominal release label", line.contains("'Nominal Release' label"));
		assertTrue("Missing Colon: expected output", line.contains("Nominal Release Time (y,m,d,h,m,s):"));
	}

	@Test public void tooMuchPaddingAfterLabel() throws IOException {
		state.executeLineCheck("Nominal Release Time (y,m,d,h,m,s): 2007, 07, 09, 13:31:54", 12, store, log);
		assertTrue("Too Much Padding: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Too Much Padding: nominal release", line.contains("'Nominal Release'"));
		assertTrue("Too Much Padding: invalid date", line.contains("does not have a valid date"));
	}
	
	@Test public void twoDigitYear() throws IOException {
		state.executeLineCheck("Nominal Release Time (y,m,d,h,m,s):07, 07, 09, 13:31:54", 12, store, log);
		assertTrue("2 Digit Year: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("2 Digit Year: nominal release", line.contains("'Nominal Release'"));
		assertTrue("2 Digit Year: invalid date", line.contains("does not have a valid date"));
	}

	@Test public void nonNumericYear() throws IOException {
		state.executeLineCheck("Nominal Release Time (y,m,d,h,m,s):2x07, 07, 09, 13:31:54", 12, store, log);
		assertTrue("Non-Numeric Year: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Non-Numeric Year: nominal release", line.contains("'Nominal Release'"));
		assertTrue("Non-Numeric Year: invalid date", line.contains("does not have a valid date"));		
	}
	
	@Test public void oneDigitMonth() throws IOException {
		state.executeLineCheck("Nominal Release Time (y,m,d,h,m,s):2007, 7, 09, 13:31:54", 12, store, log);
		assertTrue("1 Digit Month: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("1 Digit Month: nominal release", line.contains("'Nominal Release'"));
		assertTrue("1 Digit Month: invalid date", line.contains("does not have a valid date"));		
	}
	
	@Test public void illegalMonth() throws IOException {
		state.executeLineCheck("Nominal Release Time (y,m,d,h,m,s):2007, 13, 09, 13:31:54", 12, store, log);
		assertTrue("Illegal Month: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Illegal Month: nominal release", line.contains("'Nominal Release'"));
		assertTrue("Illegal Month: invalid date", line.contains("does not have a valid date"));				
	}

	@Test public void nonNumericMonth() throws IOException {
		state.executeLineCheck("Nominal Release Time (y,m,d,h,m,s):2007, 0x, 09, 13:31:54", 12, store, log);
		assertTrue("Non-Numeric Month: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Non-Numeric Month: nominal release", line.contains("'Nominal Release'"));
		assertTrue("Non-Numeric Month: invalid date", line.contains("does not have a valid date"));		
	}
	
	@Test public void oneDigitDay() throws IOException {
		state.executeLineCheck("Nominal Release Time (y,m,d,h,m,s):2007, 07, 9, 13:31:54", 12, store, log);
		assertTrue("1 Digit Day: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("1 Digit Day: nominal release", line.contains("'Nominal Release'"));
		assertTrue("1 Digit Day: invalid date", line.contains("does not have a valid date"));				
	}
	
	@Test public void illegalDay() throws IOException {
		state.executeLineCheck("Nominal Release Time (y,m,d,h,m,s):2007, 07, 00, 13:31:54", 12, store, log);
		assertTrue("Illegal Day: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Illegal Day: nominal release", line.contains("'Nominal Release'"));
		assertTrue("Illegal Day: invalid date", line.contains("does not have a valid date"));				
	}

	@Test public void nonNumericDay() throws IOException {
		state.executeLineCheck("Nominal Release Time (y,m,d,h,m,s):2007, 07, 0x, 13:31:54", 12, store, log);
		assertTrue("Non-Numeric Day: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Non-Numeric Day: nominal release", line.contains("'Nominal Release'"));
		assertTrue("Non-Numeric Day: invalid date", line.contains("does not have a valid date"));		
	}
	
	@Test public void oneDigitHour() throws IOException {
		state.executeLineCheck("Nominal Release Time (y,m,d,h,m,s):2007, 07, 9, 1:31:54", 12, store, log);
		assertTrue("1 Digit Hour: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("1 Digit Hour: nominal release", line.contains("'Nominal Release'"));
		assertTrue("1 Digit Hour: invalid date", line.contains("does not have a valid date"));				
	}
	
	@Test public void illegalHour() throws IOException {
		state.executeLineCheck("Nominal Release Time (y,m,d,h,m,s):2007, 07, 00, 24:31:54", 12, store, log);
		assertTrue("Illegal Hour: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Illegal Hour: nominal release", line.contains("'Nominal Release'"));
		assertTrue("Illegal Hour: invalid date", line.contains("does not have a valid date"));				
	}

	@Test public void nonNumericHour() throws IOException {
		state.executeLineCheck("Nominal Release Time (y,m,d,h,m,s):2007, 07, 09, x3:31:54", 12, store, log);
		assertTrue("Non-Numeric Hour: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Non-Numeric Hour: nominal release", line.contains("'Nominal Release'"));
		assertTrue("Non-Numeric Hour: invalid date", line.contains("does not have a valid date"));		
	}
	
	@Test public void oneDigitMinute() throws IOException {
		state.executeLineCheck("Nominal Release Time (y,m,d,h,m,s):2007, 07, 9, 13:3:54", 12, store, log);
		assertTrue("1 Digit Minute: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("1 Digit Minute: nominal release", line.contains("'Nominal Release'"));
		assertTrue("1 Digit Minute: invalid date", line.contains("does not have a valid date"));				
	}
	
	@Test public void illegalMinute() throws IOException {
		state.executeLineCheck("Nominal Release Time (y,m,d,h,m,s):2007, 07, 00, 13:60:54", 12, store, log);
		assertTrue("Illegal Minute: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Illegal Minute: nominal release", line.contains("'Nominal Release'"));
		assertTrue("Illegal Minute: invalid date", line.contains("does not have a valid date"));				
	}

	@Test public void nonNumericMinute() throws IOException {
		state.executeLineCheck("Nominal Release Time (y,m,d,h,m,s):2007, 07, 09, 13:3x:54", 12, store, log);
		assertTrue("Non-Numeric Minute: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Non-Numeric Minute: nominal release", line.contains("'Nominal Release'"));
		assertTrue("Non-Numeric Minute: invalid date", line.contains("does not have a valid date"));		
	}
	
	@Test public void oneDigitSecond() throws IOException {
		state.executeLineCheck("Nominal Release Time (y,m,d,h,m,s):2007, 07, 9, 13:31:5", 12, store, log);
		assertTrue("1 Digit SEcond: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("1 Digit Second: nominal release", line.contains("'Nominal Release'"));
		assertTrue("1 Digit Second: invalid date", line.contains("does not have a valid date"));				
	}
	
	@Test public void illegalSecond() throws IOException {
		state.executeLineCheck("Nominal Release Time (y,m,d,h,m,s):2007, 07, 00, 13:31:60", 12, store, log);
		assertTrue("Illegal Second: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Illegal Second: nominal release", line.contains("'Nominal Release'"));
		assertTrue("Illegal Second: invalid date", line.contains("does not have a valid date"));				
	}

	@Test public void nonNumericSecond() throws IOException {
		state.executeLineCheck("Nominal Release Time (y,m,d,h,m,s):2007, 07, 09, 13:31:5x", 12, store, log);
		assertTrue("Non-Numeric Second: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Non-Numeric Second: nominal release", line.contains("'Nominal Release'"));
		assertTrue("Non-Numeric Second: invalid date", line.contains("does not have a valid date"));		
	}
	
	@Test public void illegalYearMonthSeparator() throws IOException {
		state.executeLineCheck("Nominal Release Time (y,m,d,h,m,s):2007/07, 09, 13:31:53", 12, store, log);
		assertTrue("Illegal Year-Month Separator: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Illegal Year-Month Separator: nominal release", line.contains("'Nominal Release'"));
		assertTrue("Illegal Year-Month Separator: invalid date", line.contains("does not have a valid date"));				
	}
	
	@Test public void illegalMonthDaySeparator() throws IOException {
		state.executeLineCheck("Nominal Release Time (y,m,d,h,m,s):2007, 07/09, 13:31:53", 12, store, log);
		assertTrue("Illegal Month-Day Separator: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Illegal Month-Day Separator: nominal release", line.contains("'Nominal Release'"));
		assertTrue("Illegal Month-Day Separator: invalid date", line.contains("does not have a valid date"));				
	}
	
	@Test public void illegalDayHourSeparator() throws IOException {
		state.executeLineCheck("Nominal Release Time (y,m,d,h,m,s):2007, 07, 09 13:31:53", 12, store, log);
		assertTrue("Illegal Day-Hour Separator: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Illegal Day-Hour Separator: nominal release", line.contains("'Nominal Release'"));
		assertTrue("Illegal Day-Hour Separator: invalid date", line.contains("does not have a valid date"));				
	}
	
	@Test public void illegalHourMinuteSeparator() throws IOException {
		state.executeLineCheck("Nominal Release Time (y,m,d,h,m,s):2007, 07, 09, 13 31:53", 12, store, log);
		assertTrue("Illegal Hour-Minute Separator: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Illegal Hour-Minute Separator: nominal release", line.contains("'Nominal Release'"));
		assertTrue("Illegal Hour-Minute Separator: invalid date", line.contains("does not have a valid date"));				
	}
	
	@Test public void illegalMinuteSecondSeparator() throws IOException {
		state.executeLineCheck("Nominal Release Time (y,m,d,h,m,s):2007, 07, 09, 13:31 53", 12, store, log);
		assertTrue("Illegal Minute-Second Separator: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Illegal Minute-Second Separator: nominal release", line.contains("'Nominal Release'"));
		assertTrue("Illegal Minute-Second Separator: invalid date", line.contains("does not have a valid date"));				
	}
	
	@Test public void trailingWhiteSpace() throws IOException {
		state.executeLineCheck("Nominal Release Time (y,m,d,h,m,s):2007, 07, 09, 13:31:53 ", 12, store, log);
		assertTrue("Trailing Whitespace: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Trailing Whitespace: nominal release", line.contains("'Nominal Release'"));
		assertTrue("Trailing Whitespace: invalid date", line.contains("does not have a valid date"));				
	}

	@Test public void lineNumberCorrect() throws IOException {
		state.executeLineNumberCheck(12, log);
		assertFalse("Correct Line Number: ready", reader.ready());
	}
	
	@Test public void lineNumberTooSmall() throws IOException {
		state.executeLineNumberCheck(11, log);
		assertTrue("Too Small Line Number: ready", reader.ready());
		assertTrue("Too Small Line Number: log read", reader.readLine().contains(" expected on line 12"));
	}
	
	@Test public void lineNumberTooBig() throws IOException {
		state.executeLineNumberCheck(13, log);
		assertTrue("Too Big Line Number: ready", reader.ready());
		assertTrue("Too Big Line Number: log read", reader.readLine().contains(" expected on line 12"));
	}
}
