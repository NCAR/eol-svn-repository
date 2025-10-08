package dmg.ua.sounding.check;

import static org.junit.Assert.*;

import java.io.*;
import org.junit.*;

public class UnitsCheckStateTest {

	private UnitsCheckState state;
	
	private BufferedReader reader;
	private PrintWriter log;
	
	@Before public void setUp() throws IOException {
    	PipedInputStream in = new PipedInputStream();
    	PipedOutputStream out = new PipedOutputStream(in);
    	
    	reader = new BufferedReader(new InputStreamReader(in));
    	log = new PrintWriter(out);
	
		state = new UnitsCheckState(reader, "", 14);		
	}
	
	
	//"  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code"
	
	@Test public void correctUnitLine() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code", 14, null, log);
		assertFalse("Correct Unit Line: ready", reader.ready());
	}
	
	@Test public void lineTooLong() throws IOException {
		state.executeLineCheck("   sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code", 14, null, log);
		assertTrue("Line Too Long: ready", reader.ready());
		assertTrue("Line Too Long: read log", reader.readLine().contains("130 characters long"));
	}
	
	@Test public void lineTooShort() throws IOException {
		state.executeLineCheck(" sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code", 14, null, log);
		assertTrue("Line Too Short: ready", reader.ready());
		assertTrue("Line Too Short: read log", reader.readLine().contains("130 characters long"));
	}
	
	@Test public void badTimeUnit() throws IOException {
		state.executeLineCheck("  Sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code", 14, null, log);
		assertTrue("Bad Time: ready", reader.ready());
		assertTrue("Bad Time: read log", reader.readLine().contains("'sec'"));
	}
	
	@Test public void noSpaceAt6() throws IOException {
		state.executeLineCheck("  sec x  mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code", 14, null, log);
		assertTrue("No Space @ 6: ready", reader.ready());
		assertTrue("No Space @ 6: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void badPressureUnit() throws IOException {
		state.executeLineCheck("  sec    MB     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code", 14, null, log);
		assertTrue("Bad Press: ready", reader.ready());
		assertTrue("Bad Press: read log", reader.readLine().contains("'mb'"));
	}
	
	@Test public void noSpaceAt13() throws IOException {
		state.executeLineCheck("  sec    mb  x  C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code", 14, null, log);
		assertTrue("No Space @ 13: ready", reader.ready());
		assertTrue("No Space @ 13: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void badTemperatureUnit() throws IOException {
		state.executeLineCheck("  sec    mb     c     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code", 14, null, log);
		assertTrue("Bad Temp: ready", reader.ready());
		assertTrue("Bad Temp: read log", reader.readLine().contains("'C'"));
	}
	
	@Test public void noSpaceAt19() throws IOException {
		state.executeLineCheck("  sec    mb     C  x  C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code", 14, null, log);
		assertTrue("No Space @ 19: ready", reader.ready());
		assertTrue("No Space @ 19: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void badDewPointUnit() throws IOException {
		state.executeLineCheck("  sec    mb     C     c     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code", 14, null, log);
		assertTrue("Bad Dewpt: ready", reader.ready());
		assertTrue("Bad Dewpt: read log", reader.readLine().contains("'C'"));
	}
	
	@Test public void noSpaceAt25() throws IOException {
		state.executeLineCheck("  sec    mb     C     C  x  %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code", 14, null, log);
		assertTrue("No Space @ 25: ready", reader.ready());
		assertTrue("No Space @ 25: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void badRelativeHumidityUnit() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     P     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code", 14, null, log);
		assertTrue("Bad RH: ready", reader.ready());
		assertTrue("Bad RH: read log", reader.readLine().contains("'%'"));
	}
	
	@Test public void noSpaceAt31() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %  x  m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code", 14, null, log);
		assertTrue("No Space @ 31: ready", reader.ready());
		assertTrue("No Space @ 31: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void badUComponentUnit() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     mps    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code", 14, null, log);
		assertTrue("Bad Ucmp: ready", reader.ready());
		assertTrue("Bad Ucmp: read log", reader.readLine().contains("m/s"));
	}
	
	@Test public void noSpaceAt38() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s x  m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code", 14, null, log);
		assertTrue("No Space @ 38: ready", reader.ready());
		assertTrue("No Space @ 38: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void badVComponentUnit() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    mps   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code", 14, null, log);
		assertTrue("Bad VCmp: ready", reader.ready());
		assertTrue("Bad VCmp: read log", reader.readLine().contains("'m/s'"));
	}
	
	@Test public void noSpaceAt45() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s x m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code", 14, null, log);
		assertTrue("No Space @ 45: ready", reader.ready());
		assertTrue("No Space @ 45: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void badWindSpeedUnit() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s   mps   deg   m/s      deg     deg   deg   deg     m    code code code code code code", 14, null, log);
		assertTrue("Bad spd: ready", reader.ready());
		assertTrue("Bad spd: read log", reader.readLine().contains("'m/s'"));
	}
	
	@Test public void noSpaceAt51() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s   m/s x deg   m/s      deg     deg   deg   deg     m    code code code code code code", 14, null, log);
		assertTrue("No Space @ 51: ready", reader.ready());
		assertTrue("No Space @ 51: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void badWindDirectionUnit() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s   m/s   Deg   m/s      deg     deg   deg   deg     m    code code code code code code", 14, null, log);
		assertTrue("Bad dir: ready", reader.ready());
		assertTrue("Bad dir: read log", reader.readLine().contains("'deg'"));
	}
	
	@Test public void noSpaceAt57() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s   m/s   deg x m/s      deg     deg   deg   deg     m    code code code code code code", 14, null, log);
		assertTrue("No Space @ 57: ready", reader.ready());
		assertTrue("No Space @ 57: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void badAscentRateUnit() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s   m/s   deg   mps      deg     deg   deg   deg     m    code code code code code code", 14, null, log);
		assertTrue("Bad Wcmp: ready", reader.ready());
		assertTrue("Bad Wcmp: read log", reader.readLine().contains("'m/s'"));
	}
	
	@Test public void noSpaceAt63() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s x    deg     deg   deg   deg     m    code code code code code code", 14, null, log);
		assertTrue("No Space @ 63: ready", reader.ready());
		assertTrue("No Space @ 63: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void badLongitudeUnit() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      Deg     deg   deg   deg     m    code code code code code code", 14, null, log);
		assertTrue("Bad Lon: ready", reader.ready());
		assertTrue("Bad Lon: read log", reader.readLine().contains("'deg'"));
	}
	
	@Test public void noSpaceAt72() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg x   deg   deg   deg     m    code code code code code code", 14, null, log);
		assertTrue("No Space @ 72: ready", reader.ready());
		assertTrue("No Space @ 72: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void badLatitudeUnit() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     Deg   deg   deg     m    code code code code code code", 14, null, log);
		assertTrue("Bad Lat: ready", reader.ready());
		assertTrue("Bad Lat: read log", reader.readLine().contains("'deg'"));
	}
	
	@Test public void noSpaceAt80() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg x deg   deg     m    code code code code code code", 14, null, log);
		assertTrue("No Space @ 80: ready", reader.ready());
		assertTrue("No Space @ 80: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void badVariable1Unit() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg         deg     m    code code code code code code", 14, null, log);
		assertTrue("Bad Var 1: ready", reader.ready());
		assertTrue("Bad Var 1: read log", reader.readLine().contains("first variable unit"));
	}
	
	@Test public void noSpaceAt86() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg x deg     m    code code code code code code", 14, null, log);
		assertTrue("No Space @ 86: ready", reader.ready());
		assertTrue("No Space @ 86: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void badVariable2Unit() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg           m    code code code code code code", 14, null, log);
		assertTrue("Bad Var 2: ready", reader.ready());
		assertTrue("Bad Var 2: read log", reader.readLine().contains("second variable unit"));
	}
	
	@Test public void noSpaceAt92() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg x   m    code code code code code code", 14, null, log);
		assertTrue("No Space @ 92: ready", reader.ready());
		assertTrue("No Space @ 92: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void badAltitudeUnit() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     M    code code code code code code", 14, null, log);
		assertTrue("Bad Alt: ready", reader.ready());
		assertTrue("Bad Alt: read log", reader.readLine().contains("'m'"));
	}
	
	@Test public void noSpaceAt100() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m   xcode code code code code code", 14, null, log);
		assertTrue("No Space @ 100: ready", reader.ready());
		assertTrue("No Space @ 100: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void badPressureCodeUnit() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    Code code code code code code", 14, null, log);
		assertTrue("Bad Qp: ready", reader.ready());
		assertTrue("Bad Qp: read log", reader.readLine().contains("'code'"));
	}
	
	@Test public void noSpaceAt105() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    codeXcode code code code code", 14, null, log);
		assertTrue("No Space @ 105: ready", reader.ready());
		assertTrue("No Space @ 105: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void badTemperatureCodeUnit() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code Code code code code code", 14, null, log);
		assertTrue("Bad Qt: ready", reader.ready());
		assertTrue("Bad Qt: read log", reader.readLine().contains("'code'"));
	}
	
	@Test public void noSpaceAt110() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code codeXcode code code code", 14, null, log);
		assertTrue("No Space @ 110: ready", reader.ready());
		assertTrue("No Space @ 110: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void badRelativehHumidityCodeUnit() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code Code code code code", 14, null, log);
		assertTrue("Bad Qrh: ready", reader.ready());
		assertTrue("Bad Qrh: read log", reader.readLine().contains("'code'"));
	}
	
	@Test public void noSpaceAt115() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code codexcode code code", 14, null, log);
		assertTrue("No Space @ 115: ready", reader.ready());
		assertTrue("No Space @ 115: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void badUComponentCodeUnit() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code Code code code", 14, null, log);
		assertTrue("Bad Qu: ready", reader.ready());
		assertTrue("Bad Qu: read log", reader.readLine().contains("'code'"));
	}
	
	@Test public void noSpaceAt120() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code codeXcode code", 14, null, log);
		assertTrue("No Space @ 120: ready", reader.ready());
		assertTrue("No Space @ 120: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void badVComponentCodeUnit() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code Code code", 14, null, log);
		assertTrue("Bad Qv: ready", reader.ready());
		assertTrue("Bad Qv: read log", reader.readLine().contains("'code'"));
	}
	
	@Test public void noSpaceAt125() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code codexcode", 14, null, log);
		assertTrue("No Space @ 125: ready", reader.ready());
		assertTrue("No Space @ 125: read line", reader.readLine().contains(" space "));
	}
	
	@Test public void badAscentRateCodeUnit() throws IOException {
		state.executeLineCheck("  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code Code", 14, null, log);
		assertTrue("Bad QdZ: ready", reader.ready());
		assertTrue("Bad QdZ: read line", reader.readLine().contains("'code'"));
	}
	
	@Test public void lineNumberCorrect() throws IOException {
		state.executeLineNumberCheck(14, log);
		assertFalse("Line Number Correct: ready", reader.ready());
	}
	
	@Test public void lineNumberTooBig() throws IOException {
		state.executeLineNumberCheck(15, log);
		assertTrue("Line Number Too Big: ready", reader.ready());
		assertTrue("Line Number Too Big: read log", reader.readLine().contains(" expected on line 14"));
	}
	
	@Test public void lineNumberTooSmall() throws IOException {
		state.executeLineNumberCheck(13, log);
		assertTrue("Line Number Too Small: ready", reader.ready());
		assertTrue("Line Number Too Small: read log", reader.readLine().contains(" expected on line 14"));
	}
}