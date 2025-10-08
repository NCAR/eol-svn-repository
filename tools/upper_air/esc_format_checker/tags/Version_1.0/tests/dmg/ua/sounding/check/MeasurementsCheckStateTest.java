package dmg.ua.sounding.check;

import static org.junit.Assert.*;

import java.io.*;
import org.junit.*;

public class MeasurementsCheckStateTest {

	private MeasurementsCheckState state;
	
	private BufferedReader reader;
	private PrintWriter log;
	
	@Before public void setUp() throws IOException {
    	PipedInputStream in = new PipedInputStream();
    	PipedOutputStream out = new PipedOutputStream(in);
    	
    	reader = new BufferedReader(new InputStreamReader(in));
    	log = new PrintWriter(out);
	
		state = new MeasurementsCheckState(reader, "", 12);		
	}
	
	@Test public void correctMeasurmentLine() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertFalse("Correct Measurement Line: ready", reader.ready());
	}
	
	@Test public void lineTooLong() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv    QdZ", 12, null, log);
		assertTrue("Line Too Long: ready", reader.ready());
		assertTrue("Line Too Long: log read", reader.readLine().contains(" 130 characters "));
	}

	@Test public void lineTooShort() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv  QdZ", 12, null, log);
		assertTrue("Line Too Short: ready", reader.ready());
		assertTrue("Line Too Short: log read", reader.readLine().contains(" 130 characters "));
	}
	
	@Test public void badTimeSection() throws IOException {
		state.executeLineCheck(" time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("Bad Time: ready", reader.ready());
		assertTrue("Bad Time: log read", reader.readLine().contains("'Time'"));
	}
	
	@Test public void noSpaceAtCharacter6() throws IOException {
		state.executeLineCheck(" Time xPress  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("No Space @ 6: ready", reader.ready());
		assertTrue("No Space @ 6: log read", reader.readLine().contains(" space "));		
	}
	
	@Test public void badPressureSection() throws IOException {
		state.executeLineCheck(" Time  press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("Bad Press: ready", reader.ready());
		assertTrue("Bad Press: log read", reader.readLine().contains("'Press'"));		
	}
	
	@Test public void noSpaceAtCharacter13() throws IOException {
		state.executeLineCheck(" Time  Press xTemp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("No Space @ 13: ready", reader.ready());
		assertTrue("No Space @ 13: log ready", reader.readLine().contains(" space "));
	}
	
	@Test public void badTemperatureSection() throws IOException {
		state.executeLineCheck(" Time  Press  temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("Bad Temp: ready", reader.ready());
		assertTrue("Bad Temp: log read", reader.readLine().contains("'Temp'"));
	}
	
	@Test public void noSpaceAtCharacter19() throws IOException {
		state.executeLineCheck(" Time  Press  Temp xDewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("No Space @ 19: ready", reader.ready());
		assertTrue("No Space @ 19: log read", reader.readLine().contains(" space "));
	}
	
	@Test public void badDewPointSection() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("Bad Dewpt: ready", reader.ready());
		assertTrue("Bad Dewpt: log read", reader.readLine().contains("'Dewpt'"));
	}
	
	@Test public void noSpaceAtCharacter25() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewptx RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("No Space @ 25: ready", reader.ready());
		assertTrue("No Space @ 25: log read", reader.readLine().contains(" space "));
	}
	
	@Test public void badRelativeHumiditySection() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  Rh    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("Bad RH: ready", reader.ready());
		assertTrue("Bad RH: log read", reader.readLine().contains("'RH'"));
	}
	
	@Test public void noSpaceAtCharacter31() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH  x Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("No Space @ 31: ready", reader.ready());
		assertTrue("No Space @ 31: log read", reader.readLine().contains(" space "));
	}
	
	@Test public void badUComponentSection() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("Bad Ucmp: ready", reader.ready());
		assertTrue("Bad Ucmp: log read", reader.readLine().contains("'Ucmp'"));
	}
	
	@Test public void noSpaceAtCharacter38() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp x Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("No Space @ 38: ready", reader.ready());
		assertTrue("No Space @ 38: log read", reader.readLine().contains(" space "));
	}
	
	@Test public void badVComponentSection() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("Bad Vcmp: ready", reader.ready());
		assertTrue("Bad Vcmp: log read", reader.readLine().contains("'Vcmp'"));
	}
	
	@Test public void noSpaceAtCharacter45() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp x Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("No Space @ 45: ready", reader.ready());
		assertTrue("No Space @ 45: log read", reader.readLine().contains(" space "));
	}
	
	@Test public void badWindSpeedSection() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   Spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("Bad spd: ready", reader.ready());
		assertTrue("Bad spd: log read", reader.readLine().contains("'spd'"));
	}
	
	@Test public void noSpaceAtCharacter51() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp x spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("No Space @ 51: ready", reader.ready());
		assertTrue("No Space @ 51: log read", reader.readLine().contains(" space "));
	}
	
	@Test public void badWindDirectionSection() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   Dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("Bad dir: ready", reader.ready());
		assertTrue("Bad dir: log read", reader.readLine().contains("'dir'"));
	}
	
	@Test public void noSpaceAtCharacter57() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd x dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("No Space @ 57: ready", reader.ready());
		assertTrue("No Space @ 57: log read", reader.readLine().contains(" space "));
	}
	
	@Test public void badAscentRateSection() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("Bad Wcmp: ready", reader.ready());
		assertTrue("Bad Wcmp: log read", reader.readLine().contains("'Wcmp'"));
	}
	
	@Test public void noSpaceAtCharacter63() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmpx    Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("No Space @ 63: ready", reader.ready());
		assertTrue("No Space @ 63: log read", reader.readLine().contains(" space "));
	}
	
	@Test public void badLongitudeSection() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Long    Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("Bad Lon: ready", reader.ready());
		assertTrue("Bad Lon: log ready", reader.readLine().contains("'Lon'"));
	}
	
	@Test public void noSpaceAtCharacter72() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon x   Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("No Space @ 72: ready", reader.ready());
		assertTrue("No Space @ 72: log read", reader.readLine().contains(" space "));
	}
	
	@Test public void badLatitudeSection() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("Bad Lat: ready", reader.ready());
		assertTrue("Bad Lat: log read", reader.readLine().contains("'Lat'"));
	}
	
	@Test public void noSpaceAtCharacter80() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat x  Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("No Space @ 80: ready", reader.ready());
		assertTrue("No Space @ 80: log read", reader.readLine().contains(" space "));
	}
	
	@Test public void badVariable1Section() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat          Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("Bad Var 1: ready", reader.ready());
		assertTrue("Bad Var 1: log read", reader.readLine().contains("first variable measurement"));
	}
	
	@Test public void noSpaceAtCharacter86() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Elex  Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("No Space @ 86: ready", reader.ready());
		assertTrue("No Space @ 86: log read", reader.readLine().contains(" space "));
	}
	
	@Test public void badVariable2Section() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele         Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("Bad Var 2: ready", reader.ready());
		assertTrue("Bad Var 2: log read", reader.readLine().contains("second variable measurement"));
	}
	
	@Test public void noSpaceAtCharacter92() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azix  Alt    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("No Space @ 92: ready", reader.ready());
		assertTrue("No Space @ 92: log read", reader.readLine().contains(" space "));
	}
	
	@Test public void badAltitudeSection() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Ele    Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("Bad Alt: ready", reader.ready());
		assertTrue("Bad Alt: log read", reader.readLine().contains("'Alt'"));
	}
	
	@Test public void noSpaceAtCharacter100() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt  x Qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("No Space @ 100: ready", reader.ready());
		assertTrue("No Space @ 100: log read", reader.readLine().contains(" space "));
	}
	
	@Test public void badPressureCodeSection() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    qp   Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("Bad Qp: ready", reader.ready());
		assertTrue("Bad Qp: log read", reader.readLine().contains("'Qp'"));
	}
	
	@Test public void noSpaceAtCharacter105() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp x Qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("No Space @ 105: ready", reader.ready());
		assertTrue("No Space @ 105: log read", reader.readLine().contains(" space "));
	}
	
	@Test public void badTemperatureCodeSection() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   qt   Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("Bad Qt: ready", reader.ready());
		assertTrue("Bad Qt: log read", reader.readLine().contains("'Qt'"));
	}
	
	@Test public void noSpaceAtCharacter110() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt x Qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("No Space @ 110: ready", reader.ready());
		assertTrue("No Space @ 110: log read", reader.readLine().contains(" space "));
	}
	
	@Test public void badRelativeHumidityCodeSection() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   qrh  Qu   Qv   QdZ", 12, null, log);
		assertTrue("Bad Qrh: ready", reader.ready());
		assertTrue("Bad Qrh: log read", reader.readLine().contains("'Qrh'"));
	}
	
	@Test public void noSpaceAtCharacter115() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrhx Qu   Qv   QdZ", 12, null, log);
		assertTrue("No Space @ 115: ready", reader.ready());
		assertTrue("No Space @ 115: log read", reader.readLine().contains(" space "));
	}
	
	@Test public void badUComponentCodeSection() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  qu   Qv   QdZ", 12, null, log);
		assertTrue("Bad Qu: ready", reader.ready());
		assertTrue("Bad Qu: log read", reader.readLine().contains("'Qu'"));
	}
	
	@Test public void noSpaceAtCharacter120() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu x Qv   QdZ", 12, null, log);
		assertTrue("No Space @ 120: ready", reader.ready());
		assertTrue("No Space @ 120: log read", reader.readLine().contains(" space "));
	}
	
	@Test public void badVComponentCodeSection() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   qv   QdZ", 12, null, log);
		assertTrue("Bad Qv: ready", reader.ready());
		assertTrue("Bad Qv: log read", reader.readLine().contains("'Qv'"));
	}
	
	@Test public void noSpaceAtCharacter125() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv x QdZ", 12, null, log);
		assertTrue("No Space @ 125: ready", reader.ready());
		assertTrue("No Space @ 125: log read", reader.readLine().contains(" space "));
	}
	
	@Test public void badAscentRateCodeSection() throws IOException {
		state.executeLineCheck(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   Qdz", 12, null, log);
		assertTrue("Bad QdZ: ready", reader.ready());
		assertTrue("Bad QdZ: log read", reader.readLine().contains("'QdZ'"));
	}
	
	@Test public void lineNumberCorrect() throws IOException {
		state.executeLineNumberCheck(13, log);
		assertFalse("Correct Line Number: ready", reader.ready());
	}
	
	@Test public void lineNumberTooSmall() throws IOException {
		state.executeLineNumberCheck(12, log);
		assertTrue("Too Small Line Number: ready", reader.ready());
		assertTrue("Too Small Line Number: log read", reader.readLine().contains(" expected on line 13"));
	}
	
	@Test public void lineNumberTooBig() throws IOException {
		state.executeLineNumberCheck(14, log);
		assertTrue("Too Big Line Number: ready", reader.ready());
		assertTrue("Too Big Line Number: log read", reader.readLine().contains(" expected on line 13"));
	}
}