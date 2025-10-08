package dmg.ua.sounding.check;

import static org.junit.Assert.*;

import java.io.*;

import org.junit.*;

public class HeaderSeparatorCheckStateTest {

	private HeaderSeparatorCheckState state;
	
	private BufferedReader reader;
	private PrintWriter log;
	
	@Before public void setUp() throws IOException {
    	PipedInputStream in = new PipedInputStream();
    	PipedOutputStream out = new PipedOutputStream(in);
    	
    	reader = new BufferedReader(new InputStreamReader(in));
    	log = new PrintWriter(out);
	
		state = new HeaderSeparatorCheckState(reader, "", 15);		
	}
	
	
	@Test public void correctSeparatorLine() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertFalse("Correct Separator Line: ready", reader.ready());
	}
	
	@Test public void lineTooLong() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ----  ----", 15, null, log);
		assertTrue("Line Too Long: ready", reader.ready());
		assertTrue("Line Too Long: log read", reader.readLine().contains(" does not match the required format "));
	}

	@Test public void lineTooShort() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ---", 15, null, log);
		assertTrue("Line Too Short: ready", reader.ready());
		assertTrue("Line Too Short: log read", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void badTimeSection() throws IOException {
		state.executeLineCheck("--_--- ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("Bad Time: ready", reader.ready());
		assertTrue("Bad Time: read log", reader.readLine().contains(" does not match the required format "));
	}

	@Test public void noSpaceAt6() throws IOException {
		state.executeLineCheck("------------- ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("No Space @ 6: ready", reader.ready());
		assertTrue("No Space @ 6: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void badPressureSection() throws IOException {
		state.executeLineCheck("------ ---_-- ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("Bad Press: ready", reader.ready());
		assertTrue("Bad Prees: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void noSpaceAt13() throws IOException {
		state.executeLineCheck("------ ------------ ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("No Space @ 13: ready", reader.ready());
		assertTrue("No Space @ 13: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void badTemperatureSection() throws IOException {
		state.executeLineCheck("------ ------ --_-- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("Bad Temp: ready", reader.ready());
		assertTrue("Bad Temp: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void noSpaceAt19() throws IOException {
		state.executeLineCheck("------ ------ ----------- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("No Space @ 19: ready", reader.ready());
		assertTrue("No Space @ 19: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void badDewPointSection() throws IOException {
		state.executeLineCheck("------ ------ ----- --_-- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("Bad Dewpt: ready", reader.ready());
		assertTrue("Bad Dewpt: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void noSpaceAt25() throws IOException {
		state.executeLineCheck("------ ------ ----- ----------- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("No Space @ 25: ready", reader.ready());
		assertTrue("No Space @ 25: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void badRelativeHumiditySection() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ---_- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("Bad RH: ready", reader.ready());
		assertTrue("Bad RH: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void noSpaceAt31() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ------------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("No Space @ 31: ready", reader.ready());
		assertTrue("No Space @ 31: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void badUComponentSection() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ---_-- ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("Bad Ucmp: ready", reader.ready());
		assertTrue("Bad Ucmp: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void noSpaceAt38() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------------- ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("No Space @ 38: ready", reader.ready());
		assertTrue("No Space @ 38: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void badVComponentSection() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ---_-- ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("Bad Vcmp: ready", reader.ready());
		assertTrue("Bad Vcmp: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void noSpaceAt45() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------------ ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("No Space @ 45: ready", reader.ready());
		assertTrue("No Space @ read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void badWindSpeedSection() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ --_-- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("Bad spd: ready", reader.ready());
		assertTrue("Bad spd: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void noSpaceAt51() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----------- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("No Space @ 51: ready", reader.ready());
		assertTrue("No Space @ 51: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void badWindDirectionSection() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----- --_-- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("Bad dir: ready", reader.ready());
		assertTrue("Bad dir; read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void noSpaceAt57() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----- ----------- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("No Space @ 57: ready", reader.ready());
		assertTrue("No Space @ 57: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void badAscentRateSection() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----- ----- ---_- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("Bad Wcmp: ready", reader.ready());
		assertTrue("Bad Wcmp: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void noSpaceAt63() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----- ----- -------------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("No Space @ 63: ready", reader.ready());
		assertTrue("No Space @ 63: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void badLongitudeSection() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----- ----- ----- ----_--- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("Bad Lon: ready", reader.ready());
		assertTrue("Bad Lon: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void noSpaceAt72() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----- ----- ----- ---------------- ----- ----- ------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("No Space @ 72: ready", reader.ready());
		assertTrue("No Space @ 72: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void badLatitudeSection() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ---_--- ----- ----- ------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("Bad Lat: ready", reader.ready());
		assertTrue("Bad Lat: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void noSpaceAt80() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------------- ----- ------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("No Space @ 80: ready", reader.ready());
		assertTrue("No Space @ 80: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void badVariable1Section() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- --_-- ----- ------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("Bad Var 1: ready", reader.ready());
		assertTrue("Bad Var 1: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void noSpaceAt86() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----------- ------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("No Space @ 86: ready", reader.ready());
		assertTrue("No Space @ 86: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void badVariable2Section() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- --_-- ------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("Bad Var 2: ready", reader.ready());
		assertTrue("Bad Var 2: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void noSpaceAt92() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ------------- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("No Space @ 92: ready", reader.ready());
		assertTrue("No Space @ 92: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void badAltitudeSection() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ---_--- ---- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("No Alt: ready", reader.ready());
		assertTrue("No Alt: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void noSpaceAt100() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------------ ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("No Space @ 100: ready", reader.ready());
		assertTrue("No Space @ 100: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void badPressureCodeSection() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- -_-- ---- ---- ---- ---- ----", 15, null, log);
		assertTrue("Bad Qp: ready", reader.ready());
		assertTrue("Bad Qp: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void noSpaceAt105() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- --------- ---- ---- ---- ----", 15, null, log);
		assertTrue("No Space @ 105: ready", reader.ready());
		assertTrue("No Space @ 105: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void badTemperatureCodeSection() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- --_- ---- ---- ---- ----", 15, null, log);
		assertTrue("Bad Qt: ready", reader.ready());
		assertTrue("Bad Qt: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void noSpaceAt110() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- --------- ---- ---- ----", 15, null, log);
		assertTrue("No Space @ 110: ready", reader.ready());
		assertTrue("No Space @ 110: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void badRelativeHumidityCodeSection() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- -_-- ---- ---- ----", 15, null, log);
		assertTrue("Bad Qrh: ready", reader.ready());
		assertTrue("Bad Qrh: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void noSpaceAt115() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- --------- ---- ----", 15, null, log);
		assertTrue("No Space @ 115: ready", reader.ready());
		assertTrue("No Space @ 115: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void badUComponentCodeSection() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- -_-- ---- ----", 15, null, log);
		assertTrue("Bad Qu: ready", reader.ready());
		assertTrue("Bad Qu: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void noSpaceAt120() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- --------- ----", 15, null, log);
		assertTrue("No Space @ 120: ready", reader.ready());
		assertTrue("No Space @ 120: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void badVComponentCodeSection() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- -_-- ----", 15, null, log);
		assertTrue("Bad Qv: ready", reader.ready());
		assertTrue("Bad Qv: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void noSpaceAt125() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---------", 15, null, log);
		assertTrue("No Space @ 125: ready", reader.ready());
		assertTrue("No Space @ 125: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void badAscentRateCodeSection() throws IOException {
		state.executeLineCheck("------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- --_-", 15, null, log);
		assertTrue("Bad QdZ: ready", reader.ready());
		assertTrue("Bad QdZ: read log", reader.readLine().contains(" does not match the required format "));
	}
	
	@Test public void lineNumberCorrect() throws IOException {
		state.executeLineNumberCheck(15, log);
		assertFalse("Correct Line Number: ready", reader.ready());
	}
	
	@Test public void lineNumberTooSmall() throws IOException {
		state.executeLineNumberCheck(14, log);
		assertTrue("Too Small Line Number: ready", reader.ready());
		assertTrue("Too Small Line Number: log read", reader.readLine().contains(" expected on line 15"));
	}
	
	@Test public void lineNumberTooBig() throws IOException {
		state.executeLineNumberCheck(16, log);
		assertTrue("Too Big Line Number: ready", reader.ready());
		assertTrue("Too Big Line Number: log read", reader.readLine().contains(" expected on line 15"));
	}
}