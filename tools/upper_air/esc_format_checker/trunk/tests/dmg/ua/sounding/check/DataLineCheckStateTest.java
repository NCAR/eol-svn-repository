package dmg.ua.sounding.check;

// NOTE: This also checks the RecordStore class through all the data line checkes.

import static org.junit.Assert.*;

import java.io.*;

import org.junit.*;

public class DataLineCheckStateTest {

	private DataLineCheckState state;
	private DataStore store;
	
	private BufferedReader reader;
	private PrintWriter log;
	
	@Before public void setUp() throws IOException {
    	PipedInputStream in = new PipedInputStream();
    	PipedOutputStream out = new PipedOutputStream(in);
    	
    	reader = new BufferedReader(new InputStreamReader(in));
    	log = new PrintWriter(out);
	
		state = new DataLineCheckState(reader, "", 16);
		store = new DataStore();
	}
	
	@Test public void correctDataLine() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertFalse("Correct Data Line: ready", reader.ready());
	}
	
	@Test public void allMissingDataLine() throws IOException {
		state.executeLineCheck("9999.0 9999.0 999.0 999.0 999.0 9999.0 9999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 16, store, log);
		assertFalse("Correct Data Line: ready", reader.ready());
	}
	
	@Test public void lineTooLong() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0   2.0", 16, store, log);
		assertTrue("Line Too Long: ready", reader.ready());
		assertTrue("Line Too Long: log read", reader.readLine().contains(" 130 characters "));
	}

	@Test public void lineTooShort() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0 2.0", 16, store, log);
		assertTrue("Line Too Short: ready", reader.ready());
		assertTrue("Line Too Short: log read", reader.readLine().contains(" 130 characters "));
	}
	
	@Test public void nonNumericTimeValue() throws IOException {
		state.executeLineCheck("  4X.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Non Numeric Time: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Non Numeric Time: non-numeric", line.contains("non-numeric"));
		assertTrue("Non Numeric Time: time", line.contains("'Time'"));
	}
	
	@Test public void nonNumericPressureValue() throws IOException {
		state.executeLineCheck("  48.0  9X4.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Non Numeric Press: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Non Numeric Press: non-numeric", line.contains("non-numeric"));
		assertTrue("Non Numeric Press: press", line.contains("'Press'"));
	}
	
	@Test public void nonNumericTemperatureValue() throws IOException {
		state.executeLineCheck("  48.0  974.5  X8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Non Numeric Temp: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Non Numeric Temp: non-numeric", line.contains("non-numeric"));
		assertTrue("Non Numeric Temp: temp", line.contains("'Temp'"));
	}
	
	@Test public void nonNumericDewPointValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5  .4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Non Numeric Dewpt: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Non Numeric Dewpt: non-numeric", line.contains("non-numeric"));
		assertTrue("Non Numeric Dewpt: dewpt", line.contains("'Dewpt'"));
	}
	
	@Test public void nonNumericRelativeHumidityValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  7..7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Non Numeric RH: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Non Numeric RH: non-numeric", line.contains("non-numeric"));
		assertTrue("Non Numeric RH: rh", line.contains("'RH'"));
	}

	@Test public void nonNumericUComponentValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -x.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Non Numeric Ucmp: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Non Numeric Ucmp: non-numeric", line.contains("non-numeric"));
		assertTrue("Non Numeric Ucmp: ucmp", line.contains("'Ucmp'"));
	}
	
	@Test public void nonNumericVComponentValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.X   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Non Numeric Vcmp: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Non Numeric Vcmp: non-numeric", line.contains("non-numeric"));
		assertTrue("Non Numeric Vcmp: vcmp", line.contains("'Vcmp'"));
	}
	
	@Test public void nonNumericWindSpeedValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.X 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Non Numeric spd: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Non Numeric spd: non-numeric", line.contains("non-numeric"));
		assertTrue("Non Numeric spd: spd", line.contains("'spd'"));
	}
	
	@Test public void nonNumericWindDirectionValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 17X.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Non Numeric dir: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Non Numeric dir: non-numeric", line.contains("non-numeric"));
		assertTrue("Non Numeric dir: dir", line.contains("'dir'"));
	}
	
	@Test public void nonNumericAscentRateValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   X.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Non Numeric Wcmp: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Non Numeric Wcmp: non-numeric", line.contains("non-numeric"));
		assertTrue("Non Numeric Wcmp: Wcmp", line.contains("'Wcmp'"));
	}
	
	@Test public void nonNumericLongitudeValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -1x2.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Non Numeric Lon: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Non Numeric Lon: non-numeric", line.contains("non-numeric"));
		assertTrue("Non Numeric Lon: Lon", line.contains("'Lon'"));
	}
	
	@Test public void nonNumericLatitudeValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.7x3  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Non Numeric Lat: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Non Numeric Lat: non-numeric", line.contains("non-numeric"));
		assertTrue("Non Numeric Lat: Lat", line.contains("'Lat'"));
	}
	
	@Test public void nonNumericVariable1Value() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  4X.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Non Numeric Var 1: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Non Numeric Var 1: non-numeric", line.contains("non-numeric"));
		assertTrue("Non Numeric Var 1: var 1", line.contains("'Variable Measurement 1'"));
	}
	
	@Test public void nonNumericVariable2Value() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 1Xx.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Non Numeric Var 2: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Non Numeric Var 2: non-numeric", line.contains("non-numeric"));
		assertTrue("Non Numeric Var 2: var 2", line.contains("'Variable Measurement 2"));
	}
	
	@Test public void nonNumericAltitudeValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7  x326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Non Numeric Alt: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Non Numeric Alt: non-numeric", line.contains("non-numeric"));
		assertTrue("Non Numeric Alt: Alt", line.contains("'Alt'"));
	}
	
	@Test public void nonNumericPressureCodeValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  S.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Non Numeric Qp: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Non Numeric Qp: non-numeric", line.contains("non-numeric"));
		assertTrue("Non Numeric Qp: Qp", line.contains("'Qp'"));
	}
	
	@Test public void nonNumericTemperatureCodeValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  S.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Non Numeric Qt: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Non Numeric Qt: non-numeric", line.contains("non-numeric"));
		assertTrue("Non Numeric Qt: Qt", line.contains("'Qt'"));
	}
	
	@Test public void nonNumericRelativeHumidityCodeValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  x.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Non Numeric Qrh: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Non Numeric Qrh: non-numeric", line.contains("non-numeric"));
		assertTrue("Non Numeric Qrh: Qrh", line.contains("'Qrh'"));
	}
	
	@Test public void nonNumericUComponentCodeValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  x.0  4.0  2.0", 16, store, log);
		assertTrue("Non Numeric Qu: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Non Numeric Qu: non-numeric", line.contains("non-numeric"));
		assertTrue("Non Numeric Qu: Qu", line.contains("'Qu'"));
	}
	
	@Test public void nonNumericVComponentCodeValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  x.0  2.0", 16, store, log);
		assertTrue("Non Numeric Qv: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Non Numeric Qv: non-numeric", line.contains("non-numeric"));
		assertTrue("Non Numeric Qv: Qv", line.contains("'Qv'"));
	}
	
	@Test public void nonNumericAscentRateCodeValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.x", 16, store, log);
		assertTrue("Non Numeric QdZ: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Non Numeric QdZ: non-numeric", line.contains("non-numeric"));
		assertTrue("Non Numeric QdZ: QdZ", line.contains("QdZ"));
	}
	
	@Test public void timeEndingInSpace() throws IOException {
		state.executeLineCheck(" 48.0   974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Time End in Space: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Time End in Space: bad positioning", line.contains("badly positioned value"));
		assertTrue("Time End in Space: time", line.contains("'Time'"));
	}
	
	@Test public void pressureEndingInSpace() throws IOException {
		state.executeLineCheck("  48.0  974.    8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Press End in Space: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Press End in Space: bad positioning", line.contains("badly positioned value"));
		assertTrue("Press End in Space: Press", line.contains("'Press'"));
	}
	
	@Test public void temperatureEndingInSpace() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.    4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Temp End in Space: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Temp End in Space: bad positioning", line.contains("badly positioned value"));
		assertTrue("Temp End in Space: Temp", line.contains("'Temp'"));
	}
	
	@Test public void dewPointEndingInSpace() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.   74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Dewpt End in Space: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Dewpt End in Space: bad positioning", line.contains("badly positioned value"));
		assertTrue("Dewpt End in Space: Dewpt", line.contains("'Dewpt'"));
	}
	
	@Test public void relativeHumidityEndingInSpace() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.    -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("RH End in Space: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("RH End in Space: bad positioning", line.contains("badly positioned value"));
		assertTrue("RH End in Space: RH", line.contains("'RH'"));
	}
	
	@Test public void uComponentEndingInSpace() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -1.     6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Ucmp End in Space: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Ucmp End in Space: bad positioning", line.contains("badly positioned value"));
		assertTrue("Ucmp End in Space: Ucmp", line.contains("'Ucmp'"));
	}
	
	@Test public void vComponentEndingInSpace() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.    6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Vcmp End in Space: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Vcmp End in Space: bad positioning", line.contains("badly positioned value"));
		assertTrue("Vcmp End in Space: Vcmp", line.contains("'Vcmp'"));
	}
	
	@Test public void windSpeedEndingInSpace() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.  173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Spd End in Space: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Spd End in Space: bad positioning", line.contains("badly positioned value"));
		assertTrue("Spd End in Space: spd", line.contains("'spd'"));
	}
	
	@Test public void windDirectionEndingInSpace() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.    6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Dir End in Space: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Dir End in Space: bad positioning", line.contains("badly positioned value"));
		assertTrue("Dir End in Space: dir", line.contains("'dir'"));
	}
	
	@Test public void ascentRateEndingInSpace() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.  -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Wcmp End in Space: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Wcmp End in Space: bad positioning", line.contains("badly positioned value"));
		assertTrue("Wcmp End in Space: Wcmp", line.contains("'Wcmp'"));
	}
	
	@Test public void longitudeEndingInSpace() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.20   37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Lon End in Space: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Lon End in Space: bad positioning", line.contains("badly positioned value"));
		assertTrue("Lon End in Space: Lon", line.contains("'Lon'"));
	}
	
	@Test public void latitudeEndingInSpace() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.70   48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Lat End in Space: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Lat End in Space: bad positioning", line.contains("badly positioned value"));
		assertTrue("Lat End in Space: Lat", line.contains("'Lat'"));
	}
	
	@Test public void variable1EndingInSpace() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.  162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Var 1 End in Space: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Var 1 End in Space: bad positioning", line.contains("badly positioned value"));
		assertTrue("Var 1 End in Space: var 1", line.contains("Variable Measurement 1"));
	}
	
	@Test public void variable2EndingInSpace() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.    326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Var 2 End in Space: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Var 2 End in Space: bad positioning", line.contains("badly positioned value"));
		assertTrue("Var 2 End in Space: var 2", line.contains("Variable Measurement 2"));
	}
	
	@Test public void altitudeEndingInSpace() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.   1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Alt End in Space: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Alt End in Space: bad positioning", line.contains("badly positioned value"));
		assertTrue("Alt End in Space: Alt", line.contains("'Alt'"));
	}
	
	@Test public void pressureCodeEndingInSpace() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.   1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Qp End in Space: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qp End in Space: bad positioning", line.contains("badly positioned value"));
		assertTrue("Qp End in Space: Qp", line.contains("'Qp'"));
	}
	
	@Test public void temperatureCodeEndingInSpace() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.   1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Qt End in Space: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qt End in Space: bad positioning", line.contains("badly positioned value"));
		assertTrue("Qt End in Space: Qt", line.contains("'Qt'"));
	}
	
	@Test public void relativeHumidityCodeEndingInSpace() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.   4.0  4.0  2.0", 16, store, log);
		assertTrue("Qrh End in Space: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qrh End in Space: bad positioning", line.contains("badly positioned value"));
		assertTrue("Qrh End in Space: Qrh", line.contains("'Qrh'"));
	}
	
	@Test public void uComponentCodeEndingInSpace() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.   4.0  2.0", 16, store, log);
		assertTrue("Qu End in Space: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qu End in Space: bad positioning", line.contains("badly positioned value"));
		assertTrue("Qu End in Space: Qu", line.contains("'Qu'"));
	}
	
	@Test public void vComponentCodeEndingInSpace() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.   2.0", 16, store, log);
		assertTrue("Qv End in Space: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qv End in Space: bad positioning", line.contains("badly positioned value"));
		assertTrue("Qv End in Space: Qv", line.contains("'Qv'"));
	}
	
	@Test public void ascentRateCodeEndingInSpace() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2. ", 16, store, log);
		assertTrue("QdZ End in Space: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("QdZ End in Space: bad positioning", line.contains("badly positioned value"));
		assertTrue("QdZ End in Space: QdZ", line.contains("'QdZ'"));
	}
	
	@Test public void timeNegativeZero() throws IOException {
		state.executeLineCheck("  -0.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Time -0: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Time -0: negative zero", line.contains("negative zero"));
		assertTrue("Time -0: Time", line.contains("'Time'"));
	}
	
	@Test public void pressureNegativeZero() throws IOException {
		state.executeLineCheck("  48.0   -0.0   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Press -0: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Press -0: negative zero", line.contains("negative zero"));
		assertTrue("Press -0: Press", line.contains("'Press'"));
	}
	
	@Test public void temperatureNegativeZero() throws IOException {
		state.executeLineCheck("  48.0  974.5  -0.0   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Temp -0: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Temp -0: negative zero", line.contains("negative zero"));
		assertTrue("Temp -0: Temp", line.contains("'Temp'"));
	}
	
	@Test public void dewPointNegativeZero() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5  -0.0  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Dewpt -0: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Dewpt -0: negative zero", line.contains("negative zero"));
		assertTrue("Dewpt -0: Dewpt", line.contains("'Dewpt'"));
	}
	
	@Test public void relativeHumidityNegativeZero() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  -0.0   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("RH -0: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("RH -0: negative zero", line.contains("negative zero"));
		assertTrue("RH -0: RH", line.contains("'RH'"));
	}
	
	@Test public void uComponentNegativeZero() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.0    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Ucmp -0: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Ucmp -0: negative zero", line.contains("negative zero"));
		assertTrue("Ucmp -0: Ucmp", line.contains("'Ucmp'"));
	}
	
	@Test public void vComponentNegativeZero() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7   -0.0   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Vcmp -0: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Vcmp -0: negative zero", line.contains("negative zero"));
		assertTrue("Vcmp -0: Vcmp", line.contains("'Vcmp'"));
	}
	
	@Test public void windSpeedNegativeZero() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1  -0.0 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Spd -0: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Spd -0: negative zero", line.contains("negative zero"));
		assertTrue("Spd -0: spd", line.contains("'spd'"));
	}
	
	@Test public void windDirectionNegativeZero() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1  -0.0   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Dir -0: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Dir -0: negative zero", line.contains("negative zero"));
		assertTrue("Dir -0: dir", line.contains("'dir'"));
	}
	
	@Test public void ascentRateNegativeZero() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5  -0.0 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Wcmp -0: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Wcmp -0: negative zero", line.contains("negative zero"));
		assertTrue("Wcmp -0: Wcmp", line.contains("'Wcmp'"));
	}
	
	@Test public void longitudeNegativeZero() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3   -0.000  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Lon -0: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Lon -0: negative zero", line.contains("negative zero"));
		assertTrue("Lon -0: Lon", line.contains("'Lon'"));
	}
	
	@Test public void latitudeNegativeZero() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  -0.000  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Lat -0: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Lat -0: negative zero", line.contains("negative zero"));
		assertTrue("Lat -0: Lat", line.contains("'Lat'"));
	}
	
	@Test public void variable1NegativeZero() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  -0.0 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Var 1 -0: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Var 1 -0: negative zero", line.contains("negative zero"));
		assertTrue("Var 1 -0: Var 1", line.contains("Variable Measurement 1"));
	}
	
	@Test public void variable2NegativeZero() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4  -0.0   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Var 2 -0: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Var 2 -0: negative zero", line.contains("negative zero"));
		assertTrue("Var 2 -0: Var 2", line.contains("Variable Measurement 2"));
	}
	
	@Test public void altitudeNegativeZero() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7    -0.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Alt -0: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Alt -0: negative zero", line.contains("negative zero"));
		assertTrue("Alt -0: Alt", line.contains("'Alt'"));
	}
	
	@Test public void timeEndingInDecimal() throws IOException {
		state.executeLineCheck("   48.  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Time End in Decimal: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Time End in Decimal: bad positioning", line.contains("badly positioned value"));
		assertTrue("Time End in Decimal: time", line.contains("'Time'"));
	}
	
	@Test public void pressureEndingInDecimal() throws IOException {
		state.executeLineCheck("  48.0   974.   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Press End in Decimal: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Press End in Decimal: bad positioning", line.contains("badly positioned value"));
		assertTrue("Press End in Decimal: Press", line.contains("'Press'"));
	}
	
	@Test public void temperatureEndingInDecimal() throws IOException {
		state.executeLineCheck("  48.0  974.5    8.   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Temp End in Decimal: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Temp End in Decimal: bad positioning", line.contains("badly positioned value"));
		assertTrue("Temp End in Decimal: Temp", line.contains("'Temp'"));
	}
	
	@Test public void dewPointEndingInDecimal() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5    4.  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Dewpt End in Decimal: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Dewpt End in Decimal: bad positioning", line.contains("badly positioned value"));
		assertTrue("Dewpt End in Decimal: Dewpt", line.contains("'Dewpt'"));
	}
	
	@Test public void relativeHumidityEndingInDecimal() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3   74.   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("RH End in Decimal: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("RH End in Decimal: bad positioning", line.contains("badly positioned value"));
		assertTrue("RH End in Decimal: RH", line.contains("'RH'"));
	}
	
	@Test public void uComponentEndingInDecimal() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7    -1.    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Ucmp End in Decimal: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Ucmp End in Decimal: bad positioning", line.contains("badly positioned value"));
		assertTrue("Ucmp End in Decimal: Ucmp", line.contains("'Ucmp'"));
	}
	
	@Test public void vComponentEndingInDecimal() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7     6.   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Vcmp End in Decimal: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Vcmp End in Decimal: bad positioning", line.contains("badly positioned value"));
		assertTrue("Vcmp End in Decimal: Vcmp", line.contains("'Vcmp'"));
	}
	
	@Test public void windSpeedEndingInDecimal() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1    6. 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Spd End in Decimal: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Spd End in Decimal: bad positioning", line.contains("badly positioned value"));
		assertTrue("Spd End in Decimal: spd", line.contains("'spd'"));
	}
	
	@Test public void windDirectionEndingInDecimal() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1  173.   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Dir End in Decimal: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Dir End in Decimal: bad positioning", line.contains("badly positioned value"));
		assertTrue("Dir End in Decimal: dir", line.contains("'dir'"));
	}
	
	@Test public void ascentRateEndingInDecimal() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5    6. -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Wcmp End in Decimal: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Wcmp End in Decimal: bad positioning", line.contains("badly positioned value"));
		assertTrue("Wcmp End in Decimal: Wcmp", line.contains("'Wcmp'"));
	}
	
	@Test public void longitudeEndingInDecimal() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3    -122.  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Lon End in Decimal: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Lon End in Decimal: bad positioning", line.contains("badly positioned value"));
		assertTrue("Lon End in Decimal: Lon", line.contains("'Lon'"));
	}
	
	@Test public void latitudeEndingInDecimal() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201     37.  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Lat End in Decimal: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Lat End in Decimal: bad positioning", line.contains("badly positioned value"));
		assertTrue("Lat End in Decimal: Lat", line.contains("'Lat'"));
	}
	
	@Test public void variable1EndingInDecimal() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703   48. 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Var 1 End in Decimal: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Var 1 End in Decimal: bad positioning", line.contains("badly positioned value"));
		assertTrue("Var 1 End in Decimal: Var 1", line.contains("Variable Measurement 1"));
	}
	
	@Test public void variable2EndingInDecimal() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4  162.   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Var 2 End in Decimal: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Var 2 End in Decimal: bad positioning", line.contains("badly positioned value"));
		assertTrue("Var 2 End in Decimal: Var 2", line.contains("Variable Measurement 2"));
	}
	
	@Test public void altitudeEndingInDecimal() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7    326.  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Alt End in Decimal: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Alt End in Decimal: bad positioning", line.contains("badly positioned value"));
		assertTrue("Alt End in Decimal: Alt", line.contains("'Alt'"));
	}
	
	@Test public void pressureCodeEndingInDecimal() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0   1.  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Qp End in Decimal: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qp End in Decimal: bad positioning", line.contains("badly positioned value"));
		assertTrue("Qp End in Decimal: Qp", line.contains("'Qp'"));
	}
	
	@Test public void temperatureCodeEndingInDecimal() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0   1.  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Qt End in Decimal: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qt End in Decimal: bad positioning", line.contains("badly positioned value"));
		assertTrue("Qt End in Decimal: Qt", line.contains("'Qt'"));
	}
	
	@Test public void relativeHumidityCodeEndingInDecimal() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0   1.  4.0  4.0  2.0", 16, store, log);
		assertTrue("Qrh End in Decimal: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qrh End in Decimal: bad positioning", line.contains("badly positioned value"));
		assertTrue("Qrh End in Decimal: Qrh", line.contains("'Qrh'"));
	}
	
	@Test public void uComponentCodeEndingInDecimal() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0   4.  4.0  2.0", 16, store, log);
		assertTrue("Qu End in Decimal: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qu End in Decimal: bad positioning", line.contains("badly positioned value"));
		assertTrue("Qu End in Decimal: Qu", line.contains("'Qu'"));
	}
	
	@Test public void vComponentCodeEndingInDecimal() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0   4.  2.0", 16, store, log);
		assertTrue("Qv End in Decimal: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qv End in Decimal: bad positioning", line.contains("badly positioned value"));
		assertTrue("Qv End in Decimal: Qv", line.contains("'Qv'"));
	}
	
	@Test public void ascentRateCodeEndingInDecimal() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0   2.", 16, store, log);
		assertTrue("QdZ End in Decimal: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("QdZ End in Decimal: bad positioning", line.contains("badly positioned value"));
		assertTrue("QdZ End in Decimal: QdZ", line.contains("'QdZ'"));
	}
	
	@Test public void timeTooManySignificantDigits() throws IOException {
		state.executeLineCheck(" 48.00  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Time Sig Dig: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Time Sig Dig: bad positioning", line.contains("badly positioned value"));
		assertTrue("Time Sig Dig: time", line.contains("'Time'"));
	}
	
	@Test public void pressureTooManySignificantDigits() throws IOException {
		state.executeLineCheck("  48.0  97.50   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Press Sig Dig: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Press Sig Dig: bad positioning", line.contains("badly positioned value"));
		assertTrue("Press Sig Dig: Press", line.contains("'Press'"));
	}
	
	@Test public void temperatureTooManySignificantDigits() throws IOException {
		state.executeLineCheck("  48.0  974.5  8.52   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Temp Sig Dig: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Temp Sig Dig: bad positioning", line.contains("badly positioned value"));
		assertTrue("Temp Sig Dig: Temp", line.contains("'Temp'"));
	}
	
	@Test public void dewPointTooManySignificantDigits() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5  4.30  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Dewpt Sig Dig: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Dewpt Sig Dig: bad positioning", line.contains("badly positioned value"));
		assertTrue("Dewpt Sig Dig: Dewpt", line.contains("'Dewpt'"));
	}
	
	@Test public void relativeHumidityTooManySignificantDigits() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  7.70   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("RH Sig Dig: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("RH Sig Dig: bad positioning", line.contains("badly positioned value"));
		assertTrue("RH Sig Dig: RH", line.contains("'RH'"));
	}
	
	@Test public void uComponentTooManySignificantDigits() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7  -0.74    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Ucmp Sig Dig: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Ucmp Sig Dig: bad positioning", line.contains("badly positioned value"));
		assertTrue("Ucmp Sig Dig: Ucmp", line.contains("'Ucmp'"));
	}
	
	@Test public void vComponentTooManySignificantDigits() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7   6.16   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Vcmp Sig Dig: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Vcmp Sig Dig: bad positioning", line.contains("badly positioned value"));
		assertTrue("Vcmp Sig Dig: Vcmp", line.contains("'Vcmp'"));
	}
	
	@Test public void windSpeedTooManySignificantDigits() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1  6.18 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Spd Sig Dig: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Spd Sig Dig: bad positioning", line.contains("badly positioned value"));
		assertTrue("Spd Sig Dig: spd", line.contains("'spd'"));
	}
	
	@Test public void windDirectionTooManySignificantDigits() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 13.50   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Dir Sig Dig: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Dir Sig Dig: bad positioning", line.contains("badly positioned value"));
		assertTrue("Dir Sig Dig: dir", line.contains("'dir'"));
	}
	
	@Test public void ascentRateTooManySignificantDigits() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5  6.33 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Wcmp Sig Dig: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Wcmp Sig Dig: bad positioning", line.contains("badly positioned value"));
		assertTrue("Wcmp Sig DIg: Wcmp", line.contains("'Wcmp'"));
	}
	
	@Test public void longitudeTooManySignificantDigits() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -12.2201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Lon Sig Dig: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Lon Sig Dig: bad positioning", line.contains("badly positioned value"));
		assertTrue("Lon Sig Dig: Lon", line.contains("'Lon'"));
	}
	
	@Test public void latitudeTooManySignificantDigits() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201 37.0703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Lat Sig Dig: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Lat Sig Dig: bad positioning", line.contains("badly positioned value"));
		assertTrue("Lat Sig Dig: Lat", line.contains("'Lat'"));
	}
	
	@Test public void variable1TooManySignificantDigits() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703 48.34 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Var 1 Sig Dig: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Var 1 Sig Dig: bad positioning", line.contains("badly positioned value"));
		assertTrue("Var 1 Sig Dig: var 1", line.contains("Variable Measurement 1"));
	}
	
	@Test public void variable2TooManySignificantDigits() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 12.37   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Var 2 Sig Dig: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Var 2 Sig Dig: bad positioning", line.contains("badly positioned value"));
		assertTrue("Var 2 Sig Dig: var 2", line.contains("Variable Measurement 2"));
	}
	
	@Test public void altitudeTooManySignificantDigits() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7  326.10  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Alt Sig Dig: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Alt Sig Dig: bad positioning", line.contains("badly positioned value"));
		assertTrue("Alt Sig Dig: Alt", line.contains("'Alt'"));
	}
	
	@Test public void pressureCodeTooManySignificantDigits() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0 1.00  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Qp Sig Dig: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qp Sig Dig: bad positioning", line.contains("badly positioned value"));
		assertTrue("Qp Sig Dig: Qp", line.contains("'Qp'"));
	}
	
	@Test public void temperatureCodeTooManySignificantDigits() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0 1.00  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Qt Sig Dig: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qt Sig Dig: bad positioning", line.contains("badly positioned value"));
		assertTrue("Qt Sig Dig: Qt", line.contains("'Qt'"));
	}
	
	@Test public void relativeHumidityCodeTooManySignificantDigits() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0 1.00  4.0  4.0  2.0", 16, store, log);
		assertTrue("Qrh Sig Dig: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qrh Sig Dig: bad positioning", line.contains("badly positioned value"));
		assertTrue("Qrh Sig Dig: Qrh", line.contains("'Qrh'"));
	}
	
	@Test public void uComponentCodeTooManySignificantDigits() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0 4.00  4.0  2.0", 16, store, log);
		assertTrue("Qu Sig Dig: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qu Sig Dig: bad positioning", line.contains("badly positioned value"));
		assertTrue("Qu Sig Dig: Qu", line.contains("'Qu'"));
	}
	
	@Test public void vComponentCodeTooManySignificantDigits() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0 4.00  2.0", 16, store, log);
		assertTrue("Qv Sig Dig: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qv Sig Dig: bad positioning", line.contains("badly positioned value"));
		assertTrue("Qv Sig Dig: Qv", line.contains("'Qv'"));
	}
	
	@Test public void ascentRateCodeTooManySignificantDigits() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0 2.00", 16, store, log);
		assertTrue("QdZ Sig Dig: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("QdZ Sig Dig: bad positioning", line.contains("badly positioned value"));
		assertTrue("QdZ Sig Dig: QdZ", line.contains("'QdZ'"));
	}
	
	@Test public void timeIntegralValue() throws IOException {
		state.executeLineCheck("    48  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Time Integer: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Time Integer: bad positioning", line.contains("badly positioned value"));
		assertTrue("Time Integer: Time", line.contains("'Time'"));
	}
	
	@Test public void pressureIntegralValue() throws IOException {
		state.executeLineCheck("  48.0    974   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Press Int: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Press Int: bad positioning", line.contains("badly positioned value"));
		assertTrue("Press Int: Press", line.contains("'Press'"));
	}
	
	@Test public void temperatureIntegralValue() throws IOException {
		state.executeLineCheck("  48.0  974.5     8   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Temp Int: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Temp Int: bad positioning", line.contains("badly positioned value"));
		assertTrue("Temp Int: Temp", line.contains("'Temp'"));
	}
	
	@Test public void dewPointIntegralValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5     4  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Dewpt Int: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Dewpt Int: bad positioning", line.contains("badly positioned value"));
		assertTrue("Dewpt Int: Dewpt", line.contains("'Dewpt'"));
	}
	
	@Test public void relativeHumidityIntegralValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3    74   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("RH Int: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("RH Int: bad positioning", line.contains("badly positioned value"));
		assertTrue("RH Int: RH", line.contains("'RH'"));
	}
	
	@Test public void uComponentIntegralValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7      0    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Ucmp Int: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Ucmp Int: bad positioning", line.contains("badly positioned value"));
		assertTrue("Ucmp Int: Ucmp", line.contains("'Ucmp'"));
	}
	
	@Test public void vComponentIntegralValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7      6   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Vcmp Int: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Vcmp Int: bad positioning", line.contains("badly positioned value"));
		assertTrue("Vcmp Int: Vcmp", line.contains("'Vcmp'"));
	}
	
	@Test public void windSpeedIntegralValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1     6 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Spd Int: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Spd Int: bad positioning", line.contains("badly positioned value"));
		assertTrue("Spd Int: spd", line.contains("'spd'"));
	}
	
	@Test public void windDirectionIntegralValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1   173   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Dir Int: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Dir Int: bad positioning", line.contains("badly positioned value"));
		assertTrue("Dir Int: dir", line.contains("'dir'"));
	}
	
	@Test public void ascentRateIntegralValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5     6 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Wcmp Int: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Wcmp Int: bad positioning", line.contains("badly positioned value"));
		assertTrue("Wcmp Int: Wcmp", line.contains("'Wcmp'"));
	}
	
	@Test public void longitudeIntegralValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3     -122  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Lon Int: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Lon Int: bad positioning", line.contains("badly positioned value"));
		assertTrue("Lon Int: Lon", line.contains("'Lon'"));
	}
	
	@Test public void latitudeIntegralValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201      37  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Lat Int: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Lat Int: bad positioning", line.contains("badly positioned value"));
		assertTrue("Lat Int: Lat", line.contains("'Lat'"));
	}
	
	@Test public void variable1IntegralValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703    48 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Var 1 Int: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Var 1 Int: bad positioning", line.contains("badly positioned value"));
		assertTrue("Var 1 Int: Var 1", line.contains("Variable Measurement 1"));
	}
	
	@Test public void variable2IntegralValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4   162   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Var 2 Int: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Var 2 Int: bad positioning", line.contains("badly positioned value"));
		assertTrue("Var 2 Int: var 2", line.contains("Variable Measurement 2"));
	}
	
	@Test public void altitudeIntegralValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7     326  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Alt Int: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Alt Int: bad positioning", line.contains("badly positioned value"));
		assertTrue("Alt Int: Alt", line.contains("'Alt'"));
	}
	
	@Test public void pressureCodeIntegralValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0    1  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Qp Int: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qp Int: bad positioning", line.contains("badly positioned value"));
		assertTrue("Qp Int: Qp", line.contains("'Qp'"));
	}
	
	@Test public void temperatureCodeIntegralValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0    1  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Qt Int: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qt Int: bad positioning", line.contains("badly positioned value"));
		assertTrue("Qt Int: Qt", line.contains("'Qt'"));
	}
	
	@Test public void relativeHumidityCodeIntegralValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0    1  4.0  4.0  2.0", 16, store, log);
		assertTrue("Qrh Int: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qrh Int: bad positioning", line.contains("badly positioned value"));
		assertTrue("Qrh Int: Qrh", line.contains("'Qrh'"));
	}
	
	@Test public void uComponentCodeIntegralValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0    4  4.0  2.0", 16, store, log);
		assertTrue("Qu Int: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qu Int: bad positioning", line.contains("badly positioned value"));
		assertTrue("Qu Int: Qu", line.contains("'Qu'"));
	}
	
	@Test public void vComponentCodeIntegralValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0    4  2.0", 16, store, log);
		assertTrue("Qv Int: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qv Int: bad positioning", line.contains("badly positioned value"));
		assertTrue("Qv Int: Qv", line.contains("'Qv'"));
	}
	
	@Test public void ascentRateCodeIntegralValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0    2", 16, store, log);
		assertTrue("QdZ Int: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("QdZ Int: bad positioning", line.contains("badly positioned value"));
		assertTrue("QdZ Int: QdZ", line.contains("'QdZ'"));
	}
	
	@Test public void timeValueTooBig() throws IOException {
		state.executeLineCheck("10000.0 974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Time Too Big: ready", reader.ready());
		assertTrue("Time Too Big: time", reader.readLine().contains("'Time'"));
	}
	
	@Test public void pressureValueTooBig() throws IOException {
		state.executeLineCheck("  48.0 10000.0  8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Press Too Big: ready", reader.ready());
		assertTrue("Press Too Big: press", reader.readLine().contains("'Press'"));
	}
	
	@Test public void temperatureValueTooBig() throws IOException {
		state.executeLineCheck("  48.0  974.5 1000.0  4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Temp Too Big: ready", reader.ready());
		assertTrue("Temp Too Big: temp", reader.readLine().contains("'Temp'"));
	}
	
	@Test public void dewPointValueTooBig() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5 1000.0 74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Dewpt Too Big: ready", reader.ready());
		assertTrue("Dewpt Too Big: dewpt", reader.readLine().contains("'Dewpt'"));
	}
	
	@Test public void relativeHumidityValueTooBig() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3 104.1   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("RH Too Big: ready", reader.ready());
		assertTrue("RH Too Big: RH", reader.readLine().contains("'RH'"));
	}
	
	@Test public void uComponentValueTooBig() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7 10000.0   6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Ucmp Too Big: ready", reader.ready());
		assertTrue("Ucmp Too Big: Ucmp", reader.readLine().contains("'Ucmp'"));
	}
	
	@Test public void vComponentValueToBig() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7 10000.0  6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Vcmp Too Big: ready", reader.ready());
		assertTrue("Vcmp Too Big: Vcmp", reader.readLine().contains("'Vcmp'"));
	}
	
	@Test public void windSpeedValueTooBig() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.11000.0 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Spd Too Big: ready", reader.ready());
		assertTrue("Spd Too Big: spd", reader.readLine().contains("'spd'"));
	}
	
	@Test public void windDirectionValueTooBig() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 360.1   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Dir Too Big: ready", reader.ready());
		assertTrue("Dir Too Big: dir", reader.readLine().contains("'dir'"));
	}
	
	@Test public void ascentRateValueTooBig() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.51000.0 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Wcmp Too Big: ready", reader.ready());
		assertTrue("Wcmp Too Big: Wcmp", reader.readLine().contains("'Wcmp'"));
	}
	
	@Test public void longitudeValueTooBig() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3  180.001  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Lon Too Big: ready", reader.ready());
		assertTrue("Lon Too Big: Lon", reader.readLine().contains("'Lon'"));
	}
	
	@Test public void latitudeValueTooBig() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  90.001  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Lat Too Big: ready", reader.ready());
		assertTrue("Lat Too Big: Lat", reader.readLine().contains("'Lat'"));
	}
	
	@Test public void variable1ValueTooBig() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.7031000.0 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Var 1 Too Big: ready", reader.ready());
		assertTrue("Var 1 Too Big: Var 1", reader.readLine().contains("Variable Measurement 1"));
	}
	
	@Test public void variable2ValueTooBig() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 1000.0  326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Var 2 Too Big: ready", reader.ready());
		assertTrue("Var 2 Too Big: Var 2", reader.readLine().contains("Variable Measurement 2"));
	}
	
	@Test public void altitudeValueTooBig() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7 100000.0 1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Alt Too Big: ready", reader.ready());
		assertTrue("Alt Too Big: Alt", reader.readLine().contains("'Alt'"));
	}
	
	@Test public void pressureCodeIllegalCode() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  0.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Qp Illegal Code: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qp Illegal Code: illegal value", line.contains("illegal value"));
		assertTrue("Qp Illegal Code: Qp", line.contains("'Qp'"));
	}
	
	@Test public void temperatureCodeIllegalCode() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  0.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Qt Illegal Code: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qt Illegal Code: illegal value", line.contains("illegal value"));
		assertTrue("Qt Illegal Code: Qt", line.contains("'Qt'"));
	}
	
	@Test public void relativeHumidityCodeIllegalCode() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  0.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Qrh Illegal Code: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qrh Illegal Code: illegal value", line.contains("illegal value"));
		assertTrue("Qrh Illegal Code: Qrh", line.contains("'Qrh'"));
	}
	
	@Test public void uComponentCodeIllegalCode() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  0.0  4.0  2.0", 16, store, log);
		assertTrue("Qu Illegal Code: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qu Illegal Code: illegal value", line.contains("illegal value"));
		assertTrue("Qu Illegal Code: Qu", line.contains("'Qu'"));
	}
	
	@Test public void vComponentCodeIllegalCode() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  0.0  2.0", 16, store, log);
		assertTrue("Qv Illegal Code: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qv Illegal Code: illegal value", line.contains("illegal value"));
		assertTrue("Qv Illegal Code: Qv", line.contains("'Qv'"));
	}
	
	@Test public void ascentRateCodeIllegalCode() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  0.0", 16, store, log);
		assertTrue("QdZ Illegal Code: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("QdZ Illegal Code: illegal value", line.contains("illegal value"));
		assertTrue("QdZ Illegal Code: QdZ", line.contains("'QdZ'"));
	}
	
	@Test public void timeValueTooSmall() throws IOException {
		state.executeLineCheck("-1000.0 974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Time Too Small: ready", reader.ready());
		assertTrue("Time Too Small: time", reader.readLine().contains("'Time'"));
	}
	
	@Test public void pressureValueTooSmall() throws IOException {
		state.executeLineCheck("  48.0 -1000.0  8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Press Too Small: ready", reader.ready());
		assertTrue("Press Too Small: press", reader.readLine().contains("'Press'"));
	}
	
	@Test public void temperatureValueTooSmall() throws IOException {
		state.executeLineCheck("  48.0  974.5 -100.0  4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Temp Too Small: ready", reader.ready());
		assertTrue("Temp Too Small: temp", reader.readLine().contains("'Temp'"));
	}
	
	@Test public void dewPointValueTooSmall() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5 -100.0 74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Dewpt Too Small: ready", reader.ready());
		assertTrue("Dewpt Too Small: dewpt", reader.readLine().contains("'Dewpt'"));
	}
	
	@Test public void relativeHumidityValueTooSmall() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  -0.6   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("RH Too Small: ready", reader.ready());
		assertTrue("RH Too Small: RH", reader.readLine().contains("'RH'"));
	}
	
	@Test public void uComponentValueTooSmall() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7 -1000.0   6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Ucmp Too Small: ready", reader.ready());
		assertTrue("Ucmp Too Small: Ucmp", reader.readLine().contains("'Ucmp'"));
	}
	
	@Test public void vComponentValueTooSmall() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7 -1000.0  6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Vcmp Too Small: ready", reader.ready());
		assertTrue("Vcmp Too Small: Vcmp", reader.readLine().contains("'Vcmp'"));
	}
	
	@Test public void windSpeedValueTooSmall() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1  -0.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Spd Too Small: ready", reader.ready());
		assertTrue("Spd Too Small: spd", reader.readLine().contains("'spd'"));
	}
	
	@Test public void windDirectionValueTooSmall() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1  -0.1   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Dir Too Small: ready", reader.ready());
		assertTrue("Dir Too Small: dir", reader.readLine().contains("'dir'"));
	}
	
	@Test public void ascentRateValueTooSmall() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5-100.0 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Wcmp Too Small: ready", reader.ready());
		assertTrue("Wcmp Too Small: Wcmp", reader.readLine().contains("'Wcmp'"));
	}
	
	@Test public void longitudeValueTooSmall() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -180.001  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Lon Too Small: ready", reader.ready());
		assertTrue("Lon Too Small: Lon", reader.readLine().contains("'Lon'"));
	}
	
	@Test public void latitudeValueTooSmall() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201 -90.001  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Lat Too Small: ready", reader.ready());
		assertTrue("Lat Too Small: Lat", reader.readLine().contains("'Lat'"));
	}
	
	@Test public void variable1ValueTooSmall() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703-100.0 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Var 1 Too Small: ready", reader.ready());
		assertTrue("Var 1 Too Small: var 1", reader.readLine().contains("Variable Measurement 1"));
	}
	
	@Test public void variable2ValueTooSmall() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 -100.0  326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Var 2 Too Small: ready", reader.ready());
		assertTrue("Var 2 Too Small: Var 2", reader.readLine().contains("Variable Measurement 2"));
	}
	
	@Test public void altitudeValueTooSmall() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7-10000.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Alt Too Small: ready", reader.ready());
		assertTrue("Alt Too Small: Alt", reader.readLine().contains("'Alt'"));
	}
	
	@Test public void noSpaceAt6() throws IOException {
		state.executeLineCheck("  48.0X 974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("No Space @ 6: ready", reader.ready());
		assertTrue("No Space @ 6: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void noSpaceAt13() throws IOException {
		state.executeLineCheck("  48.0  974.5X  8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("No Space @ 13: ready", reader.ready());
		assertTrue("No Space @ 13: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void noSpaceAt19() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5X  4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("No Space @ 19: ready", reader.ready());
		assertTrue("No Space @ 19: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void noSpaceAt25() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3X 74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("No Space @ 25: ready", reader.ready());
		assertTrue("No Space @ 25: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void noSpaceAt31() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7X  -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("No Space @ 31: ready", reader.ready());
		assertTrue("No Space @ 31: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void noSpaceAt38() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7X   6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("No Space @ 38: ready", reader.ready());
		assertTrue("No Space @ 38: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void noSpaceAt45() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1X  6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("No Space @ 45: ready", reader.ready());
		assertTrue("No Space @ 45: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void noSpaceAt51() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1X173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("No Space @ 51: ready", reader.ready());
		assertTrue("No Space @ 51: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void noSpaceAt57() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5X  6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("No Space @ 57: ready", reader.ready());
		assertTrue("No Space @ 57: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void noSpaceAt63() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3X-122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("No Space @ 63: ready", reader.ready());
		assertTrue("No Space @ 63: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void noSpaceAt72() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201X 37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("No Space @ 72: ready", reader.ready());
		assertTrue("No Space @ 72: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void noSpaceAt80() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703X 48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("No Space @ 80: ready", reader.ready());
		assertTrue("No Space @ 80: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void noSpaceAt86() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4X162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("No Space @ 86: ready", reader.ready());
		assertTrue("No Space @ 86: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void noSpaceAt92() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7X  326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("No Space @ 92: ready", reader.ready());
		assertTrue("No Space @ 92: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void noSpaceAt100() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0X 1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("No Space @ 100: ready", reader.ready());
		assertTrue("No Space @ 100: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void noSpaceAt105() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0X 1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("No Space @ 105: ready", reader.ready());
		assertTrue("No Space @ 105: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void noSpaceAt110() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0X 1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("No Space @ 110: ready", reader.ready());
		assertTrue("No Space @ 110: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void noSpaceAt115() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0X 4.0  4.0  2.0", 16, store, log);
		assertTrue("No Space @ 115: ready", reader.ready());
		assertTrue("No Space @ 115: read log", reader.readLine().contains(" space"));
	}
	
	@Test public void noSpaceAt120() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0X 4.0  2.0", 16, store, log);
		assertTrue("No Space @ 120: ready", reader.ready());
		assertTrue("No Space @ 120: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void noSpaceAt125() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0X 2.0", 16, store, log);
		assertTrue("No Space @ 125: ready", reader.ready());
		assertTrue("No Space @ 125: read log", reader.readLine().contains(" space "));
	}
	
	@Test public void pressureCodeMissingWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  9.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Qp Value/M: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qp Value/M: mismatch", line.contains("mismatch"));
		assertTrue("Qp Value/M: press", line.contains("'Press'"));
	}
	
	@Test public void pressureCodeUncheckedWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0 99.0 99.0 99.0 99.0 99.0 99.0", 16, store, log);
		assertFalse("Qp Value/U: ready", reader.ready());
	}
	
	@Test public void pressureCodeGoodWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  1.0  1.0  1.0", 16, store, log);
		assertFalse("Qp Value/G: ready", reader.ready());
	}
	
	@Test public void pressureCodeBadWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  3.0  3.0  3.0  3.0  3.0  3.0", 16, store, log);
		assertFalse("Qp Value/B: ready", reader.ready());
	}
	
	@Test public void pressureCodeQuestionableWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  2.0  2.0  2.0  2.0  2.0  2.0", 16, store, log);
		assertFalse("Qp Value/Q: ready", reader.ready());
	}
	
	@Test public void pressureCodeEstimatedWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  4.0  4.0  4.0  4.0  4.0  4.0", 16, store, log);
		assertFalse("Qp Value/E: ready", reader.ready());
	}
	
	@Test public void temperatureCodeMissingWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  9.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Qt Value/M: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qt Value/M: mismatch", line.contains("mismatch"));
		assertTrue("Qt Value/M: temp", line.contains("'Temp'"));
	}
	
	@Test public void temperatureCodeUncheckedWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0 99.0 99.0 99.0 99.0 99.0 99.0", 16, store, log);
		assertFalse("Qt Value/U: ready", reader.ready());
	}
	
	@Test public void temperatureCodeGoodWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  1.0  1.0  1.0", 16, store, log);
		assertFalse("Qt Value/G: ready", reader.ready());
	}
	
	@Test public void temperatureCodeBadWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  3.0  3.0  3.0  3.0  3.0  3.0", 16, store, log);
		assertFalse("Qt Value/B: ready", reader.ready());
	}
	
	@Test public void temperatureCodeQuestionableWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  2.0  2.0  2.0  2.0  2.0  2.0", 16, store, log);
		assertFalse("Qt Value/Q: ready", reader.ready());
	}
	
	@Test public void temperatureCodeEstimatedWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  4.0  4.0  4.0  4.0  4.0  4.0", 16, store, log);
		assertFalse("Qt Value/E: ready", reader.ready());
	}
	
	@Test public void relativeHumidityCodeMissingWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  9.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Qrh Value/M: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qrh Value/M: mismatch", line.contains("mismatch"));
		assertTrue("Qrh Value/M: rh", line.contains("'RH'"));
	}
	
	@Test public void relativeHumidityCodeUncheckedWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0 99.0 99.0 99.0 99.0 99.0 99.0", 16, store, log);
		assertFalse("Qrh Value/U: ready", reader.ready());
	}
	
	@Test public void relativeHumidityCodeGoodWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  1.0  1.0  1.0", 16, store, log);
		assertFalse("Qrh Value/G: ready", reader.ready());
	}
	
	@Test public void relativeHumidityCodeBadWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  3.0  3.0  3.0  3.0  3.0  3.0", 16, store, log);
		assertFalse("Qrh Value/B: ready", reader.ready());
	}
	
	@Test public void relativeHumidityCodeQuestionableWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  2.0  2.0  2.0  2.0  2.0  2.0", 16, store, log);
		assertFalse("Qrh Value/Q: ready", reader.ready());
	}
	
	@Test public void relativeHumidityCodeEstimatedWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  4.0  4.0  4.0  4.0  4.0  4.0", 16, store, log);
		assertFalse("Qrh Value/E: ready", reader.ready());
	}
	
	@Test public void uComponentCodeMissingWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  9.0  4.0  2.0", 16, store, log);
		assertTrue("Qu Value/M: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qu Value/M: mismatch", line.contains("mismatch"));
		assertTrue("Qu Value/M: Ucmp", line.contains("'Ucmp'"));
	}
	
	@Test public void uComponentCodeUncheckedWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0 99.0 99.0 99.0 99.0 99.0 99.0", 16, store, log);
		assertFalse("Qu Value/U: ready", reader.ready());
	}
	
	@Test public void uComponentCodeGoodWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  1.0  1.0  1.0", 16, store, log);
		assertFalse("Qu Value/G: ready", reader.ready());
	}
	
	@Test public void uComponentCodeBadWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  3.0  3.0  3.0  3.0  3.0  3.0", 16, store, log);
		assertFalse("Qu Value/B: ready", reader.ready());
	}
	
	@Test public void uComponentCodeQuestionableWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  2.0  2.0  2.0  2.0  2.0  2.0", 16, store, log);
		assertFalse("Qu Value/Q: ready", reader.ready());
	}
	
	@Test public void uComponentCodeEstimatedWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  4.0  4.0  4.0  4.0  4.0  4.0", 16, store, log);
		assertFalse("Qu Value/E: ready", reader.ready());
	}
	
	@Test public void vComponentCodeMissingWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  9.0  2.0", 16, store, log);
		assertTrue("Qv Value/M: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qv Value/M: mismatch", line.contains("mismatch"));
		assertTrue("Qv Value/M: Vcmp", line.contains("'Vcmp'"));
	}
	
	@Test public void vComponentCodeUncheckedWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0 99.0 99.0 99.0 99.0 99.0 99.0", 16, store, log);
		assertFalse("Qv Value/U: ready", reader.ready());
	}
	
	@Test public void vComponentCodeGoodWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  1.0  1.0  1.0", 16, store, log);
		assertFalse("Qv Value/G: ready", reader.ready());
	}
	
	@Test public void vComponentCodeBadWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  3.0  3.0  3.0  3.0  3.0  3.0", 16, store, log);
		assertFalse("Qv Value/B: ready", reader.ready());
	}
	
	@Test public void vComponentCodeQuestionableWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  2.0  2.0  2.0  2.0  2.0  2.0", 16, store, log);
		assertFalse("Qv Value/Q: ready", reader.ready());
	}
	
	@Test public void vComponentCodeEstimatedWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  4.0  4.0  4.0  4.0  4.0  4.0", 16, store, log);
		assertFalse("Qv Value/E: ready", reader.ready());
	}
	
	@Test public void ascentRateCodeMissingWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  9.0", 16, store, log);
		assertTrue("QdZ Value/M: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("QdZ Value/M: mismatch", line.contains("mismatch"));
		assertTrue("QdZ Value/M: Wcmp", line.contains("'Wcmp'"));
	}
	
	@Test public void ascentRateCodeUncheckedWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0 99.0 99.0 99.0 99.0 99.0 99.0", 16, store, log);
		assertFalse("QdZ Value/U: ready", reader.ready());
	}
	
	@Test public void ascentRateCodeGoodWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  1.0  1.0  1.0", 16, store, log);
		assertFalse("QdZ Value/G: ready", reader.ready());
	}
	
	@Test public void ascentRateCodeBadWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  3.0  3.0  3.0  3.0  3.0  3.0", 16, store, log);
		assertFalse("QdZ Value/B: ready", reader.ready());
	}
	
	@Test public void ascentRateCodeQuestionableWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  2.0  2.0  2.0  2.0  2.0  2.0", 16, store, log);
		assertFalse("QdZ Value/Q: ready", reader.ready());
	}
	
	@Test public void ascentRateCodeEstimatedWithRealValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  4.0  4.0  4.0  4.0  4.0  4.0", 16, store, log);
		assertFalse("QdZ Value/E: ready", reader.ready());
	}
	
	@Test public void pressureCodeMissingWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0 9999.0   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  9.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertFalse("Qp Missing/M: ready", reader.ready());
	}
	
	@Test public void pressureCodeUncheckedWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0 9999.0   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0 99.0 99.0 99.0 99.0 99.0 99.0", 16, store, log);
		assertTrue("Qp Missing/U: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qp Missing/U: mismatch", line.contains("mismatch"));
		assertTrue("Qp Missing/U: Press", line.contains("'Press'"));
	}
	
	@Test public void pressureCodeGoodWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0 9999.0   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  1.0  1.0  1.0", 16, store, log);
		assertTrue("Qp Missing/G: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qp Missing/G: mismatch", line.contains("mismatch"));
		assertTrue("Qp Missing/G: Press", line.contains("'Press'"));
	}
	
	@Test public void pressureCodeBadWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0 9999.0   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  3.0  3.0  3.0  3.0  3.0  3.0", 16, store, log);
		assertTrue("Qp Missing/B: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qp Missing/B: mismatch", line.contains("mismatch"));
		assertTrue("Qp Missing/B: Press", line.contains("'Press'"));
	}
	
	@Test public void pressureCodeQuestionableWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0 9999.0   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  2.0  2.0  2.0  2.0  2.0  2.0", 16, store, log);
		assertTrue("Qp Missing/Q: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qp Missing/Q: mismatch", line.contains("mismatch"));
		assertTrue("Qp Missing/Q: Press", line.contains("'Press'"));
	}
	
	@Test public void pressureCodeEstimatedWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0 9999.0   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  4.0  4.0  4.0  4.0  4.0  4.0", 16, store, log);
		assertTrue("Qp Missing/E: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qp Missing/E: mismatch", line.contains("mismatch"));
		assertTrue("Qp Missing/E: Press", line.contains("'Press'"));
	}
	
	@Test public void temperatureCodeMissingWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5 999.0 999.0  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  9.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertFalse("Qt Missing/M: ready", reader.ready());
	}
	
	@Test public void temperatureCodeUncheckedWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5 999.0   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0 99.0 99.0 99.0 99.0 99.0 99.0", 16, store, log);
		assertTrue("Qt Missing/U: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qt Missing/U: mismatch", line.contains("mismatch"));
		assertTrue("Qt Missing/U: temp", line.contains("'Temp'"));
	}
	
	@Test public void temperatureCodeGoodWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5 999.0   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  1.0  1.0  1.0", 16, store, log);
		assertTrue("Qt Missing/G: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qt Missing/G: mismatch", line.contains("mismatch"));
		assertTrue("Qt Missing/G: temp", line.contains("'Temp'"));
	}
	
	@Test public void temperatureCodeBadWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5 999.0   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  3.0  3.0  3.0  3.0  3.0  3.0", 16, store, log);
		assertTrue("Qt Missing/B: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qt Missing/B: mismatch", line.contains("mismatch"));
		assertTrue("Qt Missing/B: temp", line.contains("'Temp'"));
	}
	
	@Test public void temperatureCodeQuestionableWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5 999.0   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  2.0  2.0  2.0  2.0  2.0  2.0", 16, store, log);
		assertTrue("Qt Missing/Q: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qt Missing/Q: mismatch", line.contains("mismatch"));
		assertTrue("Qt Missing/Q: temp", line.contains("'Temp'"));
	}
	
	@Test public void temperatureCodeEstimatedWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5 999.0   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  4.0  4.0  4.0  4.0  4.0  4.0", 16, store, log);
		assertTrue("Qt Missing/E: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qt Missing/E: mismatch", line.contains("mismatch"));
		assertTrue("Qt Missing/E: temp", line.contains("'Temp'"));
	}
	
	@Test public void relativeHumidityCodeMissingWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5 999.0 999.0   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  9.0  4.0  4.0  2.0", 16, store, log);
		assertFalse("Qrh Missing/M: ready", reader.ready());
	}
	
	@Test public void relativeHumidityCodeUncheckedWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3 999.0   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0 99.0 99.0 99.0 99.0 99.0 99.0", 16, store, log);
		assertTrue("Qrh Missing/U: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qrh Missing/U: mismatch", line.contains("mismatch"));
		assertTrue("Qrh Missing/U: rh", line.contains("'RH'"));
	}
	
	@Test public void relativeHumidityCodeGoodWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3 999.0   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  1.0  1.0  1.0", 16, store, log);
		assertTrue("Qrh Missing/G: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qrh Missing/G: mismatch", line.contains("mismatch"));
		assertTrue("Qrh Missing/G: rh", line.contains("'RH'"));
	}
	
	@Test public void relativeHumidityCodeBadWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3 999.0   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  3.0  3.0  3.0  3.0  3.0  3.0", 16, store, log);
		assertTrue("Qrh Missing/B: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qrh Missing/B: mismatch", line.contains("mismatch"));
		assertTrue("Qrh Missing/B: rh", line.contains("'RH'"));
	}
	
	@Test public void relativeHumidityCodeQuestionableWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3 999.0   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  2.0  2.0  2.0  2.0  2.0  2.0", 16, store, log);
		assertTrue("Qrh Missing/Q: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qrh Missing/Q: mismatch", line.contains("mismatch"));
		assertTrue("Qrh Missing/Q: rh", line.contains("'RH'"));
	}
	
	@Test public void relativeHumidityCodeEstimatedWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3 999.0   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  4.0  4.0  4.0  4.0  4.0  4.0", 16, store, log);
		assertTrue("Qrh Missing/E: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qrh Missing/E: mismatch", line.contains("mismatch"));
		assertTrue("Qrh Missing/E: rh", line.contains("'RH'"));
	}
	
	@Test public void uComponentCodeMissingWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7 9999.0 9999.0 999.0 999.0   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  9.0  9.0  2.0", 16, store, log);
		assertFalse("Qu Missing/M: ready", reader.ready());
	}
	
	@Test public void uComponentCodeUncheckedWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7 9999.0    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0 99.0 99.0 99.0 99.0 99.0 99.0", 16, store, log);
		assertTrue("Qu Missing/U: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qu Missing/U: mismatch", line.contains("mismatch"));
		assertTrue("Qu Missing/U: Ucmp", line.contains("'Ucmp'"));
	}
	
	@Test public void uComponentCodeGoodWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7 9999.0    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  1.0  1.0  1.0", 16, store, log);
		assertTrue("Qu Missing/G: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qu Missing/G: mismatch", line.contains("mismatch"));
		assertTrue("Qu Missing/G: Ucmp", line.contains("'Ucmp'"));
	}
	
	@Test public void uComponentCodeBadWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7 9999.0    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  3.0  3.0  3.0  3.0  3.0  3.0", 16, store, log);
		assertTrue("Qu Missing/B: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qu Missing/B: mismatch", line.contains("mismatch"));
		assertTrue("Qu Missing/B: Ucmp", line.contains("'Ucmp'"));
	}
	
	@Test public void uComponentCodeQuestionableWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7 9999.0    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  2.0  2.0  2.0  2.0  2.0  2.0", 16, store, log);
		assertTrue("Qu Missing/Q: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qu Missing/Q: mismatch", line.contains("mismatch"));
		assertTrue("Qu Missing/Q: Ucmp", line.contains("'Ucmp'"));
	}
	
	@Test public void uComponentCodeEstimatedWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7 9999.0    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  4.0  4.0  4.0  4.0  4.0  4.0", 16, store, log);
		assertTrue("Qu Missing/E: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qu Missing/E: mismatch", line.contains("mismatch"));
		assertTrue("Qu Missing/E: Ucmp", line.contains("'Ucmp'"));
	}
	
	@Test public void vComponentCodeMissingWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7 9999.0 9999.0 999.0 999.0   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  9.0  9.0  2.0", 16, store, log);
		assertFalse("Qv Missing/M: ready", reader.ready());
	}
	
	@Test public void vComponentCodeUncheckedWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7 9999.0   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0 99.0 99.0 99.0 99.0 99.0 99.0", 16, store, log);
		assertTrue("Qv Missing/U: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qv Missing/U: mismatch", line.contains("mismatch"));
		assertTrue("Qv Missing/U: Vcmp", line.contains("'Vcmp'"));
	}
	
	@Test public void vComponentCodeGoodWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7 9999.0   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  1.0  1.0  1.0", 16, store, log);
		assertTrue("Qv Missing/G: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qv Missing/G: mismatch", line.contains("mismatch"));
		assertTrue("Qv Missing/G: Vcmp", line.contains("'Vcmp'"));
	}
	
	@Test public void vComponentCodeBadWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7 9999.0   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  3.0  3.0  3.0  3.0  3.0  3.0", 16, store, log);
		assertTrue("Qv Missing/B: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qv Missing/B: mismatch", line.contains("mismatch"));
		assertTrue("Qv Missing/B: Vcmp", line.contains("'Vcmp'"));
	}
	
	@Test public void vComponentCodeQuestionableWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7 9999.0   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  2.0  2.0  2.0  2.0  2.0  2.0", 16, store, log);
		assertTrue("Qv Missing/Q: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qv Missing/Q: mismatch", line.contains("mismatch"));
		assertTrue("Qv Missing/Q: Vcmp", line.contains("'Vcmp'"));
	}
	
	@Test public void vComponentCodeEstimatedWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7 9999.0   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  4.0  4.0  4.0  4.0  4.0  4.0", 16, store, log);
		assertTrue("Qv Missing/E: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Qv Missing/E: mismatch", line.contains("mismatch"));
		assertTrue("Qv Missing/E: Vcmp", line.contains("'Vcmp'"));
	}
	
	@Test public void ascentRateCodeMissingWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5 999.0 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  9.0", 16, store, log);
		assertFalse("QdZ Missing/M: ready", reader.ready());
	}
	
	@Test public void ascentRateCodeUncheckedWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5 999.0 -122.201  37.703  48.4 162.7   326.0 99.0 99.0 99.0 99.0 99.0 99.0", 16, store, log);
		assertTrue("QdZ Missing/U: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("QdZ Missing/U: mismatch", line.contains("mismatch"));
		assertTrue("QdZ Missing/U: Wcmp", line.contains("'Wcmp'"));
	}
	
	@Test public void ascentRateCodeGoodWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5 999.0 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  1.0  1.0  1.0", 16, store, log);
		assertTrue("QdZ Missing/G: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("QdZ Missing/G: mismatch", line.contains("mismatch"));
		assertTrue("QdZ Missing/G: Wcmp", line.contains("'Wcmp'"));
	}
	
	@Test public void ascentRateCodeBadWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5 999.0 -122.201  37.703  48.4 162.7   326.0  3.0  3.0  3.0  3.0  3.0  3.0", 16, store, log);
		assertTrue("QdZ Missing/B: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("QdZ Missing/B: mismatch", line.contains("mismatch"));
		assertTrue("QdZ Missing/B: Wcmp", line.contains("'Wcmp'"));
	}
	
	@Test public void ascentRateCodeQuestionableWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5 999.0 -122.201  37.703  48.4 162.7   326.0  2.0  2.0  2.0  2.0  2.0  2.0", 16, store, log);
		assertTrue("QdZ Missing/Q: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("QdZ Missing/Q: mismatch", line.contains("mismatch"));
		assertTrue("QdZ Missing/Q: Wcmp", line.contains("'Wcmp'"));
	}
	
	@Test public void ascentRateCodeEstimatedWithMissingValue() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5 999.0 -122.201  37.703  48.4 162.7   326.0  4.0  4.0  4.0  4.0  4.0  4.0", 16, store, log);
		assertTrue("QdZ Missing/E: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("QdZ Missing/E: mismatch", line.contains("mismatch"));
		assertTrue("QdZ Missing/E: Wcmp", line.contains("'Wcmp'"));
	}
	
	@Test public void windComponentFlagMismatch() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  2.0  2.0", 16, store, log);
		assertTrue("Wind Component Flag: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Wind Component Flag: mismatch", line.contains("mismatch"));
		assertTrue("Wind Component Flag: Qu", line.contains("'Qu'"));
		assertTrue("Wind Component Flag: Qv", line.contains("'Qv'"));
	}
	
	@Test public void inconsisantWindNoUComponent() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7 9999.0    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  9.0  4.0  2.0", 16, store, log);
		assertTrue("No U: ready", reader.ready());
		assertTrue("No U: flag mismatch", reader.readLine().contains("flags"));
		assertTrue("No U: ready 2", reader.ready());
		assertTrue("No U: conflicting values", reader.readLine().contains("conflicting wind values"));
	}

	@Test public void inconsisantWindNoVComponent() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7 9999.0   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  9.0  2.0", 16, store, log);
		assertTrue("No V: ready", reader.ready());
		assertTrue("No V: flag mismatch", reader.readLine().contains("flags"));
		assertTrue("No V: ready 2", reader.ready());
		assertTrue("No V: conflicting values", reader.readLine().contains("conflicting wind values"));
	}
	
	@Test public void inconsisantWindNoSpeed() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1 999.0 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("No Spd: ready", reader.ready());
		assertTrue("No Spd: conflicting values", reader.readLine().contains("conflicting wind values"));
	}

	@Test public void inconsisantWindNoDirection() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1   6.1 999.0   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("No Dir: ready", reader.ready());
		assertTrue("No Dir: conflicting values", reader.readLine().contains("conflicting wind values"));
	}
	
	@Test public void inconsisantWindNoUandV() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7 9999.0 9999.0   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  9.0  9.0  2.0", 16, store, log);
		assertTrue("No U & V: ready", reader.ready());
		assertTrue("No U & V: conflicting values", reader.readLine().contains("conflicting wind values"));
	}

	@Test public void inconsisantWindNoUandSpeed() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7 9999.0    6.1 999.0 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  9.0  4.0  2.0", 16, store, log);
		assertTrue("No U & Spd: ready", reader.ready());
		assertTrue("No U & Spd: flag mismatch", reader.readLine().contains("flags"));
		assertTrue("No U & Spd: ready 2", reader.ready());
		assertTrue("No U & Spd: conflicting values", reader.readLine().contains("conflicting wind values"));
	}

	@Test public void inconsisantWindNoUandDirection() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7 9999.0    6.1   6.1 999.0   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  9.0  4.0  2.0", 16, store, log);
		assertTrue("No U & Dir: ready", reader.ready());
		assertTrue("No U & Dir: flag mismatch", reader.readLine().contains("flags"));
		assertTrue("No U & Dir: ready 2", reader.ready());
		assertTrue("No U & Dir: conflicting values", reader.readLine().contains("conflicting wind values"));
	}

	@Test public void inconsisantWindNoVandSpeed() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7 9999.0 999.0 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  9.0  2.0", 16, store, log);
		assertTrue("No V & Spd: ready", reader.ready());
		assertTrue("No V & Spd: flag mismatch", reader.readLine().contains("flags"));
		assertTrue("No V & Spd: ready 2", reader.ready());
		assertTrue("No V & Spd: conflicting values", reader.readLine().contains("conflicting wind values"));
	}

	@Test public void inconsisantWindNoVandDirection() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7 9999.0   6.1 999.0   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  9.0  2.0", 16, store, log);
		assertTrue("No V & Dir: ready", reader.ready());
		assertTrue("No V & Dir: flag mismatch", reader.readLine().contains("flags"));
		assertTrue("No V & Dir: ready 2", reader.ready());
		assertTrue("No V & Dir: conflicting values", reader.readLine().contains("conflicting wind values"));
	}

	@Test public void inconsisantWindNoSpeedAndDirection() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7    6.1 999.0 999.0   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("No Spd & Dir: ready", reader.ready());
		assertTrue("No Spd & Dir: conflicting values", reader.readLine().contains("conflicting wind values"));
	}

	@Test public void inconsisantWindOnlyUComponent() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7   -0.7 9999.0 999.0 999.0   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  9.0  2.0", 16, store, log);
		assertTrue("Only U: ready", reader.ready());
		assertTrue("Only U: flag mismatch", reader.readLine().contains("flags"));
		assertTrue("Only U: ready 2", reader.ready());
		assertTrue("Only U: conflicting values", reader.readLine().contains("conflicting wind values"));
	}

	@Test public void inconsisantWindOnlyVComponent() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7 9999.0    6.1 999.0 999.0   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  9.0  4.0  2.0", 16, store, log);
		assertTrue("Only V: ready", reader.ready());
		assertTrue("Only V: flag mismatch", reader.readLine().contains("flags"));
		assertTrue("Only V: ready 2", reader.ready());
		assertTrue("Only V: conflicting values", reader.readLine().contains("conflicting wind values"));
	}
	
	@Test public void inconsisantWindOnlySpeed() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7 9999.0 9999.0   6.1 999.0   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  9.0  9.0  2.0", 16, store, log);
		assertTrue("Only Spd: ready", reader.ready());
		assertTrue("Only Spd: conflicting values", reader.readLine().contains("conflicting wind values"));
	}

	@Test public void inconsisantWindOnlyDirection() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3  74.7 9999.0 9999.0 999.0 175.4   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  9.0  9.0  2.0", 16, store, log);
		assertTrue("Only Dir: ready", reader.ready());
		assertTrue("Only Dir: conflicting values", reader.readLine().contains("conflicting wind values"));
	}
	
	@Test public void inconsistantDewPointNoDP() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5 999.0  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("No DewPt: ready", reader.ready());
		assertTrue("No DewPt: conflicting values", reader.readLine().contains("conflicting 'Dewpt'"));
	}
	
	@Test public void inconsistantDewPointNoRH() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5   4.3 999.0   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  9.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("No RH: ready", reader.ready());
		assertTrue("No RH: conflicting values", reader.readLine().contains("conflicting 'Dewpt'"));
	}
	
	@Test public void inconsistantDewPointNoTemp() throws IOException {
		state.executeLineCheck("  48.0  974.5 999.0   4.3  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  9.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("No Temp: ready", reader.ready());
		assertTrue("No Temp: conflicting values", reader.readLine().contains("conflicting 'Dewpt'"));
	}
	
	@Test public void inconsistantDewPointOnlyDP() throws IOException {
		state.executeLineCheck("  48.0  974.5 999.0   4.3 999.0   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  9.0  9.0  4.0  4.0  2.0", 16, store, log);
		assertTrue("Only DewPt: ready", reader.ready());
		assertTrue("Only DewPt: conflicting values", reader.readLine().contains("conflicting 'Dewpt'"));
	}
	
	@Test public void inconsistantDewPointOnlyRH() throws IOException {
		state.executeLineCheck("  48.0  974.5 999.0 999.0  74.7   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  9.0  1.0  4.0  4.0  2.0", 16, store, log);
		assertFalse("Only RH: ready", reader.ready());
	}
	
	@Test public void inconsistantDewPointOnlyTemp() throws IOException {
		state.executeLineCheck("  48.0  974.5   8.5 999.0 999.0   -0.7    6.1   6.1 173.5   6.3 -122.201  37.703  48.4 162.7   326.0  1.0  1.0  9.0  4.0  4.0  2.0", 16, store, log);
		assertFalse("Only Temp: ready", reader.ready());
	}
	
	@Test public void lineNumberCorrect() throws IOException {
		state.executeLineNumberCheck(16, log);
		assertFalse("Correct Line Number: ready", reader.ready());
	}
	
	@Test public void lineNumberTooSmall() throws IOException {
		state.executeLineNumberCheck(15, log);
		assertTrue("Too Small Line Number: ready", reader.ready());
		assertTrue("Too Small Line Number: log read", reader.readLine().contains(" expected on a line after 15"));
	}	
}
