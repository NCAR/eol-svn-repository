package dmg.ua.sounding.clean;

import static org.junit.Assert.*;

import dmg.ua.sounding.esc.*;
import dmg.ua.sounding.esc.ESCSoundingRecord.ESCFlag;
import dmg.util.*;

import java.io.*;
import java.util.*;
import org.junit.*;

public class ESCSoundingCleanerTest {

	private static ESCSoundingCleaner cleaner;
	private static ESCSoundingParser parser;
	
	private static final String TEST_DATA_DIR = "test_data";
	
	private PrintWriter log;
	private StringWriter logContent;
	
	@BeforeClass public static void setUpBeforeClass() throws Exception {
		cleaner = new ESCSoundingCleaner();
		parser = new ESCSoundingParser(false);
	}
	
	@Before public void setUp() throws IOException {
		logContent = new StringWriter();
		log = new PrintWriter(logContent);
	}
	
	@After public void teardown() { log.close(); }
	
	private void compareLog(String test, File goal) throws IOException {
		// Flush to make sure all of the data has been put into the StringWriter.
		log.flush();
		String line = null;

		// Load the goal log lines into a list to be compared later.
		BufferedReader goalReader = new BufferedReader(new FileReader(goal));
		List<String> goals = new ArrayList<String>();
		while ((line = goalReader.readLine()) != null) { goals.add(line); }
		
		// Load the mod log lines into a list to be compared later.
		BufferedReader modReader = new BufferedReader(new StringReader(logContent.toString()));
		List<String> mods = new ArrayList<String>();
		while ((line = modReader.readLine()) != null) { mods.add(line); }
		
		// First make sure that there are the same number of lines (The logs can't match if they are different.)
		assertEquals(String.format("%s: Log Line Count", test), mods.size(), goals.size());
		
		// Compare each line of the mod log and the goal log for content differences.
		for (int i = 0; i < mods.size(); i++) {
			assertEquals(String.format("%s: Log Line %d", test, i), mods.get(i), goals.get(i));
		}		
	}
	
	private void compareRecords(String test, int recordNumber, ESCSoundingRecord mod, ESCSoundingRecord goal) {
		compareRecordValues(String.format("%s: Record %d: Time", test, recordNumber), mod.getTime(), goal.getTime(), .1);
		compareRecordValues(String.format("%s: Record %d: Press", test, recordNumber), mod.getPressure(), goal.getPressure(), .1);
		compareRecordValues(String.format("%s: Record %d: Temp", test, recordNumber), mod.getTemperature(), goal.getTemperature(), .1);
		compareRecordValues(String.format("%s: Record %d: DewPt", test, recordNumber), mod.getDewPoint(), goal.getDewPoint(), .1);
		compareRecordValues(String.format("%s: Record %d: RH", test, recordNumber), mod.getRelativeHumidity(), goal.getRelativeHumidity(), .1);
		compareRecordValues(String.format("%s: Record %d: UComp", test, recordNumber), mod.getUComponent(), goal.getUComponent(), .1);
		compareRecordValues(String.format("%s: Record %d: VComp", test, recordNumber), mod.getVComponent(), goal.getVComponent(), .1);
		compareRecordValues(String.format("%s: Record %d: WindSpd", test, recordNumber), mod.getWindSpeed(), goal.getWindSpeed(), .1);
		compareRecordValues(String.format("%s: Record %d: WindDir", test, recordNumber), mod.getWindDirection(), goal.getWindDirection(), .1);
		compareRecordValues(String.format("%s: Record %d: Zcmp", test, recordNumber), mod.getAscentRate(), goal.getAscentRate(), .1);
		compareRecordValues(String.format("%s: Record %d: Lon", test, recordNumber), mod.getLongitude(), goal.getLongitude(), .001);
		compareRecordValues(String.format("%s: Record %d: Lat", test, recordNumber), mod.getLatitude(), goal.getLatitude(), .001);
		compareRecordValues(String.format("%s: Record %d: Var 1", test, recordNumber), mod.getVariableField1(), goal.getVariableField1(), .1);
		compareRecordValues(String.format("%s: Record %d: Var 2", test, recordNumber), mod.getVariableField2(), goal.getVariableField2(), .1);
		compareRecordValues(String.format("%s: Record %d: Alt", test, recordNumber), mod.getAltitude(), goal.getAltitude(), .1);
		compareRecordFlags(String.format("%s: Record %d: Qp", test, recordNumber), mod.getPressureFlag(), goal.getPressureFlag());
		compareRecordFlags(String.format("%s: Record %d: Qt", test, recordNumber), mod.getTemperatureFlag(), goal.getTemperatureFlag());
		compareRecordFlags(String.format("%s: Record %d: Qrh", test, recordNumber), mod.getRelativeHumidityFlag(), goal.getRelativeHumidityFlag());
		compareRecordFlags(String.format("%s: Record %d: Qu", test, recordNumber), mod.getUComponentFlag(), goal.getUComponentFlag());
		compareRecordFlags(String.format("%s: Record %d: Qv", test, recordNumber), mod.getVComponentFlag(), goal.getVComponentFlag());
		compareRecordFlags(String.format("%s: Record %d: Qz", test, recordNumber), mod.getAscentRateFlag(), goal.getAscentRateFlag());
	}
	
	private void compareRecordFlags(String test, ESCFlag mod, ESCFlag goal) {
		if (mod == null) {
			assertNull(test, goal);
		} else {
			assertEquals(test, mod, goal);
		}
	}
	
	private void compareRecordValues(String test, Double mod, Double goal, Double threshold) {
		if (mod == null) {
			assertNull(test, goal);
		} else {
			assertEquals(test, mod, goal, threshold);
		}
	}

	private void compareSoundings(String test, ESCSounding mod, ESCSounding goal) {
		assertEquals(String.format("%s: Actual Date", test), mod.getActualDate(), goal.getActualDate());
		assertEquals(String.format("%s: Altitude", test), mod.getAltitude(), goal.getAltitude(), .1);
		assertEquals(String.format("%s: Data Type", test), mod.getDataType(), goal.getDataType());
		assertEquals(String.format("%s: Latitude", test), mod.getLatitude(), goal.getLatitude(), .001);
		assertEquals(String.format("%s: Longitude", test), mod.getLongitude(), goal.getLongitude(), .001);
		assertEquals(String.format("%s: Nominal Date", test), mod.getNominalDate(), goal.getNominalDate());
		assertEquals(String.format("%s: Project", test), mod.getProjectId(), goal.getProjectId());
		assertEquals(String.format("%s: Release Direction", test), mod.getReleaseDirection(), goal.getReleaseDirection());
		assertEquals(String.format("%s: Station Description", test), mod.getStationDescription(), goal.getStationDescription());
		
		for (int i = 6; i <= 12; i++) {
			if (mod.getHeaderLine(i) == null) {
				assertNull(String.format("%s: Header Line: %d: Null", test, i), goal.getHeaderLine(i));
			} else {
				assertEquals(String.format("%s: Header Line %d: Label", test, i), mod.getHeaderLine(i).getLabel(), goal.getHeaderLine(i).getLabel());
				assertEquals(String.format("%s: Header Line %d: Content", test, i), mod.getHeaderLine(i).getContent(), goal.getHeaderLine(i).getContent());
			}
		}
		
		
		assertEquals(String.format("%s: Record Count", test), mod.getRecords().size(), goal.getRecords().size());
		
		for (int i = 0; i < mod.getRecords().size(); i++) {
			compareRecords(test, i + 1, mod.getRecords().get(i), goal.getRecords().get(i));
		}
	}
	
	@Test public void testBadPressureJumps() throws CleanerException, ConversionException, DateTimeException, InvalidValueException, InvalidValueWarning, IOException {
		List<ESCSounding> soundings = parser.parseFile(new File(TEST_DATA_DIR, "bad_pressure_jumps.cls"));
		cleaner.clean(soundings.get(0), log);
		compareSoundings("Bad Pressure Jumps", soundings.get(0), soundings.get(1));
		compareLog("Bad Pressure Jumps", new File(TEST_DATA_DIR, "bad_pressure_jumps.log"));
	}
	
	@Test public void testBadPressureDecreaseRates() throws CleanerException, ConversionException, DateTimeException, InvalidValueException, InvalidValueWarning, IOException {
		List<ESCSounding> soundings = parser.parseFile(new File(TEST_DATA_DIR, "bad_decrease_pressure_rates.cls"));
		cleaner.clean(soundings.get(0), log);
		compareSoundings("Bad Decrease In Pressure Rates", soundings.get(0), soundings.get(1));
		compareLog("Bad Decrease In Pressure Rates", new File(TEST_DATA_DIR, "bad_decrease_pressure_rates.log"));
	}
	
	@Test public void testBadAltitudeDrops() throws CleanerException, ConversionException, DateTimeException, InvalidValueException, InvalidValueWarning, IOException {
		List<ESCSounding> soundings = parser.parseFile(new File(TEST_DATA_DIR, "bad_altitude_drops.cls"));
		cleaner.clean(soundings.get(0), log);
		compareSoundings("Bad Altitude Drops", soundings.get(0), soundings.get(1));
		compareLog("Bad Altitude Drops", new File(TEST_DATA_DIR, "bad_altitude_drops.log"));
	}
	
	@Test public void testBadAltitudeIncreaseRates() throws CleanerException, ConversionException, DateTimeException, InvalidValueException, InvalidValueWarning, IOException {
		List<ESCSounding> soundings = parser.parseFile(new File(TEST_DATA_DIR, "bad_increase_altitude_rates.cls"));
		cleaner.clean(soundings.get(0), log);
		compareSoundings("Bad Increase In Altitude Rates", soundings.get(0), soundings.get(1));
		compareLog("Bad Increase In Altitude Rates", new File(TEST_DATA_DIR, "bad_increase_altitude_rates.log"));
	}
	
	@Test public void testBadZeroAltitudes() throws CleanerException, ConversionException, DateTimeException, InvalidValueException, InvalidValueWarning, IOException {
		List<ESCSounding> soundings = parser.parseFile(new File(TEST_DATA_DIR, "bad_zero_altitudes.cls"));
		cleaner.clean(soundings.get(0), log);
		compareSoundings("Bad Zero Altitudes", soundings.get(0), soundings.get(1));
		compareLog("Bad Zero Altitudes", new File(TEST_DATA_DIR, "bad_zero_altitudes.log"));
	}
	
	@Test public void testAcceptableZeroAltitudes() throws CleanerException, ConversionException, DateTimeException, InvalidValueException, InvalidValueWarning, IOException {
		List<ESCSounding> soundings = parser.parseFile(new File(TEST_DATA_DIR, "acceptable_zero_altitudes.cls"));
		cleaner.clean(soundings.get(0), log);
		compareSoundings("Acceptable Zero Altitudes", soundings.get(0), soundings.get(1));
		compareLog("Acceptable Zero Altitudes", new File(TEST_DATA_DIR, "acceptable_zero_altitudes.log"));
	}
	
	@Test public void testValidSounding() throws CleanerException, ConversionException, DateTimeException, InvalidValueException, InvalidValueWarning, IOException {
		List<ESCSounding> soundings = parser.parseFile(new File(TEST_DATA_DIR, "valid_sounding.cls"));
		cleaner.clean(soundings.get(0), log);
		compareSoundings("Valid Sounding", soundings.get(0), soundings.get(1));
		compareLog("Valid Sounding", new File(TEST_DATA_DIR, "valid_sounding.log"));
	}
}
