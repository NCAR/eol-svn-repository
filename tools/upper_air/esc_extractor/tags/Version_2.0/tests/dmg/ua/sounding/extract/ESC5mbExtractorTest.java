package dmg.ua.sounding.extract;

import static dmg.ua.sounding.esc.ESCSoundingRecord.*;
import static dmg.util.PressureUtils.*;
import static dmg.util.VelocityUtils.*;
import static org.junit.Assert.*;

import dmg.ua.sounding.esc.*;

import java.io.*;
import org.junit.*;

public class ESC5mbExtractorTest {

	private static final File TEST_FILE = new File("testfiles", "testfile.cls");
	
	private ESC5mbExtractor extractor;
	private ESCSounding testSounding;
	
	private BufferedReader reader;
	private PrintWriter log;
	
	@Before public void setUp() throws Exception {
		PipedInputStream in = new PipedInputStream();
		PipedOutputStream out = new PipedOutputStream(in);
		
		reader = new BufferedReader(new InputStreamReader(in));
		log = new PrintWriter(out);
		
		extractor = new ESC5mbExtractor();
		testSounding = (new ESCSoundingParser(false)).parseFile(TEST_FILE).get(0);
	}
	
	@After public void tearDown() throws IOException {
		reader.close();
		log.close();
	}

	
	@Test public void headerComparison() throws IOException {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		
		BufferedReader orig = new BufferedReader(new StringReader(testSounding.toString()));
		BufferedReader intp = new BufferedReader(new StringReader(interpolated.toString()));
		
		for (int i = 1; i <= 15; i++) {
			assertEquals(String.format("Header Line %d", i), orig.readLine(), intp.readLine());
		}
		
		orig.close();
		intp.close();
	}
	
	@Test public void initialRecordHasPressure() {
		ESCSounding interpolated = extractor.extract(testSounding, log);		
		assertEquals("Init Record w/Pressure", testSounding.getRecords().get(0), interpolated.getRecords().get(0));
	}

	@Test public void initialRecordNoPressure() throws Exception {
		testSounding.getRecords().get(0).setPressure(null, MILLIBARS);
		ESCSounding interpolated = extractor.extract(testSounding, log);
		assertEquals("Init Record w/o Pressure", testSounding.getRecords().get(3), interpolated.getRecords().get(0));
	}
	
	@Test public void replacementOfUncheckedFlags() throws Exception {
		for (int i = 0; i < 5; i++) { testSounding.getRecords().get(i).setPressure(null, MILLIBARS); }
		ESCSounding interpolated = extractor.extract(testSounding, log);
		
		assertEquals("Unchecked Flag Replacement: Pressure", GOOD_FLAG, interpolated.getRecords().get(0).getPressureFlag());
		assertEquals("Unchecked Flag Replacement: Temperature", GOOD_FLAG, interpolated.getRecords().get(0).getTemperatureFlag());
		assertEquals("Unchecked Flag Replacement: RH", GOOD_FLAG, interpolated.getRecords().get(0).getRelativeHumidityFlag());
		assertEquals("Unchecked Flag Replacement: U Comp", GOOD_FLAG, interpolated.getRecords().get(0).getUComponentFlag());
		assertEquals("Unchecked Flag Replacement: V Comp", GOOD_FLAG, interpolated.getRecords().get(0).getVComponentFlag());
		assertEquals("Unchecked Flag Replacement: Ascent Rate", UNCHECKED_FLAG, interpolated.getRecords().get(0).getAscentRateFlag());
	}
	
	@Test public void keepingDefinedFlags() throws Exception {
		for (int i = 0; i < 6; i++) { testSounding.getRecords().get(i).setPressure(null, MILLIBARS); }
		ESCSounding interpolated = extractor.extract(testSounding, log);
		
		assertEquals("Keep Defined Flag: Pressure", QUESTIONABLE_FLAG, interpolated.getRecords().get(0).getPressureFlag());
		assertEquals("Keep Defined Flag: Temperature", QUESTIONABLE_FLAG, interpolated.getRecords().get(0).getTemperatureFlag());
		assertEquals("Keep Defined Flag: RH", QUESTIONABLE_FLAG, interpolated.getRecords().get(0).getRelativeHumidityFlag());
		assertEquals("Keep Defined Flag: U Comp", QUESTIONABLE_FLAG, interpolated.getRecords().get(0).getUComponentFlag());
		assertEquals("Keep Defined Flag: V Comp", QUESTIONABLE_FLAG, interpolated.getRecords().get(0).getVComponentFlag());
		assertEquals("Keep Defined Flag: Ascent Rate", UNCHECKED_FLAG, interpolated.getRecords().get(0).getAscentRateFlag());
	}
	
	@Test public void expectedPressureRecords() throws Exception {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		int index = 1;
		for (double expected = 875.0; expected >= 50.0; expected =- 5.0) {
			assertEquals(String.format("Expected Pressure: %.1f on line %d", expected, index), expected, interpolated.getRecords().get(index++).getPressure());			
		}
	}
	
	@Test public void minimumExpectedPressure() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		assertEquals("Min Expected Pressure", 90.0, interpolated.getRecords().get(interpolated.getRecords().size() - 1).getPressure());
	}
	
	@Test public void minimumExpectedPressurePastMinAllowed() throws Exception {
		ESCSoundingRecord fifty = new ESCSoundingRecord();
		fifty.setTime(5000.0);
		fifty.setPressure(50.0, MILLIBARS);
		ESCSoundingRecord forty = new ESCSoundingRecord();
		forty.setTime(6000.0);
		forty.setPressure(40.0, MILLIBARS);
		testSounding.add(fifty);
		testSounding.add(forty);
		
		ESCSounding interpolated = extractor.extract(testSounding, log);
		assertEquals("Min Expected Pressure: Min Allowed", 50.0, interpolated.getRecords().get(interpolated.getRecords().size() - 1).getPressure());
	}
	
	@Test public void recordFoundOnIntervalLastRecord() throws Exception {
		ESCSoundingRecord fifty = new ESCSoundingRecord();
		fifty.setTime(5000.0);
		fifty.setPressure(50.0, MILLIBARS);
		testSounding.add(fifty);
		
		ESCSounding interpolated = extractor.extract(testSounding, log);
		assertEquals("Min Expected Pressure: Last Record", 50.0, interpolated.getRecords().get(interpolated.getRecords().size() - 1).getPressure());
	}
	
	@Test public void recordFoundOnIntervalWithNoNeedToInterpolateGood() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(6);
		
		assertEquals("Record Found: No Intp Good Flags: Time", 87.0, record.getTime());
		assertEquals("Record Found: No Intp Good Flags: Press", 850.0, record.getPressure());
		assertEquals("Record Found: No Intp Good Flags: Temp", 9.1, record.getTemperature());
		assertNotNull("Record Found: No Intp Good Flags: Dewpt", record.getDewPoint());
		assertEquals("Record Found: No Intp Good Flags: RH", 33.4, record.getRelativeHumidity());
		assertEquals("Record Found: No Intp Good Flags: UCmp", -4.0, record.getUComponent());
		assertEquals("Record Found: No Intp Good Flags: VCmp", 8.5, record.getVComponent());
		assertNotNull("Record Found: No Intp Good Flags: Wspd", record.getWindSpeed());
		assertNotNull("Record Found: No Intp Good Flags: Wdir", record.getWindDirection());
		assertEquals("Record Found: No Intp Good Flags: Wcmp", 2.2, record.getAscentRate());
		assertEquals("Record Found: No Intp Good Flags: Lon", -118.181, record.getLongitude());
		assertEquals("Record Found: No Intp Good Flags: Lat", 36.794, record.getLatitude());
		assertNull("Record Found: No Intp Good Flags: Var 1", record.getVariableField1());
		assertNull("Record Found: No Intp Good Flags: Var 2", record.getVariableField2());
		assertEquals("Record Found: No Intp Good Flags: Alt", 1483.6, record.getAltitude());
		assertEquals("Record Found: No Intp Good Flag: Qp", GOOD_FLAG, record.getPressureFlag());
		assertEquals("Record Found: No Intp Good Flag: Qt", GOOD_FLAG, record.getTemperatureFlag());
		assertEquals("Record Found: No Intp Good Flag: Qrh", GOOD_FLAG, record.getRelativeHumidityFlag());
		assertEquals("Record Found: No Intp Good Flag: Qu", GOOD_FLAG, record.getUComponentFlag());
		assertEquals("Record Found: No Intp Good Flag: Qv", GOOD_FLAG, record.getVComponentFlag());
		assertEquals("Record Found: No Intp Good Flag: QdZ", UNCHECKED_FLAG, record.getAscentRateFlag());
	}

	@Test public void recordFoundOnIntervalWithNoNeedToInterpolateQuestionable() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(5);
		
		assertEquals("Record Found: No Intp Quest Flags: Time", 69.0, record.getTime());
		assertEquals("Record Found: No Intp Quest Flags: Press", 855.0, record.getPressure());
		assertEquals("Record Found: No Intp Quest Flags: Temp", 9.6, record.getTemperature());
		assertNotNull("Record Found: No Intp Quest Flags: Dewpt", record.getDewPoint());
		assertEquals("Record Found: No Intp Quest Flags: RH", 32.4, record.getRelativeHumidity());
		assertEquals("Record Found: No Intp Quest Flags: UCmp", -4.8, record.getUComponent());
		assertEquals("Record Found: No Intp Quest Flags: VCmp", 8.3, record.getVComponent());
		assertNotNull("Record Found: No Intp Quest Flags: Wspd", record.getWindSpeed());
		assertNotNull("Record Found: No Intp Quest Flags: Wdir", record.getWindDirection());
		assertEquals("Record Found: No Intp Quest Flags: Wcmp", 4.0, record.getAscentRate());
		assertEquals("Record Found: No Intp Quest Flags: Lon", -118.180, record.getLongitude());
		assertEquals("Record Found: No Intp Quest Flags: Lat", 36.793, record.getLatitude());
		assertNull("Record Found: No Intp Quest Flags: Var 1", record.getVariableField1());
		assertNull("Record Found: No Intp Quest Flags: Var 2", record.getVariableField2());
		assertEquals("Record Found: No Intp Quest Flags: Alt", 1435.6, record.getAltitude());
		assertEquals("Record Found: No Intp Quest Flag: Qp", QUESTIONABLE_FLAG, record.getPressureFlag());
		assertEquals("Record Found: No Intp Quest Flag: Qt", QUESTIONABLE_FLAG, record.getTemperatureFlag());
		assertEquals("Record Found: No Intp Quest Flag: Qrh", QUESTIONABLE_FLAG, record.getRelativeHumidityFlag());
		assertEquals("Record Found: No Intp Quest Flag: Qu", QUESTIONABLE_FLAG, record.getUComponentFlag());
		assertEquals("Record Found: No Intp Quest Flag: Qv", QUESTIONABLE_FLAG, record.getVComponentFlag());
		assertEquals("Record Found: No Intp Quest Flag: QdZ", UNCHECKED_FLAG, record.getAscentRateFlag());
	}

	@Test public void recordFoundOnIntervalNeedToInterpolateBad() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(4);
		
		assertEquals("Record Found: No Intp Bad Flags: Time", 54.2, record.getTime(), .1);
		assertEquals("Record Found: No Intp Bad Flags: Press", 860.0, record.getPressure());
		assertEquals("Record Found: No Intp Bad Flags: Temp", 10.0, record.getTemperature(), .1);
		assertNotNull("Record Found: No Intp Bad Flags: Dewpt", record.getDewPoint());
		assertEquals("Record Found: No Intp Bad Flags: RH", 31.8, record.getRelativeHumidity(), .1);
		assertEquals("Record Found: No Intp Bad Flags: UCmp", -3.2, record.getUComponent(), .1);
		assertEquals("Record Found: No Intp Bad Flags: VCmp", 8.7, record.getVComponent(), .1);
		assertNotNull("Record Found: No Intp Bad Flags: Wspd", record.getWindSpeed());
		assertNotNull("Record Found: No Intp Bad Flags: Wdir", record.getWindDirection());
		assertEquals("Record Found: No Intp Bad Flags: Wcmp", 2.5, record.getAscentRate(), .1);
		assertEquals("Record Found: No Intp Bad Flags: Lon", -118.180, record.getLongitude(), .3);
		assertEquals("Record Found: No Intp Bad Flags: Lat", 36.792, record.getLatitude(), .3);
		assertNull("Record Found: No Intp Bad Flags: Var 1", record.getVariableField1());
		assertNull("Record Found: No Intp Bad Flags: Var 2", record.getVariableField2());
		assertEquals("Record Found: No Intp Bad Flags: Alt", 1387.3, record.getAltitude(), .1);
		assertEquals("Record Found: No Intp Bad Flag: Qp", GOOD_FLAG, record.getPressureFlag());
		assertEquals("Record Found: No Intp Bad Flag: Qt", GOOD_FLAG, record.getTemperatureFlag());
		assertEquals("Record Found: No Intp Bad Flag: Qrh", GOOD_FLAG, record.getRelativeHumidityFlag());
		assertEquals("Record Found: No Intp Bad Flag: Qu", GOOD_FLAG, record.getUComponentFlag());
		assertEquals("Record Found: No Intp Bad Flag: Qv", GOOD_FLAG, record.getVComponentFlag());
		assertEquals("Record Found: No Intp Bad Flag: QdZ", UNCHECKED_FLAG, record.getAscentRateFlag());
	}
	
	@Test public void recordFoundOnIntervalNeedToInterpolateMissing() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(3);
		
		assertEquals("Record Found: No Intp Missing Flags: Time", 39.1, record.getTime(), .1);
		assertEquals("Record Found: No Intp Missing Flags: Press", 865.0, record.getPressure());
		assertEquals("Record Found: No Intp Missing Flags: Temp", 10.4, record.getTemperature(), .1);
		assertNotNull("Record Found: No Intp Missing Flags: Dewpt", record.getDewPoint());
		assertEquals("Record Found: No Intp Missing Flags: RH", 31.4, record.getRelativeHumidity(), .1);
		assertEquals("Record Found: No Intp Missing Flags: UCmp", -3.1, record.getUComponent(), .1);
		assertEquals("Record Found: No Intp Missing Flags: VCmp", 8.1, record.getVComponent(), .1);
		assertNotNull("Record Found: No Intp Missing Flags: Wspd", record.getWindSpeed());
		assertNotNull("Record Found: No Intp Missing Flags: Wdir", record.getWindDirection());
		assertEquals("Record Found: No Intp Missing Flags: Wcmp", 3.3, record.getAscentRate(), .1);
		assertEquals("Record Found: No Intp Missing Flags: Lon", -118.179, record.getLongitude(), .3);
		assertEquals("Record Found: No Intp Missing Flags: Lat", 36.790, record.getLatitude(), .3);
		assertNull("Record Found: No Intp Missing Flags: Var 1", record.getVariableField1());
		assertNull("Record Found: No Intp Missing Flags: Var 2", record.getVariableField2());
		assertEquals("Record Found: No Intp Missing Flags: Alt", 1338.5, record.getAltitude(), .1);
		assertEquals("Record Found: No Intp Missing Flag: Qp", GOOD_FLAG, record.getPressureFlag());
		assertEquals("Record Found: No Intp Missing Flag: Qt", GOOD_FLAG, record.getTemperatureFlag());
		assertEquals("Record Found: No Intp Missing Flag: Qrh", GOOD_FLAG, record.getRelativeHumidityFlag());
		assertEquals("Record Found: No Intp Missing Flag: Qu", GOOD_FLAG, record.getUComponentFlag());
		assertEquals("Record Found: No Intp Missing Flag: Qv", GOOD_FLAG, record.getVComponentFlag());
		assertEquals("Record Found: No Intp Missing Flag: QdZ", UNCHECKED_FLAG, record.getAscentRateFlag());
	}
	
	@Test public void findNearestPressuresGoodFlags() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(17);
		
		assertEquals("Intp Press Values: Find Good Flags: Time", 272.9, record.getTime(), .1);
		assertEquals("Intp Press Values: Find Good Flags: Press", 795.0, record.getPressure());
		assertEquals("Intp Press Values: Find Good Flags: Wcmp", 2.7, record.getAscentRate(), .1);
		assertEquals("Intp Press Values: Find Good Flags: Alt", 2032.3, record.getAltitude(), .1);
		assertEquals("Intp Press Values: Find Good Flag: Qp", GOOD_FLAG, record.getPressureFlag());
		assertEquals("Intp Press Values: Find Good Flag: QdZ", UNCHECKED_FLAG, record.getAscentRateFlag());		
	}
	
	@Test public void findNearestPressureEstimateFlags() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(18);
		
		assertEquals("Intp Press Values: Find Est Flags: Press", 790.0, record.getPressure());
		assertEquals("Intp Press Values: Find Est Flags: Time", 293.2, record.getTime(), .1);
		assertEquals("Intp Press Values: Find Est Flags: Wcmp", 2.5, record.getAscentRate(), .1);
		assertEquals("Intp Press Values: Find Est Flags: Alt", 2083.4, record.getAltitude(), .1);
		assertEquals("Intp Press Values: Find Est Flag: Qp", ESTIMATE_FLAG, record.getPressureFlag());
		assertEquals("Intp Press Values: Find Est Flag: QdZ", UNCHECKED_FLAG, record.getAscentRateFlag());				
	}
	
	@Test public void findNearestPressureQuestionableFlags() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(19);
		
		assertEquals("Intp Press Values: Find Quest Flags: Press", 785.0, record.getPressure());
		assertEquals("Intp Press Values: Find Quest Flags: Time", 312.9, record.getTime(), .1);
		assertEquals("Intp Press Values: Find Quest Flags: Wcmp", 2.7, record.getAscentRate(), .1);
		assertEquals("Intp Press Values: Find Quest Flags: Alt", 2135.1, record.getAltitude(), .1);
		assertEquals("Intp Press Values: Find Quest Flag: Qp", QUESTIONABLE_FLAG, record.getPressureFlag());
		assertEquals("Intp Press Values: Find Quest Flag: QdZ", UNCHECKED_FLAG, record.getAscentRateFlag());				
	}
	
	@Test public void findNearestPressureBadFlags() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(20);
		
		assertEquals("Intp Press Values: Find Bad Flags: Press", 780.0, record.getPressure());
		assertEquals("Intp Press Values: Find Bad Flags: Time", 331.3, record.getTime(), .1);
		assertEquals("Intp Press Values: Find Bad Flags: Wcmp", 2.8, record.getAscentRate(), .1);
		assertEquals("Intp Press Values: Find Bad Flags: Alt", 2186.5, record.getAltitude(), .1);
		assertEquals("Intp Press Values: Find Bad Flag: Qp", BAD_FLAG, record.getPressureFlag());
		assertEquals("Intp Press Values: Find Bad Flag: QdZ", UNCHECKED_FLAG, record.getAscentRateFlag());				
	}
	
	@Test public void findNearestPressureNoLowerPressure() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(22);
		
		assertEquals("Intp Press Values: No Lower Pressure: Press", 770.0, record.getPressure());
	}
	
	@Test public void findNearestPressureNoUpperPressure() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(24);
		
		assertEquals("Intp Press Values: No Upper Pressure: Press", 760.0, record.getPressure());
	}
		
	@Test public void findNearestPressureNoLowerAltitude() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(25);
		
		assertEquals("Intp Press Values: No Lower Alt: Press", 755.0, record.getPressure());
		assertEquals("Intp Press Values: No Lower Alt: Time", 405.7, record.getTime(), .1);
		assertNull("Intp Press Values: No Lower Alt: Wcmp", record.getAscentRate());
		assertNull("Intp Press Values: No Lower Alt: Alt", record.getAltitude());
		assertEquals("Intp Press Values: No Lower Alt: Qp", GOOD_FLAG, record.getPressureFlag());
		assertEquals("Intp Press Values: No Lower Alt: QdZ", MISSING_FLAG, record.getAscentRateFlag());				
	}
	
	@Test public void findNearestPressureNoUpperAltitude() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(26);
		
		assertEquals("Intp Press Values: No Upper Alt: Press", 750.0, record.getPressure());
		assertEquals("Intp Press Values: No Upper Alt: Time", 422.3, record.getTime(), .1);
		assertNull("Intp Press Values: No Upper Alt: Wcmp", record.getAscentRate());
		assertNull("Intp Press Values: No Upper Alt: Alt", record.getAltitude());
		assertEquals("Intp Press Values: No Upper Alt: Qp", GOOD_FLAG, record.getPressureFlag());
		assertEquals("Intp Press Values: No Upper Alt: QdZ", MISSING_FLAG, record.getAscentRateFlag());				
	}
	
	@Test public void findNearestPressureEqualsTimes() throws Exception {
		testSounding.getRecords().get(440).setTime(438.0);
		
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(27);
		
		assertEquals("Intp Press Values: Equal Times: Press", 745.0, record.getPressure());
		assertEquals("Intp Press Values: Equal Times: Time", 438.0, record.getTime(), .1);
		assertNull("Intp Press Values: Equal Times: Wcmp", record.getAscentRate());
		assertEquals("Intp Press Values: Equal Times: Alt", 2556.1, record.getAltitude(), .1);
		assertEquals("Intp Press Values: Equal Times: Qp", GOOD_FLAG, record.getPressureFlag());
		assertEquals("Intp Press Values: Equal Times: QdZ", MISSING_FLAG, record.getAscentRateFlag());				
		
		assertTrue("Intp Press Values: Equal Times: log generated", reader.ready());
	}
	
	@Test public void findNearestPressureEqualAltitudes() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(28);
		
		assertEquals("Intp Press Values: Equal Times: Press", 740.0, record.getPressure());
		assertEquals("Intp Press Values: Equal Times: Time", 451.5, record.getTime(), .1);
		assertEquals("Intp Press Values: Equal Times: Wcmp", 0.0, record.getAscentRate());
		assertEquals("Intp Press Values: Equal Times: Alt", 2607.5, record.getAltitude(), .1);
		assertEquals("Intp Press Values: Equal Times: Qp", GOOD_FLAG, record.getPressureFlag());
		assertEquals("Intp Press Values: Equal Times: QdZ", UNCHECKED_FLAG, record.getAscentRateFlag());				
	}
	
	@Test public void findNearestTemperatureGoodFlags() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(97);
		
		assertEquals("Intp Temp Values: Good Flags: Press", 395.0, record.getPressure());
		assertEquals("Intp Temp Values: Good Flags: Temp", -33.9, record.getTemperature(), .1);
		assertEquals("Intp Temp Values: Good Flags: Qt", GOOD_FLAG, record.getTemperatureFlag());
	}
	
	@Test public void findNearestTemperatureEstimateFlags() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(98);
		
		assertEquals("Intp Temp Values: Est Flags: Press", 390.0, record.getPressure());
		assertEquals("Intp Temp Values: Est Flags: Temp", -34.7, record.getTemperature(), .1);
		assertEquals("Intp Temp Values: Est Flags: Qt", ESTIMATE_FLAG, record.getTemperatureFlag());
	}
	
	@Test public void findNearestTemperatureQuestionableFlags() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(99);
		
		assertEquals("Intp Temp Values: Quest Flags: Press", 385.0, record.getPressure());
		assertEquals("Intp Temp Values: Quest Flags: Temp", -35.4, record.getTemperature(), .1);
		assertEquals("Intp Temp Values: Quest Flags: Qt", QUESTIONABLE_FLAG, record.getTemperatureFlag());
	}
	
	@Test public void findNearestTemperatureBadFlags() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(100);
		
		assertEquals("Intp Temp Values: Bad Flags: Press", 380.0, record.getPressure());
		assertEquals("Intp Temp Values: Bad Flags: Temp", -36.1, record.getTemperature(), .1);
		assertEquals("Intp Temp Values: Bad Flags: Qt", BAD_FLAG, record.getTemperatureFlag());
	}
	
	@Test public void findNearestTemperatureNoLowerTemp() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(101);
		
		assertEquals("Intp Temp Values: No Lower Temp: Press", 375.0, record.getPressure());
		assertNull("Intp Temp Values: No Lower Temp: Temp", record.getTemperature());
		assertNull("Intp Temp Values: No Lower Temp: Dewpt", record.getDewPoint());
		assertEquals("Intp Temp Values: No Lower Temp: Qt", MISSING_FLAG, record.getTemperatureFlag());
	}
	
	@Test public void findNearestTemperatureNoUpperTemp() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(102);
		
		assertEquals("Intp Temp Values: No Upper Temp: Press", 370.0, record.getPressure());
		assertNull("Intp Temp Values: No Upper Temp: Temp", record.getTemperature());
		assertNull("Intp Temp Values: No Upper Temp: Dewpt", record.getDewPoint());
		assertEquals("Intp Temp Values: No Upper Temp: Qt", MISSING_FLAG, record.getTemperatureFlag());
	}
	
	@Test public void findNearestRelativeHumidityGoodFlags() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(117);
		
		assertEquals("Intp RH Values: Good Flags: Press", 295.0, record.getPressure());
		assertEquals("Intp RH Values: Good Flags: RH", 11.6, record.getRelativeHumidity(), .1);
		assertEquals("Intp RH Values: Good Flags: Qrh", GOOD_FLAG, record.getRelativeHumidityFlag());
	}
	
	@Test public void findNearestRelativeHumidityEstimateFlags() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(118);
		
		assertEquals("Intp RH Values: Est Flags: Press", 290.0, record.getPressure());
		assertEquals("Intp RH Values: Est Flags: RH", 12.2, record.getRelativeHumidity(), .1);
		assertEquals("Intp RH Values: Est Flags: Qrh", ESTIMATE_FLAG, record.getRelativeHumidityFlag());
	}
	
	@Test public void findNearestRelativeHumidityQuestionableFlags() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(119);
		
		assertEquals("Intp RH Values: Quest Flags: Press", 285.0, record.getPressure());
		assertEquals("Intp RH Values: Quest Flags: RH", 13.3, record.getRelativeHumidity(), .1);
		assertEquals("Intp RH Values: Quest Flags: Qrh", QUESTIONABLE_FLAG, record.getRelativeHumidityFlag());
	}
	
	@Test public void findNearestRelativeHumidityBadFlags() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(120);
		
		assertEquals("Intp RH Values: Bad Flags: Press", 280.0, record.getPressure());
		assertEquals("Intp RH Values: Bad Flags: RH", 14.5, record.getRelativeHumidity(), .1);
		assertEquals("Intp RH Values: Bad Flags: Qrh", BAD_FLAG, record.getRelativeHumidityFlag());
	}
	
	@Test public void findNearestRelativeHumidityNoLowerRH() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(121);
		
		assertEquals("Intp RH Values: No Lower RH: Press", 275.0, record.getPressure());
		assertNull("Intp RH Values: No Lower RH: RH", record.getRelativeHumidity());
		assertNull("Intp RH Values: No Lower RH: Dew Pt", record.getDewPoint());
		assertEquals("Intp RH Values: No Lower RH: Qrh", MISSING_FLAG, record.getRelativeHumidityFlag());
	}
	
	@Test public void findNearestRelativeHumidityNoUpperRH() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(122);
		
		assertEquals("Intp RH Values: No Lower RH: Press", 270.0, record.getPressure());
		assertNull("Intp RH Values: No Lower RH: RH", record.getRelativeHumidity());
		assertNull("Intp RH Values: No Lower RH: Dew Pt", record.getDewPoint());
		assertEquals("Intp RH Values: No Lower RH: Qrh", MISSING_FLAG, record.getRelativeHumidityFlag());
	}
	
	@Test public void findNearestWindsGoodFlags() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(57);
		
		assertEquals("Intp Wind Values: Good Flags: Press", 595.0, record.getPressure());
		assertEquals("Intp Wind Values: Good Flags: U Cmp", 3.9, record.getUComponent(), .1);
		assertEquals("Intp Wind Values: Good Flags: V Cmp", 4.7, record.getVComponent(), .1);
		assertEquals("Intp Wind Values: Good Flags: Lat", 36.842, record.getLatitude(), .001);
		assertEquals("Intp Wind Values: Good Flags: Lon", -118.184, record.getLongitude(), .001);
		assertEquals("Intp Wind Values: Good Flags: U Flag", GOOD_FLAG, record.getUComponentFlag());
		assertEquals("Intp Wind Values: Good Flags: V Flag", GOOD_FLAG, record.getVComponentFlag());
	}
	
	@Test public void findNearestWindsEstimateFlags() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(58);
		
		assertEquals("Intp Wind Values: Est Flags: Press", 590.0, record.getPressure());
		assertEquals("Intp Wind Values: Est Flags: U Cmp", 5.0, record.getUComponent(), .1);
		assertEquals("Intp Wind Values: Est Flags: V Cmp", 5.1, record.getVComponent(), .1);
		assertEquals("Intp Wind Values: Est Flags: Lat", 36.843, record.getLatitude(), .001);
		assertEquals("Intp Wind Values: Est Flags: Lon", -118.183, record.getLongitude(), .001);
		assertEquals("Intp Wind Values: Est Flags: U Flag", ESTIMATE_FLAG, record.getUComponentFlag());
		assertEquals("Intp Wind Values: Est Flags: V Flag", ESTIMATE_FLAG, record.getVComponentFlag());
	}
	
	@Test public void findNearestWindsQuestionableFlags() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(59);
		
		assertEquals("Intp Wind Values: Quest Flags: Press", 585.0, record.getPressure());
		assertEquals("Intp Wind Values: Quest Flags: U Cmp", 5.6, record.getUComponent(), .1);
		assertEquals("Intp Wind Values: Quest Flags: V Cmp", 5.6, record.getVComponent(), .1);
		assertEquals("Intp Wind Values: Quest Flags: Lat", 36.844, record.getLatitude(), .001);
		assertEquals("Intp Wind Values: Quest Flags: Lon", -118.182, record.getLongitude(), .001);
		assertEquals("Intp Wind Values: Quest Flags: U Flag", QUESTIONABLE_FLAG, record.getUComponentFlag());
		assertEquals("Intp Wind Values: Quest Flags: V Flag", QUESTIONABLE_FLAG, record.getVComponentFlag());
	}
	
	@Test public void findNearestWindsBadFlags() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(60);
		
		assertEquals("Intp Wind Values: Bad Flags: Press", 580.0, record.getPressure());
		assertEquals("Intp Wind Values: Bad Flags: U Cmp", 7.6, record.getUComponent(), .1);
		assertEquals("Intp Wind Values: Bad Flags: V Cmp", 5.5, record.getVComponent(), .1);
		assertEquals("Intp Wind Values: Bad Flags: Lat", 36.550, record.getLatitude(), .001);
		assertEquals("Intp Wind Values: Bad Flags: Lon", -118.224, record.getLongitude(), .001);
		assertEquals("Intp Wind Values: Bad Flags: U Flag", BAD_FLAG, record.getUComponentFlag());
		assertEquals("Intp Wind Values: Bad Flags: V Flag", BAD_FLAG, record.getVComponentFlag());
	}
	
	@Test public void findNearestWindsNoLowerComponents() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(61);
		
		assertEquals("Intp Wind Values: No Lower Winds: Press", 575.0, record.getPressure());
		assertNull("Intp Wind Values: No Lower Winds: U Cmp", record.getUComponent());
		assertNull("Intp Wind Values: No Lower Winds: V Cmp", record.getVComponent());
		assertNull("Intp Wind Values: No Lower Winds: Lat", record.getLatitude());
		assertNull("Intp Wind Values: No Lower Winds: Lon", record.getLongitude());
		assertEquals("Intp Wind Values: No Lower Winds: U Flag", MISSING_FLAG, record.getUComponentFlag());
		assertEquals("Intp Wind Values: No Lower Winds: V Flag", MISSING_FLAG, record.getVComponentFlag());
	}
	
	@Test public void findNearestWindsNoUpperComponents() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(62);
		
		assertEquals("Intp Wind Values: No Upper Winds: Press", 570.0, record.getPressure());
		assertNull("Intp Wind Values: No Upper Winds: U Cmp", record.getUComponent());
		assertNull("Intp Wind Values: No Upper Winds: V Cmp", record.getVComponent());
		assertNull("Intp Wind Values: No Upper Winds: Lat", record.getLatitude());
		assertNull("Intp Wind Values: No Upper Winds: Lon", record.getLongitude());
		assertEquals("Intp Wind Values: No Upper Winds: U Flag", MISSING_FLAG, record.getUComponentFlag());
		assertEquals("Intp Wind Values: No Upper Winds: V Flag", MISSING_FLAG, record.getVComponentFlag());
	}
	
	@Test public void findNearestWindsMismatchedComponentValuesNullU() throws Exception {
		testSounding.getRecords().get(1035).setUComponentFlag(null);
		testSounding.getRecords().get(1035).setUComponent(null, METERS_PER_SECOND);
		
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(64);
		
		assertTrue("Intp Wind Values: Null U Mismatch: log ready", reader.ready());	
		assertTrue("Intp Wind Values: Null U Mismatch: log contents failure", reader.readLine().contains("record at pressure 565 could not be interpolated"));
		assertTrue("Intp Wind Values: Null U Mismatch: log contents cause", reader.readLine().contains("missing U or V component with a non-missing value"));
		
		assertEquals("Intp Wind Values: Null U Mismatch: Press", 560.0, record.getPressure());
	}
	
	@Test public void findNearestWindsMismatchedComponentValuesNullV() throws Exception {
		testSounding.getRecords().get(1035).setVComponentFlag(null);
		testSounding.getRecords().get(1035).setVComponent(null, METERS_PER_SECOND);
		
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(64);
		
		assertTrue("Intp Wind Values: Null V Mismatch: log ready", reader.ready());	
		assertTrue("Intp Wind Values: Null V Mismatch: log contents failure", reader.readLine().contains("record at pressure 565 could not be interpolated"));
		assertTrue("Intp Wind Values: Null V Mismatch: log contents cause", reader.readLine().contains("missing U or V component with a non-missing value"));
		
		assertEquals("Intp Wind Values: Null V Mismatch: Press", 560.0, record.getPressure());
	}
	
	@Test public void findNearestWindsMismatchedComponentFlags() throws Exception {
		testSounding.getRecords().get(1035).setVComponentFlag(BAD_FLAG);
		
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(64);
		
		assertTrue("Intp Wind Values: Flag Mismatch: log ready", reader.ready());	
		assertTrue("Intp Wind Values: Flag Mismatch: log contents failure", reader.readLine().contains("record at pressure 565 could not be interpolated"));
		assertTrue("Intp Wind Values: Flag Mismatch: log contents cause", reader.readLine().contains("U and V component flags do not match"));
		assertEquals("Intp Wind Values: Flag Mismatch: Press", 560.0, record.getPressure());
	}
	
	@Test public void findNearestWindsNoLowerLatitude() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(65);
		
		assertEquals("Intp Wind Values: No Lower Lat: Press", 555.0, record.getPressure());
		assertEquals("Intp Wind Values: No Lower Lat: U Cmp", 9.7, record.getUComponent(), .1);
		assertEquals("Intp Wind Values: No Lower Lat: V Cmp", 4.8, record.getVComponent(), .1);
		assertNull("Intp Wind Values: No Lower Lat: Lat", record.getLatitude());
		assertEquals("Intp Wind Values: No Lower Lat: Lon", -118.170, record.getLongitude(), .001);
		assertEquals("Intp Wind Values: No Lower Lat: U Flag", GOOD_FLAG, record.getUComponentFlag());
		assertEquals("Intp Wind Values: No Lower Lat: V Flag", GOOD_FLAG, record.getVComponentFlag());
	}
	
	@Test public void findNearestWindsNoUpperLatitude() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(66);
		
		assertEquals("Intp Wind Values: No Upper Lat: Press", 550.0, record.getPressure());
		assertEquals("Intp Wind Values: No Upper Lat: U Cmp", 9.6, record.getUComponent(), .1);
		assertEquals("Intp Wind Values: No Upper Lat: V Cmp", 4.7, record.getVComponent(), .1);
		assertNull("Intp Wind Values: No Upper Lat: Lat", record.getLatitude());
		assertEquals("Intp Wind Values: No Upper Lat: Lon", -118.168, record.getLongitude(), .001);
		assertEquals("Intp Wind Values: No Upper Lat: U Flag", GOOD_FLAG, record.getUComponentFlag());
		assertEquals("Intp Wind Values: No Upper Lat: V Flag", GOOD_FLAG, record.getVComponentFlag());
	}
	
	@Test public void findNearestWindsNoLowerLongitude() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(67);
		
		assertEquals("Intp Wind Values: No Lower Lon: Press", 545.0, record.getPressure());
		assertEquals("Intp Wind Values: No Lower Lon: U Cmp", 10.0, record.getUComponent(), .1);
		assertEquals("Intp Wind Values: No Lower Lon: V Cmp", 4.5, record.getVComponent(), .1);
		assertEquals("Intp Wind Values: No Lower Lon: Lat", 36.851, record.getLatitude(), .001);
		assertNull("Intp Wind Values: No Lower Lon: Lon", record.getLongitude());
		assertEquals("Intp Wind Values: No Lower Lon: U Flag", GOOD_FLAG, record.getUComponentFlag());
		assertEquals("Intp Wind Values: No Lower Lon: V Flag", GOOD_FLAG, record.getVComponentFlag());
	}
	
	@Test public void findNearsetWindsNoUpperLongitude() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(68);
		
		assertEquals("Intp Wind Values: No Lower Lon: Press", 540.0, record.getPressure());
		assertEquals("Intp Wind Values: No Lower Lon: U Cmp", 9.5, record.getUComponent(), .1);
		assertEquals("Intp Wind Values: No Lower Lon: V Cmp", 4.4, record.getVComponent(), .1);
		assertEquals("Intp Wind Values: No Lower Lon: Lat", 36.852, record.getLatitude(), .001);
		assertNull("Intp Wind Values: No Lower Lon: Lon", record.getLongitude());
		assertEquals("Intp Wind Values: No Lower Lon: U Flag", GOOD_FLAG, record.getUComponentFlag());
		assertEquals("Intp Wind Values: No Lower Lon: V Flag", GOOD_FLAG, record.getVComponentFlag());
	}
	
	@Test public void findNearestWindsLongitude180Wrapping() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		ESCSoundingRecord record = interpolated.getRecords().get(69);
		
		assertEquals("Intp Wind Values: 180 Lon Wrap: Press", 535.0, record.getPressure());
		assertEquals("Intp Wind Values: 180 Lon Wrap: Lon", -179.614, record.getLongitude(), .001);
		
		record = interpolated.getRecords().get(70);
		assertEquals("Intp Wind Values: 180 Lon Wrap 180 value: Press", 530.0, record.getPressure());
		assertEquals("Intp Wind Values: 180 Lon Wrap 180 value: Lon", 180.000, Math.abs(record.getLongitude()), .001);		
	}

	@Test public void missingRecordInclusion() {
		ESCSounding interpolated = extractor.extract(testSounding, log);
		
		testMissingRecord(interpolated.getRecords().get(21), 775.0);
		testMissingRecord(interpolated.getRecords().get(23), 765.0);
	}

	private void testMissingRecord(ESCSoundingRecord record, Double pressure) {
		assertNull("Missing Record: time", record.getTime());
		assertEquals("Missing Record: pressure", pressure, record.getPressure(), .1);
		assertNull("Missing Record: temp", record.getTemperature());
		assertNull("Missing Redord: dewpt", record.getDewPoint());
		assertNull("Missing Record: rh", record.getRelativeHumidity());
		assertNull("Missing Record: ucomp", record.getUComponent());
		assertNull("Missing Record: vcomp", record.getVComponent());
		assertNull("Missing Record: wind spd", record.getWindSpeed());
		assertNull("Missing Record: wind dir", record.getWindDirection());
		assertNull("Missing Record: ascent rate", record.getAscentRate());
		assertNull("Missing Record: lon", record.getLongitude());
		assertNull("Missing Record: lat", record.getLatitude());
		assertNull("Missing Record: var 1", record.getVariableField1());
		assertNull("Missing Record: var 2", record.getVariableField2());
		assertNull("Missing Record: alt", record.getAltitude());
		assertEquals("Missing Record: press flag", UNCHECKED_FLAG, record.getPressureFlag());
		assertEquals("Missing Record: temp flag", MISSING_FLAG, record.getTemperatureFlag());
		assertEquals("Missing Record: rh flag", MISSING_FLAG, record.getRelativeHumidityFlag());
		assertEquals("Missing Record: ucomp flag", MISSING_FLAG, record.getUComponentFlag());
		assertEquals("Missing Record: vcomp flag", MISSING_FLAG, record.getVComponentFlag());
		assertEquals("Missing Record: ascent rate flag", MISSING_FLAG, record.getAscentRateFlag());
	}
}

