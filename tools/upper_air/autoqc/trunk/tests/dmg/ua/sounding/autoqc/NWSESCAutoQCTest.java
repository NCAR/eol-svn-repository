package dmg.ua.sounding.autoqc;

import static dmg.ua.sounding.esc.ESCSoundingRecord.BAD_FLAG;
import static dmg.ua.sounding.esc.ESCSoundingRecord.QUESTIONABLE_FLAG;
import static dmg.ua.sounding.esc.ESCSoundingRecord.UNCHECKED_FLAG;
import static dmg.util.LengthUtils.METERS;
import static org.junit.Assert.*;

import java.io.*;
import org.junit.*;

import dmg.ua.sounding.esc.ESCSoundingRecord;
import dmg.util.CalculationWarning;
import dmg.util.ConversionException;
import dmg.util.InvalidValueWarning;

public class NWSESCAutoQCTest extends UpsondeESCAutoQCTest {

	@BeforeClass public static void setUpBeforeClass() throws Exception {
		autoQC = new NWSESCAutoQC("upsonde_test_limits");
		coldQC = new NWSESCAutoQC("upsonde_cold_test_limits");
	}
		
    @Test public void ascentRateNullStartTime() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
		records.add(buildPTURecord(1013.0, 21.4, 50.0));
		records.add(buildPTURecord(1010.3, 20.8, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		records.get(0).setTime(null);
		records.get(2).setTime(10.0);
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		double returnedValue = 
			autoQC.differentialAscensionRate(records, 10.0, false, log);
		
		// Test the output
		assertFalse("Ascent Rate - No Start Time: log ready",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Ascent Rate - No Start Time: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Ascent Rate - No Start Time: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Ascent Rate - No Start Time: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Ascent Rate - No Start Time: ascent rate", 0.0,
				returnedValue);
    }
    
    @Test public void ascentRateNullEndTime() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
		records.add(buildPTURecord(1013.0, 21.4, 50.0));
		records.add(buildPTURecord(1010.3, 20.8, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		records.get(0).setTime(0.0);
		records.get(2).setTime(null);
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		double returnedValue = 
			autoQC.differentialAscensionRate(records, 10.0, false, log);
		
		// Test the output
		assertFalse("Ascent Rate - No End Time: log ready",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Ascent Rate - No End Time: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Ascent Rate - No End Time: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Ascent Rate - No End Time: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Ascent Rate - No End Time: ascent rate", 0.0,
				returnedValue);
    }
    
    @Test public void ascentRateNullStartAlt() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
		records.add(buildPTURecord(1013.0, 21.4, 50.0));
		records.add(buildPTURecord(1010.3, 20.8, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		records.get(0).setTime(0.0);
		records.get(2).setTime(10.0);
		
		records.get(0).setAltitude(null, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		double returnedValue = 
			autoQC.differentialAscensionRate(records, 10.0, false, log);
		
		// Test the output
		assertFalse("Ascent Rate - No Start Alt: log ready",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Ascent Rate - No Start Alt: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Ascent Rate - No Start Alt: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Ascent Rate - No Start Alt: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Ascent Rate - No Start Alt: ascent rate", 0.0,
				returnedValue);
    }
    
    @Test public void ascentRateNullEndAlt() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
		records.add(buildPTURecord(1013.0, 21.4, 50.0));
		records.add(buildPTURecord(1010.3, 20.8, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		records.get(0).setTime(0.0);
		records.get(2).setTime(10.0);
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(null, METERS);
		
		double returnedValue = 
			autoQC.differentialAscensionRate(records, 10.0, false, log);
		
		// Test the output
		assertFalse("Ascent Rate - No End Alt: log ready",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Ascent Rate - No End Alt: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Ascent Rate - No End Alt: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Ascent Rate - No End Alt: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Ascent Rate - No End Alt: ascent rate", 0.0,
				returnedValue);
    }
    
    @Test public void ascentRateEqualTimes() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
		records.add(buildPTURecord(1013.0, 21.4, 50.0));
		records.add(buildPTURecord(1010.3, 20.8, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		records.get(0).setTime(0.0);
		records.get(2).setTime(0.0);
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		double returnedValue = 
			autoQC.differentialAscensionRate(records, 3.0, false, log);
		
		// Test the output
		assertTrue("Ascent Rate - Equal Times: log ready - pre",
				reader.ready());
		assertFalse("Ascent Rate - Equal Times: log output",
				"".equals(reader.readLine()));
		assertFalse("Ascent Rate - Equal Times: log ready - post",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Ascent Rate - Equal Times: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Ascent Rate - Equal Times: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Ascent Rate - Equal Times: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Ascent Rate - Equal Times: ascent rate", 0.0,
				returnedValue);
    }
    
    @Test public void ascentRateEqualAltitudes() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
		records.add(buildPTURecord(1013.0, 21.4, 50.0));
		records.add(buildPTURecord(1010.3, 20.8, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		records.get(0).setTime(0.0);
		records.get(2).setTime(10.0);
		
		records.get(0).setAltitude(1000.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		double returnedValue = 
			autoQC.differentialAscensionRate(records, 3.0, false, log);
		
		// Test the output
		assertTrue("Ascent Rate - Equal Alts: log ready - pre",
				reader.ready());
		assertFalse("Ascent Rate - Equal Alts: log output",
				"".equals(reader.readLine()));
		assertFalse("Ascent Rate - Equal Alts: log ready - post",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Ascent Rate - Equal Alts: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Ascent Rate - Equal Alts: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Ascent Rate - Equal Alts: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Ascent Rate - Equal Alts: ascent rate", 0.0,
				returnedValue);
    }
    
    @Test public void ascentRateNullPreviousRate() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
		records.add(buildPTURecord(1013.0, 21.4, 50.0));
		records.add(buildPTURecord(1010.3, 20.8, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		records.get(0).setTime(0.0);
		records.get(2).setTime(10.0);
		
		records.get(0).setAltitude(1000.0, METERS);
		records.get(2).setAltitude(1029.0, METERS);
		
		double returnedValue = 
			autoQC.differentialAscensionRate(records, null, false, log);
		
		// Test the output
		assertFalse("Ascent Rate - Null Prev Asc Rate: log ready",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Ascent Rate - Null Prev Asc Rate: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Ascent Rate - Null Prev Asc Rate: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Ascent Rate - Null Prev Asc Rate: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Ascent Rate - Null Prev Asc Rate: ascent rate", 2.9,
				returnedValue);
    }
    
    @Test public void ascentRateValidPreviousRate() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
		records.add(buildPTURecord(1013.0, 21.4, 50.0));
		records.add(buildPTURecord(1010.3, 20.8, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		records.get(0).setTime(0.0);
		records.get(2).setTime(10.0);
		
		records.get(0).setAltitude(1000.0, METERS);
		records.get(2).setAltitude(1029.0, METERS);
		
		double returnedValue = 
			autoQC.differentialAscensionRate(records, 1.0, false, log);
		
		// Test the output
		assertFalse("Ascent Rate - Prev Asc Rate: log ready",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Ascent Rate - Prev Asc Rate: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Ascent Rate - Prev Asc Rate: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Ascent Rate - Prev Asc Rate: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Ascent Rate - Prev Asc Rate: ascent rate", 2.9,
				returnedValue);
    }
    
    @Test public void ascentRateBegin() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
		records.add(buildPTURecord(1013.0, 21.4, 50.0));
		records.add(buildPTURecord(1010.3, 20.8, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		records.get(0).setTime(0.0);
		records.get(2).setTime(10.0);
		
		records.get(0).setAltitude(1000.0, METERS);
		records.get(2).setAltitude(1039.0, METERS);
		
		double returnedValue = 
			autoQC.differentialAscensionRate(records, 1.0, true, log);
		
		// Test the output
		assertFalse("Ascent Rate - Begin == true: log ready",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Ascent Rate - Begin == true: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Ascent Rate - Begin == true: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Ascent Rate - Begin == true: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Ascent Rate - Begin == true: ascent rate", 3.9,
				returnedValue);
    }
    
    @Test public void ascentRateNotBegin() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
		records.add(buildPTURecord(1013.0, 21.4, 50.0));
		records.add(buildPTURecord(1010.3, 20.8, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		records.get(0).setTime(0.0);
		records.get(2).setTime(10.0);
		
		records.get(0).setAltitude(1000.0, METERS);
		records.get(2).setAltitude(1039.0, METERS);
		
		double returnedValue = 
			autoQC.differentialAscensionRate(records, 0.0, false, log);
		
		// Test the output
		assertTrue("Ascent Rate - Begin == false: log ready - pre",
				reader.ready());
		assertFalse("Ascent Rate - Begin == false: log output",
				"".equals(reader.readLine()));
		assertFalse("Ascent Rate - Begin == false: log ready - post",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Ascent Rate - Begin == false: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Ascent Rate - Begin == false: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Ascent Rate - Begin == false: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Ascent Rate - Begin == false: ascent rate", 3.9,
				returnedValue);
    }
    
    @Test public void ascentRatePosQuestBound() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
		records.add(buildPTURecord(1013.0, 21.4, 50.0));
		records.add(buildPTURecord(1010.3, 20.8, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		records.get(0).setTime(0.0);
		records.get(2).setTime(10.0);
		
		records.get(0).setAltitude(1000.0, METERS);
		records.get(2).setAltitude(1029.9, METERS);
		
		double returnedValue = 
			autoQC.differentialAscensionRate(records, 0.0, false, log);
		
		// Test the output
		assertFalse("Ascent Rate - Pos Quest Bound: log ready",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Ascent Rate - Pos Quest Bound: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Ascent Rate - Pos Quest Bound: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Ascent Rate - Pos Quest Bound: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Ascent Rate - Pos Quest Bound: ascent rate", 2.99,
				returnedValue, THRESHOLD);
    }
    
    @Test public void ascentRatePosQuestBoundViolation() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
		records.add(buildPTURecord(1013.0, 21.4, 50.0));
		records.add(buildPTURecord(1010.3, 20.8, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		records.get(0).setTime(0.0);
		records.get(2).setTime(10.0);
		
		records.get(0).setAltitude(1000.0, METERS);
		records.get(2).setAltitude(1030.0, METERS);
		
		double returnedValue = 
			autoQC.differentialAscensionRate(records, 0.0, false, log);
		
		// Test the output
		assertTrue("Ascent Rate - Pos Quest Bound Violation: log ready- pre",
				reader.ready());
		assertFalse("Ascent Rate - Pos Quest Bound Violation: log output",
				"".equals(reader.readLine()));		
		assertFalse("Ascent Rate - Pos Quest Bound Violation: log ready - post",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Ascent Rate - Pos Quest Bound Violation: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Ascent Rate - Pos Quest Bound Violation: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Ascent Rate - Pos Quest Bound Violation: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Ascent Rate - Pos Quest Bound Violation: ascent rate", 3.0,
				returnedValue, THRESHOLD);
    }
    
    @Test public void ascentRatePosBadBound() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
		records.add(buildPTURecord(1013.0, 21.4, 50.0));
		records.add(buildPTURecord(1010.3, 20.8, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		records.get(0).setTime(0.0);
		records.get(2).setTime(10.0);
		
		records.get(0).setAltitude(1000.0, METERS);
		records.get(2).setAltitude(1049.9, METERS);
		
		double returnedValue = 
			autoQC.differentialAscensionRate(records, 0.0, false, log);
		
		// Test the output
		assertTrue("Ascent Rate - Pos Bad Bound: log ready- pre",
				reader.ready());
		assertFalse("Ascent Rate - Pos Bad Bound: log output",
				"".equals(reader.readLine()));		
		assertFalse("Ascent Rate - Pos Bad Bound: log ready - post",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Ascent Rate - Pos Bad Bound: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Ascent Rate - Pos Bad Bound: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Ascent Rate - Pos Bad Bound: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Ascent Rate - Pos Bad Bound: ascent rate", 4.99,
				returnedValue, THRESHOLD);
    }
    
    @Test public void ascentRatePosBadBoundViolation() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
		records.add(buildPTURecord(1013.0, 21.4, 50.0));
		records.add(buildPTURecord(1010.3, 20.8, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		records.get(0).setTime(0.0);
		records.get(2).setTime(10.0);
		
		records.get(0).setAltitude(1000.0, METERS);
		records.get(2).setAltitude(1050.0, METERS);
		
		double returnedValue = 
			autoQC.differentialAscensionRate(records, 0.0, false, log);
		
		// Test the output
		assertTrue("Ascent Rate - Pos Bad Bound Violation: log ready- pre",
				reader.ready());
		assertFalse("Ascent Rate - Pos Bad Bound Violation: log output",
				"".equals(reader.readLine()));		
		assertFalse("Ascent Rate - Pos Bad Bound Violation: log ready - post",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Ascent Rate - Pos Bad Bound Violation: Press Flag "+i, 
					BAD_FLAG, records.get(i).getPressureFlag());
			assertEquals("Ascent Rate - Pos Bad Bound Violation: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Ascent Rate - Pos Bad Bound Violation: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Ascent Rate - Pos Bad Bound Violation: ascent rate", 5.0,
				returnedValue, THRESHOLD);
    }

    @Test public void ascentRateNegQuestBound() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
		records.add(buildPTURecord(1013.0, 21.4, 50.0));
		records.add(buildPTURecord(1010.3, 20.8, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		records.get(0).setTime(10.0);
		records.get(2).setTime(0.0);
		
		records.get(0).setAltitude(1000.0, METERS);
		records.get(2).setAltitude(1029.9, METERS);
		
		double returnedValue = 
			autoQC.differentialAscensionRate(records, 0.0, false, log);
		
		// Test the output
		assertFalse("Ascent Rate - Neg Quest Bound: log ready",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Ascent Rate - Neg Quest Bound: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Ascent Rate - Neg Quest Bound: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Ascent Rate - Neg Quest Bound: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Ascent Rate - Neg Quest Bound: ascent rate", -2.99,
				returnedValue, THRESHOLD);
    }
    
    @Test public void ascentRateNegQuestBoundViolation() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
		records.add(buildPTURecord(1013.0, 21.4, 50.0));
		records.add(buildPTURecord(1010.3, 20.8, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		records.get(0).setTime(10.0);
		records.get(2).setTime(0.0);
		
		records.get(0).setAltitude(1000.0, METERS);
		records.get(2).setAltitude(1030.0, METERS);
		
		double returnedValue = 
			autoQC.differentialAscensionRate(records, 0.0, false, log);
		
		// Test the output
		assertTrue("Ascent Rate - Neg Quest Bound Violation: log ready- pre",
				reader.ready());
		assertFalse("Ascent Rate - Neg Quest Bound Violation: log output",
				"".equals(reader.readLine()));		
		assertFalse("Ascent Rate - Neg Quest Bound Violation: log ready - post",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Ascent Rate - Neg Quest Bound Violation: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Ascent Rate - Neg Quest Bound Violation: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Ascent Rate - Neg Quest Bound Violation: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Ascent Rate - Neg Quest Bound Violation: ascent rate", -3.0,
				returnedValue, THRESHOLD);
    }
    
    @Test public void ascentRateNegBadBound() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
		records.add(buildPTURecord(1013.0, 21.4, 50.0));
		records.add(buildPTURecord(1010.3, 20.8, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		records.get(0).setTime(10.0);
		records.get(2).setTime(0.0);
		
		records.get(0).setAltitude(1000.0, METERS);
		records.get(2).setAltitude(1049.9, METERS);
		
		double returnedValue = 
			autoQC.differentialAscensionRate(records, 0.0, false, log);
		
		// Test the output
		assertTrue("Ascent Rate - Neg Bad Bound: log ready- pre",
				reader.ready());
		assertFalse("Ascent Rate - Neg Bad Bound: log output",
				"".equals(reader.readLine()));		
		assertFalse("Ascent Rate - Neg Bad Bound: log ready - post",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Ascent Rate - Neg Bad Bound: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Ascent Rate - Neg Bad Bound: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Ascent Rate - Neg Bad Bound: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Ascent Rate - Neg Bad Bound: ascent rate", -4.99,
				returnedValue, THRESHOLD);
    }
    
    @Test public void ascentRateNegBadBoundViolation() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
		records.add(buildPTURecord(1013.0, 21.4, 50.0));
		records.add(buildPTURecord(1010.3, 20.8, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		records.get(0).setTime(10.0);
		records.get(2).setTime(0.0);
		
		records.get(0).setAltitude(1000.0, METERS);
		records.get(2).setAltitude(1050.0, METERS);
		
		double returnedValue = 
			autoQC.differentialAscensionRate(records, 0.0, false, log);
		
		// Test the output
		assertTrue("Ascent Rate - Neg Bad Bound Violation: log ready- pre",
				reader.ready());
		assertFalse("Ascent Rate - Neg Bad Bound Violation: log output",
				"".equals(reader.readLine()));		
		assertFalse("Ascent Rate - Neg Bad Bound Violation: log ready - post",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Ascent Rate - Neg Bad Bound Violation: Press Flag "+i, 
					BAD_FLAG, records.get(i).getPressureFlag());
			assertEquals("Ascent Rate - Neg Bad Bound Violation: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Ascent Rate - Neg Bad Bound Violation: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Ascent Rate - Neg Bad Bound Violation: ascent rate", -5.0,
				returnedValue, THRESHOLD);
    }

	@Override
	public void canCheckPressureRates() throws ConversionException, InvalidValueWarning, IOException {
		ESCSoundingRecord record = buildPTURecord(100.1, null, null);		
		assertTrue("NWS Press Rate Check: Over Threshold",
				autoQC.canCheckPressureRates(record));

		record = buildPTURecord(100.0, null, null);
		assertFalse("NWS Press Rate Check: Equal Threshold",
				autoQC.canCheckPressureRates(record));
		
		record = buildPTURecord(99.9, null, null);
		assertFalse("NWS Press Rate Check: Under Threshold",
				autoQC.canCheckPressureRates(record));
	}
}
