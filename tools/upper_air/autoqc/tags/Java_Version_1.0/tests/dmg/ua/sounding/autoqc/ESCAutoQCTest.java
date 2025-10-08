package dmg.ua.sounding.autoqc;

import static org.junit.Assert.*;
import static dmg.ua.sounding.esc.ESCSoundingRecord.*;
import static dmg.util.LengthUtils.*;
import static dmg.util.PressureUtils.*;
import static dmg.util.TemperatureUtils.*;
import static dmg.util.VelocityUtils.*;

import dmg.ua.sounding.esc.*;
import dmg.util.*;

import java.io.*;
import java.util.*;
import org.junit.*;

public abstract class ESCAutoQCTest {

	protected static final Double THRESHOLD = Math.pow(1, -10);
	protected static ESCAutoQC autoQC, coldQC;
	
	protected List<ESCSoundingRecord> records;
	
	protected BufferedReader reader;
	protected PrintWriter log;
	
    @Before public void setUp() throws IOException {
    	records = new ArrayList<ESCSoundingRecord>();
    	
    	PipedInputStream in = new PipedInputStream();
    	PipedOutputStream out = new PipedOutputStream(in);
    	
    	reader = new BufferedReader(new InputStreamReader(in));
    	log = new PrintWriter(out);
    }
    
    @After public void tearDown() throws IOException {
    	reader.close();
    	log.close();
    }
	
    protected ESCSoundingRecord buildPTURecord(Double press, Double temp,
    		Double rh) throws ConversionException, InvalidValueWarning {
    	ESCSoundingRecord record = new ESCSoundingRecord();
    	
    	try {
	    	record.setPressure(press, MILLIBARS);
	    	record.setTemperature(temp, CELCIUS);
	    	record.setRelativeHumidity(rh);
    	} catch (CalculationWarning e) {}
    	
    	return record;
    }
    
    protected ESCSoundingRecord buildWindRecord(Double uComp, Double vComp,
    		Double spd, Double dir) throws
    ConversionException, InvalidValueWarning {
    	ESCSoundingRecord record = new ESCSoundingRecord();

    	try {
	    	record.setWindDirection(dir);
	    	record.setWindSpeed(spd, METERS_PER_SECOND);
	    	record.setUComponent(uComp, METERS_PER_SECOND);
	    	record.setVComponent(vComp, METERS_PER_SECOND);
    	} catch (CalculationWarning e) {}
    	
    	return record;
    }

    @Test public abstract void canCheckPressureRates() throws
    	ConversionException, InvalidValueWarning, IOException;
    
    @Test public abstract void decreasingPressureDecrease() throws
    	ConversionException, InvalidValueWarning, IOException;
    
    @Test public abstract void decreasingPressureEqual()  throws
		ConversionException, InvalidValueWarning, IOException;
    
    @Test public abstract void decreasingPressureIncrease()  throws
    	ConversionException, InvalidValueWarning, IOException;
    
    @Test public abstract void decreasePressureDecreaseOverThreshold() throws
    	ConversionException, InvalidValueWarning, IOException;
    
    @Test public abstract void decreasePressureEqualOverThreshold() throws
    	ConversionException, InvalidValueWarning, IOException;
    
    @Test public abstract void decreasePressureIncreaseOverThreshold() throws
    	ConversionException, InvalidValueWarning, IOException;
    
    @Test public abstract void checkTimeNoCurrentNoNextNoPrevTime() throws
	ConversionException, InvalidValueWarning, IOException;
    
    @Test public abstract void checkTimeNoCurrentNoNextValidPrevTime() throws
	ConversionException, InvalidValueWarning, IOException;
    
    @Test public abstract void checkTimeNoCurrentValidNextNoPrevTime() throws
	ConversionException, InvalidValueWarning, IOException, CalculationWarning;
    
    @Test public abstract void checkTimeNoCurrentValidNextValidPrevTimeGreat()
    throws ConversionException, InvalidValueWarning, IOException, CalculationWarning;

    @Test public abstract void checkTimeNoCurrentValidNextValidPrevTimeEqual()
    throws ConversionException, InvalidValueWarning, IOException, CalculationWarning;

    @Test public abstract void checkTimeNoCurrentValidNextValidPrevTimeLess()
    throws ConversionException, InvalidValueWarning, IOException, CalculationWarning;

    @Test public abstract void checkTimeValidCurrentNoNextNoPrevTime() throws
	ConversionException, InvalidValueWarning, IOException, CalculationWarning;
    
    @Test public abstract void checkTimeValidCurrentNoNextValidPrevTime() throws
	ConversionException, InvalidValueWarning, IOException, CalculationWarning;
    
    @Test public abstract void checkTimeValidCurrentValidNextNoPrevTimeGreat() throws
	ConversionException, InvalidValueWarning, IOException, CalculationWarning;
    
    @Test public abstract void checkTimeValidCurrentValidNextValidPrevTimeGreat()
    throws ConversionException, InvalidValueWarning, IOException, CalculationWarning;

    @Test public abstract void checkTimeValidCurrentValidNextNoPrevTimeEqual() throws
	ConversionException, InvalidValueWarning, IOException, CalculationWarning;
    
    @Test public abstract void checkTimeValidCurrentValidNextValidPrevTimeEqual()
    throws ConversionException, InvalidValueWarning, IOException, CalculationWarning;

    @Test public abstract void checkTimeValidCurrentValidNextNoPrevTimeLess() throws
	ConversionException, InvalidValueWarning, IOException, CalculationWarning;
    
    @Test public abstract void checkTimeValidCurrentValidNextValidPrevTimeLess()
    throws ConversionException, InvalidValueWarning, IOException, CalculationWarning;

    
    
    @Test public void decreasePressureDecreaseEqualThreshold() throws
    ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
    	ESCSoundingRecord start = buildPTURecord(100.0, 20.0, 15.0);
		ESCSoundingRecord end = buildPTURecord(99.9, 10.0, 5.0);
		
		boolean pressErr = autoQC.decreasingPressureCheck(start, end, log);
		
		assertFalse("Dec Press - Dec Equal Thresh: log ready", 
				reader.ready());
		
		assertEquals("Dec Press - Dec Equal Thresh: start press", 100.0,  
				start.getPressure());
		assertEquals("Dec Press - Dec Equal Thresh: start temp", 20.0, 
				start.getTemperature());
		assertEquals("Dec Press - Dec Equal Thresh: start rh", 15.0, 
				start.getRelativeHumidity());
		assertEquals("Dec Press - Dec Equal Thresh: start Press Flag ", 
				UNCHECKED_FLAG, start.getPressureFlag());
		assertEquals("Dec Press - Dec Equal Thresh: start Temp Flag ", 
				UNCHECKED_FLAG, start.getTemperatureFlag());
		assertEquals("Dec Press - Dec Equal Thresh: start RH Flag ", 
				UNCHECKED_FLAG, start.getRelativeHumidityFlag());

		assertEquals("Dec Press - Dec Equal Thresh: end press", 99.9,
				end.getPressure());
		assertEquals("Dec Press - Dec Equal Thresh: end temp", 10.0, 
				end.getTemperature());
		assertEquals("Dec Press - Dec Equal Thresh: end rh", 5.0, 
				end.getRelativeHumidity());
		assertEquals("Dec Press - Dec Equal Thresh: end Press Flag ", 
				UNCHECKED_FLAG, end.getPressureFlag());
		assertEquals("Dec Press - Dec Equal Thresh: end Temp Flag ", 
				UNCHECKED_FLAG, end.getTemperatureFlag());
		assertEquals("Dec Press - Dec Equal Thresh: end RH Flag ", 
				UNCHECKED_FLAG, end.getRelativeHumidityFlag());    
	
		assertFalse("Dec Press - Dec Equal Thresh: press err", pressErr);
    }
    
    @Test public void decreasePressureEqualEqualThreshold() throws
    ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
    	ESCSoundingRecord start = buildPTURecord(100.0, 20.0, 15.0);
		ESCSoundingRecord end = buildPTURecord(100.0, 10.0, 5.0);
		
		boolean pressErr = autoQC.decreasingPressureCheck(start, end, log);
		
		assertFalse("Dec Press - Equal Equal Thresh: log ready", 
				reader.ready());
		
		assertEquals("Dec Press - Equal Equal Thresh: start press", 100.0,  
				start.getPressure());
		assertEquals("Dec Press - Equal Equal Thresh: start temp", 20.0, 
				start.getTemperature());
		assertEquals("Dec Press - Equal Equal Thresh: start rh", 15.0, 
				start.getRelativeHumidity());
		assertEquals("Dec Press - Equal Equal Thresh: start Press Flag ", 
				UNCHECKED_FLAG, start.getPressureFlag());
		assertEquals("Dec Press - Equal Equal Thresh: start Temp Flag ", 
				UNCHECKED_FLAG, start.getTemperatureFlag());
		assertEquals("Dec Press - Equal Equal Thresh: start RH Flag ", 
				UNCHECKED_FLAG, start.getRelativeHumidityFlag());

		assertEquals("Dec Press - Equal Equal Thresh: end press", 100.0,
				end.getPressure());
		assertEquals("Dec Press - Equal Equal Thresh: end temp", 10.0, 
				end.getTemperature());
		assertEquals("Dec Press - Equal Equal Thresh: end rh", 5.0, 
				end.getRelativeHumidity());
		assertEquals("Dec Press - Equal Equal Thresh: end Press Flag ", 
				UNCHECKED_FLAG, end.getPressureFlag());
		assertEquals("Dec Press - Equal Equal Thresh: end Temp Flag ", 
				UNCHECKED_FLAG, end.getTemperatureFlag());
		assertEquals("Dec Press - Equal Equal Thresh: end RH Flag ", 
				UNCHECKED_FLAG, end.getRelativeHumidityFlag());    
	
		assertFalse("Dec Press - Equal Equal Thresh: press err", pressErr);
    }
    
    @Test public void decreasePressureIncreaseEqualThreshold() throws
    ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
    	ESCSoundingRecord start = buildPTURecord(100.0, 20.0, 15.0);
		ESCSoundingRecord end = buildPTURecord(100.1, 10.0, 5.0);
		
		boolean pressErr = autoQC.decreasingPressureCheck(start, end, log);
		
		assertTrue("Dec Press - Inc Equal Thresh: log ready - pre", 
				reader.ready());
		assertFalse("Dec Press - Inc Equal Thresh: log output", 
				"".equals(reader.readLine()));
		assertFalse("Dec Press - Inc Equal Thresh: log ready - post", 
				reader.ready());
		
		assertEquals("Dec Press - Inc Equal Thresh: start press", 100.0,  
				start.getPressure());
		assertEquals("Dec Press - Inc Equal Thresh: start temp", 20.0, 
				start.getTemperature());
		assertEquals("Dec Press - Inc Equal Thresh: start rh", 15.0, 
				start.getRelativeHumidity());
		assertEquals("Dec Press - Inc Equal Thresh: start Press Flag ", 
				UNCHECKED_FLAG, start.getPressureFlag());
		assertEquals("Dec Press - Inc Equal Thresh: start Temp Flag ", 
				UNCHECKED_FLAG, start.getTemperatureFlag());
		assertEquals("Dec Press - Inc Equal Thresh: start RH Flag ", 
				UNCHECKED_FLAG, start.getRelativeHumidityFlag());

		assertEquals("Dec Press - Inc Equal Thresh: end press", 100.1,
				end.getPressure());
		assertEquals("Dec Press - Inc Equal Thresh: end temp", 10.0, 
				end.getTemperature());
		assertEquals("Dec Press - Inc Equal Thresh: end rh", 5.0, 
				end.getRelativeHumidity());
		assertEquals("Dec Press - Inc Equal Thresh: end Press Flag ", 
				QUESTIONABLE_FLAG, end.getPressureFlag());
		assertEquals("Dec Press - Inc Equal Thresh: end Temp Flag ", 
				QUESTIONABLE_FLAG, end.getTemperatureFlag());
		assertEquals("Dec Press - Inc Equal Thresh: end RH Flag ", 
				QUESTIONABLE_FLAG, end.getRelativeHumidityFlag());    
	
		assertTrue("Dec Press - Inc Equal Thresh: press err", pressErr);
    }
    
    @Test public void decreasePressureDecreaseUnderThreshold() throws
    ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
    	ESCSoundingRecord start = buildPTURecord(99.9, 20.0, 15.0);
		ESCSoundingRecord end = buildPTURecord(99.8, 10.0, 5.0);
		
		boolean pressErr = autoQC.decreasingPressureCheck(start, end, log);
		
		assertFalse("Dec Press - Dec Under Thresh: log ready", 
				reader.ready());
		
		assertEquals("Dec Press - Dec Under Thresh: start press", 99.9,  
				start.getPressure());
		assertEquals("Dec Press - Dec Under Thresh: start temp", 20.0, 
				start.getTemperature());
		assertEquals("Dec Press - Dec Under Thresh: start rh", 15.0, 
				start.getRelativeHumidity());
		assertEquals("Dec Press - Dec Under Thresh: start Press Flag ", 
				UNCHECKED_FLAG, start.getPressureFlag());
		assertEquals("Dec Press - Dec Under Thresh: start Temp Flag ", 
				UNCHECKED_FLAG, start.getTemperatureFlag());
		assertEquals("Dec Press - Dec Under Thresh: start RH Flag ", 
				UNCHECKED_FLAG, start.getRelativeHumidityFlag());

		assertEquals("Dec Press - Dec Under Thresh: end press", 99.8,
				end.getPressure());
		assertEquals("Dec Press - Dec Under Thresh: end temp", 10.0, 
				end.getTemperature());
		assertEquals("Dec Press - Dec Under Thresh: end rh", 5.0, 
				end.getRelativeHumidity());
		assertEquals("Dec Press - Dec Under Thresh: end Press Flag ", 
				UNCHECKED_FLAG, end.getPressureFlag());
		assertEquals("Dec Press - Dec Under Thresh: end Temp Flag ", 
				UNCHECKED_FLAG, end.getTemperatureFlag());
		assertEquals("Dec Press - Dec Under Thresh: end RH Flag ", 
				UNCHECKED_FLAG, end.getRelativeHumidityFlag());    
	
		assertFalse("Dec Press - Dec Under Thresh: press err", pressErr);
    }
    
    @Test public void decreasePressureEqualUnderThreshold() throws
    ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
    	ESCSoundingRecord start = buildPTURecord(99.9, 20.0, 15.0);
		ESCSoundingRecord end = buildPTURecord(99.9, 10.0, 5.0);
		
		boolean pressErr = autoQC.decreasingPressureCheck(start, end, log);
		
		assertFalse("Dec Press - Equal Under Thresh: log ready", 
				reader.ready());
		
		assertEquals("Dec Press - Equal Under Thresh: start press", 99.9,  
				start.getPressure());
		assertEquals("Dec Press - Equal Under Thresh: start temp", 20.0, 
				start.getTemperature());
		assertEquals("Dec Press - Equal Under Thresh: start rh", 15.0, 
				start.getRelativeHumidity());
		assertEquals("Dec Press - Equal Under Thresh: start Press Flag ", 
				UNCHECKED_FLAG, start.getPressureFlag());
		assertEquals("Dec Press - Equal Under Thresh: start Temp Flag ", 
				UNCHECKED_FLAG, start.getTemperatureFlag());
		assertEquals("Dec Press - Equal Under Thresh: start RH Flag ", 
				UNCHECKED_FLAG, start.getRelativeHumidityFlag());

		assertEquals("Dec Press - Equal Under Thresh: end press", 99.9,
				end.getPressure());
		assertEquals("Dec Press - Equal Under Thresh: end temp", 10.0, 
				end.getTemperature());
		assertEquals("Dec Press - Equal Under Thresh: end rh", 5.0, 
				end.getRelativeHumidity());
		assertEquals("Dec Press - Equal Under Thresh: end Press Flag ", 
				UNCHECKED_FLAG, end.getPressureFlag());
		assertEquals("Dec Press - Equal Under Thresh: end Temp Flag ", 
				UNCHECKED_FLAG, end.getTemperatureFlag());
		assertEquals("Dec Press - Equal Under Thresh: end RH Flag ", 
				UNCHECKED_FLAG, end.getRelativeHumidityFlag());    
	
		assertFalse("Dec Press - Equal Under Thresh: press err", pressErr);
    }
    
    @Test public void decreasePressureIncreaseUnderThreshold() throws
    ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
    	ESCSoundingRecord start = buildPTURecord(99.9, 20.0, 15.0);
		ESCSoundingRecord end = buildPTURecord(100.0, 10.0, 5.0);
		
		boolean pressErr = autoQC.decreasingPressureCheck(start, end, log);
		
		assertTrue("Dec Press - Inc Under Thresh: log ready - pre", 
				reader.ready());
		assertFalse("Dec Press - Inc Under Thresh: log output", 
				"".equals(reader.readLine()));
		assertFalse("Dec Press - Inc Under Thresh: log ready - post", 
				reader.ready());
		
		assertEquals("Dec Press - Inc Under Thresh: start press", 99.9,  
				start.getPressure());
		assertEquals("Dec Press - Inc Under Thresh: start temp", 20.0, 
				start.getTemperature());
		assertEquals("Dec Press - Inc Under Thresh: start rh", 15.0, 
				start.getRelativeHumidity());
		assertEquals("Dec Press - Inc Under Thresh: start Press Flag ", 
				UNCHECKED_FLAG, start.getPressureFlag());
		assertEquals("Dec Press - Inc Under Thresh: start Temp Flag ", 
				UNCHECKED_FLAG, start.getTemperatureFlag());
		assertEquals("Dec Press - Inc Under Thresh: start RH Flag ", 
				UNCHECKED_FLAG, start.getRelativeHumidityFlag());

		assertEquals("Dec Press - Inc Under Thresh: end press", 100.0,
				end.getPressure());
		assertEquals("Dec Press - Inc Under Thresh: end temp", 10.0, 
				end.getTemperature());
		assertEquals("Dec Press - Inc Under Thresh: end rh", 5.0, 
				end.getRelativeHumidity());
		assertEquals("Dec Press - Inc Under Thresh: end Press Flag ", 
				QUESTIONABLE_FLAG, end.getPressureFlag());
		assertEquals("Dec Press - Inc Under Thresh: end Temp Flag ", 
				QUESTIONABLE_FLAG, end.getTemperatureFlag());
		assertEquals("Dec Press - Inc Under Thresh: end RH Flag ", 
				QUESTIONABLE_FLAG, end.getRelativeHumidityFlag());    
	
		assertTrue("Dec Press - Inc Under Thresh: press err", pressErr);
    }
    
    @Test public void decreasePressureNoStartPressure() throws
    ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
    	ESCSoundingRecord start = buildPTURecord(null, 20.0, 15.0);
		ESCSoundingRecord end = buildPTURecord(900.0, 10.0, 5.0);
		
		boolean pressErr = autoQC.decreasingPressureCheck(start, end, log);
		
		assertFalse("Dec Press - No Start Press: log ready", 
				reader.ready());
		
		assertNull("Dec Press - No Start Press: start press",  
				start.getPressure());
		assertEquals("Dec Press - No Start Press: start temp", 20.0, 
				start.getTemperature());
		assertEquals("Dec Press - No Start Press: start rh", 15.0, 
				start.getRelativeHumidity());
		assertEquals("Dec Press - No Start Press: start Press Flag ", 
				MISSING_FLAG, start.getPressureFlag());
		assertEquals("Dec Press - No Start Press: start Temp Flag ", 
				UNCHECKED_FLAG, start.getTemperatureFlag());
		assertEquals("Dec Press - No Start Press: start RH Flag ", 
				UNCHECKED_FLAG, start.getRelativeHumidityFlag());

		assertEquals("Dec Press - No Start Press: end press", 900.0,
				end.getPressure());
		assertEquals("Dec Press - No Start Press: end temp", 10.0, 
				end.getTemperature());
		assertEquals("Dec Press - No Start Press: end rh", 5.0, 
				end.getRelativeHumidity());
		assertEquals("Dec Press - No Start Press: end Press Flag ", 
				UNCHECKED_FLAG, end.getPressureFlag());
		assertEquals("Dec Press - No Start Press: end Temp Flag ", 
				UNCHECKED_FLAG, end.getTemperatureFlag());
		assertEquals("Dec Press - No Start Press: end RH Flag ", 
				UNCHECKED_FLAG, end.getRelativeHumidityFlag());    
	
		assertFalse("Dec Press - No Start Press: press err", pressErr);
    }
    
    @Test public void decreasePressureNoEndPressure() throws
    ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
    	ESCSoundingRecord start = buildPTURecord(1000.0, 20.0, 15.0);
		ESCSoundingRecord end = buildPTURecord(null, 10.0, 5.0);
		
		boolean pressErr = autoQC.decreasingPressureCheck(start, end, log);
		
		assertFalse("Dec Press - No End Press: log ready", 
				reader.ready());
		
		assertEquals("Dec Press - No End Press: start press", 1000.0, 
				start.getPressure());
		assertEquals("Dec Press - No End Press: start temp", 20.0, 
				start.getTemperature());
		assertEquals("Dec Press - No End Press: start rh", 15.0, 
				start.getRelativeHumidity());
		assertEquals("Dec Press - No End Press: start Press Flag ", 
				UNCHECKED_FLAG, start.getPressureFlag());
		assertEquals("Dec Press - No End Press: start Temp Flag ", 
				UNCHECKED_FLAG, start.getTemperatureFlag());
		assertEquals("Dec Press - No End Press: start RH Flag ", 
				UNCHECKED_FLAG, start.getRelativeHumidityFlag());

		assertNull("Dec Press - No End Press: end press",  
				end.getPressure());
		assertEquals("Dec Press - No End Press: end temp", 10.0, 
				end.getTemperature());
		assertEquals("Dec Press - No End Press: end rh", 5.0, 
				end.getRelativeHumidity());
		assertEquals("Dec Press - No End Press: end Press Flag ", 
				MISSING_FLAG, end.getPressureFlag());
		assertEquals("Dec Press - No End Press: end Temp Flag ", 
				UNCHECKED_FLAG, end.getTemperatureFlag());
		assertEquals("Dec Press - No End Press: end RH Flag ", 
				UNCHECKED_FLAG, end.getRelativeHumidityFlag());    
	
		assertFalse("Dec Press - No End Press: press err", pressErr);
    }
    
    @Test public void diffAscentRateNullStartTime() throws CalculationWarning,
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
		assertFalse("Diff Ascent Rate - No Start Time: log ready",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Diff Ascent Rate - No Start Time: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Diff Ascent Rate - No Start Time: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Diff Ascent Rate - No Start Time: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Diff Ascent Rate - No Start Time: ascent rate", 0.0,
				returnedValue);
    }
    
    @Test public void diffAscentRateNullEndTime() throws CalculationWarning,
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
		assertFalse("Diff Ascent Rate - No End Time: log ready",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Diff Ascent Rate - No End Time: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Diff Ascent Rate - No End Time: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Diff Ascent Rate - No End Time: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Diff Ascent Rate - No End Time: ascent rate", 0.0,
				returnedValue);
    }
    
    @Test public void diffAscentRateNullStartAlt() throws CalculationWarning,
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
		assertFalse("Diff Ascent Rate - No Start Alt: log ready",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Diff Ascent Rate - No Start Alt: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Diff Ascent Rate - No Start Alt: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Diff Ascent Rate - No Start Alt: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Diff Ascent Rate - No Start Alt: ascent rate", 0.0,
				returnedValue);
    }
    
    @Test public void diffAscentRateNullEndAlt() throws CalculationWarning,
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
		assertFalse("Diff Ascent Rate - No End Alt: log ready",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Diff Ascent Rate - No End Alt: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Diff Ascent Rate - No End Alt: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Diff Ascent Rate - No End Alt: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Diff Ascent Rate - No End Alt: ascent rate", 0.0,
				returnedValue);
    }
    
    @Test public void diffAscentRateEqualTimes() throws CalculationWarning,
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
		assertTrue("Diff Ascent Rate - Equal Times: log ready - pre",
				reader.ready());
		assertFalse("Diff Ascent Rate - Equal Times: log output",
				"".equals(reader.readLine()));
		assertFalse("Diff Ascent Rate - Equal Times: log ready - post",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Diff Ascent Rate - Equal Times: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Diff Ascent Rate - Equal Times: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Diff Ascent Rate - Equal Times: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Diff Ascent Rate - Equal Times: ascent rate", 0.0,
				returnedValue);
    }
    
    @Test public void diffAscentRateEqualAltitudes() throws CalculationWarning,
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
		assertTrue("Diff Ascent Rate - Equal Alts: log ready - pre",
				reader.ready());
		assertFalse("Diff Ascent Rate - Equal Alts: log output",
				"".equals(reader.readLine()));
		assertFalse("Diff Ascent Rate - Equal Alts: log ready - post",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Diff Ascent Rate - Equal Alts: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Diff Ascent Rate - Equal Alts: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Diff Ascent Rate - Equal Alts: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Diff Ascent Rate - Equal Alts: ascent rate", 0.0,
				returnedValue);
    }
    
    @Test public void diffAscentRateNullPreviousRate() throws
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
		assertFalse("Diff Ascent Rate - Null Prev Asc Rate: log ready",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Diff Ascent Rate - Null Prev Asc Rate: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Diff Ascent Rate - Null Prev Asc Rate: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Diff Ascent Rate - Null Prev Asc Rate: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Diff Ascent Rate - Null Prev Asc Rate: ascent rate", 2.9,
				returnedValue);
    }
    
    @Test public void diffAscentRateValidPreviousRate() throws
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
		assertFalse("Diff Ascent Rate - Prev Asc Rate: log ready",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Diff Ascent Rate - Prev Asc Rate: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Diff Ascent Rate - Prev Asc Rate: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Diff Ascent Rate - Prev Asc Rate: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Diff Ascent Rate - Prev Asc Rate: ascent rate", 2.9,
				returnedValue);
    }
    
    @Test public void diffAscentRateBegin() throws
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
		assertFalse("Diff Ascent Rate - Begin == true: log ready",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Diff Ascent Rate - Begin == true: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Diff Ascent Rate - Begin == true: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Diff Ascent Rate - Begin == true: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Diff Ascent Rate - Begin == true: ascent rate", 3.9,
				returnedValue);
    }
    
    @Test public void diffAscentRateNotBegin() throws
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
		assertTrue("Diff Ascent Rate - Begin == false: log ready - pre",
				reader.ready());
		assertFalse("Diff Ascent Rate - Begin == false: log output",
				"".equals(reader.readLine()));
		assertFalse("Diff Ascent Rate - Begin == false: log ready - post",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Diff Ascent Rate - Begin == false: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Diff Ascent Rate - Begin == false: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Diff Ascent Rate - Begin == false: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Diff Ascent Rate - Begin == false: ascent rate", 3.9,
				returnedValue);
    }
    
    @Test public void diffAscentRatePosQuestBound() throws
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
		assertFalse("Diff Ascent Rate - Pos Quest Bound: log ready",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Diff Ascent Rate - Pos Quest Bound: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Diff Ascent Rate - Pos Quest Bound: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Diff Ascent Rate - Pos Quest Bound: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Diff Ascent Rate - Pos Quest Bound: ascent rate", 2.99,
				returnedValue, THRESHOLD);
    }
    
    @Test public void diffAscentRatePosQuestBoundViolation() throws
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
		assertTrue("Diff Ascent Rate - Pos Quest Bound Violation: log ready- pre",
				reader.ready());
		assertFalse("Diff Ascent Rate - Pos Quest Bound Violation: log output",
				"".equals(reader.readLine()));		
		assertFalse("Diff Ascent Rate - Pos Quest Bound Violation: log ready - post",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Diff Ascent Rate - Pos Quest Bound Violation: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Diff Ascent Rate - Pos Quest Bound Violation: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Diff Ascent Rate - Pos Quest Bound Violation: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Diff Ascent Rate - Pos Quest Bound Violation: ascent rate", 3.0,
				returnedValue, THRESHOLD);
    }
    
    @Test public void diffAscentRatePosBadBound() throws
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
		assertTrue("Diff Ascent Rate - Pos Bad Bound: log ready- pre",
				reader.ready());
		assertFalse("Diff Ascent Rate - Pos Bad Bound: log output",
				"".equals(reader.readLine()));		
		assertFalse("Diff Ascent Rate - Pos Bad Bound: log ready - post",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Diff Ascent Rate - Pos Bad Bound: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Diff Ascent Rate - Pos Bad Bound: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Diff Ascent Rate - Pos Bad Bound: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Diff Ascent Rate - Pos Bad Bound: ascent rate", 4.99,
				returnedValue, THRESHOLD);
    }
    
    @Test public void diffAscentRatePosBadBoundViolation() throws
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
		assertTrue("Diff Ascent Rate - Pos Bad Bound Violation: log ready- pre",
				reader.ready());
		assertFalse("Diff Ascent Rate - Pos Bad Bound Violation: log output",
				"".equals(reader.readLine()));		
		assertFalse("Diff Ascent Rate - Pos Bad Bound Violation: log ready - post",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Diff Ascent Rate - Pos Bad Bound Violation: Press Flag "+i, 
					BAD_FLAG, records.get(i).getPressureFlag());
			assertEquals("Diff Ascent Rate - Pos Bad Bound Violation: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Diff Ascent Rate - Pos Bad Bound Violation: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Diff Ascent Rate - Pos Bad Bound Violation: ascent rate", 5.0,
				returnedValue, THRESHOLD);
    }

    @Test public void diffAscentRateNegQuestBound() throws
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
		assertFalse("Diff Ascent Rate - Neg Quest Bound: log ready",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Diff Ascent Rate - Neg Quest Bound: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Diff Ascent Rate - Neg Quest Bound: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Diff Ascent Rate - Neg Quest Bound: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Diff Ascent Rate - Neg Quest Bound: ascent rate", -2.99,
				returnedValue, THRESHOLD);
    }
    
    @Test public void diffAscentRateNegQuestBoundViolation() throws
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
		assertTrue("Diff Ascent Rate - Neg Quest Bound Violation: log ready- pre",
				reader.ready());
		assertFalse("Diff Ascent Rate - Neg Quest Bound Violation: log output",
				"".equals(reader.readLine()));		
		assertFalse("Diff Ascent Rate - Neg Quest Bound Violation: log ready - post",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Diff Ascent Rate - Neg Quest Bound Violation: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Diff Ascent Rate - Neg Quest Bound Violation: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Diff Ascent Rate - Neg Quest Bound Violation: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Diff Ascent Rate - Neg Quest Bound Violation: ascent rate", -3.0,
				returnedValue, THRESHOLD);
    }
    
    @Test public void diffAscentRateNegBadBound() throws
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
		assertTrue("Diff Ascent Rate - Neg Bad Bound: log ready- pre",
				reader.ready());
		assertFalse("Diff Ascent Rate - Neg Bad Bound: log output",
				"".equals(reader.readLine()));		
		assertFalse("Diff Ascent Rate - Neg Bad Bound: log ready - post",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Diff Ascent Rate - Neg Bad Bound: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Diff Ascent Rate - Neg Bad Bound: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Diff Ascent Rate - Neg Bad Bound: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Diff Ascent Rate - Neg Bad Bound: ascent rate", -4.99,
				returnedValue, THRESHOLD);
    }
    
    @Test public void diffAscentRateNegBadBoundViolation() throws
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
		assertTrue("Diff Ascent Rate - Neg Bad Bound Violation: log ready- pre",
				reader.ready());
		assertFalse("Diff Ascent Rate - Neg Bad Bound Violation: log output",
				"".equals(reader.readLine()));		
		assertFalse("Diff Ascent Rate - Neg Bad Bound Violation: log ready - post",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Diff Ascent Rate - Neg Bad Bound Violation: Press Flag "+i, 
					BAD_FLAG, records.get(i).getPressureFlag());
			assertEquals("Diff Ascent Rate - Neg Bad Bound Violation: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Diff Ascent Rate - Neg Bad Bound Violation: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		assertEquals("Diff Ascent Rate - Neg Bad Bound Violation: ascent rate", -5.0,
				returnedValue, THRESHOLD);
    }

    
    @Test public void increaseAltitudeNoStartAlt() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord start = buildPTURecord(1000.0, 20.0, 15.0);
    	ESCSoundingRecord end = buildPTURecord(900.0, 10.0, 5.0);
    	end.setAltitude(5000.0, METERS);
    	
    	autoQC.increasingAltitudeCheck(start, end, false, log);
    	
		assertFalse("Inc Alt - No Start Alt: log ready", reader.ready());
		
		assertNull("Inc Alt - No Start Alt: start alt", start.getAltitude()); 
		assertEquals("Inc Alt - No Start Alt: start press", 1000.0, 
				start.getPressure());
		assertEquals("Inc Alt - No Start Alt: start temp", 20.0, 
				start.getTemperature());
		assertEquals("Inc Alt - No Start Alt: start rh", 15.0, 
				start.getRelativeHumidity());
		assertEquals("Inc Alt - No Start Alt: start Press Flag ", 
				UNCHECKED_FLAG, start.getPressureFlag());
		assertEquals("Inc Alt - No Start Alt: start Temp Flag ", 
				UNCHECKED_FLAG, start.getTemperatureFlag());
		assertEquals("Inc Alt - No Start Alt: start RH Flag ", 
				UNCHECKED_FLAG, start.getRelativeHumidityFlag());

		assertEquals("Inc Alt - No Start Alt: end alt", 5000.0, 
				end.getAltitude()); 
		assertEquals("Inc Alt - No Start Alt: end press", 900.0, 
				end.getPressure());
		assertEquals("Inc Alt - No Start Alt: end temp", 10.0, 
				end.getTemperature());
		assertEquals("Inc Alt - No Start Alt: end rh", 5.0, 
				end.getRelativeHumidity());
		assertEquals("Inc Alt - No Start Alt: end Press Flag ", 
				UNCHECKED_FLAG, end.getPressureFlag());
		assertEquals("Inc Alt - No Start Alt: end Temp Flag ", 
				UNCHECKED_FLAG, end.getTemperatureFlag());
		assertEquals("Inc Alt - No Start Alt: end RH Flag ", 
				UNCHECKED_FLAG, end.getRelativeHumidityFlag());
    }
    
    @Test public void increaseAltitudeNoEndAlt() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord start = buildPTURecord(1000.0, 20.0, 15.0);
    	ESCSoundingRecord end = buildPTURecord(900.0, 10.0, 5.0);
    	start.setAltitude(5000.0, METERS);
    	
    	autoQC.increasingAltitudeCheck(start, end, false, log);
    	
		assertFalse("Inc Alt - No End Alt: log ready", reader.ready());
		
		assertEquals("Inc Alt - No End Alt: start alt", 5000.0, 
				start.getAltitude()); 
		assertEquals("Inc Alt - No End Alt: start press", 1000.0, 
				start.getPressure());
		assertEquals("Inc Alt - No End Alt: start temp", 20.0, 
				start.getTemperature());
		assertEquals("Inc Alt - No End Alt: start rh", 15.0, 
				start.getRelativeHumidity());
		assertEquals("Inc Alt - No End Alt: start Press Flag ", 
				UNCHECKED_FLAG, start.getPressureFlag());
		assertEquals("Inc Alt - No End Alt: start Temp Flag ", 
				UNCHECKED_FLAG, start.getTemperatureFlag());
		assertEquals("Inc Alt - No End Alt: start RH Flag ", 
				UNCHECKED_FLAG, start.getRelativeHumidityFlag());

		assertNull("Inc Alt - No End Alt: end alt", end.getAltitude()); 
		assertEquals("Inc Alt - No End Alt: end press", 900.0, 
				end.getPressure());
		assertEquals("Inc Alt - No End Alt: end temp", 10.0, 
				end.getTemperature());
		assertEquals("Inc Alt - No End Alt: end rh", 5.0, 
				end.getRelativeHumidity());
		assertEquals("Inc Alt - No End Alt: end Press Flag ", 
				UNCHECKED_FLAG, end.getPressureFlag());
		assertEquals("Inc Alt - No End Alt: end Temp Flag ", 
				UNCHECKED_FLAG, end.getTemperatureFlag());
		assertEquals("Inc Alt - No End Alt: end RH Flag ", 
				UNCHECKED_FLAG, end.getRelativeHumidityFlag());
    }
    
    @Test public void increaseAltitudePressureError() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord start = buildPTURecord(1000.0, 20.0, 15.0);
    	ESCSoundingRecord end = buildPTURecord(900.0, 10.0, 5.0);
    	start.setAltitude(5000.0, METERS);
    	end.setAltitude(6000.0, METERS);
    	
    	autoQC.increasingAltitudeCheck(start, end, true, log);
    	
		assertFalse("Inc Alt - Press Err: log ready", reader.ready());
		
		assertEquals("Inc Alt - Press Err: start alt", 5000.0, 
				start.getAltitude()); 
		assertEquals("Inc Alt - Press Err: start press", 1000.0, 
				start.getPressure());
		assertEquals("Inc Alt - Press Err: start temp", 20.0, 
				start.getTemperature());
		assertEquals("Inc Alt - Press Err: start rh", 15.0, 
				start.getRelativeHumidity());
		assertEquals("Inc Alt - Press Err: start Press Flag ", 
				UNCHECKED_FLAG, start.getPressureFlag());
		assertEquals("Inc Alt - Press Err: start Temp Flag ", 
				UNCHECKED_FLAG, start.getTemperatureFlag());
		assertEquals("Inc Alt - Press Err: start RH Flag ", 
				UNCHECKED_FLAG, start.getRelativeHumidityFlag());

		assertEquals("Inc Alt - Press Err: start alt", 6000.0, 
				end.getAltitude()); 
		assertEquals("Inc Alt - Press Err: end press", 900.0, 
				end.getPressure());
		assertEquals("Inc Alt - Press Err: end temp", 10.0, 
				end.getTemperature());
		assertEquals("Inc Alt - Press Err: end rh", 5.0, 
				end.getRelativeHumidity());
		assertEquals("Inc Alt - Press Err: end Press Flag ", 
				UNCHECKED_FLAG, end.getPressureFlag());
		assertEquals("Inc Alt - Press Err: end Temp Flag ", 
				UNCHECKED_FLAG, end.getTemperatureFlag());
		assertEquals("Inc Alt - Press Err: end RH Flag ", 
				UNCHECKED_FLAG, end.getRelativeHumidityFlag());
    }
    
    @Test public void increaseAltitudeDecreasingAlt() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord start = buildPTURecord(1000.0, 20.0, 15.0);
    	ESCSoundingRecord end = buildPTURecord(900.0, 10.0, 5.0);
    	start.setAltitude(5000.0, METERS);
    	end.setAltitude(4999.9, METERS);
    	
    	autoQC.increasingAltitudeCheck(start, end, false, log);
    	
    	assertTrue("Inc Alt - Dec Alt: log ready - pre", reader.ready());
    	assertFalse("Inc Alt - Dec Alt: log output",
    			"".equals(reader.readLine()));
		assertFalse("Inc Alt - Dec Alt: log ready - post", reader.ready());
		
		assertEquals("Inc Alt - Dec Alt: start alt", 5000.0, 
				start.getAltitude()); 
		assertEquals("Inc Alt - Dec Alt: start press", 1000.0, 
				start.getPressure());
		assertEquals("Inc Alt - Dec Alt: start temp", 20.0, 
				start.getTemperature());
		assertEquals("Inc Alt - Dec Alt: start rh", 15.0, 
				start.getRelativeHumidity());
		assertEquals("Inc Alt - Dec Alt: start Press Flag ", 
				QUESTIONABLE_FLAG, start.getPressureFlag());
		assertEquals("Inc Alt - Dec Alt: start Temp Flag ", 
				QUESTIONABLE_FLAG, start.getTemperatureFlag());
		assertEquals("Inc Alt - Dec Alt: start RH Flag ", 
				QUESTIONABLE_FLAG, start.getRelativeHumidityFlag());

		assertEquals("Inc Alt - Dec Alt: start alt", 4999.9, 
				end.getAltitude()); 
		assertEquals("Inc Alt - Dec Alt: end press", 900.0, 
				end.getPressure());
		assertEquals("Inc Alt - Dec Alt: end temp", 10.0, 
				end.getTemperature());
		assertEquals("Inc Alt - Dec Alt: end rh", 5.0, 
				end.getRelativeHumidity());
		assertEquals("Inc Alt - Dec Alt: end Press Flag ", 
				QUESTIONABLE_FLAG, end.getPressureFlag());
		assertEquals("Inc Alt - Dec Alt: end Temp Flag ", 
				QUESTIONABLE_FLAG, end.getTemperatureFlag());
		assertEquals("Inc Alt - Dec Alt: end RH Flag ", 
				QUESTIONABLE_FLAG, end.getRelativeHumidityFlag());
    }
    
    @Test public void increaseAltitudeEqualAlt() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord start = buildPTURecord(1000.0, 20.0, 15.0);
    	ESCSoundingRecord end = buildPTURecord(900.0, 10.0, 5.0);
    	start.setAltitude(5000.0, METERS);
    	end.setAltitude(5000.0, METERS);
    	
    	autoQC.increasingAltitudeCheck(start, end, false, log);
    	
    	assertTrue("Inc Alt - Equal Alt: log ready - pre", reader.ready());
    	assertFalse("Inc Alt - Equal Alt: log output",
    			"".equals(reader.readLine()));
		assertFalse("Inc Alt - Equal Alt: log ready - post", reader.ready());
		
		assertEquals("Inc Alt - Equal Alt: start alt", 5000.0, 
				start.getAltitude()); 
		assertEquals("Inc Alt - Equal Alt: start press", 1000.0, 
				start.getPressure());
		assertEquals("Inc Alt - Equal Alt: start temp", 20.0, 
				start.getTemperature());
		assertEquals("Inc Alt - Equal Alt: start rh", 15.0, 
				start.getRelativeHumidity());
		assertEquals("Inc Alt - Equal Alt: start Press Flag ", 
				QUESTIONABLE_FLAG, start.getPressureFlag());
		assertEquals("Inc Alt - Equal Alt: start Temp Flag ", 
				QUESTIONABLE_FLAG, start.getTemperatureFlag());
		assertEquals("Inc Alt - Equal Alt: start RH Flag ", 
				QUESTIONABLE_FLAG, start.getRelativeHumidityFlag());

		assertEquals("Inc Alt - Equal Alt: start alt", 5000.0, 
				end.getAltitude()); 
		assertEquals("Inc Alt - Equal Alt: end press", 900.0, 
				end.getPressure());
		assertEquals("Inc Alt - Equal Alt: end temp", 10.0, 
				end.getTemperature());
		assertEquals("Inc Alt - Equal Alt: end rh", 5.0, 
				end.getRelativeHumidity());
		assertEquals("Inc Alt - Equal Alt: end Press Flag ", 
				QUESTIONABLE_FLAG, end.getPressureFlag());
		assertEquals("Inc Alt - Equal Alt: end Temp Flag ", 
				QUESTIONABLE_FLAG, end.getTemperatureFlag());
		assertEquals("Inc Alt - Equal Alt: end RH Flag ", 
				QUESTIONABLE_FLAG, end.getRelativeHumidityFlag());
    }
    
    @Test public void increaseAltitudeIncreasingAlt() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord start = buildPTURecord(1000.0, 20.0, 15.0);
    	ESCSoundingRecord end = buildPTURecord(900.0, 10.0, 5.0);
    	start.setAltitude(5000.0, METERS);
    	end.setAltitude(5000.1, METERS);
    	
    	autoQC.increasingAltitudeCheck(start, end, false, log);
    	
		assertFalse("Inc Alt - Inc Alt: log ready", reader.ready());
		
		assertEquals("Inc Alt - Inc Alt: start alt", 5000.0, 
				start.getAltitude()); 
		assertEquals("Inc Alt - Inc Alt: start press", 1000.0, 
				start.getPressure());
		assertEquals("Inc Alt - Inc Alt: start temp", 20.0, 
				start.getTemperature());
		assertEquals("Inc Alt - Inc Alt: start rh", 15.0, 
				start.getRelativeHumidity());
		assertEquals("Inc Alt - Inc Alt: start Press Flag ", 
				UNCHECKED_FLAG, start.getPressureFlag());
		assertEquals("Inc Alt - Inc Alt: start Temp Flag ", 
				UNCHECKED_FLAG, start.getTemperatureFlag());
		assertEquals("Inc Alt - Inc Alt: start RH Flag ", 
				UNCHECKED_FLAG, start.getRelativeHumidityFlag());

		assertEquals("Inc Alt - Inc Alt: start alt", 5000.1, 
				end.getAltitude()); 
		assertEquals("Inc Alt - Inc Alt: end press", 900.0, 
				end.getPressure());
		assertEquals("Inc Alt - Inc Alt: end temp", 10.0, 
				end.getTemperature());
		assertEquals("Inc Alt - Inc Alt: end rh", 5.0, 
				end.getRelativeHumidity());
		assertEquals("Inc Alt - Inc Alt: end Press Flag ", 
				UNCHECKED_FLAG, end.getPressureFlag());
		assertEquals("Inc Alt - Inc Alt: end Temp Flag ", 
				UNCHECKED_FLAG, end.getTemperatureFlag());
		assertEquals("Inc Alt - Inc Alt: end RH Flag ", 
				UNCHECKED_FLAG, end.getRelativeHumidityFlag());
    }
    
    @Test public void nullifyWindsNoValues() {
    	ESCSoundingRecord record = new ESCSoundingRecord();
    	
    	autoQC.nullifyWindValues(record);
    	
    	assertNull("Null Winds - No Values: U Comp", record.getUComponent());
    	assertNull("Null Winds - No Values: V Comp", record.getVComponent());
    	assertNull("Null Winds - No Values: spd", record.getWindSpeed());
    	assertNull("Null Winds - No Values: dir", record.getWindDirection());
    	assertEquals("Null Winds - No Values: U Flag",
    			MISSING_FLAG, record.getUComponentFlag());
    	assertEquals("Null Winds - No Values: V Flag",
    			MISSING_FLAG, record.getVComponentFlag());
    }
    
    @Test public void nullifyWindsAllValues() throws
    ConversionException, InvalidFlagException, InvalidValueWarning {
    	ESCSoundingRecord record = buildWindRecord(5.0, 2.0, 10.0, 90.0);
    	record.setUComponentFlag(GOOD_FLAG);
    	record.setVComponentFlag(BAD_FLAG);
    	
    	autoQC.nullifyWindValues(record);
    	
    	assertNull("Null Winds - All Values: U Comp", record.getUComponent());
    	assertNull("Null Winds - All Values: V Comp", record.getVComponent());
    	assertNull("Null Winds - All Values: spd", record.getWindSpeed());
    	assertNull("Null Winds - All Values: dir", record.getWindDirection());
    	assertEquals("Null Winds - All Values: U Flag",
    			MISSING_FLAG, record.getUComponentFlag());
    	assertEquals("Null Winds - All Values: V Flag",
    			MISSING_FLAG, record.getVComponentFlag());
    }
    
    @Test public void nullifyWindsNoFlags() throws
    ConversionException, InvalidValueWarning {
    	ESCSoundingRecord record = buildWindRecord(5.0, 2.0, 10.0, 90.0);
    	
    	autoQC.nullifyWindValues(record);
    	
    	assertNull("Null Winds - No Flags: U Comp", record.getUComponent());
    	assertNull("Null Winds - No Flags: V Comp", record.getVComponent());
    	assertNull("Null Winds - No Flags: spd", record.getWindSpeed());
    	assertNull("Null Winds - No Flags: dir", record.getWindDirection());
    	assertEquals("Null Winds - No Flags: U Flag",
    			MISSING_FLAG, record.getUComponentFlag());
    	assertEquals("Null Winds - No Flags: V Flag",
    			MISSING_FLAG, record.getVComponentFlag());
    }
    
    @Test public void nullifyWindsComponentsOnly() throws
    ConversionException, InvalidValueWarning {
    	ESCSoundingRecord record = buildWindRecord(5.0, 2.0, null, null);
    	
    	autoQC.nullifyWindValues(record);
    	
    	assertNull("Null Winds - U/V Only: U Comp", record.getUComponent());
    	assertNull("Null Winds - U/V Only: V Comp", record.getVComponent());
    	assertNull("Null Winds - U/V Only: spd", record.getWindSpeed());
    	assertNull("Null Winds - U/V Only: dir", record.getWindDirection());
    	assertEquals("Null Winds - U/V Only: U Flag",
    			MISSING_FLAG, record.getUComponentFlag());
    	assertEquals("Null Winds - U/V Only: V Flag",
    			MISSING_FLAG, record.getVComponentFlag());
    }
    
    @Test public void nullifyWindsComponentsOnlyWithFlags() throws
    ConversionException, InvalidFlagException, InvalidValueWarning {
    	ESCSoundingRecord record = buildWindRecord(5.0, 2.0, null, null);
    	record.setUComponentFlag(BAD_FLAG);
    	record.setVComponentFlag(GOOD_FLAG);
    	
    	autoQC.nullifyWindValues(record);
    	
    	assertNull("Null Winds - U/V w/Flags: U Comp", record.getUComponent());
    	assertNull("Null Winds - U/V w/Flags: V Comp", record.getVComponent());
    	assertNull("Null Winds - U/V w/Flags: spd", record.getWindSpeed());
    	assertNull("Null Winds - U/V w/Flags: dir", record.getWindDirection());
    	assertEquals("Null Winds - U/V w/Flags: U Flag",
    			MISSING_FLAG, record.getUComponentFlag());
    	assertEquals("Null Winds - U/V w/Flags: V Flag",
    			MISSING_FLAG, record.getVComponentFlag());
    }
    
    @Test public void nullifyWindsSpeedDirectionOnly() throws
    ConversionException, InvalidValueWarning {
    	ESCSoundingRecord record = buildWindRecord(null, null, 10.0, 90.0);
    	
    	autoQC.nullifyWindValues(record);
    	
    	assertNull("Null Winds - Spd/Dir: U Comp", record.getUComponent());
    	assertNull("Null Winds - Spd/Dir: V Comp", record.getVComponent());
    	assertNull("Null Winds - Spd/Dir: spd", record.getWindSpeed());
    	assertNull("Null Winds - Spd/Dir: dir", record.getWindDirection());
    	assertEquals("Null Winds - Spd/Dir: U Flag",
    			MISSING_FLAG, record.getUComponentFlag());
    	assertEquals("Null Winds - Spd/Dir: V Flag",
    			MISSING_FLAG, record.getVComponentFlag());
    }
    
    @Test public void altitudeNullValue() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 20.0, 10.0);

    	autoQC.qcAltitudeValue(record, log, 0);
    	
		assertFalse("Altitude Null: log ready", reader.ready());
		assertEquals("Altitude Null: Press Flag ", 
				UNCHECKED_FLAG, record.getPressureFlag());
		assertEquals("Altitude Null: Temp Flag ", 
				UNCHECKED_FLAG, record.getTemperatureFlag());
		assertEquals("Altitude Null: RH Flag ", 
				UNCHECKED_FLAG, record.getRelativeHumidityFlag());
		assertNull("Altitude Null: alt", record.getAltitude()); 
		assertEquals("Altitude Null: press", 1000.0, 
				record.getPressure());
		assertEquals("Altitude Null: temp", 20.0, 
				record.getTemperature());
		assertEquals("Altitude Null: rh", 10.0, 
				record.getRelativeHumidity());
    }
    
    @Test public void altitudeMaxQuestionableBoundary() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 20.0, 10.0);
    	record.setAltitude(40000.0, METERS);

    	autoQC.qcAltitudeValue(record, log, 0);
    	
		assertFalse("Altitude Max Quest Bound: log ready", reader.ready());
		assertEquals("Altitude Max Quest Bound: Press Flag ", 
				UNCHECKED_FLAG, record.getPressureFlag());
		assertEquals("Altitude Max Quest Bound: Temp Flag ", 
				UNCHECKED_FLAG, record.getTemperatureFlag());
		assertEquals("Altitude Max Quest Bound: RH Flag ", 
				UNCHECKED_FLAG, record.getRelativeHumidityFlag());
		assertEquals("Altitude Max Quest Bound: ascent alt", 40000.0,
				record.getAltitude()); 
		assertEquals("Altitude Max Quest Bound: press", 1000.0, 
				record.getPressure());
		assertEquals("Altitude Max Quest Bound: temp", 20.0, 
				record.getTemperature());
		assertEquals("Altitude Max Quest Bound: rh", 10.0, 
				record.getRelativeHumidity());
    }
    
    @Test public void altitudeMaxQuestionableBoundaryViolation() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 20.0, 10.0);
    	record.setAltitude(40000.1, METERS);

    	autoQC.qcAltitudeValue(record, log, 0);
    	
		assertTrue("Altitude Max Quest Bound Violation: log ready - pre",
				reader.ready());
		assertFalse("Altitude Max Quest Bound Violation: log output",
				"".equals(reader.readLine()));
		assertFalse("Altitude Max Quest Bound Violation: log ready - post",
				reader.ready());
		assertEquals("Altitude Max Quest Bound Violation: Press Flag ", 
				QUESTIONABLE_FLAG, record.getPressureFlag());
		assertEquals("Altitude Max Quest Bound Violation: Temp Flag ", 
				QUESTIONABLE_FLAG, record.getTemperatureFlag());
		assertEquals("Altitude Max Quest Bound Violation: RH Flag ", 
				QUESTIONABLE_FLAG, record.getRelativeHumidityFlag());
		assertEquals("Altitude Max Quest Bound Violation: ascent alt", 40000.1,
				record.getAltitude()); 
		assertEquals("Altitude Max Quest Bound Violation: press", 1000.0, 
				record.getPressure());
		assertEquals("Altitude Max Quest Bound Violation: temp", 20.0, 
				record.getTemperature());
		assertEquals("Altitude Max Quest Bound Violation: rh", 10.0, 
				record.getRelativeHumidity());
    }
    
    @Test public void altitudeMinQuestionableBoundary() throws 
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 20.0, 10.0);
    	record.setAltitude(0.0, METERS);

    	autoQC.qcAltitudeValue(record, log, 0);
    	
		assertFalse("Altitude Min Quest Bound: log ready", reader.ready());
		assertEquals("Altitude Min Quest Bound: Press Flag ", 
				UNCHECKED_FLAG, record.getPressureFlag());
		assertEquals("Altitude Min Quest Bound: Temp Flag ", 
				UNCHECKED_FLAG, record.getTemperatureFlag());
		assertEquals("Altitude Min Quest Bound: RH Flag ", 
				UNCHECKED_FLAG, record.getRelativeHumidityFlag());
		assertEquals("Altitude Min Quest Bound: ascent alt", 0.0,
				record.getAltitude()); 
		assertEquals("Altitude Min Quest Bound: press", 1000.0, 
				record.getPressure());
		assertEquals("Altitude Min Quest Bound: temp", 20.0, 
				record.getTemperature());
		assertEquals("Altitude Min Quest Bound: rh", 10.0, 
				record.getRelativeHumidity());
    }
    
    @Test public void altitudeMinQuestionableBoundaryViolation() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 20.0, 10.0);
    	record.setAltitude(-0.1, METERS);

    	autoQC.qcAltitudeValue(record, log, 0);
    	
		assertTrue("Altitude Min Quest Bound Violation: log ready - pre",
				reader.ready());
		assertFalse("Altitude Min Quest Bound Violation: log output",
				"".equals(reader.readLine()));
		assertFalse("Altitude Min Quest Bound Violation: log ready - post",
				reader.ready());
		assertEquals("Altitude Min Quest Bound Violation: Press Flag ", 
				QUESTIONABLE_FLAG, record.getPressureFlag());
		assertEquals("Altitude Min Quest Bound Violation: Temp Flag ", 
				QUESTIONABLE_FLAG, record.getTemperatureFlag());
		assertEquals("Altitude Min Quest Bound Violation: RH Flag ", 
				QUESTIONABLE_FLAG, record.getRelativeHumidityFlag());
		assertEquals("Altitude Min Quest Bound Violation: ascent alt", -0.1,
				record.getAltitude()); 
		assertEquals("Altitude Min Quest Bound Violation: press", 1000.0, 
				record.getPressure());
		assertEquals("Altitude Min Quest Bound Violation: temp", 20.0, 
				record.getTemperature());
		assertEquals("Altitude Min Quest Bound Violation: rh", 10.0, 
				record.getRelativeHumidity());
    }
    
    
    @Test public void ascentRateNullValue() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 20.0, 10.0);

    	autoQC.qcAscentRateValue(record, log, 0);
    	
		assertFalse("Ascent Rate Null: log ready", reader.ready());
		assertEquals("Ascent Rate Null: Press Flag ", 
				UNCHECKED_FLAG, record.getPressureFlag());
		assertEquals("Ascent Rate Null: Ascent Rate Flag ", 
				MISSING_FLAG, record.getAscentRateFlag());
		assertNull("Ascent Rate Null: ascent rate", record.getAscentRate()); 
		assertEquals("Ascent Rate Null: press", 1000.0, 
				record.getPressure());
    }
    
    @Test public void ascentRateMaxPositiveQuestionableBoundary() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 20.0, 10.0);
    	record.setAscentRate(10.0, METERS_PER_SECOND);

    	autoQC.qcAscentRateValue(record, log, 0);
    	
		assertFalse("Ascent Rate Pos Max Quest Bound: log ready", 
				reader.ready());
		assertEquals("Ascent Rate Pos Max Quest Bound: Press Flag ", 
				UNCHECKED_FLAG, record.getPressureFlag());
		assertEquals("Ascent Rate Pos Max Quest Bound: Ascent Rate Flag ", 
				UNCHECKED_FLAG, record.getAscentRateFlag());
		assertEquals("Ascent Rate Pos Max Quest Bound: ascent rate", 10.0, 
				record.getAscentRate()); 
		assertEquals("Ascent Rate Pos Max Quest Bound: press", 1000.0, 
				record.getPressure());
    }
    
    @Test public void ascentRatePositiveMaxQuestionableBoundaryViolation() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 20.0, 10.0);
    	record.setAscentRate(10.1, METERS_PER_SECOND);

    	autoQC.qcAscentRateValue(record, log, 0);
    	
		assertTrue("Ascent Rate Pos Max Quest Bound Violation: log ready - pre", 
				reader.ready());
		assertFalse("Ascent Rate Pos Max Quest Bound Violation: log output", 
				"".equals(reader.readLine()));
		assertFalse("Ascent Rate Pos Max Quest Bound Violation: log ready - post", 
				reader.ready());
		assertEquals("Ascent Rate Pos Max Quest Bound Violation: Press Flag ", 
				QUESTIONABLE_FLAG, record.getPressureFlag());
		assertEquals("Ascent Rate Pos Max Quest Bound Violation: Ascent Rate Flag ", 
				UNCHECKED_FLAG, record.getAscentRateFlag());
		assertEquals("Ascent Rate Pos Max Quest Bound Violation: ascent rate",
				10.1, record.getAscentRate()); 
		assertEquals("Ascent Rate Pos Max Quest Bound Violation: press", 1000.0, 
				record.getPressure());
    }
    
    @Test public void ascentRateNegativeMaxQuestionableBoundary() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 20.0, 10.0);
    	record.setAscentRate(-10.0, METERS_PER_SECOND);

    	autoQC.qcAscentRateValue(record, log, 0);
    	
		assertFalse("Ascent Rate Neg Max Quest Bound: log ready", 
				reader.ready());
		assertEquals("Ascent Rate Neg Max Quest Bound: Press Flag ", 
				UNCHECKED_FLAG, record.getPressureFlag());
		assertEquals("Ascent Rate Neg Max Quest Bound: Ascent Rate Flag ", 
				UNCHECKED_FLAG, record.getAscentRateFlag());
		assertEquals("Ascent Rate Neg Max Quest Bound: ascent rate", -10.0, 
				record.getAscentRate()); 
		assertEquals("Ascent Rate Neg Max Quest Bound: press", 1000.0, 
				record.getPressure());
    }
    
    @Test public void ascentRateNegativeMaxQuestionableBoundaryViolation() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 20.0, 10.0);
    	record.setAscentRate(-10.1, METERS_PER_SECOND);

    	autoQC.qcAscentRateValue(record, log, 0);
    	
		assertTrue("Ascent Rate Neg Max Quest Bound Violation: log ready - pre", 
				reader.ready());
		assertFalse("Ascent Rate Neg Max Quest Bound Violation: log output", 
				"".equals(reader.readLine()));
		assertFalse("Ascent Rate Neg Max Quest Bound Violation: log ready - post", 
				reader.ready());
		assertEquals("Ascent Rate Neg Max Quest Bound Violation: Press Flag ", 
				QUESTIONABLE_FLAG, record.getPressureFlag());
		assertEquals("Ascent Rate Neg Max Quest Bound Violation: Ascent Rate Flag ", 
				UNCHECKED_FLAG, record.getAscentRateFlag());
		assertEquals("Ascent Rate Neg Max Quest Bound Violation: ascent rate",
				-10.1, record.getAscentRate()); 
		assertEquals("Ascent Rate Neg Max Quest Bound Violation: press", 1000.0, 
				record.getPressure());
    }
    
    @Test public void dewPointFieldNullValue() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 20.0, null);
    	
    	autoQC.qcDewPointField(record, log);
    	
		assertFalse("Dew Point Field Null: log ready", reader.ready());
		assertEquals("Dew Point Field Null: RH Flag ", 
				MISSING_FLAG, record.getRelativeHumidityFlag());
		assertEquals("Dew Point Field Null: Temp Flag ", 
				UNCHECKED_FLAG, record.getTemperatureFlag());
		assertNull("Dew Point Field Null: dew point", record.getDewPoint()); 
		assertNull("Dew Point Field Null: rh", record.getRelativeHumidity()); 
		assertEquals("Dew Point Field Null: temp", 20.0, 
				record.getTemperature());
    }
    
    @Test public void dewPointFieldLowerBoundary() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 20.0, 12.0);
    	record.setDewPoint(-99.8, CELCIUS);
    	
    	autoQC.qcDewPointField(record, log);
    	
		assertFalse("Dew Point Field Lower Bound: log ready", reader.ready());
		assertEquals("Dew Point Field Lower Bound: RH Flag ", 
				UNCHECKED_FLAG, record.getRelativeHumidityFlag());
		assertEquals("Dew Point Field Lower Bound: Temp Flag ", 
				UNCHECKED_FLAG, record.getTemperatureFlag());
		assertEquals("Dew Point Field Lower Bound: dew point", -99.8, 
				record.getDewPoint()); 
		assertEquals("Dew Point Field Lower Bound: rh", 12.0,
				record.getRelativeHumidity()); 
		assertEquals("Dew Point Field Lower Bound: temp", 20.0, 
				record.getTemperature());
    }
    
    @Test public void dewPointFieldLowerBoundaryViolation() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 20.0, 12.0);
    	record.setDewPoint(-99.9, CELCIUS);
    	
    	autoQC.qcDewPointField(record, log);
    	
    	assertTrue("Dew Point Field Lower Bound Violation: log ready - pre",
    			reader.ready());
    	assertFalse("Dew Point Field Lower Bound Violation: log output",
    			"".equals(reader.readLine()));
		assertFalse("Dew Point Field Lower Bound Violation: log ready - post",
				reader.ready());
		assertEquals("Dew Point Field Lower Bound Violation: RH Flag ", 
				QUESTIONABLE_FLAG, record.getRelativeHumidityFlag());
		assertEquals("Dew Point Field Lower Bound Violation: Temp Flag ", 
				UNCHECKED_FLAG, record.getTemperatureFlag());
		assertEquals("Dew Point Field Lower Bound Violation: dew point", -99.9, 
				record.getDewPoint()); 
		assertEquals("Dew Point Field Lower Bound Violation: rh", 12.0,
				record.getRelativeHumidity()); 
		assertEquals("Dew Point Field Lower Bound Violation: temp", 20.0, 
				record.getTemperature());
    }
    
    
    @Test public void dewPointValueNullValue() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 20.0, null);
    	
    	autoQC.qcDewPointValue(record, log, 0);
    	
		assertFalse("Dew Point Null: log ready", reader.ready());
		assertEquals("Dew Point Null: RH Flag ", 
				MISSING_FLAG, record.getRelativeHumidityFlag());
		assertEquals("Dew Point Null: Temp Flag ", 
				UNCHECKED_FLAG, record.getTemperatureFlag());
		assertNull("Dew Point Null: dew point", record.getDewPoint()); 
		assertNull("Dew Point Null: rh", record.getRelativeHumidity()); 
		assertEquals("Dew Point Null: temp", 20.0, record.getTemperature()); 
    }
    
    @Test public void dewPointValueNullRH() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, -10.0, null);
    	record.setDewPoint(900.0, CELCIUS);
    	
    	autoQC.qcDewPointValue(record, log, 0);
    	
		assertFalse("Dew Point Null RH: log ready", reader.ready());
		assertEquals("Dew Point Null RH: RH Flag ", 
				MISSING_FLAG, record.getRelativeHumidityFlag());
		assertEquals("Dew Point Null RH: Temp Flag ", 
				UNCHECKED_FLAG, record.getTemperatureFlag());
		assertEquals("Dew Point Null RH: dew point", 900.0,
				record.getDewPoint()); 
		assertNull("Dew Point Null RH: rh", record.getRelativeHumidity()); 
		assertEquals("Dew Point Null RH: temp", -10.0, record.getTemperature());
    }
    
    @Test public void dewPointValueMaxQuestionableBoundary() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 77.0, null);
    	record.setDewPoint(33.0, CELCIUS);
    	
    	autoQC.qcDewPointValue(record, log, 0);
    	
		assertFalse("Dew Point Max Quest Boundary: log ready", reader.ready());
		assertEquals("Dew Point Max Quest Boundary: RH Flag ", 
				UNCHECKED_FLAG, record.getRelativeHumidityFlag());
		assertEquals("Dew Point Max Quest Boundary: Temp Flag ", 
				UNCHECKED_FLAG, record.getTemperatureFlag());
		assertEquals("Dew Point Max Quest Boundary: dew point", 33.0,
				record.getDewPoint()); 
		assertEquals("Dew Point Max Quest Boundary: rh", 11.808936,
				record.getRelativeHumidity(), THRESHOLD); 
		assertEquals("Dew Point Max Quest Boundary: temp", 77.0,
				record.getTemperature());
    }
    
    @Test public void dewPointValueMaxQuestionableBoundaryViolation() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 77.0, null);
    	record.setDewPoint(33.1, CELCIUS);
    	
    	autoQC.qcDewPointValue(record, log, 0);
    	
    	assertTrue("Dew Point Max Quest Boundary Violation: log ready - pre",
    			reader.ready());
    	assertFalse("Dew Point Max Quest Boundary Violation: log output",
    			"".equals(reader.readLine()));
		assertFalse("Dew Point Max Quest Boundary Violation: log ready - post",
				reader.ready());
		assertEquals("Dew Point Max Quest Boundary Violation: RH Flag ", 
				QUESTIONABLE_FLAG, record.getRelativeHumidityFlag());
		assertEquals("Dew Point Max Quest Boundary Violation: Temp Flag ", 
				UNCHECKED_FLAG, record.getTemperatureFlag());
		assertEquals("Dew Point Max Quest Boundary Violation: dew point", 33.1,
				record.getDewPoint()); 
		assertEquals("Dew Point Max Quest Boundary Violation: rh", 11.875558,
				record.getRelativeHumidity(), THRESHOLD); 
		assertEquals("Dew Point Max Quest Boundary Violation: temp", 77.0,
				record.getTemperature());
    }
    
    @Test public void dewPointValueMaxMissingBoundary() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 77.0, null);
    	record.setDewPoint(74.9, CELCIUS);
    	
    	autoQC.qcDewPointValue(record, log, 0);
    	
    	assertTrue("Dew Point Max Miss Boundary: log ready - pre",
    			reader.ready());
    	assertFalse("Dew Point Max Miss Boundary: log output",
    			"".equals(reader.readLine()));
		assertFalse("Dew Point Max Miss Boundary: log ready - post",
				reader.ready());
		assertEquals("Dew Point Max Miss Boundary: RH Flag ", 
				QUESTIONABLE_FLAG, record.getRelativeHumidityFlag());
		assertEquals("Dew Point Max Miss Boundary: Temp Flag ", 
				UNCHECKED_FLAG, record.getTemperatureFlag());
		assertEquals("Dew Point Max Miss Boundary: dew point", 74.9,
				record.getDewPoint()); 
		assertEquals("Dew Point Max Miss Boundary: rh", 91.526385,
				record.getRelativeHumidity(), THRESHOLD); 
		assertEquals("Dew Point Max Miss Boundary: temp", 77.0,
				record.getTemperature());
    }
    
    @Test public void dewPointValueMaxMissingBoundaryViolation() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 77.0, null);
    	record.setDewPoint(75.0, CELCIUS);
    	
    	autoQC.qcDewPointValue(record, log, 0);
    	
    	assertTrue("Dew Point Max Miss Boundary Violation: log ready - pre",
    			reader.ready());
    	assertFalse("Dew Point Max Miss Boundary Violation: log output",
    			"".equals(reader.readLine()));
		assertFalse("Dew Point Max Miss Boundary Violation: log ready - post",
				reader.ready());
		assertEquals("Dew Point Max Miss Boundary Violation: RH Flag ", 
				MISSING_FLAG, record.getRelativeHumidityFlag());
		assertEquals("Dew Point Max Miss Boundary Violation: Temp Flag ", 
				UNCHECKED_FLAG, record.getTemperatureFlag());
		assertNull("Dew Point Max Miss Boundary Violation: dew point", 
				record.getDewPoint()); 
		assertNull("Dew Point Max Miss Boundary Violation: rh", 
				record.getRelativeHumidity()); 
		assertEquals("Dew Point Max Miss Boundary Violation: temp", 77.0,
				record.getTemperature());
    }
    
    @Test public void dewPointValueDewPointEqualTemp() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 20.0, null);
    	record.setDewPoint(20.0, CELCIUS);
    	
    	autoQC.qcDewPointValue(record, log, 0);
    	
		assertFalse("Dew Point Equal Temp: log ready", reader.ready());
		assertEquals("Dew Point Equal Temp: RH Flag ", 
				UNCHECKED_FLAG, record.getRelativeHumidityFlag());
		assertEquals("Dew Point Equal Temp: Temp Flag ", 
				UNCHECKED_FLAG, record.getTemperatureFlag());
		assertEquals("Dew Point Equal Temp: dew point", 20.0,
				record.getDewPoint()); 
		assertEquals("Dew Point Equal Temp: rh", 100.0,
				record.getRelativeHumidity(), THRESHOLD); 
		assertEquals("Dew Point Equal Temp: temp", 20.0,
				record.getTemperature());
    }
    
    @Test public void dewPointValueDewPointGreaterThanTemp() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 20.0, null);
    	record.setDewPoint(20.1, CELCIUS);
    	
    	autoQC.qcDewPointValue(record, log, 0);
    	
    	assertTrue("Dew Point Temp > Dew Point: log ready - pre",
    			reader.ready());
    	assertFalse("Dew Point Temp > Dew Point: log output",
    			"".equals(reader.readLine()));
		assertFalse("Dew Point Temp > Dew Point: log ready - post",
				reader.ready());
		assertEquals("Dew Point Temp > Dew Point: RH Flag ", 
				QUESTIONABLE_FLAG, record.getRelativeHumidityFlag());
		assertEquals("Dew Point Temp > Dew Point: Temp Flag ", 
				UNCHECKED_FLAG, record.getTemperatureFlag());
		assertEquals("Dew Point Temp > Dew Point: dew point", 20.1, 
				record.getDewPoint()); 
		assertEquals("Dew Point Temp > Dew Point: rh", 100.621377,
				record.getRelativeHumidity(), THRESHOLD); 
		assertEquals("Dew PointTemp > Dew Point: temp", 20.0,
				record.getTemperature());
    }
    
    
    @Test public void pressureNullValue() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(null, 20.0, 50.0);
    	
    	autoQC.qcPressureValue(record, log, 0);
    	
		assertFalse("Press Null: log ready", reader.ready());
		assertEquals("Press Null: Press Flag ", 
				MISSING_FLAG, record.getPressureFlag());
		assertNull("Press Null: pressure", record.getPressure()); 
    }
    
    @Test public void pressureMaxMissingBoundary() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1099.9, 20.0, 50.0);
    	
    	autoQC.qcPressureValue(record, log, 0);
    	
    	assertTrue("Press Max Miss Boundary: log ready - pre", reader.ready());
    	assertFalse("Press Max Miss Boundary: log output",
    			"".equals(reader.readLine()));
		assertFalse("Press Max Miss Boundary: log ready - post",reader.ready());
		assertEquals("Press Max Miss Boundary: Press Flag ", 
				BAD_FLAG, record.getPressureFlag());
		assertEquals("Press Max Miss Boundary: pressure", 1099.9, 
				record.getPressure()); 
    }
    
    @Test public void pressureMaxMissingBoundaryViolation() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1100.0, 20.0, 50.0);
    	
    	autoQC.qcPressureValue(record, log, 0);
    	
    	assertTrue("Press Max Miss Boundary Violation: log ready - pre", 
    			reader.ready());
    	assertFalse("Press Max Miss Boundary Violation: log output",
    			"".equals(reader.readLine()));
		assertFalse("Press Max Miss Boundary Violation: log ready - post",
				reader.ready());
		assertEquals("Press Max Miss Boundary Violation: Press Flag ", 
				MISSING_FLAG, record.getPressureFlag());
		assertNull("Press Max Miss Boundary Violation: pressure",  
				record.getPressure());
    }
    
    @Test public void pressureMaxQuestionableBoundary() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1050.0, 20.0, 50.0);
    	
    	autoQC.qcPressureValue(record, log, 0);

		assertFalse("Press Max Quest Boundary: log ready - post",
				reader.ready());
		assertEquals("Press Max Quest Boundary: Press Flag ", 
				UNCHECKED_FLAG, record.getPressureFlag());
		assertEquals("Press Max Quest Boundary: pressure", 1050.0, 
				record.getPressure());
    }
    
    @Test public void pressureMaxQuestionableBoundaryViolation() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1050.1, 20.0, 50.0);
    	
    	autoQC.qcPressureValue(record, log, 0);
    	
    	assertTrue("Press Max Quest Boundary Violation: log ready - pre", 
    			reader.ready());
    	assertFalse("Press Max Quest Boundary Violation: log output",
    			"".equals(reader.readLine()));
		assertFalse("Press Max Quest Boundary Violation: log ready - post",
				reader.ready());
		assertEquals("Press MaxQuest Boundary Violation: Press Flag ", 
				BAD_FLAG, record.getPressureFlag());
		assertEquals("Press Max Quest Boundary Violation: pressure", 1050.1, 
				record.getPressure());
    }
    
    @Test public void pressureMinQuestionableBoundary() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(0.1, 20.0, 50.0);
    	
    	autoQC.qcPressureValue(record, log, 0);

		assertFalse("Press Min Quest Boundary: log ready - post",
				reader.ready());
		assertEquals("Press Min Quest Boundary: Press Flag ", 
				UNCHECKED_FLAG, record.getPressureFlag());
		assertEquals("Press Min Quest Boundary: pressure", 0.1, 
				record.getPressure());
    }
    
    @Test public void pressureMinQuestionableBoundaryViolation() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(0.0, 20.0, 50.0);
    	
    	autoQC.qcPressureValue(record, log, 0);
    	
    	assertTrue("Press Min Quest Boundary Violation: log ready - pre", 
    			reader.ready());
    	assertFalse("Press Min Quest Boundary Violation: log output",
    			"".equals(reader.readLine()));
		assertFalse("Press Min Quest Boundary Violation: log ready - post",
				reader.ready());
		assertEquals("Press Min Quest Boundary Violation: Press Flag ", 
				BAD_FLAG, record.getPressureFlag());
		assertEquals("Press Min Quest Boundary Violation: pressure", 0.0, 
				record.getPressure());
    }
    
    
    @Test public void rhNullValue() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 20.0, null);
    	
    	autoQC.qcRelativeHumidityValue(record, log, 0);
    	
		assertFalse("RH Null: log ready", reader.ready());
		assertEquals("RH Null: RH Flag ", 
				MISSING_FLAG, record.getRelativeHumidityFlag());
		assertNull("RH Null: rh", record.getRelativeHumidity()); 
    }
    
    @Test public void rhUpperBoundary() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 20.0, 100.0);
    	
    	autoQC.qcRelativeHumidityValue(record, log, 0);
    	
		assertFalse("RH Upper Bound: log ready", reader.ready());
		assertEquals("RH Upper Bound: RH Flag ", 
				UNCHECKED_FLAG, record.getRelativeHumidityFlag());
		assertEquals("RH Upper Bound: rh", 100.0, record.getRelativeHumidity()); 
    }
    
    @Test public void rhUpperBoundaryViolation() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 20.0, 100.1);
    	
    	autoQC.qcRelativeHumidityValue(record, log, 0);
    	
    	assertTrue("RH Upper Bound Violation: log ready - pre", reader.ready());
    	assertFalse("RH Upper Bound Violation: log output",
    			"".equals(reader.readLine()));
		assertFalse("RH Upper Bound Violation: log ready - post",
				reader.ready());
		assertEquals("RH Upper Bound Violation: RH Flag ", 
				QUESTIONABLE_FLAG, record.getRelativeHumidityFlag());
		assertEquals("RH Upper Bound Violation: rh", 100.1,
				record.getRelativeHumidity()); 
    }
    
    @Test public void rhLowerBoundary() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 20.0, 0.0);
    	
    	autoQC.qcRelativeHumidityValue(record, log, 0);
    	
		assertFalse("RH Lower Bound: log ready", reader.ready());
		assertEquals("RH Lower Bound: RH Flag ", 
				UNCHECKED_FLAG, record.getRelativeHumidityFlag());
		assertEquals("RH Lower Bound: rh", 0.0, record.getRelativeHumidity());
    }
    
    @Test public void rhLowerBoundaryViolation() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 20.0, -0.1);

    	autoQC.qcRelativeHumidityValue(record, log, 0);

    	assertTrue("RH Lower Bound Violation: log ready - pre", reader.ready());
    	assertFalse("RH Lower Bound Violation: log output",
    			"".equals(reader.readLine()));
    	assertFalse("RH Lower Bound Violation: log ready - post",
    			reader.ready());
    	assertEquals("RH Lower Bound Violation: RH Flag ", 
    			MISSING_FLAG, record.getRelativeHumidityFlag());
    	assertNull("RH Lower Bound Violation: rh", 
    			record.getRelativeHumidity());    
    }
    
    @Test public void rhTempNotEqualDewPt() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 20.0, 100.0);
    	record.setDewPoint(21.0, CELCIUS);

    	autoQC.qcRelativeHumidityValue(record, log, 0);

    	assertFalse("RH Temp != DewPt: log ready", reader.ready());
    	assertEquals("RH Temp != DewPt: RH Flag ", 
    			UNCHECKED_FLAG, record.getRelativeHumidityFlag());
    	assertEquals("RH Temp != DewPt: rh", 100.0, 
    			record.getRelativeHumidity());
    	assertEquals("RH Temp != DewPt: temp", 20.0, record.getTemperature());
    	assertEquals("RH Temp != DewPt: dewpt", 20.0, record.getDewPoint());
    }

    @Test public void timeValueNullValue() throws CalculationWarning,
    InvalidValueWarning, IOException {
    	ESCSoundingRecord record = new ESCSoundingRecord();
    	
    	autoQC.qcTimeValue(record, log, 0);
    	
		assertFalse("Time Null: log ready", reader.ready());
		assertNull("Time Null: time", record.getTime()); 
    }
    
    @Test public void timeValueLowerBoundary() throws CalculationWarning,
    InvalidValueWarning, IOException {
    	ESCSoundingRecord record = new ESCSoundingRecord();
    	record.setTime(-600.0);
    	
    	autoQC.qcTimeValue(record, log, 0);
    	
		assertFalse("Time Lower Boundary: log ready", reader.ready());
		assertEquals("Time Lower Boundary: time", -600.0, record.getTime()); 
	}
    
    @Test public void timeValueLowerBoundaryViolation() throws 
    CalculationWarning, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = new ESCSoundingRecord();
    	record.setTime(-600.1);
    	
    	autoQC.qcTimeValue(record, log, 0);
    	
    	assertTrue("Time Lower Boundary Violation: log ready - pre", 
    			reader.ready());
    	assertFalse("Time Lower Boundary Violation: log output",
    			"".equals(reader.readLine()));
		assertFalse("Time Lower Boundary Violation: log ready - post",
				reader.ready());
		assertEquals("Time Lower Boundary Violation: time", -600.1, 
				record.getTime()); 
    }
    
    @Test public void temperatureNullValue() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, null, 50.0);
    
    	autoQC.qcTemperatureValue(record, log, 0);
    	
		assertFalse("Temperature Null: log ready", reader.ready());
		assertEquals("Temperature Null: Temp Flag ", 
				MISSING_FLAG, record.getTemperatureFlag());
		assertNull("Temperature Null: temperature", 
				record.getTemperature());
    }
    
    @Test public void temperatureValidValue() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 20.0, 50.0);
        
    	autoQC.qcTemperatureValue(record, log, 0);
    	
		assertFalse("Temperature Valid: log ready", reader.ready());
		assertEquals("Temperature Valid: Temp Flag ", 
				UNCHECKED_FLAG, record.getTemperatureFlag());
		assertEquals("Temperature Valid: temperature", 20.0, 
				record.getTemperature());
    }
    
    @Test public void temperatureMaximumMissingValue() throws 
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 75.0, 50.0);
    
    	autoQC.qcTemperatureValue(record, log, 0);
    	
    	assertTrue("Temperature Max Missing: log ready - pre",
    			reader.ready());
    	assertFalse("Temperature Max Missing: log output",
    			"".equals(reader.readLine()));
		assertFalse("Temperature Max Missing: log ready - post",
				reader.ready());
		assertEquals("Temperature Max Missing: Temp Flag ", 
				MISSING_FLAG, record.getTemperatureFlag());
		assertNull("Temperature Max Missing: temperature", 
				record.getTemperature());
    }
    
    @Test public void temperatureMaximumQuestionableBoundaryViolation() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 45.1, 50.0);
        
    	autoQC.qcTemperatureValue(record, log, 0);
    	
    	assertTrue("Temperature Max Questionable Violation: log ready - pre",
    			reader.ready());
    	assertFalse("Temperature Max Questionable Violation: log output",
    			"".equals(reader.readLine()));
		assertFalse("Temperature Max Questionable Violation: log ready - post",
				reader.ready());
		assertEquals("Temperature Max Questionable Violation: Temp Flag ", 
				QUESTIONABLE_FLAG, record.getTemperatureFlag());
		assertEquals("Temperature Max Questionable Violation: temperature", 45.1, 
				record.getTemperature());
    }
    
    @Test public void temperatureMinimumQuestionableBoundaryViolation() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, -90.1, 50.0);
        
    	autoQC.qcTemperatureValue(record, log, 0);
    	
    	assertTrue("Temperature Min Questionable Violation: log ready - pre",
    			reader.ready());
    	assertFalse("Temperature Min Questionable Violation: log output",
    			"".equals(reader.readLine()));
		assertFalse("Temperature Min Questionable Violation: log ready - post",
				reader.ready());
		assertEquals("Temperature Min Questionable Violation: Temp Flag ", 
				QUESTIONABLE_FLAG, record.getTemperatureFlag());
		assertEquals("Temperature Min Questionable Violation: temperature", -90.1, 
				record.getTemperature());
    }
    
    @Test public void temperatureMaximumQuestionableBoundary() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, 45.0, 50.0);
        
    	autoQC.qcTemperatureValue(record, log, 0);
    	
		assertFalse("Temperature Max Questionable: log ready",
				reader.ready());
		assertEquals("Temperature Max Questionable: Temp Flag ", 
				UNCHECKED_FLAG, record.getTemperatureFlag());
		assertEquals("Temperature Max Questionable: temperature", 45.0, 
				record.getTemperature());
    }
    
    @Test public void temperatureMinimumQuestionableBoundary() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildPTURecord(1000.0, -90.0, 50.0);
        
    	autoQC.qcTemperatureValue(record, log, 0);
    	
		assertFalse("Temperature Min Questionable: log ready",
				reader.ready());
		assertEquals("Temperature Min Questionable: Temp Flag ", 
				UNCHECKED_FLAG, record.getTemperatureFlag());
		assertEquals("Temperature Min Questionable: temperature", -90.0, 
				record.getTemperature());
    }

    
    @Test public void uvFieldsNullUwithVNoWinds() throws ConversionException,
    InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(null, 5.0, null, null);
    	
    	autoQC.qcUVFields(record, log);
    	
		assertTrue("UV Null U / No Wind: log ready pre",
				reader.ready());
		assertFalse("UV Null U / No Wind: log output 1",
				"".equals(reader.readLine()));
		assertFalse("UV Null U / No Wind: log output 2",
				"".equals(reader.readLine()));
		assertFalse("UV Null U / No Wind: log ready post",
				reader.ready());
		assertEquals("UV Null U / No Wind: U Comp Flag ", 
				MISSING_FLAG, record.getUComponentFlag());
		assertEquals("UV Null U / No Wind: V Comp Flag ", 
				MISSING_FLAG, record.getVComponentFlag());
		assertNull("UV Null U / No Wind: wind speed", 
				record.getWindSpeed());
		assertNull("UV Null U / No Wind: wind direction", 
				record.getWindDirection());
		assertNull("UV Null U / No Wind: u component", 
				record.getUComponent());
		assertNull("UV Null U / No Wind: v component", 
				record.getVComponent());
    }
    
    @Test public void uvFieldNullVwithUNoWinds() throws ConversionException,
    InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(5.0, null, null, null);
    	
    	autoQC.qcUVFields(record, log);
    	
		assertTrue("UV Null V / No Wind: log ready pre",
				reader.ready());
		assertFalse("UV Null V / No Wind: log output 1",
				"".equals(reader.readLine()));
		assertFalse("UV Null V / No Wind: log output 2",
				"".equals(reader.readLine()));
		assertFalse("UV Null V / No Wind: log ready post",
				reader.ready());
		assertEquals("UV Null V / No Wind: U Comp Flag ", 
				MISSING_FLAG, record.getUComponentFlag());
		assertEquals("UV Null V / No Wind: V Comp Flag ", 
				MISSING_FLAG, record.getVComponentFlag());
		assertNull("UV Null V / No Wind: wind speed", 
				record.getWindSpeed());
		assertNull("UV Null V / No Wind: wind direction", 
				record.getWindDirection());
		assertNull("UV Null V / No Wind: u component", 
				record.getUComponent());
		assertNull("UV Null V / No Wind: v component", 
				record.getVComponent());
    }
    
    @Test public void uvFieldsNullUwithVandWinds() throws ConversionException,
    InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(null, 5.0, 10.0, 90.0);
    	
    	autoQC.qcUVFields(record, log);
    	
		assertTrue("UV Null U / Wind: log ready pre",
				reader.ready());
		assertFalse("UV Null U / Wind: log output 1",
				"".equals(reader.readLine()));
		assertFalse("UV Null U / Wind: log output 2",
				"".equals(reader.readLine()));
		assertFalse("UV Null U / Wind: log ready post",
				reader.ready());
		assertEquals("UV Null U / Wind: U Comp Flag ", 
				UNCHECKED_FLAG, record.getUComponentFlag());
		assertEquals("UV Null U / Wind: V Comp Flag ", 
				UNCHECKED_FLAG, record.getVComponentFlag());
		assertEquals("UV Null U / Wind: wind speed", 10.0, 
				record.getWindSpeed());
		assertEquals("UV Null U / Wind: wind direction", 90.0,
				record.getWindDirection());
		assertEquals("UV Null U / Wind: u component", -10.0,
				record.getUComponent(), THRESHOLD);
		assertEquals("UV Null U / Wind: v component", 0.0,
				record.getVComponent(), THRESHOLD);
    }
    
    @Test public void uvFieldsNullVwithUandWinds() throws ConversionException,
    InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(5.0, null, 10.0, 90.0);
    	
    	autoQC.qcUVFields(record, log);
    	
		assertTrue("UV Null V / Wind: log ready pre",
				reader.ready());
		assertFalse("UV Null V / Wind: log output 1",
				"".equals(reader.readLine()));
		assertFalse("UV Null V / Wind: log output 2",
				"".equals(reader.readLine()));
		assertFalse("UV Null V / Wind: log ready post",
				reader.ready());
		assertEquals("UV Null V / Wind: U Comp Flag ", 
				UNCHECKED_FLAG, record.getUComponentFlag());
		assertEquals("UV Null V / Wind: V Comp Flag ", 
				UNCHECKED_FLAG, record.getVComponentFlag());
		assertEquals("UV Null V / Wind: wind speed", 10.0, 
				record.getWindSpeed());
		assertEquals("UV Null V / Wind: wind direction", 90.0,
				record.getWindDirection());
		assertEquals("UV Null V / Wind: u component", -10.0,
				record.getUComponent(), THRESHOLD);
		assertEquals("UV Null V / Wind: v component", 0.0,
				record.getVComponent(), THRESHOLD);
    }
    
    @Test public void uComponentNullValue() throws ConversionException,
    InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(null, 5.0, 10.0, 90.0);

    	autoQC.qcUComponentValue(record, log, 0);
    	
		assertFalse("U Component Null: log ready", reader.ready());
		assertEquals("U Component Null: U Comp Flag ", 
				MISSING_FLAG, record.getUComponentFlag());
		assertEquals("U Component Null: V Comp Flag ", 
				MISSING_FLAG, record.getVComponentFlag());
		assertNull("U Component Null: wind speed", 
				record.getWindSpeed());
		assertNull("U Component Null: wind direction", 
				record.getWindDirection());
		assertNull("U Component Null: u component", 
				record.getUComponent());
		assertNull("U Component Null: v component", 
				record.getVComponent());
    }
    
    @Test public void uComponentZeroValue() throws ConversionException,
    InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(0.0, 5.0, 10.0, 90.0);

    	autoQC.qcUComponentValue(record, log, 0);
    	
		assertFalse("U Component 0: log ready", reader.ready());
		assertEquals("U Component 0: U Comp Flag ", 
				UNCHECKED_FLAG, record.getUComponentFlag());
		assertEquals("U Component 0: V Comp Flag ", 
				UNCHECKED_FLAG, record.getVComponentFlag());
		assertEquals("U Component 0: wind speed", 10.0,
				record.getWindSpeed());
		assertEquals("U Component 0: wind direction", 90.0,
				record.getWindDirection());
		assertEquals("U Component 0: u component", 0.0,
				record.getUComponent());
		assertEquals("U Component 0: v component", 5.0,
				record.getVComponent());
    }
    
    @Test public void uComponentValidNegativeValue() throws ConversionException,
    InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(-2.0, 5.0, 10.0, 90.0);

    	autoQC.qcUComponentValue(record, log, 0);
    	
		assertFalse("U Component Neg: log ready", reader.ready());
		assertEquals("U Component Neg: U Comp Flag ", 
				UNCHECKED_FLAG, record.getUComponentFlag());
		assertEquals("U Component Neg: V Comp Flag ", 
				UNCHECKED_FLAG, record.getVComponentFlag());
		assertEquals("U Component Neg: wind speed", 10.0,
				record.getWindSpeed());
		assertEquals("U Component Neg: wind direction", 90.0,
				record.getWindDirection());
		assertEquals("U Component Neg: u component", -2.0,
				record.getUComponent());
		assertEquals("U Component Neg: v component", 5.0,
				record.getVComponent());
    }
    
    @Test public void uComponentValidPositiveValue() throws ConversionException,
    InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(5.0, 2.0, 10.0, 90.0);

    	autoQC.qcUComponentValue(record, log, 0);
    	
		assertFalse("U Component Pos: log ready", reader.ready());
		assertEquals("U Component Pos: U Comp Flag ", 
				UNCHECKED_FLAG, record.getUComponentFlag());
		assertEquals("U Component Pos: V Comp Flag ", 
				UNCHECKED_FLAG, record.getVComponentFlag());
		assertEquals("U Component Pos: wind speed", 10.0,
				record.getWindSpeed());
		assertEquals("U Component Pos: wind direction", 90.0,
				record.getWindDirection());
		assertEquals("U Component Pos: u component", 5.0,
				record.getUComponent());
		assertEquals("U Component Pos: v component", 2.0,
				record.getVComponent());
    }
    
    @Test public void uComponentQuestionableNegativeValue() throws
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(-100.1, 5.0, 10.0, 90.0);

    	autoQC.qcUComponentValue(record, log, 0);
    	
		assertTrue("U Component Neg Questionable: log ready pre",
				reader.ready());
		assertFalse("U Component Neg Questionable: log output",
				"".equals(reader.readLine()));
		assertFalse("U Component Neg Questionable: log ready post",
				reader.ready());
		assertEquals("U Component Neg Questionable: U Comp Flag ", 
				QUESTIONABLE_FLAG, record.getUComponentFlag());
		assertEquals("U Component Neg Questionable: V Comp Flag ", 
				UNCHECKED_FLAG, record.getVComponentFlag());
		assertEquals("U Component Neg Questionable: wind speed", 10.0,
				record.getWindSpeed());
		assertEquals("U Component Neg Questionable: wind direction", 90.0,
				record.getWindDirection());
		assertEquals("U Component Neg Questionable: u component", -100.1,
				record.getUComponent());
		assertEquals("U Component Neg Questionable: v component", 5.0,
				record.getVComponent());
    }
    
    @Test public void uComponentQuestionablePositiveValue() throws
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(100.1, 5.0, 10.0, 90.0);

    	autoQC.qcUComponentValue(record, log, 0);
    	
		assertTrue("U Component Pos Questionable: log ready pre",
				reader.ready());
		assertFalse("U Component Pos Questionable: log output",
				"".equals(reader.readLine()));
		assertFalse("U Component Pos Questionable: log ready post",
				reader.ready());
		assertEquals("U Component Pos Questionable: U Comp Flag ", 
				QUESTIONABLE_FLAG, record.getUComponentFlag());
		assertEquals("U Component Pos Questionable: V Comp Flag ", 
				UNCHECKED_FLAG, record.getVComponentFlag());
		assertEquals("U Component Pos Questionable: wind speed", 10.0,
				record.getWindSpeed());
		assertEquals("U Component Pos Questionable: wind direction", 90.0,
				record.getWindDirection());
		assertEquals("U Component Pos Questionable: u component", 100.1,
				record.getUComponent());
		assertEquals("U Component Pos Questionable: v component", 5.0,
				record.getVComponent());
    }
    
    @Test public void uComponentBadNegativeValue() throws
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(-150.1, 5.0, 10.0, 90.0);

    	autoQC.qcUComponentValue(record, log, 0);
    	
		assertTrue("U Component Neg Bad: log ready pre",
				reader.ready());
		assertFalse("U Component Neg Bad: log output",
				"".equals(reader.readLine()));
		assertFalse("U Component Neg Bad: log ready post",
				reader.ready());
		assertEquals("U Component Neg Bad: U Comp Flag ", 
				BAD_FLAG, record.getUComponentFlag());
		assertEquals("U Component Neg Bad: V Comp Flag ", 
				UNCHECKED_FLAG, record.getVComponentFlag());
		assertEquals("U Component Neg Bad: wind speed", 10.0,
				record.getWindSpeed());
		assertEquals("U Component Neg Bad: wind direction", 90.0,
				record.getWindDirection());
		assertEquals("U Component Neg Bad: u component", -150.1,
				record.getUComponent());
		assertEquals("U Component Neg Bad: v component", 5.0,
				record.getVComponent());
    }
    
    @Test public void uComponentBadPositiveValue() throws
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(150.1, 5.0, 10.0, 90.0);

    	autoQC.qcUComponentValue(record, log, 0);
    	
		assertTrue("U Component Pos Bad: log ready pre",
				reader.ready());
		assertFalse("U Component Pos Bad: log output",
				"".equals(reader.readLine()));
		assertFalse("U Component Pos Bad: log ready post",
				reader.ready());
		assertEquals("U Component Pos Bad: U Comp Flag ", 
				BAD_FLAG, record.getUComponentFlag());
		assertEquals("U Component Pos Bad: V Comp Flag ", 
				UNCHECKED_FLAG, record.getVComponentFlag());
		assertEquals("U Component Pos Bad: wind speed", 10.0,
				record.getWindSpeed());
		assertEquals("U Component Pos Bad: wind direction", 90.0,
				record.getWindDirection());
		assertEquals("U Component Pos Bad: u component", 150.1,
				record.getUComponent());
		assertEquals("U Component Pos Bad: v component", 5.0,
				record.getVComponent());
    }
    
    
    @Test public void vComponentNullValue() throws ConversionException,
    InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(5.0, null, 10.0, 90.0);

    	autoQC.qcVComponentValue(record, log, 0);
    	
		assertFalse("V Component Null: log ready", reader.ready());
		assertEquals("V Component Null: U Comp Flag ", 
				MISSING_FLAG, record.getUComponentFlag());
		assertEquals("V Component Null: V Comp Flag ", 
				MISSING_FLAG, record.getVComponentFlag());
		assertNull("V Component Null: wind speed", 
				record.getWindSpeed());
		assertNull("V Component Null: wind direction", 
				record.getWindDirection());
		assertNull("V Component Null: u component", 
				record.getUComponent());
		assertNull("V Component Null: v component", 
				record.getVComponent());
    }
    
    @Test public void vComponentZeroValue() throws ConversionException,
    InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(5.0, 0.0, 10.0, 90.0);

    	autoQC.qcVComponentValue(record, log, 0);
    	
		assertFalse("V Component 0: log ready", reader.ready());
		assertEquals("V Component 0: U Comp Flag ", 
				UNCHECKED_FLAG, record.getUComponentFlag());
		assertEquals("V Component 0: V Comp Flag ", 
				UNCHECKED_FLAG, record.getVComponentFlag());
		assertEquals("V Component 0: wind speed", 10.0,
				record.getWindSpeed());
		assertEquals("V Component 0: wind direction", 90.0,
				record.getWindDirection());
		assertEquals("V Component 0: u component", 5.0,
				record.getUComponent());
		assertEquals("V Component 0: v component", 0.0,
				record.getVComponent());
    }
    
    @Test public void vComponentValidNegativeValue() throws ConversionException,
    InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(5.0, -2.0, 10.0, 90.0);

    	autoQC.qcVComponentValue(record, log, 0);
    	
		assertFalse("V Component Neg: log ready", reader.ready());
		assertEquals("V Component Neg: U Comp Flag ", 
				UNCHECKED_FLAG, record.getUComponentFlag());
		assertEquals("V Component Neg: V Comp Flag ", 
				UNCHECKED_FLAG, record.getVComponentFlag());
		assertEquals("V Component Neg: wind speed", 10.0,
				record.getWindSpeed());
		assertEquals("V Component Neg: wind direction", 90.0,
				record.getWindDirection());
		assertEquals("V Component Neg: u component", 5.0,
				record.getUComponent());
		assertEquals("V Component Neg: v component", -2.0,
				record.getVComponent());
    }
    
    @Test public void vComponentValidPositiveValue() throws ConversionException,
    InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(5.0, 2.0, 10.0, 90.0);

    	autoQC.qcVComponentValue(record, log, 0);
    	
		assertFalse("V Component Pos: log ready", reader.ready());
		assertEquals("V Component Pos: U Comp Flag ", 
				UNCHECKED_FLAG, record.getUComponentFlag());
		assertEquals("V Component Pos: V Comp Flag ", 
				UNCHECKED_FLAG, record.getVComponentFlag());
		assertEquals("V Component Pos: wind speed", 10.0,
				record.getWindSpeed());
		assertEquals("V Component Pos: wind direction", 90.0,
				record.getWindDirection());
		assertEquals("V Component Pos: u component", 5.0,
				record.getUComponent());
		assertEquals("V Component Pos: v component", 2.0,
				record.getVComponent());
    }
    
    @Test public void vComponentQuestionableNegativeValue() throws
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(5.0, -100.1, 10.0, 90.0);

    	autoQC.qcVComponentValue(record, log, 0);
    	
		assertTrue("V Component Neg Questionable: log ready pre",
				reader.ready());
		assertFalse("V Component Neg Questionable: log output",
				"".equals(reader.readLine()));
		assertFalse("V Component Neg Questionable: log ready post",
				reader.ready());
		assertEquals("V Component Neg Questionable: U Comp Flag ", 
				UNCHECKED_FLAG, record.getUComponentFlag());
		assertEquals("V Component Neg Questionable: V Comp Flag ", 
				QUESTIONABLE_FLAG, record.getVComponentFlag());
		assertEquals("V Component Neg Questionable: wind speed", 10.0,
				record.getWindSpeed());
		assertEquals("V Component Neg Questionable: wind direction", 90.0,
				record.getWindDirection());
		assertEquals("V Component Neg Questionable: u component", 5.0,
				record.getUComponent());
		assertEquals("V Component Neg Questionable: v component", -100.1,
				record.getVComponent());
    }
    
    @Test public void vComponentQuestionablePositiveValue() throws
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(5.0, 100.1, 10.0, 90.0);

    	autoQC.qcVComponentValue(record, log, 0);
    	
		assertTrue("V Component Pos Questionable: log ready pre",
				reader.ready());
		assertFalse("V Component Pos Questionable: log output",
				"".equals(reader.readLine()));
		assertFalse("V Component Pos Questionable: log ready post",
				reader.ready());
		assertEquals("V Component Pos Questionable: U Comp Flag ", 
				UNCHECKED_FLAG, record.getUComponentFlag());
		assertEquals("V Component Pos Questionable: V Comp Flag ", 
				QUESTIONABLE_FLAG, record.getVComponentFlag());
		assertEquals("V Component Pos Questionable: wind speed", 10.0,
				record.getWindSpeed());
		assertEquals("V Component Pos Questionable: wind direction", 90.0,
				record.getWindDirection());
		assertEquals("V Component Pos Questionable: u component", 5.0,
				record.getUComponent());
		assertEquals("V Component Pos Questionable: v component", 100.1,
				record.getVComponent());
    }
    
    @Test public void vComponentBadNegativeValue() throws
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(5.0, -150.1, 10.0, 90.0);

    	autoQC.qcVComponentValue(record, log, 0);
    	
		assertTrue("V Component Neg Bad: log ready pre",
				reader.ready());
		assertFalse("V Component Neg Bad: log output",
				"".equals(reader.readLine()));
		assertFalse("V Component Neg Bad: log ready post",
				reader.ready());
		assertEquals("V Component Neg Bad: U Comp Flag ", 
				UNCHECKED_FLAG, record.getUComponentFlag());
		assertEquals("V Component Neg Bad: V Comp Flag ", 
				BAD_FLAG, record.getVComponentFlag());
		assertEquals("V Component Neg Bad: wind speed", 10.0,
				record.getWindSpeed());
		assertEquals("V Component Neg Bad: wind direction", 90.0,
				record.getWindDirection());
		assertEquals("V Component Neg Bad: u component", 5.0,
				record.getUComponent());
		assertEquals("V Component Neg Bad: v component", -150.1,
				record.getVComponent());
    }
    
    @Test public void vComponentBadPositiveValue() throws
    ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(5.0, 150.1, 10.0, 90.0);

    	autoQC.qcVComponentValue(record, log, 0);
    	
		assertTrue("V Component Pos Bad: log ready pre",
				reader.ready());
		assertFalse("V Component Pos Bad: log output",
				"".equals(reader.readLine()));
		assertFalse("V Component Pos Bad: log ready post",
				reader.ready());
		assertEquals("V Component Pos Bad: U Comp Flag ", 
				UNCHECKED_FLAG, record.getUComponentFlag());
		assertEquals("V Component Pos Bad: V Comp Flag ", 
				BAD_FLAG, record.getVComponentFlag());
		assertEquals("V Component Pos Bad: wind speed", 10.0,
				record.getWindSpeed());
		assertEquals("V Component Pos Bad: wind direction", 90.0,
				record.getWindDirection());
		assertEquals("V Component Pos Bad: u component", 5.0,
				record.getUComponent());
		assertEquals("V Component Pos Bad: v component", 150.1,
				record.getVComponent());
    }
    
    
    @Test public void windDirectionNullValue() throws 
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(5.0, 5.0, 10.0, null);

    	autoQC.qcWindDirectionValue(record, log, 0);
    	
		assertFalse("Wind Direction Null: log ready", reader.ready());
		assertEquals("Wind Direction Null: U Comp Flag ", 
				MISSING_FLAG, record.getUComponentFlag());
		assertEquals("Wind Direction Null: V Comp Flag ", 
				MISSING_FLAG, record.getVComponentFlag());
		assertNull("Wind Direction Null: wind speed", 
				record.getWindSpeed());
		assertNull("Wind Direction Null: wind direction", 
				record.getWindDirection());
		assertNull("Wind Direction Null: wind speed", 
				record.getUComponent());
		assertNull("Wind Direction Null: wind speed", 
				record.getVComponent());
    }
    
    @Test public void windDirectionNegativeValue() throws 
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(5.0, 5.0, 10.0, -0.1);

    	autoQC.qcWindDirectionValue(record, log, 0);
    	
    	assertTrue("Wind Direction Negative: log ready pre", reader.ready());
    	assertFalse("Wind Direction Negative: log output",
    			"".equals(reader.readLine()));
		assertFalse("Wind Direction Negative: log ready post", reader.ready());
		assertEquals("Wind Direction Negative: U Comp Flag ", 
				MISSING_FLAG, record.getUComponentFlag());
		assertEquals("Wind Direction Negative: V Comp Flag ", 
				MISSING_FLAG, record.getVComponentFlag());
		assertNull("Wind Direction Negative: wind speed", 
				record.getWindSpeed());
		assertNull("Wind Direction Negative: wind direction", 
				record.getWindDirection());
		assertNull("Wind Direction Negative: wind speed", 
				record.getUComponent());
		assertNull("Wind Direction Negative: wind speed", 
				record.getVComponent());
    }
    
    @Test public void windDirectionUpperBoundary() throws 
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(5.0, 5.0, 10.0, 360.0);

    	autoQC.qcWindDirectionValue(record, log, 0);
    	
		assertFalse("Wind Direction 360: log ready", reader.ready());
		assertEquals("Wind Direction 360: U Comp Flag ", 
				UNCHECKED_FLAG, record.getUComponentFlag());
		assertEquals("Wind Direction 360: V Comp Flag ", 
				UNCHECKED_FLAG, record.getVComponentFlag());
		assertEquals("Wind Direction 360: wind speed", 10.0, 
				record.getWindSpeed());
		assertEquals("Wind Direction 360: wind direction", 360.0, 
				record.getWindDirection());
		assertEquals("Wind Direction 360: u component", 5.0,
				record.getUComponent());
		assertEquals("Wind Direction 360: v component", 5.0,
				record.getVComponent());
    }
    
    @Test public void windDirectionUpperBoundaryViolation() throws 
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(5.0, 5.0, 10.0, 360.1);

    	autoQC.qcWindDirectionValue(record, log, 0);
    	
    	assertTrue("Wind Direction Upper Violation: log ready pre",
    			reader.ready());
    	assertFalse("Wind Direction Upper Violation: log output",
    			"".equals(reader.readLine()));
		assertFalse("Wind Direction Upper Violation: log ready post",
				reader.ready());
		assertEquals("Wind Direction Upper Violation: U Comp Flag ", 
				MISSING_FLAG, record.getUComponentFlag());
		assertEquals("Wind Direction Upper Violation: V Comp Flag ", 
				MISSING_FLAG, record.getVComponentFlag());
		assertNull("Wind Direction Upper Violation: wind speed", 
				record.getWindSpeed());
		assertNull("Wind Direction Upper Violation: wind direction", 
				record.getWindDirection());
		assertNull("Wind Direction Upper Violation: u component", 
				record.getUComponent());
		assertNull("Wind Direction Upper Violation: v component", 
				record.getVComponent());
    }
	
    
    
    @Test public void windFieldCheckNullSpeedWithDirection() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(null, null, null, 100.0);

    	autoQC.qcWindFields(record, log);
    	
		assertTrue("Wind Speed Null with Direction: log ready pre",
				reader.ready());
		assertFalse("Wind Speed Null with Direction: log output",
				"".equals(reader.readLine()));
		assertFalse("Wind Speed Null with Direction: log ready post",
				reader.ready());
		assertEquals("Wind Speed Null with Direction: U Comp Flag ", 
				MISSING_FLAG, record.getUComponentFlag());
		assertEquals("Wind Speed Null with Direction: V Comp Flag ", 
				MISSING_FLAG, record.getVComponentFlag());
		assertNull("Wind Speed Null with Direction: wind speed", 
				record.getWindSpeed());
		assertNull("Wind Speed Null with Direction: wind direction", 
				record.getWindDirection());
		assertNull("Wind Speed Null with Direction: wind speed", 
				record.getUComponent());
		assertNull("Wind Speed Null with Direction: wind speed", 
				record.getVComponent());
    }
    
    @Test public void windFieldCheckNullSpeedWithUComponent() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(10.0, null, null, 90.0);

    	autoQC.qcWindFields(record, log);
    	
		assertTrue("Wind Speed Null with U Comp: log ready pre",
				reader.ready());
		assertFalse("Wind Speed Null with U Comp: log output",
				"".equals(reader.readLine()));
		assertFalse("Wind Speed Null with U Comp: log ready post",
				reader.ready());
		assertEquals("Wind Speed Null with U Comp: U Comp Flag ", 
				MISSING_FLAG, record.getUComponentFlag());
		assertEquals("Wind Speed Null with U Comp: V Comp Flag ", 
				MISSING_FLAG, record.getVComponentFlag());
		assertNull("Wind Speed Null with U Comp: wind speed", 
				record.getWindSpeed());
		assertNull("Wind Speed Null with U Comp: wind direction", 
				record.getWindDirection());
		assertNull("Wind Speed Null with U Comp: wind speed", 
				record.getUComponent());
		assertNull("Wind Speed Null with U Comp: wind speed", 
				record.getVComponent());
    }
    
    @Test public void windFieldCheckNullSpeedWithVComponent() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(null, 10.0, null, 90.0);

    	autoQC.qcWindFields(record, log);
    	
		assertTrue("Wind Speed Null with V Comp: log ready pre",
				reader.ready());
		assertFalse("Wind Speed Null with V Comp: log output",
				"".equals(reader.readLine()));
		assertFalse("Wind Speed Null with V Comp: log ready post",
				reader.ready());
		assertEquals("Wind Speed Null with V Comp: U Comp Flag ", 
				MISSING_FLAG, record.getUComponentFlag());
		assertEquals("Wind Speed Null with V Comp: V Comp Flag ", 
				MISSING_FLAG, record.getVComponentFlag());
		assertNull("Wind Speed Null with V Comp: wind speed", 
				record.getWindSpeed());
		assertNull("Wind Speed Null with V Comp: wind direction", 
				record.getWindDirection());
		assertNull("Wind Speed Null with V Comp: wind speed", 
				record.getUComponent());
		assertNull("Wind Speed Null with V Comp: wind speed", 
				record.getVComponent());
    }
    
    @Test public void windFieldCheckNullDirectionWithSpeed() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(null, null, 10.0, null);

    	autoQC.qcWindFields(record, log);
    	
		assertTrue("Wind Direction Null with Speed: log ready pre",
				reader.ready());
		assertFalse("Wind Direction Null with Speed: log output",
				"".equals(reader.readLine()));
		assertFalse("Wind Direction Null with Speed: log ready post",
				reader.ready());
		assertEquals("Wind Direction Null with Speed: U Comp Flag ", 
				MISSING_FLAG, record.getUComponentFlag());
		assertEquals("Wind Direction Null with Speed: V Comp Flag ", 
				MISSING_FLAG, record.getVComponentFlag());
		assertNull("Wind Direction Null with Speed: wind speed", 
				record.getWindSpeed());
		assertNull("Wind Direction Null with Speed: wind direction", 
				record.getWindDirection());
		assertNull("Wind Direction Null with Speed: wind speed", 
				record.getUComponent());
		assertNull("Wind Direction Null with Speed: wind speed", 
				record.getVComponent());
    }
    
    @Test public void windFieldCheckNullDirectionWithUComponent() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(10.0, null, 10.0, null);

    	autoQC.qcWindFields(record, log);
    	
		assertTrue("Wind Direction Null with U Comp: log ready pre",
				reader.ready());
		assertFalse("Wind Direction Null with U Comp: log output",
				"".equals(reader.readLine()));
		assertFalse("Wind Direction Null with U Comp: log ready post",
				reader.ready());
		assertEquals("Wind Direction Null with U Comp: U Comp Flag ", 
				MISSING_FLAG, record.getUComponentFlag());
		assertEquals("Wind Direction Null with U Comp: V Comp Flag ", 
				MISSING_FLAG, record.getVComponentFlag());
		assertNull("Wind Direction Null with U Comp: wind speed", 
				record.getWindSpeed());
		assertNull("Wind Direction Null with U Comp: wind direction", 
				record.getWindDirection());
		assertNull("Wind Direction Null with U Comp: wind speed", 
				record.getUComponent());
		assertNull("Wind Direction Null with U Comp: wind speed", 
				record.getVComponent());
    }
    
    @Test public void windFieldCheckNullDirectionWithVComponent() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	ESCSoundingRecord record = buildWindRecord(null, 10.0, 10.0, null);

    	autoQC.qcWindFields(record, log);
    	
		assertTrue("Wind Direction Null with V Comp: log ready pre",
				reader.ready());
		assertFalse("Wind Direction Null with V Comp: log output",
				"".equals(reader.readLine()));
		assertFalse("Wind Direction Null with V Comp: log ready post",
				reader.ready());
		assertEquals("Wind Direction Null with V Comp: U Comp Flag ", 
				MISSING_FLAG, record.getUComponentFlag());
		assertEquals("Wind Direction Null with V Comp: V Comp Flag ", 
				MISSING_FLAG, record.getVComponentFlag());
		assertNull("Wind Direction Null with V Comp: wind speed", 
				record.getWindSpeed());
		assertNull("Wind Direction Null with V Comp: wind direction", 
				record.getWindDirection());
		assertNull("Wind Direction Null with V Comp: wind speed", 
				record.getUComponent());
		assertNull("Wind Direction Null with V Comp: wind speed", 
				record.getVComponent());
    }
    
    
    /**
     * Test the wind speed check with null wind speed value.
     */
    @Test public void windSpeedNullValue()throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	// Build the records used for the check
    	records.add(buildWindRecord(10.0, 10.0, null, 150.0));

    	// Run the check
    	autoQC.qcWindSpeedValue(records.get(0), log, 0);

		// Test the output
		assertFalse("Wind Speed Null: log ready", reader.ready());
		assertEquals("Wind Speed Null: U Comp Flag ", 
				MISSING_FLAG, records.get(0).getUComponentFlag());
		assertEquals("Wind Speed Null: V Comp Flag ", 
				MISSING_FLAG, records.get(0).getVComponentFlag());
		assertNull("Wind Speed Null: wind speed", 
				records.get(0).getWindSpeed());
		assertNull("Wind Speed Null: wind direction", 
				records.get(0).getWindDirection());
		assertNull("Wind Speed Null: wind speed", 
				records.get(0).getUComponent());
		assertNull("Wind Speed Null: wind speed", 
				records.get(0).getVComponent());
    }
    
    /**
     * Test the wind speed check with a valid value.
     */
    @Test public void windSpeedValidValue() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	// Build the records used for the check
    	records.add(buildWindRecord(null, null, 10.0, 150.0));

    	// Run the check
    	autoQC.qcWindSpeedValue(records.get(0), log, 0);

		// Test the output
		assertFalse("Wind Speed Valid: log ready", reader.ready());
		assertEquals("Wind Speed Valid: U Comp Flag ", 
				UNCHECKED_FLAG, records.get(0).getUComponentFlag());
		assertEquals("Wind Speed Valid: V Comp Flag ", 
				UNCHECKED_FLAG, records.get(0).getVComponentFlag());
		assertEquals("Wind Speed Valid: wind speed", 10.0,
				records.get(0).getWindSpeed());
		assertEquals("Wind Speed Valid: wind direction", 150.0,
				records.get(0).getWindDirection());
		assertEquals("Wind Speed Valid: u component", -5.0,
				records.get(0).getUComponent(), THRESHOLD);
		assertEquals("Wind Speed Valid: v component", 8.6602540378444,
				records.get(0).getVComponent(), THRESHOLD);
    }
    
    /**
     * Test the wind speed check with a maximum missing value exceeded.
     */
    @Test public void windSpeedMaximumMissingValue() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	// Build the records used for the check
    	records.add(buildWindRecord(null, null, 200.1, 150.0));

    	// Run the check
    	autoQC.qcWindSpeedValue(records.get(0), log, 0);

		// Test the output
    	assertTrue("Wind Speed Max Missing: log ready pre", reader.ready());
    	assertFalse("Wind Speed Max Missing: log output", 
    			"".equals(reader.readLine()));
    	assertFalse("Wind Speed Max Missing: log ready post", reader.ready());
		assertEquals("Wind Speed Max Missing: U Comp Flag ", 
				MISSING_FLAG, records.get(0).getUComponentFlag());
		assertEquals("Wind Speed Max Missing: V Comp Flag ", 
				MISSING_FLAG, records.get(0).getVComponentFlag());
		
		assertNull("Wind Speed Max Missing: wind speed",
				records.get(0).getWindSpeed());
		assertNull("Wind Speed Max Missing: wind direction",
				records.get(0).getWindDirection());
		assertNull("Wind Speed Max Missing: u component",
				records.get(0).getUComponent());
		assertNull("Wind Speed Max Missing: v component",
				records.get(0).getVComponent());
    }
    
    /**
     * Test the wind speed check with a negative wind speed.
     */
    @Test public void windSpeedNegativeValue() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	// Build the records used for the check
    	records.add(buildWindRecord(5.0, 5.0, -0.1, 150.0));

    	// Run the check
    	autoQC.qcWindSpeedValue(records.get(0), log, 0);

		// Test the output
    	assertTrue("Wind Speed Negative: log ready pre", reader.ready());
    	assertFalse("Wind Speed Negative: log output", 
    			"".equals(reader.readLine()));
    	assertFalse("Wind Speed Negative: log ready post", reader.ready());
		assertEquals("Wind Speed Negative: U Comp Flag ", 
				MISSING_FLAG, records.get(0).getUComponentFlag());
		assertEquals("Wind Speed Negative: V Comp Flag ", 
				MISSING_FLAG, records.get(0).getVComponentFlag());
		
		assertNull("Wind Speed Negative: wind speed",
				records.get(0).getWindSpeed());
		assertNull("Wind Speed Negative: wind direction",
				records.get(0).getWindDirection());
		assertNull("Wind Speed Negative: u component",
				records.get(0).getUComponent());
		assertNull("Wind Speed Negative: v component",
				records.get(0).getVComponent());
    }
    
    /**
     * Test the wind speed check with a questionable wind speed.
     */
    @Test public void windSpeedQuestionableValue() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	// Build the records used for the check
    	records.add(buildWindRecord(null, null, 100.1, 150.0));

    	// Run the check
    	autoQC.qcWindSpeedValue(records.get(0), log, 0);

		// Test the output
    	assertTrue("Wind Speed Questionable: log ready pre", reader.ready());
    	assertFalse("Wind Speed Questionable: log output", 
    			"".equals(reader.readLine()));
    	assertFalse("Wind Speed Questionable: log ready post", reader.ready());
		assertEquals("Wind Speed Questionable: U Comp Flag ", 
				QUESTIONABLE_FLAG, records.get(0).getUComponentFlag());
		assertEquals("Wind Speed Questionable: V Comp Flag ", 
				QUESTIONABLE_FLAG, records.get(0).getVComponentFlag());
		
		assertEquals("Wind Speed Questionable: wind speed", 100.1,
				records.get(0).getWindSpeed());
		assertEquals("Wind Speed Questionable: wind direction", 150.0,
				records.get(0).getWindDirection());
		assertEquals("Wind Speed Questionable: u component", -50.05,
				records.get(0).getUComponent(), THRESHOLD);
		assertEquals("Wind Speed Questionable: v component", 86.68914291882,
				records.get(0).getVComponent(), THRESHOLD);
    }
    
    /**
     * Test the wind speed check with a bad wind speed.
     */
    @Test public void windSpeedBadValue() throws CalculationWarning,
    ConversionException, InvalidValueWarning, IOException {
    	// Build the records used for the check
    	records.add(buildWindRecord(null, null, 150.1, 150.0));

    	// Run the check
    	autoQC.qcWindSpeedValue(records.get(0), log, 0);

		// Test the output
    	assertTrue("Wind Speed Bad: log ready pre", reader.ready());
    	assertFalse("Wind Speed Bad: log output", 
    			"".equals(reader.readLine()));
    	assertFalse("Wind Speed Bad: log ready post", reader.ready());
		assertEquals("Wind Speed Bad: U Comp Flag ", 
				BAD_FLAG, records.get(0).getUComponentFlag());
		assertEquals("Wind Speed Bad: V Comp Flag ", 
				BAD_FLAG, records.get(0).getVComponentFlag());
		
		assertEquals("Wind Speed Questionable: wind speed", 150.1,
				records.get(0).getWindSpeed());
		assertEquals("Wind Speed Questionable: wind direction", 150.0,
				records.get(0).getWindDirection());
		assertEquals("Wind Speed Questionable: u component", -75.05,
				records.get(0).getUComponent(), THRESHOLD);
		assertEquals("Wind Speed Questionable: v component", 129.99041310804,
				records.get(0).getVComponent(), THRESHOLD);
    }
    
    
    /**
     * Test the rapid pressure increase check when the start pressure is null.
     */
    @Test (expected = NullPointerException.class)
    public void rapidPressureCheckNoStartPressure() throws
    CalculationWarning, ConversionException, InvalidValueWarning {
    	// Build the records used for the check
    	records.add(buildPTURecord(null, 24.0, 50.0));
    	records.add(buildPTURecord(250.0, 22.0, 51.0));
    	records.add(buildPTURecord(250.0, 20.0, 52.0));

    	records.get(0).setTime(900.0);
    	records.get(2).setTime(901.0);

    	// Run the check
    	autoQC.rapidPressureCheck(records, log);
    }
    
    /**
     * Test the rapid pressure increase check when the end pressure is null.
     */
    @Test (expected = NullPointerException.class)
    public void rapidPressureCheckNoEndPressure()throws
    CalculationWarning, ConversionException, InvalidValueWarning {
    	// Build the records used for the check
    	records.add(buildPTURecord(250.0, 24.0, 50.0));
    	records.add(buildPTURecord(250.0, 22.0, 51.0));
    	records.add(buildPTURecord(null, 20.0, 52.0));

    	records.get(0).setTime(900.0);
    	records.get(2).setTime(901.0);

    	// Run the check
    	autoQC.rapidPressureCheck(records, log);
    }
    
    /**
     * Test the rapid pressure increase check when the start time is null.
     */
    @Test (expected = NullPointerException.class)
    public void rapidPressureCheckNoStartTime()throws
    CalculationWarning, ConversionException, InvalidValueWarning {
    	// Build the records used for the check
    	records.add(buildPTURecord(250.1, 24.0, 50.0));
    	records.add(buildPTURecord(250.0, 22.0, 51.0));
    	records.add(buildPTURecord(250.0, 20.0, 52.0));

    	records.get(0).setTime(null);
    	records.get(2).setTime(901.0);

    	// Run the check
    	autoQC.rapidPressureCheck(records, log);
    }
    
    /**
     * Test the rapid pressure increase check when the end time is null.
     */
    @Test (expected = NullPointerException.class)
    public void rapidPressureCheckNoEndTime()throws
    CalculationWarning, ConversionException, InvalidValueWarning {
    	// Build the records used for the check
    	records.add(buildPTURecord(255.0, 24.0, 50.0));
    	records.add(buildPTURecord(250.0, 22.0, 51.0));
    	records.add(buildPTURecord(250.0, 20.0, 52.0));

    	records.get(0).setTime(900.0);
    	records.get(2).setTime(null);

    	// Run the check
    	autoQC.rapidPressureCheck(records, log);
    }
    
    /**
     * Test the rapid pressure increase check when the times are equal.
     */
    @Test public void rapidPressureCheckEqualTimesPositiveInfinity() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	// Build the records used for the check
    	records.add(buildPTURecord(255.0, 24.0, 50.0));
    	records.add(buildPTURecord(252.5, 22.0, 51.0));
    	records.add(buildPTURecord(250.0, 20.0, 52.0));

    	records.get(0).setTime(900.0);
    	records.get(2).setTime(900.0);

    	// Run the check
    	autoQC.rapidPressureCheck(records, log);
    	
		// Test the output
		assertFalse("Rapid Press Equal Times Pos: log ready", reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Press Equal Times Pos: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Press Equal Times Pos: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Press Equal Times Pos: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid pressure increase check when the times are equal.
     */
    @Test public void rapidPressureCheckEqualTimesNegativeInfinity() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	// Build the records used for the check
    	records.add(buildPTURecord(245.0, 24.0, 50.0));
    	records.add(buildPTURecord(252.5, 22.0, 51.0));
    	records.add(buildPTURecord(250.0, 20.0, 52.0));

    	records.get(0).setTime(900.0);
    	records.get(2).setTime(900.0);

    	// Run the check
    	autoQC.rapidPressureCheck(records, log);
    	
		// Test the output
		assertFalse("Rapid Press Equal Times Neg: log ready", reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Press Equal Times Neg: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Press Equal Times Neg: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Press Equal Times Neg: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid pressure increase check when the pressures are equal.
     */
    @Test public void rapidPressureCheckEqualPressures() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	// Build the records used for the check
    	records.add(buildPTURecord(250.0, 24.0, 50.0));
    	records.add(buildPTURecord(252.5, 22.0, 51.0));
    	records.add(buildPTURecord(250.0, 20.0, 52.0));

    	records.get(0).setTime(899.0);
    	records.get(2).setTime(900.0);

    	// Run the check
    	autoQC.rapidPressureCheck(records, log);
    	
		// Test the output
		assertFalse("Rapid Press Equal Pressures: log ready", reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Press Equal Pressures: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Press Equal Pressures: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Press Equal Pressures: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid pressure increase check for an acceptable positive value.
     */
    @Test public void rapidPressureCheckAcceptablePositive() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	// Build the records used for the check
    	records.add(buildPTURecord(749.0, 24.0, 50.0));
    	records.add(buildPTURecord(749.5, 22.0, 51.0));
    	records.add(buildPTURecord(750.0, 20.0, 52.0));

    	records.get(0).setTime(899.0);
    	records.get(2).setTime(900.0);

    	// Run the check
    	autoQC.rapidPressureCheck(records, log);
    	
		// Test the output
		assertFalse("Rapid Press Acceptable Pos: log ready", reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Press Acceptable Pos: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Press Acceptable Pos: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Press Acceptable Pos: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid pressure increase check for an acceptable negative value.
     */
    @Test public void rapidPressureCheckAcceptableNegative() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	// Build the records used for the check
    	records.add(buildPTURecord(749.0, 24.0, 50.0));
    	records.add(buildPTURecord(749.5, 22.0, 51.0));
    	records.add(buildPTURecord(750.0, 20.0, 52.0));

    	records.get(0).setTime(899.0);
    	records.get(2).setTime(898.0);

    	// Run the check
    	autoQC.rapidPressureCheck(records, log);
    	
		// Test the output
		assertFalse("Rapid Press Acceptable Neg: log ready", reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Press Acceptable Neg: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Press Acceptable Neg: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Press Acceptable Neg: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid pressure increase check for a questionable positive value.
     */
    @Test public void rapidPressureCheckQuestionablePositive() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	// Build the records used for the check
    	records.add(buildPTURecord(749.0, 24.0, 50.0));
    	records.add(buildPTURecord(749.5, 22.0, 51.0));
    	records.add(buildPTURecord(750.5, 20.0, 52.0));

    	records.get(0).setTime(899.0);
    	records.get(2).setTime(900.0);

    	// Run the check
    	autoQC.rapidPressureCheck(records, log);
    	
		// Test the output
		assertTrue("Rapid Press Questionable Pos: log ready pre",
				reader.ready());
    	assertFalse("Rapid Press Questionable Pos: log value", 
    			"".equals(reader.readLine()));
		assertFalse("Rapid Press Questionable Pos: log ready post", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Press Questionable Pos: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Press Questionable Pos: Temp Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Press Questionable Pos: RH Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid pressure check for a questionable negative value.
     */
    @Test public void rapidPressureCheckQuestionableNegative() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	// Build the records used for the check
    	records.add(buildPTURecord(749.0, 24.0, 50.0));
    	records.add(buildPTURecord(749.5, 22.0, 51.0));
    	records.add(buildPTURecord(750.5, 20.0, 52.0));

    	records.get(0).setTime(899.0);
    	records.get(2).setTime(898.0);

    	// Run the check
    	autoQC.rapidPressureCheck(records, log);
    	
		// Test the output
		assertTrue("Rapid Press Questionable Neg: log ready pre",
				reader.ready());
    	assertFalse("Rapid Press Questionable Neg: log value", 
    			"".equals(reader.readLine()));
		assertFalse("Rapid Press Questionable Neg: log ready post", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Press Questionable Neg: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Press Questionable Neg: Temp Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Press Questionable Neg: RH Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid pressure check for a bad positive change.
     */
    @Test public void rapidPressureCheckBadPositive() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	// Build the records used for the check
    	records.add(buildPTURecord(749.0, 24.0, 50.0));
    	records.add(buildPTURecord(749.5, 22.0, 51.0));
    	records.add(buildPTURecord(751.0, 20.0, 52.0));

    	records.get(0).setTime(899.0);
    	records.get(2).setTime(900.0);

    	// Run the check
    	autoQC.rapidPressureCheck(records, log);
    	
		// Test the output
		assertTrue("Rapid Press Bad Pos: log ready pre",
				reader.ready());
    	assertFalse("Rapid Press Bad Pos: log value", 
    			"".equals(reader.readLine()));
		assertFalse("Rapid Press Bad Pos: log ready post", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Press Bad Pos: Press Flag "+i, 
					BAD_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Press Bad Pos: Temp Flag "+i, 
					BAD_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Press Bad Pos: RH Flag "+i, 
					BAD_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid pressure check for a bad negative change.
     */
    @Test public void rapidPressureCheckBadNegative() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
    	// Build the records used for the check
    	records.add(buildPTURecord(749.0, 24.0, 50.0));
    	records.add(buildPTURecord(749.5, 22.0, 51.0));
    	records.add(buildPTURecord(751.0, 20.0, 52.0));

    	records.get(0).setTime(899.0);
    	records.get(2).setTime(898.0);

    	// Run the check
    	autoQC.rapidPressureCheck(records, log);
    	
		// Test the output
		assertTrue("Rapid Press Bad Neg: log ready pre",
				reader.ready());
    	assertFalse("Rapid Press Bad Neg: log value", 
    			"".equals(reader.readLine()));
		assertFalse("Rapid Press Bad Neg: log ready post", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Press Bad Neg: Press Flag "+i, 
					BAD_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Press Bad Neg: Temp Flag "+i, 
					BAD_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Press Bad Neg: RH Flag "+i, 
					BAD_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    
    
    /**
     * Test the rapid temperature increase check in hot latitudes when the
     * start pressure is null.
     */
    @Test (expected = NullPointerException.class) 
    public void rapidTemperatureHotCheckNoStartPressure() throws
    CalculationWarning, ConversionException, InvalidValueWarning {
		// Build the records used for the check
		records.add(buildPTURecord(null, 25.0, 50.0));
		records.add(buildPTURecord(250.0, 22.0, 51.0));
		records.add(buildPTURecord(250.0, 20.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheckForHotLatitudes(records, log);
    }
    
    /**
     * Test the rapid temperature increase check in hot latitudes when the
     * end pressure is null.
     */
    @Test (expected = NullPointerException.class) 
    public void rapidTemperatureHotCheckNoEndPressure() throws
    CalculationWarning, ConversionException, InvalidValueWarning {
		// Build the records used for the check
		records.add(buildPTURecord(250.0, 25.0, 50.0));
		records.add(buildPTURecord(250.0, 22.0, 51.0));
		records.add(buildPTURecord(null, 20.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheckForHotLatitudes(records, log);
    }
    
    /**
     * Test the rapid temperature increase check in hot latitudes when the
     * pressure is in the general pressure range and there is no start
     * temperature.
     */
    @Test (expected = NullPointerException.class)
    public void rapidTemperatureHotGenPressRangeCheckNoStartTemp() throws
    CalculationWarning, ConversionException, InvalidValueWarning {
		// Build the records used for the check
		records.add(buildPTURecord(250.0, null, 50.0));
		records.add(buildPTURecord(250.0, 22.0, 51.0));
		records.add(buildPTURecord(250.0, 20.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheckForHotLatitudes(records, log);
    }
    
    /**
     * Test the rapid temperature increase check in hot latitudes when the
     * pressure is in the general pressure range and there is no end 
     * temperature.
     */
    @Test (expected = NullPointerException.class)
    public void rapidTemperatureHotGenPressRangeCheckNoEndTemp() throws
    CalculationWarning, ConversionException, InvalidValueWarning {
		// Build the records used for the check
		records.add(buildPTURecord(250.0, 24.0, 50.0));
		records.add(buildPTURecord(250.0, 22.0, 51.0));
		records.add(buildPTURecord(250.0, null, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheckForHotLatitudes(records, log);
    }
    
    /**
     * Test the rapid temperature incrase check in hot latitudes when the
     * pressure is in the general pressure range and there is no start
     * altitude.
     */
    @Test (expected = NullPointerException.class)
    public void rapidTemperatureHotGenPressRangeCheckNoStartAlt() throws
    CalculationWarning, ConversionException, InvalidValueWarning {
		// Build the records used for the check
		records.add(buildPTURecord(250.0, 24.0, 50.0));
		records.add(buildPTURecord(250.0, 22.0, 51.0));
		records.add(buildPTURecord(250.0, 20.0, 52.0));
		
		records.get(0).setAltitude(null, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheckForHotLatitudes(records, log);
    }
    
    /**
     * Test the rapid temperature increase check in hot latitudes when the
     * pressure is in the general pressure range and there is no end
     * altitude.
     */
    @Test (expected = NullPointerException.class)
    public void rapidTemperatureHotGenPressRangeCheckNoEndAlt() throws
    CalculationWarning, ConversionException, InvalidValueWarning {
		// Build the records used for the check
		records.add(buildPTURecord(250.0, 24.0, 50.0));
		records.add(buildPTURecord(250.0, 22.0, 51.0));
		records.add(buildPTURecord(250.0, null, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheckForHotLatitudes(records, log);
    }
    
    /**
     * Test the rapid temperature increase check in hot latitudes when the
     * pressure is in the general pressure range and the altitudes are equal.
     */
    @Test public void rapidTemperatureHotGenPressRangeCheckEqualAlt() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check
		records.add(buildPTURecord(250.0, 24.0, 50.0));
		records.add(buildPTURecord(250.0, 22.0, 51.0));
		records.add(buildPTURecord(250.0, 20.0, 52.0));
		
		records.get(0).setAltitude(1000.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheckForHotLatitudes(records, log);
		
		// Test the output
		assertFalse("Rapid Temp Hot Gen Press Equal Alt: log ready", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Hot Gen Press Equal Alt: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Hot Gen Press Equal Alt: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Hot Gen Press Equal Alt: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid temperature increase check in hot latitudes when the
     * pressure is in the general pressure range the lapse rate is acceptable.
     */
    @Test
    public void rapidTemperatureHotGenPressRangeCheckAcceptableLapseRate() 
    throws CalculationWarning, ConversionException, InvalidValueWarning, 
    IOException {
		// Build the records used for the check
		records.add(buildPTURecord(250.0, 24.0, 50.0));
		records.add(buildPTURecord(250.0, 22.0, 51.0));
		records.add(buildPTURecord(250.0, 20.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheckForHotLatitudes(records, log);

		// Check the output and results.
		assertFalse("Rapid Temp Inc Hot Gen Press OK Lapse Rate: log ready",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Inc Hot Gen Press OK Lapse Rate: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Inc Hot Gen Press OK Lapse Rate: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Inc Hot Gen Press OK Lapse Rate: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid temperature increase check in hot latitudes when the
     * pressure is in the general pressure range and the laspe rate is
     * questionable.
     */
    @Test
    public void rapidTemperatureHotGenPressRangeCheckQuestionableLapseRate() 
    throws CalculationWarning, ConversionException, InvalidValueWarning, 
    IOException {
		// Build the records used for the check
		records.add(buildPTURecord(250.0, 20.0, 50.0));
		records.add(buildPTURecord(250.0, 22.0, 51.0));
		records.add(buildPTURecord(250.0, 25.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheckForHotLatitudes(records, log);

		// Check the output and results.
		assertTrue("Rapid Temp Inc Hot Gen Press Questionable Lapse Rate: log ready pre", 
				reader.ready());
		assertFalse("Rapid Temp Inc Hot Gen Press Questionable Lapse Rate: log value", 
				"".equals(reader.readLine()));
		assertFalse("Rapid Temp Inc Hot Gen Press Questionable Lapse Rate: log ready post", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Inc Hot Gen Press Questionable Lapse Rate: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Inc Hot Gen Press Questionable Lapse Rate: Temp Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Inc Hot Gen Press Questionable Lapse Rate: RH Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid temperature increase check in hot latitudes when the
     * pressure is in the general pressure range and the lapse rate is bad.
     */
    @Test
    public void rapidTemperatureHotGenPressRangeCheckBadLapseRate()
    throws CalculationWarning, ConversionException, InvalidValueWarning, 
    IOException {
    	// Build the records used for the check
    	records.add(buildPTURecord(250.0, 20.0, 50.0));
    	records.add(buildPTURecord(250.0, 22.0, 51.0));
    	records.add(buildPTURecord(250.0, 30.0, 52.0));

    	records.get(0).setAltitude(900.0, METERS);
    	records.get(2).setAltitude(1000.0, METERS);

    	// Run the check
    	autoQC.rapidTemperatureCheckForHotLatitudes(records, log);

    	// Check the output and results.
    	assertTrue("Rapid Temp Inc Hot Gen Press Bad Lapse Rate: log ready pre", 
    			reader.ready());
    	assertFalse("Rapid Temp Inc Hot Gen Press Bad Lapse Rate: log value", 
    			"".equals(reader.readLine()));
    	assertFalse("Rapid Temp Inc Hot Gen Press Bad Lapse Rate: log ready post", 
    			reader.ready());
    	for (int i = 0; i < records.size(); i++) {
    		assertEquals("Rapid Temp Inc Hot Gen Press Bad Lapse Rate: Press Flag "+i, 
    				BAD_FLAG, records.get(i).getPressureFlag());
    		assertEquals("Rapid Temp Inc Hot Gen Press Bad Lapse Rate: Temp Flag "+i, 
    				BAD_FLAG, records.get(i).getTemperatureFlag());
    		assertEquals("Rapid Temp Inc Hot Gen Press Bad Lapse Rate: RH Flag "+i, 
    				BAD_FLAG, records.get(i).getRelativeHumidityFlag());
    	}
    }
    
    
    /**
     * Test the rapid temperature increase check in hot latitudes when the
     * pressure is in the strat pressure range and there is no start
     * temperature.
     */
    @Test (expected = NullPointerException.class)
    public void rapidTemperatureHotStratPressRangeCheckNoStartTemp() throws
    CalculationWarning, ConversionException, InvalidValueWarning {
		// Build the records used for the check
		records.add(buildPTURecord(249.9, null, 50.0));
		records.add(buildPTURecord(249.9, 22.0, 51.0));
		records.add(buildPTURecord(249.8, 20.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheckForHotLatitudes(records, log);
    }
    
    /**
     * Test the rapid temperature increase check in hot latitudes when the
     * pressure is in the strat pressure range and there is no end 
     * temperature.
     */
    @Test (expected = NullPointerException.class)
    public void rapidTemperatureHotStratPressRangeCheckNoEndTemp() throws
    CalculationWarning, ConversionException, InvalidValueWarning {
		// Build the records used for the check
		records.add(buildPTURecord(249.9, 24.0, 50.0));
		records.add(buildPTURecord(249.9, 22.0, 51.0));
		records.add(buildPTURecord(249.8, null, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheckForHotLatitudes(records, log);
    }
    
    /**
     * Test the rapid temperature incrase check in hot latitudes when the
     * pressure is in the strat pressure range and there is no start
     * altitude.
     */
    @Test (expected = NullPointerException.class)
    public void rapidTemperatureHotStratPressRangeCheckNoStartAlt() throws
    CalculationWarning, ConversionException, InvalidValueWarning {
		// Build the records used for the check
		records.add(buildPTURecord(249.9, 24.0, 50.0));
		records.add(buildPTURecord(249.9, 22.0, 51.0));
		records.add(buildPTURecord(249.8, 20.0, 52.0));
		
		records.get(0).setAltitude(null, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheckForHotLatitudes(records, log);
    }
    
    /**
     * Test the rapid temperature increase check in hot latitudes when the
     * pressure is in the strat pressure range and there is no end
     * altitude.
     */
    @Test (expected = NullPointerException.class)
    public void rapidTemperatureHotStratPressRangeCheckNoEndAlt() throws
    CalculationWarning, ConversionException, InvalidValueWarning {
		// Build the records used for the check
		records.add(buildPTURecord(249.9, 24.0, 50.0));
		records.add(buildPTURecord(249.9, 22.0, 51.0));
		records.add(buildPTURecord(249.8, 20.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(null, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheckForHotLatitudes(records, log);
    }
    
    /**
     * Test the rapid temperature increase check in hot latitudes when the
     * pressure is in the strat pressure range and the altitudes are equal.
     */
    @Test public void rapidTemperatureHotStratPressRangeCheckEqualAlt() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check
		records.add(buildPTURecord(249.9, 24.0, 50.0));
		records.add(buildPTURecord(249.9, 22.0, 51.0));
		records.add(buildPTURecord(249.8, 20.0, 52.0));
		
		records.get(0).setAltitude(1000.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheckForHotLatitudes(records, log);
		
		// Test the output
		assertFalse("Rapid Temp Hot Strat Press Equal Alt: log ready", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Hot Strat Press Equal Alt: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Hot Strat Press Equal Alt: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Hot Strat Press Equal Alt: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid temperature increase check in hot latitudes when the
     * pressure is in the strat pressure range the lapse rate is acceptable.
     */
    @Test
    public void rapidTemperatureHotStratPressRangeCheckAcceptableLapseRate() 
    throws CalculationWarning, ConversionException, InvalidValueWarning, 
    IOException {
		// Build the records used for the check
		records.add(buildPTURecord(249.9, 29.9, 50.0));
		records.add(buildPTURecord(249.9, 22.0, 51.0));
		records.add(buildPTURecord(249.8, 20.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheckForHotLatitudes(records, log);

		// Check the output and results.
		assertFalse("Rapid Temp Inc Hot Strat Press OK Lapse Rate: log ready",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Inc Hot Strat Press OK Lapse Rate: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Inc Hot Strat Press OK Lapse Rate: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Inc Hot Strat Press OK Lapse Rate: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid temperature increase check in hot latitudes when the
     * pressure is in the strat pressure range and the laspe rate is
     * questionable.
     */
    @Test
    public void rapidTemperatureHotStratPressRangeCheckQuestionableLapseRate() 
    throws CalculationWarning, ConversionException, InvalidValueWarning, 
    IOException {
		// Build the records used for the check
		records.add(buildPTURecord(249.9, 19.9, 50.0));
		records.add(buildPTURecord(249.9, 19.9, 51.0));
		records.add(buildPTURecord(249.8, 20.0, 52.0));
		
		records.get(0).setAltitude(999.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheckForHotLatitudes(records, log);

		// Check the output and results.
		assertTrue("Rapid Temp Inc Hot Strat Press Questionable Lapse Rate: log ready pre", 
				reader.ready());
		assertFalse("Rapid Temp Inc Hot Strat Press Questionable Lapse Rate: log value", 
				"".equals(reader.readLine()));
		assertFalse("Rapid Temp Inc Hot Strat Press Questionable Lapse Rate: log ready post", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Inc Hot Strat Press Questionable Lapse Rate: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Inc Hot Strat Press Questionable Lapse Rate: Temp Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Inc Hot Strat Press Questionable Lapse Rate: RH Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid temperature increase check in hot latitudes when the
     * pressure is in the strat pressure range and the lapse rate is bad.
     */
    @Test
    public void rapidTemperatureHotStratPressRangeCheckBadLapseRate() 
    throws CalculationWarning, ConversionException, InvalidValueWarning, 
    IOException {
		// Build the records used for the check
		records.add(buildPTURecord(249.9, 19.8, 50.0));
		records.add(buildPTURecord(249.9, 19.9, 51.0));
		records.add(buildPTURecord(249.8, 20.1, 52.0));
		
		records.get(0).setAltitude(999.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheckForHotLatitudes(records, log);

		// Check the output and results.
		assertTrue("Rapid Temp Inc Hot Strat Press Bad Lapse Rate: log ready pre", 
				reader.ready());
		assertFalse("Rapid Temp Inc Hot Strat Press Bad Lapse Rate: log value", 
				"".equals(reader.readLine()));
		assertFalse("Rapid Temp Inc Hot Strat Press Bad Lapse Rate: log ready post", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Inc Hot Strat Press Bad Lapse Rate: Press Flag "+i, 
					BAD_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Inc Hot Strat Press Bad Lapse Rate: Temp Flag "+i, 
					BAD_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Inc Hot Strat Press Bad Lapse Rate: RH Flag "+i, 
					BAD_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }

    /**
     * Test the rapid temperature incrase check in hot latitudes when the
     * pressures are equal and in the strat pressure range.
     */
    @Test 
    public void rapidTemperatureHotStratPressRangeCheckEqualPressures() 
    throws CalculationWarning, ConversionException, InvalidValueWarning, 
    IOException {
		// Build the records used for the check
		records.add(buildPTURecord(249.9, 19.9, 50.0));
		records.add(buildPTURecord(249.9, 19.9, 51.0));
		records.add(buildPTURecord(249.9, 20.0, 52.0));
		
		records.get(0).setAltitude(999.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheckForHotLatitudes(records, log);

		// Check the output and results.
		assertFalse("Rapid Temp Inc Hot Strat Press Equal Pressures: log ready", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Inc Hot Strat Press Equal Pressures: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Inc Hot Strat Press Equal Pressures: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Inc Hot Strat Press Equal Pressures: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid temperature increase check in hot latitudes when the
     * start pressure is over the limit, but the end pressure is not.
     */
    @Test public void rapidTemperatureHotPressLimitBoundaryStartOverEnd() 
    throws CalculationWarning, ConversionException, InvalidValueWarning, 
    IOException {
		// Build the records used for the check
		records.add(buildPTURecord(250.0, 19.8, 50.0));
		records.add(buildPTURecord(249.9, 19.9, 51.0));
		records.add(buildPTURecord(249.9, 20.0, 52.0));
		
		records.get(0).setAltitude(999.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheckForHotLatitudes(records, log);

		// Check the output and results.
		assertTrue("Rapid Temp Inc Hot Strat Press Start Over End Boundary: log ready pre", 
				reader.ready());
		assertFalse("Rapid Temp Inc Hot Strat Press Start Over End Boundary: log value", 
				"".equals(reader.readLine()));
		assertFalse("Rapid Temp Inc Hot Strat Press Start Over End Boundary: log ready post", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Inc Hot Strat Start Over End Boundary: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Inc Hot Strat Start Over End Boundary: Temp Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Inc Hot Strat Start Over End Boundary: RH Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid temperature increase check in hot latitudes when the
     * start pressure is under the limit, but the end pressure is not.
     */
    @Test public void rapidTemperatureHotPressLimitBoundaryEndOverStart() 
    throws CalculationWarning, ConversionException, InvalidValueWarning, 
    IOException {
		// Build the records used for the check
		records.add(buildPTURecord(249.9, 19.9, 50.0));
		records.add(buildPTURecord(249.9, 19.9, 51.0));
		records.add(buildPTURecord(250.0, 20.0, 52.0));
		
		records.get(0).setAltitude(999.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheckForHotLatitudes(records, log);

		// Check the output and results.
		assertTrue("Rapid Temp Inc Hot Strat Press End Over Start Boundary: log ready pre", 
				reader.ready());
		assertFalse("Rapid Temp Inc Hot Strat Press End Over Start Boundary: log value", 
				"".equals(reader.readLine()));
		assertFalse("Rapid Temp Inc Hot Strat Press End Over Start Boundary: log ready post", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Inc Hot Strat End Over Start Boundary: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Inc Hot Strat End Over Start Boundary: Temp Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Inc Hot Strat End Over Start Boundary: RH Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    
    
    
    /**
     * Test the rapid temperature increase check in cold latitudes when the
     * start pressure is null.
     */
    @Test (expected = NullPointerException.class) 
    public void rapidTemperatureColdCheckNoStartPressure() throws
    CalculationWarning, ConversionException, InvalidValueWarning {
		// Build the records used for the check
		records.add(buildPTURecord(null, 24.0, 50.0));
		records.add(buildPTURecord(250.0, 22.0, 51.0));
		records.add(buildPTURecord(250.0, 20.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		coldQC.rapidTemperatureCheckForColdLatitudes(records, log);
    }
    
    /**
     * Test the rapid temperature increase check in cold latitudes when the
     * end pressure is null.
     */
    @Test (expected = NullPointerException.class)
    public void rapidTemperatureColdCheckNoEndPressure() throws
    CalculationWarning, ConversionException, InvalidValueWarning {
		// Build the records used for the check
		records.add(buildPTURecord(250.0, 24.0, 50.0));
		records.add(buildPTURecord(250.0, 22.0, 51.0));
		records.add(buildPTURecord(null, 20.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		coldQC.rapidTemperatureCheckForColdLatitudes(records, log);    
    }
    
    /**
     * Test the rapid temperature increase check in cold latitudes when the
     * pressure is in the general pressure range and there is no start
     * temperature.
     */
    @Test (expected = NullPointerException.class)
    public void rapidTemperatureColdGenPressRangeCheckNoStartTemp() throws
    CalculationWarning, ConversionException, InvalidValueWarning {
		// Build the records used for the check
		records.add(buildPTURecord(250.0, null, 50.0));
		records.add(buildPTURecord(250.0, 22.0, 51.0));
		records.add(buildPTURecord(250.0, 20.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		coldQC.rapidTemperatureCheckForColdLatitudes(records, log);
    }
    
    /**
     * Test the rapid temperature increase check in cold latitudes when the
     * pressure is in the general pressure range and there is no end 
     * temperature.
     */
    @Test (expected = NullPointerException.class)
    public void rapidTemperatureColdGenPressRangeCheckNoEndTemp() throws
    CalculationWarning, ConversionException, InvalidValueWarning {
		// Build the records used for the check
		records.add(buildPTURecord(250.0, 24.0, 50.0));
		records.add(buildPTURecord(250.0, 22.0, 51.0));
		records.add(buildPTURecord(250.0, null, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		coldQC.rapidTemperatureCheckForColdLatitudes(records, log);
    }
    
    /**
     * Test the rapid temperature incrase check in cold latitudes when the
     * pressure is in the general pressure range and there is no start
     * altitude.
     */
    @Test (expected = NullPointerException.class)
    public void rapidTemperatureColdGenPressRangeCheckNoStartAlt() throws
    CalculationWarning, ConversionException, InvalidValueWarning {
		// Build the records used for the check
		records.add(buildPTURecord(250.0, 24.0, 50.0));
		records.add(buildPTURecord(250.0, 22.0, 51.0));
		records.add(buildPTURecord(250.0, 20.0, 52.0));
		
		records.get(0).setAltitude(null, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		coldQC.rapidTemperatureCheckForColdLatitudes(records, log);
    }
    
    /**
     * Test the rapid temperature increase check in cold latitudes when the
     * pressure is in the general pressure range and there is no end
     * altitude.
     */
    @Test (expected = NullPointerException.class)
    public void rapidTemperatureColdGenPressRangeCheckNoEndAlt() throws
    CalculationWarning, ConversionException, InvalidValueWarning {
		// Build the records used for the check
		records.add(buildPTURecord(250.0, 24.0, 50.0));
		records.add(buildPTURecord(250.0, 22.0, 51.0));
		records.add(buildPTURecord(250.0, 20.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(null, METERS);
		
		// Run the check
		coldQC.rapidTemperatureCheckForColdLatitudes(records, log);
    }
    
    /**
     * Test the rapid temperature increase check in cold latitudes when the
     * pressure is in the general pressure range and the altitudes are equal.
     */
    @Test public void rapidTemperatureColdGenPressRangeCheckEqualAlt() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check
		records.add(buildPTURecord(250.0, 24.0, 50.0));
		records.add(buildPTURecord(250.0, 22.0, 51.0));
		records.add(buildPTURecord(250.0, 20.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(900.0, METERS);
		
		// Run the check
		coldQC.rapidTemperatureCheckForColdLatitudes(records, log);

		// Test the output
		assertFalse("Rapid Temp Cold Gen Press Equal Alt: log ready", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Cold Gen Press Equal Alt: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Cold Gen Press Equal Alt: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Cold Gen Press Equal Alt: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid temperature increase check in cold latitudes when the
     * pressure is in the general pressure range the lapse rate is acceptable.
     */
    @Test
    public void rapidTemperatureColdGenPressRangeCheckAcceptableLapseRate() 
    throws CalculationWarning, ConversionException, InvalidValueWarning, 
    IOException {
		// Build the records used for the check
		records.add(buildPTURecord(250.0, 24.0, 50.0));
		records.add(buildPTURecord(250.0, 22.0, 51.0));
		records.add(buildPTURecord(250.0, 20.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		coldQC.rapidTemperatureCheckForColdLatitudes(records, log);

		// Check the output and results.
		assertFalse("Rapid Temp Inc Cold Gen Press OK Lapse Rate: log ready",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Inc Cold Gen Press OK Lapse Rate: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Inc Cold Gen Press OK Lapse Rate: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Inc Cold Gen Press OK Lapse Rate: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid temperature increase check in cold latitudes when the
     * pressure is in the general pressure range and the laspe rate is
     * questionable.
     */
    @Test
    public void rapidTemperatureColdGenPressRangeCheckQuestionableLapseRate() 
    throws CalculationWarning, ConversionException, InvalidValueWarning, 
    IOException {
		// Build the records used for the check
		records.add(buildPTURecord(250.0, 20.0, 50.0));
		records.add(buildPTURecord(250.0, 22.0, 51.0));
		records.add(buildPTURecord(250.0, 25.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		coldQC.rapidTemperatureCheckForColdLatitudes(records, log);

		// Check the output and results.
		assertTrue("Rapid Temp Inc Cold Gen Press Questionable Lapse Rate: log ready pre", 
				reader.ready());
		assertFalse("Rapid Temp Inc Cold Gen Press Questionable Lapse Rate: log value", 
				"".equals(reader.readLine()));
		assertFalse("Rapid Temp Inc Cold Gen Press Questionable Lapse Rate: log ready post", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Inc Cold Gen Press Questionable Lapse Rate: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Inc Cold Gen Press Questionable Lapse Rate: Temp Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Inc Cold Gen Press Questionable Lapse Rate: RH Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getRelativeHumidityFlag());
		}

    }
    
    /**
     * Test the rapid temperature increase check in cold latitudes when the
     * pressure is in the general pressure range and the lapse rate is bad.
     */
    @Test
    public void rapidTemperatureColdGenPressRangeCheckBadLapseRate()
    throws CalculationWarning, ConversionException, InvalidValueWarning, 
    IOException {
    	// Build the records used for the check
    	records.add(buildPTURecord(250.0, 20.0, 50.0));
    	records.add(buildPTURecord(250.0, 22.0, 51.0));
    	records.add(buildPTURecord(250.0, 30.0, 52.0));

    	records.get(0).setAltitude(900.0, METERS);
    	records.get(2).setAltitude(1000.0, METERS);

    	// Run the check
    	coldQC.rapidTemperatureCheckForColdLatitudes(records, log);

    	// Check the output and results.
    	assertTrue("Rapid Temp Inc Cold Gen Press Bad Lapse Rate: log ready pre", 
    			reader.ready());
    	assertFalse("Rapid Temp Inc Cold Gen Press Bad Lapse Rate: log value", 
    			"".equals(reader.readLine()));
    	assertFalse("Rapid Temp Inc Cold Gen Press Bad Lapse Rate: log ready post", 
    			reader.ready());
    	for (int i = 0; i < records.size(); i++) {
    		assertEquals("Rapid Temp Inc Cold Gen Press Bad Lapse Rate: Press Flag "+i, 
    				BAD_FLAG, records.get(i).getPressureFlag());
    		assertEquals("Rapid Temp Inc Cold Gen Press Bad Lapse Rate: Temp Flag "+i, 
    				BAD_FLAG, records.get(i).getTemperatureFlag());
    		assertEquals("Rapid Temp Inc Cold Gen Press Bad Lapse Rate: RH Flag "+i, 
    				BAD_FLAG, records.get(i).getRelativeHumidityFlag());
    	}

    }
    
    
    /**
     * Test the rapid temperature increase check in cold latitudes when the
     * pressure is in the strat pressure range and there is no start
     * temperature.
     */
    @Test (expected = NullPointerException.class)
    public void rapidTemperatureColdStratPressRangeCheckNoStartTemp() throws
    CalculationWarning, ConversionException, InvalidValueWarning {
		// Build the records used for the check
		records.add(buildPTURecord(210.1, null, 50.0));
		records.add(buildPTURecord(210.1, 22.0, 51.0));
		records.add(buildPTURecord(210.0, 20.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		coldQC.rapidTemperatureCheckForColdLatitudes(records, log);
    }
    
    /**
     * Test the rapid temperature increase check in cold latitudes when the
     * pressure is in the strat pressure range and there is no end 
     * temperature.
     */
    @Test (expected = NullPointerException.class)
    public void rapidTemperatureColdStratPressRangeCheckNoEndTemp() throws
    CalculationWarning, ConversionException, InvalidValueWarning {
		// Build the records used for the check
		records.add(buildPTURecord(210.1, 24.0, 50.0));
		records.add(buildPTURecord(210.1, 22.0, 51.0));
		records.add(buildPTURecord(210.0, null, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		coldQC.rapidTemperatureCheckForColdLatitudes(records, log);
    }
    
    /**
     * Test the rapid temperature incrase check in cold latitudes when the
     * pressure is in the strat pressure range and there is no start
     * altitude.
     */
    @Test (expected = NullPointerException.class)
    public void rapidTemperatureColdStratPressRangeCheckNoStartAlt() throws
    CalculationWarning, ConversionException, InvalidValueWarning {
		// Build the records used for the check
		records.add(buildPTURecord(210.1, 24.0, 50.0));
		records.add(buildPTURecord(210.1, 22.0, 51.0));
		records.add(buildPTURecord(210.0, 20.0, 52.0));
		
		records.get(0).setAltitude(null, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		coldQC.rapidTemperatureCheckForColdLatitudes(records, log);
    }
    
    /**
     * Test the rapid temperature increase check in cold latitudes when the
     * pressure is in the strat pressure range and there is no end
     * altitude.
     */
    @Test (expected = NullPointerException.class)
    public void rapidTemperatureColdStratPressRangeCheckNoEndAlt() throws
    CalculationWarning, ConversionException, InvalidValueWarning {
		// Build the records used for the check
		records.add(buildPTURecord(210.1, 24.0, 50.0));
		records.add(buildPTURecord(210.1, 22.0, 51.0));
		records.add(buildPTURecord(210.0, 20.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(null, METERS);
		
		// Run the check
		coldQC.rapidTemperatureCheckForColdLatitudes(records, log);
    }
    
    /**
     * Test the rapid temperature increase check in cold latitudes when the
     * pressure is in the strat pressure range and the altitudes are equal.
     */
    @Test public void rapidTemperatureColdStratPressRangeCheckEqualAlt() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check
		records.add(buildPTURecord(200.0, 24.0, 50.0));
		records.add(buildPTURecord(200.0, 22.0, 51.0));
		records.add(buildPTURecord(200.0, 20.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(900.0, METERS);
		
		// Run the check
		coldQC.rapidTemperatureCheckForColdLatitudes(records, log);

		// Test the output
		assertFalse("Rapid Temp Cold Strat Press Equal Alt: log ready", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Cold Strat Press Equal Alt: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Cold Strat Press Equal Alt: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Cold Strat Press Equal Alt: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid temperature increase check in cpld latitudes when the
     * pressure is in the strat pressure range the lapse rate is acceptable.
     */
    @Test
    public void rapidTemperatureColdStratPressRangeCheckAcceptableLapseRate() 
    throws CalculationWarning, ConversionException, InvalidValueWarning, 
    IOException {
		// Build the records used for the check
		records.add(buildPTURecord(249.9, 29.9, 50.0));
		records.add(buildPTURecord(249.9, 22.0, 51.0));
		records.add(buildPTURecord(249.8, 20.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		coldQC.rapidTemperatureCheckForColdLatitudes(records, log);

		// Check the output and results.
		assertFalse("Rapid Temp Inc Cold Strat Press OK Lapse Rate: log ready",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Inc Cold Strat Press OK Lapse Rate: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Inc Cold Strat Press OK Lapse Rate: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Inc Cold Strat Press OK Lapse Rate: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid temperature increase check in cold latitudes when the
     * pressure is in the strat pressure range and the laspe rate is
     * questionable.
     */
    @Test
    public void rapidTemperatureColdStratPressRangeCheckQuestionableLapseRate() 
    throws CalculationWarning, ConversionException, InvalidValueWarning, 
    IOException {
		// Build the records used for the check
		records.add(buildPTURecord(249.9, 19.9, 50.0));
		records.add(buildPTURecord(249.9, 19.9, 51.0));
		records.add(buildPTURecord(249.8, 20.0, 52.0));
		
		records.get(0).setAltitude(999.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheckForColdLatitudes(records, log);

		// Check the output and results.
		assertTrue("Rapid Temp Inc Cold Strat Press Questionable Lapse Rate: log ready pre", 
				reader.ready());
		assertFalse("Rapid Temp Inc Cold Strat Press Questionable Lapse Rate: log value", 
				"".equals(reader.readLine()));
		assertFalse("Rapid Temp Inc Cold Strat Press Questionable Lapse Rate: log ready post", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Inc Cold Strat Press Questionable Lapse Rate: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Inc Cold Strat Press Questionable Lapse Rate: Temp Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Inc Cold Strat Press Questionable Lapse Rate: RH Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid temperature increase check in cold latitudes when the
     * pressure is in the strat pressure range and the lapse rate is bad.
     */
    @Test
    public void rapidTemperatureColdStratPressRangeCheckBadLapseRate() 
    throws CalculationWarning, ConversionException, InvalidValueWarning, 
    IOException {
		// Build the records used for the check
		records.add(buildPTURecord(249.9, 19.8, 50.0));
		records.add(buildPTURecord(249.9, 19.9, 51.0));
		records.add(buildPTURecord(249.8, 20.1, 52.0));
		
		records.get(0).setAltitude(999.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheckForColdLatitudes(records, log);

		// Check the output and results.
		assertTrue("Rapid Temp Inc Cold Strat Press Bad Lapse Rate: log ready pre", 
				reader.ready());
		assertFalse("Rapid Temp Inc Cold Strat Press Bad Lapse Rate: log value", 
				"".equals(reader.readLine()));
		assertFalse("Rapid Temp Inc Cold Strat Press Bad Lapse Rate: log ready post", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Inc Cold Strat Press Bad Lapse Rate: Press Flag "+i, 
					BAD_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Inc Cold Strat Press Bad Lapse Rate: Temp Flag "+i, 
					BAD_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Inc Cold Strat Press Bad Lapse Rate: RH Flag "+i, 
					BAD_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    
    /**
     * Test the rapid temperature increase check in cold latitudes when the
     * pressure is in the surface pressure range and there is no start
     * temperature.
     */
    @Test (expected = NullPointerException.class)
    public void rapidTemperatureColdSurfacePressRangeCheckNoStartTemp() throws
    CalculationWarning, ConversionException, InvalidValueWarning {
		// Build the records used for the check
		records.add(buildPTURecord(900.0, null, 50.0));
		records.add(buildPTURecord(900.0, 22.0, 51.0));
		records.add(buildPTURecord(900.0, 20.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		coldQC.rapidTemperatureCheckForColdLatitudes(records, log);
    }
    
    /**
     * Test the rapid temperature increase check in cold latitudes when the
     * pressure is in the surface pressure range and there is no end 
     * temperature.
     */
    @Test (expected = NullPointerException.class)
    public void rapidTemperatureColdSurfacePressRangeCheckNoEndTemp() throws
    CalculationWarning, ConversionException, InvalidValueWarning {
		// Build the records used for the check
		records.add(buildPTURecord(900.0, 24.0, 50.0));
		records.add(buildPTURecord(900.0, 22.0, 51.0));
		records.add(buildPTURecord(900.0, null, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		coldQC.rapidTemperatureCheckForColdLatitudes(records, log);
    }
    
    /**
     * Test the rapid temperature incrase check in cold latitudes when the
     * pressure is in the surface pressure range and there is no start
     * altitude.
     */
    @Test (expected = NullPointerException.class)
    public void rapidTemperatureColdSurfacePressRangeCheckNoStartAlt() throws
    CalculationWarning, ConversionException, InvalidValueWarning {
		// Build the records used for the check
		records.add(buildPTURecord(900.0, 24.0, 50.0));
		records.add(buildPTURecord(900.0, 22.0, 51.0));
		records.add(buildPTURecord(900.0, 20.0, 52.0));
		
		records.get(0).setAltitude(null, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		coldQC.rapidTemperatureCheckForColdLatitudes(records, log);
    }
    
    /**
     * Test the rapid temperature increase check in cold latitudes when the
     * pressure is in the surface pressure range and there is no end
     * altitude.
     */
    @Test (expected = NullPointerException.class)
    public void rapidTemperatureColdSurfacePressRangeCheckNoEndAlt() throws
    CalculationWarning, ConversionException, InvalidValueWarning {
		// Build the records used for the check
		records.add(buildPTURecord(900.0, 24.0, 50.0));
		records.add(buildPTURecord(900.0, 22.0, 51.0));
		records.add(buildPTURecord(900.0, 20.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(null, METERS);
		
		// Run the check
		coldQC.rapidTemperatureCheckForColdLatitudes(records, log);
    }
    
    /**
     * Test the rapid temperature increase check in cold latitudes when the
     * pressure is in the surface pressure range and the altitudes are equal.
     */
    @Test public void rapidTemperatureColdSurfacePressRangeCheckEqualAlt() 
    throws CalculationWarning, ConversionException, InvalidValueWarning, 
    IOException {
		// Build the records used for the check
		records.add(buildPTURecord(900.0, 24.0, 50.0));
		records.add(buildPTURecord(890.0, 22.0, 51.0));
		records.add(buildPTURecord(880.0, 20.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(900.0, METERS);
		
		// Run the check
		coldQC.rapidTemperatureCheckForColdLatitudes(records, log);

		// Test the output
		assertFalse("Rapid Temp Cold Sfc Press Equal Alt: log ready", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Cold Sfc Press Equal Alt: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Cold Sfc Press Equal Alt: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Cold Sfc Press Equal Alt: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid temperature increase check in cpld latitudes when the
     * pressure is in the surface pressure range the lapse rate is acceptable.
     */
    @Test
    public void rapidTemperatureColdSurfacePressRangeCheckAcceptableLapseRate() 
    throws CalculationWarning, ConversionException, InvalidValueWarning, 
    IOException {
		// Build the records used for the check
		records.add(buildPTURecord(249.9, 29.9, 50.0));
		records.add(buildPTURecord(249.9, 22.0, 51.0));
		records.add(buildPTURecord(249.8, 20.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		coldQC.rapidTemperatureCheckForColdLatitudes(records, log);

		// Check the output and results.
		assertFalse("Rapid Temp Inc Cold Strat Press OK Lapse Rate: log ready",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Inc Cold Strat Press OK Lapse Rate: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Inc Cold Strat Press OK Lapse Rate: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Inc Cold Strat Press OK Lapse Rate: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid temperature increase check in cold latitudes when the
     * pressure is in the surface pressure range and the laspe rate is
     * questionable.
     */
    @Test
    public void rapidTemperatureColdSurfacePressRangeCheckQuestionableLapseRate() 
    throws CalculationWarning, ConversionException, InvalidValueWarning, 
    IOException {
		// Build the records used for the check
		records.add(buildPTURecord(849.9, 20.0, 50.0));
		records.add(buildPTURecord(849.9, 20.0, 51.0));
		records.add(buildPTURecord(849.8, 20.0, 52.0));
		
		records.get(0).setAltitude(999.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		coldQC.rapidTemperatureCheckForColdLatitudes(records, log);

		// Check the output and results.
		assertTrue("Rapid Temp Inc Cold Sfc Press Questionable Lapse Rate: log ready pre", 
				reader.ready());
		assertFalse("Rapid Temp Inc Cold Sfc Press Questionable Lapse Rate: log value", 
				"".equals(reader.readLine()));
		assertFalse("Rapid Temp Inc Cold Sfc Press Questionable Lapse Rate: log ready post", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Inc Cold Sfc Press Questionable Lapse Rate: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Inc Cold Sfc Press Questionable Lapse Rate: Temp Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Inc Cold Sfc Press Questionable Lapse Rate: RH Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid temperature increase check in cold latitudes when the
     * pressure is in the surface pressure range and the lapse rate is bad.
     */
    @Test
    public void rapidTemperatureColdSurfacePressRangeCheckBadLapseRate() 
    throws CalculationWarning, ConversionException, InvalidValueWarning, 
    IOException {
		// Build the records used for the check
		records.add(buildPTURecord(249.9, 19.9, 50.0));
		records.add(buildPTURecord(249.9, 19.9, 51.0));
		records.add(buildPTURecord(249.8, 20.1, 52.0));
		
		records.get(0).setAltitude(999.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheckForColdLatitudes(records, log);

		// Check the output and results.
		assertTrue("Rapid Temp Inc Cold Sfc Press Bad Lapse Rate: log ready pre", 
				reader.ready());
		assertFalse("Rapid Temp Inc Cold Sfc Press Bad Lapse Rate: log value", 
				"".equals(reader.readLine()));
		assertFalse("Rapid Temp Inc Cold Sfc Press Bad Lapse Rate: log ready post", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Inc Cold Sfc Press Bad Lapse Rate: Press Flag "+i, 
					BAD_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Inc Cold Sfc Press Bad Lapse Rate: Temp Flag "+i, 
					BAD_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Inc Cold Sfc Press Bad Lapse Rate: RH Flag "+i, 
					BAD_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }

    /**
     * Test the rapid temperature incrase check in cold latitudes when the
     * pressures are equal and in the strat pressure range.
     */
    @Test 
    public void rapidTemperatureColdStratPressRangeCheckEqualPressures() 
    throws CalculationWarning, ConversionException, InvalidValueWarning, 
    IOException {
		// Build the records used for the check
		records.add(buildPTURecord(249.9, 19.9, 50.0));
		records.add(buildPTURecord(249.9, 19.9, 51.0));
		records.add(buildPTURecord(249.9, 20.0, 52.0));
		
		records.get(0).setAltitude(999.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		coldQC.rapidTemperatureCheckForColdLatitudes(records, log);

		// Check the output and results.
		assertFalse("Rapid Temp Inc Cold Strat Press Equal Pressures: log ready", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Inc Cold Strat Press Equal Pressures: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Inc Cold Strat Press Equal Pressures: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Inc Cold Strat Press Equal Pressures: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid temperature increase check in cold latitudes when the
     * start pressure is over the limit, but the end pressure is not.
     */
    @Test public void rapidTemperatureColdPressLimitBoundaryStartOverEnd() 
    throws CalculationWarning, ConversionException, InvalidValueWarning, 
    IOException {
		// Build the records used for the check
		records.add(buildPTURecord(250.0, 19.8, 50.0));
		records.add(buildPTURecord(249.9, 19.9, 51.0));
		records.add(buildPTURecord(249.9, 20.0, 52.0));
		
		records.get(0).setAltitude(999.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		coldQC.rapidTemperatureCheckForColdLatitudes(records, log);

		// Check the output and results.
		assertTrue("Rapid Temp Inc Cold Strat Press Start Over End Boundary: log ready pre", 
				reader.ready());
		assertFalse("Rapid Temp Inc Cold Strat Press Start Over End Boundary: log value", 
				"".equals(reader.readLine()));
		assertFalse("Rapid Temp Inc Cold Strat Press Start Over End Boundary: log ready post", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Inc Cold Strat Start Over End Boundary: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Inc Cold Strat Start Over End Boundary: Temp Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Inc Cold Strat Start Over End Boundary: RH Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid temperature increase check in cold latitudes when the
     * start pressure is under the limit, but the end pressure is not.
     */
    @Test public void rapidTemperatureColdPressLimitBoundaryEndOverStart() 
    throws CalculationWarning, ConversionException, InvalidValueWarning, 
    IOException {
		// Build the records used for the check
		records.add(buildPTURecord(249.9, 19.9, 50.0));
		records.add(buildPTURecord(249.9, 19.9, 51.0));
		records.add(buildPTURecord(250.0, 20.0, 52.0));
		
		records.get(0).setAltitude(999.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		coldQC.rapidTemperatureCheckForColdLatitudes(records, log);

		// Check the output and results.
		assertTrue("Rapid Temp Inc Cold Strat Press End Over Start Boundary: log ready pre", 
				reader.ready());
		assertFalse("Rapid Temp Inc Cold Strat Press End Over Start Boundary: log value", 
				"".equals(reader.readLine()));
		assertFalse("Rapid Temp Inc Cold Strat Press End Over Start Boundary: log ready post", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Inc Cold Strat End Over Start Boundary: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Inc Cold Strat End Over Start Boundary: Temp Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Inc Cold Strat End Over Start Boundary: RH Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }


    /**
     * Test the rapid temperature increase check in cold latitudes when the
     * start pressure is over the surface limit, but the end pressure is not.
     */
    @Test public void rapidTemperatureColdSfcPressLimitBoundaryStartOverEnd() 
    throws CalculationWarning, ConversionException, InvalidValueWarning, 
    IOException {
		// Build the records used for the check
    	records.add(buildPTURecord(800.0, 20.0, 50.0));
		records.add(buildPTURecord(799.0, 22.0, 51.0));
		records.add(buildPTURecord(799.0, 25.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);

		// Run the check
		coldQC.rapidTemperatureCheckForColdLatitudes(records, log);

		// Check the output and results.
		assertTrue("Rapid Temp Inc Cold Sfc Press Start Over End Boundary: log ready pre", 
				reader.ready());
		assertFalse("Rapid Temp Inc Cold Sfc Press Start Over End Boundary: log value", 
				"".equals(reader.readLine()));
		assertFalse("Rapid Temp Inc Cold Sfc Press Start Over End Boundary: log ready post", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Inc Cold Sfc Start Over End Boundary: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Inc Cold Sfc Start Over End Boundary: Temp Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Inc Cold Sfc Start Over End Boundary: RH Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid temperature increase check in cold latitudes when the
     * start pressure is under the surface limit, but the end pressure is not.
     */
    @Test public void rapidTemperatureColdSfcPressLimitBoundaryEndOverStart() 
    throws CalculationWarning, ConversionException, InvalidValueWarning, 
    IOException {
		// Build the records used for the check
		records.add(buildPTURecord(799.0, 20.0, 50.0));
		records.add(buildPTURecord(799.0, 22.0, 51.0));
		records.add(buildPTURecord(800.0, 25.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		coldQC.rapidTemperatureCheckForColdLatitudes(records, log);

		// Check the output and results.
		assertTrue("Rapid Temp Inc Cold Sfc Press End Over Start Boundary: log ready pre", 
				reader.ready());
		assertFalse("Rapid Temp Inc Cold Sfc Press End Over Start Boundary: log value", 
				"".equals(reader.readLine()));
		assertFalse("Rapid Temp Inc Cold Sfc Press End Over Start Boundary: log ready post", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Inc Cold Sfc End Over Start Boundary: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Inc Cold Sfc End Over Start Boundary: Temp Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Inc Cold Sfc End Over Start Boundary: RH Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }

    
    
    
    /**
     * Test the rapid temperature increase check when the
     * start temperature is null.
     */
    @Test public void rapidTemperatureNoStartTemperature() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check
		records.add(buildPTURecord(1013.0, null, 50.0));
		records.add(buildPTURecord(1010.3, 22.0, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		records.get(0).setAltitude(1000.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheck(records, log);
    	
		// Test the output
		assertFalse("Rapid Temp No End Temp: log after read", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp No End Temp: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp No End Temp: Temp Flag "+i, 
					i == 0 ? MISSING_FLAG : UNCHECKED_FLAG, 
					records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp No End Temp: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid temperature increase check when the
     * end temperature is null.
     */
    @Test public void rapidTemperatureNoEndTemperature() throws
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check
		records.add(buildPTURecord(1013.0, 24.0, 50.0));
		records.add(buildPTURecord(1010.3, 22.0, 51.0));
		records.add(buildPTURecord(1000.0, null, 52.0));
		
		records.get(0).setAltitude(1000.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheck(records, log);
    	
		// Test the output
		assertFalse("Rapid Temp No End Temp: log after read", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp No End Temp: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp No End Temp: Temp Flag "+i, 
					i == 2 ? MISSING_FLAG : UNCHECKED_FLAG, 
					records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp No End Temp: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    
    
    
    
    /**
     * Test the rapid temperature increase check when the altitudes are
     * equal.
     */
    @Test public void rapidTemperatureCheckEqualAltitude() throws 
	CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check
		records.add(buildPTURecord(1013.0, 24.0, 50.0));
		records.add(buildPTURecord(1010.3, 22.0, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		records.get(0).setAltitude(1000.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheck(records, 5.0, 100.0, log);
    	
		// Test the output
		assertFalse("Adiabatic Infiniate Lapse Rate: log after read", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Adiabatic Infiniate Lapse Rate: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Adiabatic Infiniate Lapse Rate: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Adiabatic Infiniate Lapse Rate: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
		
		// Now set it up to do positive infinity.
		records.get(2).setTemperature(28.0, CELCIUS);

		// Run the check
		autoQC.rapidTemperatureCheck(records, 5.0, 100.0, log);
    	
		// Test the output
		assertFalse("Adiabatic Infiniate Lapse Rate: log after read", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Adiabatic Infiniate Lapse Rate: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Adiabatic Infiniate Lapse Rate: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Adiabatic Infiniate Lapse Rate: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid temperature check when there is not a defined end
     * temperature.
     */
    @Test (expected = NullPointerException.class)
    public void rapidTemperatureCheckNoEndTemperature() throws 
    CalculationWarning, ConversionException, InvalidValueWarning {
		// Build the records used for the check
		records.add(buildPTURecord(1013.0, 24.0, 50.0));
		records.add(buildPTURecord(1010.3, 22.0, 51.0));
		records.add(buildPTURecord(1000.0, null, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheck(records, 5.0, 100.0, log);
    }
    
    /**
     * Test the rapid temperature check when there is not a defined start
     * temperature.
     */
    @Test (expected = NullPointerException.class)
    public void rapidTemperatureCheckNoStartTemperature() throws 
    CalculationWarning, ConversionException, InvalidValueWarning {
		// Build the records used for the check
		records.add(buildPTURecord(1013.0, null, 50.0));
		records.add(buildPTURecord(1010.3, 22.0, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheck(records, 5.0, 100.0, log);
    }
    
    /**
     * Test the rapid temperature check when there is not a defined end
     * altitude.
     */
    @Test (expected = NullPointerException.class)
    public void rapidTemperatureCheckNoEndAltitude() throws 
    CalculationWarning, ConversionException, InvalidValueWarning {
		// Build the records used for the check
		records.add(buildPTURecord(1013.0, 24.0, 50.0));
		records.add(buildPTURecord(1010.3, 22.0, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(null, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheck(records, 5.0, 100.0, log);
    }
    
    /**
     * Test the rapid temperature check when there is not a defined start
     * altitude.
     */
    @Test (expected = NullPointerException.class)
    public void rapidTemperatureCheckNoStartAltitude() throws 
    CalculationWarning, ConversionException, InvalidValueWarning {
		// Build the records used for the check
		records.add(buildPTURecord(1013.0, 24.0, 50.0));
		records.add(buildPTURecord(1010.3, 22.0, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		records.get(0).setAltitude(null, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheck(records, 5.0, 100.0, log);
    }
    
    /**
     * Test the rapid temperature check when the lapse rate is acceptable.
     */
    @Test public void rapidTemperatureCheckAcceptableLapseRate() throws 
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check
		records.add(buildPTURecord(1013.0, 20.0, 50.0));
		records.add(buildPTURecord(1010.3, 22.0, 51.0));
		records.add(buildPTURecord(1000.0, 24.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheck(records, 5.0, 100.0, log);

		// Check the output and results.
		assertFalse("Rapid Temp Inc OK Lapse Rate: log ready", reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Inc OK Lapse Rate: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Inc OK Lapse Rate: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Inc OK Lapse Rate: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid temperature check when the lapse rate is questionable.
     */
    @Test public void rapidTemperatureCheckQuestionableLapseRate() throws 
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check
		records.add(buildPTURecord(1013.0, 20.0, 50.0));
		records.add(buildPTURecord(1010.3, 22.0, 51.0));
		records.add(buildPTURecord(1000.0, 25.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheck(records, 5.0, 100.0, log);

		// Check the output and results.
		assertTrue("Rapid Temp Inc Questionable Lapse Rate: log ready pre", 
				reader.ready());
		assertFalse("Rapid Temp Inc Questionable Lapse Rate: log value", 
				"".equals(reader.readLine()));
		assertFalse("Rapid Temp Inc Questionable Lapse Rate: log ready post", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Inc Questionable Lapse Rate: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Inc Questionable Lapse Rate: Temp Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Inc Questionable Lapse Rate: RH Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
    /**
     * Test the rapid temperature check when the lapse rate is bad.
     */
    @Test public void rapidTemperatureCheckBadLapseRate() throws 
    CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check
		records.add(buildPTURecord(1013.0, 20.0, 50.0));
		records.add(buildPTURecord(1010.3, 22.0, 51.0));
		records.add(buildPTURecord(1000.0, 30.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check
		autoQC.rapidTemperatureCheck(records, 5.0, 100.0, log);

		// Check the output and results.
		assertTrue("Rapid Temp Inc Bad Lapse Rate: log ready pre", 
				reader.ready());
		assertFalse("Rapid Temp Inc Bad Lapse Rate: log value", 
				"".equals(reader.readLine()));
		assertFalse("Rapid Temp Inc Bad Lapse Rate: log ready post", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Rapid Temp Inc Bad Lapse Rate: Press Flag "+i, 
					BAD_FLAG, records.get(i).getPressureFlag());
			assertEquals("Rapid Temp Inc Bad Lapse Rate: Temp Flag "+i, 
					BAD_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Rapid Temp Inc Bad Lapse Rate: RH Flag "+i, 
					BAD_FLAG, records.get(i).getRelativeHumidityFlag());
		}
    }
    
	/**
	 * Test the super adiabatic lapse rate check when there is not
	 * a defined endint temperature.
	 */
	@Test public void superAdiabaticRateNoEndTemperature() throws 
	CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check
		records.add(buildPTURecord(1013.0, 24.0, 50.0));
		records.add(buildPTURecord(1010.3, 22.0, 51.0));
		records.add(buildPTURecord(1000.0, null, 52.0));
		
		// Run the check
		autoQC.superAdiabaticLapseRateCheck(records, -15.0, -30.0, log);
		
		// Test the output
		assertFalse("Adiabatic No End Temp: log output",reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Adiabatic No End Temp: Press Flag "+i, UNCHECKED_FLAG,
					records.get(i).getPressureFlag());
			assertEquals("Adiabatic No End Temp: Temp Flag "+i, 
					i == records.size() - 1 ? MISSING_FLAG : UNCHECKED_FLAG,
					records.get(i).getTemperatureFlag());
			assertEquals("Adiabatic No End Temp: RH Flag "+i, UNCHECKED_FLAG,
					records.get(i).getRelativeHumidityFlag());
		}
	}
	
	/**
	 * Test the super adiabatic lapse rate check when there is not
	 * a defined starting temperature.
	 */
	@Test public void superAdiabaticRateNoStartTemperature() throws
	CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
		records.add(buildPTURecord(1013.0, null, 50.0));
		records.add(buildPTURecord(1010.3, 22.0, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		// Run the check.
		autoQC.superAdiabaticLapseRateCheck(records, -15.0, -30.0, log);
		
		// Test the output
		assertFalse("Adiabatic No Start Temp: log output",reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Adiabatic No Start Temp: Press Flag "+i, 
					UNCHECKED_FLAG,	records.get(i).getPressureFlag());
			assertEquals("Adiabatic No Start Temp: Temp Flag "+i, 
					i == 0 ? MISSING_FLAG : UNCHECKED_FLAG,
					records.get(i).getTemperatureFlag());
			assertEquals("Adiabatic No Start Temp: RH Flag "+i, UNCHECKED_FLAG,
					records.get(i).getRelativeHumidityFlag());
		}
	}
	
	/**
	 * Test the super adiabatic lapse rate check when the difference
	 * between the start temperature and end temparture is too small
	 * to cause a check.
	 */
	@Test public void superAdiabaticRateTooSmallTempChange() throws
	CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
		records.add(buildPTURecord(1013.0, 20.2, 50.0));
		records.add(buildPTURecord(1010.3, 20.1, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		// Run the check.
		autoQC.superAdiabaticLapseRateCheck(records, -15.0, -30.0, log);
		
		// Test the output
		assertFalse("Adiabatic Small Temp Change: log output",reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Adiabatic Small Temp Change: Press Flag "+i, 
					UNCHECKED_FLAG,	records.get(i).getPressureFlag());
			assertEquals("Adiabatic Small Temp Change: Temp Flag "+i, 
					UNCHECKED_FLAG,	records.get(i).getTemperatureFlag());
			assertEquals("Adiabatic Small Temp Change: RH Flag "+i, 
					UNCHECKED_FLAG,	records.get(i).getRelativeHumidityFlag());
		}		
	}
	
	/**
	 * Test the super adiabatic lapse rate check when the difference between
	 * the start and end temperatues are equal to the defined lapse rate
	 * difference.
	 */
	@Test public void superAdiabaticRateTempChangeEqualLapseRateDiff() throws
	CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
		records.add(buildPTURecord(1013.0, 20.25, 50.0));
		records.add(buildPTURecord(1010.3, 20.1, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		// Run the check.
		autoQC.superAdiabaticLapseRateCheck(records, -15.0, -30.0, log);
		
		// Test the output
		assertFalse("Adiabatic Equal Temp Lapse Rate: log output",
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Adiabatic Equal Temp Lapse Rate: Press Flag "+i, 
					UNCHECKED_FLAG,	records.get(i).getPressureFlag());
			assertEquals("Adiabatic Equal Temp Lapse Rate: Temp Flag "+i, 
					UNCHECKED_FLAG,	records.get(i).getTemperatureFlag());
			assertEquals("Adiabatic Equal Temp Lapse Rate: RH Flag "+i, 
					UNCHECKED_FLAG,	records.get(i).getRelativeHumidityFlag());
		}		
	}
	
	/**
	 * Test the super adiabatic lapse rate check when the start and end
	 * altitudes are equal.
	 */
	@Test public void superAdiabaticRateEqualAltitudes() throws
	CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
		records.add(buildPTURecord(1013.0, 20.3, 50.0));
		records.add(buildPTURecord(1010.3, 20.1, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		records.get(0).setAltitude(500.0, METERS);
		records.get(2).setAltitude(500.0, METERS);
		
		// Run the check.
		autoQC.superAdiabaticLapseRateCheck(records, -15.0, -30.0, log);
		
		// Test the output
		assertTrue("Adiabatic Equal Altitude: log output", reader.ready());
		assertFalse("Adiabatic Equal Altitude: log value", 
				"".equals(reader.readLine()));
		assertFalse("Adiabatic Equal Altitude: log after read", reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Adiabatic Equal Altitude: Press Flag "+i, 
					BAD_FLAG, records.get(i).getPressureFlag());
			assertEquals("Adiabatic Equal Altitude: Temp Flag "+i, 
					BAD_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Adiabatic Equal Altitude: RH Flag "+i, 
					BAD_FLAG, records.get(i).getRelativeHumidityFlag());
		}		
	}
	
	/**
	 * Test the super adiabatic lapse rate check when the calculated lapse rate
	 * is an acceptable value.
	 */
	@Test public void superAdiabaticRateAcceptableLapseRate() throws
	CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
		records.add(buildPTURecord(1013.0, 21.4, 50.0));
		records.add(buildPTURecord(1010.3, 20.8, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check.
		autoQC.superAdiabaticLapseRateCheck(records, -15.0, -30.0, log);
		
		// Test the output
		assertFalse("Adiabatic OK Lapse Rate: log after read", reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Adiabatic OK Lapse Rate: Press Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getPressureFlag());
			assertEquals("Adiabatic OK Lapse Rate: Temp Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Adiabatic OK Lapse Rate: RH Flag "+i, 
					UNCHECKED_FLAG, records.get(i).getRelativeHumidityFlag());
		}		
	}
	
	/**
	 * Test the super adiabatic lapse rate check when the calculated lapse rate
	 * is a questionable value.
	 */
	@Test public void superAdiabaticRateQuestionableLapseRate() throws
	CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
		records.add(buildPTURecord(1013.0, 21.5, 50.0));
		records.add(buildPTURecord(1010.3, 20.8, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check.
		autoQC.superAdiabaticLapseRateCheck(records, -15.0, -30.0, log);
		
		// Test the output
		assertTrue("Adiabatic Questionable Lapse Rate: log output", 
				reader.ready());
		assertFalse("Adiabatic Questionable Lapse Rate: log value", 
				"".equals(reader.readLine()));
		assertFalse("Adiabatic Questionable Lapse Rate: log after read", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Adiabatic Questionable Lapse Rate: Press Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getPressureFlag());
			assertEquals("Adiabatic Questionable Lapse Rate: Temp Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Adiabatic Questionable Lapse Rate: RH Flag "+i, 
					QUESTIONABLE_FLAG, records.get(i).getRelativeHumidityFlag());
		}		
	}
	
	/**
	 * Test the super adiabatic lapse rate check when the calculated lapse rate
	 * is a bad value.
	 */
	@Test public void superAdiabaticRateBadLapseRate() throws
	CalculationWarning, ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
		records.add(buildPTURecord(1013.0, 23.0, 50.0));
		records.add(buildPTURecord(1010.3, 20.8, 51.0));
		records.add(buildPTURecord(1000.0, 20.0, 52.0));
		
		records.get(0).setAltitude(900.0, METERS);
		records.get(2).setAltitude(1000.0, METERS);
		
		// Run the check.
		autoQC.superAdiabaticLapseRateCheck(records, -15.0, -30.0, log);
		
		// Test the output
		assertTrue("Adiabatic Bad Lapse Rate: log output", 
				reader.ready());
		assertFalse("Adiabatic Bad Lapse Rate: log value", 
				"".equals(reader.readLine()));
		assertFalse("Adiabatic Bad Lapse Rate: log after read", 
				reader.ready());
		for (int i = 0; i < records.size(); i++) {
			assertEquals("Adiabatic Bad Lapse Rate: Press Flag "+i, 
					BAD_FLAG, records.get(i).getPressureFlag());
			assertEquals("Adiabatic Bad Lapse Rate: Temp Flag "+i, 
					BAD_FLAG, records.get(i).getTemperatureFlag());
			assertEquals("Adiabatic Bad Lapse Rate: RH Flag "+i, 
					BAD_FLAG, records.get(i).getRelativeHumidityFlag());
		}		
	}
	
	@Test 
	public void pressureFlagUpdateBaseRecordBadFlagToGoodFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setPressure(1.0, MILLIBARS);
		record.setPressureFlag(BAD_FLAG);

		autoQC.updatePressureFlag(record, GOOD_FLAG);
		assertEquals("Update Pressure Flag: B -> G", BAD_FLAG, 
				record.getPressureFlag());
	}

	@Test 
	public void pressureFlagUpdateBaseRecordBadFlagToEstimateFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setPressure(1.0, MILLIBARS);
		record.setPressureFlag(BAD_FLAG);

		autoQC.updatePressureFlag(record, ESTIMATE_FLAG);
		assertEquals("Update Pressure Flag: B -> E", BAD_FLAG, 
				record.getPressureFlag());
	}

	@Test 
	public void pressureFlagUpdateBaseRecordBadFlagToMissingFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setPressure(1.0, MILLIBARS);
		record.setPressureFlag(BAD_FLAG);

		autoQC.updatePressureFlag(record, MISSING_FLAG);
		assertEquals("Update Pressure Flag: B -> M", BAD_FLAG, 
				record.getPressureFlag());
	}

	@Test public void pressureFlagUpdateBaseRecordBadFlagToQuestionableFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException, 
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setPressure(1.0, MILLIBARS);
		record.setPressureFlag(BAD_FLAG);
		
		autoQC.updatePressureFlag(record, QUESTIONABLE_FLAG);
		assertEquals("Update Pressure Flag: B -> Q", BAD_FLAG, 
				record.getPressureFlag());
	}

	@Test 
	public void pressureFlagUpdateBaseRecordBadFlagToUncheckedFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setPressure(1.0, MILLIBARS);
		record.setPressureFlag(BAD_FLAG);

		autoQC.updatePressureFlag(record, UNCHECKED_FLAG);
		assertEquals("Update Pressure Flag: B -> U", UNCHECKED_FLAG, 
				record.getPressureFlag());
	}
	
	@Test public void pressureFlagUpdateBaseRecordGoodFlagToBadFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException, 
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setPressure(1.0, MILLIBARS);
		record.setPressureFlag(GOOD_FLAG);
		
		autoQC.updatePressureFlag(record, BAD_FLAG);
		assertEquals("Update Pressure Flag: G -> B", BAD_FLAG, 
				record.getPressureFlag());
	}

	@Test 
	public void pressureFlagUpdateBaseRecordGoodFlagToEstimateFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setPressure(1.0, MILLIBARS);
		record.setPressureFlag(GOOD_FLAG);

		autoQC.updatePressureFlag(record, ESTIMATE_FLAG);
		assertEquals("Update Pressure Flag: G -> E", ESTIMATE_FLAG, 
				record.getPressureFlag());
	}

	@Test 
	public void pressureFlagUpdateBaseRecordGoodFlagToMissingFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setPressure(1.0, MILLIBARS);
		record.setPressureFlag(GOOD_FLAG);

		autoQC.updatePressureFlag(record, MISSING_FLAG);
		assertEquals("Update Pressure Flag: G -> M", GOOD_FLAG, 
				record.getPressureFlag());
	}

	@Test 
	public void pressureFlagUpdateBaseRecordGoodFlagToQuestionableFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setPressure(1.0, MILLIBARS);
		record.setPressureFlag(GOOD_FLAG);

		autoQC.updatePressureFlag(record, QUESTIONABLE_FLAG);
		assertEquals("Update Pressure Flag: G -> Q", QUESTIONABLE_FLAG, 
				record.getPressureFlag());
	}

	@Test 
	public void pressureFlagUpdateBaseRecordGoodFlagToUncheckedFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setPressure(1.0, MILLIBARS);
		record.setPressureFlag(GOOD_FLAG);

		autoQC.updatePressureFlag(record, UNCHECKED_FLAG);
		assertEquals("Update Pressure Flag: G -> U", UNCHECKED_FLAG, 
				record.getPressureFlag());
	}

	@Test public void pressureFlagUpdateBaseRecordEstimateFlagToBadFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException, 
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setPressure(1.0, MILLIBARS);
		record.setPressureFlag(ESTIMATE_FLAG);
		
		autoQC.updatePressureFlag(record, BAD_FLAG);
		assertEquals("Update Pressure Flag: E -> B", BAD_FLAG, 
				record.getPressureFlag());
	}

	@Test 
	public void pressureFlagUpdateBaseRecordEstimateFlagToGoodFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setPressure(1.0, MILLIBARS);
		record.setPressureFlag(ESTIMATE_FLAG);

		autoQC.updatePressureFlag(record, GOOD_FLAG);
		assertEquals("Update Pressure Flag: E -> G", ESTIMATE_FLAG, 
				record.getPressureFlag());
	}

	@Test 
	public void pressureFlagUpdateBaseRecordEstimateFlagToMissingFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setPressure(1.0, MILLIBARS);
		record.setPressureFlag(ESTIMATE_FLAG);

		autoQC.updatePressureFlag(record, MISSING_FLAG);
		assertEquals("Update Pressure Flag: E -> M", ESTIMATE_FLAG, 
				record.getPressureFlag());
	}

	@Test 
	public void pressureFlagUpdateBaseRecordEstimateFlagToQuestionableFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setPressure(1.0, MILLIBARS);
		record.setPressureFlag(ESTIMATE_FLAG);

		autoQC.updatePressureFlag(record, QUESTIONABLE_FLAG);
		assertEquals("Update Pressure Flag: E -> Q", QUESTIONABLE_FLAG, 
				record.getPressureFlag());
	}

	@Test 
	public void pressureFlagUpdateBaseRecordEstimateFlagToUncheckedFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setPressure(1.0, MILLIBARS);
		record.setPressureFlag(ESTIMATE_FLAG);

		autoQC.updatePressureFlag(record, UNCHECKED_FLAG);
		assertEquals("Update Pressure Flag: E -> U", UNCHECKED_FLAG, 
				record.getPressureFlag());
	}
	
	@Test public void pressureFlagUpdateBaseRecordMissingFlagToBadFlag() {
		ESCSoundingRecord record = new ESCSoundingRecord();
		autoQC.updatePressureFlag(record, BAD_FLAG);
		assertEquals("Update Pressure Flag: M -> B", MISSING_FLAG, 
				record.getPressureFlag());
	}

	@Test public void pressureFlagUpdateBaseRecordMissingFlagToGoodFlag() {
		ESCSoundingRecord record = new ESCSoundingRecord();
		autoQC.updatePressureFlag(record, GOOD_FLAG);
		assertEquals("Update Pressure Flag: M -> G", MISSING_FLAG, 
				record.getPressureFlag());
	}

	@Test public void pressureFlagUpdateBaseRecordMissingFlagToEstimateFlag(){
		ESCSoundingRecord record = new ESCSoundingRecord();
		autoQC.updatePressureFlag(record, ESTIMATE_FLAG);
		assertEquals("Update Pressure Flag: M -> E", MISSING_FLAG, 
				record.getPressureFlag());
	}

	@Test 
	public void pressureFlagUpdateBaseRecordMissingFlagToQuestionableFlag() {
		ESCSoundingRecord record = new ESCSoundingRecord();
		autoQC.updatePressureFlag(record, QUESTIONABLE_FLAG);
		assertEquals("Update Pressure Flag: M -> D", MISSING_FLAG, 
				record.getPressureFlag());
	}

	@Test 
	public void pressureFlagUpdateBaseRecordMissingFlagToUncheckedFlag() {
		ESCSoundingRecord record = new ESCSoundingRecord();
		autoQC.updatePressureFlag(record, UNCHECKED_FLAG);
		assertEquals("Update Pressure Flag: M -> U", MISSING_FLAG, 
				record.getPressureFlag());
	}

	@Test public void pressureFlagUpdateBaseRecordQuestionableFlagToBadFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException, 
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setPressure(1.0, MILLIBARS);
		record.setPressureFlag(QUESTIONABLE_FLAG);
		
		autoQC.updatePressureFlag(record, BAD_FLAG);
		assertEquals("Update Pressure Flag: Q -> B", BAD_FLAG, 
				record.getPressureFlag());
	}

	@Test 
	public void pressureFlagUpdateBaseRecordQuestionableFlagToGoodFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setPressure(1.0, MILLIBARS);
		record.setPressureFlag(QUESTIONABLE_FLAG);

		autoQC.updatePressureFlag(record, GOOD_FLAG);
		assertEquals("Update Pressure Flag: Q -> G", QUESTIONABLE_FLAG, 
				record.getPressureFlag());
	}

	@Test 
	public void pressureFlagUpdateBaseRecordQuestionableFlagToEstimateFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setPressure(1.0, MILLIBARS);
		record.setPressureFlag(QUESTIONABLE_FLAG);

		autoQC.updatePressureFlag(record, ESTIMATE_FLAG);
		assertEquals("Update Pressure Flag: Q -> E", QUESTIONABLE_FLAG, 
				record.getPressureFlag());
	}

	@Test 
	public void pressureFlagUpdateBaseRecordQuestionableFlagToMissingFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setPressure(1.0, MILLIBARS);
		record.setPressureFlag(QUESTIONABLE_FLAG);

		autoQC.updatePressureFlag(record, MISSING_FLAG);
		assertEquals("Update Pressure Flag: Q -> M", QUESTIONABLE_FLAG, 
				record.getPressureFlag());
	}

	@Test 
	public void pressureFlagUpdateBaseRecordQuestionableFlagToUncheckedFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setPressure(1.0, MILLIBARS);
		record.setPressureFlag(QUESTIONABLE_FLAG);

		autoQC.updatePressureFlag(record, UNCHECKED_FLAG);
		assertEquals("Update Pressure Flag: Q -> U", UNCHECKED_FLAG, 
				record.getPressureFlag());
	}
	
	@Test public void pressureFlagUpdateBaseRecordUncheckedFlagToBadFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException, 
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setPressure(1.0, MILLIBARS);
		
		autoQC.updatePressureFlag(record, BAD_FLAG);
		assertEquals("Update Pressure Flag: U -> B", BAD_FLAG, 
				record.getPressureFlag());
	}

	@Test 
	public void pressureFlagUpdateBaseRecordUncheckedFlagToGoodFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setPressure(1.0, MILLIBARS);

		autoQC.updatePressureFlag(record, GOOD_FLAG);
		assertEquals("Update Pressure Flag: U -> G", GOOD_FLAG, 
				record.getPressureFlag());
	}

	@Test 
	public void pressureFlagUpdateBaseRecordUncheckedFlagToEstimateFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setPressure(1.0, MILLIBARS);

		autoQC.updatePressureFlag(record, ESTIMATE_FLAG);
		assertEquals("Update Pressure Flag: U -> E", ESTIMATE_FLAG, 
				record.getPressureFlag());
	}

	@Test 
	public void pressureFlagUpdateBaseRecordUncheckedFlagToMissingFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setPressure(1.0, MILLIBARS);

		autoQC.updatePressureFlag(record, MISSING_FLAG);
		assertEquals("Update Pressure Flag: U -> M", UNCHECKED_FLAG, 
				record.getPressureFlag());
	}

	@Test 
	public void pressureFlagUpdateBaseRecordUncheckedToQuestionableFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setPressure(1.0, MILLIBARS);

		autoQC.updatePressureFlag(record, QUESTIONABLE_FLAG);
		assertEquals("Update Pressure Flag: U -> Q", QUESTIONABLE_FLAG, 
				record.getPressureFlag());
	}

	@Test 
	public void rhFlagUpdateBaseRecordBadFlagToGoodFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setRelativeHumidity(1.0);
		record.setRelativeHumidityFlag(BAD_FLAG);

		autoQC.updateRelativeHumidityFlag(record, GOOD_FLAG);
		assertEquals("Update RH Flag: B -> G", BAD_FLAG, 
				record.getRelativeHumidityFlag());
	}

	@Test 
	public void rhFlagUpdateBaseRecordBadFlagToEstimateFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setRelativeHumidity(1.0);
		record.setRelativeHumidityFlag(BAD_FLAG);

		autoQC.updateRelativeHumidityFlag(record, ESTIMATE_FLAG);
		assertEquals("Update RH Flag: B -> E", BAD_FLAG, 
				record.getRelativeHumidityFlag());
	}

	@Test 
	public void rhFlagUpdateBaseRecordBadFlagToMissingFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setRelativeHumidity(1.0);
		record.setRelativeHumidityFlag(BAD_FLAG);

		autoQC.updateRelativeHumidityFlag(record, MISSING_FLAG);
		assertEquals("Update RH Flag: B -> M", BAD_FLAG, 
				record.getRelativeHumidityFlag());
	}

	@Test public void rhFlagUpdateBaseRecordBadFlagToQuestionableFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException, 
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setRelativeHumidity(1.0);
		record.setRelativeHumidityFlag(BAD_FLAG);
		
		autoQC.updateRelativeHumidityFlag(record, QUESTIONABLE_FLAG);
		assertEquals("Update RH Flag: B -> Q", BAD_FLAG, 
				record.getRelativeHumidityFlag());
	}

	@Test 
	public void rhFlagUpdateBaseRecordBadFlagToUncheckedFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setRelativeHumidity(1.0);
		record.setRelativeHumidityFlag(BAD_FLAG);

		autoQC.updateRelativeHumidityFlag(record, UNCHECKED_FLAG);
		assertEquals("Update RH Flag: B -> U", UNCHECKED_FLAG, 
				record.getRelativeHumidityFlag());
	}
	
	@Test public void rhFlagUpdateBaseRecordGoodFlagToBadFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException, 
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setRelativeHumidity(1.0);
		record.setRelativeHumidityFlag(GOOD_FLAG);
		
		autoQC.updateRelativeHumidityFlag(record, BAD_FLAG);
		assertEquals("Update RH Flag: G -> B", BAD_FLAG, 
				record.getRelativeHumidityFlag());
	}

	@Test 
	public void rhFlagUpdateBaseRecordGoodFlagToEstimateFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setRelativeHumidity(1.0);
		record.setRelativeHumidityFlag(GOOD_FLAG);

		autoQC.updateRelativeHumidityFlag(record, ESTIMATE_FLAG);
		assertEquals("Update RH Flag: G -> E", ESTIMATE_FLAG, 
				record.getRelativeHumidityFlag());
	}

	@Test 
	public void rhFlagUpdateBaseRecordGoodFlagToMissingFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setRelativeHumidity(1.0);
		record.setRelativeHumidityFlag(GOOD_FLAG);

		autoQC.updateRelativeHumidityFlag(record, MISSING_FLAG);
		assertEquals("Update RH Flag: G -> M", GOOD_FLAG, 
				record.getRelativeHumidityFlag());
	}

	@Test 
	public void rhFlagUpdateBaseRecordGoodFlagToQuestionableFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setRelativeHumidity(1.0);
		record.setRelativeHumidityFlag(GOOD_FLAG);

		autoQC.updateRelativeHumidityFlag(record, QUESTIONABLE_FLAG);
		assertEquals("Update RH Flag: G -> Q", QUESTIONABLE_FLAG, 
				record.getRelativeHumidityFlag());
	}

	@Test 
	public void rhFlagUpdateBaseRecordGoodFlagToUncheckedFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setRelativeHumidity(1.0);
		record.setRelativeHumidityFlag(GOOD_FLAG);

		autoQC.updateRelativeHumidityFlag(record, UNCHECKED_FLAG);
		assertEquals("Update RH Flag: G -> U", UNCHECKED_FLAG, 
				record.getRelativeHumidityFlag());
	}

	@Test public void rhFlagUpdateBaseRecordEstimateFlagToBadFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException, 
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setRelativeHumidity(1.0);
		record.setRelativeHumidityFlag(ESTIMATE_FLAG);
		
		autoQC.updateRelativeHumidityFlag(record, BAD_FLAG);
		assertEquals("Update RH Flag: E -> B", BAD_FLAG, 
				record.getRelativeHumidityFlag());
	}

	@Test 
	public void rhFlagUpdateBaseRecordEstimateFlagToGoodFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setRelativeHumidity(1.0);
		record.setRelativeHumidityFlag(ESTIMATE_FLAG);

		autoQC.updateRelativeHumidityFlag(record, GOOD_FLAG);
		assertEquals("Update RH Flag: E -> G", ESTIMATE_FLAG, 
				record.getRelativeHumidityFlag());
	}

	@Test 
	public void rhFlagUpdateBaseRecordEstimateFlagToMissingFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setRelativeHumidity(1.0);
		record.setRelativeHumidityFlag(ESTIMATE_FLAG);

		autoQC.updateRelativeHumidityFlag(record, MISSING_FLAG);
		assertEquals("Update RH Flag: E -> M", ESTIMATE_FLAG, 
				record.getRelativeHumidityFlag());
	}

	@Test 
	public void rhFlagUpdateBaseRecordEstimateFlagToQuestionableFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setRelativeHumidity(1.0);
		record.setRelativeHumidityFlag(ESTIMATE_FLAG);

		autoQC.updateRelativeHumidityFlag(record, QUESTIONABLE_FLAG);
		assertEquals("Update RH Flag: E -> Q", QUESTIONABLE_FLAG, 
				record.getRelativeHumidityFlag());
	}

	@Test 
	public void rhFlagUpdateBaseRecordEstimateFlagToUncheckedFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setRelativeHumidity(1.0);
		record.setRelativeHumidityFlag(ESTIMATE_FLAG);

		autoQC.updateRelativeHumidityFlag(record, UNCHECKED_FLAG);
		assertEquals("Update RH Flag: E -> U", UNCHECKED_FLAG, 
				record.getRelativeHumidityFlag());
	}
	
	@Test public void rhFlagUpdateBaseRecordMissingFlagToBadFlag() {
		ESCSoundingRecord record = new ESCSoundingRecord();
		autoQC.updateRelativeHumidityFlag(record, BAD_FLAG);
		assertEquals("Update RH Flag: M -> B", MISSING_FLAG, 
				record.getRelativeHumidityFlag());
	}

	@Test public void rhFlagUpdateBaseRecordMissingFlagToGoodFlag() {
		ESCSoundingRecord record = new ESCSoundingRecord();
		autoQC.updateRelativeHumidityFlag(record, GOOD_FLAG);
		assertEquals("Update RH Flag: M -> G", MISSING_FLAG, 
				record.getRelativeHumidityFlag());
	}

	@Test public void rhFlagUpdateBaseRecordMissingFlagToEstimateFlag(){
		ESCSoundingRecord record = new ESCSoundingRecord();
		autoQC.updateRelativeHumidityFlag(record, ESTIMATE_FLAG);
		assertEquals("Update RH Flag: M -> E", MISSING_FLAG, 
				record.getRelativeHumidityFlag());
	}

	@Test 
	public void rhFlagUpdateBaseRecordMissingFlagToQuestionableFlag() {
		ESCSoundingRecord record = new ESCSoundingRecord();
		autoQC.updateRelativeHumidityFlag(record, QUESTIONABLE_FLAG);
		assertEquals("Update RH Flag: M -> D", MISSING_FLAG, 
				record.getRelativeHumidityFlag());
	}

	@Test 
	public void rhFlagUpdateBaseRecordMissingFlagToUncheckedFlag() {
		ESCSoundingRecord record = new ESCSoundingRecord();
		autoQC.updateRelativeHumidityFlag(record, UNCHECKED_FLAG);
		assertEquals("Update RH Flag: M -> U", MISSING_FLAG, 
				record.getRelativeHumidityFlag());
	}

	@Test public void rhFlagUpdateBaseRecordQuestionableFlagToBadFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException, 
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setRelativeHumidity(1.0);
		record.setRelativeHumidityFlag(QUESTIONABLE_FLAG);
		
		autoQC.updateRelativeHumidityFlag(record, BAD_FLAG);
		assertEquals("Update RH Flag: Q -> B", BAD_FLAG, 
				record.getRelativeHumidityFlag());
	}

	@Test 
	public void rhFlagUpdateBaseRecordQuestionableFlagToGoodFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setRelativeHumidity(1.0);
		record.setRelativeHumidityFlag(QUESTIONABLE_FLAG);

		autoQC.updateRelativeHumidityFlag(record, GOOD_FLAG);
		assertEquals("Update RH Flag: Q -> G", QUESTIONABLE_FLAG, 
				record.getRelativeHumidityFlag());
	}

	@Test 
	public void rhFlagUpdateBaseRecordQuestionableFlagToEstimateFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setRelativeHumidity(1.0);
		record.setRelativeHumidityFlag(QUESTIONABLE_FLAG);

		autoQC.updateRelativeHumidityFlag(record, ESTIMATE_FLAG);
		assertEquals("Update RH Flag: Q -> E", QUESTIONABLE_FLAG, 
				record.getRelativeHumidityFlag());
	}

	@Test 
	public void rhFlagUpdateBaseRecordQuestionableFlagToMissingFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setRelativeHumidity(1.0);
		record.setRelativeHumidityFlag(QUESTIONABLE_FLAG);

		autoQC.updateRelativeHumidityFlag(record, MISSING_FLAG);
		assertEquals("Update RH Flag: Q -> M", QUESTIONABLE_FLAG, 
				record.getRelativeHumidityFlag());
	}

	@Test 
	public void rhFlagUpdateBaseRecordQuestionableFlagToUncheckedFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setRelativeHumidity(1.0);
		record.setRelativeHumidityFlag(QUESTIONABLE_FLAG);

		autoQC.updateRelativeHumidityFlag(record, UNCHECKED_FLAG);
		assertEquals("Update RH Flag: Q -> U", UNCHECKED_FLAG, 
				record.getRelativeHumidityFlag());
	}
	
	@Test public void rhFlagUpdateBaseRecordUncheckedFlagToBadFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException, 
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setRelativeHumidity(1.0);
		
		autoQC.updateRelativeHumidityFlag(record, BAD_FLAG);
		assertEquals("Update RH Flag: U -> B", BAD_FLAG, 
				record.getRelativeHumidityFlag());
	}

	@Test 
	public void rhFlagUpdateBaseRecordUncheckedFlagToGoodFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setRelativeHumidity(1.0);

		autoQC.updateRelativeHumidityFlag(record, GOOD_FLAG);
		assertEquals("Update RH Flag: U -> G", GOOD_FLAG, 
				record.getRelativeHumidityFlag());
	}

	@Test 
	public void rhFlagUpdateBaseRecordUncheckedFlagToEstimateFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setRelativeHumidity(1.0);

		autoQC.updateRelativeHumidityFlag(record, ESTIMATE_FLAG);
		assertEquals("Update RH Flag: U -> E", ESTIMATE_FLAG, 
				record.getRelativeHumidityFlag());
	}

	@Test 
	public void rhFlagUpdateBaseRecordUncheckedFlagToMissingFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setRelativeHumidity(1.0);

		autoQC.updateRelativeHumidityFlag(record, MISSING_FLAG);
		assertEquals("Update RH Flag: U -> M", UNCHECKED_FLAG, 
				record.getRelativeHumidityFlag());
	}

	@Test 
	public void rhFlagUpdateBaseRecordUncheckedToQuestionableFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setRelativeHumidity(1.0);

		autoQC.updateRelativeHumidityFlag(record, QUESTIONABLE_FLAG);
		assertEquals("Update RH Flag: U -> Q", QUESTIONABLE_FLAG, 
				record.getRelativeHumidityFlag());
	}

	@Test 
	public void temperatureFlagUpdateBaseRecordBadFlagToGoodFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setTemperature(1.0, CELCIUS);
		record.setTemperatureFlag(BAD_FLAG);

		autoQC.updateTemperatureFlag(record, GOOD_FLAG);
		assertEquals("Update Temperature Flag: B -> G", BAD_FLAG, 
				record.getTemperatureFlag());
	}

	@Test 
	public void temperatureFlagUpdateBaseRecordBadFlagToEstimateFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setTemperature(1.0, CELCIUS);
		record.setTemperatureFlag(BAD_FLAG);

		autoQC.updateTemperatureFlag(record, ESTIMATE_FLAG);
		assertEquals("Update Temperature Flag: B -> E", BAD_FLAG, 
				record.getTemperatureFlag());
	}

	@Test 
	public void temperatureFlagUpdateBaseRecordBadFlagToMissingFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setTemperature(1.0, CELCIUS);
		record.setTemperatureFlag(BAD_FLAG);

		autoQC.updateTemperatureFlag(record, MISSING_FLAG);
		assertEquals("Update Temperature Flag: B -> M", BAD_FLAG, 
				record.getTemperatureFlag());
	}

	@Test public void temperatureFlagUpdateBaseRecordBadFlagToQuestionableFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException, 
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setTemperature(1.0, CELCIUS);
		record.setTemperatureFlag(BAD_FLAG);
		
		autoQC.updateTemperatureFlag(record, QUESTIONABLE_FLAG);
		assertEquals("Update Temperature Flag: B -> Q", BAD_FLAG, 
				record.getTemperatureFlag());
	}

	@Test 
	public void temperatureFlagUpdateBaseRecordBadFlagToUncheckedFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setTemperature(1.0, CELCIUS);
		record.setTemperatureFlag(BAD_FLAG);

		autoQC.updateTemperatureFlag(record, UNCHECKED_FLAG);
		assertEquals("Update Temperature Flag: B -> U", UNCHECKED_FLAG, 
				record.getTemperatureFlag());
	}
	
	@Test public void temperatureFlagUpdateBaseRecordGoodFlagToBadFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException, 
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setTemperature(1.0, CELCIUS);
		record.setTemperatureFlag(GOOD_FLAG);
		
		autoQC.updateTemperatureFlag(record, BAD_FLAG);
		assertEquals("Update Temperature Flag: G -> B", BAD_FLAG, 
				record.getTemperatureFlag());
	}

	@Test 
	public void temperatureFlagUpdateBaseRecordGoodFlagToEstimateFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setTemperature(1.0, CELCIUS);
		record.setTemperatureFlag(GOOD_FLAG);

		autoQC.updateTemperatureFlag(record, ESTIMATE_FLAG);
		assertEquals("Update Temperature Flag: G -> E", ESTIMATE_FLAG, 
				record.getTemperatureFlag());
	}

	@Test 
	public void temperatureFlagUpdateBaseRecordGoodFlagToMissingFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setTemperature(1.0, CELCIUS);
		record.setTemperatureFlag(GOOD_FLAG);

		autoQC.updateTemperatureFlag(record, MISSING_FLAG);
		assertEquals("Update Temperature Flag: G -> M", GOOD_FLAG, 
				record.getTemperatureFlag());
	}

	@Test 
	public void temperatureFlagUpdateBaseRecordGoodFlagToQuestionableFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setTemperature(1.0, CELCIUS);
		record.setTemperatureFlag(GOOD_FLAG);

		autoQC.updateTemperatureFlag(record, QUESTIONABLE_FLAG);
		assertEquals("Update Temperature Flag: G -> Q", QUESTIONABLE_FLAG, 
				record.getTemperatureFlag());
	}

	@Test 
	public void temperatureFlagUpdateBaseRecordGoodFlagToUncheckedFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setTemperature(1.0, CELCIUS);
		record.setTemperatureFlag(GOOD_FLAG);

		autoQC.updateTemperatureFlag(record, UNCHECKED_FLAG);
		assertEquals("Update Temperature Flag: G -> U", UNCHECKED_FLAG, 
				record.getTemperatureFlag());
	}

	@Test public void temperatureFlagUpdateBaseRecordEstimateFlagToBadFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException, 
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setTemperature(1.0, CELCIUS);
		record.setTemperatureFlag(ESTIMATE_FLAG);
		
		autoQC.updateTemperatureFlag(record, BAD_FLAG);
		assertEquals("Update Temperature Flag: E -> B", BAD_FLAG, 
				record.getTemperatureFlag());
	}

	@Test 
	public void temperatureFlagUpdateBaseRecordEstimateFlagToGoodFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setTemperature(1.0, CELCIUS);
		record.setTemperatureFlag(ESTIMATE_FLAG);

		autoQC.updateTemperatureFlag(record, GOOD_FLAG);
		assertEquals("Update Temperature Flag: E -> G", ESTIMATE_FLAG, 
				record.getTemperatureFlag());
	}

	@Test 
	public void temperatureFlagUpdateBaseRecordEstimateFlagToMissingFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setTemperature(1.0, CELCIUS);
		record.setTemperatureFlag(ESTIMATE_FLAG);

		autoQC.updateTemperatureFlag(record, MISSING_FLAG);
		assertEquals("Update Temperature Flag: E -> M", ESTIMATE_FLAG, 
				record.getTemperatureFlag());
	}

	@Test 
	public void temperatureFlagUpdateBaseRecordEstimateFlagToQuestionableFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setTemperature(1.0, CELCIUS);
		record.setTemperatureFlag(ESTIMATE_FLAG);

		autoQC.updateTemperatureFlag(record, QUESTIONABLE_FLAG);
		assertEquals("Update Temperature Flag: E -> Q", QUESTIONABLE_FLAG, 
				record.getTemperatureFlag());
	}

	@Test 
	public void temperatureFlagUpdateBaseRecordEstimateFlagToUncheckedFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setTemperature(1.0, CELCIUS);
		record.setTemperatureFlag(ESTIMATE_FLAG);

		autoQC.updateTemperatureFlag(record, UNCHECKED_FLAG);
		assertEquals("Update Temperature Flag: E -> U", UNCHECKED_FLAG, 
				record.getTemperatureFlag());
	}
	
	@Test public void temperatureFlagUpdateBaseRecordMissingFlagToBadFlag() {
		ESCSoundingRecord record = new ESCSoundingRecord();
		autoQC.updateTemperatureFlag(record, BAD_FLAG);
		assertEquals("Update Temperature Flag: M -> B", MISSING_FLAG, 
				record.getTemperatureFlag());
	}

	@Test public void temperatureFlagUpdateBaseRecordMissingFlagToGoodFlag() {
		ESCSoundingRecord record = new ESCSoundingRecord();
		autoQC.updateTemperatureFlag(record, GOOD_FLAG);
		assertEquals("Update Temperature Flag: M -> G", MISSING_FLAG, 
				record.getTemperatureFlag());
	}

	@Test public void temperatureFlagUpdateBaseRecordMissingFlagToEstimateFlag(){
		ESCSoundingRecord record = new ESCSoundingRecord();
		autoQC.updateTemperatureFlag(record, ESTIMATE_FLAG);
		assertEquals("Update Temperature Flag: M -> E", MISSING_FLAG, 
				record.getTemperatureFlag());
	}

	@Test 
	public void temperatureFlagUpdateBaseRecordMissingFlagToQuestionableFlag() {
		ESCSoundingRecord record = new ESCSoundingRecord();
		autoQC.updateTemperatureFlag(record, QUESTIONABLE_FLAG);
		assertEquals("Update Temperature Flag: M -> D", MISSING_FLAG, 
				record.getTemperatureFlag());
	}

	@Test 
	public void temperatureFlagUpdateBaseRecordMissingFlagToUncheckedFlag() {
		ESCSoundingRecord record = new ESCSoundingRecord();
		autoQC.updateTemperatureFlag(record, UNCHECKED_FLAG);
		assertEquals("Update Temperature Flag: M -> U", MISSING_FLAG, 
				record.getTemperatureFlag());
	}

	@Test public void temperatureFlagUpdateBaseRecordQuestionableFlagToBadFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException, 
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setTemperature(1.0, CELCIUS);
		record.setTemperatureFlag(QUESTIONABLE_FLAG);
		
		autoQC.updateTemperatureFlag(record, BAD_FLAG);
		assertEquals("Update Temperature Flag: Q -> B", BAD_FLAG, 
				record.getTemperatureFlag());
	}

	@Test 
	public void temperatureFlagUpdateBaseRecordQuestionableFlagToGoodFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setTemperature(1.0, CELCIUS);
		record.setTemperatureFlag(QUESTIONABLE_FLAG);

		autoQC.updateTemperatureFlag(record, GOOD_FLAG);
		assertEquals("Update Temperature Flag: Q -> G", QUESTIONABLE_FLAG, 
				record.getTemperatureFlag());
	}

	@Test 
	public void temperatureFlagUpdateBaseRecordQuestionableFlagToEstimateFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setTemperature(1.0, CELCIUS);
		record.setTemperatureFlag(QUESTIONABLE_FLAG);

		autoQC.updateTemperatureFlag(record, ESTIMATE_FLAG);
		assertEquals("Update Temperature Flag: Q -> E", QUESTIONABLE_FLAG, 
				record.getTemperatureFlag());
	}

	@Test 
	public void temperatureFlagUpdateBaseRecordQuestionableFlagToMissingFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setTemperature(1.0, CELCIUS);
		record.setTemperatureFlag(QUESTIONABLE_FLAG);

		autoQC.updateTemperatureFlag(record, MISSING_FLAG);
		assertEquals("Update Temperature Flag: Q -> M", QUESTIONABLE_FLAG, 
				record.getTemperatureFlag());
	}

	@Test 
	public void temperatureFlagUpdateBaseRecordQuestionableFlagToUncheckedFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setTemperature(1.0, CELCIUS);
		record.setTemperatureFlag(QUESTIONABLE_FLAG);

		autoQC.updateTemperatureFlag(record, UNCHECKED_FLAG);
		assertEquals("Update Temperature Flag: Q -> U", UNCHECKED_FLAG, 
				record.getTemperatureFlag());
	}
	
	@Test public void temperatureFlagUpdateBaseRecordUncheckedFlagToBadFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException, 
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setTemperature(1.0, CELCIUS);
		
		autoQC.updateTemperatureFlag(record, BAD_FLAG);
		assertEquals("Update Temperature Flag: U -> B", BAD_FLAG, 
				record.getTemperatureFlag());
	}

	@Test 
	public void temperatureFlagUpdateBaseRecordUncheckedFlagToGoodFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setTemperature(1.0, CELCIUS);

		autoQC.updateTemperatureFlag(record, GOOD_FLAG);
		assertEquals("Update Temperature Flag: U -> G", GOOD_FLAG, 
				record.getTemperatureFlag());
	}

	@Test 
	public void temperatureFlagUpdateBaseRecordUncheckedFlagToEstimateFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setTemperature(1.0, CELCIUS);

		autoQC.updateTemperatureFlag(record, ESTIMATE_FLAG);
		assertEquals("Update Temperature Flag: U -> E", ESTIMATE_FLAG, 
				record.getTemperatureFlag());
	}

	@Test 
	public void temperatureFlagUpdateBaseRecordUncheckedFlagToMissingFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setTemperature(1.0, CELCIUS);

		autoQC.updateTemperatureFlag(record, MISSING_FLAG);
		assertEquals("Update Temperature Flag: U -> M", UNCHECKED_FLAG, 
				record.getTemperatureFlag());
	}

	@Test 
	public void temperatureFlagUpdateBaseRecordUncheckedToQuestionableFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setTemperature(1.0, CELCIUS);

		autoQC.updateTemperatureFlag(record, QUESTIONABLE_FLAG);
		assertEquals("Update Temperature Flag: U -> Q", QUESTIONABLE_FLAG, 
				record.getTemperatureFlag());
	}

	@Test 
	public void uComponentFlagUpdateBaseRecordBadFlagToGoodFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setUComponent(1.0, METERS_PER_SECOND);
		record.setUComponentFlag(BAD_FLAG);

		autoQC.updateUComponentFlag(record, GOOD_FLAG);
		assertEquals("Update U Flag: B -> G", BAD_FLAG, 
				record.getUComponentFlag());
	}

	@Test 
	public void uComponentFlagUpdateBaseRecordBadFlagToEstimateFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setUComponent(1.0, METERS_PER_SECOND);
		record.setUComponentFlag(BAD_FLAG);

		autoQC.updateUComponentFlag(record, ESTIMATE_FLAG);
		assertEquals("Update U Flag: B -> E", BAD_FLAG, 
				record.getUComponentFlag());
	}

	@Test 
	public void uComponentFlagUpdateBaseRecordBadFlagToMissingFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setUComponent(1.0, METERS_PER_SECOND);
		record.setUComponentFlag(BAD_FLAG);

		autoQC.updateUComponentFlag(record, MISSING_FLAG);
		assertEquals("Update U Flag: B -> M", BAD_FLAG, 
				record.getUComponentFlag());
	}

	@Test public void uComponentFlagUpdateBaseRecordBadFlagToQuestionableFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException, 
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setUComponent(1.0, METERS_PER_SECOND);
		record.setUComponentFlag(BAD_FLAG);
		
		autoQC.updateUComponentFlag(record, QUESTIONABLE_FLAG);
		assertEquals("Update U Flag: B -> Q", BAD_FLAG, 
				record.getUComponentFlag());
	}

	@Test 
	public void uComponentFlagUpdateBaseRecordBadFlagToUncheckedFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setUComponent(1.0, METERS_PER_SECOND);
		record.setUComponentFlag(BAD_FLAG);

		autoQC.updateUComponentFlag(record, UNCHECKED_FLAG);
		assertEquals("Update U Flag: B -> U", UNCHECKED_FLAG, 
				record.getUComponentFlag());
	}
	
	@Test public void uComponentFlagUpdateBaseRecordGoodFlagToBadFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException, 
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setUComponent(1.0, METERS_PER_SECOND);
		record.setUComponentFlag(GOOD_FLAG);
		
		autoQC.updateUComponentFlag(record, BAD_FLAG);
		assertEquals("Update U Flag: G -> B", BAD_FLAG, 
				record.getUComponentFlag());
	}

	@Test 
	public void uComponentFlagUpdateBaseRecordGoodFlagToEstimateFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setUComponent(1.0, METERS_PER_SECOND);
		record.setUComponentFlag(GOOD_FLAG);

		autoQC.updateUComponentFlag(record, ESTIMATE_FLAG);
		assertEquals("Update U Flag: G -> E", ESTIMATE_FLAG, 
				record.getUComponentFlag());
	}

	@Test 
	public void uComponentFlagUpdateBaseRecordGoodFlagToMissingFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setUComponent(1.0, METERS_PER_SECOND);
		record.setUComponentFlag(GOOD_FLAG);

		autoQC.updateUComponentFlag(record, MISSING_FLAG);
		assertEquals("Update U Flag: G -> M", GOOD_FLAG, 
				record.getUComponentFlag());
	}

	@Test 
	public void uComponentFlagUpdateBaseRecordGoodFlagToQuestionableFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setUComponent(1.0, METERS_PER_SECOND);
		record.setUComponentFlag(GOOD_FLAG);

		autoQC.updateUComponentFlag(record, QUESTIONABLE_FLAG);
		assertEquals("Update U Flag: G -> Q", QUESTIONABLE_FLAG, 
				record.getUComponentFlag());
	}

	@Test 
	public void uComponentFlagUpdateBaseRecordGoodFlagToUncheckedFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setUComponent(1.0, METERS_PER_SECOND);
		record.setUComponentFlag(GOOD_FLAG);

		autoQC.updateUComponentFlag(record, UNCHECKED_FLAG);
		assertEquals("Update U Flag: G -> U", UNCHECKED_FLAG, 
				record.getUComponentFlag());
	}

	@Test public void uComponentFlagUpdateBaseRecordEstimateFlagToBadFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException, 
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setUComponent(1.0, METERS_PER_SECOND);
		record.setUComponentFlag(ESTIMATE_FLAG);
		
		autoQC.updateUComponentFlag(record, BAD_FLAG);
		assertEquals("Update U Flag: E -> B", BAD_FLAG, 
				record.getUComponentFlag());
	}

	@Test 
	public void uComponentFlagUpdateBaseRecordEstimateFlagToGoodFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setUComponent(1.0, METERS_PER_SECOND);
		record.setUComponentFlag(ESTIMATE_FLAG);

		autoQC.updateUComponentFlag(record, GOOD_FLAG);
		assertEquals("Update U Flag: E -> G", ESTIMATE_FLAG, 
				record.getUComponentFlag());
	}

	@Test 
	public void uComponentFlagUpdateBaseRecordEstimateFlagToMissingFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setUComponent(1.0, METERS_PER_SECOND);
		record.setUComponentFlag(ESTIMATE_FLAG);

		autoQC.updateUComponentFlag(record, MISSING_FLAG);
		assertEquals("Update U Flag: E -> M", ESTIMATE_FLAG, 
				record.getUComponentFlag());
	}

	@Test 
	public void uComponentFlagUpdateBaseRecordEstimateFlagToQuestionableFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setUComponent(1.0, METERS_PER_SECOND);
		record.setUComponentFlag(ESTIMATE_FLAG);

		autoQC.updateUComponentFlag(record, QUESTIONABLE_FLAG);
		assertEquals("Update U Flag: E -> Q", QUESTIONABLE_FLAG, 
				record.getUComponentFlag());
	}

	@Test 
	public void uComponentFlagUpdateBaseRecordEstimateFlagToUncheckedFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setUComponent(1.0, METERS_PER_SECOND);
		record.setUComponentFlag(ESTIMATE_FLAG);

		autoQC.updateUComponentFlag(record, UNCHECKED_FLAG);
		assertEquals("Update U Flag: E -> U", UNCHECKED_FLAG, 
				record.getUComponentFlag());
	}
	
	@Test public void uComponentFlagUpdateBaseRecordMissingFlagToBadFlag() {
		ESCSoundingRecord record = new ESCSoundingRecord();
		autoQC.updateUComponentFlag(record, BAD_FLAG);
		assertEquals("Update U Flag: M -> B", MISSING_FLAG, 
				record.getUComponentFlag());
	}

	@Test public void uComponentFlagUpdateBaseRecordMissingFlagToGoodFlag() {
		ESCSoundingRecord record = new ESCSoundingRecord();
		autoQC.updateUComponentFlag(record, GOOD_FLAG);
		assertEquals("Update U Flag: M -> G", MISSING_FLAG, 
				record.getUComponentFlag());
	}

	@Test public void uComponentFlagUpdateBaseRecordMissingFlagToEstimateFlag(){
		ESCSoundingRecord record = new ESCSoundingRecord();
		autoQC.updateUComponentFlag(record, ESTIMATE_FLAG);
		assertEquals("Update U Flag: M -> E", MISSING_FLAG, 
				record.getUComponentFlag());
	}

	@Test 
	public void uComponentFlagUpdateBaseRecordMissingFlagToQuestionableFlag() {
		ESCSoundingRecord record = new ESCSoundingRecord();
		autoQC.updateUComponentFlag(record, QUESTIONABLE_FLAG);
		assertEquals("Update U Flag: M -> D", MISSING_FLAG, 
				record.getUComponentFlag());
	}

	@Test 
	public void uComponentFlagUpdateBaseRecordMissingFlagToUncheckedFlag() {
		ESCSoundingRecord record = new ESCSoundingRecord();
		autoQC.updateUComponentFlag(record, UNCHECKED_FLAG);
		assertEquals("Update U Flag: M -> U", MISSING_FLAG, 
				record.getUComponentFlag());
	}

	@Test public void uComponentFlagUpdateBaseRecordQuestionableFlagToBadFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException, 
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setUComponent(1.0, METERS_PER_SECOND);
		record.setUComponentFlag(QUESTIONABLE_FLAG);
		
		autoQC.updateUComponentFlag(record, BAD_FLAG);
		assertEquals("Update U Flag: Q -> B", BAD_FLAG, 
				record.getUComponentFlag());
	}

	@Test 
	public void uComponentFlagUpdateBaseRecordQuestionableFlagToGoodFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setUComponent(1.0, METERS_PER_SECOND);
		record.setUComponentFlag(QUESTIONABLE_FLAG);

		autoQC.updateUComponentFlag(record, GOOD_FLAG);
		assertEquals("Update U Flag: Q -> G", QUESTIONABLE_FLAG, 
				record.getUComponentFlag());
	}

	@Test 
	public void uComponentFlagUpdateBaseRecordQuestionableFlagToEstimateFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setUComponent(1.0, METERS_PER_SECOND);
		record.setUComponentFlag(QUESTIONABLE_FLAG);

		autoQC.updateUComponentFlag(record, ESTIMATE_FLAG);
		assertEquals("Update U Flag: Q -> E", QUESTIONABLE_FLAG, 
				record.getUComponentFlag());
	}

	@Test 
	public void uComponentFlagUpdateBaseRecordQuestionableFlagToMissingFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setUComponent(1.0, METERS_PER_SECOND);
		record.setUComponentFlag(QUESTIONABLE_FLAG);

		autoQC.updateUComponentFlag(record, MISSING_FLAG);
		assertEquals("Update U Flag: Q -> M", QUESTIONABLE_FLAG, 
				record.getUComponentFlag());
	}

	@Test 
	public void uComponentFlagUpdateBaseRecordQuestionableFlagToUncheckedFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setUComponent(1.0, METERS_PER_SECOND);
		record.setUComponentFlag(QUESTIONABLE_FLAG);

		autoQC.updateUComponentFlag(record, UNCHECKED_FLAG);
		assertEquals("Update U Flag: Q -> U", UNCHECKED_FLAG, 
				record.getUComponentFlag());
	}
	
	@Test public void uComponentFlagUpdateBaseRecordUncheckedFlagToBadFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException, 
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setUComponent(1.0, METERS_PER_SECOND);
		
		autoQC.updateUComponentFlag(record, BAD_FLAG);
		assertEquals("Update U Flag: U -> B", BAD_FLAG, 
				record.getUComponentFlag());
	}

	@Test 
	public void uComponentFlagUpdateBaseRecordUncheckedFlagToGoodFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setUComponent(1.0, METERS_PER_SECOND);

		autoQC.updateUComponentFlag(record, GOOD_FLAG);
		assertEquals("Update U Flag: U -> G", GOOD_FLAG, 
				record.getUComponentFlag());
	}

	@Test 
	public void uComponentFlagUpdateBaseRecordUncheckedFlagToEstimateFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setUComponent(1.0, METERS_PER_SECOND);

		autoQC.updateUComponentFlag(record, ESTIMATE_FLAG);
		assertEquals("Update U Flag: U -> E", ESTIMATE_FLAG, 
				record.getUComponentFlag());
	}

	@Test 
	public void uComponentFlagUpdateBaseRecordUncheckedFlagToMissingFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setUComponent(1.0, METERS_PER_SECOND);

		autoQC.updateUComponentFlag(record, MISSING_FLAG);
		assertEquals("Update U Flag: U -> M", UNCHECKED_FLAG, 
				record.getUComponentFlag());
	}

	@Test 
	public void uComponentFlagUpdateBaseRecordUncheckedToQuestionableFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setUComponent(1.0, METERS_PER_SECOND);

		autoQC.updateUComponentFlag(record, QUESTIONABLE_FLAG);
		assertEquals("Update U Flag: U -> Q", QUESTIONABLE_FLAG, 
				record.getUComponentFlag());
	}

	@Test 
	public void vComponentFlagUpdateBaseRecordBadFlagToGoodFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setVComponent(1.0, METERS_PER_SECOND);
		record.setVComponentFlag(BAD_FLAG);

		autoQC.updateVComponentFlag(record, GOOD_FLAG);
		assertEquals("Update V Flag: B -> G", BAD_FLAG, 
				record.getVComponentFlag());
	}

	@Test 
	public void vComponentFlagUpdateBaseRecordBadFlagToEstimateFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setVComponent(1.0, METERS_PER_SECOND);
		record.setVComponentFlag(BAD_FLAG);

		autoQC.updateVComponentFlag(record, ESTIMATE_FLAG);
		assertEquals("Update V Flag: B -> E", BAD_FLAG, 
				record.getVComponentFlag());
	}

	@Test 
	public void vComponentFlagUpdateBaseRecordBadFlagToMissingFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setVComponent(1.0, METERS_PER_SECOND);
		record.setVComponentFlag(BAD_FLAG);

		autoQC.updateVComponentFlag(record, MISSING_FLAG);
		assertEquals("Update V Flag: B -> M", BAD_FLAG, 
				record.getVComponentFlag());
	}

	@Test public void vComponentFlagUpdateBaseRecordBadFlagToQuestionableFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException, 
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setVComponent(1.0, METERS_PER_SECOND);
		record.setVComponentFlag(BAD_FLAG);
		
		autoQC.updateVComponentFlag(record, QUESTIONABLE_FLAG);
		assertEquals("Update V Flag: B -> Q", BAD_FLAG, 
				record.getVComponentFlag());
	}

	@Test 
	public void vComponentFlagUpdateBaseRecordBadFlagToUncheckedFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setVComponent(1.0, METERS_PER_SECOND);
		record.setVComponentFlag(BAD_FLAG);

		autoQC.updateVComponentFlag(record, UNCHECKED_FLAG);
		assertEquals("Update V Flag: B -> U", UNCHECKED_FLAG, 
				record.getVComponentFlag());
	}
	
	@Test public void vComponentFlagUpdateBaseRecordGoodFlagToBadFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException, 
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setVComponent(1.0, METERS_PER_SECOND);
		record.setVComponentFlag(GOOD_FLAG);
		
		autoQC.updateVComponentFlag(record, BAD_FLAG);
		assertEquals("Update V Flag: G -> B", BAD_FLAG, 
				record.getVComponentFlag());
	}

	@Test 
	public void vComponentFlagUpdateBaseRecordGoodFlagToEstimateFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setVComponent(1.0, METERS_PER_SECOND);
		record.setVComponentFlag(GOOD_FLAG);

		autoQC.updateVComponentFlag(record, ESTIMATE_FLAG);
		assertEquals("Update V Flag: G -> E", ESTIMATE_FLAG, 
				record.getVComponentFlag());
	}

	@Test 
	public void vComponentFlagUpdateBaseRecordGoodFlagToMissingFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setVComponent(1.0, METERS_PER_SECOND);
		record.setVComponentFlag(GOOD_FLAG);

		autoQC.updateVComponentFlag(record, MISSING_FLAG);
		assertEquals("Update V Flag: G -> M", GOOD_FLAG, 
				record.getVComponentFlag());
	}

	@Test 
	public void vComponentFlagUpdateBaseRecordGoodFlagToQuestionableFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setVComponent(1.0, METERS_PER_SECOND);
		record.setVComponentFlag(GOOD_FLAG);

		autoQC.updateVComponentFlag(record, QUESTIONABLE_FLAG);
		assertEquals("Update V Flag: G -> Q", QUESTIONABLE_FLAG, 
				record.getVComponentFlag());
	}

	@Test 
	public void vComponentFlagUpdateBaseRecordGoodFlagToUncheckedFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setVComponent(1.0, METERS_PER_SECOND);
		record.setVComponentFlag(GOOD_FLAG);

		autoQC.updateVComponentFlag(record, UNCHECKED_FLAG);
		assertEquals("Update V Flag: G -> U", UNCHECKED_FLAG, 
				record.getVComponentFlag());
	}

	@Test public void vComponentFlagUpdateBaseRecordEstimateFlagToBadFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException, 
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setVComponent(1.0, METERS_PER_SECOND);
		record.setVComponentFlag(ESTIMATE_FLAG);
		
		autoQC.updateVComponentFlag(record, BAD_FLAG);
		assertEquals("Update V Flag: E -> B", BAD_FLAG, 
				record.getVComponentFlag());
	}

	@Test 
	public void vComponentFlagUpdateBaseRecordEstimateFlagToGoodFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setVComponent(1.0, METERS_PER_SECOND);
		record.setVComponentFlag(ESTIMATE_FLAG);

		autoQC.updateVComponentFlag(record, GOOD_FLAG);
		assertEquals("Update V Flag: E -> G", ESTIMATE_FLAG, 
				record.getVComponentFlag());
	}

	@Test 
	public void vComponentFlagUpdateBaseRecordEstimateFlagToMissingFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setVComponent(1.0, METERS_PER_SECOND);
		record.setVComponentFlag(ESTIMATE_FLAG);

		autoQC.updateVComponentFlag(record, MISSING_FLAG);
		assertEquals("Update V Flag: E -> M", ESTIMATE_FLAG, 
				record.getVComponentFlag());
	}

	@Test 
	public void vComponentFlagUpdateBaseRecordEstimateFlagToQuestionableFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setVComponent(1.0, METERS_PER_SECOND);
		record.setVComponentFlag(ESTIMATE_FLAG);

		autoQC.updateVComponentFlag(record, QUESTIONABLE_FLAG);
		assertEquals("Update V Flag: E -> Q", QUESTIONABLE_FLAG, 
				record.getVComponentFlag());
	}

	@Test 
	public void vComponentFlagUpdateBaseRecordEstimateFlagToUncheckedFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setVComponent(1.0, METERS_PER_SECOND);
		record.setVComponentFlag(ESTIMATE_FLAG);

		autoQC.updateVComponentFlag(record, UNCHECKED_FLAG);
		assertEquals("Update V Flag: E -> U", UNCHECKED_FLAG, 
				record.getVComponentFlag());
	}
	
	@Test public void vComponentFlagUpdateBaseRecordMissingFlagToBadFlag() {
		ESCSoundingRecord record = new ESCSoundingRecord();
		autoQC.updateVComponentFlag(record, BAD_FLAG);
		assertEquals("Update V Flag: M -> B", MISSING_FLAG, 
				record.getVComponentFlag());
	}

	@Test public void vComponentFlagUpdateBaseRecordMissingFlagToGoodFlag() {
		ESCSoundingRecord record = new ESCSoundingRecord();
		autoQC.updateVComponentFlag(record, GOOD_FLAG);
		assertEquals("Update V Flag: M -> G", MISSING_FLAG, 
				record.getVComponentFlag());
	}

	@Test public void vComponentFlagUpdateBaseRecordMissingFlagToEstimateFlag(){
		ESCSoundingRecord record = new ESCSoundingRecord();
		autoQC.updateVComponentFlag(record, ESTIMATE_FLAG);
		assertEquals("Update V Flag: M -> E", MISSING_FLAG, 
				record.getVComponentFlag());
	}

	@Test 
	public void vComponentFlagUpdateBaseRecordMissingFlagToQuestionableFlag() {
		ESCSoundingRecord record = new ESCSoundingRecord();
		autoQC.updateVComponentFlag(record, QUESTIONABLE_FLAG);
		assertEquals("Update V Flag: M -> D", MISSING_FLAG, 
				record.getVComponentFlag());
	}

	@Test 
	public void vComponentFlagUpdateBaseRecordMissingFlagToUncheckedFlag() {
		ESCSoundingRecord record = new ESCSoundingRecord();
		autoQC.updateVComponentFlag(record, UNCHECKED_FLAG);
		assertEquals("Update V Flag: M -> U", MISSING_FLAG, 
				record.getVComponentFlag());
	}

	@Test public void vComponentFlagUpdateBaseRecordQuestionableFlagToBadFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException, 
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setVComponent(1.0, METERS_PER_SECOND);
		record.setVComponentFlag(QUESTIONABLE_FLAG);
		
		autoQC.updateVComponentFlag(record, BAD_FLAG);
		assertEquals("Update V Flag: Q -> B", BAD_FLAG, 
				record.getVComponentFlag());
	}

	@Test 
	public void vComponentFlagUpdateBaseRecordQuestionableFlagToGoodFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setVComponent(1.0, METERS_PER_SECOND);
		record.setVComponentFlag(QUESTIONABLE_FLAG);

		autoQC.updateVComponentFlag(record, GOOD_FLAG);
		assertEquals("Update V Flag: Q -> G", QUESTIONABLE_FLAG, 
				record.getVComponentFlag());
	}

	@Test 
	public void vComponentFlagUpdateBaseRecordQuestionableFlagToEstimateFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setVComponent(1.0, METERS_PER_SECOND);
		record.setVComponentFlag(QUESTIONABLE_FLAG);

		autoQC.updateVComponentFlag(record, ESTIMATE_FLAG);
		assertEquals("Update V Flag: Q -> E", QUESTIONABLE_FLAG, 
				record.getVComponentFlag());
	}

	@Test 
	public void vComponentFlagUpdateBaseRecordQuestionableFlagToMissingFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setVComponent(1.0, METERS_PER_SECOND);
		record.setVComponentFlag(QUESTIONABLE_FLAG);

		autoQC.updateVComponentFlag(record, MISSING_FLAG);
		assertEquals("Update V Flag: Q -> M", QUESTIONABLE_FLAG, 
				record.getVComponentFlag());
	}

	@Test 
	public void vComponentFlagUpdateBaseRecordQuestionableFlagToUncheckedFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setVComponent(1.0, METERS_PER_SECOND);
		record.setVComponentFlag(QUESTIONABLE_FLAG);

		autoQC.updateVComponentFlag(record, UNCHECKED_FLAG);
		assertEquals("Update V Flag: Q -> U", UNCHECKED_FLAG, 
				record.getVComponentFlag());
	}
	
	@Test public void vComponentFlagUpdateBaseRecordUncheckedFlagToBadFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException, 
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setVComponent(1.0, METERS_PER_SECOND);
		
		autoQC.updateVComponentFlag(record, BAD_FLAG);
		assertEquals("Update V Flag: U -> B", BAD_FLAG, 
				record.getVComponentFlag());
	}

	@Test 
	public void vComponentFlagUpdateBaseRecordUncheckedFlagToGoodFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setVComponent(1.0, METERS_PER_SECOND);

		autoQC.updateVComponentFlag(record, GOOD_FLAG);
		assertEquals("Update V Flag: U -> G", GOOD_FLAG, 
				record.getVComponentFlag());
	}

	@Test 
	public void vComponentFlagUpdateBaseRecordUncheckedFlagToEstimateFlag() 
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setVComponent(1.0, METERS_PER_SECOND);

		autoQC.updateVComponentFlag(record, ESTIMATE_FLAG);
		assertEquals("Update V Flag: U -> E", ESTIMATE_FLAG, 
				record.getVComponentFlag());
	}

	@Test 
	public void vComponentFlagUpdateBaseRecordUncheckedFlagToMissingFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setVComponent(1.0, METERS_PER_SECOND);

		autoQC.updateVComponentFlag(record, MISSING_FLAG);
		assertEquals("Update V Flag: U -> M", UNCHECKED_FLAG, 
				record.getVComponentFlag());
	}

	@Test 
	public void vComponentFlagUpdateBaseRecordUncheckedToQuestionableFlag()
	throws CalculationWarning, ConversionException, InvalidFlagException,
	InvalidValueWarning {
		ESCSoundingRecord record = new ESCSoundingRecord();
		record.setVComponent(1.0, METERS_PER_SECOND);

		autoQC.updateVComponentFlag(record, QUESTIONABLE_FLAG);
		assertEquals("Update V Flag: U -> Q", QUESTIONABLE_FLAG, 
				record.getVComponentFlag());
	}
}
