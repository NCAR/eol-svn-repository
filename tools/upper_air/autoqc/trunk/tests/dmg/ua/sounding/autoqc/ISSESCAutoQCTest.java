package dmg.ua.sounding.autoqc;

import static dmg.ua.sounding.esc.ESCSoundingRecord.*;
import static org.junit.Assert.*;

import java.io.*;
import org.junit.*;

import dmg.ua.sounding.esc.ESCSoundingRecord;
import dmg.util.ConversionException;
import dmg.util.InvalidValueWarning;

public class ISSESCAutoQCTest extends UpsondeESCAutoQCTest {

	@BeforeClass public static void setUpBeforeClass() throws Exception {
		autoQC = new ISSESCAutoQC("upsonde_test_limits");
		coldQC = new ISSESCAutoQC("upsonde_cold_test_limits");
	}

	@Override
	public void decreasePressureEqualOverThreshold() throws
    ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
    	ESCSoundingRecord start = buildPTURecord(100.1, 20.0, 15.0);
		ESCSoundingRecord end = buildPTURecord(100.1, 10.0, 5.0);
		
		boolean pressErr = autoQC.decreasingPressureCheck(start, end, log);
		
		assertFalse("Dec Press - Equal Over Thresh: log ready", 
				reader.ready());
		
		assertEquals("Dec Press - Equal Over Thresh: start press", 100.1,  
				start.getPressure());
		assertEquals("Dec Press - Equal Over Thresh: start temp", 20.0, 
				start.getTemperature());
		assertEquals("Dec Press - Equal Over Thresh: start rh", 15.0, 
				start.getRelativeHumidity());
		assertEquals("Dec Press - Equal Over Thresh: start Press Flag ", 
				UNCHECKED_FLAG, start.getPressureFlag());
		assertEquals("Dec Press - Equal Over Thresh: start Temp Flag ", 
				UNCHECKED_FLAG, start.getTemperatureFlag());
		assertEquals("Dec Press - Equal Over Thresh: start RH Flag ", 
				UNCHECKED_FLAG, start.getRelativeHumidityFlag());

		assertEquals("Dec Press - Equal Over Thresh: end press", 100.1,
				end.getPressure());
		assertEquals("Dec Press - Equal Over Thresh: end temp", 10.0, 
				end.getTemperature());
		assertEquals("Dec Press - Equal Over Thresh: end rh", 5.0, 
				end.getRelativeHumidity());
		assertEquals("Dec Press - Equal Over Thresh: end Press Flag ", 
				UNCHECKED_FLAG, end.getPressureFlag());
		assertEquals("Dec Press - Equal Over Thresh: end Temp Flag ", 
				UNCHECKED_FLAG, end.getTemperatureFlag());
		assertEquals("Dec Press - Equal Over Thresh: end RH Flag ", 
				UNCHECKED_FLAG, end.getRelativeHumidityFlag());    
	
		assertFalse("Dec Press - Equal Over Thresh: press err", pressErr);
	}
	
	@Override
	public void decreasingPressureEqual() throws ConversionException, InvalidValueWarning, IOException {
		ESCSoundingRecord current = buildPTURecord(1000.0, null, null);
		ESCSoundingRecord next = buildPTURecord(1000.0, null, null);
		
		boolean result = autoQC.isDecreasingPressure(current, next);

		assertTrue("Is Dec Press: equal", result);
		assertEquals("Is Dec Press: equal - start press", 1000.0,
				current.getPressure());
		assertEquals("Is Dec Press: equal - end press", 1000.0,
				next.getPressure());
	}
}
