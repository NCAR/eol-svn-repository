package dmg.ua.sounding.autoqc;

import static dmg.ua.sounding.esc.ESCSoundingRecord.QUESTIONABLE_FLAG;
import static dmg.ua.sounding.esc.ESCSoundingRecord.UNCHECKED_FLAG;
import static org.junit.Assert.*;

import java.io.*;

import org.junit.*;

import dmg.ua.sounding.esc.ESCSoundingRecord;
import dmg.util.CalculationWarning;
import dmg.util.ConversionException;
import dmg.util.InvalidValueWarning;

public class DropsondeESCAutoQCTest extends ESCAutoQCTest {

	@BeforeClass public static void setUpBeforeClass() throws Exception {
		autoQC = new DropsondeESCAutoQC("upsonde_test_limits");
		coldQC = new DropsondeESCAutoQC("upsonde_cold_test_limits");
	}
	
	@Override
	public void canCheckPressureRates() throws ConversionException,
			InvalidValueWarning, IOException {
		assertTrue("Check Press Rates", autoQC.canCheckPressureRates(null));
	}

	@Override
	public void checkTimeNoCurrentNoNextNoPrevTime()
			throws ConversionException, InvalidValueWarning, IOException {
		ESCSoundingRecord start = new ESCSoundingRecord();
		ESCSoundingRecord end = new ESCSoundingRecord();
		
		Double prevTime = autoQC.timeCheck(start, end, null, log);
		
		assertFalse("Time Check - Null/Null/Null: log ready", 
				reader.ready());

		assertNull("Time Check - Null/Null/Null: start time", start.getTime());
		assertNull("Time Check - Null/Null/Null: end time", end.getTime());
		assertNull("Time Check - Null/Null/Null: prev time", prevTime);
	}

	@Override
	public void checkTimeNoCurrentNoNextValidPrevTime()
			throws ConversionException, InvalidValueWarning, IOException {
		ESCSoundingRecord start = new ESCSoundingRecord();
		ESCSoundingRecord end = new ESCSoundingRecord();
		
		Double prevTime = autoQC.timeCheck(start, end, 10.0, log);
		
		assertFalse("Time Check - Null/Null/Value: log ready", 
				reader.ready());

		assertNull("Time Check - Null/Null/Value: start time", start.getTime());
		assertNull("Time Check - Null/Null/Value: end time", end.getTime());
		assertEquals("Time Check - Null/Null/Value: prev time", 10.0, prevTime);
	}

	@Override
	public void checkTimeNoCurrentValidNextNoPrevTime()
			throws ConversionException, InvalidValueWarning, IOException,
			CalculationWarning {
		ESCSoundingRecord start = new ESCSoundingRecord();
		ESCSoundingRecord end = new ESCSoundingRecord();
		end.setTime(5.0);
		
		Double prevTime = autoQC.timeCheck(start, end, null, log);
		
		assertFalse("Time Check - Null/Value/Null: log ready", 
				reader.ready());

		assertNull("Time Check - Null/Value/Null: start time", start.getTime());
		assertEquals("Time Check - Null/Value/Null: end time", 5.0, end.getTime());
		assertEquals("Time Check - Null/Value/Null: prev time", 5.0, prevTime);
	}

	@Override
	public void checkTimeNoCurrentValidNextValidPrevTimeEqual()
			throws ConversionException, InvalidValueWarning, IOException,
			CalculationWarning {
		ESCSoundingRecord start = new ESCSoundingRecord();
		ESCSoundingRecord end = new ESCSoundingRecord();
		end.setTime(5.0);
		
		Double prevTime = autoQC.timeCheck(start, end, 5.0, log);

		assertTrue("Time Check - Null/Value/Value Equal: log ready - pre", 
				reader.ready());
		assertFalse("Time Check - Null/Value/Value Equal: log output", 
				"".equals(reader.readLine()));
		assertFalse("Time Check - Null/Value/Value Equal: log ready - post", 
				reader.ready());

		assertNull("Time Check - Null/Value/Value Equal: start time", start.getTime());
		assertEquals("Time Check - Null/Value/Value Equal: end time", 5.0, end.getTime());
		assertEquals("Time Check - Null/Value/Value Equal: prev time", 5.0, prevTime);
	}

	@Override
	public void checkTimeNoCurrentValidNextValidPrevTimeGreat()
			throws ConversionException, InvalidValueWarning, IOException,
			CalculationWarning {
		ESCSoundingRecord start = new ESCSoundingRecord();
		ESCSoundingRecord end = new ESCSoundingRecord();
		end.setTime(5.0);
		
		Double prevTime = autoQC.timeCheck(start, end, 5.1, log);
		
		assertFalse("Time Check - Null/Value/Value Great: log ready", 
				reader.ready());

		assertNull("Time Check - Null/Value/Value Great: start time", start.getTime());
		assertEquals("Time Check - Null/Value/Value Great: end time", 5.0, end.getTime());
		assertEquals("Time Check - Null/Value/Value Great: prev time", 5.0, prevTime);
	}

	@Override
	public void checkTimeNoCurrentValidNextValidPrevTimeLess()
			throws ConversionException, InvalidValueWarning, IOException,
			CalculationWarning {
		ESCSoundingRecord start = new ESCSoundingRecord();
		ESCSoundingRecord end = new ESCSoundingRecord();
		end.setTime(5.0);
		
		Double prevTime = autoQC.timeCheck(start, end, 4.9, log);
		
		assertTrue("Time Check - Null/Value/Value Less: log ready - pre", 
				reader.ready());
		assertFalse("Time Check - Null/Value/Value Less: log output", 
				"".equals(reader.readLine()));
		assertFalse("Time Check - Null/Value/Value Less: log ready - post", 
				reader.ready());

		assertNull("Time Check - Null/Value/Value Less: start time", start.getTime());
		assertEquals("Time Check - Null/Value/Value Less: end time", 5.0, end.getTime());
		assertEquals("Time Check - Null/Value/Value Less: prev time", 5.0, prevTime);
	}

	@Override
	public void checkTimeValidCurrentNoNextNoPrevTime()
			throws ConversionException, InvalidValueWarning, IOException,
			CalculationWarning {
		ESCSoundingRecord start = new ESCSoundingRecord();
		ESCSoundingRecord end = new ESCSoundingRecord();
		start.setTime(1.0);
		
		Double prevTime = autoQC.timeCheck(start, end, null, log);
		
		assertFalse("Time Check - Value/Null/Null: log ready", 
				reader.ready());

		assertEquals("Time Check - Value/Null/Null: start time", 1.0, start.getTime());
		assertNull("Time Check - Value/Null/Null: end time", end.getTime());
		assertNull("Time Check - Value/Null/Null: prev time", prevTime);
	}

	@Override
	public void checkTimeValidCurrentNoNextValidPrevTime()
			throws ConversionException, InvalidValueWarning, IOException,
			CalculationWarning {
		ESCSoundingRecord start = new ESCSoundingRecord();
		ESCSoundingRecord end = new ESCSoundingRecord();
		start.setTime(1.0);
		
		Double prevTime = autoQC.timeCheck(start, end, 10.0, log);
		
		assertFalse("Time Check - Value/Null/Value: log ready", 
				reader.ready());

		assertEquals("Time Check - Value/Null/Value: start time", 1.0, start.getTime());
		assertNull("Time Check - Value/Null/Value: end time", end.getTime());
		assertEquals("Time Check - Value/Null/Value: prev time", 10.0, prevTime);
	}

	@Override
	public void checkTimeValidCurrentValidNextNoPrevTimeEqual()
			throws ConversionException, InvalidValueWarning, IOException,
			CalculationWarning {
		ESCSoundingRecord start = new ESCSoundingRecord();
		ESCSoundingRecord end = new ESCSoundingRecord();
		start.setTime(1.0);
		end.setTime(1.0);
		
		Double prevTime = autoQC.timeCheck(start, end, null, log);
		
		assertTrue("Time Check - Value/Value/Null Equal: log ready - pre", 
				reader.ready());
		assertFalse("Time Check - Value/Value/Null Equal: log output", 
				"".equals(reader.readLine()));
		assertFalse("Time Check - Value/Value/Null Equal: log ready - post", 
				reader.ready());

		assertEquals("Time Check - Value/Value/Null Equal: start time", 1.0, start.getTime());
		assertEquals("Time Check - Value/Value/Null Equal: end time", 1.0, end.getTime());
		assertEquals("Time Check - Value/Value/Null Equal: prev time", 1.0, prevTime);
	}

	@Override
	public void checkTimeValidCurrentValidNextNoPrevTimeGreat()
			throws ConversionException, InvalidValueWarning, IOException,
			CalculationWarning {
		ESCSoundingRecord start = new ESCSoundingRecord();
		ESCSoundingRecord end = new ESCSoundingRecord();
		start.setTime(0.8);
		end.setTime(0.9);
		
		Double prevTime = autoQC.timeCheck(start, end, null, log);
		
		assertTrue("Time Check - Value/Value/Null Great: log ready - pre", 
				reader.ready());
		assertFalse("Time Check - Value/Value/Null Great: log output", 
				"".equals(reader.readLine()));
		assertFalse("Time Check - Value/Value/Null Great: log ready - post", 
				reader.ready());

		assertEquals("Time Check - Value/Value/Null Great: start time", 0.8, start.getTime());
		assertEquals("Time Check - Value/Value/Null Great: end time", 0.9, end.getTime());
		assertEquals("Time Check - Value/Value/Null Great: prev time", 0.9, prevTime);
	}

	@Override
	public void checkTimeValidCurrentValidNextNoPrevTimeLess()
			throws ConversionException, InvalidValueWarning, IOException,
			CalculationWarning {
		ESCSoundingRecord start = new ESCSoundingRecord();
		ESCSoundingRecord end = new ESCSoundingRecord();
		start.setTime(1.1);
		end.setTime(1.0);
		
		Double prevTime = autoQC.timeCheck(start, end, null, log);
		
		assertFalse("Time Check - Value/Value/Null Great: log ready", 
				reader.ready());

		assertEquals("Time Check - Value/Value/Null Great: start time", 1.1, start.getTime());
		assertEquals("Time Check - Value/Value/Null Great: end time", 1.0, end.getTime());
		assertEquals("Time Check - Value/Value/Null Great: prev time", 1.0, prevTime);
	}

	@Override
	public void checkTimeValidCurrentValidNextValidPrevTimeEqual()
			throws ConversionException, InvalidValueWarning, IOException,
			CalculationWarning {
		ESCSoundingRecord start = new ESCSoundingRecord();
		ESCSoundingRecord end = new ESCSoundingRecord();
		start.setTime(1.0);
		end.setTime(1.0);
		
		Double prevTime = autoQC.timeCheck(start, end, 2.0, log);
		
		assertTrue("Time Check - Value/Value/Value Equal: log ready - pre", 
				reader.ready());
		assertFalse("Time Check - Value/Value/Value Equal: log output", 
				"".equals(reader.readLine()));
		assertFalse("Time Check - Value/Value/Value Equal: log ready - post", 
				reader.ready());

		assertEquals("Time Check - Value/Value/Value Equal: start time", 1.0, start.getTime());
		assertEquals("Time Check - Value/Value/Value Equal: end time", 1.0, end.getTime());
		assertEquals("Time Check - Value/Value/Value Equal: prev time", 1.0, prevTime);
	}

	@Override
	public void checkTimeValidCurrentValidNextValidPrevTimeGreat()
			throws ConversionException, InvalidValueWarning, IOException,
			CalculationWarning {
		ESCSoundingRecord start = new ESCSoundingRecord();
		ESCSoundingRecord end = new ESCSoundingRecord();
		start.setTime(1.0);
		end.setTime(1.1);
		
		Double prevTime = autoQC.timeCheck(start, end, 0.8, log);
		
		assertTrue("Time Check - Value/Value/Value Great: log ready - pre", 
				reader.ready());
		assertFalse("Time Check - Value/Value/Value Great: log output", 
				"".equals(reader.readLine()));
		assertFalse("Time Check - Value/Value/Value Great: log ready - post", 
				reader.ready());

		assertEquals("Time Check - Value/Value/Value Great: start time", 1.0, start.getTime());
		assertEquals("Time Check - Value/Value/Value Great: end time", 1.1, end.getTime());
		assertEquals("Time Check - Value/Value/Value Great: prev time", 1.1, prevTime);
	}

	@Override
	public void checkTimeValidCurrentValidNextValidPrevTimeLess()
			throws ConversionException, InvalidValueWarning, IOException,
			CalculationWarning {
		ESCSoundingRecord start = new ESCSoundingRecord();
		ESCSoundingRecord end = new ESCSoundingRecord();
		start.setTime(1.1);
		end.setTime(1.0);
		
		Double prevTime = autoQC.timeCheck(start, end, 0.5, log);
		
		assertFalse("Time Check - Value/Value/Value Less: log ready", 
				reader.ready());

		assertEquals("Time Check - Value/Value/Value Less: start time", 1.1, start.getTime());
		assertEquals("Time Check - Value/Value/Value Less: end time", 1.0, end.getTime());
		assertEquals("Time Check - Value/Value/Value Less: prev time", 1.0, prevTime);
	}

	@Override
	public void decreasePressureDecreaseOverThreshold()
			throws ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
    	ESCSoundingRecord start = buildPTURecord(100.1, 20.0, 15.0);
		ESCSoundingRecord end = buildPTURecord(100.0, 10.0, 5.0);
		
		boolean pressErr = autoQC.decreasingPressureCheck(start, end, log);
		
		assertFalse("Dec Press - Dec Over Thresh: log ready", 
				reader.ready());
		
		assertEquals("Dec Press - Dec Over Thresh: start press", 100.1,  
				start.getPressure());
		assertEquals("Dec Press - Dec Over Thresh: start temp", 20.0, 
				start.getTemperature());
		assertEquals("Dec Press - Dec Over Thresh: start rh", 15.0, 
				start.getRelativeHumidity());
		assertEquals("Dec Press - Dec Over Thresh: start Press Flag ", 
				UNCHECKED_FLAG, start.getPressureFlag());
		assertEquals("Dec Press - Dec Over Thresh: start Temp Flag ", 
				UNCHECKED_FLAG, start.getTemperatureFlag());
		assertEquals("Dec Press - Dec Over Thresh: start RH Flag ", 
				UNCHECKED_FLAG, start.getRelativeHumidityFlag());

		assertEquals("Dec Press - Dec Over Thresh: end press", 100.0,
				end.getPressure());
		assertEquals("Dec Press - Dec Over Thresh: end temp", 10.0, 
				end.getTemperature());
		assertEquals("Dec Press - Dec Over Thresh: end rh", 5.0, 
				end.getRelativeHumidity());
		assertEquals("Dec Press - Dec Over Thresh: end Press Flag ", 
				UNCHECKED_FLAG, end.getPressureFlag());
		assertEquals("Dec Press - Dec Over Thresh: end Temp Flag ", 
				UNCHECKED_FLAG, end.getTemperatureFlag());
		assertEquals("Dec Press - Dec Over Thresh: end RH Flag ", 
				UNCHECKED_FLAG, end.getRelativeHumidityFlag());    
	
		assertFalse("Dec Press - Dec Over Thresh: press err", pressErr);
	}

	@Override
	public void decreasePressureEqualOverThreshold()
			throws ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
    	ESCSoundingRecord start = buildPTURecord(100.1, 20.0, 15.0);
		ESCSoundingRecord end = buildPTURecord(100.1, 10.0, 5.0);
		
		boolean pressErr = autoQC.decreasingPressureCheck(start, end, log);
		
		assertTrue("Dec Press - Equal Over Thresh: log ready - pre", 
				reader.ready());
		assertFalse("Dec Press - Equal Over Thresh: log output", 
				"".equals(reader.readLine()));
		assertFalse("Dec Press - Equal Over Thresh: log ready - post", 
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
				QUESTIONABLE_FLAG, end.getPressureFlag());
		assertEquals("Dec Press - Equal Over Thresh: end Temp Flag ", 
				QUESTIONABLE_FLAG, end.getTemperatureFlag());
		assertEquals("Dec Press - Equal Over Thresh: end RH Flag ", 
				QUESTIONABLE_FLAG, end.getRelativeHumidityFlag());    
	
		assertTrue("Dec Press - Equal Over Thresh: press err", pressErr);
	}

	@Override
	public void decreasePressureIncreaseOverThreshold()
			throws ConversionException, InvalidValueWarning, IOException {
		// Build the records used for the check.
    	ESCSoundingRecord start = buildPTURecord(100.1, 20.0, 15.0);
		ESCSoundingRecord end = buildPTURecord(100.2, 10.0, 5.0);
		
		boolean pressErr = autoQC.decreasingPressureCheck(start, end, log);
		
		assertTrue("Dec Press - Inc Over Thresh: log ready - pre", 
				reader.ready());
		assertFalse("Dec Press - Inc Over Thresh: log output", 
				"".equals(reader.readLine()));
		assertFalse("Dec Press - Inc Over Thresh: log ready - post", 
				reader.ready());
		
		assertEquals("Dec Press - Inc Over Thresh: start press", 100.1,  
				start.getPressure());
		assertEquals("Dec Press - Inc Over Thresh: start temp", 20.0, 
				start.getTemperature());
		assertEquals("Dec Press - Inc Over Thresh: start rh", 15.0, 
				start.getRelativeHumidity());
		assertEquals("Dec Press - Inc Over Thresh: start Press Flag ", 
				UNCHECKED_FLAG, start.getPressureFlag());
		assertEquals("Dec Press - Inc Over Thresh: start Temp Flag ", 
				UNCHECKED_FLAG, start.getTemperatureFlag());
		assertEquals("Dec Press - Inc Over Thresh: start RH Flag ", 
				UNCHECKED_FLAG, start.getRelativeHumidityFlag());

		assertEquals("Dec Press - Inc Over Thresh: end press", 100.2,
				end.getPressure());
		assertEquals("Dec Press - Inc Over Thresh: end temp", 10.0, 
				end.getTemperature());
		assertEquals("Dec Press - Inc Over Thresh: end rh", 5.0, 
				end.getRelativeHumidity());
		assertEquals("Dec Press - Inc Over Thresh: end Press Flag ", 
				QUESTIONABLE_FLAG, end.getPressureFlag());
		assertEquals("Dec Press - Inc Over Thresh: end Temp Flag ", 
				QUESTIONABLE_FLAG, end.getTemperatureFlag());
		assertEquals("Dec Press - Inc Over Thresh: end RH Flag ", 
				QUESTIONABLE_FLAG, end.getRelativeHumidityFlag());    
	
		assertTrue("Dec Press - Inc Over Thresh: press err", pressErr);
	}

	@Override
	public void decreasingPressureDecrease() throws ConversionException,
			InvalidValueWarning, IOException {
		ESCSoundingRecord current = buildPTURecord(1000.0, null, null);
		ESCSoundingRecord next = buildPTURecord(999.9, null, null);
		
		boolean result = autoQC.isDecreasingPressure(current, next);

		assertTrue("Is Dec Press: decrease", result);
		assertEquals("Is Dec Press: dec - start press", 1000.0,
				current.getPressure());
		assertEquals("Is Dec Press: dec - end press", 999.9,
				next.getPressure());
	}

	@Override
	public void decreasingPressureEqual() throws ConversionException,
			InvalidValueWarning, IOException {
		ESCSoundingRecord current = buildPTURecord(1000.0, null, null);
		ESCSoundingRecord next = buildPTURecord(1000.0, null, null);
		
		boolean result = autoQC.isDecreasingPressure(current, next);

		assertFalse("Is Dec Press: equal", result);
		assertEquals("Is Dec Press: equal - start press", 1000.0,
				current.getPressure());
		assertEquals("Is Dec Press: equal - end press", 1000.0,
				next.getPressure());
	}

	@Override
	public void decreasingPressureIncrease() throws ConversionException,
			InvalidValueWarning, IOException {
		ESCSoundingRecord current = buildPTURecord(1000.0, null, null);
		ESCSoundingRecord next = buildPTURecord(1000.1, null, null);
		
		boolean result = autoQC.isDecreasingPressure(current, next);

		assertFalse("Is Dec Press: increase", result);
		assertEquals("Is Dec Press: inc - start press", 1000.0,
				current.getPressure());
		assertEquals("Is Dec Press: inc - end press", 1000.1,
				next.getPressure());
	}
}
