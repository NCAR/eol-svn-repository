package dmg.util;

import static dmg.util.HumidityUtils.*;
import static org.junit.Assert.*;

import org.junit.*;

/**
 * <p>The HumidityUtilsTest class is a collection of JUnit tests for the 
 * HumidityUtils class.</p>
 * 
 * @see dmg.util.HumidityUtils
 * 
 * @author Joel Clawson
 */
public class HumidityUtilsTest {
	
	private static final Double THRESHOLD = Math.pow(1, -10);

	@Test public void relativeHumidityCalculationsDewPoint() 
	throws CalculationException {
		assertEquals("Relative Humidity: dew point = -100",0.0001610562,
				calculateRelativeHumidity(15.0, -100.0),THRESHOLD);
		assertEquals("Relative Humidity: dew point = 0",35.8675036478,
				calculateRelativeHumidity(15.0, 0.0),THRESHOLD);
		assertEquals("Relative Humidity: dew point = 15",100.0,
				calculateRelativeHumidity(15.0, 15.0),THRESHOLD);
		assertEquals("Relative Humidity: dew point = 100.0",6148.3344204998,
				calculateRelativeHumidity(15.0, 100.0),THRESHOLD);
	}
	
	@Test public void relativeHumidityCalculationsTemperatures() 
	throws CalculationException {
		assertEquals("Relative Humidity: temp = -100",62090125.67756243,
				calculateRelativeHumidity(-100.0, 15.0),THRESHOLD);
		assertEquals("Relative Humidity: temp = 0",278.8039027806,
				calculateRelativeHumidity(0.0,15.0),THRESHOLD);
		assertEquals("Relative Humidity: temp = 15",100.0,
				calculateRelativeHumidity(15.0,15.0),THRESHOLD);
		assertEquals("Relative Humidity: temp = 100",1.6264567468,
				calculateRelativeHumidity(100.0,15.0),THRESHOLD);
	}
		
	@Test (expected = CalculationException.class)
	public void relativeHumidityNullDewPoint() throws CalculationException {
		calculateRelativeHumidity(20.0, null);
	}
	
	@Test (expected = CalculationException.class)
	public void relativeHumidityNullTemperature() throws CalculationException {
		calculateRelativeHumidity(null, 20.0);
	}
	
	@Test (expected = CalculationException.class)
	public void relativeHumidityVaporPressureDewPoint() 
	throws CalculationException {
		calculateRelativeHumidity(20.0, -243.5);
	}

	@Test (expected = CalculationException.class)
	public void relativeHumidityVaporPressureTemperature() 
	throws CalculationException {
		calculateRelativeHumidity(-243.5, 20.0);
	}
	
	@Test public void specificHumidityCalculations() 
	throws CalculationException {
		assertEquals("Specific Humidity",.0179093346,
				calculateSpecificHumidity(986.0,23.0),THRESHOLD);
	}
	
	@Test (expected = CalculationException.class)
	public void specificHumidityNullDewPoint() throws CalculationException {
		calculateSpecificHumidity(989.0, null);
	}
	
	@Test (expected = CalculationException.class)
	public void specificHumidityNullPressure() throws CalculationException {
		calculateSpecificHumidity(null, 23.0);
	}
	
	@Test (expected = CalculationException.class)
	public void specificHumidityViolateVaporPressure() 
	throws CalculationException {
		calculateSpecificHumidity(986.0, -243.5);
	}
}
