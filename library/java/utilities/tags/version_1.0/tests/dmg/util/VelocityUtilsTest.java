package dmg.util;

import static dmg.util.VelocityUtils.*;
import static org.junit.Assert.*;

import org.junit.*;

/**
 * <p>The VelocityUtilsTest class is a collection of JUnit tests for the 
 * VelocityUtils class.</p>
 * 
 * @see dmg.util.VelocityUtils
 * 
 * @author Joel Clawson
 */
public class VelocityUtilsTest {
	
	private static final Double THRESHOLD = Math.pow(1, -10);
		
	@Test (expected = ConversionException.class)
	public void convertVelocityNullInputUnit() throws ConversionException {
		convertVelocity(10.0, null, METERS_PER_SECOND);
	}
	
	@Test (expected = ConversionException.class)
	public void convertVelocityNullOutputUnit() throws ConversionException {
		convertVelocity(10.0, METERS_PER_SECOND, null);
	}
	
	@Test public void convertVelocityNullValue() throws ConversionException {
		assertNull("convertVelocity: null value", 
				convertVelocity(null, METERS_PER_SECOND, KNOTS));
	}
	
	@Test public void feetPerSecondConversions() throws ConversionException {
		assertEquals("ft/s -> ft/s",10.0,convertVelocity(10.0,
				FEET_PER_SECOND,FEET_PER_SECOND), THRESHOLD);
		assertEquals("ft/s -> kph",10.9728,convertVelocity(10.0,
				FEET_PER_SECOND,KILOMETERS_PER_HOUR), THRESHOLD);
		assertEquals("ft/s -> knot",5.9248380130,convertVelocity(10.0,
				FEET_PER_SECOND,KNOTS), THRESHOLD);
		assertEquals("ft/s -> m/s",3.048,convertVelocity(10.0,
				FEET_PER_SECOND,METERS_PER_SECOND), THRESHOLD);
		assertEquals("ft/s -> mph",6.8181818182,convertVelocity(10.0,
				FEET_PER_SECOND,MILES_PER_HOUR), THRESHOLD);
	}
	
	@Test public void kilometersPerHourConversions() 
	throws ConversionException {
		assertEquals("kph -> ft/s",9.1134441528,convertVelocity(10.0,
				KILOMETERS_PER_HOUR,FEET_PER_SECOND), THRESHOLD);
		assertEquals("kph -> kph",10.0,convertVelocity(10.0,
				KILOMETERS_PER_HOUR,KILOMETERS_PER_HOUR), THRESHOLD);
		assertEquals("kph -> knot",5.3995680346,convertVelocity(10.0,
				KILOMETERS_PER_HOUR, KNOTS), THRESHOLD);
		assertEquals("kph -> m/s",2.7777777778,convertVelocity(10.0, 
				KILOMETERS_PER_HOUR, METERS_PER_SECOND), THRESHOLD);
		assertEquals("kph -> mph",6.2137119224,convertVelocity(10.0, 
				KILOMETERS_PER_HOUR, MILES_PER_HOUR), THRESHOLD);
	}
	
	@Test public void knotsConversions() throws ConversionException {
		assertEquals("knot -> ft/s",16.878098571,convertVelocity(10.0,
				KNOTS, FEET_PER_SECOND), THRESHOLD);
		assertEquals("knot -> kph",18.52,convertVelocity(10.0,
				KNOTS, KILOMETERS_PER_HOUR), THRESHOLD);
		assertEquals("knot -> knot",10.0,convertVelocity(10.0,
				KNOTS, KNOTS), THRESHOLD);
		assertEquals("knot -> m/s",5.1444444444,convertVelocity(10.0,
				KNOTS, METERS_PER_SECOND), THRESHOLD);
		assertEquals("knot -> mph",11.5077944802,convertVelocity(10.0, 
				KNOTS, MILES_PER_HOUR), THRESHOLD);
	}
	
	@Test public void metersPerSecondConversions() throws ConversionException {
		assertEquals("m/s -> ft/s",32.8083989501,convertVelocity(10.0,
				METERS_PER_SECOND, FEET_PER_SECOND), THRESHOLD);
		assertEquals("m/s -> kph",36.0,convertVelocity(10.0,
				METERS_PER_SECOND, KILOMETERS_PER_HOUR), THRESHOLD);
		assertEquals("m/s -> knot",19.4384449244,convertVelocity(10.0, 
				METERS_PER_SECOND, KNOTS), THRESHOLD);
		assertEquals("m/s -> m/s",10.0,convertVelocity(10.0, 
				METERS_PER_SECOND, METERS_PER_SECOND), THRESHOLD);
		assertEquals("m/s -> mph",22.3693629205,convertVelocity(10.0, 
				METERS_PER_SECOND, MILES_PER_HOUR), THRESHOLD);
	}
	
	@Test public void milesPerHourConversions() throws ConversionException {
		assertEquals("mph -> ft/s",14.6666666667,convertVelocity(10.0, 
				MILES_PER_HOUR, FEET_PER_SECOND), THRESHOLD);
		assertEquals("mph -> kph",16.09344,convertVelocity(10.0, 
				MILES_PER_HOUR, KILOMETERS_PER_HOUR), THRESHOLD);
		assertEquals("mph -> knot",8.6897624190,convertVelocity(10.0,
				MILES_PER_HOUR, KNOTS), THRESHOLD);
		assertEquals("mph -> m/s",4.4704,convertVelocity(10.0,
				MILES_PER_HOUR, METERS_PER_SECOND), THRESHOLD);
		assertEquals("mph -> mph",10.0,convertVelocity(10.0, 
				MILES_PER_HOUR, MILES_PER_HOUR), THRESHOLD);
	}
	
	@Test public void uvWindCalculations() throws CalculationException {
		Double[] uvComponents = new Double[2];

		uvComponents = calculateUVWinds(5.0, 0.0);
		assertEquals("U: spd = 5, dir = 0",0.0,uvComponents[0],THRESHOLD);
		assertEquals("V: spd = 5, dir = 0",-5.0,uvComponents[1],THRESHOLD);
		
		uvComponents = calculateUVWinds(5.0, 90.0);
		assertEquals("U: spd = 5, dir = 90",-5.0,uvComponents[0],THRESHOLD);
		assertEquals("V: spd = 5, dir = 90",0.0,uvComponents[1],THRESHOLD);
		
		uvComponents = calculateUVWinds(5.0, 180.0);
		assertEquals("U: spd = 5, dir = 180",0.0,uvComponents[0],THRESHOLD);
		assertEquals("V: spd = 5, dir = 180",5.0,uvComponents[1],THRESHOLD);
		
		uvComponents = calculateUVWinds(5.0, 270.0);			
		assertEquals("U: spd = 5, dir = 270",5.0,uvComponents[0],THRESHOLD);
		assertEquals("V: spd = 5, dir = 270",0.0,uvComponents[1],THRESHOLD);
		
		uvComponents = calculateUVWinds(5.0, 360.0);
		assertEquals("U: spd = 5, dir = 360",0.0,uvComponents[0],THRESHOLD);
		assertEquals("V: spd = 5, dir = 260",-5.0,uvComponents[1],THRESHOLD);
	}
	
	@Test (expected = CalculationException.class)
	public void uvWindNullDirection() throws CalculationException {
		calculateUVWinds(5.0, null);
	}
	
	@Test (expected = CalculationException.class)
	public void uvWindNullSpeed() throws CalculationException {
		calculateUVWinds(null, 0.0);
	}
	
	@Test (expected = CalculationException.class)
	public void uvWindViolateDirectionLowerBound() throws CalculationException {
		calculateUVWinds(5.0, -0.1);
	}
	
	@Test (expected = CalculationException.class)
	public void uvWindViolateDirectionUpperBound() throws CalculationException {
		calculateUVWinds(5.0, 360.1);
	}
	
	@Test (expected = CalculationException.class)
	public void uvWindViolateSpeedLowerBound() throws CalculationException {
		calculateUVWinds(-0.1, 90.0);
	}
	
	@Test public void windCalculations() throws CalculationException {
		// Positive U Component.
		Double[] winds = calculateWinds(5.0, 5.0);
		assertEquals("Wind Spd: ucomp = 5, vcomp = 5",7.0710678119,winds[0],
				THRESHOLD);
		assertEquals("Wind Dir: ucomp = 5, vcomp = 5",225.0,winds[1],THRESHOLD);			
		winds = calculateWinds(5.0, 0.0);
		assertEquals("Wind Spd: ucomp = 5, vcomp = 0",5.0,winds[0],THRESHOLD);
		assertEquals("Wind Dir: ucomp = 5, vcomp = 0",270.0,winds[1],THRESHOLD);			
		winds = calculateWinds(5.0, -5.0);
		assertEquals("Wind Spd: ucomp = 5, vcomp = -5",7.0710678119,winds[0],
				THRESHOLD);
		assertEquals("Wind Dir: ucomp = 5, vcomp = -5",315.0,winds[1],
				THRESHOLD);

		// Zero U Component
		winds = calculateWinds(0.0, 5.0);
		assertEquals("Wind Spd: ucomp = 0, vcomp = 5",5.0,winds[0],THRESHOLD);
		assertEquals("Wind Dir: ucomp = 0, vcomp = 5",180.0,winds[1],THRESHOLD);			
		winds = calculateWinds(0.0, 0.0);
		assertEquals("Wind Spd: ucomp = 0, vcomp = 0",0.0,winds[0],THRESHOLD);
		assertEquals("Wind Dir: ucomp = 0, vcomp = 0",0.0,winds[1],THRESHOLD);			
		winds = calculateWinds(0.0, -5.0);
		assertEquals("Wind Spd: ucomp = 0, vcomp = -5",5.0,winds[0],THRESHOLD);
		assertEquals("Wind Dir: ucomp = 0, vcomp = -5",0.0,winds[1],THRESHOLD);

		// Negative U Component
		winds = calculateWinds(-5.0, 5.0);
		assertEquals("Wind Spd: ucomp = -5, vcomp = 5",7.0710678119,winds[0],
				THRESHOLD);
		assertEquals("Wind Dir: ucomp = -5, vcomp = 5",135.0,winds[1],
				THRESHOLD);
		winds = calculateWinds(-5.0, 0.0);
		assertEquals("Wind Spd: ucomp = -5, vcomp = 0",5.0,winds[0],THRESHOLD);
		assertEquals("Wind Dir: ucomp = -5, vcomp = 0",90.0,winds[1],THRESHOLD);	
		winds = calculateWinds(-5.0, -5.0);
		assertEquals("Wind Spd: ucomp = -5, vcomp = -5",7.0710678119,winds[0],
				THRESHOLD);
		assertEquals("Wind Dir: ucomp = -5, vcomp = -5",45.0,winds[1],
				THRESHOLD);
	}
	
	@Test (expected = CalculationException.class)
	public void windsNullUComponent() throws CalculationException {
		calculateWinds(null, 5.0);
	}
	
	@Test (expected = CalculationException.class)
	public void windsNullVComponent() throws CalculationException {
		calculateWinds(5.0, null);
	}
}
