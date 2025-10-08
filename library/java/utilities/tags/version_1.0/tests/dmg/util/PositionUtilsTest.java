package dmg.util;

import static dmg.util.PositionUtils.*;
import static org.junit.Assert.*;

import dmg.util.PositionUtils.*;

import org.junit.*;

/**
 * <p>The PositionUtilsTest class is a collection of JUnit tests for the 
 * PositionUtils class.</p>
 * 
 * @see dmg.util.PositionUtils
 * 
 * @author Joel Clawson
 */
public class PositionUtilsTest {
	
	private static final Double THRESHOLD = Math.pow(1, -10);
	
	@Test public void testToDegreesLatitudeLowerBoundary() 
	throws ConversionException, InvalidValueWarning {
		assertEquals("90 0 0 S", -90.0, toDegrees(90.0, 0.0, 0.0, SOUTH),
				THRESHOLD);
		assertEquals("-90 0 0 N", -90.0, toDegrees(-90.0, 0.0, 0.0, NORTH),
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void testToDegreesLatitudeLowerBoundaryViolationNorth() 
	throws ConversionException, InvalidValueWarning {
		toDegrees(-90.0, 0.0, 0.1, NORTH);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void testToDegreesLatitudeLowerBoundaryViolationSouth() 
	throws ConversionException, InvalidValueWarning {
		toDegrees(90.0, 0.0, 0.1, SOUTH);
	}
	
	@Test public void testToDegreesLatitudeMinutes() 
	throws ConversionException, InvalidValueWarning {
		// Lower minute boundary tests
		assertEquals("30 0 0 N", 30.0, toDegrees(30.0, 0.0, 0.0, NORTH), 
				THRESHOLD);
		assertEquals("30 0 0 S", -30.0, toDegrees(30.0, 0.0, 0.0, SOUTH), 
				THRESHOLD);
		
		// Upper minute boundary tests
		assertEquals("30 60 0 N", 31.0, toDegrees(30.0, 60.0, 0.0, NORTH), 
				THRESHOLD);
		assertEquals("30 60 0 S", -31.0, toDegrees(30.0, 60.0, 0.0, SOUTH),
				THRESHOLD);
	}
	
	@Test public void testToDegreesLatitudeNegativeDegrees()
	throws ConversionException, InvalidValueWarning {
		assertEquals("-30 29 60 N", -30.5, toDegrees(-30.0, 29.0, 60.0, NORTH),
				THRESHOLD);
		assertEquals("-30 29 60 S", 30.5, toDegrees(-30.0, 29.0, 60.0, SOUTH),
				THRESHOLD);
	}
	
	@Test (expected = ConversionException.class)
	public void testToDegreesLatitudeNegativeMinutes() 
	throws ConversionException, InvalidValueWarning {
		toDegrees(30.0, -0.1, 0.0, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void testToDegreesLatitudeNegativeSeconds() 
	throws ConversionException, InvalidValueWarning {
		toDegrees(30.0, 0.0, -0.1, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void testToDegreesLatitudeNullDegrees() 
	throws ConversionException, InvalidValueWarning {
		toDegrees(null, 0.0, 0.0, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void testToDegreesLatitudeNullDirection() 
	throws ConversionException, InvalidValueWarning {
		toDegrees(30.0, 0.0, 0.0, (LatitudeUnit)null);
	}
	
	@Test (expected = ConversionException.class)
	public void testToDegreesLatitudeNullMinutes() throws ConversionException,
	InvalidValueWarning{
		toDegrees(30.0, null, 0.0, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void testToDegreesLatitudeNullSeconds() throws ConversionException,
	InvalidValueWarning {
		toDegrees(30.0, 0.0, null, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void testToDegreesLatitudeOver60Minutes()
	throws ConversionException, InvalidValueWarning {
		toDegrees(30.0, 60.1, 0.0, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void testToDegreesLatitudeOver60Seconds() 
	throws ConversionException, InvalidValueWarning {
		toDegrees(30.0, 60.0, 60.1, NORTH);
	}
	
	@Test public void testToDegreesLatitudeSeconds() 
	throws ConversionException, InvalidValueWarning {
		// Lower second boundary tests
		assertEquals("30 0 0 N", 30.0, toDegrees(30.0, 0.0, 0.0, NORTH), 
				THRESHOLD);
		assertEquals("30 0 0 S", -30.0, toDegrees(30.0, 0.0, 0.0, SOUTH), 
				THRESHOLD);
		
		// Upper second boundary tests
		assertEquals("30 59 60 N", 31.0, toDegrees(30.0, 59.0, 60.0, NORTH), 
				THRESHOLD);
		assertEquals("30 59 60 S", -31.0, toDegrees(30.0, 59.0, 60.0, SOUTH), 
				THRESHOLD);
	}
	
	@Test public void testToDegreesLatitudeUpperBoundary() 
	throws ConversionException, InvalidValueWarning {
		assertEquals("90 0 0 N", 90.0, toDegrees(90.0, 0.0, 0.0, NORTH),
				THRESHOLD);
		assertEquals("-90 0 0 S", 90.0, toDegrees(-90.0, 0.0, 0.0, SOUTH),
				THRESHOLD);
	}

	@Test (expected = InvalidValueWarning.class)
	public void testToDegreesLatitudeUpperBoundaryViolationNorth() 
	throws ConversionException, InvalidValueWarning {
		toDegrees(90.0, 0.0, 0.1, NORTH);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void testToDegreesLatitudeUpperBoundaryViolationSouth()
	throws ConversionException, InvalidValueWarning {
		toDegrees(-90.0, 0.0, 0.1, SOUTH);
	}
	
	@Test public void testToDegreesLongitudeLowerBoundary() 
	throws ConversionException, InvalidValueWarning {
		assertEquals("180 0 0 W", -180.0, toDegrees(180.0, 0.0, 0.0, WEST),
				THRESHOLD);
		assertEquals("-180 0 0 E", -180.0, toDegrees(-180.0, 0.0, 0.0, EAST), 
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void testToDegreesLongitudeLowerBoundaryViolationEast()
	throws ConversionException, InvalidValueWarning {
		toDegrees(-180.0, 0.0, 0.1, EAST);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void testToDegreesLongitudeLowerBoundaryViolationWest() 
	throws ConversionException, InvalidValueWarning {
		toDegrees(180.0, 0.0, 0.1, WEST);
	}
	
	@Test public void testToDegreesLongitudeMinutes() 
	throws ConversionException, InvalidValueWarning {
		// Lower minute boundary tests
		assertEquals("30 0 0 E", 30.0, toDegrees(30.0, 0.0, 0.0, EAST), 
				THRESHOLD);
		assertEquals("30 0 0 W", -30.0, toDegrees(30.0, 0.0, 0.0, WEST),
				THRESHOLD);
		
		// Upper minute boundary tests
		assertEquals("30 60 0 E", 31.0, toDegrees(30.0, 60.0, 0.0, EAST),
				THRESHOLD);
		assertEquals("30 60 0 W", -31.0, toDegrees(30.0, 60.0, 0.0, WEST), 
				THRESHOLD);
	}
	
	@Test public void testToDegreesLongitudeNegativeDegrees() 
	throws ConversionException, InvalidValueWarning {
		assertEquals("-30 29 60 E", -30.5, toDegrees(-30.0, 29.0, 60.0, EAST),
				THRESHOLD);
		assertEquals("-30 29 60 W", 30.5, toDegrees(-30.0, 29.0, 60.0, WEST),
				THRESHOLD);
	}
	
	@Test (expected = ConversionException.class)
	public void testToDegreesLongitudeNegativeMinutes() 
	throws ConversionException, InvalidValueWarning {
		toDegrees(30.0, -0.1, 0.0, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void testToDegreesLongitudeNegativeSeconds() 
	throws ConversionException, InvalidValueWarning {
		toDegrees(30.0, 0.0, -0.1, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void testToDegreesLongitudeNullDegrees() throws ConversionException,
	InvalidValueWarning {
		toDegrees(null, 0.0, 0.0, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void testToDegreesLongitudeNullDirection() 
	throws ConversionException, InvalidValueWarning {
		toDegrees(30.0, 0.0, 0.0, (LongitudeUnit)null);
	}
	
	@Test (expected = ConversionException.class)
	public void testToDegreesLongitudeNullMinutes() 
	throws ConversionException, InvalidValueWarning {
		toDegrees(30.0, null, 0.0, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void testToDegreesLongitudeNullSeconds() 
	throws ConversionException, InvalidValueWarning {
		toDegrees(30.0, 0.0, null, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void testToDegreesLongitudeOver60Minutes() 
	throws ConversionException, InvalidValueWarning {
		toDegrees(30.0, 60.1, 0.0, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void testToDegreesLongitudeOver60Seconds() 
	throws ConversionException, InvalidValueWarning {
		toDegrees(30.0, 60.0, 60.1, EAST);
	}
	
	@Test public void testToDegreesLongitudeSeconds() 
	throws ConversionException, InvalidValueWarning {
		// Lower second boundary tests
		assertEquals("30 0 0 E", 30.0, toDegrees(30.0, 0.0, 0.0, EAST), 
				THRESHOLD);
		assertEquals("30 0 0 W", -30.0, toDegrees(30.0, 0.0, 0.0, WEST), 
				THRESHOLD);
		
		// Upper second boundary tests
		assertEquals("30 59 60 E", 31.0, toDegrees(30.0, 59.0, 60.0, EAST), 
				THRESHOLD);
		assertEquals("30 59 60 W", -31.0, toDegrees(30.0, 59.0, 60.0, WEST), 
				THRESHOLD);
	}
	
	@Test public void testToDegreesLongitudeUpperBoundary() 
	throws ConversionException, InvalidValueWarning {
		assertEquals("180 0 0 E", 180.0, toDegrees(180.0, 0.0, 0.0, EAST), 
				THRESHOLD);
		assertEquals("-180 0 0 W", 180.0, toDegrees(-180.0, 0.0, 0.0, WEST), 
				THRESHOLD);
	}

	@Test (expected = InvalidValueWarning.class)
	public void testToDegreesLongitudeUpperBoundaryViolationEast() 
	throws ConversionException, InvalidValueWarning {
		toDegrees(180.0, 0.0, 0.1, EAST);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void testToDegreesLongitudeUpperBoundaryViolationWest()
	throws ConversionException, InvalidValueWarning {
		toDegrees(-180.0, 0.0, 0.1, WEST);
	}

	@Test public void testToDegreesMinutes() {
		Double[] values = toDegreesMinutes(10.0);
		assertEquals("DegMin: 10.0 (deg)", 10.0, values[0], THRESHOLD);
		assertEquals("DegMin: 10.0 (min)", 0.0, values[1], THRESHOLD);
		values = toDegreesMinutes(30.5);
		assertEquals("DegMin: 30.5 (deg)", 30.0, values[0], THRESHOLD);
		assertEquals("DegMin: 30.5 (min)", 30.0, values[1], THRESHOLD);
		values = toDegreesMinutes(100.51);
		assertEquals("DegMin: 100.51 (deg)", 100.0, values[0], THRESHOLD);
		assertEquals("DegMin: 100.51 (min)", 30.6, values[1], THRESHOLD);
		values = toDegreesMinutes(-30.5);
		assertEquals("DegMin: -30.5 (deg)", -30.0, values[0], THRESHOLD);
		assertEquals("DegMin: -30.5 (min)", 30.0, values[1], THRESHOLD);
	}
	
	@Test public void testToDegreesMinutesSeconds() {
		Double[] values = toDegreesMinutesSeconds(10.0);
		assertEquals("DMS: 10.0 (deg)", 10.0, values[0], THRESHOLD);
		assertEquals("DMS: 10.0 (min)",  0.0, values[1], THRESHOLD);
		assertEquals("DMS: 10.0 (sec)",  0.0, values[2], THRESHOLD);
		values = toDegreesMinutesSeconds(30.5);
		assertEquals("DMS: 30.5 (deg)", 30.0, values[0], THRESHOLD);
		assertEquals("DMS: 30.5 (min)", 30.0, values[1], THRESHOLD);
		assertEquals("DMS: 30.5 (sec)",  0.0, values[2], THRESHOLD);
		values = toDegreesMinutesSeconds(100.51);
		assertEquals("DMS: 100.51 (deg)", 100.0, values[0], THRESHOLD);
		assertEquals("DMS: 100.51 (min)",  30.0, values[1], THRESHOLD);
		assertEquals("DMS: 100.51 (sec)",  36.0, values[2], THRESHOLD);
		values = toDegreesMinutesSeconds(-100.51);
		assertEquals("DMS: -100.51 (deg)", -100.0, values[0], THRESHOLD);
		assertEquals("DMS: -100.51 (min)",   30.0, values[1], THRESHOLD);
		assertEquals("DMS: -100.51 (sec)",   36.0, values[2], THRESHOLD);
	}
}
