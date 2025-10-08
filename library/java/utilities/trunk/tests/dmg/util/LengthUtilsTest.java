package dmg.util;

import static dmg.util.LengthUtils.*;
import static org.junit.Assert.*;

import org.junit.*;

/**
 * <p>The LengthUtilsTest class is a collection of JUnit tests for the 
 * LengthUtils class.</p>
 * 
 * @see dmg.util.LengthUtils
 * 
 * @author Joel Clawson
 */
public class LengthUtilsTest {

	public static final Double THRESHOLD = Math.pow(1, -10);

	@Test public void altitudeCalculations() throws CalculationException {
		assertEquals("Altitude: 2 valid dew points",6338.2828767073,
				calculateAltitude(480.2,-7.9,6252.0,474.9,-8.6),
				THRESHOLD);
	}

	@Test (expected = CalculationException.class)
	public void altitudeNullCurrentPressure() throws CalculationException {
		calculateAltitude(800.0, 15.0, 100.0, null, 15.0);
	}

	@Test (expected = CalculationException.class)
	public void altitudeNullCurrentTemperature() throws CalculationException {
		calculateAltitude(900.0, 15.0, 100.0, 800.0, null);
	}

	@Test (expected = CalculationException.class)
	public void altitudeNullPreviousAltitude() throws CalculationException {
		calculateAltitude(900.0, 15.0, null, 800.0, 15.0);
	}

	@Test (expected = CalculationException.class)
	public void altitudeNullPreviousPressure() throws CalculationException {
		calculateAltitude(null, 15.0, 100.0, 800.0, 15.0);
	}

	@Test (expected = CalculationException.class)
	public void altitudeNullPreviousTemperature() throws CalculationException {
		calculateAltitude(900.0, null, 100.0, 800.0, 15.0);
	}

	@Test public void centimeterConversions() throws ConversionException {	
		assertEquals("cm -> cm",100.0,convertLength(100.0, 
				CENTIMETERS, CENTIMETERS), THRESHOLD);
		assertEquals("cm -> dm",10.0,convertLength(100.0, 
				CENTIMETERS, DECIMETERS), THRESHOLD);
		assertEquals("cm -> ft",3.280839895, convertLength(100.0, 
				CENTIMETERS, FEET), THRESHOLD);
		assertEquals("cm -> hin",3937.0078740157, convertLength(100.0, 
				CENTIMETERS, HUNDREDTHS_INCH), THRESHOLD);
		assertEquals("cm -> in",39.3700787402, convertLength(100.0, 
				CENTIMETERS, INCHES), THRESHOLD);
		assertEquals("cm -> km",.001, convertLength(100.0, 
				CENTIMETERS, KILOMETERS), THRESHOLD);
		assertEquals("cm -> m",1.0, convertLength(100.0, 
				CENTIMETERS, METERS), THRESHOLD);
		assertEquals("cm -> mile",.0006213712, convertLength(100.0, 
				CENTIMETERS, MILES), THRESHOLD);
		assertEquals("cm -> mm",1000.0, convertLength(100.0, 
				CENTIMETERS, MILLIMETERS), THRESHOLD);
		assertEquals("cm -> nmile",.0005399568, convertLength(100.0, 
				CENTIMETERS, NAUTICAL_MILES), THRESHOLD);
		assertEquals("cm -> tin",393.7007874016, convertLength(100.0, 
				CENTIMETERS, TENTHS_INCH), THRESHOLD);
		assertEquals("cm -> yd",1.0936132983, convertLength(100.0, 
				CENTIMETERS, YARDS), THRESHOLD);
	}

	@Test (expected = ConversionException.class)
	public void convertLengthNullInputUnit() throws ConversionException {
		convertLength(10.0, null, METERS);
	}

	@Test (expected = ConversionException.class)
	public void convertLengthNullOutputUnits() throws ConversionException { 
		convertLength(10.0, METERS, null);
	}

	@Test public void convertLengthNullValue() throws ConversionException {
		assertNull(convertLength(null, METERS, CENTIMETERS));
	}

	@Test public void decimeterConversions() throws ConversionException {
		assertEquals("dm -> cm",100.0,convertLength(10.0, 
				DECIMETERS, CENTIMETERS), THRESHOLD);
		assertEquals("dm -> dm",10.0,convertLength(10.0, 
				DECIMETERS, DECIMETERS), THRESHOLD);
		assertEquals("dm -> ft",3.2808398950, convertLength(10.0, 
				DECIMETERS, FEET), THRESHOLD);
		assertEquals("dm -> hin",3937.0078740157, convertLength(10.0,
				DECIMETERS, HUNDREDTHS_INCH), THRESHOLD);
		assertEquals("dm -> in",39.3700787402, convertLength(10.0,
				DECIMETERS, INCHES), THRESHOLD);
		assertEquals("dm -> km",.001, convertLength(10.0, 
				DECIMETERS, KILOMETERS), THRESHOLD);
		assertEquals("dm -> m",1.0, convertLength(10.0,
				DECIMETERS, METERS), THRESHOLD);
		assertEquals("dm -> mile",.0006213712,convertLength(10.0, 
				DECIMETERS, MILES), THRESHOLD);
		assertEquals("dm -> mm",1000.0,convertLength(10.0,
				DECIMETERS, MILLIMETERS), THRESHOLD);
		assertEquals("dm -> nmile",.0005399568,convertLength(10.0,
				DECIMETERS, NAUTICAL_MILES), THRESHOLD);
		assertEquals("dm -> tin",393.7007874016,convertLength(10.0, 
				DECIMETERS, TENTHS_INCH), THRESHOLD);
		assertEquals("dm -> yd",1.0936132983,convertLength(10.0,
				DECIMETERS, YARDS), THRESHOLD);
	}

	@Test public void footConversions() throws ConversionException {
		assertEquals("ft -> cm",304.8, convertLength(10.0, 
				FEET, CENTIMETERS), THRESHOLD);
		assertEquals("ft -> dm",30.480, convertLength(10.0,
				FEET, DECIMETERS), THRESHOLD);
		assertEquals("ft -> ft",10.0, convertLength(10.0,
				FEET, FEET), THRESHOLD);
		assertEquals("ft -> hin",12000.0, convertLength(10.0, 
				FEET, HUNDREDTHS_INCH), THRESHOLD);
		assertEquals("ft -> in", 120.0, convertLength(10.0, 
				FEET, INCHES), THRESHOLD);
		assertEquals("ft -> km", .003048, convertLength(10.0, 
				FEET, KILOMETERS), THRESHOLD);
		assertEquals("ft -> m", 3.048, convertLength(10.0, 
				FEET, METERS), THRESHOLD);
		assertEquals("ft -> mile", .0018939394, convertLength(10.0,
				FEET, MILES), THRESHOLD);
		assertEquals("ft -> mm", 3048.0, convertLength(10.0, 
				FEET, MILLIMETERS), THRESHOLD);
		assertEquals("ft -> nmile", .0016457883, convertLength(10.0,
				FEET, NAUTICAL_MILES), THRESHOLD);
		assertEquals("ft -> tin", 1200.0, convertLength(10.0, 
				FEET, TENTHS_INCH), THRESHOLD);
		assertEquals("ft -> yd", 10.0, convertLength(30.0, 
				FEET, YARDS), THRESHOLD);
	}

	@Test public void hundredthOfAnInchConversions() 
	throws ConversionException {
		assertEquals("hin -> cm", 91.44, convertLength(3600.0, 
				HUNDREDTHS_INCH, CENTIMETERS), THRESHOLD);
		assertEquals("hin -> dm", 9.144, convertLength(3600.0, 
				HUNDREDTHS_INCH, DECIMETERS), THRESHOLD);
		assertEquals("hin -> ft", 3.0, convertLength(3600.0, 
				HUNDREDTHS_INCH, FEET), THRESHOLD);
		assertEquals("hin -> hin", 3600.0, convertLength(3600.0, 
				HUNDREDTHS_INCH, HUNDREDTHS_INCH), THRESHOLD);
		assertEquals("hin -> in", 36.0, convertLength(3600.0, 
				HUNDREDTHS_INCH, INCHES), THRESHOLD);
		assertEquals("hin -> km", .0009144, convertLength(3600.0, 
				HUNDREDTHS_INCH, KILOMETERS), THRESHOLD);
		assertEquals("hin -> m", .9144, convertLength(3600.0,
				HUNDREDTHS_INCH, METERS), THRESHOLD);
		assertEquals("hin -> mile", .0005681818, convertLength(3600.0, 
				HUNDREDTHS_INCH, MILES), THRESHOLD);
		assertEquals("hin -> mm", 914.4, convertLength(3600.0, 
				HUNDREDTHS_INCH, MILLIMETERS), THRESHOLD);
		assertEquals("hin -> nmile", .0004937365, convertLength(3600.0,
				HUNDREDTHS_INCH, NAUTICAL_MILES), THRESHOLD);
		assertEquals("hin -> tin", 360.0, convertLength(3600.0, 
				HUNDREDTHS_INCH, TENTHS_INCH), THRESHOLD);
		assertEquals("hin -> yd", 1.0, convertLength(3600.0, 
				HUNDREDTHS_INCH, YARDS), THRESHOLD);
	}

	@Test public void inchConversions() throws ConversionException {
		assertEquals("in -> cm", 91.44, convertLength(36.0, 
				INCHES, CENTIMETERS), THRESHOLD);
		assertEquals("in -> dm", 9.144, convertLength(36.0,
				INCHES, DECIMETERS), THRESHOLD);
		assertEquals("in -> ft", 3.0, convertLength(36.0,
				INCHES, FEET), THRESHOLD);
		assertEquals("in -> hin", 3600.0, convertLength(36.0,
				INCHES, HUNDREDTHS_INCH), THRESHOLD);
		assertEquals("in -> in", 36.0, convertLength(36.0, 
				INCHES, INCHES), THRESHOLD);
		assertEquals("in -> km", .0009144, convertLength(36.0, 
				INCHES, KILOMETERS), THRESHOLD);
		assertEquals("in -> m", .9144, convertLength(36.0, 
				INCHES, METERS), THRESHOLD);
		assertEquals("in -> mile", .0005681818, convertLength(36.0, 
				INCHES, MILES), THRESHOLD);
		assertEquals("in -> mm", 914.4, convertLength(36.0, 
				INCHES, MILLIMETERS), THRESHOLD);
		assertEquals("in -> nmile", .0004937365, convertLength(36.0, 
				INCHES, NAUTICAL_MILES), THRESHOLD);
		assertEquals("in -> tin", 360.0, convertLength(36.0, 
				INCHES, TENTHS_INCH), THRESHOLD);
		assertEquals("in -> yd", 1.0, convertLength(36.0, 
				INCHES, YARDS), THRESHOLD);
	}

	@Test public void kilometerConversions() throws ConversionException {
		assertEquals("km -> cm",1000000.0,convertLength(10.0, 
				KILOMETERS, CENTIMETERS), THRESHOLD);
		assertEquals("km -> dm",100000.0,convertLength(10.0, 
				KILOMETERS, DECIMETERS), THRESHOLD);
		assertEquals("km -> ft",32808.3989501312,convertLength(10.0,
				KILOMETERS, FEET), THRESHOLD);
		assertEquals("km -> hin",39370078.74015748,convertLength(10.0, 
				KILOMETERS, HUNDREDTHS_INCH), THRESHOLD);
		assertEquals("km -> in",393700.7874015748,convertLength(10.0,
				KILOMETERS, INCHES), THRESHOLD);
		assertEquals("km -> km",10.0,convertLength(10.0, 
				KILOMETERS, KILOMETERS), THRESHOLD);
		assertEquals("km -> m",10000.0,convertLength(10.0,
				KILOMETERS,METERS), THRESHOLD);
		assertEquals("km -> mile",6.2137119224,convertLength(10.0, 
				KILOMETERS, MILES), THRESHOLD);
		assertEquals("km -> mm",10000000.0,convertLength(10.0, 
				KILOMETERS, MILLIMETERS), THRESHOLD);
		assertEquals("km -> nmile",5.3995680346,convertLength(10.0, 
				KILOMETERS, NAUTICAL_MILES), THRESHOLD);
		assertEquals("km -> tin",3937007.8740157476,convertLength(10.0, 
				KILOMETERS, TENTHS_INCH), THRESHOLD);
		assertEquals("km -> yd",10936.1329833771,convertLength(10.0, 
				KILOMETERS, YARDS), THRESHOLD);
	}

	@Test public void meterConversions() throws ConversionException {
		assertEquals("m -> cm",1000.0, convertLength(10.0, 
				METERS, CENTIMETERS), THRESHOLD);
		assertEquals("m -> dm",100.0, convertLength(10.0, 
				METERS, DECIMETERS), THRESHOLD);
		assertEquals("m -> ft",32.8083989501, convertLength(10.0, 
				METERS, FEET), THRESHOLD);
		assertEquals("m -> hin",39370.0787401575, convertLength(10.0, 
				METERS, HUNDREDTHS_INCH), THRESHOLD);
		assertEquals("m -> in",393.7007874016, convertLength(10.0,
				METERS, INCHES), THRESHOLD);
		assertEquals("m -> km",.01, convertLength(10.0, 
				METERS, KILOMETERS), THRESHOLD);
		assertEquals("m -> m",10.0, convertLength(10.0, 
				METERS, METERS), THRESHOLD);
		assertEquals("m -> mile", .0062137119, convertLength(10.0, 
				METERS, MILES), THRESHOLD);
		assertEquals("m -> mm", 10000.0, convertLength(10.0, 
				METERS, MILLIMETERS), THRESHOLD);
		assertEquals("m -> nmile", .0053995680, convertLength(10.0, 
				METERS, NAUTICAL_MILES), THRESHOLD);
		assertEquals("m -> tin", 3937.0078740157, convertLength(10.0, 
				METERS, TENTHS_INCH), THRESHOLD);
		assertEquals("m -> yd", 10.9361329834, convertLength(10.0, 
				METERS, YARDS), THRESHOLD);
	}

	@Test public void mileConversions() throws ConversionException {
		assertEquals("mile -> cm",1609344.0, convertLength(10.0,
				MILES, CENTIMETERS), THRESHOLD);
		assertEquals("mile -> dm",160934.4, convertLength(10.0, 
				MILES, DECIMETERS), THRESHOLD);
		assertEquals("mile -> ft",52800.0, convertLength(10.0, 
				MILES, FEET), THRESHOLD);
		assertEquals("mile -> hin",63360000.0, convertLength(10.0,
				MILES, HUNDREDTHS_INCH), THRESHOLD);
		assertEquals("mile -> in",633600.0, convertLength(10.0,
				MILES, INCHES), THRESHOLD);
		assertEquals("mile -> km",16.09344, convertLength(10.0,
				MILES, KILOMETERS), THRESHOLD);
		assertEquals("mile -> m",16093.44, convertLength(10.0, 
				MILES, METERS), THRESHOLD);
		assertEquals("mile -> mile",10.0, convertLength(10.0, 
				MILES, MILES), THRESHOLD);
		assertEquals("mile -> mm", 16093440.0, convertLength(10.0, 
				MILES, MILLIMETERS), THRESHOLD);
		assertEquals("mile -> nmile", 8.6897624190, convertLength(10.0,
				MILES, NAUTICAL_MILES), THRESHOLD);
		assertEquals("mile -> tin", 6336000.0, convertLength(10.0, 
				MILES, TENTHS_INCH), THRESHOLD);
		assertEquals("mile -> yd", 17600.0, convertLength(10.0, 
				MILES, YARDS), THRESHOLD);
	}

	@Test public void millimeterConversions() throws ConversionException {
		assertEquals("mm -> cm",100.0, convertLength(1000.0, 
				MILLIMETERS, CENTIMETERS), THRESHOLD);
		assertEquals("mm -> dm",10.0, convertLength(1000.0, 
				MILLIMETERS, DECIMETERS), THRESHOLD);
		assertEquals("mm -> ft",3.2808398950, convertLength(1000.0, 
				MILLIMETERS, FEET), THRESHOLD);
		assertEquals("mm -> hin",3937.0078740157, convertLength(1000.0, 
				MILLIMETERS, HUNDREDTHS_INCH), THRESHOLD);
		assertEquals("mm -> in",39.3700787402, convertLength(1000.0,
				MILLIMETERS, INCHES), THRESHOLD);
		assertEquals("mm -> km",.001, convertLength(1000.0, 
				MILLIMETERS, KILOMETERS), THRESHOLD);
		assertEquals("mm -> m",1.0, convertLength(1000.0, 
				MILLIMETERS, METERS), THRESHOLD);
		assertEquals("mm -> mile", .0006213712, convertLength(1000.0, 
				MILLIMETERS, MILES), THRESHOLD);
		assertEquals("mm -> mm", 1000.0, convertLength(1000.0, 
				MILLIMETERS, MILLIMETERS), THRESHOLD);
		assertEquals("mm -> nmile", .0005399568, convertLength(1000.0, 
				MILLIMETERS, NAUTICAL_MILES), THRESHOLD);
		assertEquals("mm -> tin", 393.7007874016, convertLength(1000.0,
				MILLIMETERS, TENTHS_INCH), THRESHOLD);
		assertEquals("mm -> yd", 1.0936132983, convertLength(1000.0,
				MILLIMETERS, YARDS), THRESHOLD);
	}

	@Test public void nauticalMileConversions() throws ConversionException {
		assertEquals("nmile -> cm",1852000.0, convertLength(10.0, 
				NAUTICAL_MILES, CENTIMETERS), THRESHOLD);
		assertEquals("nmile -> dm",185200.0, convertLength(10.0,
				NAUTICAL_MILES, DECIMETERS), THRESHOLD);
		assertEquals("nmile -> ft",60761.154855643, convertLength(10.0,
				NAUTICAL_MILES, FEET), THRESHOLD);
		assertEquals("nmile -> hin",72913385.82677166, convertLength(10.0,
				NAUTICAL_MILES, HUNDREDTHS_INCH), THRESHOLD);
		assertEquals("nmile -> in",729133.8582677166, convertLength(10.0, 
				NAUTICAL_MILES, INCHES), THRESHOLD);
		assertEquals("nmile -> km",18.52, convertLength(10.0, 
				NAUTICAL_MILES, KILOMETERS), THRESHOLD);
		assertEquals("nmile -> m",18520.0, convertLength(10.0, 
				NAUTICAL_MILES, METERS), THRESHOLD);
		assertEquals("nmile -> mile", 11.5077944802, convertLength(10.0, 
				NAUTICAL_MILES, MILES), THRESHOLD);
		assertEquals("nmile -> mm", 18520000.0, convertLength(10.0, 
				NAUTICAL_MILES, MILLIMETERS), THRESHOLD);
		assertEquals("nmile -> nmile", 10.0, convertLength(10.0, 
				NAUTICAL_MILES, NAUTICAL_MILES), THRESHOLD);
		assertEquals("nmile -> tin", 7291338.582677165, convertLength(10.0, 
				NAUTICAL_MILES, TENTHS_INCH), THRESHOLD);
		assertEquals("nmile -> yd", 20253.7182852144, convertLength(10.0, 
				NAUTICAL_MILES, YARDS), THRESHOLD);
	}

	@Test public void tenthOfAnInchConversions() throws ConversionException {
		assertEquals("tin -> cm", 91.44, convertLength(360.0, 
				TENTHS_INCH, CENTIMETERS), THRESHOLD);
		assertEquals("tin -> dm", 9.144, convertLength(360.0, 
				TENTHS_INCH, DECIMETERS), THRESHOLD);
		assertEquals("tin -> ft", 3.0, convertLength(360.0, 
				TENTHS_INCH, FEET), THRESHOLD);
		assertEquals("tin -> hin", 3600.0, convertLength(360.0, 
				TENTHS_INCH, HUNDREDTHS_INCH), THRESHOLD);
		assertEquals("tin -> in", 36.0, convertLength(360.0,
				TENTHS_INCH, INCHES), THRESHOLD);
		assertEquals("tin -> km", .0009144, convertLength(360.0,
				TENTHS_INCH, KILOMETERS), THRESHOLD);
		assertEquals("tin -> m", .9144, convertLength(360.0, 
				TENTHS_INCH, METERS), THRESHOLD);
		assertEquals("tin -> mile", .0005681818, convertLength(360.0,
				TENTHS_INCH, MILES), THRESHOLD);
		assertEquals("tin -> mm", 914.4, convertLength(360.0,
				TENTHS_INCH, MILLIMETERS), THRESHOLD);
		assertEquals("tin -> nmile", .0004937365, convertLength(360.0,
				TENTHS_INCH, NAUTICAL_MILES), THRESHOLD);
		assertEquals("tin -> tin", 360.0, convertLength(360.0,
				TENTHS_INCH, TENTHS_INCH), THRESHOLD);
		assertEquals("tin -> yd", 1.0, convertLength(360.0,
				TENTHS_INCH, YARDS), THRESHOLD);
	}

	@Test public void yardConversions() throws ConversionException {
		assertEquals("yd -> cm",914.4, convertLength(10.0, 
				YARDS, CENTIMETERS), THRESHOLD);
		assertEquals("yd -> dm",91.44, convertLength(10.0, 
				YARDS, DECIMETERS), THRESHOLD);
		assertEquals("yd -> ft",30.0, convertLength(10.0, 
				YARDS, FEET), THRESHOLD);
		assertEquals("yd -> hin",36000.0, convertLength(10.0, 
				YARDS, HUNDREDTHS_INCH), THRESHOLD);
		assertEquals("yd -> in",360.0, convertLength(10.0,
				YARDS, INCHES), THRESHOLD);
		assertEquals("yd -> km",.009144, convertLength(10.0,
				YARDS, KILOMETERS), THRESHOLD);
		assertEquals("yd -> m",9.144, convertLength(10.0,
				YARDS, METERS), THRESHOLD);
		assertEquals("yd -> mile", .0056818182, convertLength(10.0, 
				YARDS, MILES), THRESHOLD);
		assertEquals("yd -> mm", 9144.0, convertLength(10.0, 
				YARDS, MILLIMETERS), THRESHOLD);
		assertEquals("yd -> nmile", .0049373650, convertLength(10.0, 
				YARDS, NAUTICAL_MILES), THRESHOLD);
		assertEquals("yd -> tin", 3600.0, convertLength(10.0, 
				YARDS, TENTHS_INCH), THRESHOLD);
		assertEquals("yd -> yd", 10.0, convertLength(10.0,
				YARDS, YARDS), THRESHOLD);
	}
}
