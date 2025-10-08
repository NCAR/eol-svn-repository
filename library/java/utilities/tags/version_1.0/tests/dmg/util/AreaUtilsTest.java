package dmg.util;

import static dmg.util.AreaUtils.*;
import static org.junit.Assert.*;

import org.junit.*;

public class AreaUtilsTest {

	private static final Double THRESHOLD = Math.pow(1, -10);

	@Test public void acresConversions() throws ConversionException {
		assertEquals("acres -> acres", 1.0, convertArea(1.0, ACRES, ACRES), 
				THRESHOLD);
		assertEquals("acres -> cm2", 40468564.224, convertArea(1.0, ACRES,
				SQUARE_CENTIMETERS), THRESHOLD);
		assertEquals("acres -> ft2", 43560.0, convertArea(1.0, ACRES, 
				SQUARE_FEET), THRESHOLD);
		assertEquals("acres -> in2", 6272640.0, convertArea(1.0, ACRES, 
				SQUARE_INCHES), THRESHOLD * 10.0);
		assertEquals("acres -> km2", .004046856422, convertArea(1.0, ACRES,
				SQUARE_KILOMETERS), THRESHOLD);
		assertEquals("acres -> m2", 4046.8564224, convertArea(1.0, ACRES, 
				SQUARE_METERS), THRESHOLD);
		assertEquals("acres -> mile2", .0015625, convertArea(1.0, ACRES, 
				SQUARE_MILES), THRESHOLD);
		assertEquals("acres -> mm2", 4046856422.4, convertArea(1.0, ACRES, 
				SQUARE_MILLIMETERS), THRESHOLD);
		assertEquals("acres -> yd2", 4840.0, convertArea(1.0, ACRES, 
				SQUARE_YARDS), THRESHOLD);
	}

	@Test (expected = ConversionException.class)
	public void areaConversionNullInputUnit() throws ConversionException {
		convertArea(10.0, null, ACRES);
	}

	@Test (expected = ConversionException.class)
	public void areaConversionNullOutputUnit() throws ConversionException {
		convertArea(10.0, ACRES, null);
	}

	@Test public void areaConversionNullValue() throws ConversionException {
		assertNull("Area: null value", convertArea(null, ACRES, SQUARE_METERS));
	}

	@Test public void squareCentimetersConversions()
	throws ConversionException {
		assertEquals("cm2 -> acres", .00002471054, convertArea(1000.0, 
				SQUARE_CENTIMETERS, ACRES), THRESHOLD);
		assertEquals("cm2 -> cm2", 1000.0, convertArea(1000.0, 
				SQUARE_CENTIMETERS, SQUARE_CENTIMETERS), THRESHOLD);
		assertEquals("cm2 -> ft2", 1.07639104167, convertArea(1000.0, 
				SQUARE_CENTIMETERS, SQUARE_FEET), THRESHOLD);
		assertEquals("cm2 -> in2", 155.00031000062003, convertArea(1000.0,
				SQUARE_CENTIMETERS, SQUARE_INCHES), THRESHOLD);
		assertEquals("cm2 -> km2", .0000001, convertArea(1000.0, 
				SQUARE_CENTIMETERS, SQUARE_KILOMETERS), THRESHOLD);
		assertEquals("cm2 -> m2", .1, convertArea(1000.0, SQUARE_CENTIMETERS, 
				SQUARE_METERS), THRESHOLD);
		assertEquals("cm2 -> mile2", .0000000386102, convertArea(1000.0, 
				SQUARE_CENTIMETERS, SQUARE_MILES), THRESHOLD);
		assertEquals("cm2 -> mm2", 100000.0, convertArea(1000.0, 
				SQUARE_CENTIMETERS, SQUARE_MILLIMETERS), THRESHOLD);
		assertEquals("cm2 -> yd2", .11959900463, convertArea(1000.0, 
				SQUARE_CENTIMETERS, SQUARE_YARDS), THRESHOLD);
	}

	@Test public void squareFeetConversions() throws ConversionException {
		assertEquals("ft2 -> acres", .022956841139, convertArea(1000.0, 
				SQUARE_FEET, ACRES), THRESHOLD);
		assertEquals("ft2 -> cm2", 929.0304, convertArea(1.0, SQUARE_FEET, 
				SQUARE_CENTIMETERS), THRESHOLD);
		assertEquals("ft2 -> ft2", 1.0, convertArea(1.0, SQUARE_FEET, 
				SQUARE_FEET), THRESHOLD);
		assertEquals("ft2 -> in2", 144.0, convertArea(1.0, SQUARE_FEET, 
				SQUARE_INCHES), THRESHOLD);
		assertEquals("ft2 -> km2", .00009290304, convertArea(1000.0,
				SQUARE_FEET, SQUARE_KILOMETERS), THRESHOLD);
		assertEquals("ft2 -> m2", .09290304, convertArea(1.0, SQUARE_FEET, 
				SQUARE_METERS), THRESHOLD);
		assertEquals("ft2 -> mile2", .000035870064, convertArea(1000.0, 
				SQUARE_FEET, SQUARE_MILES), THRESHOLD);
		assertEquals("ft2 -> mm2", 92903.04, convertArea(1.0, SQUARE_FEET, 
				SQUARE_MILLIMETERS), THRESHOLD);
		assertEquals("ft2 -> yd2", .1111111111111, convertArea(1.0, 
				SQUARE_FEET, SQUARE_YARDS), THRESHOLD);
	}

	@Test public void squareInchesConversions() throws ConversionException {
		assertEquals("in2 -> acres", .000159422507907, convertArea(1000.0, 
				SQUARE_INCHES, ACRES), THRESHOLD);
		assertEquals("in2 -> cm2", 6.4516, convertArea(1.0, SQUARE_INCHES, 
				SQUARE_CENTIMETERS), THRESHOLD);
		assertEquals("in2 -> ft2", .006944444444, convertArea(1.0, 
				SQUARE_INCHES, SQUARE_FEET), THRESHOLD);
		assertEquals("in2 -> in2", 1.0, convertArea(1.0, SQUARE_INCHES, 
				SQUARE_INCHES), THRESHOLD);
		assertEquals("in2 -> km2", .00000064516, convertArea(1000.0, 
				SQUARE_INCHES, SQUARE_KILOMETERS), THRESHOLD);
		assertEquals("in2 -> m2", .00064516, convertArea(1.0, SQUARE_INCHES,
				SQUARE_METERS), THRESHOLD);
		assertEquals("in2 -> mile2", .000000249098, convertArea(1000.0, 
				SQUARE_INCHES, SQUARE_MILES), THRESHOLD);
		assertEquals("in2 -> mm2", 645.16, convertArea(1.0, SQUARE_INCHES, 
				SQUARE_MILLIMETERS), THRESHOLD);
		assertEquals("in2 -> yd2", .000771604938, convertArea(1.0, 
				SQUARE_INCHES, SQUARE_YARDS), THRESHOLD);
	}

	@Test public void squareKilometersConversions() throws ConversionException {
		assertEquals("km2 -> acres", 247.10538146716527, convertArea(1.0,
				SQUARE_KILOMETERS, ACRES), THRESHOLD);
		assertEquals("km2 -> cm2", 10000000000.0, convertArea(1.0, 
				SQUARE_KILOMETERS, SQUARE_CENTIMETERS), THRESHOLD);
		assertEquals("km2 -> ft2", 10763910.41670972, convertArea(1.0,
				SQUARE_KILOMETERS, SQUARE_FEET), THRESHOLD);
		assertEquals("km2 -> in2", 1550003100.0062, convertArea(1.0,
				SQUARE_KILOMETERS, SQUARE_INCHES), THRESHOLD);
		assertEquals("km2 -> km2", 1.0, convertArea(1.0, SQUARE_KILOMETERS,
				SQUARE_KILOMETERS), THRESHOLD);
		assertEquals("km2 -> m2", 1000000.0, convertArea(1.0,
				SQUARE_KILOMETERS, SQUARE_METERS), THRESHOLD);
		assertEquals("km2 -> mile2", .386102158542, convertArea(1.0, 
				SQUARE_KILOMETERS, SQUARE_MILES), THRESHOLD);
		assertEquals("km2 -> mm2", 1000000000000.0, convertArea(1.0, 
				SQUARE_KILOMETERS, SQUARE_MILLIMETERS), THRESHOLD);
		assertEquals("km2 -> yd2", 1195990.0463010804, convertArea(1.0, 
				SQUARE_KILOMETERS, SQUARE_YARDS), THRESHOLD);
	}

	@Test public void squareMetersConversions() throws ConversionException {
		assertEquals("m2 -> acres", .000247105381, convertArea(1.0, 
				SQUARE_METERS, ACRES), THRESHOLD);
		assertEquals("m2 -> cm2", 10000.0, convertArea(1.0, SQUARE_METERS, 
				SQUARE_CENTIMETERS), THRESHOLD);
		assertEquals("m2 -> ft2", 10.7639104167, convertArea(1.0, SQUARE_METERS,
				SQUARE_FEET), THRESHOLD);
		assertEquals("m2 -> in2", 1550.0031000062002, convertArea(1.0, 
				SQUARE_METERS, SQUARE_INCHES), THRESHOLD);
		assertEquals("m2 -> km2", .000001, convertArea(1.0, SQUARE_METERS, 
				SQUARE_KILOMETERS), THRESHOLD);
		assertEquals("m2 -> m2", 1.0, convertArea(1.0, SQUARE_METERS, 
				SQUARE_METERS), THRESHOLD);
		assertEquals("m2 -> mile2", .0000003861022, convertArea(1.0, 
				SQUARE_METERS, SQUARE_MILES), THRESHOLD);
		assertEquals("m2 -> mm2", 1000000.0, convertArea(1.0, SQUARE_METERS,
				SQUARE_MILLIMETERS), THRESHOLD);
		assertEquals("m2 -> yd2", 1.1959900463, convertArea(1.0, SQUARE_METERS,
				SQUARE_YARDS), THRESHOLD);
	}

	@Test public void squareMilesConversions() throws ConversionException {
		assertEquals("mile2 -> acres", 640.0, convertArea(1.0, 
				SQUARE_MILES, ACRES), THRESHOLD);
		assertEquals("mile2 -> cm2", 25899881103.36, convertArea(1.0, 
				SQUARE_MILES, SQUARE_CENTIMETERS), THRESHOLD);
		assertEquals("mile2 -> ft2", 27878400.0, convertArea(1.0, 
				SQUARE_MILES, SQUARE_FEET), .00000001);
		assertEquals("mile2 -> in2", 4014489600.0, convertArea(1.0, 
				SQUARE_MILES, SQUARE_INCHES), .000001);
		assertEquals("mile2 -> km2", 2.58998811034, convertArea(1.0, 
				SQUARE_MILES, SQUARE_KILOMETERS), THRESHOLD);
		assertEquals("mile2 -> m2", 2589988.110336, convertArea(1.0, 
				SQUARE_MILES, SQUARE_METERS), THRESHOLD);
		assertEquals("mile2 -> mile2", 1.0, convertArea(1.0, SQUARE_MILES, 
				SQUARE_MILES), THRESHOLD);
		assertEquals("mile2 -> mm2", 2589988110336.0, convertArea(1.0, 
				SQUARE_MILES, SQUARE_MILLIMETERS), THRESHOLD);
		assertEquals("mile2 -> yd2", 3097600.0, convertArea(1.0, SQUARE_MILES, 
				SQUARE_YARDS), .00000001);
	}

	@Test public void squareMillimetersConversions() 
	throws ConversionException {
		assertEquals("mm2 -> acres", .000247105381467, convertArea(1000000.0, 
				SQUARE_MILLIMETERS, ACRES), THRESHOLD);
		assertEquals("mm2 -> cm2", .01, convertArea(1.0, SQUARE_MILLIMETERS, 
				SQUARE_CENTIMETERS), THRESHOLD);
		assertEquals("mm2 -> ft2", .0107639104167, convertArea(1000.0, 
				SQUARE_MILLIMETERS, SQUARE_FEET), THRESHOLD);
		assertEquals("mm2 -> in2", .15500031000062, convertArea(100.0, 
				SQUARE_MILLIMETERS, SQUARE_INCHES), THRESHOLD);
		assertEquals("mm2 -> km2", .000001, convertArea(1000000.0,
				SQUARE_MILLIMETERS, SQUARE_KILOMETERS), THRESHOLD);
		assertEquals("mm2 -> m2", 1.0, convertArea(1000000.0, 
				SQUARE_MILLIMETERS, SQUARE_METERS), THRESHOLD);
		assertEquals("mm2 -> mile2", .000000386102, convertArea(1000000.0, 
				SQUARE_MILLIMETERS, SQUARE_MILES), THRESHOLD);
		assertEquals("mm2 -> mm2", 1.0, convertArea(1.0, SQUARE_MILLIMETERS,
				SQUARE_MILLIMETERS), THRESHOLD);
		assertEquals("mm2 -> yd2", .001195990046301, convertArea(1000.0, 
				SQUARE_MILLIMETERS, SQUARE_YARDS), THRESHOLD);
	}

	@Test public void squareYardsConversions() throws ConversionException {
		assertEquals("yd2 -> acres", .020661157025, convertArea(100.0, 
				SQUARE_YARDS, ACRES), THRESHOLD);
		assertEquals("yd2 -> cm2", 8361.2736, convertArea(1.0, SQUARE_YARDS,
				SQUARE_CENTIMETERS), THRESHOLD);
		assertEquals("yd2 -> ft2", 9.0, convertArea(1.0, SQUARE_YARDS, 
				SQUARE_FEET), THRESHOLD);
		assertEquals("yd2 -> in2", 1296.0, convertArea(1.0, SQUARE_YARDS, 
				SQUARE_INCHES), THRESHOLD);
		assertEquals("yd2 -> km2", .00083612736, convertArea(1000.0, 
				SQUARE_YARDS, SQUARE_KILOMETERS), THRESHOLD);
		assertEquals("yd2 -> m2", .83612736, convertArea(1.0, SQUARE_YARDS, 
				SQUARE_METERS), THRESHOLD);
		assertEquals("yd2 -> mile2", .000322830578512, convertArea(1000.0, 
				SQUARE_YARDS, SQUARE_MILES), THRESHOLD);
		assertEquals("yd2 -> mm2", 836127.36, convertArea(1.0, SQUARE_YARDS, 
				SQUARE_MILLIMETERS), THRESHOLD);
		assertEquals("yd2 -> yd2", 1.0, convertArea(1.0, SQUARE_YARDS, 
				SQUARE_YARDS), THRESHOLD);
	}
}
