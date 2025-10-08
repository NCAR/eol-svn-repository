package dmg.util;

import static dmg.util.VolumeUtils.*;
import static org.junit.Assert.*;

import org.junit.*;

public class VolumeUtilsTest {
	
	private static final Double THRESHOLD = Math.pow(1, -10);

	@Test public void acreFeetConversions() throws ConversionException {
		assertEquals("Volume: af -> af", 1.0, 
				convertVolume(1.0, ACRE_FEET, ACRE_FEET), THRESHOLD);
		assertEquals("Volume: af -> cm3", 1233481837.5475202, 
				convertVolume(1.0, ACRE_FEET, CUBIC_CENTIMETERS), THRESHOLD);
		assertEquals("Volume: af -> ft3", 43560.0, 
				convertVolume(1.0, ACRE_FEET, CUBIC_FEET), THRESHOLD);
		assertEquals("Volume: af -> gal", 325851.42857172975, 
				convertVolume(1.0, ACRE_FEET, GALLONS), THRESHOLD);
		assertEquals("Volume: af -> in3", 75271680.0, 
				convertVolume(1.0, ACRE_FEET, CUBIC_INCHES), 
				10.0 * (Math.pow(10.0, -8.0)));
		assertEquals("Volume: af -> liter", 1233481.8375475202,
				convertVolume(1.0, ACRE_FEET, LITERS), THRESHOLD);
		assertEquals("Volume: af -> m3", 1233.4818375475204, 
				convertVolume(1.0, ACRE_FEET, CUBIC_METERS), THRESHOLD);
		assertEquals("Volume: af -> ml", 1233481837.5475202, 
				convertVolume(1.0, ACRE_FEET, MILLILITERS), THRESHOLD);
		assertEquals("Volume: af -> oz", 41708982.85718141, 
				convertVolume(1.0, ACRE_FEET, OUNCES), THRESHOLD);
		assertEquals("Volume: af -> pt", 2606811.428573838, 
				convertVolume(1.0, ACRE_FEET, PINTS), THRESHOLD);
		assertEquals("Volume: af -> qt", 1303405.714286919,
				convertVolume(1.0, ACRE_FEET, QUARTS), THRESHOLD);
	}
	
	@Test public void cubicCentimetersConversions() throws ConversionException {
		assertEquals("volume: cm3 -> af", 0.00000008107, 
				convertVolume(100.0, CUBIC_CENTIMETERS, ACRE_FEET), THRESHOLD);
		assertEquals("volume: cm3 -> cm3", 100.0, 
				convertVolume(100.0, CUBIC_CENTIMETERS, CUBIC_CENTIMETERS), 
				THRESHOLD);
		assertEquals("volume: cm3 -> ft3", 0.00353146667, 
				convertVolume(100.0, CUBIC_CENTIMETERS, CUBIC_FEET), THRESHOLD);
		assertEquals("volume: cm3 -> gal", 0.02641720524, 
				convertVolume(100.0, CUBIC_CENTIMETERS, GALLONS), THRESHOLD);
		assertEquals("volume: cm3 -> in3", 6.10237440947, 
				convertVolume(100.0, CUBIC_CENTIMETERS, CUBIC_INCHES), 
				THRESHOLD);
		assertEquals("volume: cm3 -> liter", 0.1, 
				convertVolume(100.0, CUBIC_CENTIMETERS, LITERS), THRESHOLD);
		assertEquals("volume: cm3 -> m3", 0.0001, 
				convertVolume(100.0, CUBIC_CENTIMETERS, CUBIC_METERS), 
				THRESHOLD);
		assertEquals("volume: cm3 -> ml", 100.0, 
				convertVolume(100.0, CUBIC_CENTIMETERS, MILLILITERS),
				THRESHOLD);
		assertEquals("volume: cm3 -> oz", 3.381402270187, 
				convertVolume(100.0, CUBIC_CENTIMETERS, OUNCES), THRESHOLD);
		assertEquals("volume: cm3 -> pt", 0.211337641887, 
				convertVolume(100.0, CUBIC_CENTIMETERS, PINTS), THRESHOLD);
		assertEquals("volume: cm3 -> qt", 0.105668820943, 
				convertVolume(100.0, CUBIC_CENTIMETERS, QUARTS), THRESHOLD);
	}
	
	@Test public void cubicFeetConversions() throws ConversionException {
		assertEquals("volume: ft3 -> af", 0.002295684114,
				convertVolume(100.0, CUBIC_FEET, ACRE_FEET), THRESHOLD);
		assertEquals("volume: ft3 -> cm3", 2831684.6592,
				convertVolume(100.0, CUBIC_FEET, CUBIC_CENTIMETERS), THRESHOLD);
		assertEquals("volume: ft3 -> ft3", 100.0, 
				convertVolume(100.0, CUBIC_FEET, CUBIC_FEET), THRESHOLD);
		assertEquals("volume: ft3 -> gal", 748.05194805268,
				convertVolume(100.0, CUBIC_FEET, GALLONS), THRESHOLD);
		assertEquals("volume: ft3 -> in3", 172800.0,
				convertVolume(100.0, CUBIC_FEET, CUBIC_INCHES), THRESHOLD);
		assertEquals("volume: ft3 -> liter", 2831.6846592,
				convertVolume(100.0, CUBIC_FEET, LITERS), THRESHOLD);
		assertEquals("volume: ft3 -> m3", 2.8316846592,
				convertVolume(100.0, CUBIC_FEET, CUBIC_METERS), THRESHOLD);
		assertEquals("volume: ft3 -> ml", 2831684.6592, 
				convertVolume(100.0, CUBIC_FEET, MILLILITERS), THRESHOLD);
		assertEquals("volume: ft3 -> oz", 95750.64935073783, 
				convertVolume(100.0, CUBIC_FEET, OUNCES), THRESHOLD);
		assertEquals("volume: ft3 -> pt", 5984.4155844211145, 
				convertVolume(100.0, CUBIC_FEET, PINTS), THRESHOLD);
		assertEquals("volume: ft3 -> qt", 2992.2077922106, 
				convertVolume(100.0, CUBIC_FEET, QUARTS), THRESHOLD);
	}
	
	@Test public void cubicInchesConversions() throws ConversionException {
		assertEquals("volume: in3 -> af", 0.00001328521,
				convertVolume(1000.0, CUBIC_INCHES, ACRE_FEET), THRESHOLD);
		assertEquals("volume: in3 -> cm3", 16387.064,
				convertVolume(1000.0, CUBIC_INCHES, CUBIC_CENTIMETERS), 
				THRESHOLD);
		assertEquals("volume: in3 -> ft3", 0.5787037037037, 
				convertVolume(1000.0, CUBIC_INCHES, CUBIC_FEET), THRESHOLD);
		assertEquals("volume: in3 -> gal", 4.329004329, 
				convertVolume(1000.0, CUBIC_INCHES, GALLONS), THRESHOLD);
		assertEquals("volume: in3 -> in3", 1000.0, 
				convertVolume(1000.0, CUBIC_INCHES, CUBIC_INCHES), THRESHOLD);
		assertEquals("volume: in3 -> liter", 16.387064, 
				convertVolume(1000.0, CUBIC_INCHES, LITERS), THRESHOLD);
		assertEquals("volume: in3 -> m3", 0.016387064, 
				convertVolume(1000.0, CUBIC_INCHES, CUBIC_METERS), THRESHOLD);
		assertEquals("volume: in3 -> ml", 16387.064, 
				convertVolume(1000.0, CUBIC_INCHES, MILLILITERS), THRESHOLD);
		assertEquals("volume: in3 -> oz", 554.112554113,
				convertVolume(1000.0, CUBIC_INCHES, OUNCES), THRESHOLD);
		assertEquals("volume: in3 -> pt", 34.632034632, 
				convertVolume(1000.0, CUBIC_INCHES, PINTS), THRESHOLD);
		assertEquals("volume: in3 -> qt", 17.316017316,
				convertVolume(1000.0, CUBIC_INCHES, QUARTS), THRESHOLD);
	}
	
	@Test public void cubicMetersConversions() throws ConversionException {
		assertEquals("volume: m3 -> af", 0.0081071319, 
				convertVolume(10.0, CUBIC_METERS, ACRE_FEET), THRESHOLD);
		assertEquals("volume: m3 -> cm3", 10000000.0,
				convertVolume(10.0, CUBIC_METERS, CUBIC_CENTIMETERS), 
				THRESHOLD);
		assertEquals("volume: m3 -> ft3", 353.1466672149,
				convertVolume(10.0, CUBIC_METERS, CUBIC_FEET), THRESHOLD);
		assertEquals("volume: m3 -> gal", 2641.720523584, 
				convertVolume(10.0, CUBIC_METERS, GALLONS), THRESHOLD);
		assertEquals("volume: m3 -> in3", 610237.440947323, 
				convertVolume(10.0, CUBIC_METERS, CUBIC_INCHES), THRESHOLD);
		assertEquals("volume: m3 -> liter", 10000.0, 
				convertVolume(10.0, CUBIC_METERS, LITERS), THRESHOLD);
		assertEquals("volume: m3 -> m3", 10.0, 
				convertVolume(10.0, CUBIC_METERS, CUBIC_METERS), THRESHOLD);
		assertEquals("volume: m3 -> ml", 10000000.0, 
				convertVolume(10.0, CUBIC_METERS, MILLILITERS), THRESHOLD);
		assertEquals("volume: m3 -> oz", 338140.2270187425, 
				convertVolume(10.0, CUBIC_METERS, OUNCES), THRESHOLD);
		assertEquals("volume: m3 -> pt", 21133.764188671405,
				convertVolume(10.0, CUBIC_METERS, PINTS), THRESHOLD);
		assertEquals("volume: m3 -> qt", 10566.882094335702,
				convertVolume(10.0, CUBIC_METERS, QUARTS), THRESHOLD);
	}
	
	@Test public void gallonsConversions() throws ConversionException {
		assertEquals("volume: gal -> af", 0.000306888328, 
				convertVolume(100.0, GALLONS, ACRE_FEET), THRESHOLD);
		assertEquals("volume: gal -> cm3", 378541.1783996502, 
				convertVolume(100.0, GALLONS, CUBIC_CENTIMETERS), THRESHOLD);
		assertEquals("volume: gal -> ft3", 13.36805555554,
				convertVolume(100.0, GALLONS, CUBIC_FEET), THRESHOLD);
		assertEquals("volume: gal -> gal", 100.0, 
				convertVolume(100.0, GALLONS, GALLONS), THRESHOLD);
		assertEquals("volume: gal -> in3", 23.1, 
				convertVolume(0.1, GALLONS, CUBIC_INCHES), THRESHOLD);
		assertEquals("volume: gal -> liter", 378.5411783997, 
				convertVolume(100.0, GALLONS, LITERS), THRESHOLD);
		assertEquals("volume: gal -> m3", 0.3785411783997, 
				convertVolume(100.0, GALLONS, CUBIC_METERS), THRESHOLD);
		assertEquals("volume: gal -> ml", 378541.1783996502, 
				convertVolume(100.0, GALLONS, MILLILITERS), THRESHOLD);
		assertEquals("volume: gal -> oz", 12800.0, 
				convertVolume(100.0, GALLONS, OUNCES), THRESHOLD);
		assertEquals("volume: gal -> pt", 800.0,
				convertVolume(100.0, GALLONS, PINTS), THRESHOLD);
		assertEquals("volume: gal -> qt", 400.0,
				convertVolume(100.0, GALLONS, QUARTS), THRESHOLD);
	}
	
	@Test public void litersConversions() throws ConversionException {
		assertEquals("volume: liter -> af", 0.000081071319, 
				convertVolume(100.0, LITERS, ACRE_FEET), THRESHOLD);
		assertEquals("volume: liter -> cm3", 100000.0, 
				convertVolume(100.0, LITERS, CUBIC_CENTIMETERS), THRESHOLD);
		assertEquals("volume: liter -> ft3", 3.53146667215, 
				convertVolume(100.0, LITERS, CUBIC_FEET), THRESHOLD);
		assertEquals("volume: liter -> gal", 26.41720523584, 
				convertVolume(100.0, LITERS, GALLONS), THRESHOLD);
		assertEquals("volume: liter -> in3", 6102.374409473229,
				convertVolume(100.0, LITERS, CUBIC_INCHES), THRESHOLD);
		assertEquals("volume: liter -> liter", 100.0, 
				convertVolume(100.0, LITERS, LITERS), THRESHOLD);
		assertEquals("volume: liter -> m3", 0.1,
				convertVolume(100.0, LITERS, CUBIC_METERS), THRESHOLD);
		assertEquals("volume: liter -> ml", 100000.0, 
				convertVolume(100.0, LITERS, MILLILITERS), THRESHOLD);
		assertEquals("volume: liter -> oz", 3381.4022701874246, 
				convertVolume(100.0, LITERS, OUNCES), THRESHOLD);
		assertEquals("volume: liter -> pt", 211.3376418867, 
				convertVolume(100.0, LITERS, PINTS), THRESHOLD);
		assertEquals("volume: liter -> qt", 105.6688209434, 
				convertVolume(100.0, LITERS, QUARTS), THRESHOLD);
	}
	
	@Test public void millilitersConversions() throws ConversionException {
		assertEquals("volume: ml -> af", 0.00000008107, 
				convertVolume(100.0, MILLILITERS, ACRE_FEET), THRESHOLD);
		assertEquals("volume: ml -> cm3", 100.0, 
				convertVolume(100.0, MILLILITERS, CUBIC_CENTIMETERS),
				THRESHOLD);
		assertEquals("volume: ml -> ft3", 0.00353146667,
				convertVolume(100.0, MILLILITERS, CUBIC_FEET), THRESHOLD);
		assertEquals("volume: ml -> gal", 0.02641720524,
				convertVolume(100.0, MILLILITERS, GALLONS), THRESHOLD);
		assertEquals("volume: ml -> in3", 6.10237440947, 
				convertVolume(100.0, MILLILITERS, CUBIC_INCHES), THRESHOLD);
		assertEquals("volume: ml -> liter", 0.1,
				convertVolume(100.0, MILLILITERS, LITERS), THRESHOLD);
		assertEquals("volume: ml -> m3", 0.0001, 
				convertVolume(100.0, MILLILITERS, CUBIC_METERS), THRESHOLD);
		assertEquals("volume: ml -> ml", 100.0, 
				convertVolume(100.0, MILLILITERS, MILLILITERS), THRESHOLD);
		assertEquals("volume: ml -> oz", 3.381402270187,
				convertVolume(100.0, MILLILITERS, OUNCES), THRESHOLD);
		assertEquals("volume: ml -> pt", 0.211337641887,
				convertVolume(100.0, MILLILITERS, PINTS), THRESHOLD);
		assertEquals("volume: ml -> qt", 0.105668820943,
				convertVolume(100.0, MILLILITERS, QUARTS), THRESHOLD);
	}
	
	@Test public void ouncesConversions() throws ConversionException {
		assertEquals("volume: oz -> af", 0.0002397565, 
				convertVolume(10000.0, OUNCES, ACRE_FEET), THRESHOLD);
		assertEquals("volume: oz -> cm3", 295735.29562472674, 
				convertVolume(10000.0, OUNCES, CUBIC_CENTIMETERS), THRESHOLD);
		assertEquals("volume: oz -> ft3", 10.443793402768126,
				convertVolume(10000.0, OUNCES, CUBIC_FEET), THRESHOLD);
		assertEquals("volume: oz -> gal", 78.125, 
				convertVolume(10000.0, OUNCES, GALLONS), THRESHOLD);
		assertEquals("volume: oz -> in3", 1.8046875,
				convertVolume(1.0, OUNCES, CUBIC_INCHES), THRESHOLD);
		assertEquals("volume: oz -> liter", 295.7352956247,
				convertVolume(10000.0, OUNCES, LITERS), THRESHOLD);
		assertEquals("volume: oz -> m3", 0.295735295625, 
				convertVolume(10000.0, OUNCES, CUBIC_METERS), THRESHOLD);
		assertEquals("volume: oz -> ml", 295735.29562472674, 
				convertVolume(10000.0, OUNCES, MILLILITERS), THRESHOLD);
		assertEquals("volume: oz -> oz", 10000.0,
				convertVolume(10000.0, OUNCES, OUNCES), THRESHOLD);
		assertEquals("volume: oz -> pt", 625.0, 
				convertVolume(10000.0, OUNCES, PINTS), THRESHOLD);
		assertEquals("volume: oz -> qt", 312.5, 
				convertVolume(10000.0, OUNCES, QUARTS), THRESHOLD);
	}
	
	@Test public void pintsConversions() throws ConversionException {
		assertEquals("volume: pt -> af", 0.00038361041, 
				convertVolume(1000.0, PINTS, ACRE_FEET), THRESHOLD);
		assertEquals("volume: pt -> cm3", 473.17647299956275,
				convertVolume(1.0, PINTS, CUBIC_CENTIMETERS), THRESHOLD);
		assertEquals("volume: pt -> ft3", 16.710069444429,
				convertVolume(1000.0, PINTS, CUBIC_FEET), THRESHOLD);
		assertEquals("volume: pt -> gal", 125.0, 
				convertVolume(1000.0, PINTS, GALLONS), THRESHOLD);
		assertEquals("volume: pt -> in3", 28.875, 
				convertVolume(1.0, PINTS, CUBIC_INCHES), THRESHOLD);
		assertEquals("volume: pt -> liter", 473.17647299956, 
				convertVolume(1000.0, PINTS, LITERS), THRESHOLD);
		assertEquals("volume: pt -> m3", 0.47317647299956,
				convertVolume(1000.0, PINTS, CUBIC_METERS), THRESHOLD);
		assertEquals("volume: pt -> ml", 473.17647299956275,
				convertVolume(1.0, PINTS, MILLILITERS), THRESHOLD);
		assertEquals("volume: pt -> oz", 16000.0, 
				convertVolume(1000.0, PINTS, OUNCES), THRESHOLD);
		assertEquals("volume: pt -> pt", 1000.0, 
				convertVolume(1000.0, PINTS, PINTS), THRESHOLD);
		assertEquals("volume: pt -> qt", 500.0, 
				convertVolume(1000.0, PINTS, QUARTS), THRESHOLD);
	}
	
	@Test public void quartsConversions() throws ConversionException {
		assertEquals("volume: qt -> af", 0.00007672208193, 
				convertVolume(100.0, QUARTS, ACRE_FEET), THRESHOLD);
		assertEquals("volume: qt -> cm3", 94635.29459991255, 
				convertVolume(100.0, QUARTS, CUBIC_CENTIMETERS), THRESHOLD);
		assertEquals("volume: qt -> ft3", 3.3420138888858,
				convertVolume(100.0, QUARTS, CUBIC_FEET), THRESHOLD);
		assertEquals("volume: qt -> gal", 25.0,
				convertVolume(100.0, QUARTS, GALLONS), THRESHOLD);
		assertEquals("volume: qt -> in3", 57.750, 
				convertVolume(1.0, QUARTS, CUBIC_INCHES), THRESHOLD);
		assertEquals("volume: qt -> liter", 94.63529459991256, 
				convertVolume(100.0, QUARTS, LITERS), THRESHOLD);
		assertEquals("volume: qt -> m3", 0.09463529459991256, 
				convertVolume(100.0, QUARTS, CUBIC_METERS), THRESHOLD);
		assertEquals("volume: qt -> ml", 94635.29459991255, 
				convertVolume(100.0, QUARTS, MILLILITERS), THRESHOLD);
		assertEquals("volume: qt -> oz", 3200.0, 
				convertVolume(100.0, QUARTS, OUNCES), THRESHOLD);
		assertEquals("volume: qt -> pt", 200.0, 
				convertVolume(100.0, QUARTS, PINTS), THRESHOLD);
		assertEquals("volume: qt -> qt", 100.0, 
				convertVolume(100.0, QUARTS, QUARTS), THRESHOLD);
	}
	
	@Test (expected = ConversionException.class)
	public void volumeConversionNullInputUnit() throws ConversionException {
		convertVolume(10.0, null, LITERS);
	}
	
	@Test (expected = ConversionException.class)
	public void volumeConversionNullOutputUnit() throws ConversionException {
		convertVolume(10.0, LITERS, null);
	}
	
	@Test public void volumeConversionNullValue() throws ConversionException {
		assertNull("Volume: null value", convertVolume(null, LITERS, GALLONS));
	}
}
