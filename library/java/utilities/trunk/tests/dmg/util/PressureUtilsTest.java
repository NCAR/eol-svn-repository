package dmg.util;

import static dmg.util.PressureUtils.*;
import static org.junit.Assert.*;

import org.junit.*;

/**
 * <p>The PressureUtilsTest class is a collection of JUnit tests for the 
 * PressureUtils class.</p>
 * 
 * @see dmg.util.PressureUtils
 * 
 * @author Joel Clawson
 */
public class PressureUtilsTest {

	public static final Double THRESHOLD = Math.pow(1, -10);
	
	@Test public void airDensityCalculationPressures() 
	throws CalculationException {
		assertEquals("Air Density: press = 0",0.0,
				calculateAirDensity(0.0, 300.0),THRESHOLD);
		assertEquals("Air Density: press = 100",.0011612783,
				calculateAirDensity(100.0, 300.0),THRESHOLD);
		assertEquals("Air Density: press = 1013.25",.0117666527,
				calculateAirDensity(1013.25, 300.0),THRESHOLD);
	}
	
	@Test public void airDensityCalculationTemperatures() 
	throws CalculationException {
		assertEquals("Air Density: temp = 10",.3529995819,
				calculateAirDensity(1013.25, 10.0),THRESHOLD);
		assertEquals("Air Density: temp = 273.15",.0129232869,
				calculateAirDensity(1013.25, 273.15),THRESHOLD);
		assertEquals("Air Density: temp = 373.15",.0094599915,
				calculateAirDensity(1013.25, 373.15),THRESHOLD);
	}
	
	@Test (expected = CalculationException.class)
	public void airDensityNullPressure() throws CalculationException {
		calculateAirDensity(null, 300.0);
	}
	
	@Test (expected = CalculationException.class)
	public void airDensityNullTemperature() throws CalculationException {
		calculateAirDensity(1013.25, null);
	}
	
	@Test (expected = CalculationException.class)
	public void airDensityZeroTemperature() throws CalculationException {
		calculateAirDensity(1013.25, 0.0);
	}
	
	@Test public void atmospheresConversions() throws ConversionException {
		assertEquals("atm -> atm", 1.0, convertPressure(1.0, 
				ATMOSPHERES, ATMOSPHERES), THRESHOLD);
		assertEquals("atm -> bar", 1.01325, convertPressure(1.0, 
				ATMOSPHERES, BARS), THRESHOLD);
		assertEquals("atm -> dyne/cm2", 1013250.0, convertPressure(1.0,
				ATMOSPHERES, DYNE_CM2), THRESHOLD);
		assertEquals("atm -> hPa",1013.25,convertPressure(1.0, 
				ATMOSPHERES, HECTOPASCALS), THRESHOLD);
		assertEquals("atm -> inHg",29.9212598425,convertPressure(1.0, 
				ATMOSPHERES, INCHES_MERCURY), THRESHOLD);
		assertEquals("atm -> kPa",101.325,convertPressure(1.0,
				ATMOSPHERES, KILOPASCALS), THRESHOLD);
		assertEquals("atm -> mbar",1013.25,convertPressure(1.0, 
				ATMOSPHERES, MILLIBARS), THRESHOLD);
		assertEquals("atm -> mmHg",760.0,convertPressure(1.0, 
				ATMOSPHERES, MILLIMETERS_MERCURY), THRESHOLD);
		assertEquals("atm -> Pa",101325.0,convertPressure(1.0, 
				ATMOSPHERES, PASCALS), THRESHOLD);
	}
	
	@Test public void barsConversions() throws ConversionException {
		assertEquals("bar -> atm",1.0,convertPressure(1.01325, 
				BARS, ATMOSPHERES), THRESHOLD);
		assertEquals("bar -> bar",1.01325,convertPressure(1.01325, 
				BARS, BARS), THRESHOLD);
		assertEquals("bar -> dyne/cm2",1013250.0,convertPressure(1.01325,
				BARS, DYNE_CM2), THRESHOLD);
		assertEquals("bar -> hPa",1013.25,convertPressure(1.01325,
				BARS, HECTOPASCALS), THRESHOLD);
		assertEquals("bar -> inHg",29.9212598425,convertPressure(1.01325, 
				BARS, INCHES_MERCURY), THRESHOLD);
		assertEquals("bar -> kPa",101.325,convertPressure(1.01325,
				BARS, KILOPASCALS), THRESHOLD);
		assertEquals("bar -> mbar",1013.25,convertPressure(1.01325, 
				BARS, MILLIBARS), THRESHOLD);
		assertEquals("bar -> mmHg",760.0,convertPressure(1.01325, 
				BARS, MILLIMETERS_MERCURY), THRESHOLD);
		assertEquals("bar -> Pa",101325.0,convertPressure(1.01325, 
				BARS, PASCALS), THRESHOLD);
	}
		
	@Test (expected = ConversionException.class)
	public void convertPressureNullInputUnit() throws ConversionException {
		convertPressure(10.0, null, BARS);
	}
	
	@Test (expected = ConversionException.class)
	public void convertPressureNullOutputUnit() throws ConversionException {
		convertPressure(10.0, BARS, null);
	}
	
	@Test public void convertPressureNullValue() throws ConversionException {
		assertNull("convertPressure: null value", 
				convertPressure(null, BARS, MILLIBARS));
	}
	
	@Test public void dyneCM2Conversions() throws ConversionException {
		assertEquals("dyne/cm2 -> atm",1.0,convertPressure(1013250.0,
				DYNE_CM2, ATMOSPHERES), THRESHOLD);
		assertEquals("dyne/cm2 -> bar",1.01325,convertPressure(1013250.0,
				DYNE_CM2, BARS), THRESHOLD);
		assertEquals("dyne/cm2 -> dyne/cm2",1013250.0,convertPressure(1013250.0,
				DYNE_CM2, DYNE_CM2), THRESHOLD);
		assertEquals("dyne/cm2 -> hPa",1013.25,convertPressure(1013250.0, 
				DYNE_CM2, HECTOPASCALS), THRESHOLD);
		assertEquals("dyne/cm2 -> inHg",29.9212598425,convertPressure(1013250.0,
				DYNE_CM2, INCHES_MERCURY), THRESHOLD);
		assertEquals("dyne/cm2 -> kPa",101.325,convertPressure(1013250.0, 
				DYNE_CM2, KILOPASCALS), THRESHOLD);
		assertEquals("dyne/cm2 -> mbar",1013.25,convertPressure(1013250.0, 
				DYNE_CM2, MILLIBARS), THRESHOLD);
		assertEquals("dyne/cm2 -> mmHg",760.0,convertPressure(1013250.0,
				DYNE_CM2, MILLIMETERS_MERCURY), THRESHOLD);
		assertEquals("dyne/cm2 -> Pa",101325.0,convertPressure(1013250.0, 
				DYNE_CM2, PASCALS), THRESHOLD);
	}
	
	@Test public void hectopascalConversions() throws ConversionException {
		assertEquals("hPa -> atm",1.0,convertPressure(1013.25, 
				HECTOPASCALS, ATMOSPHERES), THRESHOLD);
		assertEquals("hPa -> bar",1.01325,convertPressure(1013.25, 
				HECTOPASCALS, BARS), THRESHOLD);
		assertEquals("hPa -> dyne/cm2",1013250.0,convertPressure(1013.25,
				HECTOPASCALS, DYNE_CM2), THRESHOLD);
		assertEquals("hPa -> hPa",1013.25,convertPressure(1013.25,
				HECTOPASCALS, HECTOPASCALS), THRESHOLD);
		assertEquals("hPa -> inHg",29.9212598425,convertPressure(1013.25,
				HECTOPASCALS, INCHES_MERCURY), THRESHOLD);
		assertEquals("hPa -> kPa",101.325,convertPressure(1013.25, 
				HECTOPASCALS, KILOPASCALS), THRESHOLD);
		assertEquals("hPa -> mbar",1013.25,convertPressure(1013.25, 
				HECTOPASCALS, MILLIBARS), THRESHOLD);
		assertEquals("hPa -> mmHg",760.0,convertPressure(1013.25,
				HECTOPASCALS, MILLIMETERS_MERCURY), THRESHOLD);
		assertEquals("hPa -> Pa",101325.0,convertPressure(1013.25, 
				HECTOPASCALS, PASCALS), THRESHOLD);
	}
	
	@Test public void inchesOfMercuryConversions() throws ConversionException {
		assertEquals("inHg -> atm",1.0,convertPressure(29.92125984251968, 
				INCHES_MERCURY, ATMOSPHERES), THRESHOLD);
		assertEquals("inHg -> bar",1.01325,convertPressure(29.92125984251968,
				INCHES_MERCURY, BARS), THRESHOLD);
		assertEquals("inHg -> dyne/cm2",1013250.0,
				convertPressure(29.92125984251968, INCHES_MERCURY, DYNE_CM2), 
				THRESHOLD);
		assertEquals("inHg -> hPa",1013.25,convertPressure(29.92125984251968, 
				INCHES_MERCURY, HECTOPASCALS), THRESHOLD);
		assertEquals("inHg -> inHg",29.9212598425,
				convertPressure(29.92125984251968, INCHES_MERCURY, 
						INCHES_MERCURY), THRESHOLD);
		assertEquals("inHg -> kPa",101.325,convertPressure(29.92125984251968, 
				INCHES_MERCURY, KILOPASCALS), THRESHOLD);
		assertEquals("inHg -> mbar",1013.25,convertPressure(29.92125984251968, 
				INCHES_MERCURY, MILLIBARS), THRESHOLD);
		assertEquals("inHg -> mmHg",760.0,convertPressure(29.92125984251968, 
				INCHES_MERCURY, MILLIMETERS_MERCURY), THRESHOLD);
		assertEquals("inHg -> Pa",101325.0,convertPressure(29.92125984251968,
				INCHES_MERCURY, PASCALS), THRESHOLD);
	}

	@Test public void kilopascalConversions() throws ConversionException {
		assertEquals("kPa -> atm",1.0,
				convertPressure(101.325, KILOPASCALS, ATMOSPHERES), THRESHOLD);
		assertEquals("kPa -> bar",1.01325,
				convertPressure(101.325, KILOPASCALS, BARS), THRESHOLD);
		assertEquals("kPa -> dyne/cm2",1013250.0,
				convertPressure(101.325, KILOPASCALS, DYNE_CM2), THRESHOLD);
		assertEquals("kPa -> hPa",1013.25,
				convertPressure(101.325, KILOPASCALS, HECTOPASCALS), THRESHOLD);
		assertEquals("kPa -> inHg",29.9212598425, convertPressure(101.325, 
				KILOPASCALS, INCHES_MERCURY), THRESHOLD);
		assertEquals("kPa -> kPa",101.325,
				convertPressure(101.325, KILOPASCALS, KILOPASCALS), THRESHOLD);
		assertEquals("kPa -> mbar",1013.25,
				convertPressure(101.325, KILOPASCALS, MILLIBARS), THRESHOLD);
		assertEquals("kPa -> mmHg",760.0,
				convertPressure(101.325, KILOPASCALS, MILLIMETERS_MERCURY), THRESHOLD);
		assertEquals("kPa -> Pa",101325.0,
				convertPressure(101.325, KILOPASCALS, PASCALS), THRESHOLD);
	}
	
	@Test public void millibarsConversions() throws ConversionException {
		assertEquals("mbar -> atm",1.0,convertPressure(1013.25, 
				MILLIBARS, ATMOSPHERES), THRESHOLD);
		assertEquals("mbar -> bar",1.01325,convertPressure(1013.25,
				MILLIBARS, BARS), THRESHOLD);
		assertEquals("mbar -> dyne/cm2",1013250.0,convertPressure(1013.25,
				MILLIBARS, DYNE_CM2), THRESHOLD);
		assertEquals("mbar -> hPa",1013.25,convertPressure(1013.25,
				MILLIBARS, HECTOPASCALS), THRESHOLD);
		assertEquals("mbar -> inHg",29.9212598425,convertPressure(1013.25, 
				MILLIBARS, INCHES_MERCURY), THRESHOLD);
		assertEquals("mbar -> kPa",101.325,convertPressure(1013.25, 
				MILLIBARS, KILOPASCALS), THRESHOLD);
		assertEquals("mbar -> mbar",1013.25,convertPressure(1013.25, 
				MILLIBARS, MILLIBARS), THRESHOLD);
		assertEquals("mbar -> mmHg",760.0,convertPressure(1013.25, 
				MILLIBARS, MILLIMETERS_MERCURY), THRESHOLD);
		assertEquals("mbar -> Pa",101325.0,convertPressure(1013.25, 
				MILLIBARS, PASCALS), THRESHOLD);
	}
	
	@Test public void millimetersOfMercuryConversions() 
	throws ConversionException {
		assertEquals("mmHg -> atm",1.0,convertPressure(760.0, 
				MILLIMETERS_MERCURY, ATMOSPHERES), THRESHOLD);
		assertEquals("mmHg -> bar",1.01325,convertPressure(760.0, 
				MILLIMETERS_MERCURY, BARS), THRESHOLD);
		assertEquals("mmHg -> dyne/cm2",1013250.0,convertPressure(760.0,
				MILLIMETERS_MERCURY, DYNE_CM2), THRESHOLD);
		assertEquals("mmHg -> hPa",1013.25,convertPressure(760.0, 
				MILLIMETERS_MERCURY, HECTOPASCALS), THRESHOLD);
		assertEquals("mmHg -> inHg",29.9212598425,convertPressure(760.0,
				MILLIMETERS_MERCURY, INCHES_MERCURY), THRESHOLD);
		assertEquals("mmHg -> kPa",101.325,convertPressure(760.0, 
				MILLIMETERS_MERCURY, KILOPASCALS), THRESHOLD);
		assertEquals("mmHg -> mbar",1013.25,convertPressure(760.0, 
				MILLIMETERS_MERCURY, MILLIBARS), THRESHOLD);
		assertEquals("mmHg -> mmHg",760.0,convertPressure(760.0,
				MILLIMETERS_MERCURY, MILLIMETERS_MERCURY), THRESHOLD);
		assertEquals("mmHg -> Pa",101325.0,convertPressure(760.0,
				MILLIMETERS_MERCURY, PASCALS), THRESHOLD);
	}
	
	@Test public void mixingRatioCalculations() throws CalculationException {
		assertEquals("Mixing Ratio: press = 0",-0.62197,
				calculateMixingRatio(0.0, 40.0),THRESHOLD);
		assertEquals("Mixing Ratio: vapor = 0",0.0,
				calculateMixingRatio(1013.25, 0.0),THRESHOLD);
		assertEquals("Mixing Ratio: press = 1013.25, vapor = 40",.0256810515,
				calculateMixingRatio(1013.25, 40.0),THRESHOLD);
	}
	
	@Test (expected = CalculationException.class)
	public void mixingRatioNullPressure() throws CalculationException {
		calculateMixingRatio(null, 40.0);
	}
	
	@Test (expected = CalculationException.class)
	public void mixingRatioNullVaporPressure() throws CalculationException {
		calculateMixingRatio(1013.25, null);
	}
	
	@Test public void pascalsConversions() throws ConversionException {
		assertEquals("Pa -> atm",1.0,
				convertPressure(101325.0, PASCALS, ATMOSPHERES), THRESHOLD);
		assertEquals("Pa -> bar",1.01325,
				convertPressure(101325.0, PASCALS, BARS), THRESHOLD);
		assertEquals("Pa -> dyne/cm2",1013250.0,
				convertPressure(101325.0, PASCALS, DYNE_CM2), THRESHOLD);
		assertEquals("Pa -> hPa",1013.25,
				convertPressure(101325.0, PASCALS, HECTOPASCALS), THRESHOLD);
		assertEquals("Pa -> inHg",29.9212598425,
				convertPressure(101325.0, PASCALS, INCHES_MERCURY), THRESHOLD);
		assertEquals("Pa -> kPa",101.325,
				convertPressure(101325.0, PASCALS, KILOPASCALS), THRESHOLD);
		assertEquals("Pa -> mbar",1013.25,
				convertPressure(101325.0, PASCALS, MILLIBARS), THRESHOLD);
		assertEquals("Pa -> mmHg",760.0,
				convertPressure(101325.0, PASCALS, MILLIMETERS_MERCURY), THRESHOLD);
		assertEquals("Pa -> Pa",101325.0,
				convertPressure(101325.0, PASCALS, PASCALS), THRESHOLD);
	}
	
	@Test public void pressureCalculations() throws CalculationException {
		assertEquals("Press: alt = 986, elev = 1562",815.7647980652,
				calculatePressure(986.0,1562.0),THRESHOLD);
	}
	
	@Test (expected = CalculationException.class)
	public void pressureNotANumberResult() throws CalculationException {
		calculatePressure(0.0, 1000.0);
	}
	
	@Test (expected = CalculationException.class)
	public void pressureNullAltimeter() throws CalculationException {
		calculatePressure(null, 1562.0);
	}
	
	@Test (expected = CalculationException.class)
	public void pressureNullElevation() throws CalculationException {
		calculatePressure(986.0, null);
	}
	
	@Test public void seaLevelPressureCalculationsDewPoints() 
	throws CalculationException {
		assertEquals("SLP: dewpt = 12.0",1025.2843154008,
				calculateSeaLevelPressure(1013.25, 15.0, 12.0, 100.0),
				THRESHOLD);
		assertEquals("SLP: dewpt = -100",1014.6582795968,
				calculateSeaLevelPressure(900.0, 15.0, -100.0, 1000.0),
				THRESHOLD);
		assertEquals("SLP: dewpt = 100",961.9748762872,
				calculateSeaLevelPressure(900.0, 15.0, 100.0, 1000.0),
				THRESHOLD);
	}
	
	@Test public void seaLevelPressureCalculationsElevations() 
	throws CalculationException {
		assertEquals("SLP: elev = 100.0",1025.2843154008,
				calculateSeaLevelPressure(1013.25, 15.0, 12.0, 100.0),
				THRESHOLD);
		assertEquals("SLP: elev = -100",889.4666803346,
				calculateSeaLevelPressure(900.0, 15.0, 12.0, -100.0),THRESHOLD);
		assertEquals("SLP: elev = 0",900.0,
				calculateSeaLevelPressure(900.0, 15.0, 12.0, 0.0),THRESHOLD);
		assertEquals("SLP: elev = 9000",2928.3041238023,
				calculateSeaLevelPressure(900.0, 15.0, 12.0, 9000.0),THRESHOLD);
	}
	
	@Test public void seaLevelPressureCalculationsPressures() 
	throws CalculationException {
		assertEquals("SLP: press = 1013.25",1025.2843154008,
				calculateSeaLevelPressure(1013.25, 15.0, 12.0, 100.0),
				THRESHOLD);
		assertEquals("SLP: press = 300",337.4946330252,
				calculateSeaLevelPressure(300.0, 15.0, 12.0, 1000.0),THRESHOLD);
		assertEquals("SLP: press = 1200",1352.1497595539,
				calculateSeaLevelPressure(1200.0, 15.0, 12.0, 1000.0),
				THRESHOLD);
	}
	
	@Test public void seaLevelPressureCalculationsTemperatures() 
	throws CalculationException {
		assertEquals("SLP: temp = 15.0",1025.2843154008,
				calculateSeaLevelPressure(1013.25, 15.0, 12.0, 100.0),
				THRESHOLD);
		assertEquals("SLP: temp = -100",1099.1154687941,
				calculateSeaLevelPressure(900.0, -100.0, 12.0, 1000.0),
				THRESHOLD);
		assertEquals("SLP: temp = 100",986.5377672163,
				calculateSeaLevelPressure(900.0, 100.0, 12.0, 1000.0),
				THRESHOLD);
	}
	
	@Test (expected = CalculationException.class)
	public void seaLevelPressureNullDewPoint() throws CalculationException {
		calculateSeaLevelPressure(900.0, 15.0, null, 1000.0);
	}
	
	@Test (expected = CalculationException.class)
	public void seaLevelPressureNullElevation() throws CalculationException {
		calculateSeaLevelPressure(900.0, 15.0, 12.0, null);
	}
	
	@Test (expected = CalculationException.class)
	public void seaLevelPressureNullPressure() throws CalculationException {
		calculateSeaLevelPressure(null, 15.0, 12.0, 1000.0);
	}
	
	@Test (expected = CalculationException.class)
	public void seaLevelPressureNullTemperature() throws CalculationException {
		calculateSeaLevelPressure(900.0, null, 12.0, 1000.0);
	}
	
	@Test public void vaporPressureCalculations() throws CalculationException {
		assertEquals("Vapor Pressure: temp = -100",.0000274452,
				calculateVaporPressure(-100.0),THRESHOLD);
		assertEquals("Vapor Pressure: temp = 0",6.1121,
				calculateVaporPressure(0.0),THRESHOLD);
		assertEquals("Vapor Pressure: temp = 30",42.4564490581,
				calculateVaporPressure(30.0),THRESHOLD);
		assertEquals("Vapor Pressure: temp = 100",1047.7237328964,
				calculateVaporPressure(100.0),THRESHOLD);
	}
	
	@Test (expected = CalculationException.class)
	public void vaporPressureDivideByZero() throws CalculationException {
		calculateVaporPressure(-243.5);
	}
	
	@Test (expected = CalculationException.class)
	public void vaporPressureNullTemperature() throws CalculationException {
		calculateVaporPressure(null);
	}
}
