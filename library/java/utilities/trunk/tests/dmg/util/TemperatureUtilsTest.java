package dmg.util;

import static dmg.util.TemperatureUtils.*;
import static org.junit.Assert.*;
import org.junit.*;

public class TemperatureUtilsTest {

	private static Double THRESHOLD = Math.pow(1, -10);

	@Test public void celciusConversions() throws ConversionException {
		assertEquals("C -> C", 10.0, convertTemperature(10.0, CELCIUS, CELCIUS),
				THRESHOLD);
		assertEquals("C -> F", 32.0, convertTemperature(0.0, CELCIUS, 
				FAHRENHEIT), THRESHOLD);
		assertEquals("C -> K", 273.15, convertTemperature(0.0, CELCIUS, KELVIN),
				THRESHOLD);
	}

	@Test (expected = ConversionException.class)
	public void convertTemperatureNullInputUnit() throws ConversionException {
		convertTemperature(10.0, null, CELCIUS);
	}

	@Test (expected = ConversionException.class)
	public void convertTemperatureNullOutputUnit() throws ConversionException {
		convertTemperature(10.0, CELCIUS, null);
	}

	@Test public void convertTemperatureNullValue() throws ConversionException {
		assertNull("convertTemperature: null value", convertTemperature(null, 
				CELCIUS, KELVIN));
	}

	@Test public void dewPointCalculationsRelativeHumidities() 
	throws CalculationException {
		assertEquals("Dew Point: rh = 5",-24.4287813828,
				calculateDewPoint(15.0,5.0),THRESHOLD);
		assertEquals("Dew Point: rh = 100",15.0,
				calculateDewPoint(15.0, 100.0),THRESHOLD);
		assertEquals("Dew Point: rh = 105",15.7599635582,
				calculateDewPoint(15.0, 105.0),THRESHOLD);
	}

	@Test public void dewPointCalculationsTemperatures() 
	throws CalculationException {
		assertEquals("Dew Point: temp = -105",-108.0227762938,
				calculateDewPoint(-105.0,50.0),THRESHOLD);
		assertEquals("Dew Point: temp = -100",-103.2424121103,
				calculateDewPoint(-100.0,50.0),THRESHOLD);
		assertEquals("Dew Point: temp = 0",-9.1913078301,
				calculateDewPoint(0.0, 50.0),THRESHOLD);
		assertEquals("Dew Point: temp = 100",81.9884035673,
				calculateDewPoint(100.0, 50.0),THRESHOLD);
		assertEquals("Dew Point: temp = 105",86.4743719849,
				calculateDewPoint(105.0, 50.0),THRESHOLD);
	}

	@Test (expected = CalculationException.class)
	public void dewPointNegativeRelativeHumidity() throws CalculationException {
		calculateDewPoint(20.0, -0.1);
	}

	@Test (expected = CalculationException.class)
	public void dewPointNullRelativeHumidity() throws CalculationException {
		calculateDewPoint(20.0, null);
	}

	@Test (expected = CalculationException.class)
	public void dewPointNullTemperature() throws CalculationException {
		calculateDewPoint(null, 50.0);
	}

	@Test (expected = CalculationException.class)
	public void dewPointVaporPressureDivideByZero()
	throws CalculationException {
		calculateDewPoint(-243.5, 50.0);
	}

	@Test public void fahrenheitConversions() throws ConversionException {
		assertEquals("F -> C", 0.0, convertTemperature(32.0, FAHRENHEIT, 
				CELCIUS), THRESHOLD);
		assertEquals("F -> F", 32.0, convertTemperature(32.0, FAHRENHEIT, 
				FAHRENHEIT), THRESHOLD);
		assertEquals("F -> K", 273.15, convertTemperature(32.0, FAHRENHEIT, 
				KELVIN), THRESHOLD);
	}

	@Test public void kelvinConversions() throws ConversionException {
		assertEquals("K -> C", 0.0, convertTemperature(273.15, KELVIN, 
				CELCIUS), THRESHOLD);
		assertEquals("K -> F", 32.0, convertTemperature(273.15, KELVIN, 
				FAHRENHEIT), THRESHOLD);
		assertEquals("K -> K", 273.15, convertTemperature(273.15, KELVIN, 
				KELVIN), THRESHOLD);
	}
	
	@Test public void virtualTemperatureCalculationsMixingRatios() 
	throws CalculationException {
		assertEquals("Virtual Temp: mixRatio = 0",290.0,
				calculateVirtualTemperature(290.0, 0.0),THRESHOLD);
		assertEquals("Virtual Temp: mixRatio = .5",348.7534768558,
				calculateVirtualTemperature(290.0, 0.5),THRESHOLD);
		assertEquals("Virtual Temp: mixRatio = 1",378.1302152837,
				calculateVirtualTemperature(290.0, 1.0),THRESHOLD);
		assertEquals("Virtual Temp: mixRatio = 5",436.8836921395,
				calculateVirtualTemperature(290.0, 5.0),THRESHOLD);
	}

	@Test public void virtualTemperatureCalculationsTemperatures() 
	throws CalculationException {
		assertEquals("Virtual Temp: temp = 0",0.0,
				calculateVirtualTemperature(0.0, .5),THRESHOLD);
		assertEquals("Virtual Temp: temp = 273.15",328.4896972523,
				calculateVirtualTemperature(273.15, .5),THRESHOLD);
		assertEquals("Virtual Temp: temp = 373.15",448.7495168577,
				calculateVirtualTemperature(373.15, .5),THRESHOLD);
	}

	@Test (expected = CalculationException.class)
	public void virtualTemperatureDivideByZero() 
	throws CalculationException {
		calculateVirtualTemperature(300.0, -1.0);
	}

	@Test (expected = CalculationException.class)
	public void virtualTemperatureNullMixingRatio() 
	throws CalculationException {
		calculateVirtualTemperature(300.0, null);
	}

	@Test (expected = CalculationException.class)
	public void virtualTemperatureNullTemperature() 
	throws CalculationException {
		calculateVirtualTemperature(null, .5);
	}
}
