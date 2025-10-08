package dmg.record;

import static dmg.util.LengthUtils.*;
import static dmg.util.PressureUtils.*;
import static dmg.util.TemperatureUtils.*;
import static org.junit.Assert.*;
import dmg.util.*;
import dmg.util.LengthUtils.*;
import dmg.util.PressureUtils.*;
import dmg.util.TemperatureUtils.*;

import org.junit.*;

public class PTHDataTest {

    private static final Double THRESHOLD = Math.pow(1, -10);
    
    private PTHData data;
    
    @Before public void initialize() {
	data = new TestPTHData(true);
    }
    
    @Test public void dewPoint() throws ConversionException, 
					CalculationWarning, InvalidValueWarning {
	assertNull("Dew Point: default", data.getDewPoint());
	
	data.setDewPoint(12.0, CELCIUS);
	assertEquals("Dew Point: set value", 12.0, data.getDewPoint(), 
		     THRESHOLD);
	data.setDewPoint(32.0, FAHRENHEIT);
	assertEquals("Dew Point: set diff unit", 0.0, data.getDewPoint(), 
		     THRESHOLD);
	data.setDewPoint(0.0, CELCIUS);
	assertEquals("Dew Point: zero", 0.0, data.getDewPoint(), THRESHOLD);
	data.setDewPoint(-10.0, CELCIUS);
	assertEquals("Dew Point: negative", -10.0, data.getDewPoint(), 
		     THRESHOLD);
	data.setDewPoint(null, CELCIUS);
	assertNull("Dew Point: set null", data.getDewPoint());
    }
    
    @Test public void dewPointCalculation() throws CalculationWarning, 
						   ConversionException, InvalidValueWarning {
	assertNull("DP Calc: sanity check", data.getDewPoint());
	
	data.setTemperature(15.0, CELCIUS);
	data.setRelativeHumidity(100.0);
	assertEquals("DP Calc: calculated", 15.0, data.getDewPoint(), 
		     THRESHOLD);
	data.setDewPoint(11.11, CELCIUS);
	assertEquals("DP Calc: manual override", 11.11, data.getDewPoint(), 
		     THRESHOLD);
	data.setTemperature(10.0, CELCIUS);
	assertEquals("DP Calc: temp change in override", 11.11, 
		     data.getDewPoint(), THRESHOLD);
	data.setRelativeHumidity(50.0);
	assertEquals("DP Calc: rh change in override", 11.11, 
		     data.getDewPoint(), THRESHOLD);
	data.setDewPoint(null, CELCIUS);
	assertEquals("DP Calc: dew point set to null", 0.05367608149, 
		     data.getDewPoint(), THRESHOLD);
    }
    
    @Test (expected = CalculationWarning.class)
	public void dewPointCalculationInvalidTemperature() throws 
	    CalculationWarning, ConversionException, InvalidValueWarning  {
	data.setRelativeHumidity(10.0);
	data.setTemperature(-243.5, CELCIUS);
    }
    
    @Test (expected = CalculationWarning.class)
	public void dewPointCalculationNonPositiveRH() throws CalculationWarning,
							      ConversionException, InvalidValueWarning  {
	data.setTemperature(10.0, CELCIUS);
	data.setRelativeHumidity(0.0);
    }
    
    @Test public void dewPointCalculationRelativeHumidity() throws 
	CalculationWarning, ConversionException, InvalidValueWarning {
	data.setTemperature(10.0, CELCIUS);
	assertEquals("DP Calc: temp set sanity check", 10.0, 
		     data.getTemperature(), THRESHOLD);
	assertNull("DP Calc: temp only set", data.getDewPoint());
	
	data.setRelativeHumidity(100.0);
	assertEquals("DP Calc: rh now set", 10.0, data.getDewPoint(), 
		     THRESHOLD);
	data.setRelativeHumidity(50.0);
	assertEquals("DP Calc: change rh", 0.05367608149, data.getDewPoint(), 
		     THRESHOLD);
	data.setRelativeHumidity(50.0);
	assertEquals("DP Calc: change rh same", 0.05367608149, 
		     data.getDewPoint(), THRESHOLD);
	
	// Make sure a bad rh causes the calculated dew point to be null.
	try { data.setRelativeHumidity(0.0); }
	catch (CalculationWarning e) {
	    assertNull("DP Calc: bad rh value", data.getDewPoint());
	    data.setRelativeHumidity(50.0);
	}
	
	data.setRelativeHumidity(null);
	assertNull("DP Calc: change rh null", data.getDewPoint());
    }
    
    @Test public void dewPointCalculationTemperature() throws
	CalculationWarning, ConversionException, InvalidValueWarning {
	data.setRelativeHumidity(100.0);
	assertEquals("DP Calc: rh set sanity check", 100.0, 
		     data.getRelativeHumidity(), THRESHOLD);
	assertNull("DP Calc: dew point - rh only set", data.getDewPoint());
	
	data.setTemperature(10.0, CELCIUS);
	assertEquals("DP Calc: dew point - temp now set", 10.0, 
		     data.getDewPoint(), THRESHOLD);
	data.setTemperature(0.0, CELCIUS);
	assertEquals("DP Calc: dew point - change temp", 0.0, 
		     data.getDewPoint(), THRESHOLD);
	data.setTemperature(32.0, FAHRENHEIT);
	assertEquals("DP Calc: dew point - change temp same", 0.0, 
		     data.getDewPoint(), THRESHOLD);
	
	// Make sure a bad temp value causes the calculated dew point to be 
	// null.
	try { data.setTemperature(-243.5, CELCIUS); }
	catch (CalculationWarning e) {
	    assertNull("DP Calc: bad temp change", data.getDewPoint());
	    data.setTemperature(0.0, CELCIUS);
	}		
	
	data.setTemperature(null, CELCIUS);
	assertNull("DP Calc: dew point - change temp null", data.getDewPoint());
    }
    
    @Test (expected = ConversionException.class)
	public void dewPointNullUnit() throws CalculationWarning, 
					      ConversionException, InvalidValueWarning  {
	data.setDewPoint(0.0, null);
    }
    
    @Test public void elevation() throws CalculationWarning,
					 ConversionException, InvalidValueWarning {
	assertNull("Elevation: default", data.getElevation());
	data.setElevation(10.0, METERS);
	assertEquals("Elevation: set m", 10.0, data.getElevation(), THRESHOLD);
	data.setElevation(1.0, KILOMETERS);
	assertEquals("Elevation: set diff unit", 1000.0, data.getElevation(), 
		     THRESHOLD);
	data.setElevation(0.0, METERS);
	assertEquals("Elevation: zero", 0.0, data.getElevation(), THRESHOLD);
	data.setElevation(-100.0, METERS);
	assertEquals("Elevation: negative", -100.0, data.getElevation(), 
		     THRESHOLD);
	data.setElevation(null, METERS);
	assertNull("Elevation: null", data.getElevation());
    }
    
    @Test (expected = ConversionException.class)
	public void elevationNullUnit() throws CalculationWarning, 
					       ConversionException, InvalidValueWarning {
	data.setElevation(10.0, null);
    }
    
    @Test public void pressure() throws CalculationWarning, ConversionException,
					InvalidValueWarning  {
	assertNull("Pressure: default", data.getPressure());
	data.setPressure(1000.0, MILLIBARS);
	assertEquals("Pressure: set m", 1000.0, data.getPressure(), THRESHOLD);
	data.setPressure(1.0, ATMOSPHERES);
	assertEquals("Pressure: set diff unit", 1013.25, data.getPressure(), 
		     THRESHOLD);
	data.setPressure(0.0, MILLIBARS);
	assertEquals("Pressure: zero", 0.0, data.getPressure(), THRESHOLD);
	data.setPressure(-100.0, MILLIBARS);
	assertEquals("Pressure: negative", -100.0, data.getPressure(), 
		     THRESHOLD);
	data.setPressure(null, MILLIBARS);
	assertNull("Pressure: null", data.getPressure());
    }
    
    @Test (expected = ConversionException.class)
	public void pressureNullUnit() throws CalculationWarning, ConversionException, InvalidValueWarning {
	data.setPressure(1000.0, null);
    }
    
    @Test public void relativeHumidity() throws CalculationWarning,
						InvalidValueWarning {
	assertNull("RH: default", data.getRelativeHumidity());
	
	data.setRelativeHumidity(12.0);
	assertEquals("RH: set value", 12.0, data.getRelativeHumidity(), 
		     THRESHOLD);
	data.setRelativeHumidity(0.0);
	assertEquals("RH: zero", 0.0, data.getRelativeHumidity(), THRESHOLD);
	data.setRelativeHumidity(-10.0);
	assertEquals("RH: negative", -10.0, data.getRelativeHumidity(),
		     THRESHOLD);
	data.setRelativeHumidity(null);
	assertNull("RH: set null", data.getRelativeHumidity());
    }
    
    @Test public void relativeHumidityCalculation() throws CalculationWarning,
							   ConversionException, InvalidValueWarning  {
	assertNull("RH Calc: sanity check", data.getDewPoint());
	
	data.setTemperature(15.0, CELCIUS);
	data.setDewPoint(15.0, CELCIUS);
	assertEquals("RH Calc: calculated", 100.0, data.getRelativeHumidity(),
		     THRESHOLD);
	data.setRelativeHumidity(11.11);
	assertEquals("RH Calc: manual override", 11.11, 
		     data.getRelativeHumidity(), THRESHOLD);
	data.setTemperature(10.0, CELCIUS);
	assertEquals("RH Calc: temp change in override", 11.11, 
		     data.getRelativeHumidity(), THRESHOLD);
	data.setDewPoint(10.0, CELCIUS);
	assertEquals("RH Calc: dp change in override", 11.11, 
		     data.getRelativeHumidity(), THRESHOLD);
	data.setRelativeHumidity(null);
	assertEquals("RH Calc: rh set to null", 100.0, 
		     data.getRelativeHumidity(), THRESHOLD);
    }
    
    @Test public void relativeHumidityCalculationDewPoint() throws
	CalculationWarning, ConversionException, InvalidValueWarning {
	data.setTemperature(10.0, CELCIUS);
	assertEquals("RH Calc: temp set sanity check", 10.0, 
		     data.getTemperature(), THRESHOLD);
	assertNull("RH Calc: temp only set", data.getRelativeHumidity());
	
	data.setDewPoint(10.0, CELCIUS);
	assertEquals("RH Calc: dp now set", 100.0, data.getRelativeHumidity(),
		     THRESHOLD);
	data.setDewPoint(15.0, CELCIUS);
	assertEquals("RH Calc: change dp", 138.8601424483, 
		     data.getRelativeHumidity(), THRESHOLD);
	data.setDewPoint(15.0, CELCIUS);
	assertEquals("RH Calc: change dp same", 138.8601424483, 
		     data.getRelativeHumidity(), THRESHOLD);
	
	// Make sure the bad dp value causes the rh to be null.
	try { data.setDewPoint(-243.5, CELCIUS); }
	catch (CalculationWarning e) {
	    assertNull("RH Calc: bad dp", data.getRelativeHumidity());
	    data.setDewPoint(15.0, CELCIUS);
	}
	
	data.setDewPoint(null, CELCIUS);
	assertNull("RH Calc: change dp null", data.getRelativeHumidity());
    }
    
    @Test (expected = CalculationWarning.class)
	public void relativeHumidityCalculationInvalidDewPoint() throws
	    CalculationWarning, ConversionException, InvalidValueWarning {
	data.setTemperature(15.0, CELCIUS);
	data.setDewPoint(-243.5, CELCIUS);
    }
    
    @Test (expected = CalculationWarning.class)
	public void relativeHumidityCalculationInvalidTemperature() throws
	    CalculationWarning, ConversionException, InvalidValueWarning {
	data.setDewPoint(12.0, CELCIUS);
	data.setTemperature(-243.5, CELCIUS);
    }
    
    @Test public void relativeHumidityCalculationTemperature() throws
	CalculationWarning, ConversionException, InvalidValueWarning {
	data.setDewPoint(15.0, CELCIUS);
	assertEquals("RH Calc: dew point set sanity check", 15.0, 
		     data.getDewPoint(), THRESHOLD);
	assertNull("RH Calc: rh - dp only set", data.getRelativeHumidity());
	
	data.setTemperature(15.0, CELCIUS);
	assertEquals("RH Calc: rh - temp now set", 100.0, 
		     data.getRelativeHumidity(), THRESHOLD);
	data.setTemperature(0.0, CELCIUS);
	assertEquals("RH Calc: rh - change temp", 278.8039027806, 
		     data.getRelativeHumidity(), THRESHOLD);
	data.setTemperature(32.0, FAHRENHEIT);
	assertEquals("RH Calc: rh - change temp same", 278.8039027806, 
		     data.getRelativeHumidity(), THRESHOLD);
	
	// Make sure the bad temp value causes the rh to be null.
	try { data.setTemperature(-243.5, CELCIUS); }
	catch (CalculationWarning e) {
	    assertNull("RH Calc: bad temp", data.getRelativeHumidity());
	    data.setTemperature(0.0, CELCIUS);
	}
	
	data.setTemperature(null, CELCIUS);
	assertNull("RH Calc: rh - change temp null", 
		   data.getRelativeHumidity());
    }
    
    @Test public void seaLevelPressureCalculation() {
	assertNull("SLP Calc: sanity check", data.getSeaLevelPressure());
    }
    
    @Test public void seaLevelPressureCalculationDewPoint() throws
	CalculationWarning, ConversionException, InvalidValueWarning {
	data.setElevation(1000.0, METERS);
	data.setPressure(980.0, MILLIBARS);
	data.setTemperature(15.0, CELCIUS);
	assertNull("SLP Calc: slp - no dp set", data.getSeaLevelPressure());
	
	data.setDewPoint(12.0, CELCIUS);
	assertEquals("SLP Calc: slp - set dp", 1104.12283361399, 
		     data.getSeaLevelPressure(), THRESHOLD);
	try { data.setDewPoint(-243.5, CELCIUS); }
	catch (CalculationWarning e) {
	    assertNull("SLP Calc: bad dp", data.getSeaLevelPressure());
	}
	data.setDewPoint(15.0, CELCIUS);
	assertEquals("SLP Calc: slp - change dp", 1103.965927981864, 
		     data.getSeaLevelPressure(), THRESHOLD);
	data.setDewPoint(null, CELCIUS);
	assertNull("SLP Calc: slp - null dp", data.getSeaLevelPressure());
    }
    
    @Test public void seaLevelPressureCalculationElevation() throws
	CalculationWarning, ConversionException, InvalidValueWarning {
	data.setDewPoint(12.0, CELCIUS);
	data.setPressure(980.0, MILLIBARS);
	data.setTemperature(15.0, CELCIUS);
	assertNull("SLP Calc: slp - no elev set", data.getSeaLevelPressure());
	
	data.setElevation(1000.0, METERS);
	assertEquals("SLP Calc: slp - set elev", 1104.12283361399, 
		     data.getSeaLevelPressure(), THRESHOLD);
	data.setElevation(100.0, METERS);
	assertEquals("SLP Calc: slp - change elev", 991.6373151323, 
		     data.getSeaLevelPressure(), THRESHOLD);
	data.setElevation(null, METERS);
	assertNull("SLP Calc: slp - null elev", data.getSeaLevelPressure());
    }
	
    @Test (expected = CalculationWarning.class)
	public void seaLevelPressureCalculationInvalidDewPoint() throws
	    CalculationWarning, ConversionException, InvalidValueWarning {
	data.setElevation(1000.0, METERS);
	data.setPressure(980.0, MILLIBARS);
	data.setTemperature(15.0, CELCIUS);
	data.setDewPoint(-243.5, CELCIUS);
    }
    
    @Test public void seaLevelPressureCalculationPressure() throws
	CalculationWarning, ConversionException, InvalidValueWarning {
	data.setDewPoint(12.0, CELCIUS);
	data.setElevation(1000.0, METERS);
	data.setTemperature(15.0, CELCIUS);
	assertNull("SLP Calc: slp - no press set", data.getSeaLevelPressure());
	
	data.setPressure(980.0, MILLIBARS);
	assertEquals("SLP Calc: slp - set press", 1104.12283361399, 
		     data.getSeaLevelPressure(), THRESHOLD);
	data.setPressure(100.0, MILLIBARS);
	assertEquals("SLP Calc: slp - change press", 112.0174011348,
		     data.getSeaLevelPressure(), THRESHOLD);
	data.setPressure(null, MILLIBARS);
	assertNull("SLP Calc: slp - null press", data.getSeaLevelPressure());
    }
    
    @Test public void seaLevelPressureCalculationRelativeHumidity() 
	throws CalculationWarning, ConversionException, InvalidValueWarning {
	data.setElevation(1000.0, METERS);
	data.setPressure(980.0, MILLIBARS);
	data.setTemperature(15.0, CELCIUS);
	assertNull("SLP Calc: slp - no rh set", data.getSeaLevelPressure());
	
	data.setRelativeHumidity(50.0);
	assertEquals("SLP Calc: slp - set rh", 1104.4079229682172,
		     data.getSeaLevelPressure(), THRESHOLD);
	data.setRelativeHumidity(100.0);
	assertEquals("SLP Calc: slp - change rh", 1103.965927981864,
		     data.getSeaLevelPressure(), THRESHOLD);
	data.setRelativeHumidity(null);
	assertNull("SLP Calc: slp - null rh", data.getSeaLevelPressure());
    }
    
    @Test public void seaLevelPressureCalculationTemperature() throws
	CalculationWarning, ConversionException, InvalidValueWarning {
	data.setDewPoint(12.0, CELCIUS);
	data.setElevation(1000.0, METERS);
	data.setPressure(980.0, MILLIBARS);
	assertNull("SLP Calc: slp - no temp set", data.getSeaLevelPressure());
	
	data.setTemperature(15.0, CELCIUS);
	assertEquals("SLP Calc: slp - set temp", 1104.12283361399, 
		     data.getSeaLevelPressure(), THRESHOLD);
	data.setTemperature(10.0, CELCIUS);
	assertEquals("SLP Calc: slp - change temp", 1106.4772994309,
		     data.getSeaLevelPressure(), THRESHOLD);
	data.setTemperature(null, CELCIUS);
	assertNull("SLP Calc: slp - null temp", data.getSeaLevelPressure());
    }
    
    @Test public void temperature() throws CalculationWarning,
					   ConversionException, InvalidValueWarning  {
	assertNull("Temperature: default", data.getTemperature());
	data.setTemperature(10.0, CELCIUS);
	assertEquals("Temperature: set C", 10.0, data.getTemperature(), 
		     THRESHOLD);
	data.setTemperature(32.0, FAHRENHEIT);
	assertEquals("Temperature: set F", 0.0, data.getTemperature(),
		     THRESHOLD);
	data.setTemperature(-10.0, CELCIUS);
	assertEquals("Temperature: negative", -10.0, data.getTemperature(),
		     THRESHOLD);
	data.setTemperature(0.0, CELCIUS);
	assertEquals("Temperature: zero", 0.0, data.getTemperature(),
		     THRESHOLD);
	data.setTemperature(null, CELCIUS);
	assertNull("Temperature: null", data.getTemperature());
    }
    
    @Test (expected = ConversionException.class)
	public void temperatureNullUnit() throws CalculationWarning,
						 ConversionException, InvalidValueWarning {
	data.setTemperature(10.0, null);
    }
    
    private class TestPTHData extends PTHData {
	
	public TestPTHData(boolean calculationsAllowed) {
	    super(calculationsAllowed);
	}
	
	@Override
	    public LengthUnit getDefaultLengthUnit() { return METERS; }
	
	@Override
	    public PressureUnit getDefaultPressureUnit() { return MILLIBARS; }
	
	@Override
	    public TemperatureUnit getDefaultTemperatureUnit() { return CELCIUS; }
	
	@Override
	    protected Double validateDewPoint(Double dewPoint) {
	    return dewPoint;
	}
	
	@Override
	    protected Double validateElevation(Double elevation) {
	    return elevation;
	}
	
	@Override
	    protected Double validatePressure(Double pressure) {
	    return pressure;
	}
	
	@Override
	    protected Double validateRelativeHumidity(Double rh) {
	    return rh;
	}
	
	@Override
	    protected Double validateSeaLevelPressure(Double slpressure) {
	    return slpressure;
	}
	
	@Override
	    protected Double validateTemperature(Double temperature) {
	    return temperature;
	}
    }
}
