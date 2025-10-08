package dmg.record;

import static dmg.util.VelocityUtils.*;
import static org.junit.Assert.*;
import dmg.util.*;
import dmg.util.VelocityUtils.*;

import org.junit.*;

public class WindDataTest {
	
    private static final Double THRESHOLD = Math.pow(1, -10);
	
    private WindData data, nocalc;
	
    @Before public void setup() {
	data = new WindTestData(true, METERS_PER_SECOND);
	nocalc = new WindTestData(false, METERS_PER_SECOND);
    }
	
    @Test public void differentDefaultUnits() throws CalculationWarning,
						     ConversionException, InvalidValueWarning {
	data = new WindTestData(true, MILES_PER_HOUR);
	
	data.setUComponent(10.0, MILES_PER_HOUR);
	data.setVComponent(10.0, MILES_PER_HOUR);
	data.setWindSpeed(5.0, METERS_PER_SECOND);
	data.setWindDirection(180.0);
	
	assertEquals("Different Units: u component", 10.0, data.getUComponent(),
		     THRESHOLD);
	assertEquals("Different Units: v component", 10.0, data.getVComponent(),
		     THRESHOLD);
	assertEquals("Different Units: wind speed", 11.1846814603, 
		     data.getWindSpeed(), THRESHOLD);
	assertEquals("Different Units: wind direction", 180.0, 
		     data.getWindDirection(), THRESHOLD);
    }
    
	@Test (expected = ConversionException.class)
	public void uComponentNullUnit() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		data.setUComponent(10.0, null);
	}
	
	@Test (expected = ConversionException.class)
	public void vComponentNullUnit() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		data.setVComponent(10.0, null);
	}
	
	@Test public void uvComponents() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		// Test the default u/v wind value.
		assertNull("U Wind: default",data.getUComponent());
		assertNull("V Wind: default",data.getVComponent());
		
		// Test a valid value.
		data.setUComponent(10.0, METERS_PER_SECOND);
		data.setVComponent(10.0, METERS_PER_SECOND);
		assertEquals("U Wind: value",10.0,data.getUComponent(),THRESHOLD);
		assertEquals("V Wind: value",10.0,data.getVComponent(),THRESHOLD);
		// Test a valid value with different units
		data.setUComponent(10.0, FEET_PER_SECOND);
		data.setVComponent(5.0, FEET_PER_SECOND);
		assertEquals("U Wind: diff unit",3.048,data.getUComponent(),THRESHOLD);
		assertEquals("V Wind: diff unit",1.524,data.getVComponent(),THRESHOLD);
		// Test setting a null U value.
		data.setUComponent(null, METERS_PER_SECOND);
		assertNull("UComponent: null U",data.getUComponent());
		assertEquals("VComponent: null U",1.524,data.getVComponent(),THRESHOLD);
		// Test setting a null V value.
		data.setVComponent(null,METERS_PER_SECOND);
		assertNull("UComponent: null V",data.getUComponent());
		assertNull("VComponent: null V",data.getVComponent());
	}
	
    @Test public void uvComponentCalculations() throws CalculationWarning, 
						       ConversionException, InvalidValueWarning {
	// Test the default u/v values.
	assertNull("U Wind Calc: default",data.getUComponent());
	assertNull("V Wind Calc: default",data.getVComponent());
	
	// Both values set
	data.setWindSpeed(10.0, METERS_PER_SECOND);
	data.setWindDirection(180.0);
	assertEquals("U Wind Calc: two real values",0.0,
		     data.getUComponent(),THRESHOLD);
	assertEquals("V Wind Calc: two real values",10.0,
		     data.getVComponent(),THRESHOLD);
	
	// Set to a bad wind speed
	try {
	    data.setWindSpeed(-1.0, METERS_PER_SECOND);
	} catch (InvalidValueWarning e) {
	    assertNull("U Wind Calc: negative wind speed", 
		       data.getUComponent());
	    assertNull("V Wind Calc: negative wind speed", 
		       data.getVComponent());
	    data.setWindSpeed(10.0, METERS_PER_SECOND);
	}
	
	// Set to a negative wind direction
	try {
	    data.setWindDirection(-1.0);
	} catch (InvalidValueWarning e) {
	    assertNull("U Wind Calc: negative wind direction",
		       data.getUComponent());
	    assertNull("V Wind Calc: negative wind direction", 
		       data.getVComponent());
	    data.setWindDirection(180.0);
	}
	
	// Set to a bad wind direction
	try {
	    data.setWindDirection(360.1);
	} catch (InvalidValueWarning e) {
	    assertNull("U Wind Calc: over positive wind direction",
		       data.getUComponent());
	    assertNull("V Wind Calc: over positive wind direction",
		       data.getVComponent());
	    data.setWindDirection(180.0);
	}
	
	// Change only the wind speed
	data.setWindSpeed(15.0, METERS_PER_SECOND);
	assertEquals("U Wind Calc: change speed",0.0,data.getUComponent(),
		     THRESHOLD);
	assertEquals("V Wind Calc: change speed",15.0,data.getVComponent(),
		     THRESHOLD);
	
	// Change only the wind direction
	data.setWindDirection(0.0);
	assertEquals("U Wind Calc: change direction",0.0,data.getUComponent(),
		     THRESHOLD);
	assertEquals("V Wind Calc: change direction",-15.0,data.getVComponent(),
		     THRESHOLD);
	
	// Null the wind speed
	data.setWindSpeed(null, METERS_PER_SECOND);
	assertNull("U Wind Calc: null speed",data.getUComponent());
	assertNull("V Wind Calc: null speed",data.getVComponent());
	
	// Put the wind speed back
	data.setWindSpeed(15.0, METERS_PER_SECOND);
	assertEquals("U Wind Calc: return speed",0.0,data.getUComponent(),
		     THRESHOLD);
	assertEquals("V Wind Calc: return speed",-15.0,data.getVComponent(),
		     THRESHOLD);
	
	// Null the wind direction
	data.setWindDirection(null);
	assertNull("U Wind Calc: null direction",data.getUComponent());
	assertNull("V Wind Calc: null direction",data.getVComponent());
	
	// Put the direction back
	data.setWindDirection(0.0);
	assertEquals("U Wind Calc: return direction",0.0,data.getUComponent(),
		     THRESHOLD);
	assertEquals("V Wind Calc: return direction",-15.0,data.getVComponent(),
		     THRESHOLD);
	
	// Manually set the U Value
	data.setUComponent(2.0, METERS_PER_SECOND);
	assertEquals("U Wind Calc: manual U only",2.0,data.getUComponent(),
		     THRESHOLD);
	assertNull("V Wind Calc: manual U only",data.getVComponent());
	
	// Manually set the V Value
	data.setUComponent(null, METERS_PER_SECOND);
	data.setVComponent(6.0, METERS_PER_SECOND);
	assertNull("U Wind Calc: manual V only",data.getUComponent());
	assertEquals("V Wind Calc: manual V only",6.0,data.getVComponent(),
		     THRESHOLD);
	
	// Manually set both U and V
	data.setUComponent(10.0, METERS_PER_SECOND);
	data.setVComponent(10.0, METERS_PER_SECOND);
	assertEquals("U Wind Calc: manual both",10.0,data.getUComponent(),
		     THRESHOLD);
	assertEquals("V Wind Calc: manual both",10.0,data.getVComponent(),
		     THRESHOLD);
	
	// Change the wind speed after a manual change
	data.setWindSpeed(1.5, METERS_PER_SECOND);
	assertEquals("U Wind Calc: speed change after manual",10.0,
		     data.getUComponent(),THRESHOLD);
	assertEquals("V Wind Calc: speed change after manual",10.0,
		     data.getVComponent(),THRESHOLD);
	
	// Change the wind direction after a manual change
	data.setWindDirection(180.0);
	assertEquals("U Wind Calc: direction change after manual",10.0,
		     data.getUComponent(),THRESHOLD);
	assertEquals("V Wind Calc: direction change after manual",10.0,
		     data.getVComponent(),THRESHOLD);
	
	// Change the U and V back to null to make sure the calcuation starts 
	// again.
	data.setUComponent(null, METERS_PER_SECOND);
	data.setVComponent(null, METERS_PER_SECOND);
	assertEquals("U Wind Calc: nulled u and v",0.0,data.getUComponent(),
		     THRESHOLD);
	assertEquals("V Wind Calc: nulled u and v",1.5,data.getVComponent(),
		     THRESHOLD);
    }

    @Test public void uvComponentCalculationsNotAllowed() throws 
	CalculationWarning, ConversionException, InvalidValueWarning
								 
    {
	nocalc.setWindSpeed(5.0, METERS_PER_SECOND);
	nocalc.setWindDirection(150.0);

	assertNull("U Wind Calc: calculations not allowed", nocalc.getUComponent());
	assertNull("V Wind Calc: calculations not allowed", nocalc.getVComponent());
    }

    
    @Test (expected = InvalidValueWarning.class)
	public void uvComponentCalculationsNegativeWindDirection() throws
	    CalculationWarning, ConversionException, InvalidValueWarning {
	data.setWindSpeed(5.0, METERS_PER_SECOND);
	data.setWindDirection(-1.0);
    }
    
    @Test (expected = InvalidValueWarning.class)
	public void uvComponentCalculationsNegativeWindSpeed() throws
	    CalculationWarning, ConversionException, InvalidValueWarning {
	data.setWindDirection(4.0);
	data.setWindSpeed(-1.0, METERS_PER_SECOND);
    }
    
    @Test (expected = InvalidValueWarning.class)
	public void uvComponentCalculationsOverPositiveWindDirection() throws
	    CalculationWarning, ConversionException, InvalidValueWarning {
	data.setWindSpeed(5.0, METERS_PER_SECOND);
	data.setWindDirection(360.1);
    }
    
    @Test public void windSpeedAndDirection() throws CalculationWarning,
						     ConversionException, InvalidValueWarning {
	// Test the default wind values.
	assertNull("Wind Speed: default",data.getWindSpeed());
	assertNull("Wind Direction: default",data.getWindDirection());
	// Test a valid value.
	data.setWindSpeed(10.0, METERS_PER_SECOND);
	data.setWindDirection(10.0);
	assertEquals("Wind Speed: value",10.0,data.getWindSpeed(),THRESHOLD);
		assertEquals("Wind Direction: value",10.0,data.getWindDirection(),
				THRESHOLD);
		// Test a valid value with different units
		data.setWindSpeed(10.0, FEET_PER_SECOND);
		assertEquals("Wind Speed: diff unit",3.048,data.getWindSpeed(),
				THRESHOLD);
		assertEquals("Wind Direction: diff unit",10.0,data.getWindDirection(),
				THRESHOLD);
		// Test setting a null wind speed value.
		data.setWindSpeed(null, METERS_PER_SECOND);
		assertNull("Wind Speed: null wind speed",data.getWindSpeed());
		assertEquals("Wind Direction: null wind speed",10.0,
				data.getWindDirection(),THRESHOLD);
		// Test setting a null wind direction value.
		data.setWindDirection(null);
		assertNull("Wind Speed: null wind direction",data.getWindSpeed());
		assertNull("Wind Direction: null wind direction",
				data.getWindDirection());
	}
	
	@Test public void windSpeedAndDirectionCalculations() throws
	CalculationWarning, ConversionException, InvalidValueWarning {
		// Test the default u/v values.
		assertNull("Wind Speed Calc: default",data.getWindSpeed());
		assertNull("Wind Direction Calc: default",data.getWindDirection());
		
		// Both values set
		data.setUComponent(0.0, METERS_PER_SECOND);
		data.setVComponent(10.0, METERS_PER_SECOND);
		assertEquals("Wind Speed Calc: two real values",10.0,
				data.getWindSpeed(),THRESHOLD);
		assertEquals("Wind Direction Calc: two real values",180.0,
				data.getWindDirection(),THRESHOLD);
		
		// Change only the U component
		data.setUComponent(15.0, METERS_PER_SECOND);
		assertEquals("Wind Speed Calc: change U",18.0277563773,
				data.getWindSpeed(),THRESHOLD);
		assertEquals("Wind Direction Calc: change U",236.309932474,
				data.getWindDirection(),THRESHOLD);
		
		// Change only the V component
		data.setVComponent(0.0, METERS_PER_SECOND);
		assertEquals("Wind Speed Calc: change V",15.0,
				data.getWindSpeed(),THRESHOLD);
		assertEquals("Wind Direction Calc: change V",270.0,
				data.getWindDirection(),THRESHOLD);
		
		// Null the U component
		data.setUComponent(null, METERS_PER_SECOND);
		assertNull("Wind Speed Calc: null U",data.getWindSpeed());
		assertNull("Wind Direction Calc: null U",data.getWindDirection());
		
		// Put the U component back
		data.setUComponent(15.0, METERS_PER_SECOND);
		assertEquals("Wind Speed Calc: return U",15.0,data.getWindSpeed(),
				THRESHOLD);
		assertEquals("Wind Direction Calc: return U",270.0,
				data.getWindDirection(),THRESHOLD);
		
		// Null the v component
		data.setVComponent(null, METERS_PER_SECOND);
		assertNull("Wind Speed Calc: null V",data.getWindSpeed());
		assertNull("Wind Direction Calc: null V",data.getWindDirection());
		
		// Put the V component back
		data.setVComponent(0.0, METERS_PER_SECOND);
		assertEquals("Wind Speed Calc: return V",15.0,data.getWindSpeed(),
				THRESHOLD);
		assertEquals("Wind Direction Calc: return V",270.0,
				data.getWindDirection(),THRESHOLD);
		
		// Manually set the speed
		data.setWindSpeed(2.0, METERS_PER_SECOND);
		assertEquals("Wind Speed Calc: manual speed",2.0,
				data.getWindSpeed(),THRESHOLD);
		assertNull("Wind Direction Calc: manual speed",
				data.getWindDirection());
		
		// Manually set the direction
		data.setWindSpeed(null, METERS_PER_SECOND);
		data.setWindDirection(6.0);
		assertNull("Wind Speed Calc: manual direction",data.getWindSpeed());
		assertEquals("Wind Direction Calc: manual direction",6.0,
				data.getWindDirection(),THRESHOLD);
		
		// Manually set both speed and direction
		data.setWindSpeed(10.0, METERS_PER_SECOND);
		data.setWindDirection(10.0);
		assertEquals("Wind Speed Calc: manual both",10.0,data.getWindSpeed(),
				THRESHOLD);
		assertEquals("Wind Direction Calc: manual both",10.0,
				data.getWindDirection(),THRESHOLD);
		
		// Change the u component after a manual change
		data.setUComponent(15.0, METERS_PER_SECOND);
		assertEquals("Wind Speed Calc: U change after manual",10.0,
				data.getWindSpeed(),THRESHOLD);
		assertEquals("Wind Direction Calc: U change after manual",10.0,
				data.getWindDirection(),THRESHOLD);
		
		// Change the v component after a manual change
		data.setVComponent(0.0, METERS_PER_SECOND);
		assertEquals("Wind Speed Calc: V change after manual",10.0,
				data.getWindSpeed(),THRESHOLD);
		assertEquals("Wind Direction Calc: V change after manual",10.0,
				data.getWindDirection(),THRESHOLD);
		
		// Change the speed and direction back to null to make sure the 
		// calcuation starts again.
		data.setWindSpeed(null, METERS_PER_SECOND);
		data.setWindDirection(null);
		assertEquals("Wind Speed Calc: nulled speed and direction",15.0,
				data.getWindSpeed(),THRESHOLD);
		assertEquals("Wind Direction Calc: nulled speed and direction",270.0,
				data.getWindDirection(),THRESHOLD);
	}
	
    @Test public void windSpeedDirectionCalculationsNotAllowed() throws
	CalculationWarning, ConversionException, InvalidValueWarning
    {
	nocalc.setUComponent(10.0, METERS_PER_SECOND);
	nocalc.setVComponent(1.0, METERS_PER_SECOND);

	assertNull("Wind Speed Calc: calculations not allowed", nocalc.getWindSpeed());
	assertNull("Wind Direction Calc: calculations not allowed", nocalc.getWindDirection());
    }

    @Test (expected = ConversionException.class)
	public void windSpeedNullUnit() throws CalculationWarning,
					       ConversionException, InvalidValueWarning {
	data.setWindSpeed(10.0, null);
    }
    
	
	
    private class WindTestData extends WindData {
	
	private VelocityUnit defaultUnit;
	
	public WindTestData(boolean calculationsAllowed, VelocityUnit unit) { 
	    super(calculationsAllowed);
	    defaultUnit = unit; 
	}
	
	@Override
	    public VelocityUnit getDefaultVelocityUnit() { return defaultUnit; }
	
	@Override
	    protected Double validateUComponent(Double uComponent) {
	    return uComponent;
	}
	
	@Override
	    protected Double validateVComponent(Double vComponent) {
	    return vComponent;
	}
	
	@Override
	    protected Double validateWindDirection(Double direction) {
	    return (direction == null || direction < 0 || direction > 360) ?
		null : direction;
	}
	
	@Override
	    protected Double validateWindSpeed(Double speed) {
	    return (speed == null || speed < 0) ? null : speed;
	}
    }
}
