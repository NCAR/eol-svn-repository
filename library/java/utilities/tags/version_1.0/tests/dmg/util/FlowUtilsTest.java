package dmg.util;

import static dmg.util.FlowUtils.*;
import static org.junit.Assert.*;

import org.junit.*;

public class FlowUtilsTest {

	private static final Double THRESHOLD = Math.pow(1, -10);
	
	@Test public void cubicFeetPerSecondConversions() 
	throws ConversionException {
		assertEquals("ft3/s -> ft3/s", 100.0, convertFlow(100.0, 
				CUBIC_FEET_PER_SECOND, CUBIC_FEET_PER_SECOND), THRESHOLD);
		assertEquals("ft3/s -> m3/s", 2.8316846592, convertFlow(100.0, 
				CUBIC_FEET_PER_SECOND, CUBIC_METERS_PER_SECOND), THRESHOLD);
	}
	
	@Test public void cubicMetersPerSecondConversions() 
	throws ConversionException {
		assertEquals("m3/s -> ft3/s", 353.1466672149, convertFlow(10.0, 
				CUBIC_METERS_PER_SECOND, CUBIC_FEET_PER_SECOND), THRESHOLD);
		assertEquals("m3/s -> m3/s", 10.0, convertFlow(10.0, 
				CUBIC_METERS_PER_SECOND, CUBIC_METERS_PER_SECOND), THRESHOLD);
	}

	@Test (expected = ConversionException.class)
	public void radiationConversionNullInputUnit() throws ConversionException {
		convertFlow(10.0, null, CUBIC_FEET_PER_SECOND);
	}
	
	@Test (expected = ConversionException.class)
	public void radiationConversionNullOutputUnit() throws ConversionException {
		convertFlow(10.0, CUBIC_FEET_PER_SECOND, null);
	}
	
	@Test public void radiationConverionNullValue() throws ConversionException {
		assertNull("convertFlow: null value", convertFlow(null, 
				CUBIC_FEET_PER_SECOND, CUBIC_METERS_PER_SECOND));
	}
}
