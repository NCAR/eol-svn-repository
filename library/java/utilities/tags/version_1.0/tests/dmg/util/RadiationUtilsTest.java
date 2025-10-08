package dmg.util;

import static dmg.util.RadiationUtils.*;
import static org.junit.Assert.*;

import org.junit.*;

public class RadiationUtilsTest {
	
	private static final Double THRESHOLD = Math.pow(1, -10);

	@Test public void langlyConversions() throws ConversionException {
		assertEquals("langly -> langly", 10.0, convertRadiation(10.0,
				LANGLY, LANGLY), THRESHOLD);
		assertEquals("langly -> w/m2", 100.0, convertRadiation(10.0,
				LANGLY, WATTS_PER_SQUARE_METER), THRESHOLD);
	}
	
	@Test (expected = ConversionException.class)
	public void radiationConversionNullInputUnit() throws ConversionException {
		convertRadiation(10.0, null, LANGLY);
	}
	
	@Test (expected = ConversionException.class)
	public void radiationConversionNullOutputUnit() throws ConversionException {
		convertRadiation(10.0, LANGLY, null);
	}
	
	@Test public void radiationConverionNullValue() throws ConversionException {
		assertNull("convertRadiation: null value", convertRadiation(null, 
				LANGLY, WATTS_PER_SQUARE_METER));
	}

	@Test public void wattsPerSquareMeterConversions() throws
	ConversionException {
		assertEquals("w/m2 -> langly", 1.0, convertRadiation(10.0, 
				WATTS_PER_SQUARE_METER, LANGLY), THRESHOLD);
		assertEquals("w/m2 -> w/m2", 10.0, convertRadiation(10.0, 
				WATTS_PER_SQUARE_METER, WATTS_PER_SQUARE_METER), THRESHOLD);
	}
}
