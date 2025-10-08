package dmg.util;

import static org.junit.Assert.*;

import org.junit.*;

public class StateCodeMapTest {

	private static StateCodeMap codeMap;
	
	@BeforeClass public static void setUpBeforeClass() throws Exception {
		codeMap = StateCodeMap.getInstance();
	}
	
	@Test public void codeStateCA() throws ConversionException {
		assertEquals("Code State: 3, CA", "Manitoba", 
				codeMap.getStateCode("CA", 3));
	}
	
	@Test (expected = ConversionException.class)
	public void codeStateNullCountry() throws ConversionException {
		codeMap.getStateCode(null, "CO");
	}
	
	@Test public void codeStateMX() throws ConversionException {
		assertEquals("Code State: 25, MX","Sinaloa",
				codeMap.getStateCode("MX", 25));
	}
	
	@Test public void codeStatePR() throws ConversionException {
		assertEquals("Code State: PR, 99","XX",codeMap.getStateCode("PR", 99));
	}
	
	@Test (expected = ConversionException.class)
	public void codeStateUnknownCountry() throws ConversionException {
		codeMap.getStateCode("NOTHING", 8);
	}
	
	@Test (expected = ConversionException.class)
	public void codeStateUnknownState() throws ConversionException {
		codeMap.getStateCode("US", -1);
	}
	
	@Test public void codeStateUnspecified() throws ConversionException {
		assertEquals("Code State: XX, 99","XX",codeMap.getStateCode("XX", 99));
	}

	@Test public void codeStateUS() throws ConversionException {
		assertEquals("Code State: 8, US","CO",codeMap.getStateCode("US", 8));
	}
	
	
	
	
	
	
	
	@Test public void stateCodeCA() throws ConversionException {
		assertEquals("State Code: Manitoba, CA",3,codeMap.getStateCode("CA", 
				"Manitoba"));
	}
	
	@Test (expected = ConversionException.class)
	public void stateCodeNullCountry() throws ConversionException {
		codeMap.getStateCode(null, "CO");
	}
	
	@Test (expected = ConversionException.class)
	public void stateCodeNullState() throws ConversionException {
		codeMap.getStateCode("US", null);
	}
	
	@Test public void stateCodeMX() throws ConversionException {
		assertEquals("State Code: Sinaloa, MX",25,
				codeMap.getStateCode("MX", "Sinaloa"));
	}
	
	@Test public void stateCodePR() throws ConversionException {
		assertEquals("State Code: PR, XX",99,codeMap.getStateCode("PR", "XX"));
	}
	
	@Test (expected = ConversionException.class)
	public void stateCodeUnknownCountry() throws ConversionException {
		codeMap.getStateCode("NOTHING", "CO");
	}
	
	@Test (expected = ConversionException.class)
	public void stateCodeUnknownState() throws ConversionException {
		codeMap.getStateCode("US", "AA");
	}
	
	@Test public void stateCodeUnspecified() throws ConversionException {
		assertEquals("State Code: XX, XX",99,codeMap.getStateCode("XX", "XX"));
	}

	@Test public void stateCodeUS() throws ConversionException {
		assertEquals("State Code: CO, US",8,codeMap.getStateCode("US", "CO"));
	}
}
