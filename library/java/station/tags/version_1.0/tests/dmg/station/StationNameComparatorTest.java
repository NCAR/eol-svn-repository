package dmg.station;

import static org.junit.Assert.*;
import dmg.util.*;
import org.junit.*;

public class StationNameComparatorTest {

	private static StationNameComparator comparator;
	
	@BeforeClass public static void initalize() { 
		comparator = new StationNameComparator();
	}
	
	@Test public void compareNetworks() throws InvalidValueException, 
	RestrictedOperationException {
		Station first = new Station();
		Station second = new Station();
		
		// Test when both networks are the same
		assertTrue("Network: value, value",
				comparator.compare(first, second) == 0);
		assertTrue("Network: value, value",
				comparator.compare(second, first) == 0);
		
		// Test when the networks are different.
		second.setNetworkName("A Test");
		assertTrue("Network: big, little",
				comparator.compare(first, second) > 0);
		assertTrue("Network: little, big",
				comparator.compare(second, first) < 0);
	}
}
