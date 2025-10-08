package dmg.station;

import static org.junit.Assert.*;
import dmg.util.*;

import org.junit.*;

public class StationPositionComparatorTest {

	private static StationPositionComparator comparator;
	
	@BeforeClass public static void setup() { 
		comparator = new StationPositionComparator();
	}
	
	@Test public void latitudeComparisons() throws InvalidValueException, 
	InvalidValueWarning, RestrictedOperationException {
		Station first = new Station();
		Station second = new Station();
		
		// Test when both latitudes are null.
		assertTrue("Latitude: null, null",
				comparator.compare(first, second) == 0);
		assertTrue("Latitude: null, null",
				comparator.compare(second, first) == 0);
		
		// Test when one latitude is null
		first.setLatitude(10.0);
		assertTrue("Latitude: value, null",
				comparator.compare(first, second) > 0);
		assertTrue("Latitude: null, value",
				comparator.compare(second, first) < 0);
		
		// Test when the latitudes have different values.
		second.setLatitude(-10.0);
		assertTrue("Latitude: big, little",
				comparator.compare(first, second) > 0);
		assertTrue("Latitude: little, big",
				comparator.compare(second, first) < 0);
		
		// Test when the latitudes have the same value.
		second.setLatitude(10.0);
		assertTrue("Latitude: value, value",
				comparator.compare(first, second) == 0);
		assertTrue("Latitude: value, value",
				comparator.compare(second, first) == 0);
	}
	
	@Test public void longitudeComparisons() throws InvalidValueException, 
	InvalidValueWarning, RestrictedOperationException {
		Station first = new Station();
		Station second = new Station();
		
		// Test when both longitudes are null.
		assertTrue("Longitude: null, null",
				comparator.compare(first, second) == 0);
		assertTrue("Longitude: null, null",
				comparator.compare(second, first) == 0);
		
		// Test when one longitude is null
		first.setLongitude(10.0);
		assertTrue("Longitude: value, null",
				comparator.compare(first, second) > 0);
		assertTrue("Longitude: null, value",
				comparator.compare(second, first) < 0);
		
		// Test when the longitudes have different values.
		second.setLongitude(-10.0);
		assertTrue("Longitude: big, little",
				comparator.compare(first, second) > 0);
		assertTrue("Longitude: little, big",
				comparator.compare(second, first) < 0);
		
		// Test when the longitudes have the same value.
		second.setLongitude(10.0);
		assertTrue("Longitude: value, value",
				comparator.compare(first, second) == 0);
		assertTrue("Longitude: value, value",
				comparator.compare(second, first) == 0);

	}
	
	@Test public void networkNameComparisons() throws InvalidValueException,
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
	
	@Test public void stationIdComparisons() throws InvalidValueException, 
	RestrictedOperationException {
		Station first = new Station();
		Station second = new Station();
		
		// Test when both id are the same
		assertTrue("ID: value, value",comparator.compare(first, second) == 0);
		assertTrue("ID: value, value",comparator.compare(second, first) == 0);
		
		// Test when the ids are different.
		second.setStationId("A Test");
		assertTrue("ID: big, little",comparator.compare(first, second) > 0);
		assertTrue("ID: little, big",comparator.compare(second, first) < 0);
	}
}
