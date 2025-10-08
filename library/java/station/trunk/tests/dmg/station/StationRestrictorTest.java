package dmg.station;

import static dmg.util.LengthUtils.*;
import static org.junit.Assert.*;
import dmg.util.*;

import org.junit.*;

/**
 * The StationRestrictorTest is a set of JUnit test cases for testing the restrictions
 * using a StationRestrictor.
 *
 *
 * @author Joel Clawson
 */
public class StationRestrictorTest implements StationRestrictor {

	private static final Double THRESHOLD = Math.pow(1, -10);
	
	private Station station;
	private Double oldDouble,newDouble;
	private String oldString,newString;
	private boolean elevationChanged,latitudeChanged,longitudeChanged,
						networkChanged,stationIdChanged;
	
	@Before public void setup() {
		station = new Station();
		// Make sure all of the state variables are reset
		elevationChanged = false;
		latitudeChanged = false;
		longitudeChanged = false;
		networkChanged = false;
		stationIdChanged = false;
		oldDouble = null;
		newDouble = null;
		oldString = null;
		newString = null;
	}
	
	@Test public void addRestrictor() throws InvalidValueException, 
	RestrictedOperationException {
		station.addStationRestrictor(this);
		
		// Need to change a value to make sure the add restrictor was accepted
		assertFalse("Add: default id",stationIdChanged);
		station.setStationId("NewStnID");
		assertTrue("Add: station id changed",stationIdChanged);
	}
	
	@Test public void elevationChange() throws ConversionException,
	InvalidValueException, RestrictedOperationException {
		// Make sure the test is listening for restrictions
		station.addStationRestrictor(this);

		// Set to the same value first (shouldn't notify observers)
		station.setElevation(null, METERS);
		assertFalse("Elevation: null",elevationChanged);
		
		// Set to a some value
		station.setElevation(100.0, METERS);
		assertTrue("Elevation: changed - 100",elevationChanged);
		assertNull("Elevation: old - 100",oldDouble);
		assertEquals("Elevation: new - 100",100.0,newDouble,THRESHOLD);
		elevationChanged = false;
		
		// Set to the same value
		station.setElevation(100.0, METERS);
		assertFalse("Elevation: same",elevationChanged);
		
		// Set back to null
		station.setElevation(null, METERS);
		assertTrue("Elevation: changed - null",elevationChanged);
		assertEquals("Elevation: old - null",100.0,oldDouble,THRESHOLD);
		assertNull("Elevation: new - null",newDouble);
	}

	public void elevationChangeInProgress(Station station, Double original, 
			Double changed) throws RestrictedOperationException {
		if (changed != null && changed == -11.11) {
			throw new RestrictedOperationException("Bad elevation value");
		}
				
		elevationChanged = true;
		oldDouble = original;
		newDouble = changed;
	}
	
	@Test (expected = RestrictedOperationException.class)
	public void elevationChangeRestricted() throws ConversionException, 
	InvalidValueException, RestrictedOperationException {
		// Make sure the test is listening for restrictions
		station.addStationRestrictor(this);
		
		// Set to a value that will throw the exception
		station.setElevation(-11.11, METERS);
	}
	
	@Test public void latitudeChange() throws InvalidValueException, 
	InvalidValueWarning, RestrictedOperationException {
		// Make sure the test is listening for restrictions
		station.addStationRestrictor(this);

		// Set to the same value first (shouldn't notify observers)
		station.setLatitude(null);
		assertFalse("Latitude: null",latitudeChanged);
		
		// Set to a some value
		station.setLatitude(10.0);
		assertTrue("Latitude: changed - 10",latitudeChanged);
		assertNull("Latitude: old - 10",oldDouble);
		assertEquals("Latitude: new - 10",10.0,newDouble,THRESHOLD);
		latitudeChanged = false;
		
		// Set to the same value
		station.setLatitude(10.0);
		assertFalse("Latitude: same",latitudeChanged);
		
		// Set back to null
		station.setLatitude(null);
		assertTrue("Latitude: changed - null",latitudeChanged);
		assertEquals("Latitude: old - null",10.0,oldDouble,THRESHOLD);
		assertNull("Latitude: new - null",newDouble);
	}

	public void latitudeChangeInProgress(Station station, Double original, 
			Double changed) throws RestrictedOperationException {
		if (changed != null && changed == -11.11) {
			throw new RestrictedOperationException("Bad latitude value");
		}
		latitudeChanged = true;
		oldDouble = original;
		newDouble = changed;
	}

	@Test (expected = RestrictedOperationException.class)
	public void latitudeChangeRestricted() throws InvalidValueException, 
	InvalidValueWarning, RestrictedOperationException {
		// Make sure the test is listening for restrictions
		station.addStationRestrictor(this);
		
		// Set to a value that will throw the exception
		station.setLatitude(-11.11);
	}
	
	@Test public void longitudeChange() throws InvalidValueException, 
	InvalidValueWarning, RestrictedOperationException {
		// Make sure the test is listening for restrictions
		station.addStationRestrictor(this);

		// Set to the same value first (shouldn't notify observers)
		station.setLongitude(null);
		assertFalse("Longitude: null",longitudeChanged);
		
		// Set to a some value
		station.setLongitude(10.0);
		assertTrue("Longitude: changed - 10",longitudeChanged);
		assertNull("Longitude: old - 10",oldDouble);
		assertEquals("Longitude: new - 10",10.0,newDouble,THRESHOLD);
		longitudeChanged = false;
		
		// Set to the same value
		station.setLongitude(10.0);
		assertFalse("Longitude: same",longitudeChanged);
		
		// Set back to null
		station.setLongitude(null);
		assertTrue("Longitude: changed - null",longitudeChanged);
		assertEquals("Longitude: old - null",10.0,oldDouble,THRESHOLD);
		assertNull("Longitude: new - null",newDouble);
	}
	
	public void longitudeChangeInProgress(Station station, Double original, 
			Double changed) throws RestrictedOperationException {
		if (changed != null && changed == -11.11) {
			throw new RestrictedOperationException("Bad longitude value");
		}
		longitudeChanged = true;
		oldDouble = original;
		newDouble = changed;
	}

	@Test (expected = RestrictedOperationException.class)
	public void longitudeChangeRestricted() throws InvalidValueException, 
	InvalidValueWarning, RestrictedOperationException {
		// Make sure the test is listening for restrictions
		station.addStationRestrictor(this);
		
		// Set to a value that will throw the exception
		station.setLongitude(-11.11);
	}
	
	@Test public void networkNameChange() throws InvalidValueException, 
	RestrictedOperationException {
		// Make sure the test is listening for restrictions
		station.addStationRestrictor(this);

		// Set to the same value first (shouldn't notify observers)
		station.setNetworkName("Network");
		assertFalse("Network Name: Network",networkChanged);
		
		// Set to a some value
		station.setNetworkName("NewNet");
		assertTrue("Network Name: changed",networkChanged);
		assertEquals("Network Name: old","Network",oldString);
		assertEquals("Network Name: new","NewNet",newString);
	}
	
	public void networkNameChangeInProgress(Station station, String original, 
			String changed) throws RestrictedOperationException {
		if (changed != null && changed.equals("OPPS")) {
			throw new RestrictedOperationException("Bad network value");
		}
		
		networkChanged = true;
		oldString = original;
		newString = changed;
	}
	
	@Test (expected = RestrictedOperationException.class)
	public void networkNameChangeRestricted() throws InvalidValueException,
	RestrictedOperationException {
		// Make sure the test is listening for restrictions
		station.addStationRestrictor(this);
		
		// Set to a value that will throw the exception
		station.setNetworkName("OPPS");
	}
	
	@Test public void removeRestrictor() throws InvalidValueException, 
	RestrictedOperationException {
		// Make sure the test is listening for restrictions
		station.addStationRestrictor(this);
		
		// Make sure that this class is receiving the notification
		assertFalse("Remove (Add): default id",stationIdChanged);
		station.setStationId("NewStnID");
		assertTrue("Remove (Add): station id changed",stationIdChanged);
		// Reset the state variable
		stationIdChanged = false;
		// Now remove the listener
		station.removeStationRestrictor(this);
		// Make a change to the station and make sure that the state didn't 
		// change.
		station.setStationId("NewStnID");
		assertFalse("Remove: station id",stationIdChanged);
	}

	@Test public void stationIdChange() throws InvalidValueException, 
	RestrictedOperationException {
		// Make sure the test is listening for restrictions
		station.addStationRestrictor(this);

		station.setStationId("Station");
		assertFalse("Station Id: Station",stationIdChanged);
		
		// Set to a some value
		station.setStationId("NewStnId");
		assertTrue("Station Id: changed",stationIdChanged);
		assertEquals("Station Id: old","Station",oldString);
		assertEquals("Station Id: new","NewStnId",newString);
	}
	
	public void stationIdChangeInProgress(Station station, String original, 
			String changed) throws RestrictedOperationException {
		if (changed != null && changed.equals("OPPS")) {
			throw new RestrictedOperationException("Bad network value");
		}
		
		stationIdChanged = true;
		oldString = original;
		newString = changed;
	}

	@Test (expected = RestrictedOperationException.class)
	public void stationIdChangeRestricted() throws InvalidValueException,
	RestrictedOperationException {
		// Make sure the test is listening for restrictions
		station.addStationRestrictor(this);
		
		// Set to a value that will throw the exception
		station.setStationId("OPPS");
	}
	
}
