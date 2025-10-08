package dmg.station;

import static dmg.util.LengthUtils.*;
import static org.junit.Assert.*;

import java.util.*;

import org.junit.*;

import dmg.util.*;


public class ElevatedStationListTest extends 
StationListTest<ElevatedStationList> {

	@Override
	public void add() throws ConversionException, InvalidValueException, 
	InvalidValueWarning, RestrictedOperationException {
		// Different Station ID
		Station stationA = new Station("NW0","NW");
		stationA.setLatitude(10.0);
		stationA.setLongitude(-10.0);
		stationA.setElevation(10.0, METERS);
		stationList.add(stationA);
		
		// Different network
		Station stationB = new Station("NW1","NETWORK");
		stationB.setLatitude(10.0);
		stationB.setLongitude(-10.0);
		stationB.setElevation(10.0, METERS);
		stationList.add(stationB);
		
		// Different latitude
		Station stationC = new Station("NW1","NW");
		stationC.setLatitude(25.0);
		stationC.setLongitude(-10.0);
		stationC.setElevation(10.0, METERS);
		stationList.add(stationC);
		
		// Different longitude
		Station stationD = new Station("NW1","NW");
		stationD.setLatitude(10.0);
		stationD.setLongitude(-25.0);
		stationD.setElevation(10.0, METERS);
		stationList.add(stationD);
		
		// Different elevation
		Station stationE = new Station("NW1","NW");
		stationE.setLatitude(10.0);
		stationE.setLongitude(-10.0);
		stationE.setElevation(10.0, FEET);
		stationList.add(stationE);
	}

	@Test (expected = InvalidValueException.class)
	public void addEquivalentStation() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		Station station = new Station("NW1","NW");
		station.setLatitude(10.0);
		station.setLongitude(-10.0);
		station.setElevation(10.0, METERS);
		stationList.add(station);
	}
	
	@Test (expected = InvalidValueException.class)
	public void addExactStation() throws InvalidValueException {
		stationList.add(mobile1);
	}
	
	@Override
	public void contains() throws ConversionException, InvalidValueException, 
	InvalidValueWarning, RestrictedOperationException {
  		assertTrue("Contains: NW1/NW/10/-10/10 memory equal",
  				stationList.contains(nw1));

		Station allSame = new Station("NW1","NW");
		allSame.setLatitude(10.0);
		allSame.setLongitude(-10.0);
		allSame.setElevation(10.0, METERS);
		assertTrue("Contains: NW1/NW/10/10/10 diff stns",
				stationList.contains(allSame));
	}

	@Override
	public void containsNonExistant() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		Station diffId = new Station("NW0","NW");
		diffId.setLatitude(10.0);
		diffId.setLongitude(-10.0);
		diffId.setElevation(10.0, METERS);
		Station diffNet = new Station("NW1","NewNet");
		diffNet.setLatitude(10.0);
		diffNet.setLongitude(-10.0);
		diffNet.setElevation(10.0, METERS);
		Station diffLat = new Station("NW1","NW");
		diffLat.setLatitude(15.0);
		diffLat.setLongitude(-10.0);
		diffLat.setElevation(10.0, METERS);
		Station diffLon = new Station("NW1","NW");
		diffLon.setLatitude(10.0);
		diffLon.setLongitude(-15.0);
		diffLon.setElevation(10.0, METERS);
		Station diffElev = new Station("NW1","NW");
		diffElev.setLatitude(10.0);
		diffElev.setLongitude(-10.0);
		diffElev.setElevation(15.0, METERS);
		assertFalse("Contains: NW0/NW/10/-10/10",stationList.contains(diffId));
		assertFalse("Contains: NW1/NewNet/10/-10/10",
				stationList.contains(diffNet));
		assertFalse("Contains: NW1/NW/15/-10/10",stationList.contains(diffLat));
		assertFalse("Contains: NW1/NW/10/-15/10",stationList.contains(diffLon));
		assertFalse("Contains: NW1/NW/10/-10/10",
				stationList.contains(diffElev));
		assertFalse("Contains: null",stationList.contains(null));
	}

	@Test public void containsStringsDoubles() {
		assertTrue("Contains: NW1/NW/10/-10/10 Strings",
				stationList.contains("NW1","NW",10.0,-10.0,10.0));
		assertFalse("Contains: NW0/NW/10/-10/10 Strings",
				stationList.contains("NW0","NW",10.0,-10.0,10.0));
		assertFalse("Contains: NW1/NETWORK/10/-10/10 Strings",
				stationList.contains("NW1","NETWORK",10.0,-10.0,10.0));
		assertFalse("Contains: NW1/NW/-10/-10/10 Strings",
				stationList.contains("NW1","NW",-10.0,-10.0,10.0));
		assertFalse("Contains: NW1/NW/10/10/10 Strings",
				stationList.contains("NW1","NW",10.0,10.0,10.0));
		assertFalse("Contains: NW1/NW/10/-10/15 Strings",
				stationList.contains("NW1","NW",10.0,-10.0,15.0));
	}
	
	@Override
	public void get() throws ConversionException, InvalidValueException, 
	InvalidValueWarning, RestrictedOperationException {
		assertEquals("Get: NW1/NW/10/-10/10 memory equal",nw1,
				stationList.get(nw1));
		
		Station allSame = new Station("NW1","NW");
		allSame.setLatitude(10.0);
		allSame.setLongitude(-10.0);
		allSame.setElevation(10.0, METERS);
		assertEquals("Get: NW1/NW/10/10/10 diff stns",nw1,
				stationList.get(allSame));		
	}

	@Override
	public void getNonExistant() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		Station diffId = new Station("NW0","NW");
		diffId.setLatitude(10.0);
		diffId.setLongitude(-10.0);
		diffId.setElevation(10.0, METERS);
		Station diffNet = new Station("NW1","NewNet");
		diffNet.setLatitude(10.0);
		diffNet.setLongitude(-10.0);
		diffNet.setElevation(10.0, METERS);
		Station diffLat = new Station("NW1","NW");
		diffLat.setLatitude(15.0);
		diffLat.setLongitude(-10.0);
		diffLat.setElevation(10.0, METERS);
		Station diffLon = new Station("NW1","NW");
		diffLon.setLatitude(10.0);
		diffLon.setLongitude(-15.0);
		diffLon.setElevation(10.0, METERS);
		Station diffElev = new Station("NW1","NW");
		diffElev.setLatitude(10.0);
		diffElev.setLongitude(-10.0);
		diffElev.setElevation(15.0, METERS);
		assertNull("Get: NW0/NW/10/-10/10",stationList.get(diffId));
		assertNull("Get: NW1/NewNet/10/-10/10",stationList.get(diffNet));
		assertNull("Get: NW1/NW/15/-10/10",stationList.get(diffLat));
		assertNull("Get: NW1/NW/10/-15/10",stationList.get(diffLon));
		assertNull("Get: NW1/NW/10/-10/10",stationList.get(diffElev));
	}
	
	@Test public void getStringsDoubles() {
		assertEquals("Get: NW1/NW/10/-10/10 Strings",nw1,
				stationList.get("NW1","NW",10.0,-10.0,10.0));
		assertNull("Get: NW0/NW/10/-10/10 Strings",
				stationList.get("NW0","NW",10.0,-10.0,10.0));
		assertNull("Get: NW1/NETWORK/10/-10/10 Strings",
				stationList.get("NW1","NETWORK",10.0,-10.0,10.0));
		assertNull("Get: NW1/NW/-10/-10/10 Strings",
				stationList.get("NW1","NW",-10.0,-10.0,10.0));
		assertNull("Get: NW1/NW/10/10/10 Strings",
				stationList.get("NW1","NW",10.0,10.0,10.0));
		assertNull("Get: NW1/NW/10/-10/15 Strings",
				stationList.get("NW1","NW",10.0,-10.0,15.0));
	}

	@Override
	public void iterator() {
		ArrayList<Station> expected = new ArrayList<Station>();
		expected.add(mobile1);
		expected.add(mobile2);
		expected.add(un1);
		expected.add(un2);
		expected.add(sw2);
		expected.add(sw1);
		expected.add(se1);
		expected.add(se2);
		expected.add(nw3);
		expected.add(nw2);
		expected.add(nw1);
		expected.add(ne1);
		expected.add(ne2);
	
		// Loop through the array and pull of the next Station in the Iterator 
		// and make sure they are the same.
		Iterator<Station> itr = stationList.iterator();		
		for (int i = 0; i < expected.size(); i++) {
			assertEquals(String.format("Iterator (%d)",i + 1),expected.get(i),
					itr.next());
		}
	}

	@Override
	public ElevatedStationList newInstance() { 
		return new ElevatedStationList();
	}

	@Override
	public void remove() {
		// Remove an exact station from the station list
		assertTrue("Remove: contains = true",stationList.contains(mobile1));
		assertEquals("Remove: station equal",mobile1,
				stationList.remove(mobile1));
		assertFalse("Remove: contains = false",stationList.contains(mobile1));
		
		assertTrue("Remove: contains = true (Strings)",
				stationList.contains("SE1","SE",-10.0,10.0,10.0));
		assertEquals("Remove: station equal (Strings)",se1,
				stationList.remove("SE1","SE",-10.0,10.0,10.0));
		assertFalse("Remove: contains = false (Strings)",
				stationList.contains(se1));
	}
		
	@Test public void removeEquivalent() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		Station station = new Station("NW1","NW");
		station.setLatitude(10.0);
		station.setLongitude(-10.0);
		station.setElevation(10.0, METERS);
		assertTrue("Remove: contains = true (diff memory)",
				stationList.contains(station));
		assertEquals("Remove: station equal (diff memory)",nw1,
				stationList.remove(station));
		assertFalse("Remove: contains = false (diff memory)",
				stationList.contains(nw1));
	}

	@Override
	public void removeNonExistant() throws InvalidValueException {
		Station station = new Station("Here","AndNow");
		assertNull("Remove: null station",stationList.remove(station));
		
		assertNull("Remove: diff id (Strings)",
				stationList.remove("NW0","NW",10.0,-100.0,10.0));
		assertNull("Remove: diff network (Strings)",
				stationList.remove("NW2","SE",10.0,-100.0,10.0));
		assertNull("Remove: diff latitude (Strings)",
				stationList.remove("NW2","NW",11.0,-100.0,10.0));
		assertNull("Remove: diff longitude (Strings)",
				stationList.remove("NW2","NW",10.0,-110.0,10.0));
		assertNull("Remove: diff elevation (Strings)",
				stationList.remove("NW2","NW",10.0,-100.0,15.0));
	}

	@Test (expected = RestrictedOperationException.class)
	public void stationRestrictionsElevationFail() throws ConversionException,
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		assertTrue("Elevation Update Fail init", stationList.contains(mobile2));
		mobile2.setStationId("NW1");
		mobile2.setNetworkName("NW");
		mobile2.setLatitude(10.0);
		mobile2.setLongitude(-10.0);
		assertTrue("Elevation Update Fail changed values", 
				stationList.contains(mobile2));
		mobile2.setElevation(10.0, METERS);
	}
	
	@Test public void stationRestrictionsElevationPass() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		assertTrue("Elevation Update Pass init", stationList.contains(mobile2));
		mobile2.setStationId("NW1");
		mobile2.setNetworkName("NW");
		mobile2.setLatitude(10.0);
		mobile2.setLongitude(-10.0);
		assertTrue("Elevation Update Pass changed values", 
				stationList.contains(mobile2));
		mobile2.setElevation(100.0, METERS);
		assertTrue("Elevation Update Pass changed elevation", 
				stationList.contains("NW1", "NW", 10.0, -10.0, 100.0));
		assertTrue("Elevation Update Pass other station", 
				stationList.contains("NW1", "NW", 10.0, -10.0, 10.0));
	}

	@Test (expected = RestrictedOperationException.class)
	public void stationRestrictionsLatitudeFail() throws ConversionException,
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		assertTrue("Latitude Update Fail init", stationList.contains(mobile2));
		mobile2.setStationId("NW1");
		mobile2.setNetworkName("NW");
		mobile2.setLongitude(-10.0);
		mobile2.setElevation(10.0, METERS);
		assertTrue("Latitude Update Fail changed values", 
				stationList.contains(mobile2));
		mobile2.setLatitude(10.0);
	}
	
	@Test public void stationRestrictionsLatitudePass() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		assertTrue("Latitude Update Pass init", stationList.contains(mobile2));
		mobile2.setStationId("NW1");
		mobile2.setNetworkName("NW");
		mobile2.setLongitude(-10.0);
		mobile2.setElevation(10.0, METERS);
		assertTrue("Latitude Update Pass changed values", 
				stationList.contains(mobile2));
		mobile2.setLatitude(11.11);
		assertTrue("Latitude Update Pass changed latitude", 
				stationList.contains("NW1", "NW", 11.11, -10.0, 10.0));
		assertTrue("Latitude Update Pass other station", 
				stationList.contains("NW1", "NW", 10.0, -10.0, 10.0));
	}
	
	@Test (expected = RestrictedOperationException.class)
	public void stationRestrictionsLongitudeFail() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		assertTrue("Longitude Update Fail init", stationList.contains(mobile2));
		mobile2.setStationId("NW1");
		mobile2.setNetworkName("NW");
		mobile2.setLatitude(10.0);
		mobile2.setElevation(10.0, METERS);
		assertTrue("Longitude Update Fail changed values", 
				stationList.contains(mobile2));
		mobile2.setLongitude(-10.0);
	}
	
	@Test public void stationRestrictionsLongitudePass() throws 
	ConversionException, InvalidValueException, InvalidValueWarning,
	RestrictedOperationException {
		assertTrue("Longitude Update Pass init", stationList.contains(mobile2));
		mobile2.setStationId("NW1");
		mobile2.setNetworkName("NW");
		mobile2.setLatitude(10.0);
		mobile2.setElevation(10.0, METERS);
		assertTrue("Longitude Update Pass changed values", 
				stationList.contains(mobile2));
		mobile2.setLongitude(-11.11);
		assertTrue("Longitude Update Pass changed longitude", 
				stationList.contains("NW1", "NW", 10.0, -11.11, 10.0));
		assertTrue("Longitude Update Pass other station", 
				stationList.contains("NW1", "NW", 10.0, -10.0, 10.0));
	}
	
	@Test (expected = RestrictedOperationException.class)
	public void stationRestrictionsNetworkFail() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		assertTrue("Network Update Fail init", stationList.contains(mobile2));
		mobile2.setStationId("NW1");
		mobile2.setLatitude(10.0);
		mobile2.setLongitude(-10.0);
		mobile2.setElevation(10.0, METERS);
		assertTrue("Network Update Fail changed values", 
				stationList.contains(mobile2));
		mobile2.setNetworkName("NW");
	}

	@Test public void stationRestrictionsNetworkPass() throws 
	InvalidValueException, RestrictedOperationException {
		assertTrue("Network Update (init)", stationList.contains(mobile2));
		mobile2.setNetworkName("Network");
		assertTrue("Network Update changed", 
				stationList.contains("Mobile2", "Network", null, null, null));
	}

	@Test (expected = RestrictedOperationException.class)
	public void stationRestrictionsStationFail() throws 
	InvalidValueException, RestrictedOperationException {
		assertTrue("Station Id Update (init)",stationList.contains(mobile2));
		mobile2.setStationId("Mobile1");
	}

	@Test public void stationRestrictionsStationPass() throws 
	InvalidValueException, RestrictedOperationException {
		assertTrue("Station Id Update (init)",stationList.contains(mobile2));
		mobile2.setStationId("Mobile0");
		assertTrue("Station Id Update changed", 
				stationList.contains("Mobile0", mobile2.getNetworkName(), 
						null, null, null));
	}
}
