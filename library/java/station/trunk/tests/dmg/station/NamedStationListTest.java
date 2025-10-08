package dmg.station;

import static org.junit.Assert.*;

import java.util.*;

import dmg.util.*;

import org.junit.*;

public class NamedStationListTest extends StationListTest<NamedStationList> {

	@Override
	public void add() throws InvalidValueException {
		Station mobileA = new Station("MobileA","MOBILE");
		Station mobileB = new Station("Mobile1","NETWORK");
		
		stationList.add(mobileA);
		stationList.add(mobileB);
		
		assertTrue("Add MobileA", stationList.contains(mobileA));
		assertTrue("Add MobileB", stationList.contains(mobileB));
	}
	
	@Test (expected = InvalidValueException.class)
	public void addEquivalentStation() throws InvalidValueException {
		Station mobileC = new Station("Mobile1","MOBILE");
		stationList.add(mobileC);
	}
	
	@Test (expected = InvalidValueException.class)
	public void addExactStation() throws InvalidValueException {
		stationList.add(mobile1);
	}

	@Override
	public void contains() throws InvalidValueException {
		Station mobileC = new Station("Mobile1","MOBILE");
		assertTrue("Contains: Mobile1/MOBILE memory equal",
				stationList.contains(mobile1));
		assertTrue("Contains: Mobile1/MOBILE diff stns",
				stationList.contains(mobileC));
	}
	
	@Override public void containsNonExistant() throws InvalidValueException {
		Station mobileA = new Station("MobileA","MOBILE");
		Station mobileB = new Station("Mobile1","NETWORK");
		assertFalse("Contains: MobileA/MOBILE",stationList.contains(mobileA));
		assertFalse("Contains: Mobile1/NETWORK",stationList.contains(mobileB));
		assertFalse("Contains: null",stationList.contains(null));		
	}
	
	@Test public void containsStrings() {
		assertTrue("Contains: Mobile1/MOBILE Strings",
				stationList.contains("Mobile1","MOBILE"));
		assertFalse("Contains: MobileA/MOBILE Strings",
				stationList.contains("MoblieA","MOBILE"));
		assertFalse("Contains: Mobile1/NETWORK Strings",
				stationList.contains("Moblie1","NETWORK"));
	}

	@Override
	public void get() throws InvalidValueException {
		Station mobileC = new Station("Mobile1","MOBILE");
		assertEquals("Get: mobile1 memory equal",mobile1,
				stationList.get(mobile1));
		assertEquals("Get: Mobile1/MOBILE",mobile1,stationList.get(mobileC));
	}
	
	@Override public void getNonExistant() throws InvalidValueException {
		Station mobileA = new Station("MobileA","MOBILE");
		Station mobileB = new Station("Mobile1","NETWORK");
		assertNull("Get: MobileA/MOBILE",stationList.get(mobileA));
		assertNull("Get: Mobile1/NETWORK",stationList.get(mobileB));
		assertNull("Get: null",stationList.get(null));		
	}
	
	@Test public void getStrings() {
		assertEquals("Get: Mobile1/MOBILE Strings",mobile1,
				stationList.get("Mobile1","MOBILE"));
		assertNull("Get: MobileA/MOBILE Strings",
				stationList.get("MobileA","MOBILE"));
		assertNull("Get: Mobile1/NETWORK Strings",
				stationList.get("Mobile1","NETWORK"));
	}

	@Override
	public void iterator() {
		// Define an array with the Station in the expected order.
		ArrayList<Station> expected = new ArrayList<Station>();
		expected.add(mobile1);
		expected.add(mobile2);
		expected.add(ne1);
		expected.add(ne2);
		expected.add(nw1);
		expected.add(nw2);
		expected.add(nw3);
		expected.add(se1);
		expected.add(se2);
		expected.add(sw1);
		expected.add(sw2);
		expected.add(un1);
		expected.add(un2);
	
		// Loop through the array and pull of the next Station in the Iterator 
		// and make sure they are the same.
		Iterator<Station> itr = stationList.iterator();		
		for (int i = 0; i < expected.size(); i++) {
			assertEquals(String.format("Iterator (%d)",i + 1),
					expected.get(i),itr.next());
		}
	}

	@Override
	public NamedStationList newInstance() { return new NamedStationList(); }

	@Override
	public void remove() {
		assertTrue("Remove: contains = true",stationList.contains(mobile1));
		assertEquals("Remove: station equal",mobile1,
				stationList.remove(mobile1));
		assertFalse("Remove: contains = false",stationList.contains(mobile1));
		
		assertTrue("Remove: contains = true (Strings)",
				stationList.contains("NW1","NW"));
		assertEquals("Remove: station equal (Strings)",nw1,
				stationList.remove("NW1","NW"));
		assertFalse("Remove: contains = false (Strings)",
				stationList.contains(nw1));
	}
	
	@Test public void removeEquivalent() throws InvalidValueException {
		Station station = new Station("Mobile2","MOBILE");
		assertTrue("Remove: contains = true (diff memory)",
				stationList.contains(station));
		assertEquals("Remove: station equal (diff memory)",mobile2,
				stationList.remove(station));
		assertFalse("Remove: contains = false (diff memory)",
				stationList.contains(mobile2));
	}
	
	@Override public void removeNonExistant() throws InvalidValueException {
		Station station = new Station("Here","AndNow");
		assertNull("Remove: null station",stationList.remove(station));
		
		assertNull("Remove: diff id (Strings)",
				stationList.remove("NW0","NW"));
		assertNull("Remove: diff network (Strings)",
				stationList.remove("NW2","SE"));
	}

	@Test (expected = RestrictedOperationException.class)
	public void stationRestrictionsNetworkFail() throws InvalidValueException, 
	RestrictedOperationException {
		assertTrue("Network Update Fail init", stationList.contains(mobile2));
		mobile2.setStationId("NW1");
		assertTrue("Network Update Fail changed ID", 
				stationList.contains(mobile2));
		mobile2.setNetworkName("NW");
	}

	@Test public void stationRestrictionsNetworkPass() throws 
	InvalidValueException, RestrictedOperationException {
		assertTrue("Network Update (init)", stationList.contains(mobile2));
		mobile2.setNetworkName("Network");
		assertTrue("Network Update changed", stationList.contains("Mobile2", 
				"Network"));
	}

	@Test (expected = RestrictedOperationException.class)
	public void stationRestrictionsStationFail() throws InvalidValueException,
	RestrictedOperationException {
		assertTrue("Station Id Update (init)",stationList.contains(mobile2));
		mobile2.setStationId("Mobile1");
	}

	@Test public void stationRestrictionsStationPass() throws 
	InvalidValueException, RestrictedOperationException {
		assertTrue("Station Id Update (init)",stationList.contains(mobile2));
		mobile2.setStationId("Mobile0");
		assertTrue("Station Id Update changed", stationList.contains("Mobile0", mobile2.getNetworkName()));
	}
}
