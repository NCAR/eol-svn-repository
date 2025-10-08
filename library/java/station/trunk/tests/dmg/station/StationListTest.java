package dmg.station;

import static dmg.util.LengthUtils.*;
import static dmg.util.TimeUtils.*;
import static org.junit.Assert.*;

import dmg.util.*;

import java.io.*;
import java.util.*;
import org.junit.*;

public abstract class StationListTest<T extends StationList> {

	protected T emptyList,stationList;
	protected Station mobile1,mobile2,ne1,ne2,nw1,nw2,nw3,se1,se2,
						sw1,sw2,un1,un2;
	
	/**
	 * Create a new instance of the StationList being tested.
	 * @return A new instance of the StationList being tested.
	 */
	public abstract T newInstance();
	
	@Before public void setUp() throws Exception {
		emptyList = newInstance();
		stationList = newInstance();
		Calendar date = buildDate(2007, 3, 12, UTC);
		
		mobile1 = new Station("Mobile1","MOBILE");
		mobile1.insertDate(date);
		mobile1.setMobile(true);
		stationList.add(mobile1);
		
		mobile2 = new Station("Mobile2","MOBILE");
		mobile2.setMobile(true);
		stationList.add(mobile2);
		
		un1 = new Station("Unknown1","UNKNOWN");
		un1.insertDate(date);
		stationList.add(un1);
		
		un2 = new Station("Unknown2","UNKNOWN");
		stationList.add(un2);
		
		ne1 = new Station("NE1","NE");
		ne1.insertDate(date);
		ne1.setLatitude(10.0);
		ne1.setLongitude(10.0);
		ne1.setElevation(10.0, METERS);
		stationList.add(ne1);
		
		ne2 = new Station("NE2","NE");
		ne2.setLatitude(10.0);
		ne2.setLongitude(100.0);
		ne2.setElevation(10.0, METERS);
		stationList.add(ne2);
		
		nw1 = new Station("NW1","NW");
		nw1.insertDate(date);
		nw1.setLatitude(10.0);
		nw1.setLongitude(-10.0);
		nw1.setElevation(10.0, METERS);
		stationList.add(nw1);
		
		nw2 = new Station("NW2","NW");
		nw2.setLatitude(10.0);
		nw2.setLongitude(-100.0);
		nw2.setElevation(10.0, METERS);
		stationList.add(nw2);
		
		nw3 = new Station("NW3","NW");
		nw3.setLatitude(10.0);
		nw3.setLongitude(-100.0);
		nw3.setElevation(-100.0, METERS);
		stationList.add(nw3);
		
		se1 = new Station("SE1","SE");
		se1.insertDate(date);
		se1.setLatitude(-10.0);
		se1.setLongitude(10.0);
		se1.setElevation(10.0, METERS);
		stationList.add(se1);
		
		se2 = new Station("SE2","SE");
		se2.setLatitude(-10.0);
		se2.setLongitude(100.0);
		se2.setElevation(10.0, METERS);
		stationList.add(se2);
		
		sw1 = new Station("SW1","SW");
		sw1.insertDate(date);
		sw1.setLatitude(-10.0);
		sw1.setLongitude(-10.0);
		sw1.setElevation(10.0, METERS);
		stationList.add(sw1);
		
		sw2 = new Station("SW2","SW");
		sw2.setLatitude(-10.0);
		sw2.setLongitude(-100.0);
		sw2.setElevation(10.0, METERS);
		stationList.add(sw2);
	}

	@Test public abstract void add() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException;
	
	@Test public void cleanForFilename() {
		assertEquals("Clean Filename: T-REX","TREX",
				stationList.cleanForFilename("T-REX"));
		assertEquals("Clean Filename: Space s","Space_s",
				stationList.cleanForFilename("Space_s"));
		assertEquals("Clean Filename: Nothing","Nothing",
				stationList.cleanForFilename("Nothing"));
		assertEquals("Clean Filename: B O-T-H   Multiple_Type",
				"B_OTH_Multiple_Type",
				stationList.cleanForFilename("B O-T-H Multiple_Type"));
	}
	
	@Test public void clear() {
		assertFalse("clear: initial",stationList.isEmpty());
		stationList.clear();
		assertTrue("clear: cleared",stationList.isEmpty());
	}
	
	@Test public abstract void contains() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException;
	
	@Test public abstract void containsNonExistant() 
	throws ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException;
	
	@Test public abstract void get() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException;
	
	@Test public abstract void getNonExistant() throws ConversionException,
	InvalidValueException, InvalidValueWarning, RestrictedOperationException;
	
	@Test public void getFilename() {
		assertEquals("Filename: sounding","NOAA_P3_TREX_sounding_stationCD.out",
				stationList.getFilename("NOAA P3", "T-REX", "sounding"));
		assertEquals("Filename: surface","COAG60_NAME_surface_stationCD.out",
				stationList.getFilename("COAG60", "NAME", "surface"));
	}
	
	@Test public void isEmpty() {
		assertTrue("isEmpty: emptyList",emptyList.isEmpty());
		assertFalse("isEmpty: stationList",stationList.isEmpty());
	}
	
	@Test public abstract void remove() throws InvalidValueException, 
	InvalidValueWarning, RestrictedOperationException;
	
	@Test public abstract void removeNonExistant() throws InvalidValueException;
	
	@Test public void stationSummary() {
		assertEquals("Summary: empty",
				"There were 0 stations found for 0 networks.",
				emptyList.getStationSummary());	
		
		StringBuffer sb = new StringBuffer();
		sb.append("There were 13 stations found for 6 networks.\n\n");
		sb.append("-----------------------------------------------------------------------------------------------------------------------\n");
		sb.append("|     NETWORK     | TOTAL  (MISS) |  NORTHWEST  |  NORTHEAST  |  SOUTHEAST  |  SOUTHWEST  |   UNKNOWN   |    MOBILE   |\n");
		sb.append("-----------------------------------------------------------------------------------------------------------------------\n");
		sb.append("| MOBILE          |     2 (    1) |    0 (   0) |    0 (   0) |    0 (   0) |    0 (   0) |    0 (   0) |    1 (   1) |\n");
		sb.append("-----------------------------------------------------------------------------------------------------------------------\n");
		sb.append("| NE              |     2 (    1) |    0 (   0) |    1 (   1) |    0 (   0) |    0 (   0) |    0 (   0) |    0 (   0) |\n");
		sb.append("-----------------------------------------------------------------------------------------------------------------------\n");
		sb.append("| NW              |     3 (    2) |    1 (   2) |    0 (   0) |    0 (   0) |    0 (   0) |    0 (   0) |    0 (   0) |\n");
		sb.append("-----------------------------------------------------------------------------------------------------------------------\n");
		sb.append("| SE              |     2 (    1) |    0 (   0) |    0 (   0) |    1 (   1) |    0 (   0) |    0 (   0) |    0 (   0) |\n");
		sb.append("-----------------------------------------------------------------------------------------------------------------------\n");
		sb.append("| SW              |     2 (    1) |    0 (   0) |    0 (   0) |    0 (   0) |    1 (   1) |    0 (   0) |    0 (   0) |\n");
		sb.append("-----------------------------------------------------------------------------------------------------------------------\n");
		sb.append("| UNKNOWN         |     2 (    1) |    0 (   0) |    0 (   0) |    0 (   0) |    0 (   0) |    1 (   1) |    0 (   0) |\n");
		sb.append("-----------------------------------------------------------------------------------------------------------------------\n");
		sb.append("|     SUMMARY     |    13 (    7) |    1 (   2) |    1 (   1) |    1 (   1) |    1 (   1) |    1 (   1) |    1 (   1) |\n");
		sb.append("-----------------------------------------------------------------------------------------------------------------------");
		assertEquals("Summary: stationList",sb.toString(),stationList.getStationSummary());
	}

	@Test public abstract void iterator();
	
	@Test public void size() {
		assertEquals("Size: empty",0,emptyList.size());
		assertEquals("Size: stations",13,stationList.size());
	}

	@Test public void tostring() {
		assertEquals("toString empty","",emptyList.toString());
		StringBuffer output = new StringBuffer();
		output.append(mobile1.toString()).append("\n");
		output.append(ne1.toString()).append("\n");
		output.append(nw1.toString()).append("\n");
		output.append(se1.toString()).append("\n");
		output.append(sw1.toString()).append("\n");
		output.append(un1.toString());
		assertEquals("toString stationList",output.toString(),
				stationList.toString());
	}

	@Test public void writeStationCDoutFile() throws IOException {
		// Generate the stationCD file.
		stationList.writeStationCDout(new File("."), "ALL", "TEST", "surface");
			
		// Define the expected file and make sure that it was created.
		File outFile = new File(".","ALL_TEST_surface_stationCD.out");
		assertTrue("stationCD: exists",outFile.exists());
			
		// Open the file for reading and compare it line by line to the 
		// expected output
		BufferedReader reader = new BufferedReader(new FileReader(outFile));
		assertEquals("stationCD: line 1",mobile1.toString(),reader.readLine());
		assertEquals("stationCD: line 2",ne1.toString(),reader.readLine());
		assertEquals("stationCD: line 3",nw1.toString(),reader.readLine());
		assertEquals("stationCD: line 4",se1.toString(),reader.readLine());
		assertEquals("stationCD: line 5",sw1.toString(),reader.readLine());
		assertEquals("stationCD: line 6",un1.toString(),reader.readLine());
		assertNull("stationCD: EOL",reader.readLine());
		reader.close();
		
		// Remove the file that was created since it is for a test.
		outFile.delete();
	}

	@Test public void writeStationSummaryFile() throws IOException {
		File outFile = new File(".","station_summary.log");
			
		// Generate the station summary file
		stationList.writeStationSummary(outFile);
		assertTrue("station summary: exists",outFile.exists());
		
		// Open the file for reading and compare it line by line to the 
		// expected output.
		BufferedReader reader = new BufferedReader(new FileReader(outFile));
		assertEquals("station summary: line 1","There were 13 stations found for 6 networks.",reader.readLine());
		assertEquals("station summary: line 2","",reader.readLine());
		assertEquals("station summary: line 3","-----------------------------------------------------------------------------------------------------------------------",reader.readLine());
		assertEquals("station summary: line 4","|     NETWORK     | TOTAL  (MISS) |  NORTHWEST  |  NORTHEAST  |  SOUTHEAST  |  SOUTHWEST  |   UNKNOWN   |    MOBILE   |",reader.readLine());
		assertEquals("station summary: line 5","-----------------------------------------------------------------------------------------------------------------------",reader.readLine());
		assertEquals("station summary: line 6","| MOBILE          |     2 (    1) |    0 (   0) |    0 (   0) |    0 (   0) |    0 (   0) |    0 (   0) |    1 (   1) |",reader.readLine());
		assertEquals("station summary: line 7","-----------------------------------------------------------------------------------------------------------------------",reader.readLine());
		assertEquals("station summary: line 8","| NE              |     2 (    1) |    0 (   0) |    1 (   1) |    0 (   0) |    0 (   0) |    0 (   0) |    0 (   0) |",reader.readLine());
		assertEquals("station summary: line 9","-----------------------------------------------------------------------------------------------------------------------",reader.readLine());
		assertEquals("station summary: line 10","| NW              |     3 (    2) |    1 (   2) |    0 (   0) |    0 (   0) |    0 (   0) |    0 (   0) |    0 (   0) |",reader.readLine());
		assertEquals("station summary: line 11","-----------------------------------------------------------------------------------------------------------------------",reader.readLine());
		assertEquals("station summary: line 12","| SE              |     2 (    1) |    0 (   0) |    0 (   0) |    1 (   1) |    0 (   0) |    0 (   0) |    0 (   0) |",reader.readLine());
		assertEquals("station summary: line 13","-----------------------------------------------------------------------------------------------------------------------",reader.readLine());
		assertEquals("station summary: line 14","| SW              |     2 (    1) |    0 (   0) |    0 (   0) |    0 (   0) |    1 (   1) |    0 (   0) |    0 (   0) |",reader.readLine());
		assertEquals("station summary: line 15","-----------------------------------------------------------------------------------------------------------------------",reader.readLine());
		assertEquals("station summary: line 16","| UNKNOWN         |     2 (    1) |    0 (   0) |    0 (   0) |    0 (   0) |    0 (   0) |    1 (   1) |    0 (   0) |",reader.readLine());
		assertEquals("station summary: line 17","-----------------------------------------------------------------------------------------------------------------------",reader.readLine());
		assertEquals("station summary: line 18","|     SUMMARY     |    13 (    7) |    1 (   2) |    1 (   1) |    1 (   1) |    1 (   1) |    1 (   1) |    1 (   1) |",reader.readLine());
		assertEquals("station summary: line 19","-----------------------------------------------------------------------------------------------------------------------",reader.readLine());
		
		// Remove the file that was created since it is for a test.
		outFile.delete();
	}
}
