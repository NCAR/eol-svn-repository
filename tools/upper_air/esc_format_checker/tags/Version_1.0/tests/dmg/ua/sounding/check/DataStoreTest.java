package dmg.ua.sounding.check;

import static dmg.util.TimeUtils.*;
import static org.junit.Assert.*;

import dmg.util.*;
import java.io.*;
import org.junit.*;


public class DataStoreTest {

	private DataStore store;
	
	private BufferedReader reader;
	private PrintWriter log;
	
	@Before public void setUp() throws IOException {
    	PipedInputStream in = new PipedInputStream();
    	PipedOutputStream out = new PipedOutputStream(in);
    	
    	reader = new BufferedReader(new InputStreamReader(in));
    	log = new PrintWriter(out);
	
		store = new DataStore();
	}
	
	@Test public void ascendingTimeSuccess() throws IOException {
		for (double time = 0.0; time <= 10.0; time++) {	store.addTime(time); }
		store.compareDataTimeSequence(log);
		assertFalse("Ascending Time Success: ready", reader.ready());
	}
	
	@Test public void ascendingTimeSuccessWithMissingTimes() throws IOException {
		for (double time = 0.0; time <= 10.0; time++) {
			store.addTime(time);
			if (time % 2.0 == 0.0) { store.addTime(9999.0); }
		}
		store.compareDataTimeSequence(log);
		assertFalse("Ascending Time Success w/Missing: ready", reader.ready());
	}
	
	@Test public void ascendingTimeFailure() throws IOException {
		for (double time = 0.0; time <= 10.0; time++) {	store.addTime(time); }
		store.addTime(0.0);
		store.compareDataTimeSequence(log);
		assertTrue("Ascending Time Failure: ready", reader.ready());
		assertTrue("Ascending Time Failure: content", reader.readLine().contains("The sounding does not have a consistant time sequence"));
	}
	
	@Test public void ascendingTimeFailureWithMissingTimes() throws IOException {
		for (double time = 0.0; time <= 10.0; time++) {
			store.addTime(time);
			if (time % 2.0 == 0.0) { store.addTime(9999.0); }
		}
		store.addTime(0.0);
		store.compareDataTimeSequence(log);
		assertTrue("Ascending Time Failure w/Missing: ready", reader.ready());
		assertTrue("Ascending Time Failure w/Missing: content", reader.readLine().contains("The sounding does not have a consistant time sequence"));
	}
	
	@Test public void ascendingTimeWithEqualTimes() throws IOException {
		for (double time = 0.0; time <= 10.0; time++) {	store.addTime(time); }
		store.addTime(10.0);
		store.compareDataTimeSequence(log);
		assertTrue("Ascending Time w/Equal Times: ready", reader.ready());
		assertTrue("Ascending Time w/Equal Times: content", reader.readLine().contains("The sounding does not have a consistant time sequence"));
	}
	
	@Test public void ascendingTimeWithEqualTimesMissingTimes() throws IOException {
		for (double time = 0.0; time <= 10.0; time++) {
			store.addTime(time);
			if (time % 2.0 == 0.0) { store.addTime(9999.0); }
		}
		store.addTime(10.0);
		store.compareDataTimeSequence(log);
		assertTrue("Ascending Time w/Equal w/Missing: ready", reader.ready());
		assertTrue("Ascending Time w/Equal w/Missing: content", reader.readLine().contains("The sounding does not have a consistant time sequence"));		
	}
	
	@Test public void descendingTimeSuccess() throws IOException {
		for (double time = 10.0; time >= 0.0; time--) {	store.addTime(time); }
		store.compareDataTimeSequence(log);
		assertFalse("Descending Time Success: ready", reader.ready());
	}
	
	@Test public void descendingTimeSuccessWithMissingTimes() throws IOException {
		for (double time = 10.0; time >= 0.0; time--) {
			store.addTime(time);
			if (time % 2.0 == 0.0) { store.addTime(9999.0); }
		}
		store.compareDataTimeSequence(log);
		assertFalse("Descending Time Success w/Missing: ready", reader.ready());
	}
	
	@Test public void descendingTimeFailure() throws IOException {
		for (double time = 10.0; time >= 0.0; time--) {	store.addTime(time); }
		store.addTime(1.0);
		store.compareDataTimeSequence(log);
		assertTrue("Descending Time Failure: ready", reader.ready());
		assertTrue("Descending Time Failure: content", reader.readLine().contains("The sounding does not have a consistant time sequence"));
	}
	
	@Test public void descendingTimeFailureWithMissingTimes() throws IOException {
		for (double time = 10.0; time >= 0.0; time--) {
			store.addTime(time);
			if (time % 2.0 == 0.0) { store.addTime(9999.0); }
		}
		store.addTime(1.0);
		store.compareDataTimeSequence(log);
		assertTrue("Descending Time Failure w/Missing: ready", reader.ready());
		assertTrue("Descending Time Failure w/Missing: content", reader.readLine().contains("The sounding does not have a consistant time sequence"));
	}
	
	@Test public void descendingTimeWithEqualTimes() throws IOException {
		for (double time = 10.0; time >= 0.0; time--) {	store.addTime(time); }
		store.addTime(0.0);
		store.compareDataTimeSequence(log);
		assertTrue("Descending Time w/Equal Times: ready", reader.ready());
		assertTrue("Descending Time w/Equal Times: content", reader.readLine().contains("The sounding does not have a consistant time sequence"));
	}
	
	@Test public void descendingTimeWithEqualTimesMissingTimes() throws IOException {
		for (double time = 10.0; time >= 0.0; time--) {
			store.addTime(time);
			if (time % 2.0 == 0.0) { store.addTime(9999.0); }
		}
		store.addTime(0.0);
		store.compareDataTimeSequence(log);
		assertTrue("Descending Time w/Equal w/Missing: ready", reader.ready());
		assertTrue("Descending Time w/Equal w/Missing: content", reader.readLine().contains("The sounding does not have a consistant time sequence"));		
	}
	
	@Test public void initialTimeSequenceEqualValues() throws IOException {
		store.addTime(0.0);
		store.addTime(0.0);
		store.compareDataTimeSequence(log);
		assertTrue("Equal Initial Times: ready", reader.ready());
		assertTrue("Equal Initial Times: content", reader.readLine().contains("The sounding has equal times to start the series"));
	}
	
	@Test public void actualEqualNominal() throws DateTimeException, IOException {
		store.setNominalDate(buildDate(2007, 7, 10, 12, 0, 0, UTC));
		store.setActualDate(buildDate(2007, 7, 10, 12, 0, 0, UTC));
		store.compareHeaderDates(log);
		assertFalse("Equal Actual/Nominal Dates: ready", reader.ready());
	}
	
	@Test public void actualTwoHoursBeforeNominal() throws DateTimeException, IOException {
		store.setActualDate(buildDate(2007, 7, 10, 22, 0, 0, UTC));
		store.setNominalDate(buildDate(2007, 7, 11, 0, 0, 0, UTC));
		store.compareHeaderDates(log);
		assertFalse("Actual -2 Hrs from Nominal: ready", reader.ready());
	}
	
	@Test public void actualTwoHoursAfterNominal() throws DateTimeException, IOException {
		store.setActualDate(buildDate(2007, 7, 11, 2, 0, 0, UTC));
		store.setNominalDate(buildDate(2007, 7, 11, 0, 0, 0, UTC));
		store.compareHeaderDates(log);
		assertFalse("Actual +2 Hrs from Nominal: ready", reader.ready());
	}

	@Test public void actualOverTwoHoursBeforeNominal() throws DateTimeException, IOException {
		store.setActualDate(buildDate(2007, 7, 10, 21, 59, 59, UTC));
		store.setNominalDate(buildDate(2007, 7, 11, 0, 0, 0, UTC));
		store.compareHeaderDates(log);
		assertTrue("Actual over -2 Hrs from Nominal: ready", reader.ready());
		assertTrue("Actual over -2 Hrs from Nominal: content", reader.readLine().contains("not within two hours of the nominal date"));
	}
	
	@Test public void actualOverTwoHoursAfterNominal() throws DateTimeException, IOException {
		store.setActualDate(buildDate(2007, 7, 11, 2, 0, 1, UTC));
		store.setNominalDate(buildDate(2007, 7, 11, 0, 0, 0, UTC));
		store.compareHeaderDates(log);
		assertTrue("Actual over +2 Hrs from Nominal: ready", reader.ready());
		assertTrue("Actual over +2 Hrs from Nominal: content", reader.readLine().contains("not within two hours of the nominal date"));
	}
	
	@Test public void equalSfcHeaderValues() throws IOException {
		store.setHeaderLongitude(-100.032);
		store.setHeaderLatitude(32.385);
		store.setHeaderAltitude(1048.3);
		
		store.setSurfaceLongitude(-100.032, 16);
		store.setSurfaceLatitude(32.385, 16);
		store.setSurfaceAltitude(1048.3, 16);
		
		store.compareLocations(log);
		assertFalse("Equal Sfc/Head: ready", reader.ready());
	}
	
	@Test public void differentLongitudeSfcHeaderValues() throws IOException {
		store.setHeaderLongitude(-100.033);
		store.setHeaderLatitude(32.385);
		store.setHeaderAltitude(1048.3);
		
		store.setSurfaceLongitude(-100.032, 16);
		store.setSurfaceLatitude(32.385, 16);
		store.setSurfaceAltitude(1048.3, 16);
		
		store.compareLocations(log);
		assertTrue("Diff Lon Sfc/Head: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Diff Lon Sfc/Head: hdr lat", line.contains("header longitude"));
		assertTrue("Diff Lon Sfc/Head: sfc lat", line.contains("surface longitude"));
		assertTrue("Diff Lon Sfc/Head: diff", line.contains("not the same"));
	}
	
	@Test public void diifferentLatitudeSfcHeaderValues() throws IOException {
		store.setHeaderLongitude(-100.032);
		store.setHeaderLatitude(32.384);
		store.setHeaderAltitude(1048.3);
		
		store.setSurfaceLongitude(-100.032, 16);
		store.setSurfaceLatitude(32.385, 16);
		store.setSurfaceAltitude(1048.3, 16);
		
		store.compareLocations(log);
		assertTrue("Diff Lat Sfc/Head: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Diff Lat Sfc/Head: hdr lat", line.contains("header latitude"));
		assertTrue("Diff Lat Sfc/Head: sfc lat", line.contains("surface latitude"));
		assertTrue("Diff Lat Sfc/Head: diff", line.contains("not the same"));
	}
	
	@Test public void differentAltitudeSfcHeaderValues() throws IOException {
		store.setHeaderLongitude(-100.032);
		store.setHeaderLatitude(32.385);
		store.setHeaderAltitude(1048.2);
		
		store.setSurfaceLongitude(-100.032, 16);
		store.setSurfaceLatitude(32.385, 16);
		store.setSurfaceAltitude(1048.3, 16);
		
		store.compareLocations(log);
		assertTrue("Diff Alt Sfc/Head: ready", reader.ready());
		String line = reader.readLine();
		assertTrue("Diff Alt Sfc/Head: hdr alt", line.contains("header altitude"));
		assertTrue("Diff Alt Sfc/Head: sfc alt", line.contains("surface altitude"));
		assertTrue("Diff Alt Sfc/Head: diff", line.contains("not the same"));
	}

	@Test public void settingDifferentLongitudeLineSfcHeaderValues() throws IOException {
		store.setHeaderLongitude(-100.032);
		store.setHeaderLatitude(32.385);
		store.setHeaderAltitude(1048.3);
		
		store.setSurfaceLongitude(-100.032, 16);
		store.setSurfaceLatitude(32.385, 16);
		store.setSurfaceAltitude(1048.3, 16);
		
		store.setSurfaceLongitude(-90.0, 17);
		
		store.compareLocations(log);
		assertFalse("Equal Sfc/Head: ready", reader.ready());
	}
	
	@Test public void settingDifferentLatitudeLineSfcHeaderValues() throws IOException {
		store.setHeaderLongitude(-100.032);
		store.setHeaderLatitude(32.385);
		store.setHeaderAltitude(1048.3);
		
		store.setSurfaceLongitude(-100.032, 16);
		store.setSurfaceLatitude(32.385, 16);
		store.setSurfaceAltitude(1048.3, 16);
		
		store.setSurfaceLatitude(10.0, 17);
		
		store.compareLocations(log);
		assertFalse("Equal Sfc/Head: ready", reader.ready());
	}
	
	@Test public void settingDifferentAltitudeLineSfcHeaderValues() throws IOException {
		store.setHeaderLongitude(-100.032);
		store.setHeaderLatitude(32.385);
		store.setHeaderAltitude(1048.3);
		
		store.setSurfaceLongitude(-100.032, 16);
		store.setSurfaceLatitude(32.385, 16);
		store.setSurfaceAltitude(1048.3, 16);
		
		store.setSurfaceAltitude(193.0, 17);
		
		store.compareLocations(log);
		assertFalse("Equal Sfc/Head: ready", reader.ready());
	}
	
}
