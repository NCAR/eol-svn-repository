package dmg.util;

import static dmg.util.ComparisonUtils.*;
import static org.junit.Assert.*;
import java.util.*;
import org.junit.*;

public class ComparisonUtilsTest {

	@Test public void testCompareCalendarCalendar() {
		Calendar first = Calendar.getInstance();
		Calendar second = Calendar.getInstance();
		second.roll(Calendar.DAY_OF_MONTH, true);
		
		assertTrue("Compare Calendar: null null", compare((Calendar)null, (Calendar)null) == 0);
		assertTrue("Compare Calendar: null value", compare((Calendar)null, second) < 0);
		assertTrue("Compare Calendar: value null", compare(first, (Calendar)null) > 0);
		assertTrue("Compare Calendar: value value", compare(first, second) < 0);
	}

	@Test public void testCompareDoubleDouble() {
		assertTrue("Compare Double: null null", compare((Double)null, (Double)null) == 0);
		assertTrue("Compare Double: null value", compare((Double)null, 1.0) < 0);
		assertTrue("Compare Double: value null", compare(-1.0, (Double)null) > 0);
		assertTrue("Compare Double: value value", compare(-1.0, 1.0) < 0);
	}

	@Test public void testCompareStringString() {
		assertTrue("Compare String: null null", compare((String)null, (String)null) == 0);
		assertTrue("Compare String: null value", compare((String)null, "") < 0);
		assertTrue("Compare String: value null", compare("", (String)null) > 0);
		assertTrue("Compare String: value value", compare("", "A") < 0);
	}
}
