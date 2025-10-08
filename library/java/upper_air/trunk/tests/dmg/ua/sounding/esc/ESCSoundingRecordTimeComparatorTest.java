package dmg.ua.sounding.esc;

import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;

import dmg.util.PressureUtils;

public class ESCSoundingRecordTimeComparatorTest {

	private ESCSoundingRecord record1, record2, record3;

    @Before public void setup() throws Exception {
    	record1 = new ESCSoundingRecord();
    	record1.setTime(5.0);
    	record1.setPressure(5.0, PressureUtils.MILLIBARS);
    	
    	record2 = new ESCSoundingRecord();
    	record2.setTime(6.0);
    	record2.setPressure(10.0, PressureUtils.MILLIBARS);
    	
    	record3 = new ESCSoundingRecord();
    	record3.setTime(5.0);
    	record3.setPressure(5.0, PressureUtils.MILLIBARS);
    }
    
    @Test public void reverseSortComparison() {
    	ESCSoundingRecordTimeComparator comparator = new ESCSoundingRecordTimeComparator(true);
    	
    	assertTrue("Rev: A < B", comparator.compare(record1, record2) > 0);
    	assertTrue("Rev: A = C", comparator.compare(record1, record3) == 0);
    	assertTrue("Rev: B > C", comparator.compare(record2, record3) < 0);
    	
    	assertTrue("Rev: B > A", comparator.compare(record2, record1) < 0);
    	assertTrue("Rev: C = A", comparator.compare(record3, record1) == 0);
    	assertTrue("Rev: C < B", comparator.compare(record3, record2) > 0);
    }

    @Test public void standardSortComparison() {
    	ESCSoundingRecordTimeComparator comparator = new ESCSoundingRecordTimeComparator(false);
    	
    	assertTrue("Std: A < B", comparator.compare(record1, record2) < 0);
    	assertTrue("Std: A = C", comparator.compare(record1, record3) == 0);
    	assertTrue("Std: B > C", comparator.compare(record2, record3) > 0);
    	
    	assertTrue("Std: B > A", comparator.compare(record2, record1) > 0);
    	assertTrue("Std: C = A", comparator.compare(record3, record1) == 0);
    	assertTrue("Std: C < B", comparator.compare(record3, record2) < 0);
    }
}
