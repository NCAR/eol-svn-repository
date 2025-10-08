package dmg;

import dmg.station.*;
import org.junit.runner.*;
import org.junit.runners.*;

@RunWith (Suite.class)
@Suite.SuiteClasses({
	StationTest.class,
	StationRestrictorTest.class,
	StationElevationComparatorTest.class,
	StationNameComparatorTest.class,
	StationPositionComparatorTest.class,
	ElevatedStationListTest.class,
	NamedStationListTest.class,
	PositionedStationListTest.class
})


public class StationTests {
    
    public static void main(String[] args) {
	org.junit.runner.JUnitCore.main("dmg.StationTests");
    }
}
