package dmg.ua.sounding.check;

import org.junit.runner.*;
import org.junit.runners.*;

@RunWith (Suite.class)
@Suite.SuiteClasses({
	DataTypeCheckStateTest.class,
	ProjectCheckStateTest.class,
	SiteCheckStateTest.class,
	LocationCheckStateTest.class,
	ActualTimeCheckStateTest.class,
	OptionalHeaderLineCheckStateTest.class,
	NominalTimeCheckStateTest.class,
	MeasurementsCheckStateTest.class,
	UnitsCheckStateTest.class,
	HeaderSeparatorCheckStateTest.class,
	DataLineCheckStateTest.class,
	DataStoreTest.class
})

public class ESCCheckFormatTests {

	public static void main(String[] args) {
		org.junit.runner.JUnitCore.
				main("dmg.ua.sounding.check.ESCCheckFormatTests");
	}
}
