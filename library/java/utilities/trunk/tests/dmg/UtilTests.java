package dmg;

import dmg.record.*;
import dmg.util.*;

import org.junit.runner.*;
import org.junit.runners.*;

@RunWith (Suite.class)
@Suite.SuiteClasses({
	PTHDataTest.class,
	WindDataTest.class,
	AreaUtilsTest.class,
	ComparisonUtilsTest.class,
	FlowUtilsTest.class,
	HumidityUtilsTest.class,
	LengthUtilsTest.class,
	PositionUtilsTest.class,
	PressureUtilsTest.class,
	RadiationUtilsTest.class,
	StateCodeMapTest.class,
	TemperatureUtilsTest.class,
	TimeUtilsTest.class,
	VelocityUtilsTest.class,
	VolumeUtilsTest.class
})


public class UtilTests {

	public static void main(String[] args) {
		org.junit.runner.JUnitCore.main("dmg.UtilTests");
	}
}