package dmg;

import dmg.ua.sounding.esc.*;
import org.junit.runner.*;
import org.junit.runners.*;

@RunWith (Suite.class)
@Suite.SuiteClasses({
	ESCSoundingParserTest.class,
	ESCSoundingRecordTest.class,
	ESCSoundingTest.class
})

public class UpperAirTests {
	
    public static void main(String[] args) {
		org.junit.runner.JUnitCore.main("dmg.UpperAirTests");
    }
}
