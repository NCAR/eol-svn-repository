package dmg.ua.sounding.extract;

import org.junit.runner.*;
import org.junit.runners.*;

@RunWith (Suite.class)
@Suite.SuiteClasses({
	ESC5mbExtractorTest.class
})

public class ESCExtractorTests {

	public static void main(String[] args) {
		org.junit.runner.JUnitCore.main("dmg.ua.sounding.extract.ESCExtractorTests");
	}
}
