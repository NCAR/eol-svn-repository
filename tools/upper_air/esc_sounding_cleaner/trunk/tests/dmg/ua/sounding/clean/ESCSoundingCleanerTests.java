package dmg.ua.sounding.clean;

import org.junit.runner.*;
import org.junit.runners.*;

@RunWith (Suite.class)
    @Suite.SuiteClasses({
	    ESCSoundingCleanerTest.class
		})
    
    
    public class ESCSoundingCleanerTests {
	public static void main(String[] args) {
	    org.junit.runner.JUnitCore.main("dmg.ua.sounding.clean.ESCSoundingCleanerTests");
	}
    }