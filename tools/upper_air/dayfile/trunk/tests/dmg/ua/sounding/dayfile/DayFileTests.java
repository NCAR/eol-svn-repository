package dmg.ua.sounding.dayfile;

import org.junit.runner.*;
import org.junit.runners.*;

@RunWith (Suite.class)
@Suite.SuiteClasses({
	ESCDayFileCreatorTest.class
})

public class DayFileTests {

   public static void main(String[] args) {
      org.junit.runner.JUnitCore.main("dmg.ua.sounding.dayfile.DayFileTests");
   }
}
