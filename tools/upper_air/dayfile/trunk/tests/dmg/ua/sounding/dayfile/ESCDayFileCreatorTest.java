package dmg.ua.sounding.dayfile;

import static org.junit.Assert.*;
import dmg.ua.sounding.esc.*;
import dmg.util.*;

import java.io.*;
import java.util.*;
import org.junit.*;

public class ESCDayFileCreatorTest extends DayFileCreatorTest {


	@Test public void findDataFiles() throws InvalidParameterException {
		ESCDayFileCreator creator = new ESCDayFileCreator();
		creator.parseArguments(new String[] { "PRE_", "data", "dayfiles", "\\.cls$" });
		
		List<File> files = creator.findDataFiles();
		assertEquals("find data files: count", 4, files.size());
		
		// Order doesn't matter at this point.
		assertTrue("find data files: contains sounding1cls", 
				files.contains(sounding1cls));
		assertTrue("find data files: contains sounding2cls", 
				files.contains(sounding2cls));
		assertTrue("find data files: contains sounding3cls",
				files.contains(sounding3cls));
		assertTrue("find data files: contains sounding4cls", 
				files.contains(sounding4cls));
	}
	
	@Test public void findDataFilesGziped() throws InvalidParameterException {
		ESCDayFileCreator creator = new ESCDayFileCreator();
		creator.parseArguments(new String[] { "PRE_", "data", "dayfiles", "\\.cls\\.gz$"});
		
		List<File> files = creator.findDataFiles();
		assertEquals("find data files gzip: count", 4, files.size());

		// Order doesn't matter at this point.
		assertTrue("find data files gzip: contains sounding1gz", 
				files.contains(sounding1gz));
		assertTrue("find data files gzip: contains sounding2gz", 
				files.contains(sounding2gz));
		assertTrue("find data files gzip: contains sounding3gz",
				files.contains(sounding3gz));
		assertTrue("find data files gzip: contains sounding4gz", 
				files.contains(sounding4gz));
	}
	
	@Test public void findDataFilesNoMatches() throws
	InvalidParameterException {
		ESCDayFileCreator creator = new ESCDayFileCreator();
		creator.parseArguments(new String[] { "PRE_", "data", "dayfiles", "\\.txt$" });
		
		List<File> files = creator.findDataFiles();
		assertEquals("find data files no files: count", 0, files.size());
	}
	
	@Test public void createDayFiles() throws ConversionException,
	DateTimeException, InvalidParameterException, InvalidValueException, 
	InvalidValueWarning, IOException {
		ESCDayFileCreator creator = new ESCDayFileCreator();
		creator.parseArguments(new String[] { "PRE_", "data", "dayfiles", "\\.cls$"});
		creator.createDayFiles();
		
		assertTrue("create day files: directory exists", 
				(new File("dayfiles")).exists());
		assertTrue("create day files: file 1 exists", 
				(new File("dayfiles", "PRE_20060301.cls.gz")).exists());
		assertTrue("create day files: file 2 exists", 
				(new File("dayfiles", "PRE_20060302.cls.gz")).exists());
		assertEquals("create day files: file count", 2, 
				(new File("dayfiles")).listFiles().length);
	
		ESCSoundingParser parser = new ESCSoundingParser(false);
		List<ESCSounding> soundings = 
			parser.parseFile(new File("dayfiles", "PRE_20060301.cls.gz"));
		assertEquals("create day files: file 1 file count", 2, 
				soundings.size());
		assertEquals("create day files: file 1 sounding 1", 
				parser.parseFile(sounding3cls).get(0).toString(),
				soundings.get(0).toString());
		assertEquals("create day files: file 1 sounding 2", 
				parser.parseFile(sounding1cls).get(0).toString(), 
				soundings.get(1).toString());
		
		soundings = parser.parseFile(new File("dayfiles", "PRE_20060302.cls.gz"));
		assertEquals("create day files: file 2 files count", 2, 
				soundings.size());
		assertEquals("create day files: file 2 sounding 1", 
				parser.parseFile(sounding4cls).get(0).toString(), 
				soundings.get(0).toString());
		assertEquals("create day files: file 2 sounding 2", 
				parser.parseFile(sounding2cls).get(0).toString(),
				soundings.get(1).toString());
	}

	@Test public void createDayFilesNoGzip() throws ConversionException,
	DateTimeException, InvalidParameterException, InvalidValueException,
	InvalidValueWarning, IOException {
		ESCDayFileCreator creator = new ESCDayFileCreator();
		creator.parseArguments(new String[] { "-Z", "PRE_", "data", "dayfiles",
				"\\.cls$"});
		creator.createDayFiles();
		
		assertTrue("create day files -Z: directory exists", 
				(new File("dayfiles")).exists());
		assertTrue("create day files -Z: file 1 exists", 
				(new File("dayfiles", "PRE_20060301.cls")).exists());
		assertTrue("create day files -Z: file 2 exists", 
				(new File("dayfiles", "PRE_20060302.cls")).exists());
		assertEquals("create day files -Z: file count", 2, 
				(new File("dayfiles")).listFiles().length);
	
		ESCSoundingParser parser = new ESCSoundingParser(false);
		List<ESCSounding> soundings = 
			parser.parseFile(new File("dayfiles", "PRE_20060301.cls"));
		assertEquals("create day files -Z: file 1 file count", 2, 
				soundings.size());
		assertEquals("create day files -Z: file 1 sounding 1",
				parser.parseFile(sounding3cls).get(0).toString(), 
				soundings.get(0).toString());
		assertEquals("create day files -Z: file 1 sounding 2", 
				parser.parseFile(sounding1cls).get(0).toString(), 
				soundings.get(1).toString());
		
		soundings = parser.parseFile(new File("dayfiles", "PRE_20060302.cls"));
		assertEquals("create day files -Z: file 2 files count", 2,
				soundings.size());
		assertEquals("create day files -Z: file 2 sounding 1",
				parser.parseFile(sounding4cls).get(0).toString(), 
				soundings.get(0).toString());
		assertEquals("create day files -Z: file 2 sounding 2", 
				parser.parseFile(sounding2cls).get(0).toString(), 
				soundings.get(1).toString());
	}
}
