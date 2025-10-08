package dmg.ua.sounding.esc;

import static dmg.ua.sounding.Sounding.*;
import static dmg.ua.sounding.esc.ESCSoundingRecord.*;
import static dmg.util.LengthUtils.*;
import static dmg.util.PressureUtils.*;
import static dmg.util.TemperatureUtils.*;
import static dmg.util.TimeUtils.*;
import static dmg.util.VelocityUtils.*;
import static org.junit.Assert.*;
import dmg.util.*;

import java.io.*;
import java.util.*;
import org.junit.*;

public class ESCSoundingParserTest {

	private ESCSounding sounding;
	private ESCSoundingParser parser;
	
    @Before public void setup() throws Exception {
	parser = new ESCSoundingParser(true);
	sounding = new ESCSounding();
    }

	@Test public void parseSoundingFile() throws CalculationWarning,
	ConversionException, DateTimeException, InvalidFlagException, 
	InvalidValueException, InvalidValueWarning, IOException {
		// Build a sounding to be written to a test file.
		sounding.setActualRelease(2006, 3, 8, 23, 2, 0, UTC);
		sounding.setAltitude(2.0, METERS);
		sounding.setDataType("National Weather Service Sounding.");
		sounding.setLatitude(37.7);
		sounding.setLongitude(-122.2);
		sounding.setNominalRelease(2006, 3, 9, 0, 0, 0, UTC);
		sounding.setProjectId("0");
		sounding.setReleaseDirection(ASCENDING);
		sounding.setStationDescription("OAK Oakland, CA");
		sounding.setStationId("OAK");
		sounding.setVariableMeasurement1("Ele");
		sounding.setVariableMeasurement2("Azi");
		sounding.setVariableUnit1("deg");
		sounding.setVariableUnit2("deg");
		
		sounding.setHeaderLine(6, "Ascension No:", "1175");
		sounding.setHeaderLine(7, "Radiosonde Serial Number:", "84966655.CSN");
		sounding.setHeaderLine(8, "Radiosonde Manufacturer:", "VIZ B2");
		
		ESCSoundingRecord first = new ESCSoundingRecord();
		first.setTime(0.0);
		first.setPressure(1021.6, MILLIBARS);
		first.setTemperature(13.0, CELCIUS);
		first.setDewPoint(6.1, CELCIUS);
		first.setRelativeHumidity(63.0);
		first.setUComponent(4.1, METERS_PER_SECOND);
		first.setVComponent(0.0, METERS_PER_SECOND);
		first.setWindSpeed(4.1, METERS_PER_SECOND);
		first.setWindDirection(180.0);
		first.setLongitude(-122.2);
		first.setLatitude(37.7);
		first.setVariableField1(46.7);
		first.setVariableField2(287.6);
		first.setAltitude(2.0, METERS);
		first.setPressureFlag(QUESTIONABLE_FLAG);
		first.setTemperatureFlag(QUESTIONABLE_FLAG);
		first.setRelativeHumidityFlag(QUESTIONABLE_FLAG);
		first.setUComponentFlag(GOOD_FLAG);
		first.setVComponentFlag(GOOD_FLAG);		
		sounding.add(first);
		
		ESCSoundingRecord second = new ESCSoundingRecord();
		second.setTime(6.0);
		second.setPressure(1014.2, MILLIBARS);
		second.setTemperature(11.8, CELCIUS);
		second.setDewPoint(5.1, CELCIUS);
		second.setRelativeHumidity(63.6);
		second.setUComponent(4.1, METERS_PER_SECOND);
		second.setVComponent(-0.0, METERS_PER_SECOND);
		second.setWindSpeed(4.1, METERS_PER_SECOND);
		second.setWindDirection(180.0);
		second.setAscentRate(10.2, METERS_PER_SECOND);
		second.setLongitude(-122.2);
		second.setLatitude(37.7);
		second.setVariableField1(36.3);
		second.setVariableField2(275.3);
		second.setAltitude(63.0, METERS);
		second.setPressureFlag(QUESTIONABLE_FLAG);
		second.setTemperatureFlag(QUESTIONABLE_FLAG);
		second.setRelativeHumidityFlag(QUESTIONABLE_FLAG);
		second.setUComponentFlag(ESTIMATE_FLAG);
		second.setVComponentFlag(ESTIMATE_FLAG);
		second.setAscentRateFlag(UNCHECKED_FLAG);
		sounding.add(second);
		
		ESCSoundingRecord third = new ESCSoundingRecord();
		third.setTime(12.0);
		third.setPressure(1010.3, MILLIBARS);
		third.setTemperature(11.4, CELCIUS);
		third.setDewPoint(5.2, CELCIUS);
		third.setRelativeHumidity(65.6);
		third.setUComponent(4.1, METERS_PER_SECOND);
		third.setVComponent(-0.1, METERS_PER_SECOND);
		third.setWindSpeed(4.1, METERS_PER_SECOND);
		third.setWindDirection(271.4);
		third.setAscentRate(5.3, METERS_PER_SECOND);
		third.setLongitude(-122.199);
		third.setLatitude(37.7);
		third.setVariableField1(27.0);
		third.setVariableField2(282.1);
		third.setAltitude(95.0, METERS);
		third.setPressureFlag(QUESTIONABLE_FLAG);
		third.setTemperatureFlag(GOOD_FLAG);
		third.setRelativeHumidityFlag(GOOD_FLAG);
		third.setUComponentFlag(ESTIMATE_FLAG);
		third.setVComponentFlag(ESTIMATE_FLAG);
		third.setAscentRateFlag(UNCHECKED_FLAG);
		sounding.add(third);
		
		// Generate a test file to be parsed.
		File testFile = new File("testfile.cls");
		PrintWriter out = new PrintWriter(new FileWriter(testFile));
		out.println(sounding);
		out.println(new ESCSounding());
		out.close();
	
		// Parse the file and test it.
		List<ESCSounding> soundings = 
			parser.parseFile(new File("testfile.cls"));

		assertEquals("parseSoundings: size", 2, soundings.size());
		assertEquals("parseSoundings: first sounding", 
				sounding.toString(), soundings.get(0).toString());
		assertEquals("parseSoudnings: second sounding", 
				(new ESCSounding()).toString(), soundings.get(1).toString());		
		
		// Remove the test file now that it is no longer needed.
		testFile.delete();
	}	
}
