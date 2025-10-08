package dmg.ua.sounding.clean;

import dmg.ua.sounding.esc.*;
import dmg.util.*;

import java.io.*;
import java.util.*;

/**
 * The ESCSoundingCleaner is a tool to clean up records in an ESCSounding by setting
 * <i>screwed up</i> data sections to missing values.  These <i>screwed up</i> data
 * sections are blocks of records that are significantly different than its neighboring
 * records and include increases of pressure over 10 mb, 3 mb/second decreases in pressure,
 * decreases of altitude over 100 meters, 50 m/s increases of altitude, or 0 altitudes that
 * are out of order and not within 50 meters of the assumed surface.
 * 
 * @author Joel Clawson
 */
public class ESCSoundingCleaner {

	/** Defines the base decrease of altitude (in m) from the last accepted record. */
	public static final Double BASE_DECREASE_IN_ALTITUDE = 100.0;
	
	/** Defines the base increase of pressure (in mb) from the last accepted record. */
	public static final Double BASE_INCREASE_IN_PRESSURE = 10.0;
	
	/** Defines the allowed decrease of pressure (in mb/s) from the last accepted record. */
	public static final Double DECREASE_IN_PRESSURE_RATE = 3.0;
	
	/** Defines the allowed increase of altitude (in m/s) from the last accepted record. */
	public static final Double INCREASE_IN_ALTITUDE_RATE = 50.0;
	
	/** Defines the number of data points used to recover from a bad data section. */
	public static final int DATA_POINT_RECOVERY_COUNT = 3;
	
	/** Defines the distance allowed from the surface a zero altitude is allowed. */
	public static final Double ZERO_ALTITUDE_SURFACE_DISTANCE = 50.0;

	
	private ESCSoundingParser parser;
	private File INPUT_FILE, LOG_FILE, OUTPUT_FILE;
	
	/** Create a new instance of an ESCSoundingCleaner. */
	public ESCSoundingCleaner() {
		parser = new ESCSoundingParser(false);
	}
	
	/**
	 * Calculate the ascent rate between to the two specified records. 
	 * @param first The first record to use in the calculation.
	 * @param second The second record to use in the calculation.
	 * @return <code>null</code> if either record's time or altitude value is <code>null</code> otherwise
	 * the ascent rate between the two records.
	 */
	private Double calculateAscentRate(ESCSoundingRecord first, ESCSoundingRecord second) {
		if (first.getAltitude() == null || first.getTime() == null || second.getAltitude() == null || second.getTime() == null) {
			return null;
		} else {
			return (first.getAltitude() - second.getAltitude()) / (first.getTime() - second.getTime());
		}
	}
	
	/**
	 * Clean the specified sounding by nullifying records that exceed the acceptable thresholds and
	 * rates allowed for a sounding.
	 * @param sounding The sounding to be cleaned.
	 * @param log The stream where the cleaning messages are to be written when a record is nullified.
	 * @throws CleanerException if there is a problem cleaning the sounding.
	 */
	public void clean(ESCSounding sounding, PrintWriter log) throws CleanerException {
		// The last accepted record in the sounding.
		// We need both a pressure and altitude record because there is no guarantee that both
		// the pressure and altitude will be defined in the same record.
		ESCSoundingRecord lastAcceptedPressureRecord = null;
		ESCSoundingRecord lastAcceptedAltitudeRecord = null;

		// Pull out the records from the sounding for easier access and use.
		List<ESCSoundingRecord> records = sounding.getRecords();
		
		// Loop through all of the records in the sounding searching for bad data sections.
		for (int i = 0; i < records.size(); i++) {
			
			// The last accepted record should only be null at the start of the sounding.
			if (lastAcceptedPressureRecord == null) {
				// Only want to assign the last accepted record if the pressure is valid.
				if (records.get(i).getPressure() != null) { lastAcceptedPressureRecord = records.get(i); }
				if (records.get(i).getAltitude() != null) { lastAcceptedAltitudeRecord = records.get(i); }
				else {
					log.printf("Skipping start record for sounding:  Pressure is not defined to start searching for bad records.\n\t%s\n", records.get(i).toString());
				}
			} 
			// The last accepted record is known, so use it to determine if the next record is valid.
			else {
				
				Double changeInPressure = null;
				Double changeInAltitude = null;
				// Determine the change in pressure between the current record and the last accepted pressure record.
				if (records.get(i).getPressure() != null && records.get(i).getTime() != null) {
					changeInPressure = (lastAcceptedPressureRecord.getPressure().doubleValue() - records.get(i).getPressure().doubleValue()) /	(records.get(i).getTime().doubleValue() - lastAcceptedPressureRecord.getTime());
				}
				// Determine the change in altitude between the current record and the last accepted altitude record.
				if (records.get(i).getAltitude() != null && records.get(i).getTime() != null) {
					changeInAltitude = (lastAcceptedAltitudeRecord.getAltitude().doubleValue() - records.get(i).getAltitude().doubleValue()) / (lastAcceptedAltitudeRecord.getTime().doubleValue() - records.get(i).getTime());
				}
				
				// Handle the case where the pressure increases by more than the allowed threshold.
				if (records.get(i).getPressure() != null && 
						records.get(i).getPressure().doubleValue() - lastAcceptedPressureRecord.getPressure().doubleValue() > BASE_INCREASE_IN_PRESSURE.doubleValue()) {
					log.printf("Record being set to missing:  Pressure change is over %.1f mb from last accepted pressure of %.1f mb.\n\t%s\n", BASE_INCREASE_IN_PRESSURE, lastAcceptedPressureRecord.getPressure(), records.get(i).toString());
					nullifyRecord(records.get(i));
				}
				// Handle the case where the pressure is dropping too fast.
				else if (records.get(i).getPressure() != null && changeInPressure > DECREASE_IN_PRESSURE_RATE) {
					log.printf("Record being set to missing:  Pressure rate change is %.6f...\n\t%s\n", changeInPressure, records.get(i).toString());
					nullifyRecord(records.get(i));
				}
				// Handle the case where the altitude is drops by more than the allowed threshold.
				else if (records.get(i).getAltitude() != null &&
						lastAcceptedAltitudeRecord.getAltitude().doubleValue() - records.get(i).getAltitude().doubleValue() > BASE_DECREASE_IN_ALTITUDE.doubleValue()) {
					log.printf("Record being set to missing:  Altitude change is over %.1f m from last accepted altitude of %.1f m.\n\t%s\n", BASE_DECREASE_IN_ALTITUDE, lastAcceptedAltitudeRecord.getAltitude(), records.get(i).toString());
					nullifyRecord(records.get(i));
				}
				// Handle the case where a zero altitude is found, but it isn't near the surface.
				else if (records.get(i).getAltitude() != null && records.get(i).getAltitude() == 0.0 &&
						Math.abs(lastAcceptedAltitudeRecord.getAltitude().doubleValue()) > ZERO_ALTITUDE_SURFACE_DISTANCE.doubleValue()) {
					log.printf("Record being set to missing:  Altitude is 0.0 and the last accepted record (%.1f) is not within %.1f of surface.\n\t%s\n", lastAcceptedAltitudeRecord.getAltitude(), ZERO_ALTITUDE_SURFACE_DISTANCE, records.get(i).toString());
					nullifyRecord(records.get(i));
				}
				// Handle the case where the altitude is rising too fast.
				else if (records.get(i).getAltitude() != null && changeInAltitude > INCREASE_IN_ALTITUDE_RATE) {
					log.printf("Record being set to missing:  Altitude rate change is ...\n\t%s\n", records.get(i).toString());
					nullifyRecord(records.get(i));
				}
				
				
				// The record is fine, so make it the last accepted record.
				else {
					// Determine if this record is valid for being the last accepted pressure record.
					if (records.get(i).getPressure() != null && records.get(i).getTime() != null) { lastAcceptedPressureRecord = records.get(i); }

					// Determine if this record is valid for being the last accepted altitude record.
					if (records.get(i).getAltitude() != null && records.get(i).getTime() != null) {
						// Need to recalculate the ascent rate with the values in the sounding since some
						// of the values used previously for this calculation may have now been set to missing.
						try { 
						     records.get(i).setAscentRate(calculateAscentRate(records.get(i), lastAcceptedAltitudeRecord), VelocityUtils.METERS_PER_SECOND); 
						     records.get(i).setAscentRateFlag(null);
						}
						catch (CalculationWarning e) { throw new CleanerException("set ascent rate", e); }
						catch (ConversionException e) { throw new CleanerException("set ascent rate", e); }
						catch (InvalidValueWarning e) { throw new CleanerException("set ascent rate", e); }
                                                catch (InvalidFlagException e) { throw new CleanerException("set ascent rate", e); }
						lastAcceptedAltitudeRecord = records.get(i);
					}
				}
			}
		}
	}
	
	/**
	 * Clear the entire record by setting all of the values to <code>null</code>.
	 * @param record The record to have its records set to <code>null</code>.
	 * @throws CleanerException if there is any problem trying to set a value to <code>null</code>.
	 */
	private void nullifyRecord(ESCSoundingRecord record) throws CleanerException {
		// Need to set the flags to null first so it doesn't accidently cause exceptions that
		// shouldn't be thrown because of a conflict of value and flag.
		try {
			record.setAscentRateFlag(null);
			record.setUComponentFlag(null);
			record.setVComponentFlag(null);
			record.setRelativeHumidityFlag(null);
			record.setTemperatureFlag(null);
			record.setPressureFlag(null);
		} catch (InvalidFlagException e) {
			throw new CleanerException("nullify record", e);
		}
		
		// Now that the flags are reset to their default value, the values themselves can be reset.
		try {
			record.setAltitude(null, LengthUtils.METERS);
			record.setAscentRate(null, VelocityUtils.METERS_PER_SECOND);
			record.setDewPoint(null, TemperatureUtils.CELCIUS);
			record.setLatitude(null);
			record.setLongitude(null);
			record.setPressure(null, PressureUtils.MILLIBARS);
			record.setRelativeHumidity(null);
			record.setTemperature(null, TemperatureUtils.CELCIUS);
			record.setUComponent(null, VelocityUtils.METERS_PER_SECOND);
			record.setVariableField1(null);
			record.setVariableField2(null);
			record.setVComponent(null, VelocityUtils.METERS_PER_SECOND);
			record.setWindDirection(null);
			record.setWindSpeed(null, VelocityUtils.METERS_PER_SECOND);
		}
		// Wrap any of the exceptions thrown into a CleanerException.  I don't believe any of these
		// should actually occur.
		catch (CalculationWarning e) { throw new CleanerException("nullify record", e); } 
		catch (ConversionException e) { throw new CleanerException("nullify record", e); }
		catch (InvalidValueException e) { throw new CleanerException("nullify record", e); }
		catch (InvalidValueWarning e) { throw new CleanerException("nullify record", e); }
	}
	
    /**
     * Parse the command line arguments for the file cleaner.
     * @param args The list of arguments passed to the program.
     * @throws InvalidParameterException if there is a problem parsing any of
     * the arguments.
     */
    public void parseArguments(String[] args) throws InvalidParameterException {
        List<String> params = new ArrayList<String>(Arrays.asList(args));
        
        // Search for flags at the start of the list
        while (params.size() > 0 && params.get(0).startsWith("-")) {
            params.remove(0);
        }
        
        // Make sure the argument list is the expected size.
        if (params.size() < 2 || params.size() > 3) {
            throw new InvalidParameterException("parseArguments","args", "Invalid arugment list.  See the usage.");
        }
        
        // Set up the variables
        INPUT_FILE = new File(params.get(0));
        OUTPUT_FILE = new File(params.get(1));
        LOG_FILE = new File(params.get(2));
    }
	
    /**
     * Print out the usage instructions to the user on how to run this program.
     */
    public void printUsage() {
        System.out.println();
        System.out.printf("Usage: java %s <inputFile> <outputFile> <logFile>\n", getClass().getName());
        System.out.println();
        System.out.printf("Example: java %s output/src.cls output/clean.cls output/srcfile_clean.log\n", getClass().getName());
        System.out.println();
    }
    
    /**
     * Run the cleaner.
     */
    public void run() {
    	// Define the output streams for the cleaned sounding and for the log file.
    	PrintWriter out = null;
    	PrintWriter log = null;
    	
    	// Open the output streams so they can be written to.
    	try { 
    		log = new PrintWriter(new FileWriter(LOG_FILE));
    		out = new PrintWriter(new FileWriter(OUTPUT_FILE));
    	} catch (IOException e) {
    		System.err.println("Unable to open one of the output files:  "+e.getMessage());
    		return;
    	}
    	
    	try {
			List<ESCSounding> soundings = parser.parseFile(INPUT_FILE);
			for (ESCSounding sounding: soundings) {
				try {
					clean(sounding, log);
					out.println(sounding.toString());
				} catch (CleanerException e) {
					System.err.println("Unable to clean a sounding in "+INPUT_FILE);
					System.err.printf("\t%s\n", e.getMessage());
				}
			}
		} 
    	// The rest of these exceptions are parsing exceptions.  These should never occur
    	// unless the sounding being cleaned isn't in the proper format.
    	catch (Exception e) {
    		System.err.printf("Unable to parse the sounding in the file %s\n\t%s: %s\n", INPUT_FILE, e.getClass().getName(), e.getMessage());
    	}
    	
    	// Close the output streams if they were opened.
    	if (log != null) { log.close(); }
    	if (out != null) { out.close(); }
    }
    
    /**
     * Run the cleaning program using the files specified in the arguments.
     * @param args The sounding to be cleaned, the destination of the cleaned sounding and the log file.
     */
	public static void main(String[] args) {
		// Define the cleaner object.
		ESCSoundingCleaner cleaner = new ESCSoundingCleaner();
		// Parse the arguments so the cleaner can use them.
		try { cleaner.parseArguments(args); }
		catch (InvalidParameterException e) {
            System.out.println(e.getMessage());
            cleaner.printUsage();
            System.exit(1);
		}
		
		// Run the cleaner.
		cleaner.run();
	}
}
