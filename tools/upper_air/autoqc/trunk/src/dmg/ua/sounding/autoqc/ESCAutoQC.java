package dmg.ua.sounding.autoqc;

import static dmg.ua.sounding.esc.ESCSoundingRecord.*;
import static dmg.util.TemperatureUtils.*;
import static dmg.util.VelocityUtils.*;
import dmg.ua.sounding.esc.*;
import dmg.ua.sounding.esc.ESCSoundingRecord.*;
import dmg.util.*;

import java.io.*;
import java.util.*;
import java.util.regex.Pattern;
import java.util.zip.GZIPOutputStream;

/**
 * <p>The ESCAutoQC class is the core functionality for quality controlling
 * sounding through the process called automatic quality control (autoQC).  The
 * autoQC performs a number of checks on individual soundings and determines the
 * quality of the data within them.  The checks include bound checking the
 * individual values of a record, increasing/decreasing values for certain
 * parameters throughout the sounding, and rate checks over sections of records.
 * </p>
 * 
 * <p>The autoQC requires a file called <code>autoqc_limits.properties</code> to
 * be defined in the CLASSPATH.  This file contains a set of key/value pairs
 * that define various limits to be used for the QC. 
 * @see dmg.ua.sounding.autoqc.QCLimits for the full list of keys needed for the
 * autoQC.
 * 
 * @author Joel Clawson
 * @version 1.0
 * <p>Version 1.0 is the shared functionality extracted from the base autoQC
 * program when it was ported from C/C++ in summer 2007 to Java.  All 
 * functionality that was specific to an individual QC type was seperated into 
 * subclasses specific for that type of QC.</p>
 * <p>There were very minor changes between the C/C++ version and this Java
 * version.  These changes were mostly cosmetic (changes of function names and
 * variables) to make things cleaner and easier to understand.  The way
 * soundings are provided to the tool were changed to allow entire input and
 * output directories to be given instead of individual soundings.</p>
 * <p>The largest change is the requirement of the the properties file.  The
 * original C/C++ version had a header file for each project that required the
 * software to be recompiled for any change to the limits.  The new Java version
 * requires the limits to be in a properties file that are read in at runtime.
 * This allows changes to the limits without changing the software itself.</p>
 * <p>The final change was the removal of some of the too big for the field
 * checks for the parameters.  This is because the parser for the Java version
 * is more strict in its parsing than the C/C++ version was.  This version will
 * only parse the sounding to be QC'ed if it is in the proper ESC format.  This
 * means that any sounding with a value too big for the field, has a number
 * numberical value, or any other issue that breaks the definition of the ESC
 * format will prevent the sounding from every reaching the QC step.  Therefore,
 * the checks in the autoQC that made sure the values would fit in the field
 * were no longer required.</p>
 */
public abstract class ESCAutoQC {
	
	protected QCLimits limits;
	
	private File INPUT_DIR, LOG_DIR, OUTPUT_DIR;
	private String pattern;

	/**
	 * Create a new instance of an ESCAutoQC.
	 * String propertyFile The name of the file that contains the QC limits.
	 * @throws QCException if there is a problem loading the limits from
	 * the properties file.
	 */
	public ESCAutoQC(String propertyFile) throws QCException {
		limits = new QCLimits(ResourceBundle.getBundle(propertyFile));
	}
	
	/**
	 * Determine if the specified record can have it's pressure rates checked.
	 * @param record The record to be tested.
	 * @return <code>true</code> if the pressure rates can be tested for the
	 * record, <code>false</code> otherwise.
	 */
	public abstract boolean canCheckPressureRates(ESCSoundingRecord record);
	
	/**
	 * Check the two records to see if the pressures between them are decreasing
	 * (and in some cases equal).  This will set the pressure, temperature, and
	 * relative humidity flags to QUESTIONABLE for the next record only if the
	 * pressure difference is decreasing (or equal to in some cases).
	 * @param current The current record being tested.
	 * @param next The next record in the sounding to be tested agains the
	 * current record.
	 * @param log The stream where errors are to be written.
	 * @return an error flag that is <code>true</code> if the check does not 
	 * pass or <code>false</code> if it does pass. 
	 */
	public boolean decreasingPressureCheck(ESCSoundingRecord current,
			ESCSoundingRecord next, PrintWriter log) {

		boolean pressureError = false;

		// Only perform the check if both pressures are valid.
		if (current.getPressure() != null && next.getPressure() != null) {
		
			// Current pressure > threshold
			if (current.getPressure().compareTo(
					limits.getDecreasingPressureCheck()) > 0) {
				
				// Determine if equal pressures also cause problems.
				if (!isDecreasingPressure(current, next)) {
					pressureError = true;
					log.printf("Pressure increase or equal at pressure: %6.1f "+
							"and pressure: %6.1f at time %7.1f sec\n", 
							current.getPressure(), next.getPressure(), 
							next.getTime());

					// Only update the next record's flags.
					updatePressureFlag(next, QUESTIONABLE_FLAG);
					updateTemperatureFlag(next, QUESTIONABLE_FLAG);
					updateRelativeHumidityFlag(next, QUESTIONABLE_FLAG);
					
					// Force the output printed to the log to be registered and
					// printed to the stream.
					log.flush();
				}
			}
		
			// Current pressure <= threshold
			else {
				// Only care if increasing (can be equal pressures)
				if (current.getPressure().compareTo(next.getPressure()) < 0) {
					pressureError = true;
					log.printf("Pressure increase pressure: %6.1f and pressure:"
							+ " %6.1f at time %7.1f sec\n", 
							current.getPressure(), next.getPressure(),
							next.getTime());
					
					// Only update the next record's flags.
					updatePressureFlag(next, QUESTIONABLE_FLAG);
					updateTemperatureFlag(next, QUESTIONABLE_FLAG);
					updateRelativeHumidityFlag(next, QUESTIONABLE_FLAG);
					
					// Force the output printed to the log to be registered and
					// printed to the stream.
					log.flush();
				}
			}
		}
		
		return pressureError;
	}

	/**
	 * Check to see if the change in the ascension rates is acceptable.  This
	 * will flag the pressure values for all records between the end points
	 * (inclusive) with the appropriate flag if the change is too great.
	 * @param records The list of records to use to perform the check.
	 * @param previousAscentRate The previously calculated ascension rate for
	 * past records.
	 * @param begin The flag that marks the check as the first ascension rate
	 * check for the sounding.
	 * @param log The stream where errors are to be written.
	 * @return The calculated ascension rate for the records or 0.0 if it
	 * could not be calculated.
	 */
	public Double differentialAscensionRate(List<ESCSoundingRecord> records,
			Double previousAscentRate, boolean begin, PrintWriter log) {
		
		// Initiailze vairables to make them easier to use.
		ESCSoundingRecord current = records.get(0);
		ESCSoundingRecord next = records.get(records.size() - 1);
		double ascentRate = 0.0;
		
		// Make sure that all of the values needed for the calculation
		// are valid values.
		if (current.getTime() != null && current.getAltitude() != null &&
				next.getTime() != null && next.getAltitude() != null) {
		
			// Calculate the ascent rate (if possible)
			if (current.getTime().compareTo(next.getTime()) != 0 &&
					current.getAltitude().compareTo(next.getAltitude()) != 0) {
				ascentRate = ((next.getAltitude() - current.getAltitude()) /
						(next.getTime() - current.getTime()));
			}
			
			// Need to handle the case where the previous ascent rate is
			// passed in as a null value.
			if (previousAscentRate == null) { previousAscentRate = 0.0; }
			
			// Only perform the checks if this isn't the first ascent rate
			// of the sounding.
			if (!begin) {
				// Calculate the ascent rate chagne.
				double rate = previousAscentRate - ascentRate;
				
				// Look for ascent rate changes that are at least QUESTIONABLE.
				if ((new Double(Math.abs(rate))).compareTo(
						limits.getQuestionableAscentRateChange()) >= 0) {
					// Determine if the change is BAD or just QUESTIONABLE.
					ESCFlag flag =
						(new Double(Math.abs(rate))).compareTo(
								limits.getBadAscentRateChange()) >= 0 ?
								BAD_FLAG : QUESTIONABLE_FLAG;
						
					// Update all of the records between the end points with
					// the appropriate flag.
					for (ESCSoundingRecord record: records) {
						updatePressureFlag(record, flag);
					}
	
					log.printf("Rapid Ascendrate change between pressure: %6.1f"
							+ "and %6.1f is: %6.1f m/s\n", 
							current.getPressure(), next.getPressure(), rate);
					
					// Force the output printed to the log to be registered and
					// printed to the stream.
					log.flush();
				}
			}
		}
		
		// Return the calculated ascent rate for the records.
		return ascentRate;
	}
	
	/**
	 * Search the input directory recursively for soundings that match the
	 * file pattern provided from the command line {default \.cls(\.gz)?}.
	 * @return The list of sounding files to be qc'ed.
	 */
	public List<File> findInputFiles() {
		List<File> files = new ArrayList<File>();
		
		// Generate the pattern to use to find the sounding files.
		Pattern pattern = Pattern.compile(this.pattern);
		
		// Create a list of files to check for sounding files.
		List<File> cycle = new ArrayList<File>();
		cycle.add(INPUT_DIR);
		
		while (!cycle.isEmpty()) {
			// Remove the file so the list will eventually be empty.
			File current = cycle.remove(0);
			
			if (current.isDirectory()) {
				// Add all of the directory's files to the cycle list.
				cycle.addAll(Arrays.asList(current.listFiles()));
			} else if (pattern.matcher(current.getName()).find()) {
				// The file name matched the pattern, so add it to the sounding
				// list.
				files.add(current);
			}
		}
		
		return files;
	}

	/**
	 * Check to see if the altitude is increasing between the specified records.
	 * This check is only performed if both altitudes exist and there was not
	 * a pressure error.  If an equal or decreasing altitude difference is
	 * found, it will flag the pressure, temperature, and relative humidity
	 * values for both records QUESTIONABLE.
	 * @param start The first record to be checked.
	 * @param end The second record to be checked.
	 * @param pressureError A flag that tells the function if a pressure error
	 * was discovered previous to this check.
	 * @param log The stream where errors are to be written.
	 */
	public void increasingAltitudeCheck(ESCSoundingRecord start,
			ESCSoundingRecord end, boolean pressureError, PrintWriter log) {
		
		if (start.getAltitude() != null && end.getAltitude() != null &&
				!pressureError && 
				end.getAltitude().compareTo(start.getAltitude()) <= 0) {
			log.printf("Altitude decrease or equal at pressure %6.1f between" +
					" altitude %6.1f and %6.1f\n", end.getPressure(),
					end.getAltitude(), start.getAltitude());
			
			updatePressureFlag(start, QUESTIONABLE_FLAG);
			updateTemperatureFlag(start, QUESTIONABLE_FLAG);
			updateRelativeHumidityFlag(start, QUESTIONABLE_FLAG);

			updatePressureFlag(end, QUESTIONABLE_FLAG);
			updateTemperatureFlag(end, QUESTIONABLE_FLAG);
			updateRelativeHumidityFlag(end, QUESTIONABLE_FLAG);
			
			// Force the output printed to the log to be registered and
			// printed to the stream.
			log.flush();
		}
	}
	
	/**
	 * Determine if the difference between the pressures of the current
	 * record and the next record is decreasing (or equal to in some cases).
	 * @param current The current record to be tested.
	 * @param next The record after the current record used to compare.
	 * @return <code>true</code> if the pressures decrease between the two
	 * records, <code>false</code> if they don't.
	 */
	public abstract boolean isDecreasingPressure(ESCSoundingRecord current,
			ESCSoundingRecord next);
	
	/**
	 * Replace all of the wind values with their appropriate missing values.
	 * This includes the U and V components, wind speed, wind direction and the
	 * U and V component flags.
	 * @param record The record that is to have the wind values set to missing.
	 */
	public void nullifyWindValues(ESCSoundingRecord record) {
		// Need to nullify the wind component flags first in the case that 
		// a wind componen flag is set.  This will prevent an exception 
		// later and the sounding will automatically set them to missing 
		// as desired.
		try {
			record.setUComponentFlag(null);
			record.setVComponentFlag(null);
		} 
		// This cannot happen since null is a valid flag, so bomb if it
		// occurs because something has changed in the sounding definition.
		catch (InvalidFlagException e) {
			e.printStackTrace();
			System.exit(1);
		}
		
		try {
			// The catching of the calculation warnings need to be unique
			// to each set method to allow all of the values to be reset
			// without causing an error.  They can be ignored because any
			// warning that may be thrown early will not be valid by the
			// time all of the values have been reset to null/missing
			// values.
			try {
				record.setWindSpeed(null, VelocityUtils.METERS_PER_SECOND);
			} catch (CalculationWarning e) {}
			try { record.setWindDirection(null); }
			catch (CalculationWarning e) {} 
			try {
				record.setUComponent(null, VelocityUtils.METERS_PER_SECOND);
			} catch (CalculationWarning e) {}
			try {
				record.setVComponent(null, VelocityUtils.METERS_PER_SECOND);
			} catch (CalculationWarning e) {}
		}
		// This cannot happen as long as METERS_PER_SECOND is a valid wind
		// conversion, so bomb if it occurs.
		catch (ConversionException e) {
			e.printStackTrace();
			System.exit(1);
		}
		// This cannot occur unless the definition of the sounding prevents
		// null from being a valid value, so bomb if this happens.
		catch (InvalidValueWarning e) {
			e.printStackTrace();
			System.exit(1);
		}

	}
	
	/**
	 * Parse the command line arguments for the qc.
	 * @param args The list of arguments passed to the program.
	 * @throws InvalidParameterException if there is a problem parsing any of
	 * the arguments.
	 */
	public void parseArguments(String[] args) throws InvalidParameterException {
		// Need to skip the first argument since this is the auto QC properties.
		List<String> params = new ArrayList<String>(Arrays.asList(args).subList(1, args.length));
		
		// Search for flags at the start of the list
		while (params.size() > 0 && params.get(0).startsWith("-")) {
			params.remove(0);
		}
		
		// Make sure the argument list is the expected size.
		if (params.size() < 3 || params.size() > 4) {
			throw new InvalidParameterException("parseArguments","args",
					"Invalid arugment list.  See the usage.");
		}
		
		// Set up the variables
		INPUT_DIR = new File(params.get(0));
		OUTPUT_DIR = new File(params.get(1));
		LOG_DIR = new File(params.get(2));
		// Set the default pattern if it didn't come through the command line.
		pattern = params.size() == 4 ? params.get(3) : "\\.cls(\\.gz)?$";
	}
	
	/**
	 * Print out the usage instructions to the user on how to run this program.
	 */
	public void printUsage() {
		System.out.println();
		System.out.printf("Usage: java %s <dataDirectory> <outputDirectory>\n", 
				getClass().getName());
		System.out.printf("Usage: java %s <dataDirectory> <outputDirectory> " +
				"<logDirectory> <filePattern>\n", getClass().getName());
		System.out.printf("Usage: java %s <flag> <dataDirectory> " +
				"<outputDirectory> <logDirectory> <filePattern>\n", 
				getClass().getName());
		System.out.println();
		System.out.printf("Flags:\n");
		System.out.printf("\t-Z : Turn off the compression of the final " +
				"data.\n");
		System.out.println();
		System.out.printf("Example: java %s final dayfiles logs\n", 
				getClass().getName());
		System.out.printf("Example: java %s final dayfiles logs " +
				"\\.cls\\.qc(\\.gz)?\n", getClass().getName());
		System.out.printf("Example: java %s -Z final dayfiles logs " +
				"\\.cls\\.qc(\\.gz)?\n", getClass().getName());
		System.out.println();
	}

	/**
	 * Execute all of the quality control checks on the specified sounding.
	 * @param sounding The sounding to be checked.
	 * @param log The stream where errors are to be written.
	 */
	public void qc(ESCSounding sounding, PrintWriter log) {
		qcValues(sounding, log);
		qcRates(sounding, log);
		qcNWS(sounding.getRecords(), log);
	}
	
	/**
	 * Quality control the altitude value for the specified record.  This makes
	 * sure the altitude is within the acceptable range and flags the pressure,
	 * temperature, and relative humidity as QUESTIONABLE if it is not.
	 * @param record The record to be checked.
	 * @param log The stream where errors are to be written.
	 * @param recordNumber The index of the record in the sounding.
	 */
	public void qcAltitudeValue(ESCSoundingRecord record, PrintWriter log,
			int recordNumber) {
		// Only can test with a valid value.
		if (record.getAltitude() != null) {
			// Look for BAD altitude values and flag the pressure, temperature,
			// and relative humidity values accordingly.
			if (record.getAltitude().compareTo(
					limits.getMaximumQuestionableAltitude()) > 0 ||
					record.getAltitude().compareTo(
							limits.getMinimumQuestionableAltitude()) < 0)
			{
				log.printf("Alt at press: %6.1f is: %7.1f m.\n",
						record.getPressure(), record.getAltitude());
				updatePressureFlag(record, QUESTIONABLE_FLAG);
				updateTemperatureFlag(record, QUESTIONABLE_FLAG);
				updateRelativeHumidityFlag(record, QUESTIONABLE_FLAG);
				
				// Force the output printed to the log to be registered and
				// printed to the stream.
				log.flush();
			}
		}
	}
	
	/**
	 * Quality control the ascent rate value for the specified record.  This
	 * tests that the ascent/decent rate falls within the acceptable range or
	 * will flag the pressure QUESTIONABLE.
	 * @param record The record to be checked.
	 * @param log The stream where errors are to be written.
	 * @param recordNumber The index of the record in the sounding.
	 */
	public void qcAscentRateValue(ESCSoundingRecord record, PrintWriter log,
			int recordNumber) {
		// Only try to QC a valid value.
		if (record.getAscentRate() != null) {
			// Look for QUESTIONABLE ascent rate values.  This needs to be 
			// an absolute value comparison to allow both radiosondes and 
			// dropsondes.
			if ((new Double(Math.abs(record.getAscentRate()))).compareTo(
					limits.getMaximumQuestionableAscentRate()) > 0) {
				log.printf("Ascendrate at press: %6.1f is: %7.1f m/s.\n",
						record.getPressure(), record.getAscentRate());
				updatePressureFlag(record, QUESTIONABLE_FLAG);
				
				// Force the output printed to the log to be registered and
				// printed to the stream.
				log.flush();
			}
		}
	}
	
	/**
	 * Test the dew point field for the specified record.  This makes sure the
	 * dew point does not exceed the lower questionable bounds and flags the
	 * relative humidity accordingly.
	 * @param record The record to be checked.
	 * @param log The stream where errors are to be written.
	 */
	public void qcDewPointField(ESCSoundingRecord record, PrintWriter log) {
		if (record.getDewPoint() != null && 
				record.getDewPoint().compareTo( 
					limits.getMinimumQuestionableDewPoint()) <= 0) {
			log.printf("DewPt at pressure: %6.1f mb is: %7.1f deg .C.\n", 
					record.getPressure(), record.getDewPoint());
			try { record.setDewPoint(-99.9, CELCIUS); }
			catch (DefaultException e) {
				e.printStackTrace();
				System.exit(1);
			} catch (DefaultWarning e) {
				e.printStackTrace();
				System.exit(1);
			}
			updateRelativeHumidityFlag(record, QUESTIONABLE_FLAG);
			
			// Force the output printed to the log to be registered and
			// printed to the stream.
			log.flush();
		}
	}
	
	/**
	 * Quality control the dew point value for the specified record.  This tests
	 * to see if the dew point value is acceptable related to thresholds and the
	 * temperature and relative humidity values.  Any flagging caused by a dew
	 * point is associated with the relative humidity.
	 * @param record The record to be checked.
	 * @param log The stream where the errors are to be written.
	 * @param recordNumber The index of the record in the sounding.
	 */
	public void qcDewPointValue(ESCSoundingRecord record, PrintWriter log,
			int recordNumber) {
		
		// Only try to QC a real dew point
		if (record.getDewPoint() != null) {
			// (1) Log the issue of a dew point without a relative humidity.
			// This shouldn't happen because the sounding will automatically
			// calculate the value if it is missing.  It will only happen if
			// the rh can't be calculated.
			if (record.getRelativeHumidity() == null) {
				log.printf("Dew point with missing RH at press: %6.1f.\n",
						record.getPressure());
			}
			// (2) Look for a QUESTIONABLE or really bad dew point value.  Since
			// dew point does not have a flag, the relative humidity flag is
			// changed.
			else if (record.getDewPoint().compareTo(
					limits.getMaximumQuestionableDewPoint()) > 0) {
				log.printf("Dew point at press: %6.1f is %6.1f deg .C.\n",
						record.getPressure(), record.getDewPoint());
				
				// Look for dew point values that are too bad and set to missing
				if (record.getDewPoint().compareTo(
						limits.getMaximumMissingDewPoint()) >= 0) {
					// Need to nullify the relative humidity flag first in the 
					// case that the relative humidity flag is set.  This will 
					// prevent an exception later andthe sounding will 
					// automatically set it to missing as desired.
					try { record.setRelativeHumidityFlag(null); }
					// This cannot happen since null is a valid flag, so bomb if
					// it occurs because something has changed in the sounding 
					// definition.
					catch (InvalidFlagException e) { 
						e.printStackTrace();
						System.exit(1);
					}
					
					try { 
						// The catching of the calculation warnings need to be 
						// unique to each set method to allow all of the values 
						// to be reset without causing an error.  They can be 
						// ignored because any warning that may be thrown early 
						// will not be valid by the time all of the values have 
						// been reset to null/missing values or if it is an sea
						// level pressure calculation the ESC format does not 
						// care and can be ignored anyway.
						try { record.setRelativeHumidity(null); }
						catch (CalculationWarning e) {}
						try { 
							record.setDewPoint(null, TemperatureUtils.CELCIUS);
						} catch (CalculationWarning e) {}
					}
					// This cannot happen as long as CELCIUS is a valid 
					// temperature conversion, so bomb if it occurs.
					catch (ConversionException e) {
						e.printStackTrace();
						System.exit(1);
					}
					// This cannot occur unless the definition of the sounding 
					// prevents null from being a valid value, so bomb if this
					// happens.
					catch (InvalidValueWarning e) {
						e.printStackTrace();
						System.exit(1);
					}
				}
				// or it is just QUESTIONABLE
				else {
					updateRelativeHumidityFlag(record, QUESTIONABLE_FLAG);
				}
				
				// Force the output printed to the log to be registered and
				// printed to the stream.
				log.flush();
			}
			
			// (3) When the dew point is greater than the temperature the RH is
			// questionable.
			else if (record.getDewPoint().compareTo(
					record.getTemperature()) > 0) {
				log.printf("Dew point greater than Temp at press: %6.1f.\n", 
						record.getPressure());
				updateRelativeHumidityFlag(record, QUESTIONABLE_FLAG);
				// Force the output printed to the log to be registered and
				// printed to the stream.
				log.flush();
			}
		}
	}
	
	/**
	 * This is a special function used by the NWS MicroArt QC process which
	 * performs some extra checks after the more general tests.  This can also
	 * be used for any other post general QC tests.
	 * @param records The list of records to be tested.
	 * @param log The stream where errors are to be written.
	 */
	public abstract void qcNWS(List<ESCSoundingRecord> records,PrintWriter log);

	/**
	 * Quality control the pressure value for the specified record.  This will
	 * make sure the pressure value falls within the questionable limits and 
	 * wil flag the pressure BAD if it is out of the range or will set it to
	 * missing if the value is too bad. 
	 * @param record The record to be checked.
	 * @param log The stream where errors are to be written.
	 * @param recordNumber The index of the record in the sounding.
	 */
	public void qcPressureValue(ESCSoundingRecord record, PrintWriter log,
			int recordNumber) {
		
		// Only try to QC a valid value.
		if (record.getPressure() != null) {
			// (1) Look for a BAD pressure value.  This occurs when the pressure
			// is beyond the QUESTIONABLE range, but should not be replaced with
			// the missing value
			if ((record.getPressure().compareTo(
					limits.getMaximumQuestionablePressure()) > 0 &&
					record.getPressure().compareTo(
							limits.getMaximumMissingPressure()) < 0) ||
							record.getPressure().compareTo(
								limits.getMinimumQuestionablePressure()) <= 0)
			{
				log.printf("Press: %6.1f mb on line %4d.\n",
						record.getPressure(), recordNumber);
				updatePressureFlag(record, BAD_FLAG);

				// Force the output printed to the log to be registered and
				// printed to the stream.
				log.flush();
			}

			// (2) Look for a pressure value that is worse than BAD and set it 
			// to MISSING.
			else if (record.getPressure().compareTo(
					limits.getMaximumMissingPressure()) >= 0 ||
					record.getPressure().compareTo(0.0) < 0) {
				log.printf("Press: %6.1f mb on line %4d.\n",
						record.getPressure(), recordNumber);

				// Need to nullify the pressure flag first in the case that the
				// pressure flag is set.  This will prevent an exception later 
				// and the sounding will automatically set it to missing as 
				// expected.
				try { record.setPressureFlag(null); }
				// This cannot happen since null is a valid flag, so bomb if it
				// occurs because something has changed in the sounding 
				// definition.
				catch (InvalidFlagException e) { 
					e.printStackTrace();
					System.exit(1);
				}

				try { record.setPressure(null, PressureUtils.MILLIBARS); }
				// This only occurs from a sea level pressure calculation and 
				// ESC doesn't care about that, so neither do we.
				catch (CalculationWarning e) {}
				// This cannot happen as long as MILLIBARS is a valid pressure
				// conversion, so bomb if it occurs.
				catch (ConversionException e) {
					e.printStackTrace();
					System.exit(1);
				}
				// This cannot occur unless the definition of the sounding 
				// prevents null from being a valid value, so bomb if this
				// happens.
				catch (InvalidValueWarning e) {
					e.printStackTrace();
					System.exit(1);
				}

				// Force the output printed to the log to be registered and
				// printed to the stream.
				log.flush();
			}
		}
	}
	
	/**
	 * Perform a set of quality control checks that involve checking different
	 * rates across the sounding.  It includes a decreasing pressure check, an 
	 * increasing altitude check, a super adiabatic laspe rate check, a rapid
	 * temperature increase check, a check for proper time order, and more.
	 * @param sounding The sounding to be checked.
	 * @param log The stream where errors are to be written.
	 */
	public void qcRates(ESCSounding sounding, PrintWriter log) {
		List<ESCSoundingRecord> records = sounding.getRecords();

		// Initialize variables to be maintained throughout the loop.
		boolean begin = true, pressureError = false;
		Double previousAscentRate = null, previousTime = null;

		// Loop through all of the records in the sounding.
		for (int i = 0; i + 1 < records.size(); i++) {
			int next = i + limits.getAveragingValue();
			List<ESCSoundingRecord> recordRange = records.subList(i, 
					Math.min(next + 1, records.size()));
			
			// Only can perform the checks if the expected end record (next)
			// exists in the sounding.
			if (next < records.size()) {
				pressureError = false;
				
				// Check the pressure rates
				if (records.get(i).getPressure() != null &&
						records.get(next).getPressure() != null) {
					
					// Determine if the pressure rates can be checked.
					if (canCheckPressureRates(records.get(next))) {
						
						// Can only perform the checks if the records have
						// valid time values.
						if (records.get(i).getTime() != null &&
								records.get(next).getTime() != null) {
							
							// Test for rapid pressure changes.
							// (Need to do next + 1 to include next in the 
							// list.)
							rapidPressureCheck(recordRange, log);
							
							// Can only preform ascent rate checks if the
							// records have valid altitude values.
							if (records.get(i).getAltitude() != null &&
									records.get(next).getAltitude() != null) {
								// Test for ascent rate changes.
								previousAscentRate = 
									differentialAscensionRate(recordRange,
											previousAscentRate, begin, log);

								if (begin) { begin = false; }
							}							
						}
					}
				
					// Only can perform the super adiabatic and rapid
					// temperature change checks if the altitudes are defined.
					if (records.get(i).getAltitude() != null &&
							records.get(next).getAltitude() != null) {
						
						// Only perform the surface check within 10 millibars
						// of the surface (forces the first record's pressure
						// to be valid).
						if (records.get(0).getPressure() == null || 
								Math.abs(records.get(i).getPressure() - 
								records.get(0).getPressure()) >= 10.0) {
							superAdiabaticLapseRateCheck(recordRange,
									limits.getQuestionableLapseRate(),
									limits.getBadLapseRate(), log);
						} else {
							superAdiabaticLapseRateCheck(recordRange,
									limits.getQuestionableSurfaceLapseRate(),
									limits.getBadSurfaceLapseRate(), log);
						}
						
						// Test for rapid temperature changes.
						rapidTemperatureCheck(recordRange, log);
					}
				
					// Can only check for decreasing pressure if the end point
					// times and pressures are valid.
					if (records.get(i+1).getTime() != null &&
							records.get(i).getTime() != null &&
							records.get(i+1).getPressure() != null) {
						pressureError = decreasingPressureCheck(records.get(i),
								records.get(i+1), log);
					}
					
					// Make sure that the altitude is increasing.
					increasingAltitudeCheck(records.get(i), records.get(i+1),
							pressureError, log);
				}
			
				// Make sure the time is heading in the correct direction
				// for the sounding.  (Should be increasing for an upsonde
				// and decreasing for a dropsonde.)
				previousTime = timeCheck(records.get(i), records.get(i+1),
						previousTime, log);
			}
		}
	}
	
	/**
	 * Quality control the relative humidity value for the specified record.  It
	 * makes sure the rh value is between 0 and 100, marking rh values over 100
	 * as QUESTIONABLE and setting negative rh values to missing.  It also makes
	 * sure that the dew point equals the temperature when the rh value equals
	 * 100.
	 * @param record The record to be checked.
	 * @param log The stream where errors are to be written.
	 * @param recordNumber The index of the record in the sounding.
	 */
	public void qcRelativeHumidityValue(ESCSoundingRecord record, 
			PrintWriter log, int recordNumber) {
		
		// Only try to QC a valid value.
		if (record.getRelativeHumidity() != null) {
			
			// (1) Look for a QUESTIONABLE relative humidity value.
			if (record.getRelativeHumidity().compareTo(100.0) > 0) {
				log.printf("RH at press: %6.1f is: %6.1f \n",
						record.getPressure(), record.getRelativeHumidity());
				updateRelativeHumidityFlag(record, QUESTIONABLE_FLAG);
				
				// Force the output printed to the log to be registered and
				// printed to the stream.
				log.flush();
			}
			// (2) Look for a value so bad that it must be replaced with missing.
			else if (record.getRelativeHumidity().compareTo(0.0) < 0) {
				log.printf("RH at press: %6.1f is: %6.1f \n",
						record.getPressure(), record.getRelativeHumidity());
				
				// Need to nullify the relative humidity flag first in the case
				// that the relative humidity flag is set.  This will prevent an 
				// exception later andthe sounding will automatically set it to
				// missing as desired.
				try { record.setRelativeHumidityFlag(null); }
				// This cannot happen since null is a valid flag, so bomb if it
				// occurs because something has changed in the sounding 
				// definition.
				catch (InvalidFlagException e) { 
					e.printStackTrace();
					System.exit(1);
				}
				
				try { 
					// The catching of the calculation warnings need to be 
					// unique to each set method to allow all of the values to 
					// be reset without causing an error.  They can be ignored 
					// because any warning that may be thrown early will not be
					// valid by the time all of the values have been reset to 
					// null/missing values or if it is an sea level pressure 
					// calculation the ESC format does not care and can be 
					// ignored anyway.
					try { record.setRelativeHumidity(null); }
					catch (CalculationWarning e) {}
					try { record.setDewPoint(null, TemperatureUtils.CELCIUS); }
					catch (CalculationWarning e) {}
				}
				// This cannot happen as long as CELCIUS is a valid temperature
				// conversion, so bomb if it occurs.
				catch (ConversionException e) {
					e.printStackTrace();
					System.exit(1);
				}
				// This cannot occur unless the definition of the sounding 
				// prevents null from being a valid value, so bomb if this 
				// happens.
				catch (InvalidValueWarning e) {
					e.printStackTrace();
					System.exit(1);
				}
				
				// Force the output printed to the log to be registered and
				// printed to the stream.
				log.flush();
			}
			// (3) Need to make sure that dew point and temperature values are
			// equal when the relative humidity displays as 100.0
			else if (Math.abs(record.getRelativeHumidity() - 100.0) < .001 &&
					(record.getDewPoint() != null && 
							record.getTemperature() != null && 
				record.getDewPoint().compareTo(record.getTemperature()) != 0))
			{
				
				try {
					record.setDewPoint(record.getTemperature(),
							TemperatureUtils.CELCIUS);
				}
				// This can be ignored because sea level pressure is not used,
				// the dew point is being set manually, and the relative 
				// humidity is already set (or it wouldn't make it here).
				catch (CalculationWarning e) {}
				// This cannot happen as long as CELCIUS is a valid temperature
				// conversion, so bomb if it occurs.
				catch (ConversionException e) {
					e.printStackTrace();
					System.exit(1);
				}
				// This cannot occur unless the definition of the sounding 
				// prevents null from being a valid value, so bomb if this
				// happens.
				catch (InvalidValueWarning e) {
					e.printStackTrace();
					System.exit(1);
				}
				
			}
		}
	}
	
	/**
	 * Quality control the temperature value for the specified record.  This
	 * checks to see if the temperature is QUESTIONABLE or if it should be
	 * replaced with missing if it is considered to be too bad.
	 * @param record The record to be checked.
	 * @param log The stream where errors are to be written.
	 * @param recordNumber The index of the record in the sounding.
	 */
	public void qcTemperatureValue(ESCSoundingRecord record, PrintWriter log,
			int recordNumber) {
		
		// Only try to QC a valid value.
		if (record.getTemperature() != null) {
			// (1) Look for a QUESTIONABLE temperature value.
			if ((record.getTemperature().compareTo(
					limits.getMaximumQuestionableTemperature()) > 0 || 
				record.getTemperature().compareTo(
						limits.getMinimumQuestionableTemperature()) < 0) &&
				record.getTemperature().compareTo(
						limits.getMaximumMissingTemperature()) < 0) {
				log.printf("Temp at press: %6.1f is: %6.1f deg .C.\n", 
						record.getPressure(), record.getTemperature());
				
				updateTemperatureFlag(record, QUESTIONABLE_FLAG);
				
				// Force the output printed to the log to be registered and
				// printed to the stream.
				log.flush();
			}
			// (2) Look for a temperature that is so bad that it must be set to
			// missing.
			else if (record.getTemperature().compareTo(
				limits.getMaximumMissingTemperature()) >= 0) {
				
				log.printf("Temp at press: %6.1f is: %6.1f deg .C.\n", 
						record.getPressure(), record.getTemperature());
				
				// Need to nullify the temperature flag first in the case that 
				// the temperature flag is set.  This will prevent an exception 
				// later and the sounding will automatically set it to missing 
				// as desired.
				try { record.setTemperatureFlag(null); }
				// This cannot happen since null is a valid flag, so bomb if it
				// occurs because something has changed in the sounding
				// definition.
				catch (InvalidFlagException e) { 
					e.printStackTrace();
					System.exit(1);
				}
				
				try { 
					// The catching of the calculation warnings need to be 
					// unique to each set method to allow all of the values to
					// be reset without causing an error.  They can be ignored
					// because any warning that may be thrown early will not be
					// valid by the time all of the values have been reset to 
					// null/missing values or if it is an sea level pressure 
					// calculation the ESC format does not care and can be 
					// ignored anyway.
					try {
						record.setTemperature(null, TemperatureUtils.CELCIUS);
					} catch (CalculationWarning e) {}
					try { record.setDewPoint(null, TemperatureUtils.CELCIUS); }
					catch (CalculationWarning e) {}
				}
				// This cannot happen as long as CELCIUS is a valid temperature
				// conversion, so bomb if it occurs.
				catch (ConversionException e) {
					e.printStackTrace();
					System.exit(1);
				}
				// This cannot occur unless the definition of the sounding
				// prevents null from being a valid value, so bomb if this 
				// happens.
				catch (InvalidValueWarning e) {
					e.printStackTrace();
					System.exit(1);
				}
	
				// Force the output printed to the log to be registered and
				// printed to the stream.
				log.flush();
			}
		}
	}
	
	/**
	 * Quality control the time value for the specified record.  This only 
	 * checks the time to see if it is a reasonable and generates a warning.  It
	 * does not change any of the record's values.
	 * @param record The record to be checked.
	 * @param log The stream where errors are to be written.
	 * @param recordNumber The index of the record in the sounding.
	 */
	public void qcTimeValue(ESCSoundingRecord record, PrintWriter log,
			int recordNumber) {
		
		// Only try to QC a real value.
		if (record.getTime() != null) {
			
			if (record.getTime().compareTo(-600.0) < 0) {
				log.printf("Time at press: %6.1f is: %6.1f sec.\n", 
						record.getPressure(), record.getTime());
				// Force the output printed to the log to be registered and
				// printed to the stream.
				log.flush();
			}
		}
	}
	
	/**
	 * Quality control the U-Component value for the specified record.
	 * @param record The record to be qc'ed.
	 * @param log The stream where errors are to be written.
	 * @param recordNumber The index of the record in the sounding.
	 */
	public void qcUComponentValue(ESCSoundingRecord record, PrintWriter log, 
			int recordNumber) {
		
		// Only try to QC a real value.
		if (record.getUComponent() != null) {
			// Look for a QUESTIONABLE or BAD U component value.  This needs to
			// be an absolute value comparison to handle negative values as well
			if ((new Double(Math.abs(record.getUComponent()))).compareTo( 
					limits.getMaximumQuestionableWindSpeed()) >= 0) {
				log.printf("Ucmp at press: %6.1f is: %6.1f m/s.\n", 
						record.getPressure(), record.getUComponent());
				// Check to see if the value is BAD
				if ((new Double(Math.abs(record.getUComponent()))).compareTo(
						limits.getMaximumBadWindSpeed()) >= 0) {
					updateUComponentFlag(record, BAD_FLAG);
				}			
				// or just QUESTIONABLE
				else {
					updateUComponentFlag(record, QUESTIONABLE_FLAG);
				}
	
				// Force the output printed to the log to be registered and
				// printed to the stream.
				log.flush();
			}
		}
		// This should be taken care of in an earlier function, but this is
		// a safety valve in case something changes in the program.
		else {
			nullifyWindValues(record);
		}
	}
	
	/**
	 * Quality control the U and V wind component fields.  This only makes sure
	 * that both U and V components exist and with set them both to missing if
	 * either of the components are missing.
	 * @param record The record to be QC'ed.
	 * @param log The stream where errors are to be written.
	 */
	public void qcUVFields(ESCSoundingRecord record, PrintWriter log) {
		if ((record.getUComponent() == null || record.getVComponent() == null)
				&& (record.getUComponent() != null ||
						record.getVComponent() != null)) {
			log.printf("Ucmp at pressure: %6.1f mb is: %7.1f m/s.\n",
					record.getPressure(), record.getUComponent());
			log.printf("Vcmp at pressure: %6.1f mb is: %7.1f m/s.\n",
					record.getPressure(), record.getVComponent());
			
			try {
				record.setUComponentFlag(null);
				record.setVComponentFlag(null);			
				record.setUComponent(null, METERS_PER_SECOND);
				record.setVComponent(null, METERS_PER_SECOND);
			}
			catch (DefaultException e) {
				e.printStackTrace();
				System.exit(1);
			}
			catch (DefaultWarning e) {
				e.printStackTrace();
				System.exit(1);
			}

			// Force the output printed to the log to be registered and
			// printed to the stream.
			log.flush();		
		}
	}
	
	/**
	 * Quality control the V-Component value for the specified record.
	 * @param record The record to be qc'ed.
	 * @param log The stream where errors are to be written.
	 * @param recordNumber The index of the record in the sounding.
	 */
	public void qcVComponentValue(ESCSoundingRecord record, PrintWriter log, 
			int recordNumber) {
		
		// Only attempt to QC a valid value.
		if (record.getVComponent() != null) {
			
			// Look for a QUESTIONABLE or BAD V component value.  This needs to
			// be an absolute value comparison to handle negative values as well
			if ((new Double(Math.abs(record.getVComponent()))).compareTo(
					limits.getMaximumQuestionableWindSpeed()) >= 0) {
				log.printf("Vcmp at press: %6.1f is: %6.1f m/s.\n", 
						record.getPressure(), record.getVComponent());
				// Check to see if the value is BAD
				if ((new Double(Math.abs(record.getVComponent()))).compareTo(
						limits.getMaximumBadWindSpeed()) >= 0) {
					updateVComponentFlag(record, BAD_FLAG);
				}			
				// or just QUESTIONABLE
				else {
					updateVComponentFlag(record, QUESTIONABLE_FLAG);
				}
	
				// Force the output printed to the log to be registered and
				// printed to the stream.
				log.flush();
			}
		}
		
		// This should be taken care of in an earlier function, but this is
		// a safety valve in case something changes in the program.
		else {
			nullifyWindValues(record);
		}
	}
	
	/**
	 * Quality control the values of the sounding.  This will loop through each
	 * record in the sounding, checking the individual values for validity.
	 * @param sounding The sounding that is being qc'ed.
	 * @param log The stream where errors are to be printed.
	 */
	public void qcValues(ESCSounding sounding, PrintWriter log) {
		for (int recordNumber = 1; 
				recordNumber <= sounding.getRecords().size(); recordNumber++) {
			
			ESCSoundingRecord record = 
				sounding.getRecords().get(recordNumber - 1);
			
			// QC the dew point field
			qcDewPointField(record, log);
			// QC the U and V wind components
			qcUVFields(record, log);
			// QC the wind speed and direction
			qcWindFields(record, log);

			// QC the pressure value.
			qcPressureValue(record, log, recordNumber);
			// QC the altitude value.
			qcAltitudeValue(record, log, recordNumber);
			// QC the ascent rate value.
			qcAscentRateValue(record, log, recordNumber);
			// QC the wind speed value.
			qcWindSpeedValue(record, log, recordNumber);
			// QC the wind direction value.
			qcWindDirectionValue(record, log, recordNumber);
			// QC the U wind component value.
			qcUComponentValue(record, log, recordNumber);
			// QC the V wind component value.
			qcVComponentValue(record, log, recordNumber);
			// QC the temperature value.
			qcTemperatureValue(record, log, recordNumber);
			// QC the relative humidity value.
			qcRelativeHumidityValue(record, log, recordNumber);
			// QC the dew point value.  This must be done after the temperature
			// and relative humidity qc methods because the dew point may be
			// adjusted by either/both of those methods.
			qcDewPointValue(record, log, recordNumber);
			// QC the time value.
			qcTimeValue(record, log, recordNumber);
		}
	}
	
	/**
	 * Quality control the wind direction value for the specified record.  Any
	 * value that does not fall within the 0-360 degree range will be replaced
	 * with missing along with all of the other wind parameters for the record.
	 * @param record The record to be qc'ed.
	 * @param log The stream where error messages are to be printed.
	 * @param recordNumber The index of the record number in the sounding.
	 */
	public void qcWindDirectionValue(ESCSoundingRecord record, PrintWriter log,
			int recordNumber) {
		
		// Only try to QC a real wind direction value.
		if (record.getWindDirection() != null) {
			
			// Look for wind direction values out of the 0-360 range and set 
			// all of the wind values to missing.
			if (record.getWindDirection().compareTo(0.0) < 0 || 
					record.getWindDirection().compareTo(360.0) > 0) {
				log.printf("Wind dir at press: %6.1f is : %6.1f deg.\n",
						record.getPressure(), record.getWindDirection());
	
				nullifyWindValues(record);
	
				// Force the output printed to the log to be registered and
				// printed to the stream.
				log.flush();
			}
		}
		
		// This should be taken care of in an earlier function, but this is
		// a safety valve in case something changes in the program.
		else {
			nullifyWindValues(record);
		}
	}
	
	/**
	 * Quality control the wind speed and wind direction fields of the specified
	 * record.  This makes sure that both a wind speed and a direction exist or
	 * it will set all of the wind parameters to missing/<code>null</code>. 
	 * @param record The record to be qc'ed.
	 * @param log The stream where error logs are to be printed.
	 */
	public void qcWindFields(ESCSoundingRecord record, PrintWriter log) {
		// Look for different combination of null wind speeds and directions.
		if (((record.getWindSpeed() == null && 
				record.getWindDirection() != null) ||
			 (record.getWindSpeed() != null && 
					 record.getWindDirection() == null)) ||
			  record.getUComponent() == null || record.getVComponent() == null) 
		{

			log.printf("Windspd at pressure: %6.1f mb is: %6.1f m/s.\n",
					record.getPressure(), 999.0);
			
			// Set all of the wind values to null/missing.
			try {
				record.setUComponentFlag(null);
				record.setVComponentFlag(null);
				record.setWindDirection(null);
				record.setWindSpeed(null, METERS_PER_SECOND);
				record.setUComponent(null, METERS_PER_SECOND);
				record.setVComponent(null, METERS_PER_SECOND);
			}
			catch (DefaultException e) {
				e.printStackTrace();
				System.exit(1);
			}
			catch (DefaultWarning e) {
				e.printStackTrace();
				System.exit(1);
			}
			
			// Force the output printed to the log to be registered and
			// printed to the stream.
			log.flush();
		}
	}
	
	/**
	 * Quality control the wind speed value for the specified record.
	 * @param record The record to have the wind speed qc'ed.
	 * @param log The stream that errors are to be written to.
	 * @param recordNumber The index of the record in the sounding.
	 */
	public void qcWindSpeedValue(ESCSoundingRecord record, PrintWriter log,
			int recordNumber) {
		
		// Only QC the wind speed if it is a real value.
		if (record.getWindSpeed() != null) {
			// (1) Look for wind speeds that are so bad that they are to be 
			// replaced with missing values.
			if (record.getWindSpeed().compareTo(
					limits.getMaximumMissingWindSpeed()) >= 0 ||
					record.getWindSpeed().compareTo(0.0) < 0) {
				log.printf("Windspd at press: %6.1f is: %6.1f m/s.\n", 
						record.getPressure(), record.getWindSpeed());			
				nullifyWindValues(record);

				// Force the output printed to the log to be registered and
				// printed to the stream.
				log.flush();
			}
		
			// (2) Look for a BAD or QUESTIONABLE wind speed value and flag
			// accordingly.
			else if (record.getWindSpeed().compareTo(
					limits.getMaximumQuestionableWindSpeed()) > 0 ||
					record.getWindSpeed().compareTo(
							limits.getMinimumQuestionableWindSpeed()) < 0) {
				log.printf("Windspd at press: %6.1f is: %6.1f m/s.\n", 
						record.getPressure(), record.getWindSpeed());

				// Look for only a QUESTIONABLE wind speed.
				if (record.getWindSpeed().compareTo(
						limits.getMinimumQuestionableWindSpeed()) > 0 &&
						record.getWindSpeed().compareTo(
								limits.getMaximumBadWindSpeed()) < 0) {
					updateUComponentFlag(record, QUESTIONABLE_FLAG);
					updateVComponentFlag(record, QUESTIONABLE_FLAG);
				}
				// Otherwise it is a BAD wind speed.
				else {
					updateUComponentFlag(record, BAD_FLAG);
					updateVComponentFlag(record, BAD_FLAG);
				}

				// Force the output printed to the log to be registered and
				// printed to the stream.
				log.flush();
			}
		}
		
		// This should be taken care of in an earlier function, but this is
		// a safety valve in case something changes in the program.
		else {
			nullifyWindValues(record);
		}
	}
	
	/**
	 * Test for a rapid pressure change between the two end points of the
	 * specified list of records.  If a rapid pressure change occurs, flag
	 * the pressure, temperature, and relative humidity according to the
	 * change in pressure.
	 * @param records The list of records to be flagged on a rapid pressure
	 * change.
	 * @param log The stream where the errors are to be written.
	 */
	public void rapidPressureCheck(List<ESCSoundingRecord> records,
			PrintWriter log) {
		// Define the end points to use in checking the pressure change.
		ESCSoundingRecord current = records.get(0);
		ESCSoundingRecord last = records.get(records.size() - 1);
		
		// Calculate the change in pressure.
		double pressureChange = Math.abs(
				(current.getPressure() - last.getPressure()) /
				(current.getTime() - last.getTime()));
		
		// Handle the case were times are equal.
		if (Double.isInfinite(pressureChange)) { return; }
		
		// Search for a change of pressure that is at least questionable.
		if (limits.getQuestionableRapidPressureIncrease().compareTo(
				pressureChange) < 0) {
			
			// Determine if the pressure change is bad or just questionable.
			ESCFlag flag = 
				limits.getBadRapidPressureIncrease().compareTo(
						pressureChange) <= 0 ? BAD_FLAG : QUESTIONABLE_FLAG;
				
			// Flag all of the records between the end points with the
			// specified flag.
			for (ESCSoundingRecord record: records) {
				updatePressureFlag(record, flag);
				updateTemperatureFlag(record, flag);
				updateRelativeHumidityFlag(record, flag);
			}
			
			log.printf("Rapid pressure change between pressure: %6.1f " +
					"and pressure %6.1f is %7.1f mb/s\n",
					current.getPressure(), last.getPressure(), pressureChange);
			
			// Force the output printed to the log to be registered and
			// printed to the stream.
			log.flush();
		}
	}
	
	/**
	 * Test for a rapid temperature increase between the two end points of
	 * the specified records.  If a rapid temperature increase is detected,
	 * flag the pressure, temperature, and relative humidity for all of the
	 * records between the two end points (inclusive).
	 * @param records The records to use for checking for a rapid temperature
	 * increase.
	 * @param log The stream where errors are written to.
	 */
	public void rapidTemperatureCheck(List<ESCSoundingRecord> records,
			PrintWriter log) {

		// Make sure the end points allow for a rapid temperature check.
		if (records.get(0).getTemperature() != null &&
				records.get(records.size() - 1).getTemperature() != null &&
				records.get(records.size() - 1).getTemperature() -
					records.get(0).getTemperature() > .15) {
			
			// Determine if the latitude is for a hot or cold latitude check.
			if (limits.isHotLatitude()) {
				rapidTemperatureCheckForHotLatitudes(records, log);
			} else {
				rapidTemperatureCheckForColdLatitudes(records, log);
			}
		}
	}
	
	/**
	 * Test for a rapid temperature increase between the two end points of
	 * the specified records.  If a rapid temperature increase is detected,
	 * flag the pressure, temperature, and relative humidity for all of the
	 * records between the two end points (inclusive).
	 * @param records The records being checked for a rapid temperature
	 * increase.
	 * @param rapidTempIncrease The threshold such that lapse rates greater
	 * than or equal to the threshold are considered to be QUESTIONABLE.
	 * @param badRapidTempIncrease The threshold such that lapse rates greater
	 * than or equal to the threshold are considered to be BAD.
	 * @param log The stream where errors are to be written.
	 */
	public void rapidTemperatureCheck(List<ESCSoundingRecord> records, 
			Double rapidTempIncrease, Double badRapidTempIncrease,
			PrintWriter log) {
		
		// Define the end points to use for the calculation.
		ESCSoundingRecord start = records.get(0);
		ESCSoundingRecord end = records.get(records.size() - 1);
		
		// Calculate the lapse rate.
		double lapseRate = 
			(end.getTemperature() - start.getTemperature()) /
			(end.getAltitude() - start.getAltitude());

		// This will handle the case where the start and end altitudes are
		// equal (results in lapse rate being +/- Infinity.
		if (Double.isInfinite(lapseRate)) { return ; }
		
		// Check to see if the lapse rate is at least QUESTIONABLE
		if (lapseRate >= rapidTempIncrease * limits.getAdiabaticLapseRate() &&
				rapidTempIncrease.compareTo(lapseRate * 100.0) <= 0) {
			
			log.printf("Rapid temperature increase between pressure %6.1f " +
					"and %6.1f is %7.2f deg C/km.\n",
					start.getPressure(), end.getPressure(), lapseRate * 1000.0);
			
			// Check to see if the lapse rate is actually BAD and not
			// just QUESTIONABLE.
			ESCFlag flag = badRapidTempIncrease.compareTo(
					lapseRate * 1000.0) <= 0 ? BAD_FLAG : QUESTIONABLE_FLAG;

			// Update the flags of all of the records between the end points
			// with the appropriate flag.
			for (ESCSoundingRecord record: records) {
				updatePressureFlag(record, flag);
				updateTemperatureFlag(record, flag);
				updateRelativeHumidityFlag(record, flag);
			}

			// Force the output printed to the log to be registered and
			// printed to the stream.
			log.flush();
		}
	}
	
	/**
	 * Test for a rapid temperature increase between the two end points of
	 * the specified records in a cold latitude.  If a rapid temperature 
	 * increase is detected, flag the pressure, temperature, and relative 
	 * humidity for all of the records between the two end points (inclusive).
	 * @param records The records to be checked for the rapid temperature
	 * increase.
	 * @param log The stream where error messages are to be written.
	 */
	public void rapidTemperatureCheckForColdLatitudes(
			List<ESCSoundingRecord> records, PrintWriter log) {
		
		// Define the end points to use in the check.
		ESCSoundingRecord start = records.get(0);
		ESCSoundingRecord end = records.get(records.size() - 1);
		
		// Check for temperatures over the PRESSURE_LIMIT, but under the
		// SURFACE_PRESSURE_LIMIT.
		if (start.getPressure().compareTo(limits.getPressureLimit()) >= 0 &&
			end.getPressure().compareTo(limits.getPressureLimit()) >= 0 &&
			(start.getPressure().compareTo(limits.getSurfacePressureLimit()) < 0
			||
			end.getPressure().compareTo(limits.getSurfacePressureLimit()) < 0)) 
		{
			rapidTemperatureCheck(records, limits.getRapidTemperatureIncrease(),
					limits.getBadRapidTemperatureIncrease(), log);
		}
		
		// Check for temperatures under the PRESSURE_LIMIT.
		else if ((start.getPressure().compareTo(limits.getPressureLimit()) <0 ||
				end.getPressure().compareTo(limits.getPressureLimit()) < 0) &&
				start.getPressure().compareTo(end.getPressure()) != 0) {
			rapidTemperatureCheck(records, 
					limits.getStratRapidTemperatureIncrease(),
					limits.getBadStratRapidTemperatureIncrease(), log);
		}
		
		// Check for temperatures over the SURFACE_PRESSURE_LIMIT.
		else if (start.getPressure().compareTo( 
					limits.getSurfacePressureLimit()) >= 0 &&
				end.getPressure().compareTo( 
						limits.getSurfacePressureLimit()) >= 0) {
			rapidTemperatureCheck(records,
					limits.getSurfaceRapidTemperatureIncrease(),
					limits.getBadSurfaceRapidTemperatureIncrease(), log);
		}
	}
	
	/**
	 * Test for a rapid temperature increase between the two end points of
	 * the specified records in a hot latitude.  If a rapid temperature 
	 * increase is detected, flag the pressure, temperature, and relative 
	 * humidity for all of the records between the two end points (inclusive).
	 * @param records The records to be checked for the rapid temperature
	 * increase.
	 * @param log The stream where error messages are to be written.
	 */
	public void rapidTemperatureCheckForHotLatitudes(
			List<ESCSoundingRecord> records, PrintWriter log) {
		
		// Define the end points to use in the check.
		ESCSoundingRecord start = records.get(0);
		ESCSoundingRecord end = records.get(records.size() - 1);
		
		// Check for temperature values over the PRESSURE_LIMIT
		if (start.getPressure().compareTo(limits.getPressureLimit()) >= 0 &&
			end.getPressure().compareTo(limits.getPressureLimit()) >= 0) {
			rapidTemperatureCheck(records, limits.getRapidTemperatureIncrease(),
					limits.getBadRapidTemperatureIncrease(), log);
		}
		
		// Check for temperatures under the PRESSURE_LIMIT.
		else if ((start.getPressure().compareTo(limits.getPressureLimit()) < 0 
				|| end.getPressure().compareTo(limits.getPressureLimit()) < 0) 
				&& start.getPressure().compareTo(end.getPressure()) != 0) {
			rapidTemperatureCheck(records, 
					limits.getStratRapidTemperatureIncrease(),
					limits.getBadStratRapidTemperatureIncrease(), log);
		}
	}
	
	/**
	 * Execute the quality control process.  This will find all files in the
	 * input directory (recursively), perform the quality control on each
	 * individual sounding, and generate a log file with all warnings for
	 * each input file.
	 */
	public void run() {
		ESCSoundingParser parser = new ESCSoundingParser(true);
		// Make sure the output directory exists before trying to 
		// generate the output files.
		if (!OUTPUT_DIR.exists()) { OUTPUT_DIR.mkdir(); }
		if (!LOG_DIR.exists()) { LOG_DIR.mkdir(); }

		// Find the soundings that are to be extracted and loop through them.
		List<File> files = findInputFiles();
		for (File file: files) {
			System.out.println("Processing file: "+file);
			
			try {
				// Setup the log file
				File logFile = new File(LOG_DIR, String.format("%s.err", 
						file.getName()));
				// Create the directories where the log file will be stored.
				logFile.getParentFile().mkdirs();			
				// Open the log file.
				PrintWriter log = new PrintWriter(new FileWriter(logFile));

				// Setup the output file.
				File outFile = new File(OUTPUT_DIR, file.getName());
				// Create the directories where the log file will be stored.
				outFile.getParentFile().mkdirs();

				// Open the output file.
				PrintWriter out = null;
				// Open a gzip file.
				if (outFile.getName().toLowerCase().endsWith(".gz")) {
					out = new PrintWriter(new OutputStreamWriter(
							new GZIPOutputStream(
									new FileOutputStream(outFile))));
				}
				// Open a standard text file.
				else {
					out = new PrintWriter(new FileWriter(outFile));
				}


				// Parse the file into a list of soundings.
				List<ESCSounding> soundings = parser.parseFile(file);
				// Let the user know that the file does not contain a 
				// sounding.
				// This really shouldn't happen, but just in case...
				if (soundings.isEmpty()) {
					throw new QCRuntimeException(String.format(
							"%s does not have any soundings.", 
							file.getName()));
				}

				// Parse the soundings in the file.
				else {
					for (ESCSounding sounding: soundings) {
						qc(sounding, log);
						out.println(sounding.toString());
					}

					log.close();
					out.close();
				}
			} catch (ConversionException e) {
				throw new QCRuntimeException(String.format(
						"Unable to QC %s.  ConversionException: %s", 
						file.getName(),	e.getMessage()));
			} catch (DateTimeException e) {
				throw new QCRuntimeException(String.format(
						"Unable to QC %s.  DateTimeException: %s", 
						file.getName(),	e.getMessage()));
			} catch (InvalidValueException e) {
				throw new QCRuntimeException(String.format(
						"Unable to QC %s.  InvalidValueException: %s", 
						file.getName(), e.getMessage()));
			} catch (InvalidValueWarning e) {
				throw new QCRuntimeException(String.format(
						"Unable to QC %s.  InvalidValueWarning: %s",
						file.getName(),	e.getMessage()));
			} catch (IOException e) {
				throw new QCRuntimeException(String.format(
						"Unable to QC %s.  IOException: %s", file.getName(),
						e.getMessage()));
			}
		}		
	}
	
	/**
	 * Test for a super adiabatic lapse rate between the end points of the
	 * specified records.  If a super adiabatic lapse rate is detected, flag
	 * all of the records between the two end points with the appropriate flag
	 * based on the lapse rate value.
	 * @param records The record range whose end points are to be checked for
	 * a super adiabatic laspe rate.
	 * @param questionableLapseRate The threshold that defines all lapse rate
	 * values greater than or equal to the threshold to be QUESTIONABLE.
	 * @param badLapseRate The threshold that defines all lapse rate values
	 * greater than or equal to the threshold to be BAD.
	 * @param log The stream where error messages are to be printed.
	 */
	public void superAdiabaticLapseRateCheck(List<ESCSoundingRecord> records,
			Double questionableLapseRate, Double badLapseRate, PrintWriter log)
	{
		// Define the end points for the check.
		ESCSoundingRecord start = records.get(0);
		ESCSoundingRecord end = records.get(records.size() - 1);

		// Only want to check lapse rates where the change in temperature
		// exceeds the lapse rate difference threshold
		if (start.getTemperature() != null && end.getTemperature() != null &&
				limits.getLapseRateDifference().compareTo(
						start.getTemperature() - end.getTemperature()) < 0) { 
			
			// Equal altitudes cause a divide by zero error in the lapse
			// rate calculation.  This is bad and is flagged that way.
			if (start.getAltitude().compareTo(end.getAltitude()) == 0) {
				log.printf("Super adiabatic lapse rate between pressure %6.1f" +
						" and %6.1f with no altitude change.\n",
						start.getPressure(), end.getPressure());
				for (ESCSoundingRecord record: records) {
					updatePressureFlag(record, BAD_FLAG);
					updateTemperatureFlag(record, BAD_FLAG);
					updateRelativeHumidityFlag(record, BAD_FLAG);
				}
			}
			
			// Altitudes aren't equal.
			else {
				// Calculate the lapse rate.
				Double lapseRate = 
					(start.getTemperature() - end.getTemperature()) /
					(start.getAltitude() - end.getAltitude());
				
				// Check to see if the lapse rate is at least a QUESTIONABLE
				// lapse rate.
				if (lapseRate.compareTo(-1 * limits.getLapseRateLimit() * 
						limits.getAdiabaticLapseRate()) <= 0 &&
					(new Double(lapseRate * 1000.0)).compareTo(
							questionableLapseRate) <= 0) {
					
					log.printf("Super adiabatic lapse rate between pressure" +
							" %6.1f and %6.1f is %8.2f deg C/km.\n", 
							start.getPressure(), end.getPressure(), 
							lapseRate * 1000.0);

					// The lapse rate is at least QUESTIONABLE, but it could
					// be BAD.
					ESCFlag flag = badLapseRate.compareTo(
							lapseRate * 1000.0) >= 0 ? BAD_FLAG :
								QUESTIONABLE_FLAG;
					// Update the flags for all of the records between
					// the two end points with the QUESTIONABLE (or worse)
					// lapse rate
					for (ESCSoundingRecord record : records) {
						updatePressureFlag(record, flag);
						updateTemperatureFlag(record, flag);
						updateRelativeHumidityFlag(record, flag);
					}
				}
			}
			
			// Force the output printed to the log to be registered and
			// printed to the stream.
			log.flush();
		}
	}
			
	/**
	 * Check to see if the times are increasing/decreasing as expected for the
	 * sounding between the two records.
	 * @param current The current record being checked.
	 * @param next The record following the current record to use in the check.
	 * @param previousTime The last time used in the time check for the
	 * sounding.
	 * @param log The stream where errors are to be written.
	 * @return The latest time found in the sounding.
	 */
	public abstract Double timeCheck(ESCSoundingRecord current, 
			ESCSoundingRecord next, Double previousTime, PrintWriter log);
	
	/**
	 * Update the specified record's pressure flag with the specified flag.  
	 * This will only change the flag if the specified flag is worse than than 
	 * the current flag or if the current flag is set to UNCHECKED.
	 * @param record The record to have the flag changed.
	 * @param flag The flag to be replaced in the record.
	 */
	public void updatePressureFlag(ESCSoundingRecord record, ESCFlag flag) {
		if (record.getPressureFlag().equals(UNCHECKED_FLAG) || 
				flag.isWorseFlag(record.getPressureFlag())) {
			try { record.setPressureFlag(flag); }
			catch (InvalidFlagException e) {}
		}
	}
	
	/**
	 * Update the specified record's relative humidity flag with the specified 
	 * flag.  This will only change the flag if the specified flag is worse than 
	 * than the current flag or if the current flag is set to UNCHECKED.
	 * @param record The record to have the flag changed.
	 * @param flag The flag to be replaced in the record.
	 */
	public void updateRelativeHumidityFlag(ESCSoundingRecord record,
			ESCFlag flag) {
		if (record.getRelativeHumidityFlag().equals(UNCHECKED_FLAG) || 
				flag.isWorseFlag(record.getRelativeHumidityFlag())) {
			try { record.setRelativeHumidityFlag(flag); }
			catch (InvalidFlagException e) {}
		}
	}
	
	/**
	 * Update the specified record's temperature flag with the specified flag.  
	 * This will only change the flag if the specified flag is worse than than 
	 * the current flag or if the current flag is set to UNCHECKED.
	 * @param record The record to have the flag changed.
	 * @param flag The flag to be replaced in the record.
	 */
	public void updateTemperatureFlag(ESCSoundingRecord record, ESCFlag flag) {
		if (record.getTemperatureFlag().equals(UNCHECKED_FLAG) || 
				flag.isWorseFlag(record.getTemperatureFlag())) {
			try { record.setTemperatureFlag(flag); }
			catch (InvalidFlagException e) {}
		}
	}
	
	/**
	 * Update the specified record's U Component with the specified flag.  This
	 * will only change the flag if the specified flag is worse than than the
	 * current flag or if the current flag is set to UNCHECKED.
	 * @param record The record to have the flag changed.
	 * @param flag The flag to be replaced in the record.
	 */
	public void updateUComponentFlag(ESCSoundingRecord record, ESCFlag flag) {
		if (record.getUComponentFlag().equals(UNCHECKED_FLAG) ||
				flag.isWorseFlag(record.getUComponentFlag())) {
			try { record.setUComponentFlag(flag); }
			catch (InvalidFlagException e) {}
		}
	}
	
	/**
	 * Update the specified record's V Component with the specified flag.  This
	 * will only change the flag if the specified flag is worse than than the
	 * current flag or if the current flag is set to UNCHECKED.
	 * @param record The record to have the flag changed.
	 * @param flag The flag to be replaced in the record.
	 */
	public void updateVComponentFlag(ESCSoundingRecord record, ESCFlag flag) {
		if (record.getVComponentFlag().equals(UNCHECKED_FLAG) || 
				flag.isWorseFlag(record.getVComponentFlag())) {
			try { record.setVComponentFlag(flag); }
			catch (InvalidFlagException e) {}
		}
	}
}
