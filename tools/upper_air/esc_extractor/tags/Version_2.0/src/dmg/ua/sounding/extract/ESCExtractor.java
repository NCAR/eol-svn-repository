package dmg.ua.sounding.extract;

import static dmg.ua.sounding.esc.ESCSoundingRecord.*;
import static dmg.util.LengthUtils.*;
import static dmg.util.PressureUtils.*;
import static dmg.util.TemperatureUtils.*;
import static dmg.util.VelocityUtils.*;

import java.io.*;
import java.util.*;
import java.util.regex.*;
import java.util.zip.*;

import dmg.ua.sounding.esc.*;
import dmg.util.*;

/**
 * The ESCExtractor class is the generic class for extracting a pressure level frequency
 * from a sounding.  It is designed to be extended by individual frequencies.
 *
 * @author Joel Clawson
 */
public abstract class ESCExtractor {

    private boolean GZIP, DEBUG;
    private File INPUT_DIRECTORY, OUTPUT_DIRECTORY, LOG_FILE;
    private String PATTERN;
    
    private int soundingIndex;
    
    /**
     * Create a new instance of an ESCExtractor.
     */
    public ESCExtractor() {
	GZIP = true;
	DEBUG = false;
    }
    
    /**
     * Create the interpolated record for the specified pressure.
     * @param interpolationPressure The pressure the record is being interpolated for.
     * @param sounding The sounding being interpolated.
     * @param log The output stream where errors/warning are to be written.
     * @return The interpolated record for the specified pressure or <code>null</code> if the
     * record could not be interpolated.
     * @throws ExtractionException when there is a major problem that prevents the record from
     * being interpolated.
     */
    private ESCSoundingRecord buildInterpolatedRecord(int interpolationPressure, ESCSoundingRecord previousInterpolatedRecord, ESCSounding sounding, PrintWriter log) throws ExtractionException {		
	// Determine the set of records that can be used in the interpolation.
	List<ESCSoundingRecord> secondRangeRecords = findRecordsWithinSecondRange(interpolationPressure, previousInterpolatedRecord, sounding, log);
	
	Double pressure = new Double(interpolationPressure);
	
	// Create the interpolated record.
	ESCSoundingRecord interpolated = new ESCSoundingRecord();
	// Interpolate the pressure related values.
	interpolatePressureValues(interpolated, pressure, secondRangeRecords, log);
	
	// If the pressure could not be set, the record is useless, so return null and don't try
	// to interpolate any other values.
	if (interpolated.getPressure() == null) { return null; }
	
	// Interpolate the rest of the record's values.
	interpolateTemperatureValues(interpolated, pressure, secondRangeRecords, log);
	interpolateRelativeHumidityValues(interpolated, pressure, secondRangeRecords, log);
	interpolateWindValues(interpolated, pressure, secondRangeRecords, log);
	
	return interpolated;
    }
    
    /**
     * Calculate the ascent rate between the two provided records.
     * @param lower The lower pressure record to use in the calculation.
     * @param upper The upper pressure record to use in the calculation.
     * @return The ascent rate between the records or <code>null</code> if the ascent rate
     * could not be calculated.
     */
    private Double calculateAscentRate(ESCSoundingRecord lower, ESCSoundingRecord upper) {
	if (lower == null || upper == null || lower.getTime() == null || lower.getAltitude() == null || upper.getTime() == null || upper.getAltitude() == null) {
	    return null;
	}
	else if (lower.equals(upper)) { return lower.getAscentRate(); }
	else {
	    return ((upper.getAltitude() - lower.getAltitude()) / (upper.getTime() - lower.getTime()));
	}
    }
    
    /**
     * Determine the first pressure value that should be interpolated for the sounding.  This
     * makes sure that the first interpolation pressure is not the same as the first pressure
     * record since this will already be output as the surface data line.
     * @param sounding The sounding to be interpolated.
     * @return The initial interpolation pressure for the extracted sounding or <code>-1</code>
     * if the initial pressure cannot be determined.
     */
    private int calculateInitialInterpolationPressure(ESCSounding sounding) {
	int pressure = -1;
	for (ESCSoundingRecord record: sounding.getRecords()) {
	    if (record.getPressure() != null) {
		// Determine the initial interpolation pressure.
		pressure = ((int)(record.getPressure() / getInterpolationInterval())) * getInterpolationInterval();
		// Don't want to interpolate the pressure if it is the same
		// as the initial record in the sounding.  (It will be output
		// anyway as the first record.)
		if ((new Double(pressure)).equals(sounding.getRecords().get(0).getPressure())) {
		    pressure -= getInterpolationInterval();
		}
		return pressure;
	    }
	}
	return pressure;
    }
    
    /**
     * Create the file where the interpolated sounding(s) are to be written.
     * @param file The original file containing the high resolution sounding(s).
     * @return The file where the interpolated sounding(s) are to be written.
     */
    private File createOutputFile(File file) {
	String absolutePath = file.getAbsolutePath();
	absolutePath = absolutePath.replaceFirst(INPUT_DIRECTORY.getAbsolutePath(), OUTPUT_DIRECTORY.getAbsolutePath());
	absolutePath = absolutePath.replaceFirst(PATTERN, String.format(".%02dmb%s", getInterpolationInterval(), GZIP ? ".gz" : ""));
	return new File(absolutePath);
    }
    
    /**
     * Create the extracted/interpolated sounding from the specified sounding.
     * @param sounding The sounding to be interpolated.
     * @param log The output stream where errors are to be written.
     * @return The interpolated/extracted sounding.
     */
    public ESCSounding extract(ESCSounding sounding, PrintWriter log) {
	replaceUncheckedFlags(sounding);
	
	// Initialize the extracted sounding by copying the original sounding
	// header and the first valid record to it.
	ESCSounding extract = initializeInterpolatedSounding(sounding);
	
	// Determine the first pressure value to be interpolated.
	int interpolationPressure = calculateInitialInterpolationPressure(sounding);
	
	// Special case when the interpolation pressure could not be determined.
	if (interpolationPressure < 0) { return extract; }
	
	// Continue to loop through the set of interpolation pressure increments
	// until the interpolation pressure passes the smallest pressure that
	// is allowed to be interpolated.
	while (interpolationPressure >= getMinimumInterpolationPressure()) {
	    // Create the record for the current interpolation pressure and
	    // add it to the extracted sounding.
	    try {
		if (DEBUG) {
		    log.printf("Building Intp Record for %04d\n", interpolationPressure);
		    log.flush();
		}
		
		ESCSoundingRecord record = buildInterpolatedRecord(interpolationPressure, extract.getRecords().get(extract.getRecords().size() - 1), sounding, log);
		if (record != null) { 
		    extract.add(record);
		}
	    } catch (ExtractionException e) {
		log.printf("The record at pressure %d could not be interpolated.\n", interpolationPressure);
		log.println("\t"+e.getMessage());
		log.flush();
	    }
	    
	    // Adjust the interpolation pressure to the next pressure in the incrementation.
	    interpolationPressure -= getInterpolationInterval();
	}

	// Add all of the missing pressure records.
	Double nextInterpPressure = new Double(calculateInitialInterpolationPressure(sounding));
	// Loop through all of the known records.
	for (int i = 1; i < extract.getRecords().size(); i++) {

	    // If the next interpolation pressure is greater than the current record's pressure, there
	    // is a missing pressure record.  Add the new record to the current location in the record
	    // list.  This will push the current record to be the next record in the list (and making
	    // the new record the current one).  This will make the size of the record list grow by one
	    // so the next iteration of the for loop will make the original current record the new
	    // current record allowing for multiple missing records to be added before the original
	    // current record as necessary.  
	    if (nextInterpPressure.compareTo(extract.getRecords().get(i).getPressure()) > 0) {

		// Build the new record and add it to the extracted sounding.
		ESCSoundingRecord record = new ESCSoundingRecord();
		try {
		    // Assign the pressure to a new record.
		    record.setPressure(nextInterpPressure, MILLIBARS);
		    // Add the new missing record to the current location in the list.
		    extract.getRecords().add(i, record);
		}
		// This can be ignored because it is caused by a sea level pressure calculation.		
		catch (CalculationWarning e) {}
		// This exception should only occur if the program/library changes to not allow MILLIBARS
		// as a valid pressure unit.  This is a programming issue, not a data issue, so blow up.
		catch (ConversionException e) {
		    e.printStackTrace();
		    System.exit(1);
		}
		// This occurs if the value being assigned to the pressure.  This is a major problem
		// since the value is the basis of the extraction.  This is such a potentially big
		// problem that the program is going to blow up since it is probably a programming error.
		catch (InvalidValueWarning e) {
		    e.printStackTrace();
		    System.exit(1);
		}
	    }

	    // Finally, the next interpolated pressure needs be decremented by the interval for the next
	    // iteration since the current interpolated already existed or has just been created as a
	    // missing record.
	    nextInterpPressure -= getInterpolationInterval();
	}

	// The sounding has successfully been extracted, so it can be returned.
	return extract;
    }
    
    /**
     * Find the set of high resolution soundings that are to be interpolated.
     * @return The list of files to be interpolated.
     */
    public List<File> findInputFiles() {
	List<File> files = new ArrayList<File>();
	
	// Generate the pattern to use to find the sounding files.
	Pattern pattern = Pattern.compile(PATTERN);
	
	// Create a list of files to check for sounding files.
	List<File> cycle = new ArrayList<File>();
	cycle.add(INPUT_DIRECTORY);
	
	while (!cycle.isEmpty()) {
	    // Remove the file so the list will eventually be empty.
	    File current = cycle.remove(0);
	    
	    if (current.isDirectory()) {
		// Add all of the directory's files to the cycle list.
		cycle.addAll(Arrays.asList(current.listFiles()));
	    } else if (pattern.matcher(current.getName()).find()) {
		// The file name matched the pattern, so add it to the sounding list.
		files.add(current);
	    }
	}
	
	return files;
    }
    
    /**
     * Determine the set of records (in decreasing pressure order) that are allowed to be included
     * as potential end points for the interpolation of record values.
     * @param interpolatedPressure The pressure value that is being interpolated.
     * @param sounding The sounding that contains the high resolution data records.
     * @return The list of high resolution data records that can be used in the interpolation of values.
     */
    private List<ESCSoundingRecord> findRecordsWithinSecondRange(int interpolatedPressure, ESCSoundingRecord previousInterpolatedRecord, ESCSounding sounding, PrintWriter log) {
	// Define the record lists used.
	List<ESCSoundingRecord> allRecords = sounding.getRecords();		
	List<ESCSoundingRecord> secondRangeRecords = new ArrayList<ESCSoundingRecord>();
	
	// Initialize variables used in the searching.
	int index = soundingIndex;
	Double time = null;
	Double pressure = new Double(interpolatedPressure);
	
	//System.out.printf("Attempting pressure: %.1f\n", pressure);
	
	// Determine the time of the record at the interpolation pressure.
	while (index < allRecords.size() && time == null) {
	    if (index + 1 < allRecords.size()) {
		Double ascentRate = calculateAscentRate(previousInterpolatedRecord, allRecords.get(index));
		//System.out.printf("\tAscent Rate at Pressure %.1f: %s (%d)\n", pressure, ascentRate, index);
		//System.out.printf("\t\t%s\n\t\t%s\n\n", previousInterpolatedRecord, allRecords.get(index));
		if ((ascentRate == null && previousInterpolatedRecord.equals(allRecords.get(index))) || 
		    (ascentRate != null && (ascentRate.compareTo(20.0) > 0 || ascentRate.compareTo(-40.0) < 0))) {
		    
		}
		
		else if (allRecords.get(index).getPressure() != null && (allRecords.get(index).getPressure().compareTo(pressure) == 0 ||
									 (allRecords.get(index+1).getPressure() != null && allRecords.get(index).getPressure().compareTo(pressure) >= 0 &&
									  allRecords.get(index+1).getPressure().compareTo(pressure) < 0))) {
		    if (allRecords.get(index).getPressure().compareTo(pressure) == 0) {
			time = allRecords.get(index).getTime();
		    }
		    else {
			time = interpolateTime(pressure, allRecords.get(index), allRecords.get(index + 1));
		    }
		    soundingIndex = index;
		    
		    if (time == null) {
			int lowerIndex = index;
			int upperIndex = allRecords.get(index).getPressure().compareTo(pressure) == 0 ? index : index + 1;
			
			while (lowerIndex >= 0 && (allRecords.get(lowerIndex).getPressure() == null || allRecords.get(lowerIndex).getTime() == null)) { lowerIndex--; }
			while (upperIndex < allRecords.size() && (allRecords.get(upperIndex).getPressure() == null || allRecords.get(upperIndex).getTime() == null)) { upperIndex++; }
			
			if (lowerIndex >= 0 && upperIndex < allRecords.size()) {
			    time = interpolateTime(pressure, allRecords.get(lowerIndex), allRecords.get(upperIndex));
			    soundingIndex = lowerIndex;
			}
		    }				
		}
	    } else {
		if (allRecords.get(index).getPressure() != null && allRecords.get(index).getPressure().compareTo(pressure) == 0) {
		    time = allRecords.get(index).getTime();
		    soundingIndex = index;
		}
	    }
	    index++;
	}
	
	if (DEBUG) {
	    log.printf("Time of Interpolated Pressure %04d = %.1f\n", interpolatedPressure, time);
	    log.flush();
	}
	
	// Back up the index to the first record within the second range of the interpolation pressure.
	// This is needed to back up the index if the pressure interval record is the last record of the sounding.
	while (index >= allRecords.size()) { index--; }
	while (time != null && index > 0 && (allRecords.get(index).getTime() == null || Math.abs(allRecords.get(index).getTime() - time) <= getSecondRange())) { index--; }
	// Need to increment this index once to get it back into line with the first allowed time.
	index++;
	if (DEBUG) {
	    if (index < allRecords.size()) {
		log.printf("Index of First Record for Pressure %04d = %d (Time = %.1f)\n", interpolatedPressure, index, allRecords.get(index).getTime());
	    } else {
		log.printf("Index of First Record for Pressure %04d = null\n", interpolatedPressure);
	    }
	    log.flush();
	}
	
	// Now continue through the records adding them to the range list until the second range
	// distance has passed.
	while (time != null && index < allRecords.size() && (allRecords.get(index).getTime() == null || Math.abs(allRecords.get(index).getTime() - time) <= getSecondRange())) {
	    if (allRecords.get(index).getPressure() != null && allRecords.get(index).getTime() != null) {
		secondRangeRecords.add(allRecords.get(index));
	    }
	    index++;
	}
	
	// This is a special case that is required if a record lands on the exact interval, but there are not
	// any other records within the allowed time range.
	while (time != null && secondRangeRecords.size() == 0 && index >= 0) {
	    if (index < allRecords.size() && allRecords.get(index).getPressure() != null && pressure.compareTo(allRecords.get(index).getPressure()) == 0) {
		secondRangeRecords.add(allRecords.get(index));
	    }
	    index--;
	}
	
	
	if (DEBUG) {
	    if (secondRangeRecords.size() > 0) {
		log.printf("Index of Final Record for Pressure %04d = %d (Time = %.1f)\n", interpolatedPressure, allRecords.indexOf(secondRangeRecords.get(secondRangeRecords.size() - 1)), allRecords.get(allRecords.indexOf(secondRangeRecords.get(secondRangeRecords.size() - 1))).getTime());
		log.flush();
	    } else {
		log.printf("Index of Final Record for Pressure %04d = null\n", interpolatedPressure);
		log.flush();
	    }
	}
	
	// Now return the list of records within second range of the interpolation pressure.
	return secondRangeRecords;
    }

    /**
     * Get the difference in pressure between interpolated pressure values.
     * @return The pressure interpolation interval in millibars. 
     */
    public abstract int getInterpolationInterval();
    
    /**
     * Get the pressure value that is that smallest allowed pressure to be interpolated for the 
     * sounding.
     * @return The minimam pressure that can be interpolated.
     */
    public abstract int getMinimumInterpolationPressure();
    
    /**
     * Get the number of seconds on either side of the interpolated pressure a record is allowed
     * to be from the interpolated pressure's time to be included as a potential end point in the
     * interpolation of record values.
     * @return The number of seconds from the interpolation pressure's time.
     */
    public abstract int getSecondRange();
    
    /**
     * Create the new interpolated sounding by copying the header from the source sounding and
     * assigning the interpolated sounding the first record with a valid pressure value.
     * @param sounding
     * @return
     */
    private ESCSounding initializeInterpolatedSounding(ESCSounding sounding) {
	ESCSounding extract = new ESCSounding();
	soundingIndex = 0;
	
	// Copy the data type and release direction line.
	extract.setDataType(sounding.getDataType());
	extract.setReleaseDirection(sounding.getReleaseDirection());
	
	// Copy the project line.
	extract.setProjectId(sounding.getProjectId());
	
	// Copy the station information.
	extract.setStationDescription(sounding.getStationDescription());
	extract.setStationId(sounding.getStationId());
	
	// Copy the release location information.
	try { 
	    extract.setLatitude(sounding.getLatitude());
	    extract.setLongitude(sounding.getLongitude());
	}
	// This should never happen and would only occur if there was an illegal
	// value in the initial sounding.
	catch (InvalidValueWarning e) {
	    e.printStackTrace();
	    System.exit(1);
	}		
	try { extract.setAltitude(sounding.getAltitude(), METERS); }
	// This should only happen if the length unit is null.  This would be a 
	// programming error or a change in the sounding itself, so it should blow up.
	catch (ConversionException e) {
	    e.printStackTrace();
	    System.exit(1);
	}
	
	// Copy the actual release time.
	extract.setActualRelease(sounding.getActualDate());		
	
	// Copy the variable header lines.
	for (int i = 5; i < 15; i++) {
	    if (sounding.getHeaderLine(i) != null) {
		try { 
		    extract.setHeaderLine(i, sounding.getHeaderLine(i).getLabel(),
					  sounding.getHeaderLine(i).getContent()); }
		// This should never happen.  If the values were illegal, they 
		// wouldn't have been allowed in the original sounding.
		catch (InvalidValueException e) {
		    e.printStackTrace();
		    System.exit(1);
		}
	    }
	}
	
	// Copy the nominal release time.
	extract.setNominalRelease(sounding.getNominalDate());		
	
	// Add the surface record as is to the interpolated record.
	int i = 0;
	while (extract.getRecords().isEmpty() && i < sounding.getRecords().size()) {
	    if (sounding.getRecords().get(i).getPressure() != null) {
		extract.add(sounding.getRecords().get(i));
	    }
	    i++;
	}
	// Remove the variable measurements because these cannot be interpolated.
	try { 
	    extract.getRecords().get(0).setVariableField1(null);
	    extract.getRecords().get(0).setVariableField1(null);
	} 
	// This should not be thrown unless the record is changed to not allow a null value.
	catch (InvalidValueException e) { e.printStackTrace(); System.exit(1); }
	
	return extract;
    }
    
    
    /**
     * Interpolate the altitude value using the specified records as end points.
     * @param pressure The pressure the altitude is being interpolated to.
     * @param lower The lower pressure end point to use in the interpolation.
     * @param upper The upper pressure end point ot use in the interpolation.
     * @return The interpolated altitude value for the specified pressure.
     */
    private Double interpolateAltitude(Double pressure, ESCSoundingRecord lower, ESCSoundingRecord upper) {
	if (lower.getAltitude() == null || upper.getAltitude() == null) { return null; }
	else if (upper.getAltitude().compareTo(lower.getAltitude()) == 0) { return lower.getAltitude(); }
	else {
	    return lower.getAltitude() + ((pressure - lower.getPressure()) / (upper.getPressure() - lower.getPressure())) * (upper.getAltitude() - lower.getAltitude());
	}
    }
    
    /**
     * Interpolate the latitude value using the specified records as end points.
     * @param pressure The pressure the latitude is being interpolated to.
     * @param lower The lower pressure end point to use in the interpolation.
     * @param upper The upper pressure end point ot use in the interpolation.
     * @return The interpolated latitude value for the specified pressure.
     */
    private Double interpolateLatitude(Double pressure, ESCSoundingRecord lower, ESCSoundingRecord upper) {
	if (upper.getLatitude().compareTo(lower.getLatitude()) == 0) { return lower.getLatitude(); }
	return lower.getLatitude() + ((pressure - lower.getPressure()) / (upper.getPressure() - lower.getPressure())) * (upper.getLatitude() - lower.getLatitude());
    }
    
    /**
     * Interpolate the longitude value using the specified records as end points.
     * @param pressure The pressure the longitude is being interpolated to.
     * @param lower The lower pressure end point to use in the interpolation.
     * @param upper The upper pressure end point ot use in the interpolation.
     * @return The interpolated longitude value for the specified pressure.
     */
    private Double interpolateLongitude(Double pressure, ESCSoundingRecord lower, ESCSoundingRecord upper) {
	if (upper.getLongitude().compareTo(lower.getLongitude()) == 0) { return lower.getLongitude(); }
	
	// Determine the actual change in longitude.  This handles the wrap at -180/180.
	double delta = Math.abs(upper.getLongitude() - lower.getLongitude());
	
	// Calculate the weighted average of the longitude.
	double avg = lower.getLongitude() + ((pressure - lower.getPressure()) / (upper.getPressure() - lower.getPressure())) * (delta);
	
	// Adjust the averaged value to be back within the -180 to 180 range.
	if (delta < 180) { return avg; }
	else if (avg < 0) { return avg + 180.0; }
	else { return avg - 180.0; }
    }
    
    /**
     * Interpolate the values related to the pressure for the specified record.
     * @param interpolated The record being interpolated.
     * @param pressure The pressure of the record being interpolated.
     * @param records The list of records that can be used in the interpolation process.
     * @param log The output stream where errors/warnings are to be written.
     */
    private void interpolatePressureValues(ESCSoundingRecord interpolated, Double pressure, List<ESCSoundingRecord> records, PrintWriter log) {
	ESCSoundingRecord lower = null, upper = null;
	for (ESCSoundingRecord record: records) {
	    // Determine the lower pressure end point for the pressure related values.
	    if (record.getPressure().compareTo(pressure) >= 0) {
		// Lower hasn't been defined, so define it.
		if (lower == null || (pressure.equals(record.getPressure()) && BAD_FLAG.isWorseFlag(record.getPressureFlag()))) {
		    lower = record;
		}
		// Lower has the same pressure flag as the record, so choose the closest record to the interpolation pressure.
		else if (lower.getPressureFlag().compareTo(record.getPressureFlag()) == 0) {
		    lower = lower.getPressure().compareTo(record.getPressure()) <= 0 ? lower : record;
		}
		// Use the record between the lower and current record that has the best pressure flag.
		else {
		    lower = lower.getPressureFlag().isWorseFlag(record.getPressureFlag())  && pressure.compareTo(lower.getPressure()) != 0 ? record : lower;
		}
	    }
	    // Determine the upper pressure end point for the pressure related values.
	    if (record.getPressure().compareTo(pressure) <= 0) {
		// Upper hasn't been defined, so define it.
		if ((upper == null && !(pressure.compareTo(record.getPressure()) == 0 && !BAD_FLAG.isWorseFlag(record.getPressureFlag()))) || (pressure.equals(record.getPressure()) && BAD_FLAG.isWorseFlag(record.getPressureFlag()))) {
		    upper = record;
		}
		// Upper has the same pressure flag as the record, so choose the record closest to the interpolation pressure.
		else if (upper != null && upper.getPressureFlag().compareTo(record.getPressureFlag()) == 0) {
		    upper = upper.getPressure().compareTo(record.getPressure()) >= 0 ? upper : record;
		}
		// Use the record between the upper and current records that has the best pressure flag.
		else {
		    upper = upper != null && upper.getPressureFlag().isWorseFlag(record.getPressureFlag()) && pressure.compareTo(upper.getPressure()) != 0 ? record : upper;
		}
	    }
	}
	
	if (DEBUG) {
	    log.printf("Pressure End Points Used:\n\t%s\n\t%s\n", lower, upper);
	}
	
	// At least one end point could not be determined for the interpolation pressure,
	// so the remaining pressure values can not be interpolated.
	if (lower == null || upper == null) { return; }
	
	// Assign the interpolation pressure to the record.
	try { interpolated.setPressure(pressure, MILLIBARS); }
	// This can be ignored because it is caused by a sea level pressure calculation.		
	catch (CalculationWarning e) {}
	// This exception should only occur if the program/library changes to not allow MILLIBARS
	// as a valid pressure unit.  This is a programming issue, not a data issue, so blow up.
	catch (ConversionException e) {
	    e.printStackTrace();
	    System.exit(1);
	}
	// This occurs if the value being assigned to the pressure.  This is a major problem
	// since the value is the basis of the extraction.  This is such a potentially big
	// problem that the program is going to blow up since it is probably a programming error.
	catch (InvalidValueWarning e) {
	    e.printStackTrace();
	    System.exit(1);
	}
	
	// Determine the pressure flag for the interpolated record.
	try {
	    if (pressure.compareTo(lower.getPressure()) == 0 && !lower.getPressureFlag().isWorseFlag(BAD_FLAG)) {
		interpolated.setPressureFlag(lower.getPressureFlag());
	    }
	    else {
		interpolated.setPressureFlag(lower.getPressureFlag().isWorseFlag(upper.getPressureFlag()) ? lower.getPressureFlag() : upper.getPressureFlag());
	    }
	    if (DEBUG) {
		log.printf("Setting Pressure %.1f Pressure Flag = %.1f\n", interpolated.getPressure(), interpolated.getPressureFlag().getValue());
	    }
	} catch (InvalidFlagException e) {
	    // This exception shoudl only occur if there is a change in the program.  The flag setter should never
	    // be able to be passed an invalid flag because the flag sources shoudl have caused an error earlier.
	    // This will blow up since it is not a data issue.
	    e.printStackTrace();
	    System.exit(1);
	}
	
	// Interpolate the time from the pressure end points.
	try { interpolated.setTime(interpolateTime(pressure, lower, upper)); } 
	// This should only occur when the ascent rate is being calculated interally for the record.
	// This probably will not occur.
	catch (CalculationWarning e) {
	    log.printf("CalculationWarning from Time %.1f mb: %s\n", pressure, e.getMessage());
	    log.flush();
	}
	// This exception occurs if an invalid time is being assigned to the record.
	catch (InvalidValueWarning e) {
	    log.printf("InvalidValueWarning from Time %.1f mb: %s\n", pressure, e.getMessage());
	    log.flush();
	}
	
	// Interpolate the altitude from the pressure end points.
	try { interpolated.setAltitude(interpolateAltitude(pressure, lower, upper), METERS); }
	// This should only occur when the ascent rate is being calculated internally for the record.
	// This probably will not occur.
	catch (CalculationWarning e) {
	    log.printf("CalculationWarning from Altitude %.1f mb: %s\n", pressure, e.getMessage());
	    log.flush();
	}
	// This will only happen if the program/library changes to no longer allow the altitude
	// to accept METERS as a valid length unit.  This is a program issues, not a data issues,
	// so it bomb.
	catch (ConversionException e) {
	    e.printStackTrace();
	    System.exit(1);
	}
	// This exception occurs if an invalid altitude is being assigned to the record.
	catch (InvalidValueWarning e) {
	    log.printf("InvalidValueWarning from Altitude %.1f mb: %s\n", pressure, e.getMessage());
	    log.flush();
	}
	
	// Calculate the ascent rate from the pressure end points
	try { 
	    interpolated.setAscentRate(calculateAscentRate(lower, upper), METERS_PER_SECOND);
	}
	// This should only occur when the ascent rate is being calculated internally for the record
	// and the value being set is null (causing a re-calculation).  This probably will not occur.
	catch (CalculationWarning e) {
	    log.printf("CalculationWarning from Ascent Rate at %.1f mb: %s\n", pressure, e.getMessage());
	    log.flush();
	}
	// This will only happen if the program/library changes to no longer allow the ascent rate to
	// accept METERS_PER_SECOND as a valid velocity unit.  This is a programming issue and not a
	// data issue, so this will bomb.
	catch (ConversionException e) {
	    e.printStackTrace();
	    System.exit(1);
	}
	// This exception occurs if an invalid ascent rate is being assigned to the record.
	catch (InvalidValueWarning e) {
	    log.printf("InvalidValueWarning from Ascent Rate at %.1f mb: %s\n", pressure, e.getMessage());
	    log.flush();
	}
    }
    
    /**
     * Interpolate the relative humidity value using the specified records as end points.
     * @param pressure The pressure the rh is being interpolated to.
     * @param lower The lower pressure end point to use in the interpolation.
     * @param upper The upper pressure end point ot use in the interpolation.
     * @return The interpolated RH value for the specified pressure.
     */
    private Double interpolateRelativeHumidity(Double pressure, ESCSoundingRecord lower, ESCSoundingRecord upper) {
	if (lower.getRelativeHumidity().compareTo(upper.getRelativeHumidity()) == 0) { return lower.getRelativeHumidity(); }
	return lower.getRelativeHumidity() + ((pressure - lower.getPressure()) / (upper.getPressure() - lower.getPressure())) * (upper.getRelativeHumidity() - lower.getRelativeHumidity());
    }
    
    /**
     * Interpoalte the relative humidity for the specified sounding record.
     * @param interpolated The sounding record being interpolated.
     * @param pressure The pressure of the interpolated record.
     * @param records The high resolution data records available to use as end points for the interpolation.
     * @param log The output stream where errors/warnings are to be recorded.
     */
    private void interpolateRelativeHumidityValues(ESCSoundingRecord interpolated, Double pressure, List<ESCSoundingRecord> records, PrintWriter log) {
	ESCSoundingRecord lower = null, upper = null;
	for (ESCSoundingRecord record: records) {
	    // Determine the lower pressure end point for the RH.
	    if (record.getPressure().compareTo(pressure) >= 0 && record.getRelativeHumidity() != null) {
		// Lower hasn't been defined, so define it.
		if (lower == null || (pressure.equals(record.getPressure()) && BAD_FLAG.isWorseFlag(record.getRelativeHumidityFlag()))) { lower = record; }
		// Lower has the same flag as the record, so choose the record closest to the interpolation pressure.
		else if (lower.getRelativeHumidityFlag().compareTo(record.getRelativeHumidityFlag()) == 0) {
		    lower = lower.getPressure().compareTo(record.getPressure()) <= 0 ? lower : record;
		}
		// Use the record between the lower and current records that has the best RH flag.
		else {
		    lower = lower.getRelativeHumidityFlag().isWorseFlag(record.getRelativeHumidityFlag()) && pressure.compareTo(lower.getPressure()) != 0 ? record : lower;
		}
	    }
	    // Determine the upper pressure end point for the RH.
	    if (record.getPressure().compareTo(pressure) <= 0 && record.getRelativeHumidity() != null) {
		// Upper hasn't been defined, so define it.
		if ((upper == null && !(pressure.compareTo(record.getPressure()) == 0 && !BAD_FLAG.isWorseFlag(record.getRelativeHumidityFlag()))) || (pressure.equals(record.getPressure()) && BAD_FLAG.isWorseFlag(record.getRelativeHumidityFlag()))) { upper = record; }
		// Upper has the same flag as the record, so choose the record closest to the interpolation pressure.
		else if (upper != null && upper.getRelativeHumidityFlag().compareTo(record.getRelativeHumidityFlag()) == 0) {
		    upper = upper.getPressure().compareTo(record.getPressure()) >= 0 ? upper : record;
		} 
		// Use the record between the upper and current records that has the best RH flag.
		else {
		    upper = upper != null && upper.getRelativeHumidityFlag().isWorseFlag(record.getRelativeHumidityFlag()) && pressure.compareTo(upper.getPressure()) != 0 ? record : upper;
		}
	    }
	}
	
	if (DEBUG) {
	    log.printf("RH End Points Used:\n\t%s\n\t%s\n", lower, upper);
	}
	
	// At least one end point could not be determined for the interpolation pressure,
	// so the RH cannot be interpolated.
	if (lower == null || upper == null) { return; }
	
	try {
	    // Handle the case where the interpolated pressure has a high resolution record.
	    // The values and flag are only kept if the RH flag is not bad or missing.
	    if (lower.getPressure().compareTo(pressure) == 0 && !lower.getRelativeHumidityFlag().isWorseFlag(BAD_FLAG) && MISSING_FLAG.compareTo(lower.getRelativeHumidityFlag()) != 0) {
		interpolated.setRelativeHumidity(lower.getRelativeHumidity());
		interpolated.setRelativeHumidityFlag(lower.getRelativeHumidityFlag());
	    }
	    // All other cases will cause the RH to be interpoalted, so calculate the value and assign it to the record.
	    else {
		interpolated.setRelativeHumidity(interpolateRelativeHumidity(pressure, lower, upper));
		interpolated.setRelativeHumidityFlag(lower.getRelativeHumidityFlag().isWorseFlag(upper.getRelativeHumidityFlag()) ? lower.getRelativeHumidityFlag() : upper.getRelativeHumidityFlag());
	    }
	    if (DEBUG) {
                log.printf("Setting RH %.1f RH Flag = %.1f\n", interpolated.getRelativeHumidity(), interpolated.getRelativeHumidityFlag().getValue());
	    }
	} catch (CalculationWarning e) {
	    // This exception occurs if there is a problem calculating the dew point value.
	    log.printf("CalculationWarning from RH %.1f mb: %s\n", pressure, e.getMessage());
	    log.flush();
	} catch (InvalidFlagException e) {
	    // This exception should only occur if there is a change in the program/library.  The flag setter should
	    // never be passed an invalid flag because the flag sources should have caused a problem eariler.  This will
	    // blow up as a programming error and not a data issue.
	    e.printStackTrace();
	    System.exit(1);
	} catch (InvalidValueWarning e) {
	    // This exception occurs if an invalid RH is being assigned to the record.
	    log.printf("InvalidValueWarning from RH %.1f mb: %s\n", pressure, e.getMessage());
	    log.flush();
	}
    }
    
    /**
     * Interpoalte the temperature value using the specified records as end points.
     * @param pressure The presure teh temperature is being interpolated to.
     * @param lower The lower pressure end point to use in the interpolation.
     * @param upper The upper pressure end point to use in the interpolation.
     * @return The interpolated temperature for the specified pressure.
     */
    private Double interpolateTemperature(Double pressure, ESCSoundingRecord lower, ESCSoundingRecord upper) {
	if (lower.getTemperature().compareTo(upper.getTemperature()) == 0) { return lower.getTemperature(); }
	return lower.getTemperature() + ((pressure - lower.getPressure()) / (upper.getPressure() - lower.getPressure())) * (upper.getTemperature() - lower.getTemperature());
    }
    
    /**
     * Interpolate the temperature for the specified sounding record.
     * @param interpolated The sounding record being interpolated.
     * @param pressure The pressure of the interpolated record.
     * @param records The high resolution data records available to use as end points for the interpolation.
     * @param log The output stream where errors/warnings are to be written.
     */
    private void interpolateTemperatureValues(ESCSoundingRecord interpolated, Double pressure, List<ESCSoundingRecord> records, PrintWriter log) {
	ESCSoundingRecord lower = null, upper = null;
	for (ESCSoundingRecord record: records) {
	    // Determine the lower pressure end point for the temperature.
	    if (record.getPressure().compareTo(pressure) >= 0 && record.getTemperature() != null) {
		// Lower hasn't been defined, so define it.
		if (lower == null || (pressure.equals(record.getPressure()) && BAD_FLAG.isWorseFlag(record.getTemperatureFlag()))) { lower = record; }
		// Lower has the same flag as the record, so choose the closest record to the interpolation pressure.
		else if (lower.getTemperatureFlag().compareTo(record.getTemperatureFlag()) == 0) {
		    lower = lower.getPressure().compareTo(record.getPressure()) <= 0 ? lower : record;
		}
		// Use the record between the lower and current record that has the best temperature flag.
		else {
		    lower = lower.getTemperatureFlag().isWorseFlag(record.getTemperatureFlag()) && pressure.compareTo(lower.getPressure()) != 0 ? record : lower;
		}
	    }
	    // Determine the upper pressure end point for the temperature.
	    if (record.getPressure().compareTo(pressure) <= 0 && record.getTemperature() != null) {
		// Upper hasn't been defined, so define it.
		if ((upper == null && !(pressure.compareTo(record.getPressure()) == 0 && !BAD_FLAG.isWorseFlag(record.getTemperatureFlag()))) || (pressure.equals(record.getPressure()) && BAD_FLAG.isWorseFlag(record.getTemperatureFlag()))) { upper = record; }
		// Upper has the same flag as the record, so choose the closest record to the interpolated pressure.
		else if (upper != null && upper.getTemperatureFlag().compareTo(record.getTemperatureFlag()) == 0) {
		    upper = upper.getPressure().compareTo(record.getPressure()) >= 0 ? upper : record;
		}
		// Use the record between the upper and current record that has the best temperature flag.
		else {
		    upper = upper != null && upper.getTemperatureFlag().isWorseFlag(record.getTemperatureFlag()) && pressure.compareTo(upper.getPressure()) != 0 ? record : upper;
		}
	    }
	}
	
	if (DEBUG) {
	    log.printf("Temperature End Points Used:\n\t%s\n\t%s\n", lower, upper);
	}
	
	// At least one end point could not be determined for the interpolation pressure,
	// so the temperature can not be interpolated.
	if (lower == null || upper == null) { return; }
	
	try {
	    // Handle the case where the interpolated pressure has a high resolution record.
	    // The value and flag are only kept if the temperature flag is not bad or missing.
	    if (lower.getPressure().compareTo(pressure) == 0 && !lower.getTemperatureFlag().isWorseFlag(BAD_FLAG) && MISSING_FLAG.compareTo(lower.getTemperatureFlag()) != 0) {
		interpolated.setTemperature(lower.getTemperature(), CELCIUS);
		interpolated.setTemperatureFlag(lower.getTemperatureFlag());
	    }
	    // All other cases will cause the temperature to be interpolated, so calculate the
	    // value and assign it to the record.
	    else {
		interpolated.setTemperature(interpolateTemperature(pressure, lower, upper), CELCIUS);
		interpolated.setTemperatureFlag(lower.getTemperatureFlag().isWorseFlag(upper.getTemperatureFlag()) ? lower.getTemperatureFlag() : upper.getTemperatureFlag());
	    }
            if (DEBUG) {
                log.printf("Setting Temperature %.1f Temp Flag = %.1f\n", interpolated.getTemperature(), interpolated.getTemperatureFlag().getValue());
            }
	} catch (CalculationWarning e) {
	    // This exception occurs if there is a problem calculating the dew point value.
	    log.printf("Calculation Warning from Temperature %.1f mb: %s\n", pressure, e.getMessage());
	    log.flush();
	} catch (ConversionException e) {
	    // This exception should only occur if the program/library changes to not allow CELCIUS to
	    // be a valid unit for temperature.  This will blow up as a programming problem and not a data issue.
	    e.printStackTrace();
	    System.exit(1);
	} catch (InvalidFlagException e) {
	    // This exception should only occur if their is a change in the program/library.  The flag setter should
	    // never be passed an invalid flag because the flag sources should have caused a problem earlier.  This
	    // will blow up as a programming problem and not a data issue.
	    e.printStackTrace();			
	    System.exit(1);
	} catch (InvalidValueWarning e) {
	    // This exception occurs if an invalid temperature is being assigned to the record.
	    log.printf("InvalidValueWarning from Temperature %.1f mb: %s\n", pressure, e.getMessage());
	    log.flush();
	}
    }
    
    /**
     * Interpolate the time using the specified records as end points.
     * @param pressure The pressure the time is being interpolated to.
     * @param lower The lower pressure end point to use in the interpolation.
     * @param upper The upper pressure end point to use in the interpolation.
     * @return The interpolated time for the specified pressure.
     */
    private Double interpolateTime(Double pressure, ESCSoundingRecord lower, ESCSoundingRecord upper) {
	if (lower.getTime() == null || upper.getTime() == null) { return null; }
	else if (lower.getTime().compareTo(upper.getTime()) == 0) { return lower.getTime(); }
	else {
	    return lower.getTime() + ((pressure - lower.getPressure()) / (upper.getPressure() - lower.getPressure())) * (upper.getTime() - lower.getTime());	
	}
    }
    
    /**
     * Interpolate the U component wind value using the specified records as end points.
     * @param pressure The pressure the u component is being interpolated to.
     * @param lower The lower pressure end point to use in the interpolation.
     * @param upper The upper pressure end point to use in the interpolation.
     * @return The interpolated U component for the specified pressure.
     */
    private Double interpolateUComponent(Double pressure, ESCSoundingRecord lower, ESCSoundingRecord upper) {
	if (lower.getUComponent().compareTo(upper.getUComponent()) == 0) { return lower.getUComponent(); }
	return lower.getUComponent() + ((pressure - lower.getPressure()) / (upper.getPressure() - lower.getPressure())) * (upper.getUComponent() - lower.getUComponent());
    }
    
    /**
     * Interpolate the V component wind value using the specified records as end points.
     * @param pressure The pressure the v component is being interpolated to.
     * @param lower The lower pressure end point to use in the interpolation.
     * @param upper The upper pressure end point to use in the interpolation.
     * @return The interpolated V component for the specified pressure.
     */
    private Double interpolateVComponent(Double pressure, ESCSoundingRecord lower, ESCSoundingRecord upper) {
	if (lower.getVComponent().compareTo(upper.getVComponent()) == 0) { return lower.getVComponent(); }
	return lower.getVComponent() + ((pressure - lower.getPressure()) / (upper.getPressure() - lower.getPressure())) * (upper.getVComponent() - lower.getVComponent());
    }
    
    /**
     * Interpolate the wind values for the specified sounding record.
     * @param interpolated The sounding record being interpolated.
     * @param pressure The pressure of the interpolated record.
     * @param records The high resolution data records to use in the interpolation.
     * @param log The output stream where errors/warnings are to be written.
     * @throws ExtractionException if there is a problem interpolating the wind values.  This is 
     * usually caused by an improper combination of wind values and flags.
     */
    private void interpolateWindValues(ESCSoundingRecord interpolated, Double pressure, List<ESCSoundingRecord> records, PrintWriter log) throws ExtractionException {
	ESCSoundingRecord lower = null, upper = null;
	for (ESCSoundingRecord record: records) {
	    // Make sure that the both the U and V components are missing or have a value.
	    if ((record.getUComponent() == null && record.getVComponent() != null) ||
		(record.getUComponent() != null && record.getVComponent() == null)) {
		throw new ExtractionException("There is a missing U or V component with a non-missing value.");
	    }
	    // Make sure that the U and V component flags are the same.
	    if (record.getUComponentFlag().compareTo(record.getVComponentFlag()) != 0) {
		throw new ExtractionException("The U and V component flags do not match.");
	    }
	    
	    // The data checks are now out of the way, so the interpolation can begin.
	    // This allows the software to use the U component for determining the interpolation
	    // end points and know that it is okay to use them for the V component too.
	    
	    // Determine the Lower pressure end point for the wind components.
	    if (record.getPressure().compareTo(pressure) >= 0 && record.getUComponent() != null) {
		// Lower hasn't been defined, so define it.
		if (lower == null || (pressure.equals(record.getPressure()) && BAD_FLAG.isWorseFlag(record.getUComponentFlag()))) { lower = record; }
		// Lower has the same flag as the record, so choose the closest record to the interpolation pressure.
		else if (lower.getUComponentFlag().compareTo(record.getUComponentFlag()) == 0) {
		    lower = lower.getPressure().compareTo(record.getPressure()) <= 0 ? lower : record;
		}
		// Use the record between the lower and current record that has the best wind flag.
		else {
		    lower = lower.getUComponentFlag().isWorseFlag(record.getUComponentFlag()) && pressure.compareTo(lower.getPressure()) != 0 ? record : lower;
		}
	    }
	    // Determine the Upper pressure end point for the wind components.
	    if (record.getPressure().compareTo(pressure) <= 0 && record.getUComponent() != null) {
		// Upper hasn't been defined, so define it.
		if ((upper == null && !(pressure.compareTo(record.getPressure()) == 0 && !BAD_FLAG.isWorseFlag(record.getUComponentFlag()))) || (pressure.equals(record.getPressure()) && BAD_FLAG.isWorseFlag(record.getUComponentFlag()))) { upper = record; }
		// Upper has the same flag as the record, so choose the closest record to the interpolated pressure.
		else if (upper != null && upper.getUComponentFlag().compareTo(record.getUComponentFlag()) == 0) {
		    upper = upper.getPressure().compareTo(record.getPressure()) >= 0 ? upper : record;
		}
		// Use the record between the upper and current record that has the best wind flag.
		else {
		    upper = upper != null && upper.getUComponentFlag().isWorseFlag(record.getUComponentFlag()) && pressure.compareTo(upper.getPressure()) != 0 ? record : upper;
		}
	    }
	}
	
	if (DEBUG) {
	    log.printf("Wind End Points Used:\n\t%s\n\t%s\n", lower, upper);
	}
	
	// At least one end point could not be determined for the interpolation pressure,
	// so the wind values cannot be interpolated.
	if (lower == null || upper == null) { return; }
	
	try {
	    // Handle the case where the interpolated pressure has a high resolution record.
	    // The values are only kept if the wind flag is not bad.
	    if (lower.getPressure().compareTo(pressure) == 0 && !lower.getUComponentFlag().isWorseFlag(BAD_FLAG) && MISSING_FLAG.compareTo(lower.getUComponentFlag()) != 0) {
		interpolated.setUComponent(lower.getUComponent(), METERS_PER_SECOND);
		interpolated.setVComponent(lower.getVComponent(), METERS_PER_SECOND);
		interpolated.setUComponentFlag(lower.getUComponentFlag());
		interpolated.setVComponentFlag(lower.getVComponentFlag());
	    }
	    // All other cases cause the values to be interpolated, so calculate them and assign them to the record.
	    else {
		interpolated.setUComponent(interpolateUComponent(pressure, lower, upper), METERS_PER_SECOND);
		interpolated.setVComponent(interpolateVComponent(pressure, lower, upper), METERS_PER_SECOND);
		interpolated.setUComponentFlag(lower.getUComponentFlag().isWorseFlag(upper.getUComponentFlag()) ? lower.getUComponentFlag() : upper.getUComponentFlag());
		interpolated.setVComponentFlag(lower.getVComponentFlag().isWorseFlag(upper.getVComponentFlag()) ? lower.getVComponentFlag() : upper.getUComponentFlag());
	    }
            if (DEBUG) {
                log.printf("Setting U Comp %.1f U Comp Flag = %.1f\n", interpolated.getUComponent(), interpolated.getUComponentFlag().getValue());
                log.printf("Setting V Comp %.1f V Comp Flag = %.1f\n", interpolated.getVComponent(), interpolated.getVComponentFlag().getValue());
            }
	} catch (CalculationWarning e) {
	    // This exception occurs if there is a problem determining the wind speed and wind direction values.
	    log.printf("Calculation Warning from U/V Components %.1f mb: %s\n", pressure, e.getMessage());
	    log.flush();
	} catch (ConversionException e) {
	    // This exception should only occur if the program/library changes to not all METERS_PER_SECOND to
	    // be a valid unit to one of the components.  This will blow up since this is a programming issue
	    // and not a data issue.
	    e.printStackTrace();
	    System.exit(1);
	} catch (InvalidFlagException e) {
	    // This exception should only occur if there is a change in the program.  The functions should never
	    // be able to be passed an invalid flag because the sources should have caused an error earlier for
	    // the same problem.  This will blow up since it is not a data issue.
	    e.printStackTrace();
	    System.exit(1);
	} catch (InvalidValueWarning e) {
	    // This exception occurs if an invalid is being assigned to the record.
	    log.printf("InvalidValueWarning from U/V Components %.1f mb: %s\n", pressure, e.getMessage());
	    log.flush();
	}
	
	// Interpolate the latitude value for the record using the wind end points.
	try {
	    if (lower.getLatitude() != null && upper.getLatitude() != null) {
		interpolated.setLatitude(interpolateLatitude(pressure, lower, upper));
	    }
	} catch (InvalidValueWarning e) {
	    // This exception should never occur because averaging two valid latitudes should result in
	    // another valid latitude.
	    e.printStackTrace();
	    System.exit(1);
	}
	// Interpolate the longitude value for the record using the wind end points.
	try {
	    if (lower.getLongitude() != null && upper.getLongitude() != null) {
		interpolated.setLongitude(interpolateLongitude(pressure, lower, upper));
	    }
	} catch (InvalidValueWarning e) {
	    // This exception should never occur because averaging two valid longitudes should
	    // result in another valid longitude.
	    e.printStackTrace();
	    System.exit(1);
	}
    }
    
    /**
     * Open the specified file to a writeable stream.  This is used to open ASCII files and
     * gzipped ASCII files.
     * @param outFile The file to be opened.
     * @return The output stream for the opened file.
     * @throws IOException if there is a problem opening the stream for the file.
     */
    private PrintWriter openToWrite(File outFile) throws IOException {
	// Open a gzip file.
	if (outFile.getName().toLowerCase().endsWith(".gz")) {
	    return new PrintWriter(new OutputStreamWriter(new GZIPOutputStream(new FileOutputStream(outFile))));
	}
	// Open a standard text file.
	else {
	    return new PrintWriter(new FileWriter(outFile));
	}
    }
    
    /**
     * Read in the arguments from the command line and setup the variables
     * needed to create the day files.
     * @param args The list of command line arguments.
     * @throws InvalidParameterException if the argument list length is less 
     * than 2 or more than 3.
     */
    public void parseArguments(String[] args) throws InvalidParameterException {
	List<String> params = new ArrayList<String>(Arrays.asList(args));
	
	// Search for flags at the start of the list
	while (params.size() > 0 && params.get(0).startsWith("-")) {
	    if (params.get(0).equals("-Z")) { GZIP = false; }
	    else if (params.get(0).equals("-DEBUG")) { DEBUG = true; }
	    else {
		throw new InvalidParameterException("parseArguments", "args", "Invalid flag of "+params.get(0));
	    }
	    params.remove(0);
	}
	
	// Make sure the argument list is the expected size.
	if (params.size() < 3 || params.size() > 4) {
	    throw new InvalidParameterException("parseArguments", "args", "Invalid arugment list.  See the usage.");
	}
	
	// Set up the variables
	INPUT_DIRECTORY = new File(params.get(0));
	OUTPUT_DIRECTORY = new File(params.get(1));
	LOG_FILE = new File(params.get(2));
	// Set the default pattern if it didn't come through the command line.
	PATTERN = params.size() == 4 ? params.get(3) : "\\.cls(\\.gz)?$";
    }
    
    /**
     * Print the usage of the extractor to standard out.
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
    
    public void replaceUncheckedFlags(ESCSounding sounding) {
	// Replace UNCHECKED flags with GOOD flags.  This assumes that the
	// sounding has already been QCed and that any UNCHECKED values are
	// GOOD since they would have been flagged otherwise if they were
	// not.  The ascent rate flag is not changed since it is not used
	// by the extractor.
	//
	// Each time a flag is set, the InvalidFlagException is ingored.
	// This is because the exception occurs when there is a flag/value
	// mismatch.  Since there is a test for the flag being UNCHECKED
	// before the resetting of the flag to GOOD, the value cannot be
	// missing, thus preventing a flag/value mismatch and the 
	// exception from being thrown.
	for (ESCSoundingRecord record: sounding.getRecords()) {
	    if (record.getPressureFlag().equals(UNCHECKED_FLAG)) {
		try { record.setPressureFlag(GOOD_FLAG); }
		catch (InvalidFlagException e) {};
	    }
	    if (record.getTemperatureFlag().equals(UNCHECKED_FLAG)) {
		try {record.setTemperatureFlag(GOOD_FLAG); }
		catch (InvalidFlagException e) {};
	    }
	    if (record.getRelativeHumidityFlag().equals(UNCHECKED_FLAG)) {
		try { record.setRelativeHumidityFlag(GOOD_FLAG); }
		catch (InvalidFlagException e) {};
	    }
	    if (record.getUComponentFlag().equals(UNCHECKED_FLAG)) {
		try { record.setUComponentFlag(GOOD_FLAG); }
		catch (InvalidFlagException e) {};
	    }
	    if (record.getVComponentFlag().equals(UNCHECKED_FLAG)) {
		try { record.setVComponentFlag(GOOD_FLAG); }
		catch (InvalidFlagException e) {};
	    }
	}
    }

    /**
     * Run the extraction program.
     */
    public void run() throws IOException, NumberFormatException, ConversionException, DateTimeException, InvalidValueException, InvalidValueWarning {
	ESCSoundingParser parser = new ESCSoundingParser(false);
	
	// Create the log file.
	LOG_FILE.getParentFile().mkdirs();
	PrintWriter log = new PrintWriter(new FileWriter(LOG_FILE));
	
	// Find the soundings that are to be extracted and loop through them.
	List<File> files = findInputFiles();

	// Sort the files so they are processed in order.
	Collections.sort(files);

	for (File file: files) {
	    System.out.println("Processing file: "+file);
	    log.printf("Processing file: %s\n", file);
	    
	    File outFile = createOutputFile(file);
	    // Create the directories where the log file will be stored.
	    if (outFile.getParentFile() != null) { outFile.getParentFile().mkdirs(); }
	    
	    // Open the output file.
	    PrintWriter out = openToWrite(outFile);
	    
	    // Parse the file into a list of soundings.
            List<ESCSounding> soundings = parser.parseFile(file);
	    for (ESCSounding sounding: soundings) {
                // Only try to extract a sounding if it contains data.
                if (sounding.getRecords().size() > 0) {
		    // Extract the interpolation data from the sounding.
		    ESCSounding extract = extract(sounding, log);
		    // Print the sounding to the output file.
		    out.println(extract);
                }
	    }
	    
	    // Close the open output stream.
	    out.close();
	}
	
	// Close down the log file.
	log.close();		
    }
}
