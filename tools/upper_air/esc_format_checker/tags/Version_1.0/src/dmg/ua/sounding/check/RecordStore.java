package dmg.ua.sounding.check;

import java.io.*;

/**
 * <p>The RecordStore is a container for holding and testing data values on a
 * single data line.  It compares values with their flags, checks to see if the
 * Qu and Qv flags are the same, makes sure that the four wind values are all
 * missing or all have non-missing values, and tests to see if the dew point
 * exists only when there are temperature and relative humidity values.</p>
 * 
 * @author Joel Clawson
 */
public class RecordStore {

	private Double pressure, temperature, dewPoint, relativeHumidity, uComponent, vComponent, windSpeed, windDirection, ascentRate;
	private Double pressFlag, tempFlag, rhFlag, uFlag, vFlag, rateFlag;
	
	/**
	 * Create a new instance of a RecordState.
	 */
	public RecordStore() {}

	/**
	 * Determine if the specified value matches its flag.
	 * @param value The value to be compared to the flag.
	 * @param flag The flag to be compared to the value.
	 * @param missing The value that is considered missing during the test.
	 * @return <code>true</code> if the value and flag agree, <code>false</code>
	 * if they do not.
	 */
	private boolean compareFlagWithValue(Double value, Double flag, Double missing) {
		if (flag.equals(9.0) && !value.equals(missing)) { return false; }
		else if (!flag.equals(9.0) && value.equals(missing)) { return false; }
		else { return true; }
	}
	
	/**
	 * Perform the consistancy checks on the data stored in the RecordStore.
	 * @param log The output stream where failures are to be written.
	 * @param lineNumber The number of the data line in the sounding file.
	 */
	public void executeConsistancyChecks(PrintWriter log, int lineNumber) {
		// 1.  Compare the pressure value with the flag.
		if (pressure != null && pressFlag != null && !compareFlagWithValue(pressure, pressFlag, 9999.0)) {
			logFlagMismatch(log, "Press", pressure, pressFlag, lineNumber);
		}
		// 2.  Compare the temperature value with the flag.
		if (temperature != null && tempFlag != null && !compareFlagWithValue(temperature, tempFlag, 999.0)) {
			logFlagMismatch(log, "Temp", temperature, tempFlag, lineNumber);
		}
		// 3.  Compare the rh value with the flag.
		if (relativeHumidity != null && rhFlag != null && !compareFlagWithValue(relativeHumidity, rhFlag, 999.0)) {
			logFlagMismatch(log, "RH", relativeHumidity, rhFlag, lineNumber);
		}
		// 4.  Compare the u component value with the flag.
		if (uComponent != null && uFlag != null && !compareFlagWithValue(uComponent, uFlag, 9999.0)) {
			logFlagMismatch(log, "Ucmp", uComponent, uFlag, lineNumber);
		}
		// 5.  Compare the v component value with the flag.
		if (vComponent != null && vFlag != null && !compareFlagWithValue(vComponent, vFlag, 9999.0)) {
			logFlagMismatch(log, "Vcmp", vComponent, vFlag, lineNumber);
		}
		// 6.  Compare the ascent rate value with its flag.
		if (ascentRate != null && rateFlag != null && !compareFlagWithValue(ascentRate, rateFlag, 999.0)) {
			logFlagMismatch(log, "Wcmp", ascentRate, rateFlag, lineNumber);
		}		
		// 7.  Compare the wind component flags (should always be the same)
		if (uFlag != null && vFlag != null && !uFlag.equals(vFlag)) {
			log.printf("The 'Data' line has a mismatch between the 'Qu' (%.1f) and 'Qv' (%.1f) flags on line %d.\n", uFlag, vFlag, lineNumber);
		}
		
		// 8.  Make sure all of the wind values exist or are missing
		int missingWindCount = 0;
		if (windSpeed != null && windSpeed.equals(999.0)) { missingWindCount++; }
		if (windDirection != null && windDirection.equals(999.0)) { missingWindCount++; }
		if (uComponent != null && uComponent.equals(9999.0)) { missingWindCount++; }
		if (vComponent != null && vComponent.equals(9999.0)) { missingWindCount++; }
		if (missingWindCount > 0 && missingWindCount < 4) {
			log.printf("The 'Data' line has conflicting wind values %.1f %.1f %.1f %.1f (Ucmp, Vcmp, spd, dir) on line %d.\n", uComponent, vComponent, windSpeed, windDirection, lineNumber);
		}
		
		// 9.  Make sure all of the temp/dewpt/rh values match okay (dew point
		// should only exist if both the temp and rh are valid).
		if (dewPoint != null && ((!dewPoint.equals(999.0) &&
				((temperature != null && temperature.equals(999.0)) ||
				 (relativeHumidity != null && relativeHumidity.equals(999.0)))) ||
				(dewPoint.equals(999.0) && temperature != null && !temperature.equals(999.0) &&
				 relativeHumidity != null && !relativeHumidity.equals(999.0) && relativeHumidity.compareTo(0.0) > 0))) {
			log.printf("The 'Data' line has a conflicting 'Dewpt' %.1f for 'Temp' %.1f and 'RH' %.1f on line %d.\n", dewPoint, temperature, relativeHumidity, lineNumber);
		}
	}
	
	/**
	 * Print the failure of a value/flag mismatch to an output stream.
	 * @param log The output stream where the failure is to be written.
	 * @param name The name of the measurement that caused the failure.
	 * @param value The value of the measurement that didn't match the flag.
	 * @param flag The value of the flag that didn't match the value.
	 * @param lineNumber The line number of the data line the failure occured on.
	 */
	private void logFlagMismatch(PrintWriter log, String name, Double value, Double flag, int lineNumber) {
		log.printf("The 'Data' line has a mismatch between the value %.1f and flag %.1f for '%s' on line %d.\n", value, flag, name, lineNumber);
	}
	
	/**
	 * Set the ascent rate value in the RecordStore.
	 * @param rate The ascent rate value.
	 */
	public void setAscentRate(Double rate) { ascentRate = rate; }
	
	/**
	 * Set the ascent rate flag in the RecordStore.
	 * @param flag The flag for the ascent rate.
	 */
	public void setAscentRateFlag(Double flag) { rateFlag = flag; }
	
	/**
	 * Set the dew point value in the RecordStore.
	 * @param dewpt The dew point value.
	 */
	public void setDewPoint(Double dewpt) { dewPoint = dewpt; }
	
	/**
	 * Set the pressure value in the RecordStore.
	 * @param press The pressure value.
	 */
	public void setPressure(Double press) { pressure = press; }
	
	/**
	 * Set the pressure flag in the RecordStore.
	 * @param flag The flag for the pressure.
	 */
	public void setPressureFlag(Double flag) { pressFlag = flag; }
	
	/**
	 * Set the relative humidity value in the RecordStore.
	 * @param rh The relative humidity value.
	 */
	public void setRelativeHumidity(Double rh) { relativeHumidity = rh; }
	
	/**
	 * Set the relative humidity flag in the RecordStore.
	 * @param flag The flag for the relative humidity.
	 */
	public void setRelativeHumidityFlag(Double flag) { rhFlag = flag; }
	
	/**
	 * Set the temperature value in the RecordStore.
	 * @param temp The temperature value.
	 */
	public void setTemperature(Double temp) { temperature = temp; }
	
	/**
	 * Set the temperature flag in the RecordStore.
	 * @param flag The flag for the temperature.
	 */
	public void setTemperatureFlag(Double flag) { tempFlag = flag; }
	
	/**
	 * Set the U component value in the RecordStore.
	 * @param comp The U component value.
	 */
	public void setUComponent(Double comp) { uComponent = comp; }
	
	/**
	 * Set the U component flag in the RecordStore.
	 * @param flag The flag for the U component.
	 */
	public void setUComponentFlag(Double flag) { uFlag = flag; }
	
	/**
	 * Set the V component flag in the RecordStore.
	 * @param comp The V component value.
	 */
	public void setVComponent(Double comp) { vComponent = comp; }
	
	/**
	 * Set the V component flag in the RecordStore.
	 * @param flag The flag for the V component.
	 */
	public void setVComponentFlag(Double flag) { vFlag = flag; }
	
	/**
	 * Set the wind direction in the RecordStore.
	 * @param dir The wind direction value.
	 */
	public void setWindDirection(Double dir) { windDirection = dir; }
	
	/**
	 * Set the wind speed in the RecordStore.
	 * @param spd The wind speed value.
	 */
	public void setWindSpeed(Double spd) { windSpeed = spd; }
}
