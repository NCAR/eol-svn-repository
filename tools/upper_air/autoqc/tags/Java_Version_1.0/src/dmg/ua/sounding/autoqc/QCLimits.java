package dmg.ua.sounding.autoqc;

import java.util.*;

/**
 * <p>The QCLimits class is a container for holding the variable properties/
 * Quality Control limits for a specific run of the QC program.  It looks
 * for a required set of keys in a ResourceBundle defined from a .properties
 * file.</p>
 * 
 * @author Joel Clawson
 */
public class QCLimits {

	private boolean hotLatitude;
	private int averagingValue;
	
	private Double adiabaticLapseRate;
	private Double badAscentRateChange, questionableAscentRateChange;
	private Double badLapseRate, questionableLapseRate;
	private Double badRapidPressureIncrease, questionableRapidPressureIncrease;
	private Double badSurfaceLapseRate, questionableSurfaceLapseRate;
	private Double decreasingPressureCheck;
	private Double lapseRateDifference, lapseRateLimit;
	private Double maxBadWindSpeed;
	private Double maxMissingDewPoint;
	private Double maxMissingPressure;
	private Double maxMissingTemperature;
	private Double maxMissingWindSpeed;
	private Double maxQuestionableAltitude, minQuestionableAltitude;
	private Double maxQuestionableAscentRate;
	private Double maxQuestionableDewPoint, minQuestionableDewPoint;
	private Double maxQuestionablePressure, minQuestionablePressure;
	private Double maxQuestionableTemperature, minQuestionableTemperature;
	private Double maxQuestionableWindSpeed, minQuestionableWindSpeed;
	private Double pressureLimit, surfacePressureLimit;
	private Double rapidTemperatureIncrease, badRapidTemperatureIncrease;
	private Double stratRapidTemperatureIncrease;
	private Double badStratRapidTemperatureIncrease;
	private Double surfaceRapidTemperatureIncrease;
	private Double badSurfaceRapidTemperatureIncrease;
	
	/**
	 * Create a new instance of a QCLimits.
	 * @param limits The bundle containing the limits.
	 * @throws QCException if an expected limit key is missing or an illegal
	 * value is defined for a key.
	 */
	public QCLimits(ResourceBundle limits) throws QCException {
		// Parse the value used for averaging data points in the QC.
		try {
			averagingValue =
				Integer.parseInt(limits.getString("AVERAGING_VALUE")) - 1;
		} 
		// If the value is not defined in the limit file, then no averaging is
		// to be done so don't produce an error.
		catch (MissingResourceException e) {
			averagingValue = 1;
		}
		// Make sure the provided averaging value is actually a legal averaging
		// value.  This will prevent the QC from crashing when it tries to
		// average the wrong points.
		if (averagingValue < 1) {
			throw new QCException(
					String.format("Averaging value %d must be greater than 1.",
							averagingValue + 1));
		}
		
		// Read in the latitude type from the properties file.
		try {
			String latitudeType = limits.getString("LATITUDE_TYPE");
			// Look for the acceptable values for the latitude type.
			if (latitudeType.equalsIgnoreCase("HOT")) { hotLatitude = true; }
			else if (latitudeType.equalsIgnoreCase("COLD")) { 
				hotLatitude = false;
			}
			// Handle illegal latitude type values by throwing an exception
			// and preventing the limits to finish loading.
			else {
				throw new QCException("LATITUDE_TYPE is not defined to"
						+ " be either HOT or COLD.");
			}

			// QC Properties for checking pressure rates 
			decreasingPressureCheck = 
				parse(limits, "DECREASING_PRESSURE_CHECK");

			// QC Properties for checking differential ascent rates
			badAscentRateChange = parse(limits, "BAD_ASCENT_RATE_CHANGE");	
			questionableAscentRateChange =
				parse(limits, "QUESTIONABLE_ASCENT_RATE_CHANGE");
			
			// QC Properties for checking altitude values
			maxQuestionableAltitude = 
				parse(limits,"MAXIMUM_QUESTIONABLE_ALTITUDE");
			minQuestionableAltitude = 
				parse(limits,"MINIMUM_QUESTIONABLE_ALTITUDE");

			// QC Properties for checking ascent rate values
			maxQuestionableAscentRate =
				parse(limits, "MAXIMUM_QUESTIONABLE_ASCENT_RATE");
			
			// QC Properties for checking dew point values
			maxMissingDewPoint = parse(limits, "MAXIMUM_MISSING_DEW_POINT");
			maxQuestionableDewPoint =
				parse(limits, "MAXIMUM_QUESTIONABLE_DEW_POINT");
			minQuestionableDewPoint =
				parse(limits, "MINIMUM_QUESTIONABLE_DEW_POINT");

			// QC Properties for checking pressure values
			maxMissingPressure = parse(limits, "MAXIMUM_MISSING_PRESSURE");
			maxQuestionablePressure = 
				parse(limits,"MAXIMUM_QUESTIONABLE_PRESSURE");
			minQuestionablePressure = 
				parse(limits,"MINIMUM_QUESTIONABLE_PRESSURE");

			// QC Properties for checking temperature values
			maxMissingTemperature = parse(limits,"MAXIMUM_MISSING_TEMPERATURE");
			maxQuestionableTemperature =
				parse(limits, "MAXIMUM_QUESTIONABLE_TEMPERATURE");
			minQuestionableTemperature =
				parse(limits, "MINIMUM_QUESTIONABLE_TEMPERATURE");

			// QC Properties for checking wind speed values
			maxBadWindSpeed = parse(limits, "MAXIMUM_BAD_WIND_SPEED");
			maxMissingWindSpeed = parse(limits, "MAXIMUM_MISSING_WIND_SPEED");
			maxQuestionableWindSpeed = 
				parse (limits, "MAXIMUM_QUESTIONABLE_WIND_SPEED");
			minQuestionableWindSpeed = 
				parse(limits, "MINIMUM_QUESTIONABLE_WIND_SPEED");

			// QC Properties for checking rapid pressure increases
			badRapidPressureIncrease =
				parse(limits, "BAD_RAPID_PRESSURE_INCREASE");
			questionableRapidPressureIncrease =
				parse(limits, "QUESTIONABLE_RAPID_PRESSURE_INCREASE");

			// QC Properties for defining the adiabatic lapse rate used
			// for the super adiabatic lapse rate check and rapid
			// temperature increase check
			adiabaticLapseRate = parse(limits, "ADIABATIC_LAPSE_RATE");
			lapseRateDifference = parse(limits, "LAPSE_RATE_DIFFERENCE");
			lapseRateLimit = parse(limits, "LAPSE_RATE_LIMIT");

			// QC Properties for defining the pressure limits used for selecting
			// the rapid temperature increase limits
			pressureLimit = parse(limits, "PRESSURE_LIMIT");
			surfacePressureLimit = parse(limits, "SURFACE_PRESSURE_LIMIT");

			// QC Properties for the rapid temperature increase check when
			// the records are over the pressure limit, but under the surface
			// limit
			rapidTemperatureIncrease = 
				parse(limits, "RAPID_TEMPERATURE_INCREASE");
			badRapidTemperatureIncrease = 
				parse(limits, "BAD_RAPID_TEMPERATURE_INCREASE");

			// QC Properties for the rapid temperature increase check when
			// the records are under the pressure limit
			stratRapidTemperatureIncrease = 
				parse(limits, "STRAT_RAPID_TEMPERATURE_INCREASE");
			badStratRapidTemperatureIncrease = 
				parse(limits, "BAD_STRAT_RAPID_TEMPERATURE_INCREASE");

			// QC Properties for the rapid temperature increase check when
			// the records are over the surface limit.  This is only used
			// in COLD latitude types.
			if (!isHotLatitude()) {
				surfaceRapidTemperatureIncrease = 
					parse(limits, "SURFACE_RAPID_TEMPERATURE_INCREASE");
				badSurfaceRapidTemperatureIncrease = 
					parse(limits, "BAD_SURFACE_RAPID_TEMPERATURE_INCREASE");
			}			
			
			// QC Properties for checking the adiabatic lapse rate over or
			// equal to 10 mb from the surface
			badLapseRate = parse(limits, "BAD_LAPSE_RATE");
			questionableLapseRate =	parse(limits, "QUESTIONABLE_LAPSE_RATE");
			// QC Properties for checking the adiabatic lapse rate with
			// 10 mb of the surface.
			badSurfaceLapseRate = parse(limits, "BAD_SURFACE_LAPSE_RATE");
			questionableSurfaceLapseRate =
				parse(limits, "QUESTIONABLE_SURFACE_LAPSE_RATE");
		}
		
		catch (MissingResourceException e) {
			throw new QCException(
					String.format("%s is not defined in the properties file.",
							e.getKey()));
		}
	}
	
	/**
	 * Get the averaging value to use in the QC process.  It defines the number
	 * of points past the current point to use when checking different rates.
	 * @return The averaging value to use during the QC.
	 */
	public int getAveragingValue() {
		return averagingValue;
	}
	
	/**
	 * Get the adiabatic lapse rate value used by the QC for comparing the
	 * lapse rate in the super adiabatic lapse rate check and the rapid
	 * temperature increase check.
	 * @return The adiabatic lapse rate value.
	 */
	public Double getAdiabaticLapseRate() {
		return adiabaticLapseRate;
	}
	
	/**
	 * Get the ascent rate change threshold such that any ascent rate change 
	 * that is greater than or equal to the value is considered BAD for the 
	 * sounding records over the ascent rate interval.
	 * @return The difference in ascent rate threshold in m/s.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#differentialAscensionRate(List, Double, boolean, java.io.PrintWriter)
	 */
	public Double getBadAscentRateChange() {
		return badAscentRateChange;
	}
	
	/**
	 * Get the lapse rate threshold, for sounding records over or equal to 10.0
	 * mb from the surface record, such that any calculated lapse rate that is
	 * less than or equal to the threshold is considered BAD for the sounding
	 * records over which the laspe rate was calculated.
	 * @return The non-surface bad lapse rate threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#superAdiabaticLapseRateCheck(List, Double, Double, java.io.PrintWriter)
	 */
	public Double getBadLapseRate() {
		return badLapseRate;
	}
	
	/**
	 * Get the rapid pressure increase threshold such that any pressure change
	 * greater than or equal to the threshold is considered BAD for the sounding
	 * records over which the rapid pressure increase occured.
	 * @return The bad rapid pressure increase threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#rapidPressureCheck(List, java.io.PrintWriter)
	 */
	public Double getBadRapidPressureIncrease() {
		return badRapidPressureIncrease;
	}
	
	/**
	 * Get the rapid temperature increase threshold such that any temperature
	 * change greater than or equal to the threshold is considered BAD for the
	 * sounding records over which the rapid temperature increase occured.
	 * @return The bad rapid temperature increase threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#rapidTemperatureCheck(List, Double, Double, java.io.PrintWriter)
	 */
	public Double getBadRapidTemperatureIncrease() {
		return badRapidTemperatureIncrease;
	}
	
	/**
	 * Get the strat rapid temperature increase threshold such that any 
	 * temperature change greater than or equal to the threshold is considered 
	 * BAD for the sounding records over which the rapid temperature increase 
	 * occured.
	 * @return The bad strat rapid temperature increase threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#rapidTemperatureCheck(List, Double, Double, java.io.PrintWriter)
	 */
	public Double getBadStratRapidTemperatureIncrease() {
		return badStratRapidTemperatureIncrease;
	}
	
	/**
	 * Get the lapse rate threshold, for sounding records within 10.0 mb of the
	 * surface record, such that any calculated lapse rate that is less than or 
	 * equal to the threshold is considered BAD for the sounding records over 
	 * which the laspe rate was calculated.
	 * @return The surface bad lapse rate threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#superAdiabaticLapseRateCheck(List, Double, Double, java.io.PrintWriter)
	 */
	public Double getBadSurfaceLapseRate() {
		return badSurfaceLapseRate;
	}
	
	/**
	 * Get the surface rapid temperature increase threshold such that any
	 * temperature change greater than or equal to the threshild is considered
	 * bad for the sounding records over which the rapid temperature increase
	 * occured.
	 * @return The bad surface rapid temperature increase threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#rapidTemperatureCheck(List, Double, Double, java.io.PrintWriter)
	 */
	public Double getBadSurfaceRapidTemperatureIncrease() {
		return badSurfaceRapidTemperatureIncrease;
	}
	
	/**
	 * Get the pressure (in millibars) threshold used by the QC for checking for 
	 * decreasing pressure in the sounding where the QC performs different
	 * checks for low pressure values.
	 * @return The pressure threshold used in checking for decreasing pressure.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#decreasingPressureCheck(dmg.ua.sounding.esc.ESCSoundingRecord, dmg.ua.sounding.esc.ESCSoundingRecord, java.io.PrintWriter)
	 */
	public Double getDecreasingPressureCheck() {
		return decreasingPressureCheck;
	}
	
	/**
	 * Get the threshold for the difference in the super adiabatic lapse rate
	 * such that the lapse rate differences over the theshold are at least
	 * questionable.
	 * @return The lapse rate difference threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#superAdiabaticLapseRateCheck(List, Double, Double, java.io.PrintWriter)
	 */
	public Double getLapseRateDifference() {
		return lapseRateDifference;
	}
	
	/**
	 * Get the threshold for the super adiabatic lapse rate such that a lapse
	 * rate calculation over the threshold will be at least QUESTIONABLE.
	 * @return The lapse rate limit threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#superAdiabaticLapseRateCheck(List, Double, Double, java.io.PrintWriter)
	 */
	public Double getLapseRateLimit() {
		return lapseRateLimit;
	}
	
	/**
	 * Get the wind speed threshold such that any wind speed value that is
	 * greater than or equal to the threshold is considered BAD in the sounding
	 * record.
	 * @return The bad wind speed threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#qcWindSpeedValue(dmg.ua.sounding.esc.ESCSoundingRecord, java.io.PrintWriter, int)
	 */
	public Double getMaximumBadWindSpeed() {
		return maxBadWindSpeed;
	}
	
	/**
	 * Get the dew point threshold such that any dew point value that is greater
	 * than or equal to the threshold is replaced with the missing value in the
	 * sounding record.
	 * @return The missing dew point threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#qcDewPointValue(dmg.ua.sounding.esc.ESCSoundingRecord, java.io.PrintWriter, int)
	 */
	public Double getMaximumMissingDewPoint() {
		return maxMissingDewPoint;
	}
	
	/**
	 * Get the pressure threshold such that any pressure value that is greater
	 * than or equal to the threshold is replaced with the missing value in the
	 * sounding record.
	 * @return The missing pressure threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#qcPressureValue(dmg.ua.sounding.esc.ESCSoundingRecord, java.io.PrintWriter, int)
	 */
	public Double getMaximumMissingPressure() {
		return maxMissingPressure;
	}
	
	/**
	 * Get the temperature threshold such that any temperature value that is
	 * greater than or equal to the threshold is replaced with the missing value
	 * in the sounding record.
	 * @return The missing temperature threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#qcTemperatureValue(dmg.ua.sounding.esc.ESCSoundingRecord, java.io.PrintWriter, int)
	 */
	public Double getMaximumMissingTemperature() {
		return maxMissingTemperature;
	}
	
	/**
	 * Get the wind speed threshold such that any wind speed value that is
	 * greater than or equal to the threshold is replaced with the missing value
	 * in the sounding record.
	 * @return The missing wind speed threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#qcWindSpeedValue(dmg.ua.sounding.esc.ESCSoundingRecord, java.io.PrintWriter, int)
	 */
	public Double getMaximumMissingWindSpeed() {
		return maxMissingWindSpeed;
	}
	
	/**
	 * Get the altitude threshold such that any altitude value that is greater 
	 * than this altitude value is considered QUESTIONABLE for the sounding 
	 * record.
	 * @return The questionable altitude threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#qcAltitudeValue(dmg.ua.sounding.esc.ESCSoundingRecord, java.io.PrintWriter, int)
	 */
	public Double getMaximumQuestionableAltitude() {
		return maxQuestionableAltitude;
	}
	
	/**
	 * Get the ascent rate threshold such that the absolute value of the ascent 
	 * rate value that is greater than this ascent rate is considered 
	 * QUESTIONABLE for the sounding record. 
	 * @return The questionable ascent rate threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#qcAscentRateValue(dmg.ua.sounding.esc.ESCSoundingRecord, java.io.PrintWriter, int)
	 */
	public Double getMaximumQuestionableAscentRate() {
		return maxQuestionableAscentRate;
	}
	
	/**
	 * Get the dew point threshold such that any dew point value that is greater
	 * than the threshold is considered QUESTIONABLE for the sounding record.
	 * @return The questionable dew point threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#qcDewPointValue(dmg.ua.sounding.esc.ESCSoundingRecord, java.io.PrintWriter, int)
	 */
	public Double getMaximumQuestionableDewPoint() {
		return maxQuestionableDewPoint;
	}
	
	/**
	 * Get the pressure threshold such that any pressure value that is greater
	 * than the threshold is considered QUESTIONABLE for the sounding record.
	 * @return The questionable pressure threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#qcPressureValue(dmg.ua.sounding.esc.ESCSoundingRecord, java.io.PrintWriter, int)
	 */
	public Double getMaximumQuestionablePressure() {
		return maxQuestionablePressure;
	}
	
	/**
	 * Get the temperature threshold such that any temperature value that is 
	 * greater than the threshold is considered QUESTIONABLE for the sounding 
	 * record.
	 * @return The questionable temperature threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#qcTemperatureValue(dmg.ua.sounding.esc.ESCSoundingRecord, java.io.PrintWriter, int)
	 */
	public Double getMaximumQuestionableTemperature() {
		return maxQuestionableTemperature;
	}
	
	/**
	 * Get the wind speed threshold such that any wind speed value that is
	 * greater than the threshold is considered QUESTIONABLE for the sounding
	 * record.
	 * @return The questionable wind speed threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#qcWindSpeedValue(dmg.ua.sounding.esc.ESCSoundingRecord, java.io.PrintWriter, int)
	 */
	public Double getMaximumQuestionableWindSpeed() {
		return maxQuestionableWindSpeed;
	}
	
	/**
	 * Get the altitude threshold such that any altitude value that is less than
	 * this altitude value is considered QUESTIONABLE for the sounding record.
	 * @return The questionable altitude threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#qcAltitudeValue(dmg.ua.sounding.esc.ESCSoundingRecord, java.io.PrintWriter, int)
	 */
	public Double getMinimumQuestionableAltitude() {
		return minQuestionableAltitude;
	}
	
	/**
	 * Get the dew point threshold such that any dew point value that is less 
	 * than or equal to this value is considered QUESTIONABLE for the sounding 
	 * record.
	 * @return The questionable dew point threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#qcDewPointField(dmg.ua.sounding.esc.ESCSoundingRecord, java.io.PrintWriter)
	 */
	public Double getMinimumQuestionableDewPoint() {
		return minQuestionableDewPoint;
	}
	
	/**
	 * Get the pressure threshold such that any pressure value that is less
	 * than or equal to the threshold is considered QUESTIONABLE for the 
	 * sounding record.
	 * @return The questionable pressure threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#qcPressureValue(dmg.ua.sounding.esc.ESCSoundingRecord, java.io.PrintWriter, int)
	 */
	public Double getMinimumQuestionablePressure() {
		return minQuestionablePressure;
	}
	
	/**
	 * Get the temperature threshold such that any temperature value that is 
	 * less than or equal to the threshold is considered QUESTIONABLE for the 
	 * sounding record.
	 * @return The questionable temperature threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#qcTemperatureValue(dmg.ua.sounding.esc.ESCSoundingRecord, java.io.PrintWriter, int)
	 */
	public Double getMinimumQuestionableTemperature() {
		return minQuestionableTemperature;
	}
	
	/**
	 * Get the wind speed threshold such that any wind speed value that is
	 * less than the threshold is considered QUESTIONABLE for the sounding
	 * record.
	 * @return The questionable wind speed threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#qcWindSpeedValue(dmg.ua.sounding.esc.ESCSoundingRecord, java.io.PrintWriter, int)
	 */
	public Double getMinimumQuestionableWindSpeed() {
		return minQuestionableWindSpeed;
	}
	
	/**
	 * Get the pressure limit threshold used by the rapid temperature increase
	 * checker to determine which rapid temperature thresholds are valid for
	 * a set of sounding records.
	 * @return The pressure limit threshold.
	 */
	public Double getPressureLimit() {
		return pressureLimit;
	}
	
	/**
	 * Get the ascent rate change threshold such that any ascent rate change 
	 * that is greater than or equal to the value is considered QUESTIONABLE for
	 * the sounding records over the ascent rate interval.
	 * @return The difference in ascent rate threshold in m/s.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#differentialAscensionRate(List, Double, boolean, java.io.PrintWriter)
	 */
	public Double getQuestionableAscentRateChange() {
		return questionableAscentRateChange;
	}
	
	/**
	 * Get the lapse rate threshold, for sounding records over or equal to 10.0
	 * mb from the surface record, such that any calculated lapse rate that is
	 * less than or equal to the threshold is considered QUESTIONABLE for the 
	 * sounding records over which the laspe rate was calculated.
	 * @return The non-surface questionable lapse rate threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#superAdiabaticLapseRateCheck(List, Double, Double, java.io.PrintWriter)
	 */
	public Double getQuestionableLapseRate() {
		return questionableLapseRate;
	}
	
	/**
	 * Get the rapid pressure increase threshold such that any pressure change
	 * greater than or equal to the threshold is considered QUESTIONABLE for the
	 * sounding records over which the rapid pressure increase occured.
	 * @return The questionable rapid pressure increase threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#rapidPressureCheck(List, java.io.PrintWriter)
	 */
	public Double getQuestionableRapidPressureIncrease() {
		return questionableRapidPressureIncrease;
	}
	
	/**
	 * Get the lapse rate threshold, for sounding records within 10.0 mb of the
	 * surface record, such that any calculated lapse rate that is less than or 
	 * equal to the threshold is considered QUESTIONABLE for the sounding 
	 * records over which the laspe rate was calculated.
	 * @return The surface questionable lapse rate threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#superAdiabaticLapseRateCheck(List, Double, Double, java.io.PrintWriter)
	 */
	public Double getQuestionableSurfaceLapseRate() {
		return questionableSurfaceLapseRate;
	}
	
	/**
	 * Get the rapid temperature increase threshold such that any temperature
	 * change greater than or equal to the threshold is considered QUETIONABLE
	 * for the sounding records over which the rapid temperature increase 
	 * occured.
	 * @return The rapid temperature increase threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#rapidTemperatureCheck(List, Double, Double, java.io.PrintWriter)
	 */
	public Double getRapidTemperatureIncrease() {
		return rapidTemperatureIncrease;
	}
	
	/**
	 * Get the strat rapid temperature increase threshold such that any 
	 * temperature change greater than or equal to the threshold is considered 
	 * QUETIONABLE for the sounding records over which the rapid temperature 
	 * increase occured.
	 * @return The strat rapid temperature increase threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#rapidTemperatureCheck(List, Double, Double, java.io.PrintWriter)
	 */
	public Double getStratRapidTemperatureIncrease() {
		return stratRapidTemperatureIncrease;
	}
	
	/**
	 * Get the surface pressure limit threshold used by the rapid temperature 
	 * increase checker to determine which rapid temperature thresholds are 
	 * valid for a set of sounding records.
	 * @return The surface pressure limit threshold.
	 */
	public Double getSurfacePressureLimit() {
		return surfacePressureLimit;
	}
	
	/**
	 * Get the surface rapid temperature increase threshold such that any 
	 * temperature change greater than or equal to the threshold is considered 
	 * QUETIONABLE for the sounding records over which the rapid temperature 
	 * increase occured.
	 * @return The surface rapid temperature increase threshold.
	 * @see dmg.ua.sounding.autoqc.ESCAutoQC#rapidTemperatureCheck(List, Double, Double, java.io.PrintWriter)
	 */
	public Double getSurfaceRapidTemperatureIncrease() {
		return surfaceRapidTemperatureIncrease;
	}
	
	/**
	 * Determine if the latitude type for the QC is considered a HOT or COLD
	 * latitude.
	 * @return <code>true</code> if the QC is being run on HOT latitude
	 * soundings, <code>false</code> if it is being run on COLD latitude
	 * soundings.
	 */
	public boolean isHotLatitude() {
		return hotLatitude;
	}
	
	/**
	 * Pull out the specified key from the ResourceBundle.
	 * @param limits The ResourceBundle containing the value for the key.
	 * @param key The ResourceBundle key to be parsed from the ResourceBundle.
	 * @return The value assigned to the key converted to a Double value.
	 * @throws QCException when there is a problem parsing the value into
	 * a Double.
	 */
	private Double parse(ResourceBundle limits, String key) throws QCException {
		try {
			return Double.parseDouble(limits.getString(key));
		} catch (NumberFormatException e) {
			throw new QCException(
					String.format("Key %s has an improper Double format value" +
							" (%s).", key, limits.getString(key)));
		}
	}
}
