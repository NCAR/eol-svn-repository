package dmg.ua.sounding.esc;

import dmg.ua.sounding.*;

/**
 * The ESCSoundingPTHData class is a SoundingPTHData that defines
 * the acceptable ranges for the values in an ESC formatted sounding.
 *
 * @author Joel Clawson
 */
public class ESCSoundingPTHData extends SoundingPTHData {

    /**
     * Create a new instance of an ESCSoundingPTHData.
     * @param calculationsAllowed A flag that allows/prevents values from being
     * calculated within the block.
     **/
    public ESCSoundingPTHData(boolean calculationsAllowed) {
	super(calculationsAllowed);
    }

    /**
     * Determine if the dew point value is too big for the field.
     * @param dewPoint The value to be validated.
     * @return The value if it fits in the field or <code>null</code> if it does not.
     **/
    @Override protected Double validateDewPoint(Double dewPoint) {
	return (dewPoint == null || dewPoint > 999.94 ? null : dewPoint);
    }
    
    /**
     * Determine if the elevation/altitude value is too big for the field.
     * @param elevation The value to be validated.
     * @return The value if it fits in the field or <code>null</code> if it does not.
     **/
    @Override protected Double validateElevation(Double elevation) {
	return (elevation == null || elevation > 99999.94 || elevation < -9999.4 ? 
		null : elevation);
    }
    
    /**
     * Determine if the pressure value is too big for the field.
     * @param pressure The value to be validated.
     * @return The value if it fits in the field or <code>null</code> if it does not.
     **/
    @Override protected Double validatePressure(Double pressure) {
	return (pressure == null || pressure > 9999.94 || pressure < -999.94 ?
		null : pressure);
    }
    
    /**
     * Determine if the relativeh humidity value is too big for the field.
     * @param rh The value to be validated.
     * @return The value if it fits in the field or <code>null</code> if it does not.
     **/
    @Override protected Double validateRelativeHumidity(Double rh) {
	return (rh == null || rh > 999.94 || rh < -99.94 ? null : rh);
    }

    /**
     * Determine if the sea level pressure value is too big for the field.
     * @param slpressure The value to be validated.
     * @return The value if it fits in the field or <code>null</code> if it does not.
     **/
    @Override protected Double validateSeaLevelPressure(Double slpressure) {
	return slpressure;
    }
    
    /**
     * Determine if the temperature value is too big for the field.
     * @param temperature The value to be validated.
     * @return The value if it fits in the field or <code>null</code> if it does not.
     **/
    @Override protected Double validateTemperature(Double temperature) {
	return (temperature == null || temperature > 999.94 || temperature < -99.94 ? 
		null : temperature);
    }
}
