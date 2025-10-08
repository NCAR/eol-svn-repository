package dmg.ua.sounding.esc;

import dmg.ua.sounding.*;

/**
 * The ESCSoundingWindData class is a SoundingWindData instance that
 * defines the acceptable wind value ranges for ESC formatted data.
 *
 * @author Joel Clawson
 */
public class ESCSoundingWindData extends SoundingWindData {

    /**
     * Create a new instance of an ESCSoundingWindData.
     * @param calculationsAllowed A flag to allow/prevent values from being
     * calculated automatically.
     **/
    public ESCSoundingWindData(boolean calculationsAllowed) {
	super(calculationsAllowed);
    }

    /**
     * Determine if the U component value is too big for the field.
     * @param uComponent The value to be validated.
     * @return The value if it fits in the field or <code>null</code> if it does not.
     **/
    @Override protected Double validateUComponent(Double uComponent) {
	return (uComponent == null || uComponent > 9999.94 || uComponent < -999.94 ? 
		null : uComponent);
    }

    /**
     * Determine if the V component value is too big for the field.
     * @param vComponent The value to be validated.
     * @return The value if it fits in the field or <code>null</code> if it does not.
     **/
    @Override protected Double validateVComponent(Double vComponent) {
	return (vComponent == null || vComponent > 9999.94 || vComponent < -999.94 ? 
		null : vComponent);
    }

    /**
     * Determine if the direction value is too big for the field.
     * @param direction The value to be validated.
     * @return The value if it fits in the field or <code>null</code> if it does not.
     **/
    @Override protected Double validateWindDirection(Double direction) {
	return (direction == null || direction > 999.94 || direction < -99.94 ? 
		null : direction);
    }

    /**
     * Determine if the wind speed value is too big for the field.
     * @param speed The value to be validated.
     * @return The value if it fits in the field or <code>null</code> if it does not.
     **/
    @Override protected Double validateWindSpeed(Double speed) {
	return (speed == null || speed > 999.94 || speed < -99.94 ? null : speed);
    }
}
