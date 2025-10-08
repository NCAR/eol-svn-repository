package dmg.ua.sounding;

import dmg.record.*;
import dmg.util.*;
import dmg.util.VelocityUtils.*;

/**
 * <p>The SoundingWindData class is an extension of the WindData class
 * that defines the VelocityUnit to be in meters per second.</p>
 *
 * @author Joel Clawson
 */
public abstract class SoundingWindData extends WindData {

    /**
     * Create anew instance of a SoundingWindData.
     * @param calculationsAllowed A flag that prevents values from being
     * calculated.
     **/
    public SoundingWindData(boolean calculationsAllowed) {
	super(calculationsAllowed);
    }
   
    /**
     * Get the default velocity unit for soundings.
     * @return The VelocityUnit for m/s.
     * @see dmg.util.VelocityUtils#METERS_PER_SECOND
     **/
    @Override public VelocityUnit getDefaultVelocityUnit() { 
	return VelocityUtils.METERS_PER_SECOND;
    }
}
