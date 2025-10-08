package dmg.ua.sounding;

import dmg.record.*;
import dmg.util.*;
import dmg.util.LengthUtils.*;
import dmg.util.PressureUtils.*;
import dmg.util.TemperatureUtils.*;

/**
 * The SoundingPTHData is an extension of the PTHData class that defines the 
 * default units for Sounding data.
 * 
 * @author Joel Clawson
 */
public abstract class SoundingPTHData extends PTHData {

    /**
     * Create a new instance of a SoundingPTHData.
     * @param calculationsAllowed A flag that can prevent automatically calculating
     * values in the data block.
     **/
    public SoundingPTHData(boolean calculationsAllowed) {
	super(calculationsAllowed);
    }
    
    /**
     * Get the default length unit for soundings.
     * @return The LengthUnit for meters.
     * @see dmg.util.LengthUtils#METERS
     **/
    @Override public LengthUnit getDefaultLengthUnit() { return LengthUtils.METERS; }
    
    /**
     * Get the default pressure unit for soundings.
     * @return The PressureUnit for millibars.
     * @see dmg.util.PressureUtils#MILLIBARS
     **/
    @Override public PressureUnit getDefaultPressureUnit() { 
	return PressureUtils.MILLIBARS;
    }
    
    /**
     * Get the default temperature unit for soundings.
     * @return The TemperatureUnit for celcius.
     * @see dmg.util.TemperatureUtils#CELCIUS
     **/
    @Override public TemperatureUnit getDefaultTemperatureUnit() { 
	return TemperatureUtils.CELCIUS;
    }
}
