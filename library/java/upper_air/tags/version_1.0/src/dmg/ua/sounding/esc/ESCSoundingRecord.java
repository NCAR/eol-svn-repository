package dmg.ua.sounding.esc;

import static dmg.util.LengthUtils.*;
import static dmg.util.PressureUtils.*;
import static dmg.util.TemperatureUtils.*;
import static dmg.util.VelocityUtils.*;
import dmg.ua.sounding.*;
import dmg.util.*;
import dmg.util.LengthUtils.*;
import dmg.util.PressureUtils.*;
import dmg.util.TemperatureUtils.*;
import dmg.util.VelocityUtils.*;

import java.io.*;
import java.util.*;

/**
 * <p>The ESCSoundingRecord is a SoundingRecord extension for the EOL Sounding
 * Composite (ESC) formatted sounding.  It adds additional parameters, 
 * specificially flags for some of the values.  The setters are also expanded to
 * handle the missing values defined for the ESC format along with limiting the 
 * values to prevent the format from being violated.</p>
 *
 * @author Joel Clawson
 */
public class ESCSoundingRecord extends SoundingRecord {

    /**
     * The constant that represents the ESC Bad flag.
     */
    public static final ESCFlag BAD_FLAG = (new ESCSoundingRecord()).new ESCFlag(3.0);

    /**
     * The constant that represents the ESC Estimate flag.
     */
    public static final ESCFlag ESTIMATE_FLAG = (new ESCSoundingRecord()).new ESCFlag(4.0);

    /**
     * The constant that represents the ESC Good flag.
     */
    public static final ESCFlag GOOD_FLAG = (new ESCSoundingRecord()).new ESCFlag(1.0);

    /**
     * The constant that represents the ESC Missing flag.
     */
    public static final ESCFlag MISSING_FLAG = (new ESCSoundingRecord()).new ESCFlag(9.0);

    /**
     * The constant that represents the ESC Questionable/Dubious flag.
     */
    public static final ESCFlag QUESTIONABLE_FLAG = (new ESCSoundingRecord()).new ESCFlag(2.0);

    /**
     * The constant that represents the ESC Unchecked flag.
     */
    public static final ESCFlag UNCHECKED_FLAG = (new ESCSoundingRecord()).new ESCFlag(99.0);
	
    private static final List<ESCFlag> PRECEDENCE = 
	new ArrayList<ESCFlag>(Arrays.asList(new ESCFlag[] 
	    {GOOD_FLAG, ESTIMATE_FLAG, QUESTIONABLE_FLAG, BAD_FLAG, MISSING_FLAG, 
	     UNCHECKED_FLAG}));
    
    
    private Double var1, var2;
    private ESCFlag ascentRateFlag, pressureFlag, relativeHumidityFlag, temperatureFlag, uCompFlag, vCompFlag;
    
    /**
     * Create a new instance of an ESCSoundingRecord.
     */
    public ESCSoundingRecord() { this(true, null); }
    
    /**
     * Create a new instance of an ESCSoundingRecord.
     * @param previousRecord The record directly previous to this record.
     */
    public ESCSoundingRecord(ESCSoundingRecord previousRecord) {
	this(true, previousRecord);
    }

    /**
     * Create a new instance of an ESCSoundingRecord.
     * @param calculationsAllowed A flag to allow/prevent values from automatically
     * being calculated.
     */
    public ESCSoundingRecord(boolean calculationsAllowed) {
	this(calculationsAllowed, null);
    }

    /**
     * Create a new instance of an ESCSoundingRecord.
     * @param calculationsAllowed A flag to allow/prevent values from automatically
     * being calculated.
     * @param previousRecord The record directory previous to this record.
     **/
    public ESCSoundingRecord(boolean calculationsAllowed, ESCSoundingRecord previousRecord) {
	super(new ESCSoundingPTHData(calculationsAllowed),
	      new ESCSoundingWindData(calculationsAllowed),
	      calculationsAllowed, previousRecord);
    }

    /**
     * Create a new instance of an ESCSoundingRecord.
     * @param line The String to be parsed that contains the record data.
     * @param recordNumber The number of the record in the sounding.
     * @throws IOException if there is a problem parsing the data line.
     **/
    public ESCSoundingRecord(String line, int recordNumber) throws IOException {
	this(line, true, recordNumber);
    }
    
    /**
     * Create a new instance of an ESCSoundingRecord.
     * @param line The record in an ESC formatted String.
     * @param calculationsAllowed A flag to allow/prevent values from automatically
     * being calculated.
     * @param recordNumber The number of the record in the sounding.
     * @throws IOException if the line is not in the ESC format, flags do not 
     * match their values, or a values does not fit within the format.
     */
    public ESCSoundingRecord(String line, boolean calculationsAllowed, int recordNumber) 
	throws IOException
    {
	this(calculationsAllowed);		
	try {
	    StringTokenizer tokenizer = 
		new StringTokenizer(line.trim().replaceAll("\\s+", " "), " ");
	    
	    // Make sure there are the correct number of tokens before trying to 
	    // assign the values.
	    if (tokenizer.countTokens() != 21) {
		throw new IOException("The record line does not have 21 " +
				      "data points.");
	    }
	    
	    try { setTime(Double.parseDouble(tokenizer.nextToken())); }
	    catch (CalculationWarning e) {}
	    try {
		setPressure(Double.parseDouble(tokenizer.nextToken()), 
			    MILLIBARS);
	    } catch (CalculationWarning e) {}
	    try { 
		setTemperature(Double.parseDouble(tokenizer.nextToken()), 
			       CELCIUS);
	    } catch (CalculationWarning e) {}
	    try { 
		setDewPoint(Double.parseDouble(tokenizer.nextToken()), CELCIUS);
	    } catch (CalculationWarning e) {}
	    try { 
		setRelativeHumidity(Double.parseDouble(tokenizer.nextToken()));
	    } catch (CalculationWarning e) {}
	    try { 
		setUComponent(Double.parseDouble(tokenizer.nextToken()),
			      METERS_PER_SECOND);
	    } catch (CalculationWarning e) {}
	    try { 
		setVComponent(Double.parseDouble(tokenizer.nextToken()), 
			      METERS_PER_SECOND);
	    } catch (CalculationWarning e) {}
	    try { 
		setWindSpeed(Double.parseDouble(tokenizer.nextToken()), 
			     METERS_PER_SECOND); 
	    } catch (CalculationWarning e) {}
	    try { setWindDirection(Double.parseDouble(tokenizer.nextToken())); }
	    catch (CalculationWarning e) {}
	    try {
		setAscentRate(Double.parseDouble(tokenizer.nextToken()), 
			      METERS_PER_SECOND);
	    } catch (CalculationWarning e) {}
	    setLongitude(Double.parseDouble(tokenizer.nextToken()));
	    setLatitude(Double.parseDouble(tokenizer.nextToken()));
	    setVariableField1(Double.parseDouble(tokenizer.nextToken()));
	    setVariableField2(Double.parseDouble(tokenizer.nextToken()));
	    try { 
		setAltitude(Double.parseDouble(tokenizer.nextToken()), METERS);
	    } catch (CalculationWarning e) {}
	    setPressureFlag(findFlag(
				     Double.parseDouble(tokenizer.nextToken())));
	    setTemperatureFlag(findFlag(
					Double.parseDouble(tokenizer.nextToken())));
	    setRelativeHumidityFlag(findFlag(
					     Double.parseDouble(tokenizer.nextToken())));
	    setUComponentFlag(findFlag(
				       Double.parseDouble(tokenizer.nextToken())));
	    setVComponentFlag(findFlag(
				       Double.parseDouble(tokenizer.nextToken())));
	    setAscentRateFlag(findFlag(
				       Double.parseDouble(tokenizer.nextToken())));
	} catch (DefaultException e) {
	    // Convert any of the default exceptions into an IOException.  This 
	    // is because any problems that arise are caused by the format of 
	    // the line or an illegal flag.
	    throw new IOException(String.format("Record %d:  %s",
						recordNumber, e.getMessage()));
	} catch (DefaultWarning e) {
	    throw new IOException(String.format("Record %d:  %s",
						recordNumber, e.getMessage()));
	}
    }
    
    /**
     * Determine if the specified value and flag pair match.
     * @param value The value to match with the flag.
     * @param flag The flag being assigned.
     * @return The flag if can be assigned.
     * @throws InvalidFlagException if the value is <code>null</code> with a
     * non-missing flag or if the value is not <code>null</code> and the flag
     * is missing.
     */
    private ESCFlag assignFlag(Double value, ESCFlag flag) throws InvalidFlagException {
	// Don't care what the value is if the flag is null 
	// (unassigning the flag)
	if (flag == null) { return flag; }
	
	// Make sure a missing flag matches a missing/null value
	if (value == null && !flag.equals(MISSING_FLAG)) {
	    throw new InvalidFlagException("assignFlag", value, flag.getValue(),
					   "The flag is not missing for the missing value.");
	} else if (value != null && flag.equals(MISSING_FLAG)) {
	    throw new InvalidFlagException("assignFlag", value, flag.getValue(),
					   "The flag is missing for a non-missing value.");
	}
	
	return flag;
    }
    
    /**
     * Determine which flag should be returned from the specified value and flag pair.
     * @param value The value to use to determine the flag.
     * @param flag The current value of the flag being returned.
     * @return The flag if set, or the UNCHECKED_FLAG if the value is not 
     * <code>null</code> or the MISSING_FLAG if the value is <code>null</code>.
     */
    private ESCFlag determineFlag(Double value, ESCFlag flag) {
	if (flag == null) { 
	    return value == null ? MISSING_FLAG : UNCHECKED_FLAG;
	} else { return flag; }
    }
    
    /**
     * Find the ESCFlag that represents the specified value.
     * @param value The value to be represented by an ESCFlag.
     * @return The ESCFlag represented by the value.
     * @throws InvalidArgumentException if the value does not corresponed to a
     * known ESCFlag.
     */
    private ESCFlag findFlag(Double value) throws InvalidFlagException {
	if (value == null) { return null; }
	else if (value.equals(BAD_FLAG.getValue())) { return BAD_FLAG; }
	else if (value.equals(ESTIMATE_FLAG.getValue())) { 
	    return ESTIMATE_FLAG;
	} else if (value.equals(GOOD_FLAG.getValue())) { return GOOD_FLAG; }
	else if (value.equals(MISSING_FLAG.getValue())) { return MISSING_FLAG; }
	else if (value.equals(QUESTIONABLE_FLAG.getValue())) { 
	    return QUESTIONABLE_FLAG;
	} else if (value.equals(UNCHECKED_FLAG.getValue())) { 
	    return UNCHECKED_FLAG;
	} else {
	    throw new InvalidFlagException("findFlag", value, null,
					   String.format("The flag of %.1f is not known.", value));
	}
    }
    
    /**
     * Get the flag for the ascent rate.  The default value is the 
     * UNCHECKED_FLAG if there is an ascent rate value or the MISSING_FLAG if 
     * the ascent rate is value is <code>null</code>.
     * @return The ascent rate flag.
     */
    public ESCFlag getAscentRateFlag() { 
	return determineFlag(getAscentRate(), ascentRateFlag);
    }
    
    /**
     * Get the flag for the pressure.  The default value is the UNCHECKED_FLAG 
     * if there is a pressure value or the MISSING_FLAG if the pressure is value
     * is <code>null</code>.
     * @return The pressure flag.
     */
    public ESCFlag getPressureFlag() {
	return determineFlag(getPressure(), pressureFlag);
    }
    
    /**
     * Get the flag for the relative humidity.  The default values is the
     * UNCHECKED_FLAG if there is a relative humidity value or the MISSING_FLAG
     * if the relative humidity value is <code>null</code>.
     * @return The relative humidity flag.
     */
    public ESCFlag getRelativeHumidityFlag() {
	return determineFlag(getRelativeHumidity(), relativeHumidityFlag);
    }
    
    /**
     * Get the flag for the temperature.  The default value is the 
     * UNCHECKED_FLAG if there is a temperature value or the MISSING_FLAG if 
     * the temperature value is <code>null</code>.
     * @return The temperature flag.
     */
    public ESCFlag getTemperatureFlag() {
	return determineFlag(getTemperature(), temperatureFlag);
    }
    
    /**
     * Get the flag for the u component.  The default value is the 
     * UNCHECKED_FLAG if there is a u component value or the MISSING_FLAG if 
     * the u component value is <code>null</code>.
     * @return The u component flag.
     */
    public ESCFlag getUComponentFlag() {
	return determineFlag(getUComponent(), uCompFlag);
    }
    
    /**
     * Get the value of the first variable field of the record.  The default 
     * value is <code>null</code>.
     * @return The first variable field value.
     */
    public Double getVariableField1() { return var1; }
    
    /**
     * Get the value of the second variable field of the record.  The default 
     * value is <code>null</code>.
     * @return The second variable field value.
     */
    public Double getVariableField2() { return var2; }
    
    /**
     * Get the flag for the v component.  The default value is the 
     * UNCHECKED_FLAG if there is a v component value or the MISSING_FLAG if 
     * the v component value is <code>null</code>.
     * @return The v component flag.
     */
    public ESCFlag getVComponentFlag() {
	return determineFlag(getVComponent(), vCompFlag);
    }
    
    /* (non-Javadoc)
     * When setting the altitude, convert the missing value of 99999.0 to null
     * before actually trying to set the value.
     * @see dmg.ua.sounding.SoundingRecord#setAltitude(java.lang.Double, dmg.util.LengthUtils.LengthUnit)
     */
    @Override public void setAltitude(Double altitude, LengthUnit unit) 
	throws CalculationWarning, ConversionException, InvalidValueWarning {
	super.setAltitude(altitude == null || altitude == 99999.0 ? null : altitude, unit);
    }
    
    /* (non-Javadoc)
     * When setting the ascent rate, convert the missing value of 999.0 to null
     * before actually trying to set the value.
     * @see dmg.ua.sounding.SoundingRecord#setAscentRate(java.lang.Double, dmg.util.VelocityUtils.VelocityUnit)
     */
    @Override public void setAscentRate(Double rate, VelocityUnit unit) 
	throws CalculationWarning, ConversionException, InvalidValueWarning {
	super.setAscentRate(rate == null || rate == 999.0 ? null : rate, unit);
    }
    
    /**
     * Set the flag for the ascent rate.
     * @param flag The ascent rate flag.
     * @throws InvalidFlagException if the flag and ascent rate value do not 
     * match.
     */
    public void setAscentRateFlag(ESCFlag flag) throws InvalidFlagException {
	ascentRateFlag = assignFlag(getAscentRate(), flag);
    }
    
    /* (non-Javadoc)
     * When setting the dew point, convert the missing value of 999.0 to null
     * before actually trying to set the value.
     * @see dmg.ua.sounding.SoundingRecord#setDewPoint(java.lang.Double, dmg.util.TemperatureUtils.TemperatureUnit)
     */
    @Override public void setDewPoint(Double dewPoint, TemperatureUnit unit)
	throws CalculationWarning, ConversionException, InvalidValueWarning  {
	super.setDewPoint(dewPoint == null || dewPoint == 999.0 ? null : dewPoint, unit);
    }
    
    /* (non-Javadoc)
     * @see dmg.ua.sounding.SoundingRecord#setLatitude(java.lang.Double)
     */
    @Override public void setLatitude(Double degrees) throws InvalidValueWarning {
	super.setLatitude(degrees == null || degrees == 999.0 ? null : degrees);
    }
    
    /* (non-Javadoc)
     * @see dmg.ua.sounding.SoundingRecord#setLongitude(java.lang.Double)
     */
    @Override public void setLongitude(Double degrees) throws InvalidValueWarning {
	super.setLongitude(degrees == null || degrees == 9999.0 ? null : degrees);
    }
    
    /* (non-Javadoc)
     * When setting the pressure, convert the missing value of 9999.0 to null
     * before actually trying to set the value.
     * @see dmg.ua.sounding.SoundingRecord#setPressure(java.lang.Double, dmg.util.PressureUtils.PressureUnit)
     */
    @Override public void setPressure(Double pressure, PressureUnit unit) 
	throws CalculationWarning, ConversionException, InvalidValueWarning
    {
	super.setPressure(pressure == null || pressure == 9999.0 ? null : pressure, unit);
    }
    
    /**
     * Set the flag for the pressure.
     * @param flag The pressure flag.
     * @throws InvalidFlagException if the flag and pressure value do not match.
     */
    public void setPressureFlag(ESCFlag flag) throws InvalidFlagException {
	pressureFlag = assignFlag(getPressure(), flag);
    }
    
    /* (non-Javadoc)
     * When setting the relative humidity, convert the missing value of 999.0 to
     * null before actually trying to set the value.
     * @see dmg.ua.sounding.SoundingRecord#setRelativeHumidity(java.lang.Double)
     */
    @Override public void setRelativeHumidity(Double rh) 
	throws CalculationWarning, InvalidValueWarning 
    {
	super.setRelativeHumidity(rh == null || rh == 999.0 ? null : rh);
    }
	
    /**
     * Set the flag for the relative humidity.
     * @param flag The relative humidity flag.
     * @throws InvalidFlagException if the flag and relative humidity value do 
     * not match.
     */
    public void setRelativeHumidityFlag(ESCFlag flag) throws InvalidFlagException {
	relativeHumidityFlag = assignFlag(getRelativeHumidity(), flag);
    }
    
    /* (non-Javadoc)
     * When setting the temperature, convert the missing value of 999.0 to null
     * before actually trying to set the value.
     * @see dmg.ua.sounding.SoundingRecord#setTemperature(java.lang.Double, dmg.util.TemperatureUtils.TemperatureUnit)
     */
    @Override public void setTemperature(Double temperature, TemperatureUnit unit) 
	throws CalculationWarning, ConversionException, InvalidValueWarning  {
	super.setTemperature(temperature == null || temperature == 999.0 ? 
			     null : temperature, unit);
    }
    
    /**
     * Set the flag for the temperature.
     * @param flag The temperature flag.
     * @throws InvalidFlagException if the flag and temperature value do not match.
     */
    public void setTemperatureFlag(ESCFlag flag) throws InvalidFlagException {
	temperatureFlag = assignFlag(getTemperature(), flag);
    }
    
    /* (non-Javadoc)
     * When setting the time, convert the missing value of 9999.0 to null before
     * actually trying to set the value.
     * @see dmg.ua.sounding.SoundingRecord#setTime(java.lang.Double)
     */
    @Override public void setTime(Double time) throws CalculationWarning, InvalidValueWarning  {
	super.setTime(time == null || time == 9999.0 ? null : time);
    }
    
    /* (non-Javadoc)
     * When setting the u component, convert the missing value of 9999.0 to null
     * before actually trying to set the value.
     * @see dmg.ua.sounding.SoundingRecord#setUComponent(java.lang.Double, dmg.util.VelocityUtils.VelocityUnit)
     */
    @Override public void setUComponent(Double uComponent, VelocityUnit unit) 
	throws CalculationWarning, ConversionException, InvalidValueWarning  
    {
	super.setUComponent(uComponent == null || uComponent == 9999.0 ? 
			    null : uComponent, unit);
    }
    
    /**
     * Set the flag for the u wind component.
     * @param flag The u wind component flag.
     * @throws InvalidFlagException if the flag and u component value do not match.
     */
    public void setUComponentFlag(ESCFlag flag) throws InvalidFlagException {
	uCompFlag = assignFlag(getUComponent(), flag);
    }
    
    /**
     * Set the value of the first variable field.
     * @param value The value of the field.
     * @throws InvalidValueException if the variable value is not between 
     * -99.9 and 999.9.
     */
    public void setVariableField1(Double value) throws InvalidValueException {
	if (value != null && (value < -99.9 || 999.9 < value)) {
	    throw new InvalidValueException("variable 1", value, -99.9, 999.9);
	}
	var1 = value == null || value == 999.0 ? null : value;
    }
    
    /**
     * Set the value of the second variable field.
     * @param value The value of the field.
     * @throws InvalidValueException if the variable value is not between -99.9 and 999.9.
     */
    public void setVariableField2(Double value) throws InvalidValueException {
	if (value != null && (value < -99.9 || 999.9 < value)) {
	    throw new InvalidValueException("variable 1", value, -99.9, 999.9);
	}
	var2 = value == null || value == 999.0 ? null : value;
    }
    
    /* (non-Javadoc)
     * When setting the v component, convert the missing value of 9999.0 to null
     * before actually trying to set the value.
     * @see dmg.ua.sounding.SoundingRecord#setVComponent(java.lang.Double, dmg.util.VelocityUtils.VelocityUnit)
     */
    @Override public void setVComponent(Double vComponent, VelocityUnit unit) 
	throws CalculationWarning, ConversionException, InvalidValueWarning 
    {
	super.setVComponent(vComponent == null || vComponent == 9999.0 ? 
			    null : vComponent, unit);
    }
    
    /**
     * Set the flag for the v wind component.
     * @param flag The v wind component flag.
     * @throws InvalidFlagException if the flag and v component value do not match.
     */
    public void setVComponentFlag(ESCFlag flag) throws InvalidFlagException {
	vCompFlag = assignFlag(getVComponent(), flag);
    }
    
    /* (non-Javadoc)
     * When setting the wind direction, convert the missing value of 999.0 to 
     * null before actually trying to set the value.
     * @see dmg.ua.sounding.SoundingRecord#setWindDirection(java.lang.Double)
     */
    @Override public void setWindDirection(Double direction) 
	throws CalculationWarning, InvalidValueWarning
    {
	super.setWindDirection(direction == null || direction == 999.0 ? null : direction);
    }
    
    /* (non-Javadoc)
     * When setting the wind speed, convert the missing value of 999.0 to null
     * before actually trying to set the value.
     * @see dmg.ua.sounding.SoundingRecord#setWindSpeed(java.lang.Double, dmg.util.VelocityUtils.VelocityUnit)
     */
    @Override public void setWindSpeed(Double speed, VelocityUnit unit)
	throws CalculationWarning, ConversionException, InvalidValueWarning 
    {
	super.setWindSpeed(speed == null || speed == 999.0 ? null : speed, unit);
    }
    
    @Override public String toString() {
	return String.format("%6.1f %6.1f %5.1f %5.1f %5.1f %6.1f %6.1f %5.1f %5.1f %5.1f %8.3f %7.3f %5.1f %5.1f %7.1f %4.1f %4.1f %4.1f %4.1f %4.1f %4.1f",
			     getTime() == null ? 9999.0 : (getTime() <= 0.0 && getTime() > -0.05 ? 0.0 : getTime()),
			     getPressure() == null ? 9999.0 : (getPressure() <= 0.0 && getPressure() > -0.05 ? 0.0 : getPressure()),
			     getTemperature() == null ? 999.0 : (getTemperature() <= 0.0 && getTemperature() > -0.05 ? 0.0 : getTemperature()),
			     getDewPoint() == null ? 999.0 : (getDewPoint() <= 0.0 && getDewPoint() > -0.05 ? 0.0 : getDewPoint() < -99.9 ? -99.9 : getDewPoint()),
			     getRelativeHumidity() == null ? 999.0 : (getRelativeHumidity() <= 0.0 && getRelativeHumidity() > -0.05 ? 0.0 : getRelativeHumidity()),
			     getUComponent() == null ? 9999.0 : (getUComponent() <= 0.0 && getUComponent() > -0.05 ? 0.0 : getUComponent()),
			     getVComponent() == null ? 9999.0 : (getVComponent() <= 0.0 && getVComponent() > -0.05 ? 0.0 : getVComponent()),
			     getWindSpeed() == null ? 999.0 : (getWindSpeed() <= 0.0 && getWindSpeed() > -0.05 ? 0.0 : getWindSpeed()),
			     getWindDirection() == null ? 999.0 : (getWindDirection() <= 0.0 && getWindDirection() > -0.05 ? 0.0 : getWindDirection()),
			     getAscentRate() == null ? 999.0 : (getAscentRate() <= 0.0 && getAscentRate() > -0.05 ? 0.0 : getAscentRate()),
			     getLongitude() == null ? 9999.0 : getLongitude(), getLatitude() == null ? 999.0 : getLatitude(), 
			     getVariableField1() == null ? 999.0 : (getVariableField1() <= 0.0 && getVariableField1() > -0.05 ? 0.0 : getVariableField1()),
			     getVariableField2() == null ? 999.0 : (getVariableField2() <= 0.0 && getVariableField2() > -0.05 ? 0.0 : getVariableField2()),
			     getAltitude() == null ? 99999.0 : (getAltitude() <= 0.0 && getAltitude() > -0.05 ? 0.0 : getAltitude()),
			     getPressureFlag().getValue(),
			     getTemperatureFlag().getValue(),
			     getRelativeHumidityFlag().getValue(),
			     getUComponentFlag().getValue(),
			     getVComponentFlag().getValue(),
			     getAscentRateFlag().getValue());
	}

	@Override protected Double validateAscentRate(Double ascentRate) {
	    return (ascentRate == null || ascentRate > 999.94 || ascentRate < -99.94 ? 
		    null : ascentRate);
	}

	@Override protected Double validateTime(Double time) {
	    return (time == null || time > 9999.94 || time < -999.94 ? null : time);
	}

    /**
     * The ESCFlag class is the representation of a flag used in an ESC 
     * formatted sounding record.
     *
     * @author Joel Clawson
     */
    public class ESCFlag implements Comparable<ESCFlag> {
	
	private Double value;
	
	/**
	 * Create a new instance of an ESCFlag.  It is private to only allow
	 * the ESC sounding record create a new instance.
	 * @param value The value of the flag.
	 */
	private ESCFlag(Double value) { this.value = value; }
	
	/**
	 * Compare this flag to the specified flag for precedence order.
	 * @param flag The flag to compare to this flag.
	 * @return A negative integer, zero, or a positive integer if this flag 
	 * has a higher, equal, or lower precedence than the specified flag.
	 */
	public int compareTo(ESCFlag flag) {
	    return (new Integer(PRECEDENCE.indexOf(flag))).compareTo(PRECEDENCE.indexOf(this));
	}
	
	/**
	 * Degrade this flag to the next worst flag.  The BAD_FLAG is the worst
	 * a flag can become.  MISSING and UNCHECKED cannot be degraded.  
	 * GOOD and ESTIMATE flags become QUESTIONABLE and QUESTIONABLE becomes 
	 * bad.
	 * @return The next worst flag from this flag.
	 */
	public ESCFlag degrade() {
	    if (this.isWorseFlag(QUESTIONABLE_FLAG)) { return this; }
	    else if (this.equals(GOOD_FLAG)) { return QUESTIONABLE_FLAG; }
	    else { return PRECEDENCE.get(PRECEDENCE.indexOf(this) + 1); }
	}
	
	/**
	 * Get the value represented by this flag.
	 * @return The value of the ESCFlag.
	 */
	public Double getValue() { return this.value; }
	
	/**
	 * Determine if this flag is worse than the specified flag based on precedence.
	 * @param flag The flag to be tested against this flag.
	 * @return <code>true</code> if this flag is worse than the specified 
	 * flag, <code>false</code> if the specified flag is worse or equals to this flag.
	 */
	public boolean isWorseFlag(ESCFlag flag) {
	    return compareTo(flag) < 0;
	}
	
	@Override public String toString() { return getValue().toString(); }
    }
}
