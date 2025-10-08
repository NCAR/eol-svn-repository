package dmg.ua.sounding.esc;

import static dmg.util.PositionUtils.*;
import static java.lang.Math.*;
import dmg.station.*;
import dmg.ua.sounding.*;
import dmg.util.*;
import dmg.util.LengthUtils.*;

/**
 * <p>The ESCSounding class is the representation of a Sounding in the
 * EOL Sounding Composite (ESC) format.  It defines the header and contains
 * the list of ESCSoundingRecords that contain the data for the Sounding.</p>
 *
 * @author Joel Clawson
 */
public class ESCSounding extends Sounding<ESCSoundingRecord> {
    
    private String measurement1, measurement2, unit1, unit2;
    
    /**
     * Create a new instance of an ESCSounding.
     */
    public ESCSounding() { super(); }
    
    /**
     * Create a new instance of an ESCSounding.
     * @param station The Station where the sounding was released.
     */
    public ESCSounding(Station station) { super(station); }
    
    /**
     * Get the name of the first variable measurement.  The default value
     * is <code>null</code>. 
     * @return The name of the first variable measurement.
     */
    public String getVariableMeasurement1() { return measurement1; }
    
    /**
     * Get the name of the second variable measurement.  The default value
     * is <code>null</code>. 
     * @return The name of the second variable measurement.
     */
    public String getVariableMeasurement2() { return measurement2; }
    
    /**
     * Get the name of the first variable unit.  The default value is <code>null</code>. 
     * @return The name of the first variable unit.
     */
    public String getVariableUnit1() { return unit1; }
    
    /**
     * Get the name of the second variable unit.  The default value is <code>null</code>. 
     * @return The name of the second variable unit.
     */
    public String getVariableUnit2() { return unit2; }
    
    /* (non-Javadoc)
     * @see dmg.ua.sounding.Sounding#setAltitude(java.lang.Double, dmg.util.LengthUtils.LengthUnit)
     */
    @Override public void setAltitude(Double altitude, LengthUnit unit) 
	throws ConversionException {
	super.setAltitude(altitude == null || altitude == 99999.0 ? null :  altitude, unit);
    }
    
    /* (non-Javadoc)
     * @see dmg.ua.sounding.Sounding#setLatitude(java.lang.Double)
     */
    @Override public void setLatitude(Double degrees) throws InvalidValueWarning {
	super.setLatitude(degrees == null || degrees == 999.0 ? null : degrees);
    }

    /* (non-Javadoc)
     * @see dmg.ua.sounding.Sounding#setLongitude(java.lang.Double)
     */
    @Override public void setLongitude(Double degrees) throws InvalidValueWarning {
	super.setLongitude(degrees == null || degrees == 9999.0 ? null : degrees);
    }

    /**
     * Set the name of the first variable measurement.
     * @param measurement The name of the measurement.
     * @throws InvalidValueException if the length of the measurement is over 4 characters long.
     */
    public void setVariableMeasurement1(String measurement) throws InvalidValueException {
	if (measurement != null && measurement.trim().equals("")) { 
	    measurement = null;
	}
	
	if (measurement != null && measurement.length() > 4) {
	    throw new InvalidValueException("measurement", measurement,
					    String.format("The value of %s for the measurement must be under 5 characters.",measurement));
	}
	measurement1 = measurement;
    }
    
    /**
     * Set the name of the second variable measurement.
     * @param measurement The name of the measurement.
     * @throws InvalidValueException if the length of the measurement is over 4 characters long.
     */
    public void setVariableMeasurement2(String measurement) throws InvalidValueException {
	if (measurement != null && measurement.trim().equals("")) { 
	    measurement = null;
	}
	
	if (measurement != null && measurement.length() > 4) {
	    throw new InvalidValueException("measurement", measurement,
					    String.format("The value of %s for the measurement must be under 5 characters.",measurement));
	}
	measurement2 = measurement;
    }
    
    /**
     * Set the unit of the first variable measurement.
     * @param unit The name of the unit.
     * @throws InvalidValueException if the length of the unit is over 4 characters long.
     */
    public void setVariableUnit1(String unit) throws InvalidValueException {
	if (unit != null && unit.trim().equals("")) { unit = null; }
	
	if (unit != null && unit.length() > 4) {
	    throw new InvalidValueException("unit", unit, 
					    String.format("The value of %s for the unit must be under 5 characters.", unit));
	}
	unit1 = unit;
    }
    
    /**
     * Set the unit of the second variable measurement.
     * @param unit The name of the unit.
     * @throws InvalidValueException if the length of the unit is over 4 characters long.
     */
    public void setVariableUnit2(String unit) throws InvalidValueException {
	if (unit != null && unit.trim().equals("")) { unit = null; }
	
	if (unit != null && unit.length() > 4) {
	    throw new InvalidValueException("unit", unit, 
					    String.format("The value of %s for the unit must be under 5 characters.", unit));
	}
	unit2 = unit;
    }
    
    @Override public String toString() {
	StringBuffer out = new StringBuffer();
	
	Double[] longitude = new Double[2];
	if (getLongitude() == null) {
	    longitude[0] = 9999.0;
	    longitude[1] = 0.0;
	} else {
	    longitude = toDegreesMinutes(getLongitude());
	}
	Double[] latitude = new Double[2];
	if (getLatitude() == null) {
	    latitude[0] = 999.0;
	    latitude[1] = 0.0;
	} else {
	    latitude = toDegreesMinutes(getLatitude());
	}
	
	out.append(String.format("%-35s%s%s\n", "Data Type:",
				 getDataType() == null ? "" : getDataType(),
				 getReleaseDirection() == null ? "" :
				 String.format("/%s",getReleaseDirection())));
	out.append(String.format("%-35s%s\n", "Project ID:", 
				 getProjectId() == null ? "" : getProjectId()));
	out.append(String.format("%-35s%s\n", "Release Site Type/Site ID:",
				 getStationDescription() == null ? "" : 
				 getStationDescription()));
	out.append(String.format("%-35s%03d %4.2f'%s, %02d %4.2f'%s, %.3f, %.3f, %.1f\n", "Release Location (lon,lat,alt):",
				 (int)abs(longitude[0]), longitude[1], 
				 (longitude[0] < 0 ? WEST.toString() : EAST.toString()),
				 (int)abs(latitude[0]), latitude[1], 
				 (latitude[0] < 0 ? SOUTH.toString() : NORTH.toString()), 
				 getLongitude() == null ? 9999.0 : getLongitude(), 
				 getLatitude() == null ? 999.0 : getLatitude(), 
				 getAltitude() == null ? 99999.0 : getAltitude()));
	if (getActualDate() == null) {
	    out.append(String.format("%-35s0000, 00, 00, 00:00:00\n", 
				     "UTC Release Time (y,m,d,h,m,s):"));
	} else {
	    out.append(String.format(
				     "%-35s%2$tY, %2$tm, %2$td, %2$tH:%2$tM:%2$tS\n", 
				     "UTC Release Time (y,m,d,h,m,s):",getActualDate()));
	}
	for (int i = 6; i < 12; i++) {
	    if (this.getHeaderLine(i) == null) {
		out.append("/\n");
	    } else {
		out.append(String.format("%-35s%s\n", 
					 getHeaderLine(i).getLabel(), 
					 getHeaderLine(i).getContent()));
	    }
	}
	if (getNominalDate() == null) {
	    out.append(String.format("%-35s0000, 00, 00, 00:00:00\n", 
				     "Nominal Release Time (y,m,d,h,m,s):"));
	} else {
	    out.append(String.format(
				     "%-35s%2$tY, %2$tm, %2$td, %2$tH:%2$tM:%2$tS\n", 
				     "Nominal Release Time (y,m,d,h,m,s):", getNominalDate()));
	}
	out.append(String.format(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   %-4s  %-4s   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ\n",
				 getVariableMeasurement1() == null ? "Ele" : 
				 getVariableMeasurement1(),
				 getVariableMeasurement2() == null ? "Azi" : 
				 getVariableMeasurement2()));
	out.append(String.format("  sec    mb     C     C     %%     m/s    m/s   m/s   deg   m/s      deg     deg   %-4s  %-4s    m    code code code code code code\n", 
				 getVariableUnit1() == null ? "deg" :
				 getVariableUnit1(),
				 getVariableUnit2() == null ? "deg" : 
				 getVariableUnit2()));
	out.append("------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----");
	
	for (ESCSoundingRecord record: getRecords()) {
	    out.append("\n").append(record.toString());
	}
	
	return out.toString();
    }
}
