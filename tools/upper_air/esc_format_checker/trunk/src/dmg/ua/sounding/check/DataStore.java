package dmg.ua.sounding.check;

import java.io.*;
import java.util.*;

/**
 * <p>The DataStore class is a container for storing information from a parsed
 * sounding that must be compared, but is contained in multiple lines.  It also
 * performs the comparisons on the data it stores.</p>
 * <p>Included in the DataStore are comparisons between the surface location and
 * the release location from the header, testing to see if the actual release is
 * within two hours of the nominal release, and checking to see if the times of
 * the data records are constantly increasing/decreasing.</p>
 * 
 * @author Joel Clawson
 */
public class DataStore {

    private Calendar actual, nominal;
    private Double headerAltitude, headerLatitude, headerLongitude;
    private Double sfcAltitude, sfcLatitude, sfcLongitude;
    private List<Double> times;
    private int surfaceLineNumber;
    
    /**
     * Create a new instance of a DataStore.
     */
    public DataStore() {
	times = new ArrayList<Double>();
	surfaceLineNumber = -1;
    }
    
    /**
     * Compare the times in the DataStore to see if they are all increasing or
     * decreasing.
     * @param log The output stream where errors are to be written.
     */
    public void compareDataTimeSequence(PrintWriter log) {
	if (times.size() < 2) {
	    log.printf("There are only two time values in the time sequence.\n");
	    log.flush();
	    return;
	}
	
	// Need to special case the instance where the first 2 times are equal.
	if (times.get(1).equals(times.get(0))) {
	    log.printf("The sounding has equal times to start the series %.1f.\n", times.get(0));
	    log.flush();
	    return;
	}
	
	// Determine if the times are ascending or descending.
	boolean ascending = times.get(1) - times.get(0) > 0;
	
	// Loop through all of the times until a time is found to be out of
	// sequence or until the end of the list is reached.
	boolean success = true;
	int i;
	for (i = 0; success && i < times.size() - 1; i++) {
	    // Define success to be another point that is either ascending or
	    // descending based off of the first two points
	    success = ascending ? (times.get(i+1) - times.get(i) > 0) :
		(times.get(i+1) - times.get(i) < 0);
	}
	
	// Generate an error message on failure.
	if (!success) {
	    log.printf("The sounding does not have a consistant time sequence at times %.1f and %.1f.\n", times.get(i - 1), times.get(i));
	    log.flush();
	}
    }
    
    /**
     * Compare the actual and nominal times from the header and ensure that the actual
     * release is within two hours of the nominal time.
     * @param log The output stream where errors are to be written.
     */
    public void compareHeaderDates(PrintWriter log) {
	if (nominal == null || actual == null) { return; }
	
	Calendar lowDate = (Calendar)nominal.clone();
	Calendar highDate = (Calendar)nominal.clone();
	
	lowDate.add(Calendar.HOUR, -2);
	highDate.add(Calendar.HOUR, 2);
	
	if (lowDate.compareTo(actual) > 0 || highDate.compareTo(actual) < 0) {
	    log.printf("The actual date %1$tY/%1$tm/%1$td %1$tH:%1$tM:%1$tS is not within two hours of the nominal date %2$tY/%2$tm/%2$td %2$tH:%2$tM:%2$tS.\n", actual, nominal);
	    log.flush();
	}
    }
    
    /**
     * Compare the release location information with the location information from
     * the surface data line.
     * @param log The output stream where errors are to be written.
     */
    public void compareLocations(PrintWriter log) {
	if (headerLongitude == null || sfcLongitude == null || !headerLongitude.equals(sfcLongitude)) {
	    log.printf("The header longitude %.3f is not the same as the surface longitude %.3f.\n", headerLongitude, sfcLongitude);
	}
	if (headerLatitude == null || sfcLatitude == null || !headerLatitude.equals(sfcLatitude)) {
	    log.printf("The header latitude %.3f is not the same as the surface latitude %.3f.\n", headerLatitude, sfcLatitude);
	}
	if (headerAltitude == null || sfcAltitude == null || !headerAltitude.equals(sfcAltitude)) {
	    log.printf("The header altitude %.1f is not the same as the surface altitude %.1f.\n", headerAltitude, sfcAltitude);
	}
	log.flush();
    }
    
    /**
     * Add a time to the end of the time sequence list.  Any missing times will
     * not be added.
     * @param time The time to be added to the time sequence list.
     */
    public void addTime(Double time) { 
	if (!time.equals(9999.0)) { times.add(time); }
    }
    
    /**
     * Set the time the sounding was actually released.
     * @param actualDate The actual sounding release time.
     */
    public void setActualDate(Calendar actualDate) {
	actual = actualDate;
    }
    
    /**
     * Set the altitude that was parsed from the sounding header.
     * @param altitude The altitude value from the header line.
     */
    public void setHeaderAltitude(Double altitude) {
	headerAltitude = altitude;
    }
    
    /**
     * Set the latitude that was parsed from the sounding header.
     * @param latitude The latitude value from the header line.
     */
    public void setHeaderLatitude(Double latitude) {
	headerLatitude = latitude;
    }
    
    /**
     * Set the longitude that was parsed from the sounding header.
     * @param longitude The longitude value from the header line.
     */
    public void setHeaderLongitude(Double longitude) {
	headerLongitude = longitude;
    }
    
    /**
     * Set the time the sounding was nominally released.
     * @param nominalDate The nominal release time of the sounding.
     */
    public void setNominalDate(Calendar nominalDate) {
	nominal = nominalDate;
    }
    
    /**
     * Set the altitude from the data line.  This will only save the altitude
     * that the store believes is from the surface data point.
     * @param altitude The altitude value to be stored.
     * @param lineNumber The line number the altitude came from.
     */
    public void setSurfaceAltitude(Double altitude, int lineNumber) {
	if (surfaceLineNumber == -1 || surfaceLineNumber == lineNumber) {
	    sfcAltitude = altitude;
	    surfaceLineNumber = lineNumber;
	}
    }
    
    /**
     * Set the latitude from the data line.  This will only save the latitude
     * that the store believes is from the surface data point.
     * @param latitude The latitude value to be stored.
     * @param lineNumber The line number the latitude came from.
     */
    public void setSurfaceLatitude(Double latitude, int lineNumber) {
	if (surfaceLineNumber == -1 || surfaceLineNumber == lineNumber) {
	    sfcLatitude = latitude;
	    surfaceLineNumber = lineNumber;
	}
    }
    
    /**
     * Set the longitude from the data line.  This will only save the longitude
     * that the store believes is from the surface data point.
     * @param longitude The longitude value to be stored.
     * @param lineNumber The line number the longitude came from.
     */
    public void setSurfaceLongitude(Double longitude, int lineNumber) {
	if (surfaceLineNumber == -1 || surfaceLineNumber == lineNumber) {
	    sfcLongitude = longitude;
	    surfaceLineNumber = lineNumber;
	}
    }
}
