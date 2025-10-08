package dmg.station;

import dmg.util.*;

/**
 * <p>The StationElevationComparator is a StationComparator used for comparing
 * two Stations for sort order using the latitude, longitude, elevation, station
 * identifier, and network name.</p>
 * 
 * @author Joel Clawson
 */
public class StationElevationComparator extends StationNameComparator {

    /**
     * Compare two Stations for sort order.  A Station is ordered by latitude,
     * longitude, elevation, station identifer and network name.
     * @param first The first Station to be compared.
     * @param second The second Station to be comapred.
     * @return A negative number, zero, or a positive number if the first Station
     * is less than, equal to, or greater than the second Station.
     */
    public int compare(Station first, Station second) {
	// Start with comparing the latitudes
	int value = ComparisonUtils.compare(first.getLatitude(), second.getLatitude());
	if (value == 0) {
	    // The latitudes are the same, now try the longitude.
	    value = ComparisonUtils.compare(first.getLongitude(), second.getLongitude());
	    if (value == 0) {
		// The position is equal, so try the elevation.
		value = ComparisonUtils.compare(first.getElevation(), second.getElevation());
		
		if (value == 0) {
		    // Since the latitudes and longitudes are equal, compare the
		    // rest of the Station by its id and network.
		    value = super.compare(first, second);
		}
	    }
	}
	return value;
    }
}
