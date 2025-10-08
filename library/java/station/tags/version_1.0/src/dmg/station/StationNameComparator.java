package dmg.station;

import dmg.util.*;

/**
 * <p>The StationNameComparator is a StationComparator used for comparing
 * two Station for sort order using only the identifier and network name.
 * It is to be used with a StationList when it does not make sense to use
 * the location and elevation to differentiation between the Stations.</p>
 * 
 * @author Joel Clawson
 */
public class StationNameComparator extends StationComparator {

    /**
     * Compare two Stations for sort order.  A Station is ordered station
     * identifer and network name.
     * @param first The first Station to be compared.
     * @param second The second Station to be comapred.
     * @return A negative number, zero, or a positive number if the first Station
     * is less than, equal to, or greater than the second Station.
     */
    public int compare(Station first, Station second) {
	// First compare the station ids.
	int value = ComparisonUtils.compare(first.getStationId(),second.getStationId());
	if (value == 0) {
	    // Station ids are equal, so compare the network name.
	    value = ComparisonUtils.compare(first.getNetworkName(),second.getNetworkName());
	}
	return value;
    }
}
