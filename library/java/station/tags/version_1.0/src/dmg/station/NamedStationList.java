package dmg.station;

import dmg.util.*;

/**
 * <p>The NamedStationList is a StationList that uniquely defines a Station by 
 * its identifier and network name.  It assumes that the latitude, longitude, 
 * and elevation for the Stations are not important in identifying the 
 * Station.</p>
 * 
 * @see dmg.station.ElevatedStationList
 * @see dmg.station.PositionedStationList
 * @see dmg.station.StationNameComparator
 * 
 * @author Joel Clawson
 */
public class NamedStationList extends StationList {

	/**
	 * Create a new instance of a NamedStationList.
	 */
	public NamedStationList() {	super(new StationNameComparator()); }
	
	/**
	 * Determine if a Station with the specified identifier and network exists 
	 * in the StationList.
	 * @param stationId The Station identifier to find in the StationList.
	 * @param networkName The network name of the Station to find in the 
	 * StationList.
	 * @return <code>true</code> if a Station with the specified identifier and 
	 * network was found in the StationList, <code>false</code> it one was not 
	 * found.
	 */
	public boolean contains(String stationId, String networkName) {
		try {
			return contains(new Station(stationId, networkName));
		} 
		// Can't be in the list if an equivalent station can't be created.
		catch (InvalidValueException e) { return false; }
	}
	
	/**
	 * Test the restrictions on a change for elevation.  A NamedStationList does
	 * not care about elevation changes, so this function does not restrict a 
	 * change of a Station's elevation.
	 * @param station The station having its elevation changed.
	 * @param oldElevation The elevation of the Station before the change.
	 * @param newElevation The elevation for the Station after the change.
	 * @throws RestrictedOperationException is never thrown in this context.
	 */
	public void elevationChangeInProgress(Station station, Double oldElevation,
			Double newElevation) throws RestrictedOperationException {}

	/**
	 * Get the Station in the StationList defined by the specified identifier 
	 * and network.
	 * @param stationId The Station identifier of the Station to be accessed.
	 * @param networkName The name of the network for the Station to be 
	 * accessed.
	 * @return The Station in the StationList or <code>null</code> if the 
	 * Station is not in the StationList.
	 */
	public Station get(String stationId, String networkName) {
		try { return get(new Station(stationId, networkName)); }
		// Can't be in the list if an equivalent station can't be created.
		catch (InvalidValueException e) { return null; }
	}
	
	/**
	 * Test the restrictions on a change for latitude.  A NamedStationList does 
	 * not care about latitude changes, so this function does not restrict a 
	 * change of a Station's latitude.
	 * @param station The station having its latitude changed.
	 * @param oldLatitude The latitude of the Station before the change.
	 * @param newLatitude The latitude for the Station after the change.
	 * @throws RestrictedOperationException is never thrown in this context.
	 */
	public void latitudeChangeInProgress(Station station, Double oldLatitude, 
			Double newLatitude) throws RestrictedOperationException  {}

	/**
	 * Test the restrictions on a change for longitude.  A NamedStationList does
	 * not care about longitude changes, so this function does not restrict a 
	 * change of a Station's longitude.
	 * @param station The Station having its longitude changed.
	 * @param oldLongitude The longitude of the Station before the change.
	 * @param newLongitude The longitude for the Station after the change.
	 * @throws RestrictedOperationException is never thrown in this context.
	 */
	public void longitudeChangeInProgress(Station station, Double oldLongitude, 
			Double newLongitude) throws RestrictedOperationException  {}

	/**
	 * Test the restrictions on a change for a network name.  This will prevent 
	 * a Station to change the network name if the Station's identifier already 
	 * exists in the StationList for the new network.
	 * @param station The Station having its network name changed.
	 * @param oldName The network name of the Station before the change.
	 * @param newName The network name for the Station after the change.
	 * @throws RestrictedOperationException if the new network name already 
	 * exists in the StationList for another Station with the same identifier.
	 */
	public void networkNameChangeInProgress(Station station, String oldName, 
			String newName) throws RestrictedOperationException {
		if (contains(station.getStationId(), newName)) {
			throw new RestrictedOperationException(
					String.format("Station %s/%s already exists in the " +
							"StationList.",station.getStationId(),newName));
		}
	}

	/**
	 * Remove the Station identified by the specified identifier and network 
	 * from the StationList.
	 * @param stationId The Station identifier of the Station to be removed.
	 * @param networkName The name of the network for the Station to be removed.
	 * @return The Station that was removed from the StationList or 
	 * <code>null</code> if the Station was not found in the StationList.
	 */
	public Station remove(String stationId, String networkName) {
		try { return remove(new Station(stationId, networkName)); }
		// Can't remove a station that can't be in the list.
		catch (InvalidValueException e) { return null; }
	}
	
	/**
	 * Test the restrictions on a change for a Station identifier.  This will 
	 * prevent a Station to change the identifier if the Station's network name 
	 * already exists in the StationList for the new identifier.
	 * @param station The Station having its identifier changed.
	 * @param oldId The identifier of the Station before the change.
	 * @param newId The identifier for the Station after the change.
	 * @throws RestrictedOperationException if the new identifier already exists
	 * in the StationList for another Station with the same network name.
	 */
	public void stationIdChangeInProgress(Station station, String oldId, 
			String newId) throws RestrictedOperationException {
		if (contains(newId, station.getNetworkName())) {
			throw new RestrictedOperationException(
					String.format("Station %s/%s already exists in the " +
							"StationList.",newId,station.getNetworkName()));
		}
	}

	/**
	 * Create a String representation with the minimal amount of Station 
	 * information to uniquely define the Station in this StationList.
	 * @param station The Station to be represented as a String.
	 * @return The Station as a identifer/network name pair.
	 */
	@Override
	protected String uniqueStation(Station station) {
		return String.format("%s/%s",station.getStationId(),
				station.getNetworkName());
	}
}
