package dmg.station;

import dmg.util.*;

/**
 * <p>The PositionedStationList is a StationList that uniquely defines a Station
 * by its identifier, network name, altitude, and longitude.  It assumes that 
 * the elevation for the Stations are not important in identifying the 
 * Station.</p>
 * 
 * @see dmg.station.ElevatedStationList
 * @see dmg.station.NamedStationList
 * @see dmg.station.StationPositionComparator
 * 
 * @author Joel Clawson
 */
public class PositionedStationList extends StationList {
	
	/**
	 * Create a new instance of a PositionedStationList.
	 */
	public PositionedStationList() {
		super(new StationPositionComparator());
	}

	/**
	 * Determine if a Station with the specified name and location is in the 
	 * StationList.
	 * @param stationId The identifier of the Station to be found.
	 * @param networkName The name of the network the Station is in.
	 * @param latitude The latitude where the Station is located.
	 * @param longitude The longitude where the Station is located.
	 * @return <code>true</code> if the StationList has the specified Station,
	 * <code>false</code> if it does not.
	 */
	public boolean contains(String stationId, String networkName, 
			Double latitude, Double longitude) {
		try {
			Station station = new Station(stationId, networkName);
			station.setLatitude(latitude);
			station.setLongitude(longitude);
			return contains(station);
		}
		// The station couldn't be setup correctly, so it can't be in the 
		// station list.
		catch (InvalidValueException e) { return false; }
		catch (InvalidValueWarning e) { return false; }
		catch (RestrictedOperationException e) { return false; }
	}

	/**
	 * Test the restrictions on a change for elevation.  A PositionedStationList
	 * does not care about elevation changes, so this function does not restrict
	 * a change of a Station's elevation.
	 * @param station The station having its elevation changed.
	 * @param oldElevation The elevation of the Station before the change.
	 * @param newElevation The elevation for the Station after the change.
	 * @throws RestrictedOperationException is never thrown in this context.
	 */
	public void elevationChangeInProgress(Station station, Double oldElevation,
			Double newElevation) throws RestrictedOperationException {}
	
	/**
	 * Get the Station with the specified name and location.
	 * @param stationId The identifier of the Station.
	 * @param networkName The network name of the Station.
	 * @param latitude The latitude of the Station's location.
	 * @param longitude The longitude of the Station's location.
	 * @return The Station in the StationList with the specified attributes or
	 * <code>null</code> if the Station is not in the StationList.
	 */
	public Station get(String stationId, String networkName, Double latitude,
			Double longitude) {
		try {
			Station station = new Station(stationId, networkName);
			station.setLatitude(latitude);
			station.setLongitude(longitude);
			return get(station);
		}
		// The station couldn't be setup correctly, so it can't be in the 
		// station list.
		catch (InvalidValueException e) { return null; }
		catch (InvalidValueWarning e) { return null; }
		catch (RestrictedOperationException e) { return null; }
	}

	/**
	 * Test the restrictions on a change for the latitude of a Station.  This 
	 * will prevent a Station from changing its latitude if the Station's 
	 * identifier, network name and longitude already exists in the StationList 
	 * for the new latitude.
	 * @param station The Station having its latitude changed.
	 * @param oldLatitude The latitude of the Station before the change.
	 * @param newLatitude The latitude for the Station after the change.
	 * @throws RestrictedOperationException if the new latitude already exists 
	 * in the StationList for another Station with the same identifier, network 
	 * name, and longitude.
	 */
	public void latitudeChangeInProgress(Station station, Double oldLatitude, 
			Double newLatitude) throws RestrictedOperationException {
		if (contains(station.getStationId(), station.getNetworkName(), 
				newLatitude, station.getLongitude())) {
			throw new RestrictedOperationException(
					String.format("Station %s/%s/%f/%f already exists in " +
							"the StationList.",station.getStationId(),
							station.getNetworkName(),newLatitude,
							station.getLongitude()));
		}
	}

	/**
	 * Test the restrictions on a change for the longitude of a Station.  This 
	 * will prevent a Station from changing its longitude if the Station's 
	 * identifier, network name, and latitude already exist in the StationList 
	 * for the new longitude.
	 * @param station The Station having its longitude changed.
	 * @param oldLongitude The longitude of the Station before the change.
	 * @param newLongitude The longitude for the Station after the change.
	 * @throws RestrictedOperationException if the new longitude already exists 
	 * in the StationList for another Station with the same identifier, network 
	 * name, and latitude.
	 */
	public void longitudeChangeInProgress(Station station, Double oldLongitude, 
			Double newLongitude) throws RestrictedOperationException {
		if (contains(station.getStationId(), station.getNetworkName(), 
				station.getLatitude(), newLongitude)) {
			throw new RestrictedOperationException(
					String.format("Station %s/%s/%f/%f already exists in " +
							"the StationList.",station.getStationId(),
							station.getNetworkName(),station.getLatitude(),
							newLongitude));
		}
	}

	/**
	 * Test the restrictions on a change for the network name of a Station.  
	 * This will prevent a Station from changing its network name if the 
	 * Station's identifier, latitutde, and longitude already exist in the 
	 * StationList for the new network name.
	 * @param station The Station having its network name changed.
	 * @param oldName The network name of the Station before the change.
	 * @param newName The network name for the Station after the change.
	 * @throws RestrictedOperationException if the new network name already 
	 * exists in the StationList for another Station with the same identifier, 
	 * latitude, and longitude.
	 */
	public void networkNameChangeInProgress(Station station, String oldName, 
			String newName) throws RestrictedOperationException {
		if (contains(station.getStationId(), newName, station.getLatitude(), 
				station.getLongitude())) {
			throw new RestrictedOperationException(
					String.format("Station %s/%s/%f/%f already exists in " +
							"the StationList.",station.getStationId(),newName,
							station.getLatitude(),station.getLongitude()));
		}
	}
	
	/**
	 * Remove a Station with the specified name and location.
	 * @param stationId The identifier of the Station to be removed.
	 * @param networkName The network name of the Station.
	 * @param latitude The latitude of the Station's position.
	 * @param longitude The longitude of the Station's position.
	 * @return The Station that was removed from the StationList or 
	 * <code>null</code> if the Station was not in the StationList.
	 */
	public Station remove(String stationId, String networkName, Double latitude,
			Double longitude) {
		try {
			Station station = new Station(stationId, networkName);
			station.setLatitude(latitude);
			station.setLongitude(longitude);
			return remove(station);
		}
		// The station couldn't be setup correctly, so it can't be in the 
		// station list.
		catch (InvalidValueException e) { return null; }
		catch (InvalidValueWarning e) { return null; }
		catch (RestrictedOperationException e) { return null; }
	}

	/**
	 * Test the restrictions on a change for a Station identifier.  This will 
	 * prevent a Station to change the identifier if the Station's network name,
	 * latitude, and longitude already exists in the StationList for the new 
	 * identifier.
	 * @param station The Station having its identifier changed.
	 * @param oldId The identifier of the Station before the change.
	 * @param newId The identifier for the Station after the change.
	 * @throws RestrictedOperationException if the new identifier already exists
	 * in the StationList for another Station with the same network name, 
	 * latitude, and longitude.
	 */
	public void stationIdChangeInProgress(Station station, String oldId, 
			String newId) throws RestrictedOperationException {
		if (contains(newId, station.getNetworkName(), station.getLatitude(), 
				station.getLongitude())) {
			throw new RestrictedOperationException(
					String.format("Station %s/%s/%f/%f already exists in " +
							"the StationList.",newId,station.getNetworkName(),
							station.getLatitude(),station.getLongitude()));
		}
	}

	@Override
	protected String uniqueStation(Station station) {
		return String.format("%s/%s/%f/%f",station.getStationId(),
				station.getNetworkName(),station.getLatitude(),
				station.getLongitude());
	}
}
