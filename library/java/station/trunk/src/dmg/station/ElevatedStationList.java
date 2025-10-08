package dmg.station;

import static dmg.util.LengthUtils.*;
import dmg.util.*;

/**
 * <p>The ElevatedStationList is a StationList that uniquely defines a Station 
 * by its identifier, network name, altitude, longitude, and elevation.</p>
 * 
 * @see dmg.station.NamedStationList
 * @see dmg.station.PositionedStationList
 * @see dmg.station.Station
 * @see dmg.station.StationElevationComparator
 * 
 * @author Joel Clawson
 */
public class ElevatedStationList extends StationList {

	/**
	 * Create a new instance of an ElevatedStationList.
	 */
	public ElevatedStationList() { super(new StationElevationComparator()); }
	
	/**
	 * Determine if a Station with the specified name and location is in the 
	 * StationList.
	 * @param stationId The identifier of the Station to be found.
	 * @param networkName The name of the network the Station is in.
	 * @param latitude The latitude where the Station is located.
	 * @param longitude The longitude where the Station is located.
	 * @param elevation The elevation where the Station is located.
	 * @return <code>true</code> if the StationList has the specified Station,
	 * <code>false</code> if it does not.
	 */
	public boolean contains(String stationId, String networkName, 
			Double latitude, Double longitude, Double elevation) {
		try {
			Station station = new Station(stationId, networkName);
			station.setLatitude(latitude);
			station.setLongitude(longitude);
			station.setElevation(elevation, METERS);
			return contains(station);
		} 
		// The station could not be created, so it can't be in the station list.
		catch (ConversionException e) { return false; }
		catch (InvalidValueException e) { return false; }
		catch (InvalidValueWarning e) { return false; }
		catch (RestrictedOperationException e) { return false; }
	}

	/**
	 * Test the restrictions on a change for the elevation of a Station.  This 
	 * will prevent a Station from changing its elevation if the Station's 
	 * identifier, network name, latitude, and longitude already exists in the 
	 * StationList for the new elevation.
	 * @param station The Station having its elevation changed.
	 * @param oldElevation The elevation of the Station before the change.
	 * @param newElevation The elevation for the Station after the change.
	 * @throws RestrictedOperationException if the new elevation already exists 
	 * in the StationList for another Station with the same identifier, network 
	 * name, latitude, and longitude.
	 */
	public void elevationChangeInProgress(Station station, Double oldElevation,
			Double newElevation) throws RestrictedOperationException {
		if (contains(station.getStationId(), station.getNetworkName(), 
				station.getLatitude(), station.getLongitude(), newElevation)) {
			throw new RestrictedOperationException(
					String.format("Station %s/%s/%f/%f/%f already exists in " +
							"the StationList.",station.getStationId(),
							station.getNetworkName(),station.getLatitude(),
							station.getLongitude(),newElevation));
		}
	}
	
	/**
	 * Get the Station with the specified name and location.
	 * @param stationId The identifier of the Station.
	 * @param networkName The network name of the Station.
	 * @param latitude The latitude of the Station's location.
	 * @param longitude The longitude of the Station's location.
	 * @param elevation The elevation of the Station's location.
	 * @return The Station in the StationList with the specified attributes or
	 * <code>null</code> if the Station is not in the StationList.
	 */
	public Station get(String stationId, String networkName, Double latitude, 
			Double longitude, Double elevation) {
		try {
			Station station = new Station(stationId, networkName);
			station.setLatitude(latitude);
			station.setLongitude(longitude);
			station.setElevation(elevation, METERS);
			return get(station);
		}
		// The station could not be created, so it can't be in the station list.
		catch (ConversionException e) { return null; }
		catch (InvalidValueException e) { return null; }
		catch (InvalidValueWarning e) { return null; }
		catch (RestrictedOperationException e) { return null; }
	}

	/**
	 * Test the restrictions on a change for the latitude of a Station.  This 
	 * will prevent a Station from changing its latitude if the Station's
	 * identifier, network name, longitude, and elevation already exists in the 
	 * StationList for the new latitude.
	 * @param station The Station having its latitude changed.
	 * @param oldLatitude The latitude of the Station before the change.
	 * @param newLatitude The latitude for the Station after the change.
	 * @throws RestrictedOperationException if the new latitude already exists 
	 * in the StationList for another Station with the same identifier, network
	 * name, longitude, and elevation.
	 */
	public void latitudeChangeInProgress(Station station, Double oldLatitude, 
			Double newLatitude) throws RestrictedOperationException {
		if (contains(station.getStationId(), station.getNetworkName(), 
				newLatitude, station.getLongitude(), station.getElevation())) {
			throw new RestrictedOperationException(
					String.format("Station %s/%s/%f/%f/%f already exists in " +
							"the StationList.",station.getStationId(),
							station.getNetworkName(),newLatitude,
							station.getLongitude(),station.getElevation()));
		}
	}

	/**
	 * Test the restrictions on a change for the longitude of a Station.  This 
	 * will prevent a Station from changing its longitude if the Station's 
	 * identifier, network name, latitude, and elevation already exists in the 
	 * StationList for the new longitude.
	 * @param station The Station having its longitude changed.
	 * @param oldLongitude The longitude of the Station before the change.
	 * @param newLongitude The longitude for the Station after the change.
	 * @throws RestrictedOperationException if the new longitude already exists 
	 * in the StationList for another Station with the same identifier, network 
	 * name, latitude, and elevation.
	 */
	public void longitudeChangeInProgress(Station station, Double oldLongitude,
			Double newLongitude) throws RestrictedOperationException {
		if (contains(station.getStationId(), station.getNetworkName(), 
				station.getLatitude(), newLongitude, station.getElevation())) {
			throw new RestrictedOperationException(
					String.format("Station %s/%s/%f/%f/%f already exists in " +
							"the StationList.",station.getStationId(),
							station.getNetworkName(),station.getLatitude(),
							newLongitude,station.getElevation()));
		}
	}

	/**
	 * Test the restrictions on a change for the network name of a Station.  
	 * This will prevent a  Station from changing its network name if the 
	 * Station's identifier, latitude, longitude, and elevation already exists 
	 * in the StationList for the new network name.
	 * @param station The Station having its network name changed.
	 * @param oldName The network name of the Station before the change.
	 * @param newName The network name for the Station after the change.
	 * @throws RestrictedOperationException if the new network name already 
	 * exists in the StationList for another Station with the same identifier,
	 * latitude, longitude, and elevation.
	 */
	public void networkNameChangeInProgress(Station station, String oldName, 
			String newName) throws RestrictedOperationException {
		if (contains(station.getStationId(), newName, station.getLatitude(),
				station.getLongitude(), station.getElevation())) {
			throw new RestrictedOperationException(
					String.format("Station %s/%s/%f/%f/%f already exists in " +
							"the StationList.",station.getStationId(),newName,
							station.getLatitude(),station.getLongitude(),
							station.getElevation()));
		}
	}

	/**
	 * Remove a Station with the specified name and location.
	 * @param stationId The identifier of the Station to be removed.
	 * @param networkName The network name of the Station.
	 * @param latitude The latitude of the Station's position.
	 * @param longitude The longitude of the Station's position.
	 * @param elevation The elevation of the Station's position.
	 * @return The Station that was removed from the StationList or 
	 * <code>null</code> if the Station was not in the StationList.
	 */
	public Station remove(String stationId, String networkName, Double latitude,
			Double longitude, Double elevation) {
		try {
			Station station = new Station(stationId, networkName);
			station.setLatitude(latitude);
			station.setLongitude(longitude);
			station.setElevation(elevation, METERS);
			return remove(station);
		}
		// The station couldn't be setup correctly, so it can't be in the 
		// station list.
		catch (ConversionException e) { return null; }
		catch (InvalidValueException e) { return null; }
		catch (InvalidValueWarning e) { return null; }
		catch (RestrictedOperationException e) { return null; }
	}

	/**
	 * Test the restrictions on a change for the identifier of a Station.  This
	 * will prevent a Station from changing its identifier if the Station's 
	 * network name, latitude, longitude, and elevation already exists in the 
	 * StationList for the new identifier.
	 * @param station The Station having its identifier changed.
	 * @param oldId The identifier of the Station before the change.
	 * @param newId The identifier for the Station after the change.
	 * @throws RestrictedOperationException if the new identifier already exists
	 * in the StationList for another Station with the same network name, 
	 * latitude, longitude, and elevation.
	 */
	public void stationIdChangeInProgress(Station station, String oldId, 
			String newId) throws RestrictedOperationException {
		if (contains(newId, station.getNetworkName(), station.getLatitude(),
				station.getLongitude(), station.getElevation())) {
			throw new RestrictedOperationException(
					String.format("Station %s/%s/%f/%f/%f already exists in " +
							"the StationList.",newId,station.getNetworkName(),
							station.getLatitude(),station.getLongitude(),
							station.getElevation()));
		}
	}

	@Override
	protected String uniqueStation(Station station) {
		return String.format("%s/%s/%f/%f/%f",station.getStationId(),
				station.getNetworkName(),station.getLatitude(),
				station.getLongitude(),station.getElevation());
	}
}
