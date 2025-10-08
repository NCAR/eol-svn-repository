package dmg.station;

import dmg.util.*;

/**
 * <p>The StationRestrictor interface is a special form of Observer that is notified
 * of a pending change to a Station.  The StationRestrictor then tests the change to
 * see if it will allow the change.  If the change is denied, it throws a
 * RestrictedOperationException with the details of why it denied the change.
 * 
 * @author Joel Clawson
 */
public interface StationRestrictor {

	/**
	 * Receive a notification from a Station that it wants to change its elevation.
	 * @param station The Station which wants to change its elevation.
	 * @param original The elevation of the Station before the change.
	 * @param changed The new elevation of the Station after it is changed.
	 * @throws RestrictedOperationException when the restrictor cannot allow the elevation
	 * to be changed to the new value.
	 */
	public void elevationChangeInProgress(Station station, Double original, Double changed) throws RestrictedOperationException;
	
	/**
	 * Receive a notification from a Station that it wants to change its latitude.
	 * @param station The Station which wants to change its latitude.
	 * @param original The latitude of the Station before the change.
	 * @param changed The new latitude of the Station after it is changed.
	 * @throws RestrictedOperationException when the restrictor cannot allow the latitude
	 * to be changed to the new value.
	 */
	public void latitudeChangeInProgress(Station station, Double original, Double changed) throws RestrictedOperationException;
	
	/**
	 * Receive a notification from a Station that it wants to change its longitude.
	 * @param station The Station which wants to change its longitude.
	 * @param original The longitude of the Station before the change.
	 * @param changed The new longitude of the Station after the change.
	 * @throws RestrictedOperationException when the restrictor cannot allow the longitude
	 * to be changed to the new value.
	 */
	public void longitudeChangeInProgress(Station station, Double original, Double changed) throws RestrictedOperationException;
	
	/**
	 * Receive a notification from a Station that it wants to change its network name.
	 * @param station The Station which wants to change its network name.
	 * @param original The netwok name of the Station before the change.
	 * @param changed The new network name of the Station after the change.
	 * @throws RestrictedOperationException when the restrictor cannot allow the network
	 * name to be changed to the new value.
	 */
	public void networkNameChangeInProgress(Station station, String original, String changed) throws RestrictedOperationException;
	
	/**
	 * Receive a notification from a Station that wants to change its identifier.
	 * @param station The Station which wants to change its identifier.
	 * @param original The identifier of the Station before the change.
	 * @param changed The new identifier of the Station after the change.
	 * @throws RestrictedOperationException when the restrictor cannot allow the network
	 * name to be changed to the new value.
	 */
	public void stationIdChangeInProgress(Station station, String original, String changed) throws RestrictedOperationException;
}
