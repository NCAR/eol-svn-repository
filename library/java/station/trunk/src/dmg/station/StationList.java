package dmg.station;

import dmg.util.*;
import java.io.*;
import java.util.*;

/**
 * <p>The StationList is a generic class for holding and organizing a set of 
 * Stations.  A StationList is a Set and follows Set properties forcing the 
 * StationList to contain  uniquely defined Stations.  It implements the 
 * StationRestrictor interface to this end by preventing a change to a Station 
 * that would cause it to become equivalent to another Station already in the 
 * StationList.</p>
 * 
 * @author JoelClawson
 */
public abstract class StationList implements Iterable<Station>, 
StationRestrictor {

	private static final long serialVersionUID = -6063518949487920984L;

	private TreeSet<Station> stations;
	
	/**
	 * Create a new instance of a StationList.
	 * @param comparator The comparator to use to uniquely define a Station.
	 */
	public StationList(StationComparator comparator) {
		stations = new TreeSet<Station>(comparator);
	}
	
	/**
	 * Add a Station to the StationList.
	 * @param station The Station to add to the StationList.
	 * @throws InvalidValueException if the specified Station already exists
	 * in the StationList.
	 */
	public void add(Station station) throws InvalidValueException {
		if (station == null) {
			throw new InvalidValueException("station",station,
					"The station was null.");
		}
		if (stations.contains(station)) {
			throw new InvalidValueException("station", station,
					String.format("Station %s already exists in the " +
							"StationList.",uniqueStation(station)));
		}

		stations.add(station);
		station.addStationRestrictor(this);
	}

	/**
	 * Clean up a String to be used as part of a filename.  This removes hyphens
	 * and converts sequences of spaces to an underscore.
	 * @param value The String to be cleaned.
	 * @return The String ready to be used as part of a file name.
	 */
	public String cleanForFilename(String value) {
		value = value.replaceAll("\\s+", "_");
		value = value.replaceAll("\\-", "");
		return value;
	}

	/**
	 * Remove all of the Stations that are in the StationList.
	 */
	public void clear() { stations.clear(); }

	/**
	 * Determine if the specified Station is already in the StationList.
	 * @param station The Station to be checked against the StationList.
	 * @return <code>true</code> if the Station (or its equivalent) is already 
	 * in the StationList, <code>false</code> if the Station is not in the 
	 * StationList or if the Station is <code>null</code>.
	 */
	public boolean contains(Station station) {
		if (station == null) { return false; }
		else {
			boolean found = false;
			for (Station entry: this) {
				found = found || 
				stations.comparator().compare(entry, station) == 0;
			}
			return found;
		}
	}

	/**
	 * Get the Station in the StationList that is equivalent to the specified
	 * Station.
	 * @param station The Station to be found in the StationList.
	 * @return The Station equivalent to the specified Station or 
	 * <code>null</code> if
	 * the Station is not in the StationList.
	 */
	public Station get(Station station) {
		return station != null && contains(station) ?
				stations.tailSet(station).first() : null;
	}
	
	/**
	 * Get the stationCD.out filename for the StationList.
	 * @param network The network the station list is for.
	 * @param project The project the StationList is being generated for.
	 * @param type The type of Stations found in the StationList.
	 * @return The full name of the stationCD.out file.
	 */
	public String getFilename(String network, String project, String type) {
		return String.format("%s_%s_%s_stationCD.out",
				cleanForFilename(network),cleanForFilename(project),type);
	}
	
	/**
	 * Create a summary String for all of the Stations in the StationList.  It 
	 * contains a list of all networks, the number of Stations per network 
	 * (with both missing and non-missing dates) with their locations by 
	 * hemisphere/mobile/unknown, and a summary total for each count that does 
	 * not depend on the network.
	 * @return The Station summary String.
	 */
	public String getStationSummary() {
		TreeMap<String,ArrayList<Integer>> counts = 
			new TreeMap<String,ArrayList<Integer>>();
		
		for (Station station: this) {
			if (!counts.containsKey(station.getNetworkName())) {
				ArrayList<Integer> regionCounts = new ArrayList<Integer>(12);
				regionCounts.add(0, 0);  // Valid date in northwest quadrant
				regionCounts.add(1, 0);  // Valid date in northeast quadrant
				regionCounts.add(2, 0);  // Valid date in southeast quadrant
				regionCounts.add(3, 0);  // Valid date in southwest quadrant
				regionCounts.add(4, 0);  // Valid date in an unknown quadrant
				regionCounts.add(5, 0);  // Valid date for mobile stations
				regionCounts.add(6, 0);  // Null date in northwest quadrant
				regionCounts.add(7, 0);  // Null date in northeast quadrant
				regionCounts.add(8, 0);  // Null date in southeast quadrant
				regionCounts.add(9, 0);  // Null date in southwest quadrant
				regionCounts.add(10,0);  // Null date in an unknown quadrant
				regionCounts.add(11,0);  // Null date for mobile stations
				counts.put(station.getNetworkName(), regionCounts);
			}

			ArrayList<Integer> regionCounts = 
				counts.get(station.getNetworkName());
			
			// Figure out if the station has valid dates and adjust the offset 
			// for the array.
			int offset = 0;
			if (station.getBeginDate() == null || 
					station.getEndDate() == null) {
				offset = 6;
			}
			
			// Determine if the station location is known
			if (station.getLatitude() == null || 
					station.getLongitude() == null) {
				// Check to see if the unknown location is caused by the station
				// being mobile.
				if (station.isMobile()) {
					regionCounts.set(5+offset, regionCounts.get(5+offset)+1);
				} else {
					regionCounts.set(4+offset, regionCounts.get(4+offset)+1);
				}
			} else {
				// Determine which quadrant on the globe the station is located.
				if (station.getLatitude() < 0) {
					if (station.getLongitude() < 0) {
						regionCounts.set(3+offset,regionCounts.get(3+offset)+1);
					} else {
						regionCounts.set(2+offset,regionCounts.get(2+offset)+1);
					}
				} else {
					if (station.getLongitude() < 0) {
						regionCounts.set(0+offset,regionCounts.get(0+offset)+1);
					} else {
						regionCounts.set(1+offset,regionCounts.get(1+offset)+1);
					}
				}
			}
		}
		
		/*
		 * Generate the output String to summarizes the Station information.
		 */
		StringBuffer sb = new StringBuffer();
		
		// Create the header for the output
		sb.append(String.format(
				"There were %d stations found for %d networks.\n",size(),
				counts.size()));
		
		// Only generate the output if there are networks that contain 
		// information.
		if (counts.size() > 0) {
			sb.append("\n");
			sb.append("-------------------------------------------------");
			sb.append("--------------------------------------------------");
			sb.append("--------------------\n");
			sb.append("|     NETWORK     | TOTAL  (MISS) |  NORTHWEST  |  ");
			sb.append("NORTHEAST  |  SOUTHEAST  |  SOUTHWEST  |   UNKNOWN   |");
			sb.append("    MOBILE   |\n");
			sb.append("--------------------------------------------------");
			sb.append("--------------------------------------------------");
			sb.append("-------------------\n");
			
			// Generate the network specific information
			int[] summary = new int[14];
			for (String network: counts.keySet()) {
				ArrayList<Integer> networkCounts = counts.get(network);
				int validCount = networkCounts.get(0) + networkCounts.get(1) + 
				networkCounts.get(2) + networkCounts.get(3) + 
				networkCounts.get(4) + networkCounts.get(5);
				int missCount = networkCounts.get(6) + networkCounts.get(7) + 
				networkCounts.get(8) + networkCounts.get(9) + 
				networkCounts.get(10) + networkCounts.get(11);
	
				sb.append(String.format("| %-15s | %5d (%5d) | %4d (%4d) |" +
						" %4d (%4d) | %4d (%4d) | %4d (%4d) | %4d (%4d) |" +
						" %4d (%4d) |\n",network,validCount + missCount, 
						missCount, networkCounts.get(0), networkCounts.get(6),
						networkCounts.get(1), networkCounts.get(7), 
						networkCounts.get(2), networkCounts.get(8), 
						networkCounts.get(3), networkCounts.get(9), 
						networkCounts.get(4), networkCounts.get(10), 
						networkCounts.get(5), networkCounts.get(11)));
				sb.append("----------------------------------------------");
				sb.append("----------------------------------------------");
				sb.append("---------------------------\n");
				
				summary[0] += validCount;
				summary[1] += missCount;
				for (int i = 0; i < 12; i++) {
					summary[i + 2] += networkCounts.get(i);
				}
			}
			
			// Generate the summary for all of the networks.
			sb.append(String.format("|     SUMMARY     | %5d (%5d) |" +
					" %4d (%4d) | %4d (%4d) | %4d (%4d) | %4d (%4d) |" +
					" %4d (%4d) | %4d (%4d) |\n",size(),summary[1],summary[2],
					summary[8],summary[3],summary[9],summary[4],summary[10],
					summary[5],summary[11],summary[6],summary[12],summary[7],
					summary[13]));
			sb.append("-------------------------------------------------");
			sb.append("------------------------------------------------");
			sb.append("----------------------\n");
		}

		// Remove the final \n from the output.
		return sb.length() > 0 ? sb.substring(0, sb.length() - 1) : "";
	}
	
	/**
	 * Determine if this StationList contains any Stations.
	 * @return <code>true</code> if the StationList does not contain any 
	 * Stations, <code>false</code> if there is at least one Station in the 
	 * StationList.
	 */
	public boolean isEmpty() { return stations.isEmpty(); }
	
	/**
	 * Get an Iterator over the Stations in the StationList.
	 * @return An Iterator over the Stations in the StationList.
	 */
	public Iterator<Station> iterator() { return stations.iterator(); }

	/**
	 * Remove the specified Station from the StationList.
	 * @param station The Station to be removed from the StationList.
	 * @return The Station that was removed from the StationList or 
	 * <code>null</code> if the specified Station was not in the StationList.
	 */
	public Station remove(Station station) {
		if (contains(station)) {
			station = get(station);
			if (station != null) {
				stations.remove(station);
				station.removeStationRestrictor(this);
			}
			return station;
		} else {
			return null;
		}
	}
	
	/**
	 * Get the number of Stations that are currently in the StationList.
	 * @return The number of Station in the StationList.
	 */
	public int size() { return stations.size(); }
	
	/**
	 * Generate a String representation of the StationList.  It is a multiple 
	 * line String (one Station per line) ordered by the natural sort order 
	 * for a Station for only the Station in the StationList with valid dates.
	 * @return The StationList as a String of Stations with non-
	 * <code>null</code> dates.
	 */
	@Override
	public String toString() {
		StringBuffer sb = new StringBuffer();
		
		// Copy the current list into a new set that will organize the Stations
		// by their natural sort order for generating the output.
		TreeSet<Station> full = new TreeSet<Station>();
		full.addAll(stations);		
		for (Station station: full) {
			// Only output stations that were specified to contain data.
			if (station.getBeginDate() != null && 
					station.getEndDate() != null) {
				sb.append(station.toString()).append("\n");
			}
		}
		return sb.length() > 0 ? sb.substring(0, sb.length() - 1) : "";
	}
	
	/**
	 * Generate a String that displays the minimal amount of Station information
	 * to uniquely define the specified Station.
	 * @param station The Station to be uniquely defined in a String.
	 * @return The Station as a minimally uniquely defined String.
	 */
	protected abstract String uniqueStation(Station station);

	/**
	 * Generate the stationCD output file for the StationList.  This will only 
	 * contain the Stations that have non-missing begin and end dates.
	 * @param directory The directory where the output file is to be stored.
	 * @param network The network the StationList file is being generated for.
	 * @param project The project the StationList is associated with.
	 * @param type The type of Stations included in the StationList.
	 * @throws IOException if there is a problem creating or writing to the 
	 * output file.
	 */
	public void writeStationCDout(File directory, String network, 
			String project, String type) throws IOException {
		File stationCD = new File(directory,getFilename(network,project,type));
		
		PrintWriter out = new PrintWriter(new FileWriter(stationCD));
		out.println(this.toString());
		out.close();
	}
	
	/**
	 * Generate a summary file for all of the Stations in the StationList.
	 * @param file The file the summary is to be written to.
	 * @throws IOException if there is a problem creating or writing to the 
	 * output file.
	 */
	public void writeStationSummary(File file) throws IOException {
		PrintWriter out = new PrintWriter(new FileWriter(file));
		out.println(this.getStationSummary());
		out.close();
	}
}
