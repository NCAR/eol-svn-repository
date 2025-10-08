package dmg.util;

/**
 * <p>The MeasurementUnit class is a generic representation of a unit of 
 * measurement used by conversion functions.</p>
 *
 * @author Joel Clawson
 */
public class MeasurementUnit<T extends MeasurementUnit> implements 
		Comparable<T> {

	private String name;
	
	/**
     * Create a new instance of a MeasurementUnit.
     * @param name The name of the unit of measurement.
     */
	public MeasurementUnit(String name) { this.name = name; }

	/**
	 * Compare the name of this unit to the name of the specified unit.
     * @param unit The unit to be compared to this unit.
     * @return A negative integer, zero, or a positive integer if this unit's 
     * name is less than, equal to, or greater than the specified unit's name.
	 */
	public int compareTo(T unit) {
		return getName().compareTo(unit.getName());
	}
	
	/**
	 * Get the name of the unit of measurement.
	 * @return The MeasurementUnit's name.
	 */
	public String getName() { return name; }

	/**
	 * Get the String representation of the MeasurementUnit.
	 * @return The name of the MeasurementUnit.
	 */
	@Override public String toString() { return getName(); }
}
