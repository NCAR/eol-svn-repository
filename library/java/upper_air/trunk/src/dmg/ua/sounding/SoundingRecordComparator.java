package dmg.ua.sounding;

import java.util.*;

/**
 * The SoundingRecordComparator class is a generic Comparator for sorting SoundingRecord
 * instances.  It is to be extended to fit the specific needs of the record sorting and
 * defines the actual sort algorithm.  This abstract class provides the functionality to
 * allow the sort be done in both an ascending and descending direction.
 * 
 * @author jclawson
 */
public abstract class SoundingRecordComparator<T extends SoundingRecord> implements Comparator<T> {
	
	private boolean reverse;
	
	/**
	 * Create a new instance of a SoundingRecordComparator.
	 * @param reverse <code>true</code> if the sort should be done in the reverse of its default
	 * sort order, <code>false</code> otherwise.
	 */
	public SoundingRecordComparator(boolean reverse) { this.reverse = reverse; }
	
	/**
	 * Determine if the sorting should be done in the reverse of its natural sort order.
	 * @return <code>true</code> if the sorting should be done in the reverse of its natural sort
	 * order, <code>false</code> otherwise.
	 */
	public boolean isReverseSort() { return reverse; }
}