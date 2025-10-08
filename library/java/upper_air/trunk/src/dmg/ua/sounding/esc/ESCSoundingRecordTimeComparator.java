package dmg.ua.sounding.esc;

import dmg.ua.sounding.SoundingRecordComparator;

/**
 * The ESCSoundingRecordTimeComparator is a SoundingRecordComparator that compares
 * ESCSoundingRecord and sorts them based on the time value in the record.
 * 
 * @author jclawson
 */
public class ESCSoundingRecordTimeComparator extends SoundingRecordComparator<ESCSoundingRecord> {

	/**
	 * Create a new instance of an ESCSoundingRecordTimeComparator.
	 * @param reverse <code>true</code> if the sort should be done in the reverse of its default
	 * sort order, <code>false</code> otherwise.
	 */
	public ESCSoundingRecordTimeComparator(boolean reverse) { super(reverse); }

	/**
	 * Compare two sounding records for sort order by the time in the records.
	 * @param record1 The first record to be compared.
	 * @param record2 The second record to the compared.
	 * @return A negative integer, zero, or positive integer if the time in the first record is
	 * less than, equal to, or greater than the time in the second record.  (The values will be
	 * reversed if the reverse flag is set to true.)
	 */
	public int compare(ESCSoundingRecord record1, ESCSoundingRecord record2) {
		if (isReverseSort()) {
			return record2.getTime().compareTo(record1.getTime());
		} else {
			return record1.getTime().compareTo(record2.getTime());
		}
	}
}
