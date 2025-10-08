package dmg.ua.sounding.esc;

import dmg.ua.sounding.*;

/**
 * The ESCSoundingRecordPressureComparator is a SoundingRecordComparator that compares
 * ESCSoundingRecord and sorts them based on the pressure value in the record. When the
 * pressure values are equal, the sort uses the times of the record to ensure the records
 * are put into the proper order.
 * 
 * @author jclawson
 */
public class ESCSoundingRecordPressureComparator extends SoundingRecordComparator<ESCSoundingRecord> {

	/**
	 * Create a new instance of an ESCSoundingRecordPressureComparator.
	 * @param reverse <code>true</code> if the sort should be done in the reverse of its default
	 * sort order, <code>false</code> otherwise.
	 */
	public ESCSoundingRecordPressureComparator(boolean reverse) { super(reverse); }
	
	/**
	 * Compare two sounding records for sort order by the pressure in the records.
	 * @param record1 The first record to be compared.
	 * @param record2 The second record to the compared.
	 * @return A negative integer, zero, or positive integer if the pressure in the first record is
	 * less than, equal to, or greater than the pressure in the second record.  If the pressures are
	 * equal, the times are used in the sort.  (The values will be reversed if the reverse flag is 
	 * set to true.)
	 */
	public int compare(ESCSoundingRecord record1, ESCSoundingRecord record2) {
		if (isReverseSort()) {
			return record2.getPressure().compareTo(record1.getPressure()) == 0 ? 
						record1.getTime().compareTo(record2.getTime()) : 
						record2.getPressure().compareTo(record1.getPressure());
		} else {
			return record1.getPressure().compareTo(record2.getPressure()) == 0 ?
						record2.getTime().compareTo(record1.getTime()) :
						record1.getPressure().compareTo(record2.getPressure());
		}
	}
}
