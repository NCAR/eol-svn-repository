package dln.util;

import dln.beans.*;

/**
 * The ProjectEndDateComparator is a Comparator to determine the sort order
 * of ProjectBean instances based on the natural sort order of the ProjectBeans'
 * end date.
 * 
 * @author jclawson
 */
public class ProjectEndDateComparator extends ProjectComparator {

	/**
	 * Create a new instance of a ProjectEndDateComparator.
	 * @param reverse if the comparator should use a reverse sort during the comparison.
	 */
	public ProjectEndDateComparator(boolean reverse) { super(reverse); }
	
	/**
	 * Compare two ProjectBeans for sort order based on their end dates.
	 * @param first The first ProjectBean to be compared.
	 * @param second The second ProjectBean to be compared.
	 * @return A negative integer, zero, or a positive integer if the first bean is
	 * less than, equal to, or greater than the second bean.
	 */
	public int compare(ProjectBean first, ProjectBean second) {
		return isReverseSort() ? second.getEndDate().compareTo(first.getEndDate()) :
			first.getEndDate().compareTo(second.getEndDate());
	}
}
