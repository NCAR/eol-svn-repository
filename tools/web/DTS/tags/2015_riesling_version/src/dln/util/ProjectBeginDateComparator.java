package dln.util;

import dln.beans.*;

/**
 * The ProjectBeginDateComparator is a Comparator to determine the sort order
 * of ProjectBean instances based on the natural sort order of the ProjectBeans'
 * begin date.
 * 
 * @author jclawson
 */
public class ProjectBeginDateComparator extends ProjectComparator {

	/**
	 * Create a new instance of a ProjectBeginDateComparator.
	 * @param reverse if the comparator should use a reverse sort during the comparison.
	 */
	public ProjectBeginDateComparator(boolean reverse) { super(reverse); }
	
	/**
	 * Compare two ProjectBeans for sort order based on their begin dates.
	 * @param first The first ProjectBean to be compared.
	 * @param second The second ProjectBean to be compared.
	 * @return A negative integer, zero, or a positive integer if the first bean is
	 * less than, equal to, or greater than the second bean.
	 */
	public int compare(ProjectBean first, ProjectBean second) {
		return isReverseSort() ? second.getBeginDate().compareTo(first.getBeginDate()) :
			first.getBeginDate().compareTo(second.getBeginDate());
	}
}
