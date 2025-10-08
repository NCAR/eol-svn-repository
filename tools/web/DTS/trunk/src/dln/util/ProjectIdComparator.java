package dln.util;

import dln.beans.*;

/**
 * The ProjectIdComparator is a Comparator to determine the sort order
 * of ProjectBean instances based on the natural sort order of the ProjectBeans'
 * project ID.
 * 
 * @author jclawson
 */
public class ProjectIdComparator extends ProjectComparator {

	/**
	 * Create a new instance of a ProjectIdComparator.
	 * @param reverse if the comparator should use a reverse sort during the comparison.
	 */
	public ProjectIdComparator(boolean reverse) { super(reverse); }

	/**
	 * Compare two ProjectBeans for sort order based on their project IDs.
	 * @param first The first ProjectBean to be compared.
	 * @param second The second ProjectBean to be compared.
	 * @return A negative integer, zero, or a positive integer if the first bean is
	 * less than, equal to, or greater than the second bean.
	 */
	public int compare(ProjectBean first, ProjectBean second) {
		return isReverseSort() ? second.getProjectId().compareTo(first.getProjectId()) :
			first.getProjectId().compareTo(second.getProjectId());
	}
}
