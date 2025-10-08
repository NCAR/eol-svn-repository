package dln.util;

import dln.beans.*;

/**
 * The ProjectComparator is a generic Comparator to determine the sort order
 * of ProjectBean instances.
 * 
 * @author jclawson
 */
public abstract class ProjectComparator extends DefaultComparator<ProjectBean> {
	
	/**
	 * Create a new instance of a ProjectComparator.
	 * @param reverse if the comparator should use a reverse sort during the comparison.
	 */
	public ProjectComparator(boolean reverse) { super(reverse); }
}
