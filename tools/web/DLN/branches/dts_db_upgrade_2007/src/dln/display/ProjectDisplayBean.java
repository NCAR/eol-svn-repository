package dln.display;

import dln.beans.*;
import dln.util.*;

import java.util.*;

/**
 * The ProjectDisplayBean is a JSP session state for maintaining sorting information for the
 * project list.  It maintains which field is currently sorted and in which direction.
 * 
 * @author jclawson
 */
public class ProjectDisplayBean {

	private boolean reverse;
	private int sortField = -1;
	
	/** Constant to sort the projects by project ID. */
	public static int PROJECT_ID_SORT = 0;
	/** Constant to sort the projects by begin/start date. */
	public static int BEGIN_DATE_SORT = 1;
	/** Constant to sort the project by end date. */
	public static int END_DATE_SORT = 2;
	
	/**
	 * Create a new instance of a ProjectDisplayBean.  This defaults the display to
	 * sort by the project ID in the standard order.
	 */
	public ProjectDisplayBean() { 
		sortField = PROJECT_ID_SORT;
		reverse = false;
	}

	/**
	 * Determine if the projects are currently sorted in reverse order.
	 * @return <code>true</code> if the projects are sorted in reverse order,
	 * <code>false</code> if they are sorted in the standard order.
	 */
	public boolean isReverseSort() { return reverse; }

	/**
	 * Determine if the project list is sorted by the projects' begin/start dates.
	 * @return <code>true</code> if the projects are sorted by the begin/start dates,
	 * <code>false</code> if they are not.
	 */
	public boolean isSortByBeginDate() { return sortField == BEGIN_DATE_SORT; }
	
	/**
	 * Determine if the project list is sorted by the projects' end dates.
	 * @return <code>true</code> if the projects are sorted by the end dates,
	 * <code>false</code> if they are not.
	 */
	public boolean isSortByEndDate() { return sortField == END_DATE_SORT; }
	
	/**
	 * Determine if the project list is sorted by the projects' project IDs.
	 * @return <code>true</code> if the projects are sorted by the project IDs,
	 * <code>false</code> if they are not.
	 */
	public boolean isSortByProjectId() { return sortField == PROJECT_ID_SORT; }
	
	/**
	 * Set the field the project list is to be sorted by.
	 * @param field The id of the field to sort the project list.
	 */
	public void setSortField(int field) { 
		reverse = (sortField == field) ? !reverse : false;
		sortField = field;
	}
	
	/**
	 * Sort the specified project list using the current state of the display bean.
	 * @param projectList The list to be sorted.
	 */
	public void sort(List<ProjectBean> projectList) {
		if (isSortByBeginDate()) {
			Collections.sort(projectList, new ProjectBeginDateComparator(isReverseSort()));
		} else if (isSortByEndDate()) {
			Collections.sort(projectList, new ProjectEndDateComparator(isReverseSort()));
		} else {
			Collections.sort(projectList, new ProjectIdComparator(isReverseSort()));
		}
	}
}
