package dln.display;

import java.util.*;

import dln.beans.*;
import dln.util.*;

/**
 * The UserDisplayBean is a JSP session state for maintaining sorting information for the
 * user list.  It maintains which field is currently sorted and in which direction.
 * 
 * @author jclawson
 */
public class UserDisplayBean {

	private boolean reverse;
	private int sortField = -1;
	
	/** Constant to sort the user list by the users' short names. */
	public static int SHORT_NAME_SORT = 0;
	/** Constant to sort the user list by the users' full/person names. */
	public static int NAME_SORT = 1;
	/** Constant to sort the user list by the users' email addresses. */
	public static int EMAIL_SORT = 2;
	/** Constant to sort the user list by the users' active status. */
	public static int ACTIVE_SORT = 3;

	/**
	 * Create a new instance of a UserDisplayBean.  This defaults the display to
	 * sort by the active status in the standard order.
	 */
	public UserDisplayBean() {
		sortField = ACTIVE_SORT;
		reverse = false;
	}
	
	/**
	 * Determine if the users are currently sorted in reverse order.
	 * @return <code>true</code> if the users are sorted in reverse order,
	 * <code>false</code> if they are sorted in the standard order.
	 */
	public boolean isReverseSort() { return reverse; }

	/**
	 * Determine if the user list is sorted by the users' active status.
	 * @return <code>true</code> if the users are sorted by their active status,
	 * <code>false</code> if they are not.
	 */
	public boolean isSortByActiveUser() { return sortField == ACTIVE_SORT; }
	
	/**
	 * Determine if the user list is sorted by the users' email addresses.
	 * @return <code>true</code> if the users are sorted by the email addresses,
	 * <code>false</code> if they are not.
	 */
	public boolean isSortByEmail() { return sortField == EMAIL_SORT; }
	
	/**
	 * Determine if the user list is sorted by the users' full/person name.
	 * @return <code>true</code> if the users are sorted by the names,
	 * <code>false</code> if they are not.
	 */
	public boolean isSortByName() { return sortField == NAME_SORT; }
	
	/**
	 * Determine if the user list is sorted by the users' short names.
	 * @return <code>true</code> if the users are sorted by the short names,
	 * <code>false</code> if they are not.
	 */
	public boolean isSortByShortName() { return sortField == SHORT_NAME_SORT; } 
	
	/**
	 * Set the field the user list is to be sorted by.
	 * @param field The id of the field to sort the user list.
	 */
	public void setSortField(int field) { 
		reverse = (sortField == field) ? !reverse : false;
		sortField = field;
	}

	/**
	 * Sort the specified user list using the current state of the display bean.
	 * @param userList The list to be sorted.
	 */
	public void sort(List<UserBean> userList) {
		if (isSortByName()) {
			Collections.sort(userList, new UserNameComparator(isReverseSort()));
		} else if (isSortByEmail()) {
			Collections.sort(userList, new UserEmailComparator(isReverseSort()));
		} else if (isSortByShortName()) {
			Collections.sort(userList, new UserShortNameComparator(isReverseSort()));
		} else {
			Collections.sort(userList, new UserActiveComparator(isReverseSort()));
		}
	}
}
