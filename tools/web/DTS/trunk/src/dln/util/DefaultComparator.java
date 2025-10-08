package dln.util;

import dln.beans.*;
import java.util.*;

/**
 * The DefaultComparator is a generic Comparator to determine the sort order
 * of DefaultBean instances.  It defines the Comparator interface for all extensions
 * and provides the functionality to use a reverse sort.
 * 
 * @author jclawson
 */
public abstract class DefaultComparator<T extends DefaultBean> implements Comparator<T> {

	private boolean reverse;
	
	/**
	 * Create a new instance of a DefaultComparator.
	 * @param reverse if the comparator should use a reverse sort during the comparison.
	 */
	public DefaultComparator(boolean reverse) { this.reverse = reverse; }
	
	/**
	 * Determine if this comparator sorts the beans in reverse of their defined sort order.
	 * @return <code>true</code> if the comparator sorts in reverse, <code>false</code>
	 * if it sorts in the defined order.
	 */
	public boolean isReverseSort() { return reverse; }
}
