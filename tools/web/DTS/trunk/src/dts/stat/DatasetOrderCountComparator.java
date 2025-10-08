package dts.stat;

import java.util.*;

/**
 * The DatasetOrderCountComparator is a comparator used to sort Datasets by the
 * number of orders that have been placed for the data sets.
 *
 * @author Joel Clawson
 **/
public class DatasetOrderCountComparator implements Comparator<Dataset> {
    
    /**
     * Compare two data sets for sort order by the number of times they were ordered.
     * @param first The first data set to be compared.
     * @param second The second data set to be compared.
     * @return A negative integer, zero, or positive integer if the number of orders for the
     * first data set is less than, equal to, or greater than the number of orders for the
     * second data set.
     */
    public int compare(Dataset first, Dataset second) {
	return (new Integer(first.getOrderCount())).compareTo(second.getOrderCount());
    }
}
