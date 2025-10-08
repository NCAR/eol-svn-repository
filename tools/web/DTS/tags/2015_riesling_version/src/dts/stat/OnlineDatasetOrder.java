package dts.stat;

import java.util.*;

/**
 * The OnlineDatasetOrder is a DatasetOrder where the data is available through the
 * web.
 *
 * @author Joel Clawson
 */
public class OnlineDatasetOrder implements Comparable<OnlineDatasetOrder>, DatasetOrder {
    
    private Dataset dataset;
    private int fileCount, size;
    private String affiliation, email;
    
    /**
     * Create a new instance of an OnlineDatasetOrder.
     * @param dataset The data set that was ordered.
     * @param email The email address of the user that placed the order.
     * @param affiliation The affiliation of the user that placed the order.
     * @param size The size in KB of the order.
     * @param fileCount The number of files needed to fulfill the order.
     */
    public OnlineDatasetOrder(Dataset dataset, String email, String affiliation, int size, int fileCount) {
	this.dataset = dataset;
	this.email = email;
	this.affiliation = affiliation;
	this.size = size;
	this.fileCount = fileCount;
    }
    
    /**
     * Compare this order with another order for sort order.
     * @param order The order to be compared with this order.
     * @return A negative integer, zero, or positive integer if this order is less than, equal to,
     * or greater than the specified order.
     */
    public int compareTo(OnlineDatasetOrder order) {
	int retVal = getEmail().compareTo(order.getEmail());
	if (retVal == 0) {
	    retVal = getAffiliation().compareTo(order.getAffiliation());
	}
	return retVal;
    }

    /**
     * Get the affiliation of the user that placed the order.
     * @return The user's affiliation.
     */
    public String getAffiliation() { return affiliation == null ? "" : affiliation; }

    /**
     * Get the data set that was ordered.
     * @return The ordered data set.
     */
    public Dataset getDataset() { return dataset; }

    /**
     * Get the email of the user that placed the order.
     * @return The user's email address.
     */
    public String getEmail() { return email == null ? "" : email.trim(); }

    /**
     * Get the number of files ordered.
     * @return The number of files provided for the order.
     */
    public int getFileCount() { return fileCount; }

    /**
     * Get the size of the order.
     * @return The size of the order in KB.
     */
    public int getSize() { return size; }
}
