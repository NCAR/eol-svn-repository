package dts.stat;

import java.util.*;

/**
 * The Dataset class contains order statistics for an individual data set.
 *
 * @author Joel Clawson
 */
public class Dataset implements Comparable<Dataset> {

    private boolean offline, online;
    private int localhostFileCount, massStoreFileCount, offlineMediaCount;
    private List<OfflineDatasetOrder> offlineOrders;
    private List<OnlineDatasetOrder> onlineOrders;
    private long localhostSize, massStoreSize, offlineMediaSize;
    private Set<String> categories, projects;
    private String datasetId, name;
    
    /**
     * Create a new instance of a Dataset.
     * @param datasetId The EMDAC ID for the dataset.
     * @param name The name/title of the data set.
     * @param online A flag marking this data set as online orderable.
     * @param offline A flag marking this data set as offline orderable.
     */
    public Dataset(String datasetId, String name, boolean online, boolean offline) {
	this.datasetId = datasetId;
	this.name = name;
	this.online = online;
	this.offline = offline;

	categories = new TreeSet<String>();
	offlineOrders = new ArrayList<OfflineDatasetOrder>();
	onlineOrders = new ArrayList<OnlineDatasetOrder>();
	projects = new TreeSet<String>();
    }

    /**
     * Add a category to the data set.
     * @param category The name of the category to be added to the data set.
     */
    public void addCategory(String category) { categories.add(category); }
    
    /**
     * Add a project to the data set.
     * @param projectId The ID of the project to be added to the data set.
     */
    public void addProject(String projectId) { projects.add(projectId); }

    /**
     * Add an offline order to this data set.
     * @param email the email address of the user who placed teh order.
     * @param offlineMediaCount The number of tapes in the placed order.
     * @param medium The name of the medium the data for the order is on.
     */
    public void addOfflineOrder(String email, int offlineMediaCount, String medium) {
	offlineOrders.add(new OfflineDatasetOrder(this, email, medium, offlineMediaCount));
    }
    
    /**
     * Add an online order to this data set.
     * @param email The email address of the user who placed the order.
     * @param affiliation The affiliation of the user who placed the order.
     * @param size The size in KB of the placed order.
     * @param fileCount The number of files in the placed order.
     **/
    public void addOnlineOrder(String email, String affiliation, int size, int fileCount) {
	onlineOrders.add(new OnlineDatasetOrder(this, email, affiliation, size, fileCount));
    }

    /**
     * Compare this Dataset to the specified Dataset for sort order.
     * @param dataset The Dataset to be compared to this one.
     * @return A negative integer, zero, or a positive integer if this Dataset is less than,
     * equal to, or greater than the specified Dataset.
     */
    public int compareTo(Dataset dataset) { return getDatasetId().compareTo(dataset.getDatasetId()); }

    /**
     * Get the set of categories associated to this data set.
     * @return The set of categories for this data set.
     */
    public Set<String> getCategories() { return categories; }

    /**
     * Get the EMDAC ID for this data set.
     * @return The data set's EMDAC ID.
     */
    public String getDatasetId() { return datasetId; }

    /**
     * Get the number of files stored on the local disk for this data set.
     * @return The count of files for this data set on the local disk.
     */
    public int getLocalhostFileCount() { return localhostFileCount; }

    /**
     * Get the total size occupied on the local disk for this data set.
     * @return The size in KB of the files on the local disk.
     */
    public long getLocalhostSize() { return localhostSize; }

    /**
     * Get the number of files stored on the mass store for this data set.
     * @return The count of files for this data set on the mass store.
     */
    public int getMassStoreFileCount() { return massStoreFileCount; }

    /**
     * Get the total size occupied on the mass store for this data set.
     * @return The size in KB of the files on the mass store.
     */
    public long getMassStoreSize() { return massStoreSize; }

    /**
     * Get the name of the data set.
     * @return The data set's name.
     */
    public String getName() { return name; }

    /**
     * Get the number of offline media storing data for this data set.
     * @return The number of offline media storing data for this data set.
     */
    public int getOfflineMediaCount() { return offlineMediaCount; }

    /**
     * Get the total size occupied on offline media for this data set.
     * @return The size in KB of the files on offline media.
     */
    public long getOfflineMediaSize() { return offlineMediaSize; }

    /**
     * Get the number of offline orders placed for this data set.
     * @return The number of offline orderes for this data set.
     */
    public int getOfflineOrderCount() { return offlineOrders.size(); }
    
    /**
     * Get the list of offline orders placed for this data set.
     * @return The list of offline orders placed for this data set.
     */
    public List<OfflineDatasetOrder> getOfflineOrders() { return offlineOrders; }

    /**
     * Get the number of online orders placed for this data set.
     * @return The number of online orders for this data set.
     */
    public int getOnlineOrderCount() { return onlineOrders.size(); }
    
    /**
     * Get the list of online orders placed for this data set.
     * @return The list of online orders placed for this data set.
     */
    public List<OnlineDatasetOrder> getOnlineOrders() { return onlineOrders; }

    /**
     * Get the total number of orders placed for this data set.
     * @return The total number of online and offline orders for this data set.
     */
    public int getOrderCount() { return getOnlineOrderCount() + getOfflineOrderCount(); }

    /**
     * Get the number of unique users (emails) that have ordered this data set.
     * @return The number of users who have ordered this data set.
     */
    public int getUniqueUserCount() {
	Set<String> emails = new TreeSet<String>();
	for (OnlineDatasetOrder order: getOnlineOrders()) {
	    emails.add(order.getEmail().trim().toLowerCase());
	}
	for (OfflineDatasetOrder order: getOfflineOrders()) {
	    emails.add(order.getEmail().trim().toLowerCase());
	}
	return emails.size();
    }

    /**
     * Determine if this data set can be ordered offline.
     * @return <code>true</code> if this data set can be ordered offline, <code>false</code> if it can not.
     */
    public boolean isOfflineOrderable() { return offline; }

    /**
     * Determine if this data set can be ordered online.
     * @return <code>true</code> if this data set can be ordered online, <code>false</code> if it can not.
     */
    public boolean isOnlineOrderable() { return online; }
    
    /**
     * Determine if this data set is an operational data set.  (It is part of the <i>Operational</i> project.)
     * @return <code>true</code> if the data set is operational, <code>false</code> if it is not.
     */
    public boolean isOperational() { return projects.contains("Operational"); }

    /**
     * Determine if this data set is a remote link data set.
     * @return <code>true</code> if the data set can not be ordered, <code>false</code> if it can be ordered.
     */
    public boolean isRemoteLink() { return !isOnlineOrderable() && !isOfflineOrderable(); }

    /**
     * Set the number of files stored on the local disk for this data set.
     * @param fileCount The number of files on local disk.
     */
    public void setLocalhostFileCount(int fileCount) { localhostFileCount = fileCount; }

    /**
     * Set the size in KB of the files stored on the local disk for this data set.
     * @param size The size in KB of the data on local disk.
     */
    public void setLocalhostSize(long size) { localhostSize = size; }

    /**
     * Set the number of files stored on the mass store for this data set.
     * @param fileCount The number of files on the mass store.
     */
    public void setMassStoreFileCount(int fileCount) { massStoreFileCount = fileCount; }

    /**
     * Set the size in KB of the files stored on the mass store for this data set.
     * @param size The size in KB of the data on the mass store.
     */
    public void setMassStoreSize(long size) { massStoreSize = size; }

    /**
     * Set the number of offline media storing data for this data set.
     * @param count The number of offline media.
     */
    public void setOfflineMediaCount(int count) { offlineMediaCount = count; }

    /**
     * Set the size in KB of the files stored on offline media for this data set.
     * @param size The size in KB of the data on offline media.
     */
    public void setOfflineMediaSize(long size) { offlineMediaSize = size; }
}
