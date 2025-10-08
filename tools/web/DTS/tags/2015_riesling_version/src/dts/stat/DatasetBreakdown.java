package dts.stat;

import java.util.*;

/**
 * The DatasetBreakdown class is a container for data set order statistics that are grouped
 * together by a common association that isn't a project or data set.
 *
 * @author Joel Clawson
 */
public class DatasetBreakdown {

    private int operationalOrderCount, projectOrderCount;
    private Set<String> datasets, operationalDatasets, operationalEmails, projectDatasets, projectEmails;
    private String name;

    /**
     * Create a new instance of a DatasetBreakdown.
     * @param name The name of the breakdown.
     */
    public DatasetBreakdown(String name) {
	this.name = name;

	datasets = new TreeSet<String>();
	operationalDatasets = new TreeSet<String>();
	operationalEmails = new TreeSet<String>();
	projectDatasets = new TreeSet<String>();
	projectEmails = new TreeSet<String>();
    }

    /**
     * Add a data set to the breakdown.  It may or may not have been ordered.
     * @param dataset The data set to add to the breakdown.
     */
    public void addDataset(Dataset dataset) { datasets.add(dataset.getDatasetId()); }

    /**
     * Add a data set order to the breakdown.
     * @param order The order to be added to the breakdown.
     */
    public void addOrder(DatasetOrder order) {
	addDataset(order.getDataset());
	if (order.getDataset().isOperational()) {
	    operationalDatasets.add(order.getDataset().getDatasetId());
	    operationalEmails.add(order.getEmail().toLowerCase().trim());
	    operationalOrderCount++;
	} else {
	    projectDatasets.add(order.getDataset().getDatasetId());
	    projectEmails.add(order.getEmail().toLowerCase().trim());
	    projectOrderCount++;
	}
    }

    /**
     * Get the total number of data sets assigned to this breakdown.
     * @return The number of data sets in the breakdown.
     */
    public int getDatasetCount() { return datasets.size(); }

    /**
     * Get the number of orders that have been placed for this breakdown.
     * @return The total number of data set orders for the breakdown.
     */
    public int getDatasetOrderCount() { return getNonOperationalDatasetOrderCount() + getOperationalDatasetOrderCount(); }

    /**
     * Get the name of the breakdown.
     * @return The name of the breakdown.
     */
    public String getName() { return name; }

    /**
     * Get the number of times a non operational data set has been ordered.
     * @return The number of non operational data set orders.
     */
    public int getNonOperationalDatasetOrderCount() { return projectOrderCount; }

    /**
     * Get the number of non operational data sets have been ordered.
     * @return The number of ordered non-operational data sets.
     */
    public int getNonOperationalOrderedDatasetCount() { return projectDatasets.size(); }

    /**
     * Get the number of users (emails) that have ordered non operational data sets.
     * @return The number of user that have ordered non operational data sets.
     */
    public int getNonOperationalUserCount() { return projectEmails.size(); }

    /**
     * Get the number of times an operational data set has been ordered.
     * @return The number of operational data set orders.
     **/
    public int getOperationalDatasetOrderCount() { return operationalOrderCount; }

    /**
     * Get the number of operational data sets have been ordered.
     * @return The number of ordered operational data sets.
     */
    public int getOperationalOrderedDatasetCount() { return operationalDatasets.size(); }

    /**
     * Get the number of users (emails) that have ordered operational data sets.
     * @return The number of users that have ordered operational data sets.
     */
    public int getOperationalUserCount() { return operationalEmails.size(); }

    /**
     * Get the number of data sets that have been ordered at least once.
     * @return The number of ordered data sets.
     */
    public int getOrderedDatasetCount() { return getNonOperationalOrderedDatasetCount() + getOperationalOrderedDatasetCount(); }

    /**
     * Get the number of users (emails) that have placed users for this breakdown.
     * @return The number of users who have ordered data sets.
     */
    public int getUserCount() {
	// Can't just add the non-operational and operational because there could be some overlap.
	Set<String> emails = new TreeSet<String>();
	for (String email: projectEmails) { emails.add(email); }
	for (String email: operationalEmails) { emails.add(email); }
	return emails.size();
    }
}