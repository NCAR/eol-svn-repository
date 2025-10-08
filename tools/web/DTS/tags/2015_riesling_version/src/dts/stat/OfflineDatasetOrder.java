package dts.stat;

import java.util.*;

/**
 * The OfflineDatasetOrder is a DatasetOrder where the data is available on some
 * form of non-Web related media.  This could be CD's, DVD's, VHS tapes, data tapes,
 * etc.
 *
 * @author Joel Clawson
 */
public class OfflineDatasetOrder implements DatasetOrder {

    private Dataset dataset;
    private int offlineMediaCount;
    private String email, medium;

    /**
     * Create a new instance of an OfflineDatasetOrder.
     * @param dataset The data set that was ordered.
     * @param email The email address of the user that placed the order.
     * @param medium The name of the medium that data for the order was on.
     * @param mediaCount The number of offline media needed to fulfill the order.
     */
    public OfflineDatasetOrder(Dataset dataset, String email, String medium, int mediaCount) {
	this.dataset = dataset;
	this.email = email;
	this.medium = medium;
	this.offlineMediaCount = mediaCount;
    }

    /**
     * Get the data set that placed the order.
     * @return The data set that placed the order.
     */
    public Dataset getDataset() { return dataset; }

    /**
     * Get the email address of the user that placed the order.
     * @return The orderer's email address.
     **/
    public String getEmail() { return email; }

    /**
     * Get the name of the medium that data order was on.
     * @return The order's medium.
     */
    public String getMedium() { return medium; }

    /**
     * Get the number of offline media used to fill the order.
     * @return The number of media for the order.
     */
    public int getOfflineMediaCount() { return offlineMediaCount; }
}