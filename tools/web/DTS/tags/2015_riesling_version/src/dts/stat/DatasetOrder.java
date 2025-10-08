package dts.stat;

/**
 * The DatasetOrder interface is used to define a data set order.  It is to
 * provide a commonality between online and offline orders.
 *
 * @author Joel Clawson
 */
public interface DatasetOrder {

    /**
     * Get the data set that was ordered.
     * @return The ordered data set.
     */
    public Dataset getDataset();

    /**
     * Get the email address of the user that placed this order.
     * @return The orderer's email address.
     */
    public String getEmail();
}