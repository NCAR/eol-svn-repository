package dts.stat;

import java.sql.*;
import java.util.*;

/**
 * The ProjectStat is a class that accesses and provides project statistics on data sets
 * and order counts from the EMDAC database.
 *
 * @author Joel Clawson
 */
public class ProjectStat {

    private int datasetOrderCountListSize = 10;
    private Set<Dataset> datasets;
    private String projectId;

    /**
     * Create a new instance of a ProjectStat.
     */    
    public ProjectStat() { datasets = new TreeSet<Dataset>(); }

    /**
     * Get a list of the data set breakdowns by EMDAC category.
     * @return The list of category breakdowns for the data sets in the current project.
     */
    public List<DatasetBreakdown> getCategoryBreakdowns() {
        // The container for holding the breakdowns.
	Map<String, DatasetBreakdown> entries = new TreeMap<String, DatasetBreakdown>();

        // Loop through all of the data sets attached to the project.
	for (Dataset dataset: datasets) {
            // Loop through the categories associated to the data set.
	    for (String category: dataset.getCategories()) {

                // Only create a new breakdown if the category hasn't been created before.
		if (!entries.containsKey(category)) {
		    entries.put(category, new DatasetBreakdown(category));
		}

                // Add the data set to the current category breakdown.
		entries.get(category).addDataset(dataset);

                // Loop through the web orders.
		for (OnlineDatasetOrder order: dataset.getOnlineOrders()) {
		    entries.get(category).addOrder(order);
		}

                // Loop through the offline orders.
		for (OfflineDatasetOrder order: dataset.getOfflineOrders()) {
		    entries.get(category).addOrder(order);
		}
	    }
	}

        // Convert the values in the map into a list to be returned.
	return new ArrayList<DatasetBreakdown>(entries.values());
    }

    /**
     * Get the total number of data sets associated with the project.
     * @return The number of data sets in the project.
     */
    public int getDatasetCount() { return datasets.size(); }

    /**
     * Get the number of data sets to display in the most frequent ordered list.
     * @return The number of data sets to display in the frequent order list.
     */
    public int getDatasetCountListSize() {
	return getMaximumDatasetOrderCountListSize() > getDatasetCount() ? 
	    getDatasetCount() : getMaximumDatasetOrderCountListSize();
    }

    /**
     * Get a list of the data set breakdowns by the final email extension from the data set orders.
     * @return The list of email extension breakdowns for the data sets in the current project.
     */
    public List<DatasetBreakdown> getEmailExtensionBreakdowns() {
	// Define the container for holding the breakdowns.
	Map<String, DatasetBreakdown> entries = new TreeMap<String, DatasetBreakdown>();

	// Loop through all of the data sets in the project.
	for (Dataset dataset: datasets) {
	    // Handle web orders first.
	    for (OnlineDatasetOrder order: dataset.getOnlineOrders()) {
		// Determine the extension from the email address.
		String extension = order.getEmail().substring(order.getEmail().lastIndexOf(".")).toLowerCase();

		// Create a new breakdown for the exension if it hasn't already been defined.
		if (!entries.containsKey(extension)) {
		    entries.put(extension, new DatasetBreakdown(extension));
		}

		entries.get(extension).addOrder(order);
	    }
	    // Also handle offline orders.
	    for (OfflineDatasetOrder order: dataset.getOfflineOrders()) {
		// Determine the extension from the email address.
		String extension = order.getEmail().substring(order.getEmail().lastIndexOf(".")).toLowerCase();

		// Create a new breakdown for the exension if it hasn't already been defined.
		if (!entries.containsKey(extension)) {
		    entries.put(extension, new DatasetBreakdown(extension));
		}

		entries.get(extension).addOrder(order);
	    }
	}

	// Convert the mapping into a list.
	return new ArrayList<DatasetBreakdown>(entries.values());
    }
    
    /**
     * Get a connection to the EMDAC database.
     * @return The connection to the database.
     * @throws SQLException if there is a problem connecting to the database.
     **/
    private Connection getEmdacConnection() throws SQLException {
	try { Class.forName("com.mysql.jdbc.Driver"); }
	catch(ClassNotFoundException e) {
	    System.out.println( "Unable to load driver." );
	    e.printStackTrace();
	}
//	return DriverManager.getConnection("jdbc:mysql://tsunami.eol.ucar.edu/zedi8","zediview","look-123");
//	return DriverManager.getConnection("jdbc:mysql://sferic.eol.ucar.edu/zith9","zithview","look-999");
	return DriverManager.getConnection("jdbc:mysql://farskol.eol.ucar.edu/zith9","zithview","look-999");
    }
    
    /**
     * Get the number of files stored on the local disk.
     * @return The number of files stored on the local disk.
     */
    public int getLocalhostFileCount() {
	int count = 0;
	for (Dataset dataset: datasets) { count += dataset.getLocalhostFileCount(); }
	return count;
    }

    /**
     * Get the amount of space taken up on the local disk for the project.
     * @return The local host disk space taken up for the project in GB.
     */
    public float getLocalhostSize() {
	float sum = 0;
	for (Dataset dataset: datasets) { sum += dataset.getLocalhostSize(); }
        // Convert from KB to GB
	return sum / 1024 / 1024;
    }

    /**
     * Get the number of files stored on the mass store for the project.
     * @return The number of project files on the mass store.
     */
    public int getMassStoreFileCount() {
	int count = 0;
	for (Dataset dataset: datasets) { count += dataset.getMassStoreFileCount(); }
	return count;
    }

    /**
     * Get the amount of space taken up on the mass store for the project.
     * @return The mass store disk space in GB.
     */
    public float getMassStoreSize() {
	float sum = 0;
	for (Dataset dataset: datasets) { sum += dataset.getMassStoreSize(); }
	// Convert from KB to GB.
	return sum / 1024 / 1024;
    }

    /**
     * Get the maximum number of data sets to display in the order count lists.
     * @return The maximum number of data sets to display in order count lists.
     **/
    public int getMaximumDatasetOrderCountListSize() { return datasetOrderCountListSize; }

    /**
     * Get a list of the data set breakdowns by the medium which the data sets were orders.
     * @return The list of medium breakdowns for the data set orders in the current project.
     */
    public List<DatasetBreakdown> getMediumBreakdowns() {
	// Define the container for storing the breakdown information.
	Map<String, DatasetBreakdown> entries = new TreeMap<String, DatasetBreakdown>();

	// Need to put web medium here, so it is already created by the time the web order
	// section needs it.
	entries.put("Web", new DatasetBreakdown("Web"));

	// Loop through all of the data sets in the project.
	for (Dataset dataset: datasets) {

	    // Loop through the offline orders
	    for (OfflineDatasetOrder order: dataset.getOfflineOrders()) {
		// Define the medium of the order.
		String medium = order.getMedium();

		// Create a new medium breakdown if the current medium doesn't already exist.
		if (!entries.containsKey(medium)) {
		    entries.put(medium, new DatasetBreakdown(medium));
		}

		entries.get(medium).addOrder(order);
	    }

	    // Loop through the web orders.
	    for (OnlineDatasetOrder order: dataset.getOnlineOrders()) {
		entries.get("Web").addOrder(order);
	    }
	}

	// Convert the medium mapping into a list.
	return new ArrayList<DatasetBreakdown>(entries.values());
    }

    /**
     * Get the list of most ordered data sets for the project.
     * @return The list of top <i>N</i> most ordered data sets for the project.
     */
    public List<Dataset> getMostOrderedDatasets() {
	// Put all of the data sets into a new list.
	List<Dataset> list = new ArrayList<Dataset>(datasets);
	// Sort the data sets by their order count.
	Collections.sort(list, new DatasetOrderCountComparator());
	// Now reverse the list to put the most ordered in front.
	Collections.reverse(list);
	// Return the first N data sets in the list.
	return list.subList(0, getDatasetCountListSize());
    }
    
    /**
     * Get the list of most ordered data sets for the project that are not operational.
     * @return The list of top <i>N</i> most ordered non-operational data sets for the project.
     */
    public List<Dataset> getMostOrderedNonOperationalDatasets() {
	// Create a new list to store the non operational data sets.
	List<Dataset> list = new ArrayList<Dataset>();
	// Loop through all of the data sets and add the non operaional data sets to the list.
	for (Dataset dataset: datasets) {
	    if (!dataset.isOperational()) { list.add(dataset); }
	}
	// Sort the data sets by their order count.
	Collections.sort(list, new DatasetOrderCountComparator());
	// Now reverse the list to put the most ordered in front.
	Collections.reverse(list);
	// Return the first N data sets in the list.
	return list.subList(0, getNonOperationalDatasetCountListSize());
    }
    
    /**
     * Get the total number of data set associated with the project that are not operational.
     * @return The number of non-operational data sets in the project.
     */
    public int getNonOperationalDatasetCount() {
	int count = 0;
	for (Dataset dataset: datasets) {
	    if (!dataset.isOperational()) { count++; }
	}
	return count;
    }

    /**
     * Get the number of data sets to display in the most frequent ordered non-operational list.
     * @return The number of data sets to display in the non-operational frequent order list.
     */
    public int getNonOperationalDatasetCountListSize() {
	return getMaximumDatasetOrderCountListSize() > getNonOperationalDatasetCount() ? 
	    getNonOperationalDatasetCount() : getMaximumDatasetOrderCountListSize();
    }

    /**
     * Get the number of files stored on the local disk that are not operational.
     * @return The number of non operational project files on the local disk.
     */
    public int getNonOperationalLocalhostFileCount() {
	int count = 0;
	for (Dataset dataset: datasets) {
	    if (!dataset.isOperational()) { count += dataset.getLocalhostFileCount(); }
	}
	return count;
    }

    /**
     * Get the amount of space taken up on the local disk for the project's non operational data.
     * @return The local host disk space in GB.
     */
    public float getNonOperationalLocalhostSize() {
	float sum = 0;
	for (Dataset dataset: datasets) {
	    if (!dataset.isOperational()) { sum += dataset.getLocalhostSize(); }
	}
	// Convert from KB to GB.
	return sum / 1024 / 1024;
    }

    /**
     * Get the number of files stored on the mass store for the project that are not operational.
     * @return The number of non operational project files on the mass store.
     */
    public int getNonOperationalMassStoreFileCount() {
	int count = 0;
	for (Dataset dataset: datasets) {
	    if (!dataset.isOperational()) { count += dataset.getMassStoreFileCount(); }
	}
	return count;
    }

    /**
     * Get the amount of spacke taken up on the mass store for the project's non operational data.
     * @return The mass store disk space in GB.
     */
    public float getNonOperationalMassStoreSize() {
	float sum = 0;
	for (Dataset dataset: datasets) {
	    if (!dataset.isOperational()) { sum += dataset.getMassStoreSize(); }
	}
	// Convert from KB to GB.
	return sum / 1024 / 1024;
    }

    /**
     * Get the number of non operational data sets that are offline orderable.
     * @return The number of non-operational offline orderable data sets.
     */
    public int getNonOperationalOfflineDatasetCount() {
	int total = 0;
	for (Dataset dataset: datasets) {
	    if (!dataset.isOperational() && dataset.isOfflineOrderable()) { total++; }
	}
	return total;
    }

    /**
     * Get the number of offline media that are attached to this project's non-operational data sets.
     * @return The number of non-operational offline media for the project.
     */
    public int getNonOperationalOfflineMediaCount() {
	int count = 0;
	for (Dataset dataset: datasets) {
	    if (!dataset.isOperational()) { count += dataset.getOfflineMediaCount(); }
	}
	return count;
    }

    /**
     * Get the amount of disk space occupied by non-operational files on the offline media.
     * @return The disk space taken up on non-operational offline media in GB.
     */
    public float getNonOperationalOfflineMediaSize() {
	float total = 0;
	for (Dataset dataset: datasets) {
	    if (!dataset.isOperational()) { total += dataset.getOfflineMediaSize(); }
	}
	return total / 1024 / 1024;
    }

    /**
     * Get the number of offline orders that have been placed for non-operational data sets for this project.
     * @return The number of offline orders placed for the non-operational data sets in this project.
     **/ 
    public int getNonOperationalOfflineOrderCount() {
	int count = 0;
	for (Dataset dataset: datasets) {
	    if (!dataset.isOperational()) { count += dataset.getOfflineOrderCount(); }
	}
	return count;
    }
    
    /**
     * Get the number of non-operational data sets that are online orderable.
     * @return The number of non-operational online orderable data sets.
     */
    public int getNonOperationalOnlineDatasetCount() {
	int total = 0;
	for (Dataset dataset : datasets) {
	    if (!dataset.isOperational() && dataset.isOnlineOrderable()) { total++; }
	}
	return total;
    }

    /**
     * Get the total number of files for the project that are not operational.
     * @return The number of non operational project files on the local disk and mass store.
     */
    public int getNonOperationalOnlineFileCount() {
	return getNonOperationalLocalhostFileCount() + getNonOperationalMassStoreFileCount();
    }

    /**
     * Get the number of orders that have been placed for non-operational data sets for this project.
     * @return The number of orders placed for the non-operational data sets in this project.
     */
    public int getNonOperationalOnlineOrderCount() {
	int count = 0;
	for (Dataset dataset: datasets) {
	    if (!dataset.isOperational()) { count += dataset.getOnlineOrderCount(); }
	}
	return count;
    }

    /**
     * Get the amount of data that has been requested by the users for non-operational data sets.
     * @return The size of the non-operational data that has been ordered for the project in GB.
     */
    public float getNonOperationalOnlineOrderSize() {
	float total = 0;
	for (Dataset dataset: datasets) {
	    if (!dataset.isOperational()) {
		for (OnlineDatasetOrder order: dataset.getOnlineOrders()) {
		    total += order.getSize();
		}
	    }
	}
	// Convert from KB to GB.
	return total / 1024 / 1024;
    }

    /**
     * Get the total amount of disk space used by the project for non operational data.
     * @return The amount of disk space on the local disk and mass store in GB.
     **/
    public float getNonOperationalOnlineSize() {
	return getNonOperationalLocalhostSize() + getNonOperationalMassStoreSize();
    }

    /**
     * Get the number of files that have been ordered that are not operational.
     * @return The number of non-operational data set files that have been ordered.
     */
    public int getNonOperationalOrderedFileCount() {
	int total = 0;
	for (Dataset dataset: datasets) {
	    if (!dataset.isOperational()) {
		for (OnlineDatasetOrder order: dataset.getOnlineOrders()) {
		    total += order.getFileCount();
		}
	    }
	}
	return total;
    }

    /**
     * Get the number of offline media that have been ordered for non-operational data sets.
     * @return The number of non-operational media ordered.
     */
    public int getNonOperationalOrderedOfflineMediaCount() {
	int total = 0;
	for (Dataset dataset: datasets) {
	    if (!dataset.isOperational()) {
		for (OfflineDatasetOrder order: dataset.getOfflineOrders()) {
		    total += order.getOfflineMediaCount();
		}
	    }
	}
	return total;
    }

    /**
     * Get the number of non-operational remote link data sets for the project.
     * @return The number of non-operational remote link data sets.
     */
    public int getNonOperationalRemoteLinkDatasetCount() {
	int total = 0;
	for (Dataset dataset: datasets) {
	    if (!dataset.isOperational() && dataset.isRemoteLink()) { total++; }
	}
	return total;
    }

    /**
     * Get the number of non-operational offline data sets that have been ordered at least once.
     * @return The number of non-operational offline ordered data sets for the project.
     */
    public int getNonOperationalUniqueOfflineOrderedDatasetCount() {
	Set<Dataset> unique = new TreeSet<Dataset>();
	for (Dataset dataset: datasets) {
	    if (dataset.getOfflineOrderCount() > 0 && !dataset.isOperational()) { unique.add(dataset); }
	}
	return unique.size();
    }

    /**
     * Get the number of individual users (emails) that have ordered non-operational offline data sets.
     * @return The number of unique emails that have ordered non-operational offline data sets for the project.
     */
    public int getNonOperationalUniqueOfflineUserCount() {
	Set<String> emails = new TreeSet<String>();
	for (Dataset dataset: datasets) {
	    if (!dataset.isOperational()) {
		for (OfflineDatasetOrder order: dataset.getOfflineOrders()) {
		    if (order.getEmail() != null && !order.getEmail().trim().equals("")) { emails.add(order.getEmail().toLowerCase()); }
		}
	    }
	}
	return emails.size();
    }

    /**
     * Get the number of non-operational project data sets that have been ordered at least once.
     * @return The number of non-operational ordered data sets for the project.
     */
    public int getNonOperationalUniqueOnlineOrderedDatasetCount() {
	Set<Dataset> unique = new TreeSet<Dataset>();
	for (Dataset dataset: datasets) {
	    if (dataset.getOnlineOrderCount() > 0 && !dataset.isOperational()) { unique.add(dataset); }
	}
	return unique.size();
    }

    /**
     * Get the number of individual users (emails) that have ordered non-operational online data sets.
     * @return The number of unique emails that have ordered non-operational online data sets for the project.
     */
    public int getNonOperationalUniqueOnlineUserCount() {
	Set<String> emails = new TreeSet<String>();
	for (Dataset dataset: datasets) {
	    if (!dataset.isOperational()) {
		for (OnlineDatasetOrder order: dataset.getOnlineOrders()) {
		    if (order.getEmail() != null && !order.getEmail().trim().equals("")) { emails.add(order.getEmail().toLowerCase()); }
		}
	    }
	}
	return emails.size();
    }

    /**
     * Get the number of data sets that are offline orderable.
     * @return The number of offline orderable data sets.
     */
    public int getOfflineDatasetCount() {
	int total = 0;
	for (Dataset dataset: datasets) {
	    if (dataset.isOfflineOrderable()) { total++; }
	}
	return total;
    }

    /**
     * Get the number of offline media that are attached to this project's data sets.
     * @return The number of offline media for the project.
     */
    public int getOfflineMediaCount() {
	int count = 0;
	for (Dataset dataset: datasets) { count += dataset.getOfflineMediaCount(); }
	return count;
    }

    /**
     * Get the amount of disk space occupied by the files on the offline media.
     * @return The disk space taken up on offline media in GB.
     */
    public float getOfflineMediaSize() {
	float total = 0;
	for (Dataset dataset: datasets) { total += dataset.getOfflineMediaSize(); }
	return total / 1024 / 1024;
    }

    /**
     * Get the number of offline orders that have been placed for this project.
     * @return The number of offline orders placed for the data sets in this project.
     */
    public int getOfflineOrderCount() {
	int count = 0;
	for (Dataset dataset: datasets) { count += dataset.getOfflineOrderCount(); }
	return count;
    }

    /**
     * Get the number of data sets that are online orderable.
     * @return The number of online orderable data sets.
     */
    public int getOnlineDatasetCount() {
	int total = 0;
	for (Dataset dataset : datasets) {
	    if (dataset.isOnlineOrderable()) { total++; }
	}
	return total;
    }

    /**
     * Get the total number of files for the project.
     * @return The number of project files on the local disk and mass store.
     */
    public int getOnlineFileCount() { return getLocalhostFileCount() + getMassStoreFileCount(); }

    /**
     * Get the number of orders that have been placed for this project.
     * @return The number of orders placed for the data sets in this project.
     */
    public int getOnlineOrderCount() {
	int count = 0;
	for (Dataset dataset: datasets) { count += dataset.getOnlineOrderCount(); }
	return count;
    }
    
    /**
     * Get the amount of data that has been requested by the users.
     * @return The size of the data that has been ordered for the project in GB.
     */
    public float getOnlineOrderSize() {
	float total = 0;
	for (Dataset dataset: datasets) {
	    for (OnlineDatasetOrder order: dataset.getOnlineOrders()) {
		total += order.getSize();
	    }
	}
	// Convert from KB to GB.
	return total / 1024 / 1024;
    }

    /**
     * Get the total amount of disk space used by the project.
     * @return The amount of disk space on the local disk and mass store in GB.
     */
    public float getOnlineSize() { return getLocalhostSize() + getMassStoreSize(); }

    /**
     * Get the number of files that have been ordered.
     * @return The number of ordered files for the project.
     */
    public int getOrderedFileCount() {
	int total = 0;
	for (Dataset dataset: datasets) {
	    for (OnlineDatasetOrder order: dataset.getOnlineOrders()) {
		total += order.getFileCount();
	    }
	}
	return total;
    }

    /**
     * Get the number of offline media that have been ordered for the project.
     * @return The number of media ordered.
     */
    public int getOrderedOfflineMediaCount() {
	int total = 0;
	for (Dataset dataset: datasets) {
	    for (OfflineDatasetOrder order: dataset.getOfflineOrders()) {
		total += order.getOfflineMediaCount();
	    }
	}
	return total;
    }

    /**
     * Get the project ID for the current project.
     * @return The current project's EMDAC project ID.
     */
    public String getProjectId() { return projectId; }

    /**
     * Get the list of all projects in the EMDAC database.
     * @return The sorted list of all projects in the database.
     * @throws SQLException if there is a problem reading the projects from the database.
     */
    public List<String> getProjectList() throws SQLException {
	// Define the container to hold the projects.
	List<String> projects = new ArrayList<String>();

	// Get a connection to the database.
	Connection connection = getEmdacConnection();

	// Define the statement and execute on the database stream.
//	String sql = "SELECT project_id FROM project ORDER BY project_id";
	String sql = "SELECT name FROM project ORDER BY name";
	PreparedStatement stmt = connection.prepareStatement(sql);
	ResultSet results = stmt.executeQuery();

	// Loop through the results and add each project to the list.
	while (results.next()) { projects.add(results.getString(1)); }
	
	// Close all open streams, ignoring any errors.
	try { results.close(); } catch (SQLException e) {}
	try { stmt.close(); } catch (SQLException e) {}
	try { connection.close(); } catch (SQLException e) {}

	return projects;
    }

    /**
     * Get the number of remote linke data sets for the project.
     * @return The number of remote link data sets.
     */
    public int getRemoteLinkDatasetCount() {
	int total = 0;
	for (Dataset dataset: datasets) {
	    if (dataset.isRemoteLink()) { total++; }
	}
	return total;
    }

    /**
     * Get the number of offline project data sets that have been ordered at least once.
     * @return The number of offline ordered data sets for the project.
     */
    public int getUniqueOfflineOrderedDatasetCount() {
	Set<Dataset> unique = new TreeSet<Dataset>();
	for (Dataset dataset: datasets) {
	    if (dataset.getOfflineOrderCount() > 0) { unique.add(dataset); }
	}
	return unique.size();
    }

    /**
     * Get the number of individual users (emails) that have ordered offline data sets.
     * @return The number of unique emails that have ordered offline data sets for the project.
     */
    public int getUniqueOfflineUserCount() {
	Set<String> emails = new TreeSet<String>();
	for (Dataset dataset: datasets) {
	    for (OfflineDatasetOrder order: dataset.getOfflineOrders()) {
		if (order.getEmail() != null && !order.getEmail().trim().equals("")) { emails.add(order.getEmail().toLowerCase()); }
	    }
	}
	return emails.size();
    }

    /**
     * Get the number of project data sets that have been ordered at least once.
     * @return The number of ordered data sets for the project.
     */
    public int getUniqueOnlineOrderedDatasetCount() {
	Set<Dataset> unique = new TreeSet<Dataset>();
	for (Dataset dataset: datasets) {
	    if (dataset.getOnlineOrderCount() > 0) { unique.add(dataset); }
	}
	return unique.size();
    }
    
    /**
     * Get the number of individual users (emails) that have ordered online data sets.
     * @return The number of unique emails that have ordered online data sets for the project.
     */
    public int getUniqueOnlineUserCount() {
	Set<String> emails = new TreeSet<String>();
	for (Dataset dataset: datasets) {
	    for (OnlineDatasetOrder order: dataset.getOnlineOrders()) {
		if (order.getEmail() != null && !order.getEmail().trim().equals("")) { emails.add(order.getEmail().toLowerCase()); }
	    }
	}
	return emails.size();
    }
    
    /**
     * Load the project information into this stat object.
     * @param projectId The projectId that is to have the data loaded.
     * @throws SQLException if there is a problem reading the database.
     */
    public void load(String projectId) throws SQLException {
	this.projectId = projectId;
	
	// Get a connection to the EMDAC database.
	Connection connection = getEmdacConnection();
	
	// Load the basic data set information.
//	String sql = "SELECT dataset.dataset_id, name, onlineorderable, offlineorderable FROM dataset JOIN dataset_project ON dataset.dataset_id=dataset_project.dataset_id WHERE project_id=?";
	String sql = "SELECT archive_ident, dataset.title, online_orderable, offline_orderable FROM dataset JOIN dataset_project ON dataset.id=dataset_project.dataset_id JOIN project ON project.id=dataset_project.project_id WHERE project.name=?";
	PreparedStatement stmt = connection.prepareStatement(sql);
	stmt.setString(1, projectId);
	ResultSet results = stmt.executeQuery();
	while (results.next()) {
	    datasets.add(new Dataset(results.getString(1), results.getString(2), results.getBoolean(3), results.getBoolean(4)));
	}
	try { results.close(); } catch (SQLException e) {}
	try { stmt.close(); } catch (SQLException e) {}
	
	// Load the projects aassociated to each loaded data set.
//	sql = "SELECT project_id FROM dataset_project WHERE dataset_id=?";
	sql = "SELECT name FROM project JOIN dataset_project ON project.id=dataset_project.project_id JOIN dataset ON dataset.id=dataset_project.dataset_id WHERE archive_ident=?";
	stmt = connection.prepareStatement(sql);
	for (Dataset dataset: datasets) {
	    stmt.setString(1, dataset.getDatasetId());
	    results = stmt.executeQuery();
	    while (results.next()) { dataset.addProject(results.getString(1)); }
	    try { results.close(); } catch (SQLException e) {}
	}
	try { stmt.close(); } catch (SQLException e) {}
	
	// Load the EMDAC categories for each loaded data set.
//	sql = "SELECT name FROM category JOIN dataset_category ON category.category_id=dataset_category.category_id WHERE dataset_id=?";
	sql = "SELECT category.name FROM category JOIN dataset_category ON category.id=dataset_category.category_id JOIN dataset ON dataset.id=dataset_category.dataset_id WHERE archive_ident=?";
	stmt = connection.prepareStatement(sql);
	for (Dataset dataset: datasets) {
	    stmt.setString(1, dataset.getDatasetId());
	    results = stmt.executeQuery();
	    while (results.next()) {
		dataset.addCategory(results.getString(1));
	    }
	    try { results.close(); } catch (SQLException e) {}
	}
	try { stmt.close(); } catch (SQLException e) {}

	// Load the online orders for each loaded data set.
//	sql = "SELECT email, affiliation, size_kb, num_files delivery_date FROM codiac_web_orders WHERE dataset_id=?";
	sql = "SELECT email, affiliation, size_kb, num_files, delivery_date FROM codiac_web_order_log WHERE archive_ident=?";
	stmt = connection.prepareStatement(sql);
	for (Dataset dataset: datasets) {
	    stmt.setString(1, dataset.getDatasetId());
	    results = stmt.executeQuery();
	    while (results.next()) {
		dataset.addOnlineOrder(results.getString(1), results.getString(2), results.getInt(3), results.getInt(4));
	    }
	    try { results.close(); } catch (SQLException e) {}
	}
	try { stmt.close(); } catch (SQLException e) {}

	// Load the offline orders for each loaded data set.
//	sql = "SELECT email, num_tapes, medium_name FROM codiac_tape_orders JOIN medium ON codiac_tape_orders.medium_id=medium.medium_id WHERE dataset_id=?";
	sql = "SELECT email, medium.name, num_tapes FROM codiac_tape_order_log JOIN medium ON codiac_tape_order_log.medium_id=medium.id WHERE archive_ident=?";
	stmt = connection.prepareStatement(sql);
	for (Dataset dataset: datasets) {
	    stmt.setString(1, dataset.getDatasetId());
	    results = stmt.executeQuery();
	    while (results.next()) {
//		dataset.addOfflineOrder(results.getString(1), results.getInt(2), results.getString(3));
		dataset.addOfflineOrder(results.getString(1), results.getInt(3), results.getString(2));
	    }
	    try { results.close(); } catch (SQLException e) {}
	}
	try { stmt.close(); } catch (SQLException e) {}

	// Load the online localhost file stats for each loaded data set.
//	sql = "SELECT COUNT(*) AS file_count, SUM(size_kb) FROM file WHERE dataset_id=? AND host='localhost'";
	sql = "SELECT COUNT(*) AS file_count, SUM(size_kb) FROM file JOIN dataset ON file.dataset_id=dataset.id WHERE archive_ident=? AND host='localhost'";
	stmt = connection.prepareStatement(sql);
	for (Dataset dataset: datasets) {
	    stmt.setString(1, dataset.getDatasetId());
	    results = stmt.executeQuery();
	    if (results.next()) {
		dataset.setLocalhostFileCount(results.getInt(1));
		dataset.setLocalhostSize(results.getLong(2));
	    }
	    try { results.close(); } catch (SQLException e) {}
	}
	try { stmt.close(); } catch (SQLException e) {}

	// Load the online mass store file stats for each loaded data set.
//	sql = "SELECT COUNT(*) AS file_count, SUM(size_kb) FROM file WHERE dataset_id=? AND host='mass_store'";
	sql = "SELECT COUNT(*) AS file_count, SUM(size_kb) FROM file JOIN dataset ON file.dataset_id=dataset.id WHERE archive_ident=? AND (host='hpss' OR host='mass_store')";
	stmt = connection.prepareStatement(sql);
	for (Dataset dataset: datasets) {
	    stmt.setString(1, dataset.getDatasetId());
	    results = stmt.executeQuery();
	    if (results.next()) {
		dataset.setMassStoreFileCount(results.getInt(1));
		dataset.setMassStoreSize(results.getLong(2));
	    }
	    try { results.close(); } catch (SQLException e) {}
	}
	try { stmt.close(); } catch (SQLException e) {}
		
	// Load the offline file stats for each loaded data set.
//	sql = "SELECT COUNT(*) AS tape_count, SUM(size_kb) FROM tape WHERE dataset_id=?";
	sql = "SELECT COUNT(*) AS tape_count, SUM(size_kb) FROM tape JOIN dataset ON tape.dataset_id=dataset.id WHERE archive_ident=?";
	stmt = connection.prepareStatement(sql);
	for (Dataset dataset: datasets) {
	    stmt.setString(1, dataset.getDatasetId());
	    results = stmt.executeQuery();
	    if (results.next()) {
		dataset.setOfflineMediaCount(results.getInt(1));
		dataset.setOfflineMediaSize(results.getLong(2));
	    }
	    try { results.close(); } catch (SQLException e) {}
	}
	try { stmt.close(); } catch (SQLException e) {}
		
		
	// Close the connection to the database, ignoring any errors.
	try { connection.close(); } catch (SQLException e) {}
    }
    
    /**
     * Set the maximum size of the order count lists.
     * @param count The maximum size of the order count lists.
     */
    public void setMaximumDatasetOrderCountListSize(int count) { datasetOrderCountListSize = count; }
}
