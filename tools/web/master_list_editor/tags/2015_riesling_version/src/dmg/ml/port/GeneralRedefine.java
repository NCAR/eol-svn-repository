package dmg.ml.port;

import dmg.ml.*;
import dmg.ml.bean.*;
import java.sql.*;
import java.util.*;

/**
 * The GeneralRedefine class is a class that ports the <code>dmg_ml</code>
 * database to the <code>dmg_general_ml</code> database.  It simplifies
 * the database and removes redundancy by having single instances of information
 * with associations where appropriate.
 *
 * @author Joel Clawson
 **/
public class GeneralRedefine {

    // Define the MySQL User information
    private static final String username = "dts-full";
    private static final String password = "l@micbc";

    // Define the Databases used by the Port Class
    private static final String READ_DB = "jdbc:mysql://data.eol.ucar.edu/dmg_ml";
    private static final String TEST_DB = "jdbc:mysql://data.dev.eol.ucar.edu/dmg_general_ml";
    private static final String FINAL_DB = "jdbc:mysql://data.eol.ucar.edu/dmg_general_ml";

    private boolean testPort;
    private Connection dbRead,dbWrite;

    // Maps for containing information to be put in the database.
    private TreeMap<String,ProjectBean> projects;
    private TreeMap<String,ClassificationBean> categories;
    private TreeMap<String,String> parents;
    private TreeMap<String,DatasetBean> datasetIds;
    private TreeMap<ProjectBean,TreeMap<String,DatasetBean>> datasetNames;
    private HashMap<DatasetBean,TreeSet<ClassificationBean>> datasetCategories;
    private TreeMap<ProjectBean,HashMap<DatasetBean,ProjectDatasetData>> projectDatasetData;

    // Maps for counting data sets.
    private TreeMap<ProjectBean,Integer> categoryCounts;
    private TreeMap<ProjectBean,Integer> createdDatasetCounts;
    private TreeMap<ProjectBean,Integer> mergedDatasetCounts;

    /**
     * Create a new instnace of the GeneralRedefine class.
     * @throws ClassNotFoundException when the Driver for the MySQL database
     * cannot be loaded.
     */
    public GeneralRedefine() throws ClassNotFoundException {
	Class.forName("com.mysql.jdbc.Driver");
	
	projects = new TreeMap<String,ProjectBean>();
	categories = new TreeMap<String,ClassificationBean>();
	parents = new TreeMap<String,String>();

	datasetIds = new TreeMap<String,DatasetBean>();
	datasetNames = new TreeMap<ProjectBean,TreeMap<String,DatasetBean>>();
	projectDatasetData = new TreeMap<ProjectBean,HashMap<DatasetBean,ProjectDatasetData>>();
	datasetCategories = new HashMap<DatasetBean,TreeSet<ClassificationBean>>();

	categoryCounts = new TreeMap<ProjectBean,Integer>();
	createdDatasetCounts = new TreeMap<ProjectBean,Integer>();
	mergedDatasetCounts = new TreeMap<ProjectBean,Integer>();
    }

    /**
     * Remove unnecessary information from the category name and rename values
     * to be combined with similarly named categories.
     * @param name The name of the category to be cleaned.
     * @return The cleaned category name.
     */
    public String cleanCategoryName(String name) {
	name = name.replaceAll("\\s+Data\\s*$","");
	name = name.replaceAll("\\s+Output\\s*$","");
	name = name.replaceAll("^R/V\\s+","");
	if (name.equals("Ship")) { name = "Ship Based"; }
	if (name.equals("Photographs")) { name = "Photography"; }
	if (name.equals("Aircraft P-3")) { name = "NOAA P-3"; }
	return name;
    }

    /**
     * Remove unnecessary information from the dataset name including certain
     * HTML tags, category information, and extranious white space.
     * @param name The name of the data set to be cleaned.
     * @return The cleaned data set name.
     **/
    public String cleanDatasetName(String name) {
	name = name.trim();
	name = name.replaceAll("</?[bB]>"," ");
	
	name = name.replaceAll("^Aircraft\\s*:\\s*","");
	name = name.replaceAll("^Aircraft\\s+Radar\\s*:\\s*","");
	name = name.replaceAll("^GTS\\s*:\\s*","");
	name = name.replaceAll("^Ancillary\\s*:\\s*","");
	name = name.replaceAll("^GPS\\s*:\\s*","");
	name = name.replaceAll("^Land\\s*:\\s*","");
	name = name.replaceAll("^Lidar\\s*:\\s*","");
	name = name.replaceAll("^Mesonet\\s*:\\s*","");
	name = name.replaceAll("^Model\\s*:\\s*","");
	name = name.replaceAll("^Oceanographic\\s*:\\s*","");
	name = name.replaceAll("^Oceanographic\\s+Surface\\s*:\\s*","");
	name = name.replaceAll("^Photography\\s*:\\s*","");
	name = name.replaceAll("^Profiler/Sodar\\s*:\\s*","");
	name = name.replaceAll("^Radar\\s*:\\s*","");
	name = name.replaceAll("^Radiation\\s*:\\s*","");
	name = name.replaceAll("^Satellite\\s*:\\s*","");
	name = name.replaceAll("^Ship\\s*:\\s*","");
	name = name.replaceAll("^Soundings?\\s*:\\s*","");
	name = name.replaceAll("^Surface\\s*:\\s*","");
	name = name.replaceAll("^Upper(\\s+|\\-)Air\\s*:\\s*","");

	name = name.replaceAll("\\s+"," ");
	name = name.trim();
	return name;
    }

    /**
     * Properly shut down any open connections to databases.
     * @throws SQLException if there is a problem closing a connection.
     */
    public void cleanUp() throws SQLException {
	dbRead.close();

	if (dbWrite != null) {
	    dbWrite.rollback();
	    dbWrite.close();
	}
    }

    /**
     * Clean a URL by removing extra slashes and extra white space.
     * @param url The URL to be cleaned.
     * @return The cleaned URL.
     **/
    public String cleanUrl(String url) {
	if (url == null || url.equals("")) { return ""; }
	url = url.trim();
	url = url.replaceAll("/$","");
	url = url.replaceAll("//","/");
	url = url.replaceAll("http:/","http://");
	url = url.replaceAll("ftp:/","ftp://");
	return url;
    }

    /**
     * Create the connection that will read from the database that is being
     * ported.
     * @throws SQLException when there is a problem establishing a connection
     * to the database.
     */
    private void createReadConnection() throws SQLException {
	dbRead = DriverManager.getConnection(READ_DB,username,password);
	dbRead.setAutoCommit(false);
    }

    /**
     * Create the connection that will write to the database that is being
     * ported to.
     * @throws SQLException when there is a problem establishing a connection
     * to the database.
     **/
    private void createWriteConnection() throws SQLException {
	dbWrite = DriverManager.getConnection(testPort ? TEST_DB : FINAL_DB,
					      username,password);
	dbWrite.setAutoCommit(false);
    }

    /**
     * Determine if two boolean values are equal when either value could also
     * be <code>null</code>.
     * @param first The first value to be compared.
     * @param second The second value to be compared.
     * @return <code>true</code> if the two values are equal, <code>false</code>
     * otherwise.
     */
    public boolean equalsBoolean(Boolean first, Boolean second) {
	if (first == null && second != null) { return false; }
	else if (first != null && second == null) { return false; }
	else if (first == null && second == null) { return true; }
	else { return first.equals(second); }
    }

    /**
     * Determine if two dates are equal when either value could also be 
     * <code>null</code>.
     * @param first The first date to be compared.
     * @param second The second date to be compared.
     * @return <code>true</code> if the two dates are equal, <code>false</code>
     * otherwise.
     **/
    public boolean equalsDates(java.sql.Date first, java.sql.Date second) {
	if (first == null && second != null) { return false; }
	else if (first != null && second == null) { return false; }
	else if (first == null && second == null) { return true; }
	else { return first.compareTo(second) == 0; }
    }

    /**
     * Get the local copy of a loaded dataset.
     * @param project The project that requested the data set.
     * @param url The URL of the data set that contains the data set id.
     * @param name The cleaned name of the data set.
     * @return The data set in the local store.
     **/
    public DatasetBean loadDataset(ProjectBean project, String url, String name) {
	if (url != null && url.contains("/codiac/dss/id=")) {
	    return datasetIds.get(url.substring(url.lastIndexOf("=")+1));
	} else {
	    return datasetNames.get(project).get(name);
	}
    }


    public void mergeDataset(ProjectBean project, ResultSet results) throws SQLException {
	// Pull out the data set that is stored for the data set.
	DatasetBean stored = loadDataset(project,cleanUrl(results.getString(2)),
					 cleanDatasetName(results.getString(1)));
	
	//	System.out.println("Merging Dataset: "+stored.getDatasetId()+" "+stored.getName()+" for "+project.getProjectId());

	if (!projectDatasetData.get(project).containsKey(stored)) {
	    //System.out.println("\tAdding Project Specific Data.");
	    ProjectDatasetData data = new ProjectDatasetData(project,stored);
	    if (results.getDate(3) != null) {
		data.setDatePosted(results.getDate(3));
	    }
	    data.setInProgress(results.getBoolean(5));
	    projectDatasetData.get(project).put(stored,data);
	    createdDatasetCounts.put(project,new Integer(createdDatasetCounts.get(project))+1);
	} else {
	    //System.out.println("\tMerging Project Specific Data.");
	    mergedDatasetCounts.put(project,new Integer(mergedDatasetCounts.get(project))+1);
	}
	
	// Don't need to test dataset id.
	// Test Dataset Name.
	if (!cleanDatasetName(results.getString(1)).equals(stored.getName())) {
	    System.out.print("Data set "+stored.getDatasetId()+":");
	    System.out.print(stored.getName()+"-> does not match new");
	    System.out.print(" data set name. (");
	    System.out.println(project.getProjectId()+")");
	    System.out.println("\tStored Name: "+stored.getName());
	    System.out.println("\tNew Name:    "+
			       cleanDatasetName(results.getString(1)));
	    cleanUp();
	    System.exit(1);
	}
	// Test Dataset URL.
	if (!cleanUrl(results.getString(2)).equals(stored.getUrl())) {
	    System.out.print("Dataset "+stored.getDatasetId()+":");
	    System.out.print(stored.getName()+"-> does not match new");
	    System.out.print(" data set URL. (");
	    System.out.println(project.getProjectId()+")");
	    System.out.println("\tStored URL: "+stored.getUrl());
	    System.out.println("\tNew URL:    "+
			       cleanUrl(results.getString(2)));
	    cleanUp();
	    System.exit(1);
	}
	// Test Posted Date for the Project.
	if (!equalsDates(results.getDate(3),projectDatasetData.get(project).get(stored).getDatePosted())) {
	    System.out.print("Dataset "+stored.getDatasetId()+":");
	    System.out.print(stored.getName()+"-> does not match new");
	    System.out.print(" data posted. (");
	    System.out.println(project.getProjectId()+")");
	    System.out.println("\tStored Date: "+projectDatasetData.get(project).get(stored).getDatePosted());
	    System.out.println("\tNew Date:    "+results.getString(3));
	    cleanUp();
	    System.exit(1);
	}
	// Test Doc URL
	if (!cleanUrl(results.getString(4)).equals(stored.getDocUrl())) {
	    System.out.print("Dataset "+stored.getDatasetId()+":");
	    System.out.print(stored.getName()+"-> does not match new");
	    System.out.print(" data set Doc URL. (");
	    System.out.println(project.getProjectId()+")");
	    System.out.println("\tStored URL: "+stored.getDocUrl());
	    System.out.println("\tNew URL:    "+
			       cleanUrl(results.getString(4)));
	    cleanUp();
	    System.exit(1);		    
	}
	// Test In Progress Flag for the Project.
	if (!equalsBoolean(results.getBoolean(5),projectDatasetData.get(project).get(stored).isInProgress())) {
	    System.out.print("Dataset "+stored.getDatasetId()+":");
	    System.out.print(stored.getName()+"-> does not match new");
	    System.out.print(" in progress flag. (");
	    System.out.println(project.getProjectId()+")");
	    System.out.println("\tStored Flag: "+projectDatasetData.get(project).get(stored).isInProgress());
	    System.out.println("\tNew Flag:    "+results.getBoolean(5));
	    cleanUp();
	    System.exit(1);
	}
	// Test Expected Date.
	if (!equalsDates(results.getDate(6),
			 stored.getDateExpected() == null ? null :
			 java.sql.Date.valueOf(stored.getDateExpected()))) {
	    System.out.print("Dataset "+stored.getDatasetId()+":");
	    System.out.print(stored.getName()+"-> does not match new");
	    System.out.print(" date expected. (");
	    System.out.println(project.getProjectId()+")");
	    System.out.println("\tStored Date: "+stored.getDateExpected());
	    System.out.println("\tNew Date:    "+
			       results.getDate(6));
	    cleanUp();
	    System.exit(1);		    
	}
    }

    public void printCountStats() {
	System.out.println("Project: Categories, Created Datasets, Merged Datasets");
	for (ProjectBean project: categoryCounts.keySet()) {
	    System.out.println(String.format("%-15s: %03d, %03d, %03d",project.getProjectId(),
					     categoryCounts.get(project),
					     createdDatasetCounts.get(project),
					     mergedDatasetCounts.get(project)));
	}
    }

    /**
     * Read in the categories from the database.
     * @throws SQLException when there is a problem accessing the category 
     * information from the read database.
     **/
    public void readCategories() throws SQLException {
	for (ProjectBean project: projects.values()) {
	    categoryCounts.put(project,new Integer(0));
	    String sql = "SELECT heading.name,category.name FROM "+
		"head"+project.getProjectId()+" AS heading JOIN "+
		"cat"+project.getProjectId()+" AS category ON "+
		"heading.id=category.heading_id";
	    PreparedStatement stmt = dbRead.prepareStatement(sql);
	    ResultSet results = stmt.executeQuery();
	    
	    // Create a set for holding the categories for the project
	    Set<ClassificationBean> projectCats = new TreeSet<ClassificationBean>();
	    while (results.next()) {
		// Add the category to the project list after cleaning the
		// names and making sure the category does not already exist
		projectCats.add(reconcileCategory(cleanCategoryName(results.getString(1)),cleanCategoryName(results.getString(2))));
		categoryCounts.put(project,categoryCounts.get(project)+1);
	    }
	    results.close();
	    stmt.close();

	    // Add the set of categories to the project.
	    project.setCategories(new ArrayList<ClassificationBean>(projectCats));
	}
    }

    /**
     * Read in the data sets from the database.
     * @throws SQLException when there is a problem accessing the data set
     * information from the read database.
     **/
    public void readDatasets() throws SQLException {
	for (ProjectBean project: projects.values()) {
	    projectDatasetData.put(project,new HashMap<DatasetBean,ProjectDatasetData>());
	    datasetNames.put(project,new TreeMap<String,DatasetBean>());
	    createdDatasetCounts.put(project,new Integer(0));
	    mergedDatasetCounts.put(project,new Integer(0));

	    String sql = "SELECT data_set,link,date,document,in_progress,"+
		"expected_date,name FROM ds"+project.getProjectId()+
		" AS dataset JOIN cat"+project.getProjectId()+" AS category "+
		"ON dataset.category=category.id";
	    PreparedStatement stmt = dbRead.prepareStatement(sql);
	    ResultSet results = stmt.executeQuery();
	    while (results.next()) {
		// Handle the data set when there is a known data set id.
		if (results.getString(2) != null &&
		    results.getString(2).contains("/codiac/dss/id=")) {
		    String datasetId = results.getString(2).
			substring(results.getString(2).lastIndexOf("=")+1);

		    // Create a new Data Set since it doesn't exist.
		    if (!datasetIds.containsKey(datasetId)) {
			//System.out.println("Created dataset: "+datasetId+" "+cleanDatasetName(results.getString(1))+" for project "+project.getProjectId());
			DatasetBean dataset = new DatasetBean();
			dataset.setDatasetId(datasetId);
			dataset.setName(cleanDatasetName(results.getString(1)));
			dataset.setUrl(cleanUrl(results.getString(2)));
			ProjectDatasetData projectData = new ProjectDatasetData(project,dataset);
			createdDatasetCounts.put(project,new Integer(createdDatasetCounts.get(project))+1);
			if (results.getDate(3) != null) {
			    projectData.setDatePosted(results.getDate(3));
			}
			dataset.setDocUrl(cleanUrl(results.getString(4)));
			projectData.setInProgress(results.getBoolean(5));
			if (results.getDate(6) != null) {
			    dataset.setDateExpected(results.getDate(6).toString());
			}
			projectDatasetData.get(project).put(dataset,projectData);
			datasetIds.put(datasetId,dataset);
		    } else {
			mergeDataset(project,results);
		    }
		} 
		// Handle the data set when there is not a data set id.
		else {
		    String name = cleanDatasetName(results.getString(1));
		    // Create a new Data Set since it doesn't exist.
		    if (!datasetNames.get(project).containsKey(name)) {
			//System.out.println("Created dataset: "+name+" for project "+project.getProjectId());
			DatasetBean dataset = new DatasetBean();
			dataset.setName(name);
			dataset.setUrl(cleanUrl(results.getString(2)));
			ProjectDatasetData projectData = new ProjectDatasetData(project,dataset);
			createdDatasetCounts.put(project,new Integer(createdDatasetCounts.get(project))+1);
			if (results.getDate(3) != null) {
			    projectData.setDatePosted(results.getDate(3));
			}
			dataset.setDocUrl(cleanUrl(results.getString(4)));
			projectData.setInProgress(results.getBoolean(5));
			if (results.getDate(6) != null) {
			    dataset.setDateExpected(results.getDate(6).toString());
			}
			projectDatasetData.get(project).put(dataset,projectData);
			datasetNames.get(project).put(name,dataset);
		    } else {
			mergeDataset(project,results);
		    }
		}

		DatasetBean stored = loadDataset(project,cleanUrl(results.getString(2)),
						 cleanDatasetName(results.getString(1)));
		// Add the category to the set for the data set.
		if (datasetCategories.get(stored) == null) {
		    datasetCategories.put(stored,new TreeSet<ClassificationBean>());
		}
		datasetCategories.get(stored).add(categories.get(cleanCategoryName(results.getString(7))));
	    }
	    results.close();
	    stmt.close();
	}
    }

    /**
     * Read in the list of projects in the database.
     * @throws SQLException when there is a problem reading in the projects
     * from the read database.
     **/
    public void readProjects() throws SQLException {
	String sql = "SHOW TABLES LIKE 'ds%'";
	PreparedStatement stmt = dbRead.prepareStatement(sql);
	ResultSet results = stmt.executeQuery();
	while (results.next()) {
	    ProjectBean project = new ProjectBean();
	    project.setProjectId(results.getString(1).substring(2));
	    if (!project.getProjectId().equalsIgnoreCase("TEST")) {
		projects.put(project.getProjectId(),project);
	    }
	}
	results.close();
	stmt.close();
	System.out.println("Read in "+projects.keySet().size()+" projects.");
    }

    /**
     * Merge the heading and category into the local store.
     * @param heading The name of the heading to be reconciled.
     * @param category The name of the category to be reconciled.
     * @return The reconciled category represented by this category name.
     **/
    public ClassificationBean reconcileCategory(String heading, String category)
	throws SQLException {
	if (heading.equals(category)) {
	    if (!categories.containsKey(heading)) {
		ClassificationBean catBean = new ClassificationBean();
		catBean.setName(heading);
		categories.put(heading,catBean);
		parents.put(heading,null);
	    }
	} else {
	    if (!categories.containsKey(heading)) {
		ClassificationBean catBean = new ClassificationBean();
		catBean.setName(heading);
		categories.put(heading,catBean);
		parents.put(heading,null);
	    }
	    if (!categories.containsKey(category)) {
		ClassificationBean catBean = new ClassificationBean();
		catBean.setName(category);
		categories.put(category,catBean);
		
		if (!parents.containsKey(category)) {
		    parents.put(category,heading);
		}
	    }
	}
	
	// Make sure a heading is a top level category without a parent.
	if (parents.get(heading) != null) {
	    System.out.println("Heading "+heading+" does not have a null parent.");
	    cleanUp();
	    System.exit(1);
	}
	
	// Make sure categories always have the same parent.
	if (!category.equals(heading) && 
	    !parents.get(category).equals(heading)) {
	    System.out.println("New heading "+heading+" for category "+category+" does not match the previous value of "+parents.get(category));
	    cleanUp();
	    System.exit(1);
	}
	
	return categories.get(category);
    }

    /**
     * Redefine the general Master List.
     * @throws SQLException when there is a problem reading information from
     * the read database.
     * @throws MasterListException when there is a problem write information to
     * the write database.
     **/
    public void redefineMasterList() throws SQLException,MasterListException {
	createReadConnection();

	readProjects();
	readCategories();
	readDatasets();

	printCountStats();

	createWriteConnection();

	storeCategories();
	storeProjects();
	storeDatasets();
    }

    /**
     * Commit the commands sent to the write database.
     * @throws SQLException if there is a problem saving the commands to the
     * databse.
     **/
    public void save() throws SQLException {
	dbWrite.commit();
    }

    /**
     * Set the flag that marks the port as a test port or a final port.
     * @param testPort <code>true</code> if this is a test port or <code>false
     * </code> if it is a final port.
     **/
    public void setTest(boolean testPort) { this.testPort = testPort; }

    /**
     * Insert the categories into the write database.
     * @throws MasterListException when there is a problem inserting the
     * categories to the database.
     **/
    public void storeCategories() throws MasterListException {
	// First enter the top level (parent) categories
	for (String parentName: parents.keySet()) {
	    if (parents.get(parentName) == null) {
		categories.get(parentName).insert(dbWrite);
	    }
	}
	// Second enter all of the children of the parents.
	for (String categoryName: parents.keySet()) {
	    if (parents.get(categoryName) != null) {
		ClassificationBean category = categories.get(categoryName);
		category.setParentCategoryId(categories.get(parents.get(categoryName)).getCategoryId());
		category.insert(dbWrite);
	    }
	}
    }

    /**
     * Insert the data sets into the write database.
     * @throws MasterListException when there is a problem adding the data set
     * information to the write database.
     **/
    public void storeDatasets() throws MasterListException {
	// Insert the data sets with real data set ids.
	for (DatasetBean dataset: datasetIds.values()) {
	    dataset.setCategories(new ArrayList<ClassificationBean>(datasetCategories.get(dataset)));
	    dataset.insert(dbWrite);
	}
	// Insert the data sets without data set ids.
	for (ProjectBean project: datasetNames.keySet()) {
	    for (DatasetBean dataset: datasetNames.get(project).values()) {
		dataset.setCategories(new ArrayList<ClassificationBean>(datasetCategories.get(dataset)));
		dataset.insert(dbWrite);
	    }
	}

	// Update all of the data sets for each project.
	for (ProjectBean project: projectDatasetData.keySet()) {
	    for (DatasetBean dataset: projectDatasetData.get(project).keySet()) {
		DatasetProjectBean newDs = new DatasetProjectBean(project,dataset);
		newDs.setInProgress(projectDatasetData.get(project).get(dataset).isInProgress());
		if (projectDatasetData.get(project).get(dataset).getDatePosted() != null) {
		    newDs.setDatePosted(projectDatasetData.get(project).get(dataset).getDatePosted().toString());
		}

		/*
		ArrayList<ClassificationBean> cats = new ArrayList<ClassificationBean>();
		for (ClassificationProjectBean category: ClassificationProjectBean.getCategoryList(project,dbWrite)) {
		    if (datasetCategories.get(dataset).contains(category)) {
			cats.add(category);
		    }
		}

		newDs.setCategories(cats);
		*/
		newDs.updateProject(dbWrite);
	    }
	}
    }

    /**
     * Insert all of the projects into the write database.
     * @throws MasterListException when there is a problem inserting the project
     * information into the write database.
     **/
    public void storeProjects() throws MasterListException {
	for (ProjectBean project: projects.values()) {
	    // Set default values for the project.
	    project.setDefaultSystemDirectory(null);
	    project.setDefaultUrl(null);
	    project.setHomePageUrl("http://www.eol.ucar.edu/projects");
	    project.setLogoUrl("http://data.eol.ucar.edu/master_list/images/eol.gif");
	    // Make sure the display name is not null.
	    if (project.getDisplayName() == null) {
		project.setDisplayName("");
	    }

	    project.insert(dbWrite);
	}
    }

    /**
     * Run the porting from the dmg_ml database to the dmg_general_ml database.
     * @param args The type of port to be peformed (test or final).
     * @throws Exception when a problem occurs during the porting.
     **/
    public static void main(String args[]) throws Exception {
	GeneralRedefine redef = new GeneralRedefine();

	// Make sure the type parameter is test or final
	if (args.length > 0 && args[0].equalsIgnoreCase("test")) {
	    redef.setTest(true);
	} else if (args.length > 0 && args[0].equalsIgnoreCase("final")) {
	    redef.setTest(false);
	} else {
	    System.err.println("Usage: java dmg.ml.port.GeneralRedef final|test");
	    System.exit(0);
	}

	// Redefine the database.
	try {
	    redef.redefineMasterList();
	    redef.save();
	} catch (SQLException e) {
	    e.printStackTrace();
	} catch (MasterListException e) {
	    System.out.println(e.getDetails());
	    e.printStackTrace();
	}

	// Shutdown all open connections.
	redef.cleanUp();
    }


    private class ProjectDatasetData {
	
	private ProjectBean project;
	private DatasetBean dataset;
	private boolean inProgress;
	private java.sql.Date date;

	public ProjectDatasetData(ProjectBean project, DatasetBean dataset) {
	    this.project = project;
	    this.dataset = dataset;
	}

	public java.sql.Date getDatePosted() { return date; }

	public boolean isInProgress() { return inProgress; }

	public void setInProgress(boolean flag) { inProgress = flag; }

	public void setDatePosted(java.sql.Date date) { this.date = date; }
    }
}
