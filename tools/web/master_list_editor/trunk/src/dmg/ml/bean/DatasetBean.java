package dmg.ml.bean;

import dmg.jsf.TreeException;
import dmg.jsf.model.TreeNode;
import dmg.ml.DatabaseAccessBean;
import dmg.ml.MasterListException;
import dmg.ml.bean.MasterListBean;
import dmg.ml.manager.MasterListManager;
import dmg.ml.manager.state.TreeState;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.ResourceBundle;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;

/**
 * <p>The DatasetBean class is a representation of the data set for the general
 * Master List.  It contains:</p>
 * <ul>
 *    <li>Dataset Id- The unique identifier for the data set.</li>
 *    <li>Name- The name/title of the data set.</li>
 *    <li>URL- The URL where the data set can be accessed.</li>
 *    <li>Document URL- The URL for the documentation for the data set.</li>
 *    <li>Expected Date- The date the data set is expected to be made available.
 * </li>
 *    <li>Updated Date- The date the data set was updated in the database.</li>
 * </ul>
 * <p>The DatasetBean also contains an <code>originalId</code> value.  This is
 * the same as the <code>dataset_id</code> in most cases.  It must be set any
 * time that the data set is read from the database and expects to be updated.
 * This allows a user to change a data set's dataset_id without loosing the
 * initial value and properly update the database.</p>
 *
 * @author Joel Clawson
 */
public class DatasetBean extends MasterListBean<DatasetBean> {
    
    private boolean preliminary;
    private String dateExpected,dateUpdated;
    private List<ClassificationBean> classifications;
    private String authorPi,datasetId,docUrl,name,originalId,url;
    protected SimpleDateFormat dateFormat;
    
    /**
     * Create a new instance of a DatasetBean.
     **/
    public DatasetBean() { this(null); }
    
    /**
     * Create a new instance of a DatasetBean.
     * @param state The container of the expansion state for this dataset bean.
     **/
    public DatasetBean(TreeState state) { 
        super(state);
        dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        dateFormat.setLenient(false);
    }

    /**
     * Determine if the specified data set is before, equal to, or after this
     * data set.  This uses the name of the data set for comparing.
     * @param dataset The data set to be compared to this data set.
     * @return A negative integer, zero, or a positive integer if this data set
     * is less than, equal to, or greater than the specified data set.
     **/    
    public int compareTo(DatasetBean dataset) {
	return (getName()+" ["+getAuthorPi()+"]").compareTo(dataset.getName()+" ["+dataset.getAuthorPi()+"]");
    }

    /**
     * Convert a date String in YYYY-MM-DD format to a Date object used by SQL.
     * @param date The date String in YYYY-MM-DD format.
     * @return The date String as a Date object or <code>null</code> if the 
     * date String is <code>null</code> or is the empty String.
     * @throws MasterListException when the date is not in YYYY-MM-DD format or
     * if the date is not a valid date.
     **/
    protected java.sql.Date convertDate(String date) throws MasterListException{
        if (date == null || date.equals("")) { return null; }
        else {
            try {
                return new java.sql.Date(dateFormat.parse(date).getTime());
            } catch (ParseException e) {
                throw new MasterListException("Unable to parse date: "+date+"."+
                        " The date must be a valid date in YYYY-MM-DD format "+
                        " and must be a valid date.",e);
            }
        }
    }
    
    /**
     * Delete the data set from the database along with all of its associated
     * classifications.
     * @param conn The connection to execute the delete statement on.
     * @throws MasterListException when there is a problem deleting the data
     * set from the database.
     **/
    public void delete(Connection conn) throws MasterListException {
        try {
            // Prepare the statement and delete the data set from the database.
            String sql = "DELETE FROM dataset WHERE dataset_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,getDatasetId());
            stmt.execute();
        } catch (SQLException e) {
            // Convert the SQLException to a MasterListException.
            throw new MasterListException("There was an error deleting data set: "+
					  getDatasetId()+" from the database.",e);
        }
    }

    /**
     * Get the author/PI of this data set.
     * @return The data set's author/PI.
     **/
    public String getAuthorPi() { return authorPi; }

    /**
     * Get the list of classifications associated with this data set.
     * @return The list of associated classifications for the data set.
     * @throws MasterListException if there is a problem generating the list
     * of classifications.
     **/
    public List<ClassificationBean> getClassifications() throws MasterListException {
        return ClassificationBean.getClassifications(this);
    }

    /**
     * Get the list of children for this node.
     * @return An empty list since a dataset cannot have children.
     * @throws TreeException This will never occur but is required by the
     * TreeNode interface.
     **/
    public List<TreeNode> getChildren() throws TreeException {
        return new ArrayList<TreeNode>();
    }

    /**
     * Get the name of the facet that displays a <i>closed</i> dataset.
     * @return The name of the closed facet for the node.
     **/
    protected String getClosedFacetName() { return "closedDataset"; }
    
    /**
     * Get the unique identifier for this data set.
     * @return The data set's unique identifier.
     **/
    public String getDatasetId() { return datasetId; }
    
    /**
     * Get the date the data for the data set is expected to be available.
     * @return The expected date for the data set's data.
     **/
    public String getDateExpected() { return dateExpected; }

    /**
     * Get the date the data set was updated in the data archive system.
     * @return The updated date for the data set's data.
     **/
    public String getDateUpdated() { return dateUpdated; }

    /**
     * Get the URL where the document for the data set can be accessed.
     * @return The data set's document's URL.
     **/
    public String getDocUrl() { return docUrl; }
    
    /**
     * Get a unique identification number for the data set.
     * @return A unique identifier for this data set.
     **/
    public Integer getId() { return new Integer(getDatasetId().hashCode()); }
    
    /**
     * Get the name of the facet that displays a <i>leaf</i> dataset.
     * @return The name of the leaf facet for the node.
     **/
    protected String getLeafFacetName() { return "leafDataset"; }

    /**
     * Get the name of this data set.
     * @return The data set's name.
     **/
    public String getName() { return name; }
    
    /**
     * Get the name of the facet that displays a <i>open</i> dataset.
     * @return The name of the open facet for the node.
     **/
    protected String getOpenFacetName() { return "openDataset"; }
    
    /**
     * Get the initial identifier for the data set.
     * @return The identifier of the data set that is currently in the database.
     **/
    public String getOriginalId() { return originalId; }

    /**
     * Get the list of projects associated with this data set.
     * @return The list of associated projects for the data set.
     * @throws MasterListException if there is a problem generating the list
     * of projects.
     **/
    public List<ProjectBean> getProjects() throws MasterListException {
        return ProjectBean.getProjectList(this);
    }
    
    /**
     * Get the URL where the data set can be accessed.
     * @return The data set's access URL.
     **/
    public String getUrl() { return url; }
    
    /**
     * Insert the data set into the Master List database including associating
     * its classifications.
     * @param conn The connection to use to execute the SQL statements.
     * @throws MasterListException when there is a problem inserting the data
     * set into the database.
     **/
    public void insert(Connection conn) throws MasterListException {
        if (getName() == null || getName().equals("")) {
            throw new MasterListException("Data set name cannot be empty.");
        }
        java.sql.Date updatedDate = convertDate(getDateUpdated());
        java.sql.Date expectedDate = convertDate(getDateExpected());

        try {
            // If the datasetId has not been set, figure out what the next
            // Master List id available is.
            if (getDatasetId() == null || getDatasetId().equals("")) {
                // Prepare the statement that finds the next ML number.
                String sql = "SELECT MAX(number) FROM (SELECT (SUBSTRING(" +
                        "dataset_id,4))+1 AS number FROM dataset WHERE " +
                        "dataset_id LIKE 'ML.%') as ml_ids";
                PreparedStatement stmt = conn.prepareStatement(sql);
                
                // Execute the query and assign the new id to the data set.
                ResultSet results = stmt.executeQuery();
                if (results.next()) {
                    setDatasetId("ML."+results.getInt(1));
                } else {
                    // The results were empty, so the insert cannot be performed
                    // without a valid id.
                    throw new MasterListException("Unable to calculate a new "+
                            "dataset_id.");
                }
            }
            
            // Prepare the statement and insert the data set into the database.
            String sql = "INSERT INTO dataset(dataset_id,name,url,doc_url," +
                    "date_expected,date_updated,author_pi,preliminary_flag) VALUES(?,?,?,?,?,?,?,?)";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,getDatasetId());
            stmt.setString(2,getName());
            stmt.setString(3,getUrl());
            stmt.setString(4,getDocUrl());
            if (expectedDate == null) {
                stmt.setNull(5,Types.DATE);
            } else {
                stmt.setDate(5,expectedDate);
            }
            if (updatedDate == null) {
                stmt.setNull(6,Types.DATE);
            } else {
                stmt.setDate(6,updatedDate);
            }
            stmt.setString(7,getAuthorPi());
	    stmt.setBoolean(8,isPreliminary());
            stmt.execute();
            
            // Properly close the statement stream.
            stmt.close();            
        } catch (SQLException e) {
            // Convert the SQLException to a MasterListException.
            throw new MasterListException("Unable to insert the dataset: "+
					  getDatasetId()+".",e);
        }

        // Now that the dataset has been inserted, associate its classifications.
        updateClassifications(conn,classifications);    
    }
    
    /**
     * Determine if this data set is still expecting data.
     * @return <code>true</code> if the data set is expecting data, 
     * <code>false</code> otherwise.
     **/
    public boolean isExpected() { 
        return getDateExpected() != null && getDateUpdated() == null;
    }

    /**
     * Determine if this data set is associated with multiple Projects.
     * @return <code>true</code> if this data set is associated with more than
     * one Project, <code>false</code> if it is associated with only one
     * Project.
     * @throws MasterListException when there is a problem determining the
     * number of Projects associated with this data set.
     **/
    public boolean isMultiProject() throws MasterListException {
        return getProjects().size() > 1;
    }

    /**
     * Determine if this data set is a preliminary data set.
     * @return <code>true</code> if this data set is a preliminary data set.
     **/
    public boolean isPreliminary() { return preliminary; }
    
    /**
     * Determine if the data set has been updated.
     * @return <code>true</code> if the data set has been updated,
     * <code>false</code> if it has not.
     **/
    public boolean isUpdated() { 
        return getDateUpdated() != null;
    }
    
    /**
     * Load this DatasetBean with the values from the database for the set
     * dataset id.
     * @param evt The event that triggered the loading of the data set.
     * @throws MasterListException when there is a problem loading the data set
     * from the database.
     **/
    public void load(ActionEvent evt) throws MasterListException {
        // Cancel the load if the dataset id has not been set.
        if (getDatasetId() == null || getDatasetId().equals("")) { return; }
        
        // Load the dataset into a temporary variable.
        try {
            DatasetBean dataset = DatasetBean.loadDataset(datasetId);
            // Copy the values from the temporary variable into this bean.
            setDatasetId(dataset.getDatasetId());
            setOriginalId(dataset.getOriginalId());
            setName(dataset.getName());
            setUrl(dataset.getUrl());
            setDocUrl(dataset.getDocUrl());
            setDateExpected(dataset.getDateExpected());
            setDateUpdated(dataset.getDateUpdated());
            setAuthorPi(dataset.getAuthorPi());
        } catch (MasterListException e) {
            // Clear the data set ids from the bean so they won't be displayed
            // on the page.
            setDatasetId("");
            setOriginalId("");
            // Add the error to the manager so it can be displayed to the user.
            ((MasterListManager)FacesContext.getCurrentInstance().
                    getExternalContext().getSessionMap().get("manager"))
                    .appendError(e);
        }
    }
    
    /**
     * Generate a DatasetBean for the specified id from the values in the
     * database.
     * @param datasetId The identifier of the database to be loaded.
     * @return The DatasetBean representing the specified data set.
     * @throws MasterListException if there is a problem loading the data set.
     **/
    public static DatasetBean loadDataset(String datasetId) throws MasterListException {        
        DatasetBean dataset = null;
        
        // Get a connection to the database from the connection pool.
        Connection conn = DatabaseAccessBean.getConnection();
        try {
            // Prepare the statement and read the data set from the database.
            String sql = "SELECT dataset_id,name,url,doc_url,date_expected,"+
                    "date_updated,author_pi,preliminary_flag FROM dataset WHERE dataset_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,datasetId);
            ResultSet results = stmt.executeQuery();
            
            // Load the dataset into a bean
            if (results.next()) {
                dataset = new DatasetBean();
                dataset.setDatasetId(results.getString(1));
                dataset.setOriginalId(results.getString(1));
                dataset.setName(results.getString(2));
                dataset.setUrl(results.getString(3));
                dataset.setDocUrl(results.getString(4));
                if (results.getDate(5) != null) {
                    dataset.setDateExpected(results.getDate(5).toString());
                }
                if (results.getDate(6) != null) {
                    dataset.setDateUpdated(results.getDate(6).toString());
                }
                dataset.setAuthorPi(results.getString(7));
		dataset.setPreliminary(results.getBoolean(8));
            }
            
            // Close down the open streams.
            results.close();
            stmt.close();
        } catch (SQLException e) {
            // Convert an SQLException to a MasterListException
            throw new MasterListException("Unable to load the dataset: "+
                    datasetId+".",e);
        }
        
        // Check to see if the dataset was loaded
        if (dataset == null) {
            throw new MasterListException("Cannot load dataset: "+datasetId+"."+
                    "  The dataset could not be found in the database.");
        } else {
            return dataset;
        }
    }

    /**
     * Set the author/PI of the data set.
     * @param authorPi The author/PI of the data set.
     **/
    public void setAuthorPi(String authorPi) {
        String oldPi = this.authorPi;
        this.authorPi = authorPi;
        firePropertyChange("author_pi",oldPi,this.authorPi);
    }
            
    /**
     * Set the list of classifications that are to be associated with this data set.
     * @param classifications The list of classifications to associate with the data set.
     **/
    public void setClassifications(List<ClassificationBean> classifications) {
        this.classifications = classifications;
    }
    
    /**
     * Set the unique identifier of the data set.
     * @param datasetId The data set's unique identifier.
     **/
    public void setDatasetId(String datasetId) {
        String oldId = this.datasetId;
        this.datasetId = datasetId;
        firePropertyChange("dataset_id",oldId,this.datasetId);
    }
    
    /**
     * Set the date the data set is expected to have its data available.
     * @param dateExpected The date the data set's data is expected.
     **/
    public void setDateExpected(String dateExpected) {
        String oldDate = this.dateExpected;
        this.dateExpected = dateExpected;
        firePropertyChange("expected_date",oldDate,this.dateExpected);
    }
        
    /**
     * Set the date that marks the data set as updated.
     * @param dateUpdated The date the data set was updated.
     **/
    public void setDateUpdated(String dateUpdated) {
        String oldFlag = this.dateUpdated;
        this.dateUpdated = dateUpdated;
        firePropertyChange("update_flag",oldFlag,this.dateUpdated);
    }
    
    /**
     * Set the url to the default document URL for the data set if the data set
     * id is known and is not a special Master List id.
     * @param evt The event that triggered the function call.
     **/
    public void setDefaultDocUrl(ActionEvent evt) {
        // Remove all previous errors from the manager so they will not be
        // displayed again.
        ((MasterListManager)FacesContext.getCurrentInstance().
             getExternalContext().getSessionMap().get("manager")).clearErrors();
        
        // Only set the document URL if the data set id is valid.
        if (getDatasetId() == null || getDatasetId().equals("") || getDatasetId().startsWith("ML.")) {
            return;
        }

        // Get the URL from the resources.properties file.
        String doc = ResourceBundle.getBundle("resources").getString("defaultDatasetDocUrl");
        if (!doc.endsWith("/")) { doc += "/"; }
        doc += getDatasetId() + "/";
        setDocUrl(doc);
    }
    
    /**
     * Set the URL of the data set to the default Data Archive System URL.
     * @param evt The event that triggered the action call.
     **/
    public void setDefaultUrl(ActionEvent evt) {
        // Remove all previous errors from the manager so they will not be
        // displayed again.
        ((MasterListManager)FacesContext.getCurrentInstance().getExternalContext().getSessionMap().get("manager")).clearErrors();

        // Only set the URL if the data set id is value.
        if (getDatasetId() == null || getDatasetId().equals("") || getDatasetId().startsWith("ML.")) {
            return;
        }
        
        // Get the url from resources.properties file
        String url = ResourceBundle.getBundle("resources").getString("defaultDatasetUrl");
        setUrl(url + getDatasetId());
    }
    
    /**
     * Set the URL of the document for the data set.
     * @param docUrl The URL for the data set's document.
     **/
    public void setDocUrl(String docUrl) { 
        String oldUrl = this.docUrl;
        this.docUrl = docUrl;
        firePropertyChange("doc_url",oldUrl,this.docUrl);    
    }

    /**
     * Set the name of the data set.
     * @param name The new name of the data set.
     **/
    public void setName(String name) {
        String oldName = this.name;
        this.name = name;
        firePropertyChange("name",oldName,this.name);    
    }
    
    /**
     * Set the original dataset identifier for the data set.
     * @param originalId The original data set identifier.
     **/
    public void setOriginalId(String originalId) {
        String oldId = this.originalId;
        this.originalId = originalId;
        firePropertyChange("original_id",oldId,this.originalId);
    }

    /**
     * Set the flag that marks the dataset as preliminary.
     * @param prelim <code>true</code> if this dataset is a preliminary dataset,
     * <code>false</code> if it is not.
     **/
    public void setPreliminary(boolean prelim) { preliminary = prelim; }
    
    /**
     * Set the date the dataset was updated List to today.
     * @param evt The event that triggered the data assignation.
     */
    public void setUpdatedToToday(ActionEvent evt) {
        ((MasterListManager) FacesContext.getCurrentInstance().
                getExternalContext().getSessionMap().get("manager")).
                clearErrors();
        setDateUpdated(dateFormat.format(new java.util.Date()));
    }
    
    /**
     * Set the URL that accesses the data set.
     * @param url The data set's access URL.
     **/
    public void setUrl(String url) {
        String oldUrl = this.url;
        this.url = url;
        firePropertyChange("url",oldUrl,this.url);
    }

    /**
     * Update the data set in the database and associate/disassociated any 
     * classifications.
     * @param conn The connection to use to execute the SQL statements.
     * @throws MasterListException if there is a problem updating the data set.
     **/
    public void update(Connection conn) throws MasterListException {
        if (getName() == null || getName().equals("")) {
            throw new MasterListException("Data set name cannot be empty.");
        }
        java.sql.Date updatedDate = convertDate(getDateUpdated());
        java.sql.Date expectedDate = convertDate(getDateExpected());
        
        try {
            // If the datasetId has not been set to an empty value, figure out what the 
	    // next Master List id available is.
            if (getDatasetId() == null || getDatasetId().trim().equalsIgnoreCase("")) {
                // Prepare the statement that finds the next ML number.
                String sql = "SELECT MAX(number) FROM (SELECT (SUBSTRING(" +
                        "dataset_id,4))+1 AS number FROM dataset WHERE " +
                        "dataset_id LIKE 'ML.%') as ml_ids";
                PreparedStatement stmt = conn.prepareStatement(sql);
                
                // Execute the query and assign the new id to the data set.
                ResultSet results = stmt.executeQuery();
                if (results.next()) {
                    setDatasetId("ML."+results.getInt(1));
                } else {
                    // The results were empty, so the insert cannot be performed without a valid id.
                    throw new MasterListException("Unable to calculate a new dataset_id.");
                }
            }
            
            // Prepare the statement and read it from the database.
            String sql = "UPDATE dataset SET dataset_id=?,name=?,url=?," +
                    "doc_url=?,date_expected=?,date_updated=?,author_pi=?," +
                    "preliminary_flag=? WHERE dataset_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,getDatasetId());
            stmt.setString(2,getName());
            stmt.setString(3,getUrl());
            stmt.setString(4,getDocUrl());
            if (expectedDate == null) {
                stmt.setNull(5,Types.DATE);
            } else {
                stmt.setDate(5,expectedDate);
            }
            if (updatedDate == null) { stmt.setNull(6,Types.DATE); }
            else { stmt.setDate(6,updatedDate); }
            stmt.setString(7,getAuthorPi());
	    stmt.setBoolean(8,isPreliminary());
            stmt.setString(9,getOriginalId());	    
            stmt.execute();
            
            // Propertly close down the statement stream.
            stmt.close();
        } catch (SQLException e) {
            // Convert an SQLException into a MasterListException
            throw new MasterListException("Unable to update the dataset: "+
                    getOriginalId()+".",e);
        }
        
        // Associate/Disassociate the classifications with the data set.
        updateClassifications(conn,classifications);
    }
    
    /**
     * Associate the classifications in the list with the data set and remove any
     * associations for classifications not in the list.
     * @param conn The connection to use to execute the statements.
     * @param classifications The list of classifications to associate with the data set.
     * @throws MasterListException when there is a problem associating a
     * classification with the data set.
     **/
    public void updateClassifications(Connection conn, List<ClassificationBean> classifications) throws MasterListException {

	if (classifications == null || classifications.isEmpty()) {
            throw new MasterListException("Data set must have at least one " +
                    "classification in the included list.");
        }
        
        try {
            // Remove all associations for classification associated with the data set
            String sql = "DELETE FROM dataset_classification WHERE dataset_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,getDatasetId());
            stmt.execute();
            stmt.close();
            
            // Associate the classification in the list to the data set.
            sql = "INSERT INTO dataset_classification(dataset_id,class_id) " +
                    "VALUES(?,?)";
            stmt = conn.prepareStatement(sql);
            for (Iterator<ClassificationBean> itr = classifications.iterator(); itr.hasNext(); ) {
                stmt.setString(1,getDatasetId());
                stmt.setInt(2,itr.next().getClassificationId());
                stmt.execute();
            }
            stmt.close();
            
        } catch (SQLException e) {
            // Convert the SQLException to a MasterListException.
            throw new MasterListException("Unable to update the classification list "+
                    "for "+getDatasetId(),e);
        }            
    }

    /**
     * Get the list of phases associated with this dataset.
     * @return An empty list because a phase is project specific.
     * @throws MasterListException should never be thrown.
     **/
    public List<PhaseBean> getPhases() throws MasterListException { 
        return new ArrayList<PhaseBean>();
    }
    
    /**
     * Set the list of phases to be associated with this dataset.
     * @param phases The list of phases to be associated with this dataset.
     **/
    public void setPhases(List<PhaseBean> phases) {}
}
