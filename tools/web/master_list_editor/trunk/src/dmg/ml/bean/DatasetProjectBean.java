package dmg.ml.bean;

import dmg.ml.DatabaseAccessBean;
import dmg.ml.MasterListException;
import dmg.ml.manager.MasterListManager;
import dmg.ml.manager.state.TreeState;
import java.sql.Connection;
import java.sql.Date;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Iterator;
import java.util.List;
import java.util.ResourceBundle;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;

/**
 * <p>The DatasetProjectBean class is an extension of a DatasetBean that allows
 * for attributes that are project specific.  These include:</p>
 * <ul>
 *    <li>Posted Date- The date the data set was posted to the Master List.</li>
 *    <li>In Progress Flag- a flag that marks the data set as being in progress
 * for the project.</li>
 *    <li>Hidden Flag- a flag that marks the data set as hidden in the project's
 * Master List.</li>
 * </ul>
 * <p>The DatasetProjectBean also overrides many database functions to include
 * the changes necessary to the <code>dataset_project</code> table in the 
 * database.</p>
 *
 * @author Joel Clawson
 */
public class DatasetProjectBean extends DatasetBean {

    private boolean hidden,inProgress;
    private String datePosted;
    private ProjectBean project;
    private List<PhaseBean> phases;
    
    /**
     * Create a new instance of a DatasetProjectBean.
     * @param project The project being associated with a data set.
     **/
    public DatasetProjectBean(ProjectBean project) { this(null,project); }

    /**
     * Create a new instance of a DatasetProjectBean.
     * @param project The project being associated with a data set.
     * @param dataset The dataset being extended to a DatasetProjectBean.
     **/
    public DatasetProjectBean(ProjectBean project, DatasetBean dataset) {
	this(project);
	setDatasetId(dataset.getDatasetId());
	setName(dataset.getName());
	setUrl(dataset.getUrl());
	setDocUrl(dataset.getDocUrl());
	setDateExpected(dataset.getDateExpected());
	setDateUpdated(dataset.getDateUpdated());
        setAuthorPi(dataset.getAuthorPi());
    }
    
    /**
     * Create a new instance of a DatasetProjectBean.
     * @param state The container that holds the expansion state for the node.
     * @param project The project being associated with a data set.
     **/
    public DatasetProjectBean(TreeState state, ProjectBean project) {
        super(state);
        this.project = project;
    }
    
    /**
     * Delete the data set from the database using the specified connection.
     * @param conn The connection to use to send the delete statement.
     * @throws MasterListException if there is a problem deleting the data set.
     **/
    public void delete(Connection conn) throws MasterListException {
        // Determine if the entire data set should be deleted.
        boolean deleteAll = !isMultiProject();

        // Prepare the statement and remove the project association.
        try {
            String sql = "DELETE from dataset_project WHERE dataset_id=? " +
                    "AND project_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,getDatasetId());
            stmt.setString(2,project.getProjectId());
            stmt.execute();
            stmt.close();
        } catch (SQLException ex) {
            throw new MasterListException("Unable to delete the project " +
                    "assocation between dataset: "+getDatasetId()+
                    "and project: "+project.getProjectId()+".",ex);
        }

        // Only delete the entire data set if it was only in the current
        // project.
        if (deleteAll) { super.delete(conn); }        
    }

    /**
     * Get the list of classifications that are associated with this data set and
     * project.
     * @return The list of classifications associated with the data set and project.
     * @throws MasterListException if there is a problem loading the classification
     * list from the database.
     **/
    public List<ClassificationBean> getClassifications() throws MasterListException {
        List<ClassificationBean> classifications = new ArrayList<ClassificationBean>();
        
        // Get a connection to the database from the connection pool.
        Connection conn = DatabaseAccessBean.getConnection();
        try {
            // Define and execute the statement that gets the classifications.
            String sql = "SELECT classification.class_id,classification.name,classification.type_id,classification_type.name FROM classification JOIN classification_type ON classification.type_id=classification_type.type_id JOIN dataset_classification ON classification.class_id=dataset_classification.class_id JOIN project_classification ON classification.class_id=project_classification.class_id WHERE dataset_id=? AND project_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,getDatasetId());
            stmt.setString(2,project.getProjectId());

            // Execute the query, create the classifications and add them to the list
            ResultSet results = stmt.executeQuery();
            while (results.next()) {
                ClassificationBean classification = new ClassificationBean();
                classification.setClassificationId(results.getInt(1));
                classification.setName(results.getString(2));
                classification.setTypeId(results.getInt(3));
                classification.setTypeName(results.getString(4));
                classifications.add(classification);
            }

            // Properly close down the open streams.
            results.close();
            stmt.close();
        } catch (SQLException e) {
            // Convert the SQLException to a MasterListException.
            throw new MasterListException("Unable to load the classifications for " +
                    "dataset: "+getDatasetId()+" and project: "+
                    project.getProjectId()+".",e);
        }
        
        return classifications;
    }
    
    /**
     * Get the date the data set was entered into the Master List.
     * @return The entry date for the data set.
     */
    public String getDatePosted() { return datePosted; }
    
    /**
     * Insert the data set into the database with the associated project information.
     * @param conn The connection to use to insert the data set.
     * @throws MasterListException when there is a problem inserting the
     * data set into the database.
     **/
    public void insert(Connection conn) throws MasterListException {
	super.insert(conn);
	updateProject(conn);
        updatePhases(conn);
    }
   
    /**
     * Determine if the data set is hidden for the project.
     * @return <code>true</code> if the data set is hidden in the project,
     * <code>false</code> otherwise.
     **/
    public boolean isHidden() { return hidden; }
    
    /**
     * Determine if the data set is <i>in progress</i> for the current project.
     * @return <code>true</code> if the data set is in progress for the project,
     * <code>false</code> otherwise.
     **/
    public boolean isInProgress() { return inProgress; }

    /**
     * Determine if the data set is considered <i>new</i> for the current
     * project.  (The entry date is less than the length a data set is 
     * considered new by the project from today.
     * @return <code>true</code> if the data set qualifies as new by the 
     * definition of the project new length, <code>false</code> otherwise.
     **/
    public boolean isNew() {
        Date enteredDate = null; 
        try { enteredDate = convertDate(getDatePosted()); }
        catch (MasterListException e) { return false; }
        
        // Data set can't be new if a higher priority status is set.
	if (getDatePosted() == null || isInProgress() || 
                getDateUpdated() != null) { 
            return false;
        }

        int newLength = Integer.parseInt(ResourceBundle.getBundle("resources").getString("defaultNewLength"));
 
        // Determine the oldest date that is still considered new
	Calendar lastNewDate = Calendar.getInstance();
      	lastNewDate.setTime(new java.util.Date());
	lastNewDate.add(Calendar.DAY_OF_YEAR,-1 * newLength);

        // Convert the entry date into a calendar to be compared
        Calendar entered = Calendar.getInstance();
	entered.setTime(enteredDate);

        // Determine if the data set is considered new.
        return lastNewDate.compareTo(entered) <= 0;
    }

    /**
     * Determine if the data set has been updated.
     * @return <code>true</code> if the data set has been updated after the posted date,
     * <code>false</code> if it has not.
     **/
    public boolean isUpdated() { 
        if (getDateUpdated() == null) { return false; }
        if (getDatePosted() == null) { return true; }
        
        return getDateUpdated().compareTo(getDatePosted()) >= 0;
    }

    
    /**
     * Load a data set from the database for a specific project.
     * @param datasetId The data set to be loaded from the database.
     * @param project The project the data set is being loaded for.
     * @return The data set loaded from the database for the project.
     * @throws MasterListException if there is a problem loading the data set.
     **/
    public static DatasetProjectBean loadDataset(String datasetId, ProjectBean project) throws MasterListException {

        // Get a connection to the database from the connection pool.
        Connection conn = DatabaseAccessBean.getConnection();

	DatasetProjectBean dataset = loadDataset(datasetId,project,conn); 

        return dataset;

    }

    /**
     * Load a data set from the database for a specific project.
     * @param datasetId The data set to be loaded from the database.
     * @param project The project the data set is being loaded for.
     * @param conn The connection to use to load the data set.
     * @return The data set loaded from the database for the project.
     * @throws MasterListException if there is a problem loading the data set.
     **/
    public static DatasetProjectBean loadDataset(String datasetId,
						 ProjectBean project,
						 Connection conn) 
	throws MasterListException {
        DatasetProjectBean dataset = null;
        
        try {
            // Prepare the statement and read it from the database.
            String sql = "SELECT dataset.dataset_id,name,url,doc_url,date_posted,"+
                    "date_expected,date_updated,in_progress_flag,hide_flag,author_pi,preliminary_flag " +
                    "FROM dataset JOIN dataset_project ON dataset.dataset_id=dataset_project.dataset_id WHERE " +
                    "dataset.dataset_id=? AND project_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,datasetId);
            stmt.setString(2,project.getProjectId());
            ResultSet results = stmt.executeQuery();
            
            // Load the dataset into a bean
            if (results.next()) {
                dataset = new DatasetProjectBean(project);
                dataset.setDatasetId(results.getString(1));
                dataset.setOriginalId(results.getString(1));
                dataset.setName(results.getString(2));
                dataset.setUrl(results.getString(3));
                dataset.setDocUrl(results.getString(4));
                if (results.getDate(5) != null) {
                    dataset.setDatePosted(results.getDate(5).toString());
                }
                if (results.getDate(6) != null) {
                    dataset.setDateExpected(results.getDate(6).toString());
                }
                if (results.getDate(7) != null) {
                    dataset.setDateUpdated(results.getDate(7).toString());
                }
                dataset.setInProgress(results.getBoolean(8));
                dataset.setHidden(results.getBoolean(9));
                dataset.setAuthorPi(results.getString(10));
                dataset.setPreliminary(results.getBoolean(11));
            }
            
            // Close down the open streams.
            results.close();
            stmt.close();
        } catch (SQLException e) {
            throw new MasterListException("Unable to load the dataset: "+
                    datasetId+" for project:"+project.getProjectId()+".",e);
        }
                
        // Check to see if the project was loaded
        if (dataset == null) {
            throw new MasterListException("Cannot load dataset: "+datasetId+
                    " for project: "+project.getProjectId()+".  The dataset " +
                    "cannot be found in the database for the project.");
        } else {
            return dataset;
        }
    }
    
    /**
     * Set the date the data set was entered into the Master List.
     * @param datePosted The date the data set was entered into the Master List.
     */
    public void setDatePosted(String datePosted) {
        String oldDate = this.datePosted;
        this.datePosted = datePosted;
        firePropertyChange("date_posted", oldDate, this.datePosted);
    }
    
    /**
     * Set the date the dataset was entered into the Master List to today.
     * @param evt The event that triggered the data assignation.
     */
    public void setEnteredToToday(ActionEvent evt) {
        ((MasterListManager) FacesContext.getCurrentInstance().
                getExternalContext().getSessionMap().get("manager")).clearErrors();
        setDatePosted(dateFormat.format(new java.util.Date()));
    }
    
    /**
     * Set the flag that hides this data set for the project.
     * @param hidden <code>true</code> if the data set is to be hidden in this
     * project, <code>false</code> otherwise.
     **/
    public void setHidden(boolean hidden) {
        boolean oldFlag = this.hidden;
        this.hidden = hidden;
        firePropertyChange("hide_flag",oldFlag,this.hidden);    
    }
    
    /**
     * Set the flag that marks the data set as <i>in progress</i> for the 
     * project.
     * @param inProgress <code>true</code> if the data set is to be in progress
     * for the project, <code>false</code>otherwise.
     **/
    public void setInProgress(boolean inProgress) {
        boolean oldFlag = this.inProgress;
        this.inProgress = inProgress;
        firePropertyChange("in_progress_flag",oldFlag,this.inProgress);
    }
        
    /**
     * Update this data set in the database for the project.
     * @param conn The connection to use to update the data set.
     * @throws MasterListException when there is a problem updating the data
     * set in the database.
     **/
    public void update(Connection conn) throws MasterListException {
	super.update(conn);
	updateProject(conn);
        updatePhases(conn);
    }
    
    /**
     * Update the associations between the data set and its classifications.  This
     * limits the associations to the classifications that are also associated with
     * the project instead of the entire set of classifications in the database.
     * @param conn The connection to use to execute the statements.
     * @param classifications The list of classifications to be associated with the 
     * dataset. 
     * @throws MasterListException if there is a problem associating or 
     * disassociating a classification with the data set.
     **/
    public void updateClassifications(Connection conn,
            List<ClassificationBean> classifications) throws MasterListException {
        
        if (classifications.isEmpty()) {
            throw new MasterListException("Data set must have at least one " +
                    "classification in the included list.");
        }
        
        try {
            // Remove all associations for classifications associated with the 
            // current project from the data set.
            String sql = "DELETE FROM dataset_classification WHERE dataset_id=? AND "+
                    "class_id IN (SELECT class_id FROM project_classification "+
                    "WHERE project_id=?)";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,getDatasetId());
            stmt.setString(2,project.getProjectId());
            stmt.execute();
            stmt.close();
            
            // Associate the classifications in the list to the data set.
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
     * Change the hidden status of this data set in the current project.
     * @param hideFlag The new hidden status for the data set in the project.
     * @throws MasterListException when there is a problem changing the hide
     * status of the data set.
     **/
    public void updateHideStatus(boolean hideFlag) throws MasterListException {
        try {
            // Get a connection to the database from the connection pool
            Connection conn =
                    DatabaseAccessBean.getConnection();

            // Create the statement and execute it to save the hidden status.
            String sql = "UPDATE dataset_project SET hide_flag=? WHERE "+
                    "dataset_id=? AND project_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setBoolean(1,hideFlag);
            stmt.setString(2,getDatasetId());
            stmt.setString(3,project.getProjectId());
            stmt.execute();

            // Properly close the open streams.
            stmt.close();
        } catch (SQLException e) {
            throw new MasterListException("There was a problem changing the" +
                    " hidden status for dataset: "+getDatasetId()+" in project:"
                    +" "+project.getProjectId()+".",e);
        }
    }
    
    /**
     * Update this dataset's phase associations.
     * @param conn The connection to use to update the associations.
     * @throws MasterListException when there is a problem associating the phases with the dataset.
     **/
    public void updatePhases(Connection conn) throws MasterListException {
        try {
            String sql = "DELETE FROM dataset_phase WHERE dataset_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,getDatasetId());
            stmt.execute();
            stmt.close();
            
            if (!phases.isEmpty()) {
                sql = "INSERT INTO dataset_phase(dataset_id,phase_id) VALUES(?,?)";
                stmt = conn.prepareStatement(sql);
                stmt.setString(1,getDatasetId());
                for (PhaseBean phase: phases) {
                    stmt.setInt(2,phase.getPhaseId());
                    stmt.executeUpdate();
                }
                stmt.close();
            }
        } catch (SQLException e) {
            throw new MasterListException("Unable to update phases for dataset: "+getDatasetId(),e);
        }
    }
    
    /**
     * Associate the project and data set in the database.
     * @param conn The connection to use to execute the statement.
     * @throws MasterListException when there is a problem associating the
     * data set and project in the database.
     **/
    public void updateProject(Connection conn) throws MasterListException {
        java.sql.Date datePosted = convertDate(getDatePosted());
        
        try {
            // Remove the previous association from the database
            String sql = "DELETE FROM dataset_project WHERE " +
                    "dataset_id=? AND project_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,getDatasetId());
            stmt.setString(2,project.getProjectId());
            // Execute the delete
            stmt.execute();
        } catch (SQLException e) {
            throw new MasterListException("Unable to remove the association "+
                    "between dataset: "+getDatasetId()+" and project: "+
                    project.getProjectId()+".",e);
        }
        
        try {    
            // Insert the association into the database with the new values.
            String sql = "INSERT INTO dataset_project(dataset_id,project_id," +
                    "in_progress_flag,hide_flag,date_posted) VALUES(?,?,?,?,?)";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,getDatasetId());
            stmt.setString(2,project.getProjectId());
            stmt.setBoolean(3,isInProgress());
            stmt.setBoolean(4,isHidden());
            if (datePosted != null) { stmt.setDate(5,datePosted); }
            else { stmt.setNull(5,Types.DATE); }
            
            // Execute the insert.
            stmt.execute();

            // Close down the open streams.
            stmt.close();
        } catch (SQLException e) {
            // Convert the SQLException to a MasterListException
            throw new MasterListException("Unable to associate project: "+
                    project.getProjectId()+" with dataset: "+
                    getDatasetId()+".",e);
        }
    }

    /**
     * Get the list of phases associated with this dataset.
     * @return The list of associated phases.
     * @throws MasterListException when there is a problem accessing the dataset's phases.
     **/
    public List<PhaseBean> getPhases() throws MasterListException {
        List<PhaseBean> phaseList = new ArrayList<PhaseBean>();
        Connection conn = DatabaseAccessBean.getConnection();
        try {
            String sql = "SELECT phase.phase_id,project_id,name,hide_flag FROM phase JOIN dataset_phase ON phase.phase_id=dataset_phase.phase_id WHERE dataset_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,getDatasetId());
            ResultSet results = stmt.executeQuery();
            while (results.next()) {
                PhaseBean phase = new PhaseBean();
                phase.setPhaseId(results.getInt(1));
                phase.setProjectId(results.getString(2));
                phase.setName(results.getString(3));
                phase.setHidden(results.getBoolean(4));
                phaseList.add(phase);
            }
            results.close();
            stmt.close();
        } catch (SQLException e) {
            throw new MasterListException("Unable to load phases for dataset: "+getDatasetId(),e);
        }
        return phaseList;
    }

    /**
     * Set the list of phases associated to this dataset.
     * @param phases The phases to be associated to this dataset.
     **/
    public void setPhases(List<PhaseBean> phases) { this.phases = phases; }
}
