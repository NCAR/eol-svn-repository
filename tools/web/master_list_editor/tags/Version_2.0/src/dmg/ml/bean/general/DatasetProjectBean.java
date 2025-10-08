package dmg.ml.bean.general;

import dmg.ml.GeneralDatabaseAccessBean;
import dmg.ml.MasterListException;
import dmg.ml.manager.GeneralManager;
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
     * Delete the data set from the database.  This will delete the project
     * association and if there are not any other project associations the
     * entire data set.
     * @throws MasterListException when there is a problem deleting the data
     * set.
     **/
    public void delete() throws MasterListException {
        // Get a connection to the database from the connection pool.
        Connection conn =
                GeneralDatabaseAccessBean.getInstance().getConnection();
            
        // Turn off auto commit on the connection to allow for transactions.
        try {
            conn.setAutoCommit(false);
        } catch (SQLException e) {
            throw new MasterListException("Database is unable to set up " +
                    "transaction processing for deleting dataset "+
                    getDatasetId()+".",e);
        }
        
        // Delete the data set into the database
        try {
            delete(conn);
        } catch (MasterListException e) {
            // Rollback the transaction if any problem occurred during the
            // insert.
            try { conn.rollback(); }
            // Handle problems rolling back the database.
            catch (SQLException ex) {
                throw new MasterListException(ex.getMessage(),e);
            }
            // Rethrow the excpetion that caused the rollback.
            throw e;
        }
        
        // Save the transaction to the database.
        try {
            conn.commit();
        } catch (SQLException e) {
            throw new MasterListException("Unable to commit the delete of " +
                    "dataset: "+getDatasetId()+" and project: "+
                    project.getProjectId()+".",e);
        }
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
     * Get the list of categories that are associated with this data set and
     * project.
     * @return The list of categories associated with the data set and project.
     * @throws MasterListException if there is a problem loading the category
     * list from the database.
     **/
    public List<CategoryBean> getCategories() throws MasterListException {
        List<CategoryBean> categories = new ArrayList<CategoryBean>();
        
        // Get a connection to the database from the connection pool.
        Connection conn =
                GeneralDatabaseAccessBean.getInstance().getConnection();
        try {
            // Define and execute the statement that gets the categories
            String sql = "SELECT category.category_id,name,parent_id FROM " +
                    "category NATURAL JOIN dataset_category NATURAL JOIN " +
                    "project_category WHERE dataset_id=? AND project_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,getDatasetId());
            stmt.setString(2,project.getProjectId());

            // Execute the query, create the categories and add them to the list
            ResultSet results = stmt.executeQuery();
            while (results.next()) {
                CategoryBean category = new CategoryBean();
                category.setCategoryId(results.getInt(1));
                category.setName(results.getString(2));
                category.setParentCategoryId(results.getInt(3));
                categories.add(category);
            }

            // Properly close down the open streams.
            results.close();
            stmt.close();
            conn.close();
        } catch (SQLException e) {
            // Convert the SQLException to a MasterListException.
            throw new MasterListException("Unable to load the categories for " +
                    "dataset: "+getDatasetId()+" and project: "+
                    project.getProjectId()+".",e);
        }
        
        return categories;
    }
    
    /**
     * Get the date the data set was entered into the Master List.
     * @return The entry date for the data set.
     */
    public String getDatePosted() { return datePosted; }
    
    /**
     * Insert the data set into the database.
     * @throws MasterListException if there is a problem inserting the data
     * set into the database.
     **/
    public void insert() throws MasterListException {
        // Get a connection to the database from the connection pool.
        Connection conn = 
                GeneralDatabaseAccessBean.getInstance().getConnection();
        
        // Turn off auto commit on the connection to allow for transactions.
        try {
            conn.setAutoCommit(false);
        } catch (SQLException e) {
            throw new MasterListException("Database is unable to set up " +
                    "transaction processing for inserting dataset "+
                    getDatasetId()+".",e);
        }
        
        // Insert the data set into the database
        try {
            insert(conn);
            updateProject(conn);
        } catch (MasterListException e) {
            // Rollback the transaction if any problem occurred during the
            // insert.
            try { conn.rollback(); }
            // Handle problems rolling back the database.
            catch (SQLException ex) {
                throw new MasterListException(ex.getMessage(),e);
            }
            // Rethrow the excpetion that caused the rollback.
            throw e;
        }
        
        // Save the transaction to the database.
        try {
            conn.commit();
        } catch (SQLException e) {
            throw new MasterListException("Unable to commit the insert of " +
                    "dataset: "+getDatasetId()+" and project: "+
                    project.getProjectId()+".",e);
        }
        
        // Close down the connection cleanly.
        try { conn.close(); }
        catch (SQLException e) {}          
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

        // Determine the oldest date that is still considered new
	Calendar lastNewDate = Calendar.getInstance();
      	lastNewDate.setTime(new java.util.Date());
	lastNewDate.add(Calendar.DAY_OF_YEAR,-1 * project.getNewLength());

        // Convert the entry date into a calendar to be compared
        Calendar entered = Calendar.getInstance();
	entered.setTime(enteredDate);

        // Determine if the data set is considered new.
        return lastNewDate.compareTo(entered) <= 0;
    }

    /**
     * Load a data set from the database for a specific project.
     * @param datasetId The data set to be loaded from the database.
     * @param project The project the data set is being loaded for.
     * @return The data set loaded from the database for the project.
     * @throws MasterListException if there is a problem loading the data set.
     **/
    public static DatasetProjectBean loadDataset(String datasetId, 
            ProjectBean project) throws MasterListException {

        // Get a connection to the database from the connection pool.
        Connection conn = 
                GeneralDatabaseAccessBean.getInstance().getConnection();

	DatasetProjectBean dataset = null;
	try {
	    try { 
		dataset = loadDataset(datasetId,project,conn); 
		conn.close();
		return dataset;
	    }
	    catch (MasterListException e) {
		conn.close();
		throw e;
	    }
	} catch (SQLException e) {}
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
                    "date_expected,date_updated,in_progress_flag,hide_flag " +
                    "FROM dataset NATURAL JOIN dataset_project WHERE " +
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
        ((GeneralManager) FacesContext.getCurrentInstance().
                getExternalContext().getSessionMap().get("generalManager")).
                clearErrors();
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
     * @throws MasterListException when there is a problem updating the data
     * set in the database.
     **/
    public void update() throws MasterListException {
        // Get a connection to the database from the connection pool.
        Connection conn = 
                GeneralDatabaseAccessBean.getInstance().getConnection();
 
        // Turn off auto commit on the connection to allow for transactions.
        try {
            conn.setAutoCommit(false);
        } catch (SQLException e) {
            throw new MasterListException("Database is unable to set up " +
                    "transaction processing for updating dataset "+
                    getDatasetId()+".",e);
        }
        
        // Update the data set into the database
        try {
            update(conn);
            updateProject(conn);
        } catch (MasterListException e) {
            // Rollback the transaction if any problem occurred during the
            // update.
            try { conn.rollback(); }
            // Handle problems rolling back the database.
            catch (SQLException ex) {
                throw new MasterListException(ex.getMessage(),e);
            }
            // Rethrow the excpetion that caused the rollback.
            throw e;
        }
        
        // Save the transaction to the database.
        try {
            conn.commit();
        } catch (SQLException e) {
            throw new MasterListException("Unable to commit the update of " +
                    "dataset: "+getDatasetId()+" and project: "+
                    project.getProjectId()+".",e);
        }
        
        // Close down the connection cleanly.
        try { conn.close(); }
        catch (SQLException e) {}
    }
    
    /**
     * Update the associations between the data set and its categories.  This
     * limits the associations to the categories that are also associated with
     * the project instead of the entire set of categories in the database.
     * @param conn The connection to use to execute the statements.
     * @param categories The list of categories to be associated with the 
     * dataset. 
     * @throws MasterListException if there is a problem associating or 
     * disassociating a category with the data set.
     **/
    protected void updateCategories(Connection conn,
            List<CategoryBean> categories) throws MasterListException {
        
        if (categories.isEmpty()) {
            throw new MasterListException("Data set must have at least one " +
                    "category in the included list.");
        }
        
        try {
            // Remove all associations for categories associated with the 
            // current project from the data set.
            String sql = "DELETE FROM dataset_category WHERE dataset_id=? AND "+
                    "category_id IN (SELECT category_id FROM project_category "+
                    "WHERE project_id=?)";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,getDatasetId());
            stmt.setString(2,project.getProjectId());
            stmt.execute();
            stmt.close();
            
            // Associate the categories in the list to the data set.
            sql = "INSERT INTO dataset_category(dataset_id,category_id) " +
                    "VALUES(?,?)";
            stmt = conn.prepareStatement(sql);
            for (Iterator<CategoryBean> itr = 
                    categories.iterator(); itr.hasNext(); ) {
                stmt.setString(1,getDatasetId());
                stmt.setInt(2,itr.next().getCategoryId());
                stmt.execute();
            }
            stmt.close();
            
        } catch (SQLException e) {
            // Convert the SQLException to a MasterListException.
            throw new MasterListException("Unable to update the category list "+
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
                    GeneralDatabaseAccessBean.getInstance().getConnection();

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
            conn.close();
        } catch (SQLException e) {
            throw new MasterListException("There was a problem changing the" +
                    " hidden status for dataset: "+getDatasetId()+" in project:"
                    +" "+project.getProjectId()+".",e);
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
}
