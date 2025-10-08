package dmg.ml.bean;

import dmg.jsf.TreeException;
import dmg.jsf.model.TreeNode;
import dmg.ml.DatabaseAccessBean;
import dmg.ml.MasterListException;
import dmg.ml.manager.state.TreeState;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>The ClassificationProjectBean class is an extension of the ClassificationBean to
 * allow extra attributes that are specific to a project.  It adds:</p>
 * <ul>
 *    <li>Hidden Flag- a flag that hides the classification for the project.</li>
 * </ul>
 *
 * @author Joel Clawson
 */
public class ClassificationProjectBean extends ClassificationBean {
    
    private boolean hidden;
    private ProjectBean project;
    
    /**
     * Create a new instance of the ClassificationProjectBean.
     * @param state The container for the expansion state of the bean.
     * @param project The project associated to this classification.
     **/
    public ClassificationProjectBean(TreeState state, ProjectBean project) {
        super(state);
        this.project = project;
    }
    
    /**
     * Get the list of all of the classifications associated with the specified
     * project.
     * @param project The project the list is being retreived for.
     * @return The list of classifications for the specified project.
     * @throws MasterListException if there is a problem loading the classifications
     * from the database.
     **/
    public static List<ClassificationProjectBean> getClassificationList(ProjectBean project)
            throws MasterListException {
        // Get a connection from the connection pool.
        Connection conn = DatabaseAccessBean.getConnection();
        
	List<ClassificationProjectBean> list = getClassificationList(project,conn);
	return list;
    }

    /**
     * Get the list of all of the classifications associated with the specified
     * project.
     * @param project The project the list is being retreived for.
     * @param conn The connection to use to execute the command.
     * @return The list of classifications for the specified project.
     * @throws MasterListException if there is a problem loading the classifications
     * from the database.
     **/
    public static List<ClassificationProjectBean> getClassificationList(ProjectBean project,
							    Connection conn) 
	throws MasterListException {

        List<ClassificationProjectBean> list = new ArrayList<ClassificationProjectBean>();
        
        try {
            // Prepare the statement that gets the classifications from the database.
            String sql = "SELECT classification.class_id,classification.name,classification.type_id,classification_type.name,hide_flag FROM classification JOIN classification_type ON classification.type_id=classification_type.type_id JOIN project_classification ON classification.class_id=project_classification.class_id WHERE project_id=? ORDER BY classification.name";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,project.getProjectId());
            
            // Execute the statement, create new ClassificationBeans, and add them to
            // the list.
            ResultSet results = stmt.executeQuery();            
            while (results.next()) {
                ClassificationProjectBean classification = new ClassificationProjectBean(null,project);
                classification.setClassificationId(results.getInt(1));
                classification.setName(results.getString(2));
                classification.setTypeId(results.getInt(3));
                classification.setTypeName(results.getString(4));
                classification.setHidden(results.getBoolean(5));
                classification.setAssociated(true);
                list.add(classification);
            }
            
            // Properly close all of the open streams.
            results.close();
            stmt.close();
        } catch (SQLException e) {
            // Convert the SQLException to a MasterListException.
            throw new MasterListException("Unable to load the classifications for " +
                    project.getProjectId()+".",e);
        }

	return list;
    }

    /**
     * Get the list of subclassifications for this ClassificationBean.
     * @return The list of subclassifications.
     * @throws TreeException if there is a problem generating the list of 
     * subcclassifications.
     **/
    public List<TreeNode> getChildren() throws TreeException {
        List<TreeNode> children = new ArrayList<TreeNode>();
        
        try {
            // Get a connection to the database from the connection pool.
            Connection conn = DatabaseAccessBean.getConnection();
            
            // Prepare the statement to select the subclassification.
            PreparedStatement stmt = null;
            if (getProject() != null && getProject().getProjectId() != null &&
                    !getProject().getProjectId().equals("")) {
                String sql = "SELECT classification.class_id,classification.name,classification.type_id,classification_type.name,hide_flag,1 FROM classification JOIN classification_type ON classification.type_id=classification_type.type_id JOIN classification_parent ON classification.class_id=classification_parent.class_id JOIN project_classification ON classification.class_id=project_classification.class_id WHERE parent_class_id=? AND project_id=? ORDER BY classification_type.name,classification.name";
                stmt = conn.prepareStatement(sql);
                stmt.setInt(1,getClassificationId());
                stmt.setString(2,getProject().getProjectId());
            } else {
                String sql = "SELECT classification.class_id,classification.name,classification.type_id,classification_type.name,0,COUNT(DISTINCT project_id) > 0 FROM classification JOIN classification_type ON classification.type_id=classification_type.type_id JOIN classification_parent ON classification.class_id=classification_parent.class_id LEFT JOIN project_classification ON classification.class_id=project_classification.class_id WHERE parent_class_id=? GROUP BY classification.class_id ORDER BY classification_type.name,classification.name";
                stmt = conn.prepareStatement(sql);
                stmt.setInt(1,getClassificationId());
            }
            
            // Execute the query, create the ClassificationBeans, and add them to the
            // list of children.
            ResultSet results = stmt.executeQuery();
            while (results.next()) {
                ClassificationProjectBean classification = new ClassificationProjectBean(treeState,project);
                
                classification.setClassificationId(results.getInt(1));
                classification.setName(results.getString(2));
                classification.setTypeId(results.getInt(3));
                classification.setTypeName(results.getString(4));
                classification.setHidden(results.getBoolean(5));
                classification.setAssociated(results.getBoolean(6));
                classification.setParent(this);
                //classification.setExpanded(true);
                children.add(classification);
            }
            
            // Properly close all open streams.
            results.close();
            stmt.close();
        } catch (MasterListException e) {
            // Convert a MasterListException to a TreeException since the 
            // function can only throw a TreeException.
            throw new TreeException(e.getMessage());
        } catch (SQLException e) {
            // Convert a SQLException to a TreeException since the function can
            // only throw a TreeException.
            throw new TreeException(e.getMessage());
        }
        
        return children;
    }

    /**
     * Get the list of classification types used by the specified project.
     * @param project The project to use to access the type list.
     * @return The list of classification types used by the project.
     * @throws MasterListException when there is a problem reading the types from the database.
     **/
    public static List<String> getClassificationTypeList(ProjectBean project) throws MasterListException {
	List<String> types = new ArrayList<String>();
	Connection conn = DatabaseAccessBean.getConnection();
	try {
	    String sql = "SELECT DISTINCT(classification_type.name) FROM classification_type JOIN classification ON classification_type.type_id=classification.type_id JOIN project_classification ON project_classification.class_id=classification.class_id WHERE project_id=?";
	    PreparedStatement stmt = conn.prepareStatement(sql);
	    stmt.setString(1,project.getProjectId());
	    ResultSet results = stmt.executeQuery();
	    while (results.next()) {
		types.add(results.getString(1));
	    }
	    results.close();
	    stmt.close();
	} catch (SQLException e) {
	    throw new MasterListException("Unable to load classification types for project: "+project.getProjectId()+".",e);
	}

	return types;
    }

    /**
     * Get the project associated with this ClassificationBean.
     * @return The project for the classification.
     **/
    public ProjectBean getProject() { return project; }
    
    /**
     * Determine if this classification is hidden for the project.
     * @return <code>true</code> if the classification is hidden for the project,
     * <code>false</code> otherwise.
     **/
    public boolean isHidden() { return hidden; }
    
    /**
     * Get a Classification from the database for a project.
     * @param classId The Classification to be loaded from the database.
     * @param project The project the classification is to be loaded for.
     * @return The Classification loaded from the database.
     * @throws MasterListException if there is a problem reading the data from
     * the database or if the classification does not exist in the database.
     **/
    public static ClassificationProjectBean loadClassification(Integer classId, 
            ProjectBean project) throws MasterListException {
        ClassificationProjectBean classification = null;
        
        // Get a connection to the database from the connection pool.
        Connection conn = DatabaseAccessBean.getConnection();
        try {
            // Prepare the statement and read it from the database.
            String sql = "SELECT classification.class_id,classification.name,classification.type_id,classification_type.name,hide_flag FROM classification JOIN classification_type ON classification.type_id=classification_type.type_id JOIN project_classification ON classification.class_id=project_classification.class_id WHERE classification.class_id=? AND project_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setInt(1,classId);
            stmt.setString(2,project.getProjectId());
            ResultSet results = stmt.executeQuery();
            
            // Load the classification into a bean
            if (results.next()) {
                classification = new ClassificationProjectBean(null,project);
                classification.setClassificationId(results.getInt(1));
                classification.setName(results.getString(2));
                classification.setTypeId(results.getInt(3));
                classification.setTypeName(results.getString(4));
                classification.setHidden(results.getBoolean(5));
                classification.setAssociated(true);
            }
            
            // Close down the open streams.
            results.close();
            stmt.close();
        } catch (SQLException e) {
            throw new MasterListException("Unable to load the classification: "+
                    classId+".",e);
        }
        
        // Check to see if the project was loaded
        if (classification == null) {
            throw new MasterListException("Unable to load classification: "+
                    classId+".");
        } else {
            return classification;
        }
    }

    /**
     * Set the flag that hides the classification for the project.
     * @param hidden <code>true</code> if the classification should be hidden for the
     * project, <code>false</code> otherwise.
     **/
    public void setHidden(boolean hidden) {
        boolean oldFlag = this.hidden;
        this.hidden = hidden;
        firePropertyChange("hide_flag",oldFlag,this.hidden);
    }

    /**
     * Set the project to be associated with this classification.
     * @param project The project associated with the classification
     **/
    public void setProject(ProjectBean project) { this.project = project; }
    
    /**
     * Change the hidden status of this classification in the current project.
     * @param hideFlag The new hidden status for the classification in the project.
     * @throws MasterListException when there is a problem changing the hide
     * status of the classification.
     **/
    public void updateHideStatus(boolean hideFlag) throws MasterListException {
        try {
            // Get a connection to the database from the connection pool
            Connection conn = DatabaseAccessBean.getConnection();

            // Create the statement and execute it to save the hidden status.
            String sql = "UPDATE project_classification SET hide_flag=? WHERE "+
                    "class_id=? AND project_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setBoolean(1,hideFlag);
            stmt.setInt(2,getClassificationId());
            stmt.setString(3,project.getProjectId());
            stmt.execute();

            // Properly close the open streams.
            stmt.close();
        } catch (SQLException e) {
            throw new MasterListException("There was a problem changing the" +
                    " hidden status for classification: "+getClassificationId()+" in " +
                    "project: "+project.getProjectId()+".",e);
        }
    }
}
