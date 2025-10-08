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
 * <p>The ClassificationDatasetListBean is a specialized ClassificationProjectBean for
 * allowing datasets to be children of a classification.  This is used for displaying
 * the dataset lists grouped by their classification and by their project.</p>
 * <p>This class does allow the project to be <code>null</code>.  It allows for
 * the ClassificationBean to retrieve all of the datasets associated with the classification
 * along with all of its subclassifications in the database without being limited to
 * a specified project.</p>
 *
 * @author Joel Clawson
 */
public class ClassificationDatasetListBean extends ClassificationProjectBean {

    /**
     * Create a new instance of a ClassificationDatasetListBean.
     * @param state The container for the expansion state of this bean.
     **/
    public ClassificationDatasetListBean(TreeState state) {
        this(state,null);
    }
    
    /**
     * Create a new instance of a ClassificationDatasetListBean.
     * @param state The container for the expansion state of this bean.
     * @param project The project the bean is to use to limit its children.
     **/
    public ClassificationDatasetListBean(TreeState state, ProjectBean project) {
        super(state,project);
    }
    
    /**
     * Get the list of children for this Classification.  It contains an ordered list
     * of datasets (by name) followed by the ordered list of subclassifications (by
     * name) of this ClassificationBean.  The children are limited to the project 
     * defined for the bean unless the project is <code>null</code> which will
     * then load all children in the database.
     * @return The list of datasets and subclassifications for this classification.
     * @throws TreeException when there is a problem loading the children for
     * the classifications.
     **/
    public List<TreeNode> getChildren() throws TreeException {
        List<TreeNode> children = new ArrayList<TreeNode>();
        
        try {
            // Get a connection from the connection pool.
            Connection conn = DatabaseAccessBean.getConnection();
            
            // Prepare the selection of the datasets based on the project.
            PreparedStatement stmt = null;
            if (getProject() != null) {
                String sql = "SELECT dataset.dataset_id,name,url,doc_url,date_expected,"
                        +"date_updated,preliminary_flag,author_pi,date_posted,in_progress_flag,hide_flag "+
                        "FROM dataset NATURAL JOIN dataset_project NATURAL JOIN"
                        +" dataset_classification WHERE class_id=? AND "+
                        "project_id=? ORDER BY name,author_pi";
                stmt = conn.prepareStatement(sql);
                stmt.setInt(1,getClassificationId());
                stmt.setString(2,getProject().getProjectId());
            } else {
                String sql = "SELECT dataset.dataset_id,name,url,doc_url,date_expected,"
                        +"date_updated,preliminary_flag,author_pi FROM dataset NATURAL JOIN "+
                        "dataset_classification WHERE class_id=? ORDER BY name,author_pi";
                stmt = conn.prepareStatement(sql);
                stmt.setInt(1,getClassificationId());
            }
            
            // Execute the query and create new dataset beans and add them
            // to the list of children.
            ResultSet results = stmt.executeQuery();
            while (results.next()) {
                DatasetBean dataset = null;
                if (getProject() != null) {
                    // Attributes unique to the project.
                    dataset = new DatasetProjectBean(treeState,getProject());
                    if (results.getDate(9) != null) {
                        ((DatasetProjectBean)dataset).
                                setDatePosted(results.getDate(9).toString());
                    }
                    ((DatasetProjectBean)dataset).
                            setInProgress(results.getBoolean(10));
                    ((DatasetProjectBean)dataset).
                            setHidden(results.getBoolean(11));
                } else {
                    dataset = new DatasetBean(treeState);
                }
                
                // Attributes common to all types of datasets
                dataset.setDatasetId(results.getString(1));
                dataset.setName(results.getString(2));
                dataset.setUrl(results.getString(3));
                dataset.setDocUrl(results.getString(4));
                if (results.getDate(5) != null) {
                    dataset.setDateExpected(results.getDate(5).toString());
                }
                if (results.getDate(6) != null) {
                    dataset.setDateUpdated(results.getDate(6).toString());
                }
                dataset.setPreliminary(results.getBoolean(7));
                dataset.setAuthorPi(results.getString(8));
                
                dataset.setParent(this);
                children.add(dataset);
            }
            results.close();
            stmt.close();
            
            // Prepare the statement for selecting the subclassifications
            if (getProject() != null) {
                String sql = "SELECT classification.class_id,classification.name,classification.type_id,classification_type.name,hide_flag FROM classification JOIN classification_type ON classification.type_id=classification_type.type_id JOIN project_classification ON classification.class_id=project_classification.class_id JOIN classification_parent ON classification.class_id=classification_parent.class_id WHERE parent_class_id=? AND project_id=? ORDER BY classification_type.name,classification.name";
                stmt = conn.prepareStatement(sql);
                stmt.setInt(1,getClassificationId());
                stmt.setString(2,getProject().getProjectId());
            } else {
                String sql = "SELECT classification.class_id,classification.name,classification.type_id,classification_type.name,0 FROM classification JOIN classification_type ON classification.type_id=classification_type.type_id JOIN classification_parent ON classification.class_id=classification_parent.class_id WHERE parent_class_id=? ORDER BY classification_type.name,classification.name";
                stmt = conn.prepareStatement(sql);
                stmt.setInt(1,getClassificationId());
            }
            
            // Execute the query, create new classification beans and add them to
            // the list of children.
            results = stmt.executeQuery();
            while (results.next()) {
                ClassificationDatasetListBean classification = new ClassificationDatasetListBean(treeState,getProject());
                if (getProject() != null) {
                    classification.setHidden(results.getBoolean(5));
                }
                classification.setClassificationId(results.getInt(1));
                classification.setName(results.getString(2));
                classification.setTypeId(results.getInt(3));
                classification.setTypeName(results.getString(4));
                classification.setParent(this);
                //classification.setExpanded(true);
                children.add(classification);
            }
            results.close();
            stmt.close();
                
        } catch (MasterListException e) {
            // Convert a MasterListException into a TreeException because
            // this function can only throw TreeExceptions.
            throw new TreeException(e.getMessage());
        } catch (SQLException e) {
            // Convert a SQLException into a TreeException because this
            // function can only throw TreeExceptions.
            throw new TreeException(e.getMessage());
        }
        
        return children;
    }
}
