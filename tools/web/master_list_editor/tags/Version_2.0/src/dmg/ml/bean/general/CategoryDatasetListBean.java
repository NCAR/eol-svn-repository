package dmg.ml.bean.general;

import dmg.jsf.TreeException;
import dmg.jsf.model.TreeNode;
import dmg.ml.GeneralDatabaseAccessBean;
import dmg.ml.MasterListException;
import dmg.ml.manager.state.TreeState;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>The CategoryDatasetListBean is a specialized CategoryProjectBean for
 * allowing datasets to be children of a category.  This is used for displaying
 * the dataset lists grouped by their category and by their project.</p>
 * <p>This class does allow the project to be <code>null</code>.  It allows for
 * the CategoryBean to retrieve all of the datasets associated with the category
 * along with all of its subcategories in the database without being limited to
 * a specified project.</p>
 *
 * @author Joel Clawson
 */
public class CategoryDatasetListBean extends CategoryProjectBean {

    /**
     * Create a new instance of a CategoryDatasetListBean.
     * @param state The container for the expansion state of this bean.
     **/
    public CategoryDatasetListBean(TreeState state) {
        this(state,null);
    }
    
    /**
     * Create a new instance of a CategoryDatasetListBean.
     * @param state The container for the expansion state of this bean.
     * @param project The project the bean is to use to limit its children.
     **/
    public CategoryDatasetListBean(TreeState state, ProjectBean project) {
        super(state,project);
    }
    
    /**
     * Get the list of children for this Category.  It contains an ordered list
     * of datasets (by name) followed by the ordered list of subcategories (by
     * name) of this CategoryBean.  The children are limited to the project 
     * defined for the bean unless the project is <code>null</code> which will
     * then load all children in the database.
     * @return The list of datasets and subcategories for this category.
     * @throws TreeException when there is a problem loading the children for
     * the category.
     **/
    public List<TreeNode> getChildren() throws TreeException {
        List<TreeNode> children = new ArrayList<TreeNode>();
        
        try {
            // Get a connection from the connection pool.
            Connection conn =
                    GeneralDatabaseAccessBean.getInstance().getConnection();
            
            // Prepare the selection of the datasets based on the project.
            PreparedStatement stmt = null;
            if (getProject() != null) {
                String sql = "SELECT dataset.dataset_id,name,url,doc_url,date_expected,"
                        +"date_updated,date_posted,in_progress_flag,hide_flag "+
                        "FROM dataset NATURAL JOIN dataset_project NATURAL JOIN"
                        +" dataset_category WHERE category_id=? AND "+
                        "project_id=? ORDER BY name";
                stmt = conn.prepareStatement(sql);
                stmt.setInt(1,getCategoryId());
                stmt.setString(2,getProject().getProjectId());
            } else {
                String sql = "SELECT dataset.dataset_id,name,url,doc_url,date_expected,"
                        +"date_updated FROM dataset NATURAL JOIN "+
                        "dataset_category WHERE category_id=? ORDER BY name";
                stmt = conn.prepareStatement(sql);
                stmt.setInt(1,getCategoryId());
            }
            
            // Execute the query and create new dataset beans and add them
            // to the list of children.
            ResultSet results = stmt.executeQuery();
            while (results.next()) {
                DatasetBean dataset = null;
                if (getProject() != null) {
                    // Attributes unique to the project.
                    dataset = new DatasetProjectBean(treeState,getProject());
                    if (results.getDate(7) != null) {
                        ((DatasetProjectBean)dataset).
                                setDatePosted(results.getDate(7).toString());
                    }
                    ((DatasetProjectBean)dataset).
                            setInProgress(results.getBoolean(8));
                    ((DatasetProjectBean)dataset).
                            setHidden(results.getBoolean(9));
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
                
                dataset.setParent(this);
                children.add(dataset);
            }
            results.close();
            stmt.close();
            
            // Prepare the statement for selecting the subcategories
            if (getProject() != null) {
                String sql = "SELECT category.category_id,name,parent_id,hide_flag FROM"+
                        " category NATURAL JOIN project_category WHERE "+
                        "parent_id=? AND project_id=? ORDER BY name";
                stmt = conn.prepareStatement(sql);
                stmt.setInt(1,getCategoryId());
                stmt.setString(2,getProject().getProjectId());
            } else {
                String sql = "SELECT category.category_id,name,parent_id FROM category "+
                        "WHERE parent_id=? ORDER BY name";
                stmt = conn.prepareStatement(sql);
                stmt.setInt(1,getCategoryId());
            }
            
            // Execute the query, create new category beans and add them to
            // the list of children.
            results = stmt.executeQuery();
            while (results.next()) {
                CategoryDatasetListBean category =
                        new CategoryDatasetListBean(treeState,getProject());
                if (getProject() != null) {
                    category.setHidden(results.getBoolean(4));
                }
                category.setCategoryId(results.getInt(1));
                category.setName(results.getString(2));
                category.setParentCategoryId(results.getInt(3));
                category.setParent(this);
                //category.setExpanded(true);
                children.add(category);
            }
            results.close();
            stmt.close();
                
            conn.close();
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
