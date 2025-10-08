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
 * <p>The CategoryProjectBean class is an extension of the CategoryBean to
 * allow extra attributes that are specific to a project.  It adds:</p>
 * <ul>
 *    <li>Hidden Flag- a flag that hides the category for the project.</li>
 * </ul>
 *
 * @author Joel Clawson
 */
public class CategoryProjectBean extends CategoryBean {
    
    private boolean hidden;
    private ProjectBean project;
    
    /**
     * Create a new instance of the CategoryProjectBean.
     * @param state The container for the expansion state of the bean.
     * @param project The project associated to this category.
     **/
    public CategoryProjectBean(TreeState state, ProjectBean project) {
        super(state);
        this.project = project;
    }
    
    /**
     * Get the list of all of the categories associated with the specified
     * project.
     * @param project The project the list is being retreived for.
     * @return The list of categories for the specified project.
     * @throws MasterListException if there is a problem loading the categories
     * from the database.
     **/
    public static List<CategoryProjectBean> getCategoryList(ProjectBean project)
            throws MasterListException {
        // Get a connection from the connection pool.
        Connection conn =
                GeneralDatabaseAccessBean.getInstance().getConnection();
        
	List<CategoryProjectBean> list = getCategoryList(project,conn);

	try { conn.close(); }
	catch (SQLException e) {}

	return list;
    }

    /**
     * Get the list of all of the categories associated with the specified
     * project.
     * @param project The project the list is being retreived for.
     * @param conn The connection to use to execute the command.
     * @return The list of categories for the specified project.
     * @throws MasterListException if there is a problem loading the categories
     * from the database.
     **/
    public static List<CategoryProjectBean> getCategoryList(ProjectBean project,
							    Connection conn) 
	throws MasterListException {

        List<CategoryProjectBean> list = new ArrayList<CategoryProjectBean>();
        
        try {
            // Prepare the statement that gets the categories from the database.
            String sql = "SELECT category.category_id,name,parent_id FROM category "+
                    "NATURAL JOIN project_category WHERE project_id=? ORDER BY name";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,project.getProjectId());
            
            // Execute the statement, create new CategoryBeans, and add them to
            // the list.
            ResultSet results = stmt.executeQuery();            
            while (results.next()) {
                CategoryProjectBean category = 
                        new CategoryProjectBean(null,project);
                category.setCategoryId(results.getInt(1));
                category.setName(results.getString(2));
                category.setParentCategoryId(results.getInt(3));
                list.add(category);
            }
            
            // Properly close all of the open streams.
            results.close();
            stmt.close();
        } catch (SQLException e) {
            // Convert the SQLException to a MasterListException.
            throw new MasterListException("Unable to load the categories for " +
                    project.getProjectId()+".",e);
        }

	return list;
    }

    /**
     * Get the list of subcategories for this CategoryBean.
     * @return The list of subcategories.
     * @throws TreeException if there is a problem generating the list of 
     * subcategories.
     **/
    public List<TreeNode> getChildren() throws TreeException {
        List<TreeNode> children = new ArrayList<TreeNode>();
        
        try {
            // Get a connection to the database from the connection pool.
            Connection conn =
                    GeneralDatabaseAccessBean.getInstance().getConnection();
            
            // Prepare the statement to select the subcategories.
            PreparedStatement stmt = null;
            if (getProject() != null && getProject().getProjectId() != null &&
                    !getProject().getProjectId().equals("")) {
                String sql = "SELECT category.category_id,name,parent_id,hide_flag FROM"+
                        " category NATURAL JOIN project_category WHERE "+
                        "parent_id=? AND project_id=? ORDER BY name";
                stmt = conn.prepareStatement(sql);
                stmt.setInt(1,getCategoryId());
                stmt.setString(2,getProject().getProjectId());
            } else {
                String sql = "SELECT category.category_id,name,parent_id,0 FROM category "+
                        "WHERE parent_id=? ORDER BY name";
                stmt = conn.prepareStatement(sql);
                stmt.setInt(1,getCategoryId());
            }
            
            // Execute the query, create the CategoryBeans, and add them to the
            // list of children.
            ResultSet results = stmt.executeQuery();
            while (results.next()) {
                CategoryProjectBean category =
                        new CategoryProjectBean(treeState,project);
                
                category.setCategoryId(results.getInt(1));
                category.setName(results.getString(2));
                category.setParentCategoryId(results.getInt(3));
                category.setHidden(results.getBoolean(4));
                category.setParent(this);
                //category.setExpanded(true);
                children.add(category);
            }
            
            // Properly close all open streams.
            results.close();
            stmt.close();
            conn.close();
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
     * Get the project associated with this CategoryBean.
     * @return The project for the category.
     **/
    public ProjectBean getProject() { return project; }
    
    /**
     * Determine if this category is hidden for the project.
     * @return <code>true</code> if the category is hidden for the project,
     * <code>false</code> otherwise.
     **/
    public boolean isHidden() { return hidden; }
    
    /**
     * Get a Category from the database for a project.
     * @param categoryId The Category to be loaded from the database.
     * @param project The project the category is to be loaded for.
     * @return The Category loaded from the database.
     * @throws MasterListException if there is a problem reading the data from
     * the database or if the category does not exist in the database.
     **/
    public static CategoryProjectBean loadCategory(Integer categoryId, 
            ProjectBean project) throws MasterListException {
        CategoryProjectBean category = null;
        
        // Get a connection to the database from the connection pool.
        Connection conn = 
                GeneralDatabaseAccessBean.getInstance().getConnection();
        try {
            // Prepare the statement and read it from the database.
            String sql = "SELECT category.category_id,name,parent_id,hide_flag FROM " +
                    "category NATURAL JOIN project_category WHERE " +
                    "category.category_id=? AND project_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setInt(1,categoryId);
            stmt.setString(2,project.getProjectId());
            ResultSet results = stmt.executeQuery();
            
            // Load the category into a bean
            if (results.next()) {
                category = new CategoryProjectBean(null,project);
                category.setCategoryId(results.getInt(1));
                category.setName(results.getString(2));
                category.setParentCategoryId(results.getInt(3));
                category.setHidden(results.getBoolean(4));
            }
            
            // Close down the open streams.
            results.close();
            stmt.close();
        } catch (SQLException e) {
            throw new MasterListException("Unable to load the category: "+
                    categoryId+".",e);
        }
                
        // Close down the connection cleanly.
        try { conn.close(); }
        catch (SQLException e) {}
        
        // Check to see if the project was loaded
        if (category == null) {
            throw new MasterListException("Unable to load category: "+
                    categoryId+".");
        } else {
            return category;
        }
    }

    /**
     * Set the flag that hides the category for the project.
     * @param hidden <code>true</code> if the category should be hidden for the
     * project, <code>false</code> otherwise.
     **/
    public void setHidden(boolean hidden) {
        boolean oldFlag = this.hidden;
        this.hidden = hidden;
        firePropertyChange("hide_flag",oldFlag,this.hidden);
    }
    
    /**
     * Change the hidden status of this category in the current project.
     * @param hideFlag The new hidden status for the category in the project.
     * @throws MasterListException when there is a problem changing the hide
     * status of the category.
     **/
    public void updateHideStatus(boolean hideFlag) throws MasterListException {
        try {
            // Get a connection to the database from the connection pool
            Connection conn =
                    GeneralDatabaseAccessBean.getInstance().getConnection();

            // Create the statement and execute it to save the hidden status.
            String sql = "UPDATE project_category SET hide_flag=? WHERE "+
                    "category_id=? AND project_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setBoolean(1,hideFlag);
            stmt.setInt(2,getCategoryId());
            stmt.setString(3,project.getProjectId());
            stmt.execute();

            // Properly close the open streams.
            stmt.close();
            conn.close();
        } catch (SQLException e) {
            throw new MasterListException("There was a problem changing the" +
                    " hidden status for category: "+getCategoryId()+" in " +
                    "project: "+project.getProjectId()+".",e);
        }
    }
}
