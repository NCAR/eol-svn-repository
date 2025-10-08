package dmg.ml.bean.general;

import dmg.jsf.TreeException;
import dmg.jsf.model.TreeNode;
import dmg.ml.GeneralDatabaseAccessBean;
import dmg.ml.MasterListException;
import dmg.ml.bean.MasterListBean;
import dmg.ml.manager.state.TreeState;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Types;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>The CategoryBean class is the basic representation of a Category in the
 * Master List.  It contains:</p>
 * <ul>
 *    <li>Category Id- Unique identifier for the Category.</li>
 *    <li>Name- The name of the Category.</li>
 *    <li>Parent Id- Identifier of the Category's direct ancestor used for
 * defining the Category heirarchy.</li>
 * </ul>
 *
 * @author Joel Clawson
 */
public class CategoryBean extends MasterListBean implements Comparable<CategoryBean> {

    private Integer categoryId,parentCategoryId;
    private String name;
    
    /**
     * Create a new instance of a CategoryBean.
     **/
    public CategoryBean() { this(null); }
    
    /**
     * Create a new instance of a CategoryBean.
     * @param treeState A container for the expansion state for the TreeNode.
     **/
    public CategoryBean(TreeState treeState) { super(treeState); }

    /**
     * Compare this CategoryBean to another CategoryBean.
     * @param category The category to be compared to this category.
     * @return A negative integer, zero, or postive integer if this category
     * is less than, equal to, or greater than the specified category.
     **/
    public int compareTo(CategoryBean category) {
	return getName().compareTo(category.getName());
    }

    /**
     * Delete the category from the database.
     * @throws MasterListException when there is a problem deleting the category
     * form the database.
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
                    "transaction processing for deleting category: "+
                    getName()+".",e);
        }
        
        // Delete the category from the database
        try {
            try {
                String sql = "DELETE FROM category WHERE category_id=?";
                PreparedStatement stmt = conn.prepareStatement(sql);
                stmt.setInt(1,getCategoryId());
                stmt.execute();
            } catch (SQLException e) {
                // Convert the SQLException to a MasterListException
                throw new MasterListException("Unable to delete category: "+
                        getName(),e);
            }
        } catch (MasterListException e) {
            // Rollback the transaction if any problem occurred during the
            // delete.
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
                    "category: "+getName()+".",e);
        }
                
        // Properly close down the connection.
        try { conn.close(); }
        catch (SQLException e) {}
    }
    
    /**
     * Determine if a specified item refers to the same category as this
     * CategoryBean.
     * @param item The item to be tested against this CategoryBean.
     * @return <code>true</code> if the item is a CategoryBean and has the
     * same category id as this bean, <code>false</code> otherwise.
     **/
    public boolean equals(Object item) {
        try {
            return getCategoryId().equals(((CategoryBean)item).getCategoryId());
        } catch (ClassCastException e) { return false; }
    }

    /**
     * Get the list of all of the categories in the database.
     * @return The list of the categories in the database.
     * @throws MastserListException if there is a problem getting the categories
     * from the database.
     **/
    public static List<CategoryBean> getCategories() throws MasterListException{
        List<CategoryBean> categories = new ArrayList<CategoryBean>();
        
                // Get a connection to the database from the connection pool.
        Connection conn = 
                GeneralDatabaseAccessBean.getInstance().getConnection();
        try {
            // Prepare the statement and read it from the database.
            String sql = "SELECT category_id,name,parent_id FROM category "+
                "ORDER BY name";
            PreparedStatement stmt = conn.prepareStatement(sql);
            ResultSet results = stmt.executeQuery();
            
            // Load the categories into beans and add them to the list;
            while (results.next()) {
                CategoryBean category = new CategoryBean();
                category.setCategoryId(results.getInt(1));
                category.setName(results.getString(2));
                category.setParentCategoryId(results.getInt(3));
                categories.add(category);
            }
            
            // Close down the open streams.
            results.close();
            stmt.close();
        } catch (SQLException e) {
            throw new MasterListException("Unable to load the category list.");
        }
                
        // Close down the connection cleanly.
        try { conn.close(); }
        catch (SQLException e) {}
        
        return categories;
    }
    
    /**
     * Get the unique identification number for the category.
     * @return The category's unique id number.
     **/
    public Integer getCategoryId() { return categoryId; }
    
    /**
     * Get the list of children for the Category.
     * @return An empty list
     * @throws TreeException if there is a problem loading the children from
     * the database.
     **/
    public List<TreeNode> getChildren() throws TreeException {
        return new ArrayList<TreeNode>();
    }

    /**
     * Get the name of the facet to be displayed when the node is closed/not
     * displaying its children.
     * @return The name of the closed facet for the node.
     **/
    protected String getClosedFacetName() { return "closedCategory"; }

    /**
     * Get a unique identifier for this TreeNode.
     * @return The unique identifier for the node.
     **/
    public Integer getId() { return getCategoryId(); }

    /**
     * Get the name of the facet to be displayed when the node is a leaf node.
     * @return The name of the leaf facet to be displayed.
     **/
    protected String getLeafFacetName() { return "leafCategory"; }
    
    /**
     * Get the name of the Category.
     * @return The category's name.
     **/
    public String getName() { return name; }

    /**
     * Get the name of the facet to be displayed when the node is open (when it
     * is displaying its children).
     * @return The name of the open facet for the node.
     **/
    protected String getOpenFacetName() { return "openCategory"; }
        
    /**
     * Get the identifier of the Category that is the direct ancestor of this
     * Category.
     * @return The category's parent or <code>null</code> if this Category does
     * not have a parent Category.
     **/
    public Integer getParentCategoryId() { return parentCategoryId; }
    
    /**
     * Get the identifier of the parent category as a String.
     * @return The category's parent id as a String.
     **/
    public String getParentCategoryIdString() { 
        return getParentCategoryId() == null ? "0" : 
            getParentCategoryId().toString();
    }
    
    /**
     * Get the list of projects associated with this category.
     * @return The list of projects associated with the category.
     * @throws MasterListException when there is a problem getting the list of
     * categories from the database.
     **/
    public List<ProjectBean> getProjects() throws MasterListException {
        List<ProjectBean> projects = new ArrayList<ProjectBean>();
        
        // Get a connection to the database from the connection pool.
        Connection conn =
                GeneralDatabaseAccessBean.getInstance().getConnection();
        
        try {
            // Prepare the statement and execute it.
            String sql = "SELECT project.project_id,display_name,system_directory,"+
                    "url,home_page_url,logo_url,new_length FROM project NATURAL"
                    +" JOIN project_category WHERE category_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setInt(1,getCategoryId());
            ResultSet results = stmt.executeQuery();
            
            // Create a new project for each row in the result set
            while (results.next()) {
                ProjectBean project = new ProjectBean();
                project.setProjectId(results.getString(1));
                project.setOriginalId(results.getString(1));
                project.setDisplayName(results.getString(2));
                project.setSystemDirectory(results.getString(3));
                project.setUrl(results.getString(4));
                project.setHomePageUrl(results.getString(5));
                project.setLogoUrl(results.getString(6));
                project.setNewLength(results.getInt(7));
                projects.add(project);
            }
            
            // Close all open streams.
            results.close();
            stmt.close();
            conn.close();
        } catch (SQLException e) {
            // Convert a SQLException to a MasterListException
            throw new MasterListException("Unable to get project list for "+
                    "category: "+getCategoryId(),e);
        }
        
        return projects;
    }
    
    /**
     * Insert this category into the database.
     * @throws MasterListException when there is a problem inserting the
     * category into the database.
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
                    "transaction processing for inserting category "+
                    getName()+".",e);
        }
        
        // Insert the data set into the database
        try {
            insert(conn);
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
                    "category: "+getName()+".",e);
        }
        
        // Close down the connection cleanly.
        try { conn.close(); }
        catch (SQLException e) {}
    }
    
    /**
     * Insert the category into the Master List database.
     * @param conn The connection to use to execute the SQL statements.
     * @throws MasterListException when there is a problem inserting the 
     * category into the database.
     **/
    public void insert(Connection conn) throws MasterListException {
        // Make sure the values for the category are valid
        testCategoryValidity();
        
        try {
            // Prepare the statement and insert the category into the database.
            String sql = "INSERT INTO category(name,parent_id) VALUES(?,?)";
            PreparedStatement stmt = conn.prepareStatement(sql,
                    Statement.RETURN_GENERATED_KEYS);
            stmt.setString(1,getName());
            if (getParentCategoryId() == null || getParentCategoryId() == 0) {
                stmt.setNull(2,Types.INTEGER);
            } else {
                stmt.setInt(2,getParentCategoryId());
            }
            stmt.execute();
            
            // Determine the generated id and store it in the bean.
            ResultSet keys = stmt.getGeneratedKeys();
            keys.next();
            setCategoryId(keys.getInt(1));
            keys.close();
            
            // Properly close the statement stream.
            stmt.close();            
        } catch (SQLException e) {
            // Convert the SQLException to a MasterListException.
            throw new MasterListException("Unable to insert the category: "+
                    getName()+".",e);
        }
    }
    
    /**
     * Determine if this category is associated with any projects.
     * @return <code>true</code> if the Category is associated with at least
     * one project, <code>false</code> if the Category is not associated with
     * any projects.
     * @throws MasterListException if there is a problem checking for the
     * Category-Project associations.
     **/
    public boolean isAssociated() throws MasterListException {
        return !getProjects().isEmpty();
    }

    /**
     * Get a Category from the database.
     * @param categoryId The Category to be loaded from the database.
     * @return The Category loaded from the database.
     * @throws MasterListException if there is a problem reading the data from
     * the database or if the category does not exist in the database.
     **/
    public static CategoryBean loadCategory(Integer categoryId)
	throws MasterListException {
        // Get a connection to the database from the connection pool.
        Connection conn = 
                GeneralDatabaseAccessBean.getInstance().getConnection();

	CategoryBean category = loadCategory(categoryId,conn);

        // Close down the connection cleanly.
        try { conn.close(); }
        catch (SQLException e) {}
        
	return category;
    }
    
    /**
     * Get a Category from the database.
     * @param categoryId The Category to be loaded from the database.
     * @param conn The connection to use to load the category.
     * @return The Category loaded from the database.
     * @throws MasterListException if there is a problem reading the data from
     * the database or if the category does not exist in the database.
     **/
    public static CategoryBean loadCategory(Integer categoryId, Connection conn)
            throws MasterListException {
        CategoryBean category = null;
        
        try {
            // Prepare the statement and read it from the database.
            String sql = "SELECT category_id,name,parent_id FROM category "+
                "WHERE category_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setInt(1,categoryId);
            ResultSet results = stmt.executeQuery();
            
            // Load the category into a bean
            if (results.next()) {
                category = new CategoryBean();
                category.setCategoryId(results.getInt(1));
                category.setName(results.getString(2));
                category.setParentCategoryId(results.getInt(3));
            }
            
            // Close down the open streams.
            results.close();
            stmt.close();
        } catch (SQLException e) {
            throw new MasterListException("Unable to load the category: "+
                    categoryId+".",e);
        }
                
        // Check to see if the category was loaded
        if (category == null) {
            throw new MasterListException("Unable to load category: "+
                    categoryId+".");
        } else {
            return category;
        }
    }
    
    /**
     * Set the unique identification number for the Category.
     * @param categoryId The unique id for the Category.
     **/    
    public void setCategoryId(Integer categoryId) {
        Integer oldId = this.categoryId;
        this.categoryId = categoryId;
        firePropertyChange("category_id",oldId,this.categoryId);
    }
    
    /**
     * Set the name of the Category.
     * @param name The category's name.
     **/
    public void setName(String name) {
        String oldName = this.name;
        this.name = name;
        firePropertyChange("name",oldName,this.name);
    }
    
    /**
     * Set the identifier of the parent Category for this Category.
     * @param parentCategoryId The identifier of this Category's parent
     * Category.
     **/
    public void setParentCategoryId(Integer parentCategoryId) {
        if (parentCategoryId == 0) {
            parentCategoryId = null;
        }
        Integer oldId = this.parentCategoryId;
        this.parentCategoryId = parentCategoryId;
        firePropertyChange("parent_id",oldId,this.parentCategoryId);
    }
    
    /**
     * Set the identifier of the parent Category for this Category.
     * @param parentCategoryId The identifier of this Category's parent
     * Category as a String.
     **/
    public void setParentCategoryIdString(String parentCategoryId) {
        setParentCategoryId(new Integer(parentCategoryId));
    }
    
    /**
     * Test the category values to make sure that they are valid and can be
     * handled correctly in the database.
     * @throws MasterListException if any value in the category is not valid.
     **/
    private void testCategoryValidity() throws MasterListException {        
        if (getName() == null || getName().equals("")) {
            throw new MasterListException("Name cannot be empty.");
        }
        // Convert a 0 parent category id to null.
        if (getParentCategoryId() != null && getParentCategoryId() == 0) { 
            setParentCategoryId(null);
        }
    }
    
    /**
     * Get the String representation for the Category.
     * @return The category's name.
     **/
    public String toString() { return getName(); }
    
    /**
     * Update the category in the database.
     * @throws MasterListException when there is a problem updating the category
     * in the database.
     **/
    public void update() throws MasterListException {
        // Get a connection to the database from the connection pool.
        Connection conn = 
                GeneralDatabaseAccessBean.getInstance().getConnection();
        
        // Turn off auto commit to allow for transaction processing.
        try {
            conn.setAutoCommit(false);
        } catch (SQLException e) {
            throw new MasterListException("Unable to set up transaction " +
                    "processing for updating category: "+getCategoryId()+".",e);
        }
        
        // Update the project in the database
        try {
            update(conn);
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
        
        // Save the transaction in the database now that the entire update has
        // succeeded.
        try {
            conn.commit();
        } catch (SQLException e) {
            // Convert the SQLException to a MasterListException.
            throw new MasterListException("Unable to commit category update " +
                    "transaction for: "+getCategoryId()+".",e);
        }
        
        // Close down the connection cleanly.
        try { conn.close(); }
        catch (SQLException e) {}                
    }
    
    /**
     * Update the category in the database.
     * @param conn The connection to use to execute the SQL statements.
     * @throws MasterListException if there is a problem updating the category.
     **/
    protected void update(Connection conn) throws MasterListException {
        testCategoryValidity();
        
        try {
            // Prepare the statement and read it from the database.
            String sql = "UPDATE category SET name=?,parent_id=? WHERE " +
                    "category_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,getName());
            if (getParentCategoryId() == null || getParentCategoryId() == 0) {
                stmt.setNull(2,Types.INTEGER);
            } else {
                stmt.setInt(2,getParentCategoryId());
            }
            stmt.setInt(3,getCategoryId());
            stmt.execute();
            
            // Propertly close down the statement stream.
            stmt.close();
        } catch (SQLException e) {
            // Convert an SQLException into a MasterListException
            throw new MasterListException("Unable to update the category: "+
                    getCategoryId()+".",e);
        }        
    }
}
