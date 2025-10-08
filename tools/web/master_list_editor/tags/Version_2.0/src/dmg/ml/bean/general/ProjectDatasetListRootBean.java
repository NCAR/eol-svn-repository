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
import java.sql.Types;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>The ProjectDatasetListRootBean class is a specialized ProjectBean that
 * defines the root node of the dataset list tree.  It defines its children
 * to be categories that have both data sets and subcategories as children.</p>
 *
 * @author Joel Clawson
 */
public class ProjectDatasetListRootBean extends ProjectBean {

    private CategoryBean category;
    
    /**
     * Create a new instance of a ProjectDatasetListRootBean.
     * @param state The container for the expansion states of the nodes in the
     * tree.
     * @param project The project that is this root node.
     * @param category The category if the tree root is to start with a 
     * specific category.
     **/
    public ProjectDatasetListRootBean(TreeState state, ProjectBean project,
            CategoryBean category) {
        super(state);
        setProjectId(project.getProjectId());
        setOriginalId(project.getOriginalId());
        setDisplayName(project.getDisplayName());
        setUrl(project.getUrl());
        setSystemDirectory(project.getSystemDirectory());
        setNewLength(project.getNewLength());
        setHomePageUrl(project.getHomePageUrl());
        setLogoUrl(project.getLogoUrl());
        setExpanded(true);
        this.category = category;
    }
    
    /**
     * Get the list of categories that have no parent categories and are 
     * associated with this project.
     * @return The list of categories associated with this project.
     * @throws TreeException when there is a problem generating the categories
     * for this project.
     **/
    public List<TreeNode> getChildren() throws TreeException {
        List<TreeNode> children = new ArrayList<TreeNode>();

        try {
            // Get a connection to the database from the connection pool.
            Connection conn =
                    GeneralDatabaseAccessBean.getInstance().getConnection();

            // Define the statement that will get the category list.
            PreparedStatement stmt = null;            
            if (category == null) {
                String sql = "SELECT category.category_id,name,parent_id,hide_flag FROM "
                        +"category NATURAL JOIN project_category WHERE " +
                        "parent_id IS NULL AND project_id=? ORDER BY name";
                stmt = conn.prepareStatement(sql);
                stmt.setString(1,getProjectId());
            } else {
                String sql = "SELECT category.category_id,name,parent_id,hide_flag FROM "
                        +" category NATURAL JOIN project_category WHERE "+
                        "project_id=? AND category.category_id=?";
                stmt = conn.prepareStatement(sql);
                stmt.setString(1,getProjectId());
                stmt.setInt(2,category.getCategoryId());
            }
            
            // Execute the query, create the categories, and add them to the
            // list.
            ResultSet results = stmt.executeQuery();            
            while (results.next()) {
                CategoryProjectBean category = 
                        new CategoryDatasetListBean(treeState,this);
                category.setCategoryId(results.getInt(1));
                category.setName(results.getString(2));
                category.setParentCategoryId(results.getInt(3));
                category.setHidden(results.getBoolean(4));
                category.setParent(this);
                //category.setExpanded(true);
                children.add(category);
            }
            
            // Properly close the open streams.
            results.close();
            stmt.close();
            conn.close();
        } catch (MasterListException e) {
            // Convert a MasterListException to a TreeException.
            throw new TreeException(e.getMessage());
        } catch (SQLException e) {
            // Convert a SQLException to a TreeException.
            throw new TreeException(e.getMessage());
        }
                
        return children;
    }
}
