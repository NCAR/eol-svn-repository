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
 * <p>The ProjectCategoryTreeRootBean is a specialized ProjectBean that defines
 * the root of an expandable tree of CategoryBeans for a JSF menu.  It generates
 * CategoryProjectBeans as children that only have subcategories as children.
 * </p>
 *
 * @author Joel Clawson
 */
public class ProjectCategoryTreeRootBean extends ProjectBean {
    
    /**
     * Create a new instance of a ProjectCategoryTreeRootBean.
     * @param state The container of the expansion state of the node and its
     * children.
     **/
    public ProjectCategoryTreeRootBean(TreeState state) {
        super(state);
        setExpanded(true);
    }
    
    /**
     * Create a new instance of a ProjectCategoryTreeRootBean.
     * @param state The container of the expansion state of the node and its
     * children.
     * @param project The project to use as the root of the tree.
     **/
    public ProjectCategoryTreeRootBean(TreeState state, ProjectBean project) {
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
    }
    
    /**
     * Get the list of top level categories that are associated with the
     * project.
     * @return The list of categories without any parent categories that are
     * associated with this project.
     * @throws TreeException when there is a problem generating the list of 
     * children for the project.
     **/
    public List<TreeNode> getChildren() throws TreeException {
        List<TreeNode> children = new ArrayList<TreeNode>();

        try {
            // Get a connection to the database from the connection pool.
            Connection conn =
                    GeneralDatabaseAccessBean.getInstance().getConnection();
            
            // Create the statement to get the categories and execute it.
            PreparedStatement stmt;
            if (getProjectId() == null || getProjectId().equals("")) {
                String sql = "SELECT category_id,name,parent_id,0 FROM "+
                        "category WHERE parent_id IS NULL ORDER BY name";
                stmt = conn.prepareStatement(sql);
            } else {
                String sql = "SELECT category.category_id,name,parent_id,hide_flag FROM "
                        +"category NATURAL JOIN project_category WHERE "+
                        "parent_id IS NULL AND project_id=? ORDER BY name";
                stmt = conn.prepareStatement(sql);
                stmt.setString(1,getProjectId());
            }
            ResultSet results = stmt.executeQuery();
            
            // Create a new CategoryBean for each result and add it to the list.
            while (results.next()) {
                CategoryProjectBean category = 
                        new CategoryProjectBean(treeState,this);
                category.setCategoryId(results.getInt(1));
                category.setName(results.getString(2));
                category.setParentCategoryId(results.getInt(3));
                category.setHidden(results.getBoolean(4));
                category.setParent(this);
                children.add(category);
            }
            
            results.close();
            stmt.close();
            conn.close();
        } catch (MasterListException e) {
            // Convert a MasterListException to a TreeException.
            throw new TreeException(e.getMessage());
        } catch (SQLException e) {
            // Convert a SQLException to a TreeException
            throw new TreeException(e.getMessage());
        }
        
        return children;
    }
}
