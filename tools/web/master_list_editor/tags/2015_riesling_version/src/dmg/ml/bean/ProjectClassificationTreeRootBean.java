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
import java.sql.Types;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>The ProjectClassificationTreeRootBean is a specialized ProjectBean that defines
 * the root of an expandable tree of ClassificationBeans for a JSF menu.  It generates
 * ClassificationProjectBeans as children that only have subclassifications as children.
 * </p>
 *
 * @author Joel Clawson
 */
public class ProjectClassificationTreeRootBean extends ProjectBean {
    
    /**
     * Create a new instance of a ProjectClassificationTreeRootBean.
     * @param state The container of the expansion state of the node and its
     * children.
     **/
    public ProjectClassificationTreeRootBean(TreeState state) {
        super(state);
        setExpanded(true);
    }
    
    /**
     * Create a new instance of a ProjectClassificationTreeRootBean.
     * @param state The container of the expansion state of the node and its
     * children.
     * @param project The project to use as the root of the tree.
     **/
    public ProjectClassificationTreeRootBean(TreeState state, ProjectBean project) {
        super(state);
        setProjectId(project.getProjectId());
        setOriginalId(project.getOriginalId());
        setUrl(project.getUrl());
        setSystemDirectory(project.getSystemDirectory());
        setHomePageUrl(project.getHomePageUrl());
        setLogoUrl(project.getLogoUrl());
        setCssUrl(project.getCssUrl());
        setExpanded(true);
    }
    
    /**
     * Get the list of top level classifications that are associated with the
     * project.
     * @return The list of classifications without any parent classifications that are
     * associated with this project.
     * @throws TreeException when there is a problem generating the list of 
     * children for the project.
     **/
    public List<TreeNode> getChildren() throws TreeException {
        List<TreeNode> children = new ArrayList<TreeNode>();

        try {
            // Get a connection to the database from the connection pool.
            Connection conn = DatabaseAccessBean.getConnection();
            
            PreparedStatement stmt = null;
            if (getProjectId() == null || getProjectId().equals("")) {
                String sql = "SELECT type_id,name FROM classification_type";
                stmt = conn.prepareStatement(sql);
            } else {
                String sql = "SELECT DISTINCT(classification_type.type_id),classification_type.name FROM classification JOIN classification_type ON classification.type_id=classification_type.type_id JOIN project_classification ON classification.class_id=project_classification.class_id WHERE project_id=?";
                stmt = conn.prepareStatement(sql);
                stmt.setString(1,getProjectId());
            }
            
            ResultSet results = stmt.executeQuery();
            while (results.next()) {
                ClassificationTypeBean type = new ClassificationTypeBean(treeState,this);
                type.setTypeId(results.getInt(1));
                type.setName(results.getString(2));
                type.setParent(this);
                if (treeState != null && !treeState.isExpansionSet(type)) {
                    type.setExpanded(true);
                }
                children.add(type);
            }
            
            results.close();
            stmt.close();
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
