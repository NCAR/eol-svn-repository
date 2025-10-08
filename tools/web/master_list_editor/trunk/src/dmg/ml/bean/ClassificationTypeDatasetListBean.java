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
 *
 * @author Joel Clawson
 */
public class ClassificationTypeDatasetListBean extends ClassificationTypeBean {

    /**
     * Create a new instance of a ClassificationTypeDatasetListBean.
     * @param state The container for tree expansion states the type belongs in.
     * @param project The project associated with the type.
     **/
    public ClassificationTypeDatasetListBean(TreeState state, ProjectBean project) {
        super(state,project);
    }

    /**
     * Get the list of classifications from the database that are of this type and are associated
     * with this project that do not have any parents.
     * @return The list of child classifications.
     * @throws TreeException when there is a problem loading the children from the database.
     **/
    public List<TreeNode> getChildren() throws TreeException {
        List<TreeNode> children = new ArrayList<TreeNode>();
        
        try {
            // Get a connection to the database from the connection pool.
            Connection conn = DatabaseAccessBean.getConnection();
            
            PreparedStatement stmt = null;
            if (getProject().getProjectId() == null || getProject().getProjectId().equals("")) {
                String sql = "SELECT classification.class_id,classification.name,classification.type_id,classification_type.name,0 FROM classification JOIN classification_type ON classification.type_id=classification_type.type_id LEFT JOIN classification_parent ON classification.class_id=classification_parent.class_id WHERE classification_parent.parent_class_id IS NULL AND classification.type_id=? ORDER BY classification.name";
                stmt = conn.prepareStatement(sql);
                stmt.setInt(1,getTypeId());
            } else {
                String sql = "SELECT classification.class_id,classification.name,classification.type_id,classification_type.name,hide_flag FROM classification JOIN classification_type ON classification.type_id=classification_type.type_id JOIN project_classification ON classification.class_id=project_classification.class_id LEFT JOIN classification_parent ON classification.class_id=classification_parent.class_id WHERE classification_parent.parent_class_id IS NULL AND classification.type_id=? AND project_id=? ORDER BY classification.name";
                stmt = conn.prepareStatement(sql);
                stmt.setInt(1,getTypeId());
                stmt.setString(2,getProject().getProjectId());
            }
            
            ResultSet results = stmt.executeQuery();
            while (results.next()) {
                ClassificationDatasetListBean bean = new ClassificationDatasetListBean(treeState,getProject());
                bean.setClassificationId(results.getInt(1));
                bean.setName(results.getString(2));
                bean.setTypeId(results.getInt(3));
                bean.setTypeName(results.getString(4));
                bean.setHidden(results.getBoolean(5));
                bean.setParent(this);
                children.add(bean);
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
