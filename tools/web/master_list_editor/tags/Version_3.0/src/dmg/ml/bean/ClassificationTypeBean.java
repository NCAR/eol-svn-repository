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
 * The ClassificationTypeBean is the representation of a classification_type in the
 * Master List database.  Is used for grouping similar classifications.
 *
 * @author Joel Clawson
 */
public class ClassificationTypeBean extends MasterListBean<ClassificationTypeBean> {
    
    private Integer typeId;
    private ProjectBean project;
    private String name;

    /**
     * Create a new instance of a ClassificationTypeBean.
     **/
    public ClassificationTypeBean() {
        super(null);
    }
    
    /**
     * Create a new instance of a ClassificationTypeBean.
     * @param treeState The state used for holding tree expansion state information.
     * @param project The project that is using this classification type.
     **/
    public ClassificationTypeBean(TreeState treeState, ProjectBean project) {
        super(treeState);
        this.project = project;
    }

    /**
     * Compare this type to the specified type.
     * @return A negative integer, zero, or a positive integer if this type is 
     * less than, equal to, or greater than the specified type.
     **/
    public int compareTo(ClassificationTypeBean type) {
        return getName().compareTo(type.getName());
    }
    
    /**
     * Delete the classification type from the database.
     * @param conn The connection to use to delete the type.
     * @throws MasterListException when there is a problem deleting the type.
     **/
    public void delete(Connection conn) throws MasterListException {}

    /**
     * Get the list of classifications from the database that are of this type and
     * do not have any parent classifications.
     * @return The list of child classification.
     * @throws TreeException when there is a problem loading the children.
     **/
    public List<TreeNode> getChildren() throws TreeException {
        List<TreeNode> children = new ArrayList<TreeNode>();
        
        try {
            // Get a connection to the database from the connection pool.
            Connection conn = DatabaseAccessBean.getConnection();
            
            PreparedStatement stmt = null;
            if (project.getProjectId() == null || project.getProjectId().equals("")) {
                String sql = "SELECT classification.class_id,classification.name,classification.type_id,classification_type.name,0,COUNT(DISTINCT project_ID) > 0 FROM classification JOIN classification_type ON classification.type_id=classification_type.type_id LEFT JOIN classification_parent ON classification.class_id=classification_parent.class_id LEFT JOIN project_classification ON classification.class_id=project_classification.class_id WHERE classification_parent.parent_class_id IS NULL AND classification.type_id=? GROUP BY classification.class_id ORDER BY classification.name";
                stmt = conn.prepareStatement(sql);
                stmt.setInt(1,getTypeId());
            } else {
                String sql = "SELECT classification.class_id,classification.name,classification.type_id,classification_type.name,hide_flag,1 FROM classification JOIN classification_type ON classification.type_id=classification_type.type_id JOIN project_classification ON classification.class_id=project_classification.class_id LEFT JOIN classification_parent ON classification.class_id=classification_parent.class_id WHERE classification_parent.parent_class_id IS NULL AND classification.type_id=? AND project_id=? ORDER BY classification.name";
                stmt = conn.prepareStatement(sql);
                stmt.setInt(1,getTypeId());
                stmt.setString(2,project.getProjectId());
            }
            
            ResultSet results = stmt.executeQuery();
            while (results.next()) {
                ClassificationProjectBean bean = new ClassificationProjectBean(treeState,project);
                bean.setClassificationId(results.getInt(1));
                bean.setName(results.getString(2));
                bean.setTypeId(results.getInt(3));
                bean.setTypeName(results.getString(4));
                bean.setHidden(results.getBoolean(5));
                bean.setAssociated(results.getBoolean(6));
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
    
    /**
     * Get the list of all of the classification types from the database.
     * @return The list of classification types in the database.
     * @throws MasterListException when there is a problem loading the types from the database.
     **/
    public static List<ClassificationTypeBean> getClassificationTypes() throws MasterListException {
        List<ClassificationTypeBean> types = new ArrayList<ClassificationTypeBean>();
        Connection conn = DatabaseAccessBean.getConnection();
        
        try {
            String sql = "SELECT type_id,name FROM classification_type ORDER BY name";
            PreparedStatement stmt = conn.prepareStatement(sql);
            ResultSet results = stmt.executeQuery();
            while (results.next()) {
                ClassificationTypeBean type = new ClassificationTypeBean();
                type.setTypeId(results.getInt(1));
                type.setName(results.getString(2));
                types.add(type);
            }
            results.close();
            stmt.close();
        } catch (SQLException e) {
            throw new MasterListException("Unable to load the classification types.",e);
        }
        
        return types;
    }
    
    /**
     * Get the list of classification types used by the specified project.
     * @param project The project that is to have the type list retrieved.
     * @return The list of types used by the project.
     * @throws MasterListException when there is a problem loading the types from the database.
     **/
    public static List<ClassificationTypeBean> getClassificationTypes(ProjectBean project) throws MasterListException {
        List<ClassificationTypeBean> types = new ArrayList<ClassificationTypeBean>();
        
        Connection conn = DatabaseAccessBean.getConnection();
        try {
            String sql = "SELECT DISTINCT(classification_type.type_id),classification_type.name FROM classification JOIN classification_type ON classification.type_id=classification_type.type_id JOIN project_classification ON classification.class_id=project_classification.class_id WHERE project_id=? ORDER BY classification_type.name";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,project.getProjectId());
            ResultSet results = stmt.executeQuery();
            while (results.next()) {
                ClassificationTypeBean type = new ClassificationTypeBean();
                type.setTypeId(results.getInt(1));
                type.setName(results.getString(2));
                types.add(type);
            }
            results.close();
            stmt.close();
        } catch (SQLException e) {
            throw new MasterListException("Can't load classification types for project: "+project.getProjectId(),e);
        }
        
        return types;
    }
    
    /**
     * Get the name of the facet used by JSF to draw this bean when closed in a tree.
     * @return The type's closed facet name.
     **/
    protected String getClosedFacetName() { return "closedClassType"; }

    /**
     * Get the name of the facet used by JSF to draw this bean when it is a leaf in a tree.
     * @return The type's leaf facet name.
     **/
    protected String getLeafFacetName() { return "leafClassType"; }
    
    /**
     * Get the unique id of this type.
     * @return The type's unique id.
     **/
    public Integer getId() { return getTypeId(); }
    
    /**
     * Get the name of the type.
     * @return The type's name.
     **/
    public String getName() { return name; }

    /**
     * Get the name of the facet used by JSF to draw this bean when opened in a tree.
     * @return The type's open facet name.
     **/
    protected String getOpenFacetName() { return "openClassType"; }
    
    /**
     * Get the project associated to this type.
     * @return The project associated with this type.
     **/
    protected ProjectBean getProject() { return project; }
    
    /**
     * Get the unique id number for the type.
     * @return The type's unique id number.
     **/
    public Integer getTypeId() { return typeId; }

    /**
     * Insert this type into the database.
     * @param conn The connection to use to insert the type.
     * @throws MasterListException when there is a problem inserting the type.
     **/
    public void insert(Connection conn) throws MasterListException {}
    
    /**
     * Load the specified classification type from the database.
     * @param id The id of the type to be loaded.
     * @return The classification type specified by the id.
     * @throws MasterListException when there is a problem loading the type.
     **/
    public static ClassificationTypeBean loadClassificationType(Integer id) throws MasterListException {
        Connection conn = DatabaseAccessBean.getConnection();
        ClassificationTypeBean type = null;
        
        try {
            String sql = "SELECT type_id,name FROM classification_type WHERE type_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setInt(1,id);
            ResultSet results = stmt.executeQuery();
            if (results.next()) {
                type = new ClassificationTypeBean();
                type.setTypeId(results.getInt(1));
                type.setName(results.getString(2));
            }
            results.close();
            stmt.close();
        } catch (SQLException e) {
            throw new MasterListException("Unable to load classification type.",e);
        }
        
        return type;
    }
    
    /**
     * Set the name of the type.
     * @param name The name for the type.
     **/
    public void setName(String name) { this.name = name; }
    
    /**
     * Set the unique id number for the type.
     * @param typeId The id number for the type.
     **/
    public void setTypeId(Integer typeId) { this.typeId = typeId; }

    /**
     * Update this type in the database.
     * @param conn The connection to use to update the type.
     * @throws MasterListException when there is a problem updating the type.
     **/
    public void update(Connection conn) throws MasterListException {}
}
