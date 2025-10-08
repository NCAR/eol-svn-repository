package dmg.ml.bean;

import dmg.jsf.TreeException;
import dmg.jsf.model.TreeNode;
import dmg.ml.DatabaseAccessBean;
import dmg.ml.MasterListException;
import dmg.ml.bean.MasterListBean;
import dmg.ml.manager.Selectable;
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
 * <p>The ClassificationBean class is the basic representation of a Classification in the
 * Master List.  It contains:</p>
 * <ul>
 *    <li>Classification Id- Unique identifier for the Classification.</li>
 *    <li>Name- The name of the Classification.</li>
 *    <li>Type Id- Identifier of the type of the Classification.</li>
 * </ul>
 *
 * @author Joel Clawson
 */
public class ClassificationBean extends MasterListBean<ClassificationBean> implements Selectable {

    private Boolean associated;
    private Integer classId,typeId;
    private List<ClassificationBean> parents;
    private String name,typeName;
    
    /**
     * Create a new instance of a ClassificationBean.
     **/
    public ClassificationBean() { this(null); }
    
    /**
     * Create a new instance of a ClassificationBean.
     * @param treeState A container for the expansion state for the TreeNode.
     **/
    public ClassificationBean(TreeState treeState) { super(treeState); }

    /**
     * Compare this ClassificationBean to another ClassificationBean.
     * @param classification The classification to be compared to this classification.
     * @return A negative integer, zero, or postive integer if this classification
     * is less than, equal to, or greater than the specified classification.
     **/
    public int compareTo(ClassificationBean classification) {
	return getName().compareTo(classification.getName());
    }

    /**
     * Delete the classification from the database.
     * @param conn The connection to use to delete the classification.
     * @throws MasterListException when there is a problem deleting the 
     * classification form the database.
     **/
    public void delete(Connection conn) throws MasterListException {
	try {
	    String sql = "DELETE FROM classification WHERE class_id=?";
	    PreparedStatement stmt = conn.prepareStatement(sql);
	    stmt.setInt(1,getClassificationId());
	    stmt.execute();
	} catch (SQLException e) {
	    // Convert the SQLException to a MasterListException
	    throw new MasterListException("Unable to delete classification: "+
					  getName(),e);
	}
    }
    
    /**
     * Determine if a specified item refers to the same classification as this
     * ClassificationBean.
     * @param item The item to be tested against this ClassificationBean.
     * @return <code>true</code> if the item is a ClassificationBean and has the
     * same classification id as this bean, <code>false</code> otherwise.
     **/
    public boolean equals(Object item) {
        try {
            return getClassificationId().equals(((ClassificationBean)item).getClassificationId());
        } catch (ClassCastException e) { return false; }
    }

    /**
     * Get the list of all of the classifications in the database.
     * @return The list of the classifications in the database.
     * @throws MastserListException if there is a problem getting the classifications
     * from the database.
     **/
    public static List<ClassificationBean> getClassifications() throws MasterListException{
        List<ClassificationBean> classes = new ArrayList<ClassificationBean>();
        
        // Get a connection to the database from the connection pool.
        Connection conn = DatabaseAccessBean.getConnection();
        try {
            // Prepare the statement and read it from the database.
            String sql = "SELECT class_id,classification.name,classification.type_id,classification_type.name FROM classification JOIN classification_type ON classification.type_id=classification_type.type_id ORDER BY classification.name";
            PreparedStatement stmt = conn.prepareStatement(sql);
            ResultSet results = stmt.executeQuery();
            
            // Load the classifications into beans and add them to the list;
            while (results.next()) {
                ClassificationBean classification = new ClassificationBean();
                classification.setClassificationId(results.getInt(1));
                classification.setName(results.getString(2));
                classification.setTypeId(results.getInt(3));
                classification.setTypeName(results.getString(4));
                classes.add(classification);
            }
            
            // Close down the open streams.
            results.close();
            stmt.close();
        } catch (SQLException e) {
            throw new MasterListException("Unable to load the classification list.");
        }
                
        return classes;
    }

    /**
     * Get the list of classification associated with the specified dataset.
     * @param dataset The dataset to be get the classification list for.
     * @return The list of classification associated with the dataset.
     * @throws MasterListException when there is a problem loading the classification list.
     **/
    public static List<ClassificationBean> getClassifications(DatasetBean dataset) throws MasterListException {
        List<ClassificationBean> classes = new ArrayList<ClassificationBean>();

        Connection conn = DatabaseAccessBean.getConnection();
        try {
            String sql = "SELECT classification.class_id,classification.name,classification.type_id,classification_type.name FROM classification NATURAL JOIN classification_type JOIN dataset_classification ON classification.class_id=dataset_classification.class_id WHERE dataset_id=? ORDER BY classification_type.name,classification.name";
            PreparedStatement stmt = conn.prepareStatement(sql);
            ResultSet results = stmt.executeQuery();
            while (results.next()) {
                ClassificationBean classification = new ClassificationBean();
                classification.setClassificationId(results.getInt(1));
                classification.setName(results.getString(2));
                classification.setTypeId(results.getInt(3));
                classification.setTypeName(results.getString(4));
                classes.add(classification);
            }
            results.close();
            stmt.close();
        } catch (SQLException e) {
            throw new MasterListException("Unable to load the classification list for the data set: "+dataset.toString(),e);
        }

        return classes;
    }
    
    /**
     * Get the list of children for the Classification.
     * @return An empty list
     * @throws TreeException if there is a problem loading the children from
     * the database.
     **/
    public List<TreeNode> getChildren() throws TreeException {
        return new ArrayList<TreeNode>();
    }

    /**
     * Get the unique identification number for the classification.
     * @return The classification's unique id number.
     **/
    public Integer getClassificationId() { return classId; }

    /**
     * Get the list of all classification types from the database.
     * @return The list of classification types from the database.
     * @throws MasterListException when there is a problem reading the types in from the database.
     **/
    public static List<String> getClassificationTypeList() throws MasterListException {
	List<String> types = new ArrayList<String>();

	Connection conn = DatabaseAccessBean.getConnection();
	try {
	    String sql = "SELECT DISTINCT(classification_type.name) FROM classification JOIN classification_type ON classification.type_id=classification_type.type_id";
	    PreparedStatement stmt = conn.prepareStatement(sql);
	    ResultSet results = stmt.executeQuery();
	    while (results.next()) {
		types.add(results.getString(1));
	    }
	    results.close();
	    stmt.close();
	} catch (SQLException e) {
	    throw new MasterListException("Unable to load the classification types.",e);
	}

	return types;
    }

    /**
     * Get the name of the facet to be displayed when the node is closed/not
     * displaying its children.
     * @return The name of the closed facet for the node.
     **/
    protected String getClosedFacetName() { return "closedClassification"; }

    /**
     * Get a unique identifier for this TreeNode.
     * @return The unique identifier for the node.
     **/
    public Integer getId() { return getClassificationId(); }

    /**
     * Get the name of the facet to be displayed when the node is a leaf node.
     * @return The name of the leaf facet to be displayed.
     **/
    protected String getLeafFacetName() { return "leafClassification"; }
    
    /**
     * Get the name of the Classification.
     * @return The classification's name.
     **/
    public String getName() { return name; }

    /**
     * Get the name of the facet to be displayed when the node is open (when it
     * is displaying its children).
     * @return The name of the open facet for the node.
     **/
    protected String getOpenFacetName() { return "openClassification"; }

    /**
     * Get the list of Classifications that are direct parents of this Classification.
     * @return The list of parent classifications for this classification.
     * @throws MasterListException when there is a problem loading the parents for
     * the classification.
     **/
    public List<ClassificationBean> getParentClassifications() throws MasterListException {
        List<ClassificationBean> parents = new ArrayList<ClassificationBean>();
        
        // Handle the case where we create an empty bean.  It won't have parents,
        // so don't bother checking as it will result in an error.
        if (getClassificationId() == null || getClassificationId() == 0) {
            return parents;
        }

        Connection conn = DatabaseAccessBean.getConnection();

        try {
            String sql = "SELECT classification.class_id,classification.name,classification.type_id,classification_type.name FROM classification JOIN classification_type ON classification.type_id=classification_type.type_id JOIN classification_parent ON classification.class_id=classification_parent.parent_class_id WHERE classification_parent.class_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setInt(1,getClassificationId());
            ResultSet results = stmt.executeQuery();
            while (results.next()) {
                ClassificationBean parent = new ClassificationBean();
                parent.setClassificationId(results.getInt(1));
                parent.setName(results.getString(2));
                parent.setTypeId(results.getInt(3));
                parent.setTypeName(results.getString(4));
                parents.add(parent);
            }
            results.close();
            stmt.close();
        } catch (SQLException e) {
            throw new MasterListException("Unable to load parent classifications for: "+
			getName()+".",e);
        }

        return parents;
    }
        
    /**
     * Get the list of projects associated with this classification.
     * @return The list of projects associated with the classification.
     * @throws MasterListException when there is a problem getting the list of
     * classifications from the database.
     **/
    public List<ProjectBean> getProjects() throws MasterListException {
        return ProjectBean.getProjectList(this);
    }
    
    /**
     * Get the identifier of the type of this Classification.
     * @return The type identifier for the classification.
     **/
    public Integer getTypeId() { return typeId; }
    
    /**
     * Get the identifier of the type of this Classification.
     * @return The type identifier for the classification.
     **/
    public String getTypeIdString() { return getTypeId() == null ? "" : getTypeId().toString(); }

    /**
     * Get the name of the type of the Classification.
     * @return The name of the classification's type.
     **/
    public String getTypeName() { return typeName; }

    /**
     * Determine if the classification has any parent classifications.
     * @return <code>true</code> if the classification has a parent, <code>false</code>
     * if it does not have any parents.
     * @throws MasterListException when there is a problem determining the parents
     * of the classification.
     **/
    public boolean hasParentClassification() throws MasterListException {
        return !getParentClassifications().isEmpty();
    }

    /**
     * Insert the classification into the Master List database.
     * @param conn The connection to use to execute the SQL statements.
     * @throws MasterListException when there is a problem inserting the 
     * classification into the database.
     **/
    public void insert(Connection conn) throws MasterListException {
        // Make sure the values for the classification are valid
        testClassificationValidity();
        
        try {
            // Prepare the statement and insert the classification into the database.
            String sql = "INSERT INTO classification(name,type_id) VALUES(?,?)";
            PreparedStatement stmt = conn.prepareStatement(sql,Statement.RETURN_GENERATED_KEYS);
            stmt.setString(1,getName());
            stmt.setInt(2,getTypeId());
            stmt.execute();
            
            // Determine the generated id and store it in the bean.
            ResultSet keys = stmt.getGeneratedKeys();
            keys.next();
            setClassificationId(keys.getInt(1));
            keys.close();
            
            // Properly close the statement stream.
            stmt.close();            
        } catch (SQLException e) {
            // Convert the SQLException to a MasterListException.
            throw new MasterListException("Unable to insert the classification: "+
                    getName()+".",e);
        }
        
        updateParentClassifications(conn);
    }
    
    /**
     * Determine if this classification is associated with any projects.
     * @return <code>true</code> if the Classification is associated with at least
     * one project, <code>false</code> if the Classification is not associated with
     * any projects.
     * @throws MasterListException if there is a problem checking for the
     * Classification-Project associations.
     **/
    public boolean isAssociated() throws MasterListException {
        return associated == null ? !getProjects().isEmpty() : associated;
    }

    /**
     * Get a Classification from the database.
     * @param classId The Classification to be loaded from the database.
     * @return The Classification loaded from the database.
     * @throws MasterListException if there is a problem reading the data from
     * the database or if the classification does not exist in the database.
     **/
    public static ClassificationBean loadClassification(Integer classId)
	throws MasterListException {
        // Get a connection to the database from the connection pool.
        Connection conn = DatabaseAccessBean.getConnection();

	ClassificationBean classification = loadClassification(classId,conn);
        
	return classification;
    }
    
    /**
     * Get a Classification from the database.
     * @param classId The Classification to be loaded from the database.
     * @param conn The connection to use to load the classification.
     * @return The Classification loaded from the database.
     * @throws MasterListException if there is a problem reading the data from
     * the database or if the classification does not exist in the database.
     **/
    public static ClassificationBean loadClassification(Integer classId, Connection conn)
            throws MasterListException {
        ClassificationBean classification = null;
        
        try {
            // Prepare the statement and read it from the database.
            String sql = "SELECT class_id,classification.name,classification.type_id,classification_type.name FROM classification JOIN classification_type ON classification.type_id=classification_type.type_id WHERE class_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setInt(1,classId);
            ResultSet results = stmt.executeQuery();
            
            // Load the classification into a bean
            if (results.next()) {
                classification = new ClassificationBean();
                classification.setClassificationId(results.getInt(1));
                classification.setName(results.getString(2));
                classification.setTypeId(results.getInt(3));
                classification.setTypeName(results.getString(4));
            }
            
            // Close down the open streams.
            results.close();
            stmt.close();
        } catch (SQLException e) {
            throw new MasterListException("Unable to load the classification: "+
                    classId+".",e);
        }
                
        // Check to see if the classification was loaded
        if (classification == null) {
            throw new MasterListException("Unable to load classification: "+
                    classId+".");
        } else {
            return classification;
        }
    }

    /**
     * Load the classification with the specified name from the database.
     * @param name The name of the classification to be loaded.
     * @return The classification with the specified name.
     * @throws MasterListException when there is a problem loading the classification.
     **/
    public static ClassificationBean loadClassification(String name) throws MasterListException {
	Connection conn = DatabaseAccessBean.getConnection();

	ClassificationBean classification = null;

	try {
	    String sql = "SELECT class_id,classification.name,classification.type_id,classification_type.name FROM classification JOIN classification_type ON classification.type_id=classification_type.type_id WHERE classification.name=?";
	    PreparedStatement stmt = conn.prepareStatement(sql);
	    stmt.setString(1,name);
	    ResultSet results = stmt.executeQuery();
	    if (results.next()) {
		classification = new ClassificationBean();
		classification.setClassificationId(results.getInt(1));
		classification.setName(results.getString(2));
		classification.setTypeId(results.getInt(3));
		classification.setTypeName(results.getString(4));
	    }
	} catch (SQLException e) {
	    throw new MasterListException("Unable to load classification: "+name,e);
	}

	return classification;
    }

    /**
     * Mark this classification as associated to one or more projects.
     * @param associated <code>true</code> if this classification is associated
     * to at least one project, <code>false</code> otherwise.
     **/
    public void setAssociated(Boolean associated) { this.associated = associated; }
    
    /**
     * Set the unique identification number for the Classification.
     * @param classId The unique id for the Classification.
     **/    
    public void setClassificationId(Integer classId) {
        Integer oldId = this.classId;
        this.classId = classId;
        firePropertyChange("class_id",oldId,this.classId);
    }
    
    /**
     * Set the name of the Classification.
     * @param name The classification's name.
     **/
    public void setName(String name) {
        String oldName = this.name;
        this.name = name;
        firePropertyChange("name",oldName,this.name);
    }
    
    /**
     * Set the parents of this classification.
     * @param parents The parents of this classification.
     **/
    public void setParentClassifications(List<ClassificationBean> parents) {
        this.parents = parents;
    }
    
    /**
     * Set the identifier of the type for the Classification.
     * @param typeId The identifier of the type for the Classification.
     **/
    public void setTypeId(Integer typeId) {
        Integer oldId = this.typeId;
        this.typeId = typeId;
        firePropertyChange("type_id",oldId,this.typeId);
    }

    /**
     * Set the identifier of the type for the Classification.
     * @param typeId The identifier of the type for the Classification.
     **/
    public void setTypeIdString(String typeId) {
        setTypeId(Integer.parseInt(typeId));
    }

    /**
     * Set the name of the type for the Classification.
     * @param typeName The name of the type for the Classification.
     **/
    public void setTypeName(String typeName) {
        String oldName = this.typeName;
        this.typeName = typeName;
        firePropertyChange("type_name",oldName,this.typeName);
    }
    
    /**
     * Test the classification values to make sure that they are valid and can be
     * handled correctly in the database.
     * @throws MasterListException if any value in the classification is not valid.
     **/
    private void testClassificationValidity() throws MasterListException {        
        if (getName() == null || getName().equals("")) {
            throw new MasterListException("Name cannot be empty.");
        }
        if (getTypeId() == null || getTypeId() < 0) {
            throw new MasterListException("Type ID cannot be empty.");
        }
    }
    
    /**
     * Get the String representation for the Classification.
     * @return The classification's name.
     **/
    public String toString() { return getName(); }
    
    /**
     * Update the classification in the database.
     * @param conn The connection to use to execute the SQL statements.
     * @throws MasterListException if there is a problem updating the classification.
     **/
    public void update(Connection conn) throws MasterListException {
        testClassificationValidity();
        
        try {
            // Prepare the statement and read it from the database.
            String sql = "UPDATE classification SET name=?,type_id=? WHERE class_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,getName());
            stmt.setInt(2,getTypeId());
            stmt.setInt(3,getClassificationId());
            stmt.execute();
            
            // Propertly close down the statement stream.
            stmt.close();
        } catch (SQLException e) {
            // Convert an SQLException into a MasterListException
            throw new MasterListException("Unable to update the classification: "+
                    getClassificationId()+".",e);
        }
        
        updateParentClassifications(conn);
    }

    /**
     * Update the parents of this classification in the database.
     * @param conn The connection to use to to execute the statements.
     * @throws MasterListException when there is a problem updating the parent relations in the tree.
     **/
    public void updateParentClassifications(Connection conn) throws MasterListException {
        try {
            PreparedStatement stmt = conn.prepareStatement("DELETE FROM classification_parent WHERE class_id=?");
            stmt.setInt(1,getClassificationId());
            stmt.executeUpdate();
            
            if (!parents.isEmpty()) {
                String sql = "INSERT INTO classification_parent(class_id,parent_class_id) VALUES(?,?)";
                stmt = conn.prepareStatement(sql);
                stmt.setInt(1,getClassificationId());
                
                for (ClassificationBean parent: parents) {
                    stmt.setInt(2,parent.getClassificationId());
                    stmt.executeUpdate();
                }
            }
        } catch (SQLException e) {
            throw new MasterListException("There was a problem update the parents for classification: "+getName());
        }

	/***
	 * Add the entire parent tree to the projects that were affected by this change
	 * to this classification.
	 */
	if (!parents.isEmpty()) {
	    List<ProjectBean> projects = ProjectBean.getProjectList(this);
	    for (ProjectBean project: projects) {
		List<ClassificationBean> classes = new ArrayList<ClassificationBean>(ClassificationProjectBean.getClassificationList(project));
		List<ClassificationBean> parentList = parents;
		while (!parentList.isEmpty()) {
		    ClassificationBean parent = parentList.remove(0);
		    classes.add(parent);
		    parentList.addAll(parent.getParentClassifications());
		}

		project.setClassifications(classes);
		project.update(conn);
	    }
	}


    }

    /**
     * Get the list of parents for this classification.
     * @return The list of parents for this classification.
     **/
    public List<Selectable> getParents() throws MasterListException {
        return new ArrayList<Selectable>(getParentClassifications());
    }

    /**
     * Get the value used by a Selector to identify this classification.
     * @return The id to use when selecting this classification in a Selector.
     **/
    public Integer getSelectableId() { return (getType()+getClassificationId()).hashCode(); }

    /**
     * Get the type of the classification.
     * @return The name of the type of the classification.
     **/
    public String getType() { return getTypeName(); }

    /**
     * Determine if this classification has a parent classification.
     * @return <code>true</code> if this classification has at least one parent,
     * <code>false</code> otherwise.
     * @throws MasterListException when there is a problem testing for parents.
     **/
    public boolean hasParent() throws MasterListException {
        return hasParentClassification();
    }

    /**
     * Determine if the specified classification is an ancestor of this classification.
     * This function is recursive.  It calls itself on the parents of this classification
     * until there are no more parents to check or if the specified classification is
     * found to be an ancestor.
     * @param classification The classification for being tested.
     * @return <code>true</code> if the classification is an ancestor in the tree for this
     * classification, <code>false</code> otherwise.
     * @throws MasterListException when there is a problem reading the parent information from
     * the database.
     **/
    public boolean ancestorOf(ClassificationBean classification) throws MasterListException {
	boolean ancestor = false;
	Connection conn = DatabaseAccessBean.getConnection();
	String sql = "SELECT parent_class_id FROM classification_parent WHERE class_id=?";
	try {
	    PreparedStatement stmt = conn.prepareStatement(sql);
	    stmt.setInt(1,classification.getClassificationId());
	    ResultSet results = stmt.executeQuery();
	    List<ClassificationBean> parents = new ArrayList<ClassificationBean>();

	    while (results.next() && !ancestor) {
		ClassificationBean parent = new ClassificationBean();
		parent.setClassificationId(results.getInt(1));
		
		if (parent.getClassificationId() == getClassificationId()) {
		    ancestor = true;
		}
		parents.add(parent);
	    }
	    results.close();

	    if (!ancestor) {
		for (ClassificationBean parent: parents) {
		    ancestor = ancestorOf(parent);
		}
	    }
	} catch (SQLException e) {
	    throw new MasterListException("Unable to determine parentOf",e);
	}
	return ancestor;
    }
}
