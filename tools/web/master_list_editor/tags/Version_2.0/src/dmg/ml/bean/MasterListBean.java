package dmg.ml.bean;

import dmg.jsf.TreeException;
import dmg.jsf.model.TreeNode;
import dmg.ml.manager.state.TreeState;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.Iterator;
import java.util.List;

/**
 * <p>The MasterListBean class is a generic bean to be used by data beans for
 * the Master List.  It implements the TreeNode interface and provides generic
 * implementations for functions that are not dependant on the data beans
 * themselves.</p>
 *
 * @author Joel Clawson
 */
public abstract class MasterListBean implements TreeNode {
    
    private PropertyChangeSupport propertySupport;
    private TreeNode parent;
    
    /**
     * The container that holds the expansion state of the bean in the tree.
     **/
    protected TreeState treeState;
    
    /**
     * Create a new instance of a MasterListBean.
     * @param treeState A container for the expansion state for the TreeNode.
     **/
    public MasterListBean(TreeState treeState) {
        this.treeState = treeState;
        propertySupport = new PropertyChangeSupport(this);
    }
    
    /**
     * Add a listener for changes to a property of the bean.
     * @param listener The listener waiting for changes to properties of the 
     * bean.
     **/
    public void addPropertyChangeListener(PropertyChangeListener listener) {
        propertySupport.addPropertyChangeListener(listener);
    }

    /**
     * Find this node's child specified by the child's identifier.
     * @param nodeId The identifier for the child node.
     * @return The child TreeNode.
     **/
    public TreeNode getChild(String nodeId) throws TreeException {
        for (Iterator<TreeNode> kids = 
                getChildren().iterator(); kids.hasNext(); ) {
            TreeNode child = kids.next();
            if (child.getNodeId().equals(nodeId)) { return child; }
        }
        
        // Couldn't find the child so throw an exception.
        throw new TreeException("Unable to find child node: "+nodeId+" in "+
                "parent node: "+getNodeId());
    }

    /**
     * Get the name of the facet to be displayed when the node is closed/not
     * displaying its children.
     * @return The name of the closed facet for the node.
     **/
    protected abstract String getClosedFacetName();

    /**
     * Get the depth of the node in the tree heirarchy.
     * @return The number of levels down the node is in the tree.  The root has
     * a depth of zero.
     **/
    public int getDepth() {
	int depth = 0;
	if (getParent() != null) { depth = getParent().getDepth() + 1; }
        return depth;
    }
    
    /**
     * Get the name of the facet in the JSF tree to use to display this node.
     * @return The name of the facet to be used to display the node.
     **/
    public String getFacetName() {
	if (isLeaf()) { return getLeafFacetName(); }
	else if (isExpanded()) { return getOpenFacetName(); }
	else { return getClosedFacetName(); }
    }
    
    /**
     * Get the name of the facet to be displayed when the node is a leaf node.
     * @return The name of the leaf facet to be displayed.
     **/
    protected abstract String getLeafFacetName();
    
    /**
     * Get the identifier name for this TreeNode.
     * @return The identifying name for the node.
     **/
    public String getNodeId() {
	String id = getClass().getName()+"-"+getId();
	if (getParent() == null) { return id; }
	else { return getParent().getNodeId()+":"+id; }
    }
    
    /**
     * Get the name of the facet to be displayed when the node is open (when it
     * is displaying its children).
     * @return The name of the open facet for the node.
     **/
    protected abstract String getOpenFacetName();    
    
    /**
     * Get the parent node of this node in the tree.
     * @return The parent node of this node or <code>null</code> if this node
     * is the root node.
     **/
    public TreeNode getParent() { return parent; }
    
    /**
     * Notify all listeners that a property has changed for the bean.
     * @param id The identifier for the property that has changed.
     * @param oldValue The value of the property before the change.
     * @param newValue The value of the property after the change.
     **/
    protected void firePropertyChange(String id, Object oldValue, Object newValue) {
        propertySupport.firePropertyChange(id,oldValue,newValue);
    }

    /**
     * Determine if this node is expanded to display its children.
     * @return <code>true</code> if this node is to display its children, 
     * <code>false</code> otherwise.
     **/
    public boolean isExpanded() { 
        return treeState == null ? true : treeState.isExpanded(this);
    }
    
    /**
     * Determine if this node is a leaf node.  (The node does not have any
     * children.)
     * @return <code>true</code> if this node is a leaf node, <code>false</code>
     * otherwise.
     **/
    public boolean isLeaf() {
        try { return getChildren().size() == 0; }
        catch (TreeException e) { return true; }
    }
    
    /**
     * Remove a listener for changes to bean properties.
     * @param listener The listener to be removed.
     **/
    public void removePropertyChangeListener(PropertyChangeListener listener) {
        propertySupport.removePropertyChangeListener(listener);
    }
    
    /**
     * Set the status of the TreeNode if it should display its children.
     * @param expanded If this TreeNode should be expanded to display its
     * children.
     **/
    public void setExpanded(boolean expanded) {
        if (treeState != null) { treeState.setExpanded(this,expanded); }
    }
    
    /**
     * Set the parent TreeNode of this node.
     * @param parent The parent of this node in the tree.
     **/
    public void setParent(TreeNode parent) { this.parent = parent; }
}
