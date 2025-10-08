package dmg.jsf.model;

import dmg.jsf.TreeException;
import java.util.List;

/**
 * <p>The TreeNode interface is to be used by any object that is to be a part
 * of a tree structure using a TreeModel or a UITree.  It defines the basic
 * requirements for the nodes in the tree to be able to function correctly.</p>
 * <p>This interface is an adaptation of the TreeNode class described in the 
 * <u>JavaServer Faces</u> (O'Reilly) book by <i>Hans Bergsten</i>.</p>
 *
 * @author Joel Clawson
 * @see TreeModel
 * @see dmg.jsf.component.UITree
 */
public interface TreeNode {
    
    /**
     * Find the TreeNode that is a child of this node that is identified by the
     * specified nodeId.
     * @param nodeId The node to be found as a child of this node.
     * @return The node with the specified node id.
     * @throws TreeException when there is a problem trying to find the child or
     * if the child could not be found.
     **/
    public TreeNode getChild(String nodeId) throws TreeException;
    
    /**
     * Get the list of children for this node.
     * @return The list of children for the node.
     * @throws TreeException when there is a problem generating the list of
     * children.
     **/
    public List<TreeNode> getChildren() throws TreeException;
    
    /**
     * Get the depth of the node in the tree heirarchy starting with
     * <code>0</code> being the root node.
     * @return The number of levels down the node is from the root.
     **/
    public int getDepth();
    
    /**
     * Get the name of the facet in the JSF page tree to use to display this 
     * node.
     * @return The name of the facet to display the node.
     **/
    public String getFacetName();
    
    /**
     * Get a unique identification number for this node.
     * @return This node's unique id number.
     **/
    public Integer getId();
    
    /**
     * Get the identifying name for the node.
     * @return The uniquely identifying name of the node.
     **/
    public String getNodeId();
    
    /**
     * Get the TreeNode that is the direct ancestor of this node.
     * @return This node's parent node or <code>null</code> if this node is the
     * root node.
     **/
    public TreeNode getParent();
    
    /**
     * Determine if this node is expanded to display its children.
     * @return <code>true</code> if the node is open to display it children,
     * <code>false</code> otherwise.
     **/
    public boolean isExpanded();
    
    /**
     * Determine if this node is a leaf node (has no children).
     * @return <code>true</code> if the node does not have any children,
     * <code>false</code> if it does.
     **/
    public boolean isLeaf();
    
    /**
     * Set the flag that marks that this node should display its children.
     * @param expanded <code>true</code> if this node should display its
     * children, <code>false</code> if it should not.
     **/
    public void setExpanded(boolean expanded);
    
    /**
     * Set the parent node for this node.
     * @param parent This node's parent node.
     **/
    public void setParent(TreeNode parent);
}