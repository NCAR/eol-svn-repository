package dmg.jsf.model;

import java.util.StringTokenizer;
import javax.faces.component.NamingContainer;

/**
 * <p>The TreeModel class is the backing model for a UITree.  It contains the
 * set of TreeNodes for the tree and how they are related to each other.</p>
 * <p>This class is an adaptation of the TreeModel class described in the 
 * <u>JavaServer Faces</u> (O'Reilly) book by <i>Hans Bergsten</i>.
 *
 * @author Joel Clawson
 * @see TreeNode
 * @see dmg.jsf.component.UITree
 */
public class TreeModel {
    
    private static final String SEPARATOR = 
            (new Character(NamingContainer.SEPARATOR_CHAR)).toString();

    private TreeNode current,root;
    
    /**
     * Create a new TreeModel.
     * @param root The root node of the tree described by this model.
     **/
    public TreeModel(TreeNode root) { this.root = root; }
    
    /**
     * Get the currently selected node of the model.
     * @return The current node in the model.
     **/
    public TreeNode getNode() { return current; }
    
    /**
     * Get the root node of the model.
     * @return The root node of the model.
     **/
    public TreeNode getRoot() { return root; }
    
    /**
     * Set the node in the model by its identifier.
     * @param nodeId The id of the node to be set in the model.
     **/
    public void setNodeId(String nodeId) {
        if (nodeId == null) {
            current = null;
        } else {
            TreeNode node = getRoot();
            StringBuffer sb = new StringBuffer();
            StringTokenizer tokenizer = new StringTokenizer(nodeId,SEPARATOR);
            sb.append(tokenizer.nextToken()).append(SEPARATOR);
            
            // Recursively follow the root's children until the node is found.
            while (tokenizer.hasMoreTokens()) {
                sb.append(tokenizer.nextToken());
                node = node.getChild(sb.toString());
                
                // If the child node couldn't be found, an excpetion needs to
                // thrown because it isn't in the tree where it was expected
                if (node == null) {
                    throw new IllegalArgumentException("Cannot find node with "+
                            "id "+sb.toString());
                }
                
                sb.append(SEPARATOR);
            }
            
            // Set the final found node to the current node.
            current = node;
        }
    }
}
