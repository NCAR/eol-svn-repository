package dmg.jsf.component.util;

import dmg.jsf.component.UITree;
import dmg.jsf.model.TreeNode;
import javax.faces.event.ActionEvent;

/**
 * <p>The TreeNodeToggler class is a service provider for a UITree.  It provides
 * the functionality to expand/contract the currently selected node in the 
 * UITree.</p>
 * <p>This class is an adaptation of the NodeToggler class described in the 
 * <u>JavaServer Faces</u> (O'Reilly) book by <i>Hans Bergsten</i>.
 *
 * @author Joel Clawson
 * @see dmg.jsf.component.UITree
 */
public class TreeNodeToggler {
    
    private UITree tree;
    
    /**
     * Create a new TreeNodeToggler.
     * @param tree The UITree that the toggler will be toggling nodes for.
     **/
    public TreeNodeToggler(UITree tree) { this.tree = tree; }
    
    /**
     * Change the expanded state of the currently selected node in the UITree.
     * @param evt The event that caused the change of expansion state.
     **/
    public void toggle(ActionEvent evt) {
        TreeNode node = tree.getNode();
        node.setExpanded(!node.isExpanded());
    }
}
