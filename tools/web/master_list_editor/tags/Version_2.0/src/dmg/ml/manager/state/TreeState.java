package dmg.ml.manager.state;

import dmg.jsf.model.TreeNode;
import java.util.Map;
import java.util.TreeMap;

/**
 * <p>The TreeState class is a container for holding the expansion states of
 * TreeNodes.</p>
 *
 * @author Joel Clawson
 */
public class TreeState {
    
    private Map<String,Map<Integer,Boolean>> expansionMap;
    private Boolean defaultExpansion;
    
    /**
     * Create a new instance of a TreeState.
     * @param defaultExpansion The default expansion state of the nodes.
     **/
    public TreeState(boolean defaultExpansion) {
        expansionMap = new TreeMap<String,Map<Integer,Boolean>>();
        this.defaultExpansion = defaultExpansion;
    }
    
    /**
     * Determine if the specified node should display its children.
     * @param node The node to test if it should display its children.
     * @return The expansion status of the node.
     **/
    public boolean isExpanded(TreeNode node) {
        Map<Integer,Boolean> classMap = 
                expansionMap.get(node.getClass().getName());
        if (classMap == null) { return defaultExpansion; }
        else {
            Boolean expanded = classMap.get(node.getId());
            return expanded == null ? defaultExpansion : expanded.booleanValue();
        }
    }
    
    /**
     * Set the expansion value for the specified node.
     * @param node The node having its expansion value set.
     * @param expanded <code>true</code> if the node is to be expanded,
     * <code>false</code> otherwise.
     **/
    public void setExpanded(TreeNode node, boolean expanded) {
        Map<Integer,Boolean> classMap = 
                expansionMap.get(node.getClass().getName());
        if (classMap == null) {
            classMap = new TreeMap<Integer,Boolean>();
            expansionMap.put(node.getClass().getName(),classMap);
        }
        classMap.put(node.getId(),new Boolean(expanded));
    }
}
