package dmg.ml.manager.state;

import dmg.jsf.model.TreeNode;
import dmg.ml.MasterListException;
import dmg.ml.bean.ClassificationTypeBean;
import dmg.ml.bean.PhaseBean;
import dmg.ml.manager.MasterListManager;
import dmg.ml.manager.state.MasterListState;
import dmg.ml.manager.state.TreeState;
import javax.faces.event.ActionEvent;

/**
 * <p>The ProjectState class is an abstract state used by a Master List manager
 * when the primary display is to be for a <code>ProjectBean</code>.
 * <p>It is a <code>ConcreteState</code> class in the <b>State</b> pattern 
 * defined in <u>Design Patterns</u> book by Gamma, et al.</p>
 *
 * @author Joel Clawson
 */
public abstract class ProjectState extends MasterListState {
    
    /**
     * Create a new instance of a ProjectState.
     * @param manager The manager that is controlling the page.
     **/
    public ProjectState(MasterListManager manager) { super(manager); }

    /**
     * Get the TreeNode that is the root of the classification tree displayed in a JSF
     * menu.
     * @param treeState The container for the expansion state of the nodes in
     * the classification tree.
     * @return <code>null</code>
     * @throws MasterListException if there is a problem generating the root
     * node for the classification tree.
     **/
    public TreeNode getClassificationTreeRoot(TreeState treeState) throws MasterListException {
        return null;
    }
    
    /**
     * Get the classification type selected in this state.
     * @return <code>null</code> since a classification type cannot be selected in this state.
     **/
    public ClassificationTypeBean getClassificationType() { return null; }

    /**
     * Get the TreeNode that is the root of the data set list displayed in a JSF
     * page.
     * @param treeState The container for the expansion state of the nodes in 
     * the data set list.
     * @return <code>null</code>
     * @throws MasterListException if there is a problem generating the root
     * node for the data set list.
     **/
    public TreeNode getDatasetListRoot(TreeState treeState) throws MasterListException { 
        return null;
    }

    /**
     * Get the phase selected in this state.
     * @return <code>null</code> since a phase cannot be selected in this state.
     **/
    public PhaseBean getPhase() { return null; }

    /**
     * Get the TreeNode that is the root of the phase list displayed in a JSF
     * page.
     * @param treeState The container for the expansion state of the nodes in 
     * the data set list.
     * @return <code>null</code>
     * @throws MasterListException if there is a problem generating the root
     * node for the phase list.
     **/
    public TreeNode getPhaseTreeRoot(TreeState treeState) throws MasterListException {
        return null;
    }
    
    /**
     * Determine if this state's primary display is for classification information.
     * @return <code>false</code> since this is not a classification state.
     **/
    public boolean isClassificationState() { return false; }

    /**
     * Determine if this state's primary display is for data set information.
     * @return <code>false</code> since this is not a data set state.
     **/
    public boolean isDatasetState() { return false; }
    
    public boolean isPhaseState() { return false; }
    
    /**
     * Determine if this state's primary display is for project information.
     * @return <code>true</code> since this is a project state.
     **/
    public boolean isProjectState() { return true; }
}
