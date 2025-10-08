package dmg.ml.manager.state;

import dmg.jsf.model.TreeNode;
import dmg.ml.MasterListException;
import dmg.ml.manager.MasterListManager;
import dmg.ml.manager.state.MasterListState;

/**
 * <p>The ClassificationState class is an abstract state used by a Master List manager
 * when the primary display is to be for a <code>ClassificationBean</code>.
 * <p>It is a <code>ConcreteState</code> class in the <b>State</b> pattern 
 * defined in the <u>Design Patterns</u> book by Gamma, et al.</p>
 *
 * @author Joel Clawson
 */
public abstract class ClassificationState extends MasterListState {
    
    /**
     * Create a new instance of a ClassificationState.
     * @param manager The manager that generated this state.
     **/
    public ClassificationState(MasterListManager manager) { super(manager); }

    /**
     * Get the root to display a phase tree.
     * @return <code>null</code> since a ClassificationState cannot display a
     * phase tree.
     * @throws MasterListException should never be thrown.
     **/
    public TreeNode getPhaseTreeRoot(TreeState treeState) throws MasterListException {
        return null;
    }    
    
    /**
     * Determine if this state's primary display is for classification information.
     * @return <code>true</code> since this is a classification state.
     **/
    public boolean isClassificationState() { return true; }

    /**
     * Determine if this state's primary display is for data set information.
     * @return <code>false</code> since this is not a data set state.
     **/
    public boolean isDatasetState() { return false; }

    /**
     * Determine if this state's primary display is for phase information
     * @return <code>false</code> since this is not a phase state.
     **/
    public boolean isPhaseState() { return false; }

    /**
     * Determine if this state's primary display is for project information.
     * @return <code>false</code> since this is not a project state.
     **/
    public boolean isProjectState() { return false; }
}
