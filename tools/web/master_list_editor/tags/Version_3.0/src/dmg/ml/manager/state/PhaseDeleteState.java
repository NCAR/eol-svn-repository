package dmg.ml.manager.state;

import dmg.jsf.model.TreeNode;
import dmg.ml.MasterListException;
import dmg.ml.bean.ClassificationBean;
import dmg.ml.bean.ClassificationTypeBean;
import dmg.ml.bean.DatasetBean;
import dmg.ml.bean.PhaseBean;
import dmg.ml.bean.ProjectBean;
import dmg.ml.manager.MasterListManager;
import dmg.ml.manager.Selector;
import javax.faces.event.ActionEvent;

/**
 * <p>The PhaseDeleteState class is a PhaseState used by a Master List
 * manager when its display is showing a confirmation for deleting a 
 * <code>PhaseBean</code> from the database.</p>
 * <p>It is a <code>ConcreteState</code> class in the <b>State</b> pattern
 * defined in the <u>Design Patterns</u> by Gamma, et al.</p>
 *
 * @author Joel Clawson
 */
public class PhaseDeleteState extends PhaseState {
    
    private MasterListState lastState;
    private PhaseBean phase;
    
    /**
     * Create a new instance of a PhaseDeleteState.
     * @param manager The manager that generated this state.
     * @param lastState The state the manager was in when this state was
     * generated.
     * @param phase The phase to be deleted from the database.
     **/
    public PhaseDeleteState(MasterListManager manager, MasterListState lastState, PhaseBean phase) {
        super(manager);
        this.lastState = lastState;
        this.phase = phase;
    }

    /**
     * Accept the confirmation that the currently selected phase is to be
     * deleted from the database and delete it.
     * @param evt The event that triggered the project deletion.
     * @return A state that displays a list of phases.
     * @throws MasterListException when there is a problem deleting the phase
     * from the database.
     **/
    public MasterListState accept(ActionEvent evt) throws MasterListException {
        phase.delete();

	ProjectBean project = ProjectBean.loadProject(phase.getProjectId());

	regenerateMenus(project);
        makeDatasetList(project);        
        return new PhaseListState(manager,phase);
    }

    /**
     * Cancel the deletion of the phase and return to the state the manager
     * was in before this delete state.
     * @param evt The event that triggered the cancelation.
     * @return The previous state of the Master List.
     * @throws MasterListException if there is a problem chaning the state.
     **/
    public MasterListState cancel(ActionEvent evt) throws MasterListException {
        return lastState;
    }

    /**
     * Clone the current phase.
     * @param evt The event that triggered the clone action.
     * @return The current state since cloning is not allowed from this state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState clone(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Change to a state that deletes a phase from the database.
     * @param evt The event that triggered the change of state.
     * @return The current state since it is a delete state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState delete(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Change to a state that edits a phase in the database.
     * @param evt The event that triggered the state change.
     * @return The current state since an edit state cannot be reached from
     * this state.
     * @throws MasterListException should never occur.
     **/
    public MasterListState edit(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Get the currently selected classification.
     * @return <code>null</code> since a classification cannot be selected in this
     * state.
     **/
    public ClassificationBean getClassification() { return null; }

    /**
     * Get the currently selected data set.
     * @return <code>null</code> since a data set cannot be selected in this
     * state.
     **/
    public DatasetBean getDataset() { return null; }

    /**
     * Get the currently selected phase.
     * @return The phase that is to be deleted from the database.
     **/
    public PhaseBean getPhase() { return phase; }

    /**
     * Get the root of the tree that displays a phase list.
     * @param treeState The container for holding the tree's nodes' expansion states.
     * @return <code>null</code> since a phase tree is not available in this state.
     * @throws MasterListException should never be thrown.
     **/
    public TreeNode getPhaseTreeRoot(TreeState treeState) throws MasterListException {
        return null;
    }
    
    /**
     * Get the currently selected project in the state.
     * @return <code>null</code. since a project cannot be selected in this state.
     **/
    public ProjectBean getProject() { return null; }

    /**
     * Get the selector being used in this state.
     * @return <code>null</code> since this state does not used a selector.
     **/
    public Selector getSelector() { return null; }

    /**
     * Hide the currently selected phase.
     * @param evt The event that triggered the hide action.
     * @return The current state since hiding is not allowed in this state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState hide(ActionEvent evt) throws MasterListException {
        return null;
    }

    /**
     * Determine if this state is a state that deletes an entry from the
     * database.
     * @return <code>true</code>
     **/
    public boolean isDeleteState() { return true; }

    /**
     * Determine if this state is a state that edits an entry from the
     * database.
     * @return <code>false</code>
     **/
    public boolean isEditState() { return false; }

    /**
     * Determine if this state is a state that displays a list of entries from the
     * database.
     * @return <code>false</code>
     **/
    public boolean isListState() { return false; }

    /**
     * Change to a state that views a list of classifications.
     * @param evt The event that triggered the change of state.
     * @return The current state since a classification list is not accessible from
     * this state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewClassificationList(ActionEvent evt) throws 
            MasterListException {
        return this;
    }

    /**
     * Change to a state that views a list of data sets.
     * @param evt The event that triggered the state change.
     * @return The current state since a data set list is not accessible from
     * this state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewDatasetList(ActionEvent evt) throws 
            MasterListException {
        return this;
    }

    /**
     * Change to a state that views a list of data sets.
     * @param projectId The id of the project the data set list is to be
     * displayed for.
     * @return The current state since a data set list is not accessible from
     * this state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewDatasetList(String projectId) throws 
            MasterListException {
        return this;
    }

    /**
     * Change to a state that views a list of projects.
     * @param evt The event that triggered the state change.
     * @return The current state since a project list is not accessible from
     * this state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewProjectList(ActionEvent evt) throws 
            MasterListException {
        return this;
    }

    /**
     * Change to a state that views a list of phases.
     * @param evt The event that triggered the change of state.
     * @return The current state since a phase list is not accessible from
     * this state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewPhaseList(ActionEvent evt) throws MasterListException {
        return this;
    }
}
