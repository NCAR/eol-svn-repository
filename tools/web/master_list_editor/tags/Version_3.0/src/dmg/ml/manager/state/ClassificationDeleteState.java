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
import dmg.ml.manager.state.MasterListState;
import dmg.ml.manager.state.TreeState;
import javax.faces.event.ActionEvent;

/**
 * <p>The ClassificationDeleteState is a ClassificationState that is used by a Master List
 * manager when the display is showing a form for deleting a 
 * <code>ClassificationBean</code> from the database.</p>
 * <p>It is a <code>ConcreteState</code> class in the <b>State</b> pattern 
 * defined in the <u>Design Patterns</u> book by Gamma, et al.</p>
 *
 * @author Joel Clawson
 */
public class ClassificationDeleteState extends ClassificationState {
    
    private ClassificationBean classification;
    private MasterListState lastState;
    
    /**
     * Create a new instance of a ClassificationDeleteState.
     * @param manager The manager that generated this state.
     * @param lastState The state of the manager when this state was created.
     * @param classification The classification to be deleted from the database.
     **/
    public ClassificationDeleteState(MasterListManager manager, 
            MasterListState lastState, ClassificationBean classification) {
        super(manager);
        this.lastState = lastState;
        this.classification = classification;
    }

    /**
     * Accept the confirmation from the form by deleting the classification from the 
     * database.
     * @param evt The event that triggered the accept action.
     * @return A list of classifications with the deleted classification removed.
     * @throws MasterListException when there is a problem deleting the
     * classification.
     **/
    public MasterListState accept(ActionEvent evt) throws MasterListException {
        getClassification().delete();
        return new ClassificationListState(manager);
    }

    /**
     * Cancel the changes made in the form and return to the previous state of
     * the Master List.
     * @param evt The event that triggered the cancel action.
     * @return The previous state of the Master List.
     **/
    public MasterListState cancel(ActionEvent evt) throws MasterListException {
        return lastState;
    }

    /**
     * Make a copy of the current classification.
     * @param evt The event that triggered the clone action.
     * @return The current state since cloning a classification is not allowed.
     **/
    public MasterListState clone(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Change the state of the Master List to delete the classification.
     * @param evt The event that caused the delete action.
     * @return The current state since it is a delete state for the classification.
     **/
    public MasterListState delete(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Change the state of the Master List to edit the classification.
     * @param evt The event that caused the edit action.
     * @return The current state since a classification cannot be editted from this
     * state.
     **/
    public MasterListState edit(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Get the classification that is being editted.
     * @return The classification that is being editted.
     **/
    public ClassificationBean getClassification() { return classification; }

    /**
     * Get the root node for the classification tree.
     * @param treeState The container for tree's nodes' expansion states.
     * @return <code>null</code> since a classification tree is not viewable from this
     * state.
     **/
    public TreeNode getClassificationTreeRoot(TreeState treeState) throws 
            MasterListException {
        return null;
    }

    /**
     * Get the classification type taht is assigned to this state.
     * @return <code>null</code> since a classification type cannot be selected in this state.
     **/
    public ClassificationTypeBean getClassificationType() { return null; }
    
    /**
     * Get the data set that is assigned to this state.
     * @return <code>null</code> since a data set cannot be selected in this
     * state.
     **/
    public DatasetBean getDataset() { return null; }

    /**
     * Get the root of the tree for a data set list.
     * @param treeState A container for the states of the tree nodes.
     * @return <code>null</code> since a data set list is not viewable from
     * this state.
     **/
    public TreeNode getDatasetListRoot(TreeState treeState) throws MasterListException {
        return null;
    }

    /**
     * Get the selected project.
     * @return <code>null</code> since a project cannot be selected in this
     * state.
     **/
    public ProjectBean getProject() { return null; }

    /**
     * Get the selector used by the state.
     * @return <code>null</code> since a selector is not used in this state.
     **/
    public Selector getSelector() { return null; }

    /**
     * Hide/unhide the currently selected classification.
     * @param evt The event that triggered the change of hide status.
     * @return The current state since a classification cannot be hidden.
     **/
    public MasterListState hide(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Determine if this state is a state that deletes an entry from the
     * database.
     * @return <code>true</code>
     **/
    public boolean isDeleteState() { return true; }

    /**
     * Determine if the current state is a state that edits an entry in the
     * database.
     * @return <code>false</code>
     **/
    public boolean isEditState() { return false; }

    /**
     * Determine if the current state is a state that lists entries from the
     * database.
     * @return <code>false</code>
     **/
    public boolean isListState() { return false; }

    /**
     * Change the state to view the list of classifications in the database.
     * @param evt The event that triggered the change of state.
     * @return The current state since the classification list is unreachable from the
     * current state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewClassificationList(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Change the state to view a list of data sets.
     * @param evt The event that triggered the change of state.
     * @return The current state since a data set list is unreachable from the
     * current state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewDatasetList(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Change the state to view a list of data sets.
     * @param projectId The id of the project to use to display the data set
     * list.
     * @return The current state since a data set list is unreachable from the
     * current state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewDatasetList(String projectId) throws MasterListException {
        return this;
    }

    /**
     * Change the state to view a list of projects in the database.
     * @param evt The event that triggered the change of state.
     * @return The current state since a project list is unreachable from the
     * current state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewProjectList(ActionEvent evt) throws
            MasterListException {
        return this;
    }

    /**
     * Get the phase that is assigned to this state.
     * @return <code>null</code> since a phase cannot be assigned to this state.
     **/
    public PhaseBean getPhase() { return null; }

    /**
     * Change the state to view a list of phases in the database.
     * @param evt The event that triggered the change of state.
     * @return The current state since a phase list is unreachable from the
     * current state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewPhaseList(ActionEvent evt) throws MasterListException {
        return this;
    }
}
