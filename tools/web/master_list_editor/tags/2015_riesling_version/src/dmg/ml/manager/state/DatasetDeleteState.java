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
 * <p>The DatasetDeleteState is a DatasetState that is used by a Master List
 * manager when the display is showing a confirmation for deleting a 
 * <code>DatasetBean</code> from the database.</p>
 * <p>It is a <code>ConcreteState</code> class in the <b>State</b> pattern 
 * defined in <u>Design Patterns</u> book by Gamma, et al.</p>
 *
 * @author Joel Clawson
 */
public class DatasetDeleteState extends DatasetState {

    private DatasetBean dataset;
    private MasterListState lastState;
    private ProjectBean project;
    
    /**
     * Create a new instance of a DatasetDeleteState.
     * @param manager The manager that generated this state.
     * @param lastState The state the manager was in right before it was changed
     * into a delete state.
     * @param project The project the data set is being deleted from.
     * @param dataset The dataset that is to be deleted.
     **/
    public DatasetDeleteState(MasterListManager manager, MasterListState lastState,
            ProjectBean project, DatasetBean dataset) {
        super(manager);
        this.lastState = lastState;
        this.project = project;
        this.dataset = dataset;
    }
    
    /**
     * Accept the confirmation that the dataset should be deleted from the 
     * database for the current project.  This will also remove the data set if
     * it is only associated with the current project.
     * @param evt The event that triggered the accept confirmation.
     * @return A state that displays the list of data sets for the project.
     * @throws MasterListException when there is a problem deleting the dataset
     * from the database or regenerating the public data list for the project.
     **/
    public MasterListState accept(ActionEvent evt) throws MasterListException {
        dataset.delete();
        makeDatasetList(project);
        return new DatasetListState(manager,project,lastState.getClassificationType(),lastState.getClassification(),dataset);
    }

    /**
     * Cancel the delete of the data set and return to the previous state of
     * the manager.
     * @param evt The event that triggered the cancellation.
     * @return The state of the manager before this state was generated.
     * @throws MasterListException when there is a problem getting out of the
     * delete state.
     **/
    public MasterListState cancel(ActionEvent evt) throws MasterListException {
        return lastState;
    }

    /**
     * Clone a data set.  This function has no effect on the current state.
     * @param evt The event that caused the clone action.
     * @return The current state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState clone(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Change the current state to a delete state.
     * @param evt The event that triggered the change of state.
     * @return The current state since this is a delete state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState delete(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Change the current state to an edit state.
     * @param evt The event that triggered the change of state.
     * @return The current state since an edit state cannot be reached from this
     * state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState edit(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Get the currently selected classification in the state.
     * @return <code>null</code> since a classification cannot be selected in this
     * state.
     **/
    public ClassificationBean getClassification() { return null; }

    /**
     * Get the root node for the classification tree.
     * @param treeState The container for the expansion state of the tree nodes.
     * @return <code>null</code> since a classification tree is not displayed in
     * this state.
     * @throws MasterListException should never be thrown.
     **/
    public TreeNode getClassificationTreeRoot(TreeState treeState) throws MasterListException {
        return null;
    }

    /**
     * Get the currently selected classification type.
     * @return <code>null</code> since a type cannot be selected in this state.
     **/
    public ClassificationTypeBean getClassificationType() { return null; }

    /**
     * Get the currently selected data set that will be deleted.
     * @return The data set that is to be deleted.
     **/
    public DatasetBean getDataset() { return dataset; }

    /**
     * Get the root node for the data list display tree.
     * @param treeState The container for the expansion states of the tree nodes
     * @return <code>null</code> since a data set list tree is not displayed
     * in this state.
     * @throws MasterListException should never be thrown.
     **/
    public TreeNode getDatasetListRoot(TreeState treeState) throws MasterListException {
        return null;
    }

    /**
     * Get the currently selected project.
     * @return The project the data set is being deleted from.
     **/
    public ProjectBean getProject() { return project; }

    /**
     * Get the selector used for updating the data set.
     * @return <code>null</code> since a selector is not used in this state.
     **/
    public Selector getSelector() { return null; }

    /**
     * Hide/unhide the data set in the current project.
     * @param evt The event that triggered the hide/unhide action.
     * @return The current state since a data set cannot change the data set's
     * hide flag.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState hide(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Determine if this state is a state the deletes an entry from the 
     * database.
     * @return <code>true</code>
     **/
    public boolean isDeleteState() { return true; }

    /**
     * Determine if this state is a state that edits an entry in the database.
     * @return <code>false</code>
     **/
    public boolean isEditState() { return false; }

    /**
     * Determine if this state lists entries from the database.
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
     * @return The current state since a data set list is unreachable from the
     * current state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewProjectList(ActionEvent evt) throws MasterListException {
        return this;
    }    

    /**
     * Get the currently selected phase in the state.
     * @return <code>null</code> since a phase cannot be selected in this state.
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
