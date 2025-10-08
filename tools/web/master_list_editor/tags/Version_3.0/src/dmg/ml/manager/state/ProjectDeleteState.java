package dmg.ml.manager.state;

import dmg.ml.MasterListException;
import dmg.ml.bean.ClassificationBean;
import dmg.ml.bean.DatasetBean;
import dmg.ml.bean.PhaseBean;
import dmg.ml.bean.ProjectBean;
import dmg.ml.manager.MasterListManager;
import dmg.ml.manager.Selector;
import dmg.ml.manager.state.MasterListState;
import javax.faces.event.ActionEvent;

/**
 * <p>The ProjectDeleteState class is a ProjectState used by a Master List
 * manager when its display is showing a confirmation for deleting a 
 * <code>ProjectBean</code> from the database.</p>
 * <p>It is a <code>ConcreteState</code> class in the <b>State</b> pattern
 * defined in the <u>Design Patterns</u> by Gamma, et al.</p>
 *
 * @author Joel Clawson
 */
public class ProjectDeleteState extends ProjectState {
    
    private MasterListState lastState;
    private ProjectBean project;

    /**
     * Create a new instance of a ProjectDeleteState.
     * @param manager The manager that generated this state.
     * @param lastState The state the manager was in when this state was
     * generated.
     * @param project The project to be deleted from the database.
     **/
    public ProjectDeleteState(MasterListManager manager, MasterListState lastState,
            ProjectBean project) {
        super(manager);
        this.lastState = lastState;
        this.project = project;
    }

    /**
     * Accept the confirmation that the currently selected project is to be
     * deleted from the database and delete it along with all of its data set
     * associations, data sets that are only associated with the project, and 
     * the classification associations.
     * @param evt The event that triggered the project deletion.
     * @return A state that displays a list of projects.
     * @throws MasterListException when there is a problem deleting the project
     * from the database.
     **/
    public MasterListState accept(ActionEvent evt) throws MasterListException {
        getProject().delete();
        return new ProjectListState(manager);
    }

    /**
     * Cancel the deletion of the project and return to the state the manager
     * was in before this delete state.
     * @param evt The event that triggered the cancelation.
     * @return The previous state of the Master List.
     * @throws MasterListException if there is a problem chaning the state.
     **/
    public MasterListState cancel(ActionEvent evt) throws MasterListException {
        return lastState;
    }

    /**
     * Clone the current project.
     * @param evt The event that triggered the clone action.
     * @return The current state since cloning is not allowed from this state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState clone(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Change to a state that deletes a project from the database.
     * @param evt The event that triggered the change of state.
     * @return The current state since it is a delete state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState delete(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Change to a state that edits a project in the database.
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
     * Get the currently selected project.
     * @return The project that is to be deleted from the database.
     **/
    public ProjectBean getProject() { return project; }

    /**
     * Get the selector used by the state for selecting classifications.
     * @return <code>null</code> since a selector is not used in this state.
     **/
    public Selector getSelector() { return null; }

    /**
     * Hide/unhide the project.
     * @param evt The event that triggered the hide/unhide action.
     * @return The current state since this state does not allow hide/unhide
     * actions.
     * @throws MasterListException should never be thrown.
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
     * Determine if this state edits an entry in the database.
     * @return <code>false</code>
     **/
    public boolean isEditState() { return false; }

    /**
     * Determine if this state lists entries in the database.
     * @return <code>false</code>
     **/
    public boolean isListState() { return false; }

    /**
     * Change the state to a state that lists classifications.
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
     * Change the state to a state that lists data sets.
     * @param evt The event that triggered the change of state.
     * @return The current state since a data set list is not accessible from
     * this state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewDatasetList(ActionEvent evt) throws 
            MasterListException {
        return this;
    }

    /**
     * Change the state to a state that lists data sets for the specified
     * project.
     * @param projectId The id of the project that is to have its data sets
     * listed.
     * @return The current state since a data set list is not accessible from
     * this state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewDatasetList(String projectId) throws 
            MasterListException {
        return this;
    }

    /**
     * Change the state to a state that lists projects.
     * @param evt The event that triggered the change of state.
     * @return The current state since a project list is not accessible from
     * this state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewProjectList(ActionEvent evt) throws 
            MasterListException {
        return this;
    }

    /**
     * Change the state to a state that lists phases.
     * @param evt The event that triggered the change of state.
     * @return The current state since a phase list is not accessible from
     * this state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewPhaseList(ActionEvent evt) throws MasterListException {
        return this;
    }
}
