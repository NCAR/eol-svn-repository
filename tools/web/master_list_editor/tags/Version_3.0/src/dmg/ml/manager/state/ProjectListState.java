package dmg.ml.manager.state;

import dmg.ml.MasterListException;
import dmg.ml.bean.ClassificationBean;
import dmg.ml.bean.DatasetBean;
import dmg.ml.bean.PhaseBean;
import dmg.ml.bean.ProjectBean;
import dmg.ml.manager.MasterListManager;
import dmg.ml.manager.Selector;
import dmg.ml.manager.state.MasterListState;
import javax.faces.component.UIParameter;
import javax.faces.event.ActionEvent;

/**
 * <p>The ProjectListState is a ProjectState that is used by a Master List
 * manager when the display is showing a list of Projects.</p>
 * <p>It is a <code>ConcreteState</code> class in the <b>State</b> pattern 
 * defined in <u>Design Patterns</u> book by Gamma, et al.</p>
 *
 * @author Joel Clawson
 */
public class ProjectListState extends ProjectState {

    private ProjectBean project;
    
    /**
     * Create a new instance of a ProjectListState.
     * @param manager The manager that generated this state.
     **/
    public ProjectListState(MasterListManager manager) { super(manager); }

    /**
     * Save all information in this state to the database.  This function has
     * no effect in this state.
     * @param evt The event that triggered the accepting of the page.
     * @return The current state.
     **/
    public MasterListState accept(ActionEvent evt) { return this; }

    /**
     * Cancel all changes made in the current state.  This function has no
     * effect in this state.
     * @param evt The event that triggered the cancellation.
     * @return The current state.
     **/
    public MasterListState cancel(ActionEvent evt) { return this; }
    
    /**
     * Make a copy of the project selected in the current state.  This function
     * is not currently implemented and returns the current state.
     * @param evt The event that triggered the cloning.
     * @return The current state.
     **/
    public MasterListState clone(ActionEvent evt) {
        
        return this;
    }

    /**
     * Change to a ProjectDeleteState from the current state.
     * @param evt The event that triggered the state change.
     * @return A state that deletes a Project from the Master List.
     * @throws MasterListException when there is a problem loading the project
     * from the database.
     **/
    public MasterListState delete(ActionEvent evt) throws MasterListException {
        UIParameter param = findParameter(evt.getComponent(),"projectId");
        ProjectBean project = ProjectBean.loadProject((String)param.getValue());
        return new ProjectDeleteState(manager,this,project);
    }

    /**
     * Change to a ProjectEditState from the current state.
     * @param evt The event that triggered the state change.
     * @return A state that allows for the editting of a Project.
     * @throws MasterListException if there is a problem loading the project
     * from the database.
     **/
    public MasterListState edit(ActionEvent evt) throws MasterListException {
        UIParameter param = null;
        
        // Search for the projectId parameter.
        try { param = findParameter(evt.getComponent(),"projectId"); }
        catch (MasterListException e) {
            return new ProjectEditState(manager,this);
        }

        // Load the project from the database.
        ProjectBean project = ProjectBean.loadProject((String)param.getValue());

        return new ProjectEditState(manager,this,project);    
    }
    
    /**
     * Get the currently selected Classification in the state.
     * @return <code>null</code> since a classification cannot be selected in this
     * state.
     **/
    public ClassificationBean getClassification() { return null; }
    
    /**
     * Get the currently selected Dataset in the state.
     * @return <code>null</code> since a data set cannot be selected in this
     * state.
     **/
    public DatasetBean getDataset() { return null; }
    
    /**
     * Get the currently selected Project in the state.
     * @return The current project.
     **/
    public ProjectBean getProject() { return project; }

    /**
     * Get the current Selector in the state.
     * @return <code>null</code> since a selector is not allowed in this state.
     **/
    public Selector getSelector() { return null; }

    /**
     * Hide the entire Project's Master List.  This function returns the 
     * current state since a project cannot be hidden.
     * @param evt The event that caused the hide action.
     * @return The current state. 
     **/
    public MasterListState hide(ActionEvent evt) { return this; }

    /**
     * Determine if the current state is a state that deletes a project.
     * @return <code>false</code>
     **/
    public boolean isDeleteState() { return false; }

    /**
     * Determine if the current state is a state that edits a project.
     * @return <code>false</code>
     **/
    public boolean isEditState() { return false; }

    /**
     * Determine if the current state is a state that lists project information.
     * @return <code>true</code>
     **/
    public boolean isListState() { return true; }

    /**
     * Change the current state to a state that displays a list of classifications.
     * @param evt The event that caused the state change.
     * @return A state that displays a list of classifications.
     **/
    public MasterListState viewClassificationList(ActionEvent evt) {
        return new ClassificationListState(manager);
    }

    /**
     * Change the current state to a state that displays a list of data sets.
     * This requires the component that fired the event to have a paramter with
     * the name of <code>projectId</code> with the id of the project as its
     * value for the data set list to be displayed.
     * @param evt The event that triggered the state change.
     * @return A state that displays a list of data sets.
     * @throws MasterListException if there is a problem changing to a data set
     * list state.
     **/
    public MasterListState viewDatasetList(ActionEvent evt)
            throws MasterListException {
        try {
            // Set the dataset list to display just datasets in a project
            UIParameter param = findParameter(evt.getComponent(),"projectId");
            ProjectBean project = ProjectBean.loadProject((String)param.getValue());
            return new DatasetListState(manager,project);
        } catch (MasterListException e) {
            // Set the dataset list to display all datasets in the database.
            return new DatasetListState(manager);
        }
    }

    /**
     * Change the current state to a state that displays a list of data sets for
     * the specified project.
     * @param projectId The id of the project to have the data set list
     * displayed.
     * @return A state that displays a data set list for the project or the
     * current state if the projectId is not defined.
     * @throws MasterListException if there is a problem changing the state to
     * a data set list state.
     **/
    public MasterListState viewDatasetList(String projectId)
            throws MasterListException {
        if (projectId == null || projectId.equals("")) { return this; }
        else {
            ProjectBean project = ProjectBean.loadProject(projectId);
            return new DatasetListState(manager,project);
        }
    }

    /**
     * Change the current state to a state that displays the list of projects.
     * @param evt The event that triggered the state change.
     * @return The current state.
     **/
    public MasterListState viewProjectList(ActionEvent evt) { return this; }

    /**
     * Change the current state to a state that displays a list of phases.
     * @param evt The event that caused the state change.
     * @return A state that displays a list of phases.
     **/
    public MasterListState viewPhaseList(ActionEvent evt) throws MasterListException {
        return new PhaseListState(manager);
    }
}
