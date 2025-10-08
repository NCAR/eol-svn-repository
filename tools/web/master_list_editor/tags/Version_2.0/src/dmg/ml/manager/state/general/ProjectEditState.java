package dmg.ml.manager.state.general;

import dmg.ml.MasterListException;
import dmg.ml.bean.general.CategoryBean;
import dmg.ml.bean.general.CategoryProjectBean;
import dmg.ml.bean.general.DatasetBean;
import dmg.ml.bean.general.ProjectBean;
import dmg.ml.manager.GeneralManager;
import dmg.ml.manager.Selector;
import dmg.ml.manager.state.MasterListState;
import javax.faces.event.ActionEvent;

/**
 * <p>The ProjectEditState is a ProjectState that is used by a Master List
 * manager when the display is showing a form for editting a 
 * <code>ProjectBean</code> in the database.</p>
 * <p>It is a <code>ConcreteState</code> class in the <b>State</b> pattern
 * defined in the <u>Design Patterns</u> by Gamma, et al.</p>
 *
 * @author Joel Clawson
 */
public class ProjectEditState extends ProjectState {

    private MasterListState lastState;
    private ProjectBean project;
    private Selector selector;
    
    /**
     * Create a new instance of the ProjectEditState.
     * @param manager The manager that generated this state.
     * @param lastState The state the manager was in when this state was
     * generated.
     * @throws MasterListException when there is a problem creating the state.
     **/
    public ProjectEditState(GeneralManager manager, MasterListState lastState)
            throws MasterListException {
        this(manager,lastState,new ProjectBean());
    }
    
    /**
     * Create a new instance of the ProjectEditState.
     * @param manager The manager that generated this state.
     * @param lastState The state the manager was in when this state was
     * generated.
     * @throws MasterListException when there is a problem creating the state.
     **/
    public ProjectEditState(GeneralManager manager, MasterListState lastState,
            ProjectBean project) throws MasterListException {
        super(manager);
        this.lastState = lastState;
        this.project = project;
        
        // Set up the category selector for the project.
        selector = new Selector(CategoryBean.getCategories());
        selector.setIncluded(CategoryProjectBean.getCategoryList(project));
    }

    /**
     * Accept the changes that were made in the form and make the changes in
     * the database.
     * @param evt The event that triggered the accept action.
     * @return The state that displays the list of data sets for the project
     * that was just editted.
     * @throws MasterListException when there is a problem inserting or updating
     * the database with the data in the form.
     **/
    public MasterListState accept(ActionEvent evt) throws MasterListException {
        project.setCategories(selector.getIncludedCategories());
        if (project.getOriginalId() == null || 
                project.getOriginalId().equals("")) {
            project.insert();
        } else {
            project.update();
        }
       
        // Make the files for the project.
        generateIndexFile(project);
        makeDatasetList(project);
        regenerateMenus(project);
                
        return new DatasetListState(manager,getProject());
    }

    /**
     * Cancel the changes in the form and return to the previous state of the
     * Master List.
     * @param evt The event that triggered the cancel action.
     * @return The previous state of the Master List.
     * @throws MasterListException should never occur.
     **/
    public MasterListState cancel(ActionEvent evt) throws MasterListException {        
        return lastState;
    }

    /**
     * Clone the current project into a new project.
     * @param evt The event that triggered the cloning.
     * @return The current state since cloning is not allowed from this state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState clone(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Change to the state that will delete this project from the database.
     * @param evt The event that triggered the change of state.
     * @return The state that deletes the current project from the database.
     * @throws MasterListException if there is a problem chaning the state.
     **/
    public MasterListState delete(ActionEvent evt) throws MasterListException {
        return new ProjectDeleteState(manager,this,getProject());
    }

    /**
     * Change to the state that will edit this project in the database.
     * @param evt The event that triggered the change of state.
     * @return The current state since it already is an edit state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState edit(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Get the currently selected category defined in the state.
     * @return <code>null</code> since a category cannot be selected in this
     * state.
     **/
    public CategoryBean getCategory() { return null; }

    /**
     * Get the currently selected data set in the state.
     * @return <code>null</code> since a data set cannot be selected in this
     * state.
     **/
    public DatasetBean getDataset() { return null; }

    /**
     * Get the currently selected project in the state.
     * @return The project being editted in this state.
     **/
    public ProjectBean getProject() { return project; }

    /**
     * Get the Selector used for choosing the associated categories for the
     * project.
     * @return The category selector for the project.
     **/
    public Selector getSelector() { return selector; }

    /**
     * Hide the currently selected project.
     * @param evt The event that triggered the hide action.
     * @return The current state since hiding is not allowed in this state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState hide(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Determine if this state is a state that deletes an entry from the
     * database.
     * @return <code>false</code>
     **/
    public boolean isDeleteState() { return false; }

    /**
     * Determine if this state is a state that edits an entry in the database.
     * @return <code>true</code>
     **/
    public boolean isEditState() { return true; }

    /**
     * Determine if this state is a state that lists entries from the database.
     * @return <code>false</code>
     **/
    public boolean isListState() { return false; }

    /**
     * Change to a state that views a list of categories.
     * @param evt The event that triggered the change of state.
     * @return The current state since a category list is not accessible from
     * this state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewCategoryList(ActionEvent evt) throws 
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
}
