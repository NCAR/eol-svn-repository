package dmg.ml.manager.state.general;

import dmg.jsf.model.TreeNode;
import dmg.ml.MasterListException;
import dmg.ml.bean.general.CategoryBean;
import dmg.ml.bean.general.CategoryProjectBean;
import dmg.ml.bean.general.DatasetBean;
import dmg.ml.bean.general.DatasetProjectBean;
import dmg.ml.bean.general.ProjectBean;
import dmg.ml.bean.general.ProjectCategoryTreeRootBean;
import dmg.ml.manager.GeneralManager;
import dmg.ml.manager.Selector;
import dmg.ml.manager.state.MasterListState;
import dmg.ml.manager.state.TreeState;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.faces.event.ActionEvent;

/**
 * <p>The DatasetEditState is a DatasetState that is used by a Master List
 * manager when the display is showing a form for editting a 
 * <code>DatasetBean</code> in the database.</p>
 * <p>It is a <code>ConcreteState</code> class in the <b>State</b> pattern 
 * defined in the <u>Design Patterns</u> book by Gamma, et al.</p>
 *
 * @author Joel Clawson
 */
public class DatasetEditState extends DatasetState implements
        PropertyChangeListener {
    
    private DatasetBean dataset;
    private MasterListState lastState;
    private ProjectBean project;
    private Selector selector;
    
    /**
     * Create a new instance of the DatasetEditState.
     * @param manager The manager that generated this state.
     * @param lastState The state the manager was in before this edit state.
     * @param project The project the data set is associated with.
     * @throws MasterListException when there is a problem loading the data set.
     **/
    public DatasetEditState(GeneralManager manager, MasterListState lastState,
            ProjectBean project) throws MasterListException {
        this(manager,lastState,project,new DatasetProjectBean(null,project));
    }
    
    /**
     * Create a new instance of the DatasetEditState.
     * @param manager The manager that generated this state.
     * @param lastState The state the manager was in before this edit state.
     * @param project The project the data set is associated with.
     * @param dataset The dataset that is being editted.
     * @throws MasterListException when there is a problem loading the data set.
     **/
    public DatasetEditState(GeneralManager manager, MasterListState lastState,
            ProjectBean project, DatasetBean dataset) throws 
            MasterListException {
        super(manager);
        this.lastState = lastState;
        this.project = project;
        this.dataset = dataset;
        
        // Set up the category selector for the current dataset.
        selector = new Selector(CategoryProjectBean.getCategoryList(project));
        selector.setIncluded(dataset.getCategories());
        
        // Add a change listener to listen for changes to the dataset.
        dataset.addPropertyChangeListener(this);
    }

    /**
     * Save the changes that have been made in the data set edit form.  This 
     * will insert new data sets or update existing ones and update the data
     * set lists for all associated projects.
     * @param evt The event that triggered the acceptance of the state.
     * @return The state the manager was in before the edit form was set.
     * @throws MasterListException when there is a problem updating the 
     * database or generating the data set list files.
     **/
    public MasterListState accept(ActionEvent evt) throws MasterListException {
        dataset.setCategories(selector.getIncludedCategories());
        if (dataset.getOriginalId() == null || 
                dataset.getOriginalId().equals("")) {
            dataset.insert();
        } else {            
            dataset.update();
        }
        
        regenerateFiles(dataset);
        
        return new DatasetListState(manager,lastState.getProject(),
                lastState.getCategory(),getDataset());
    }

    /**
     * Cancel all changes that have been made in the edit form and return to the
     * previous state.
     * @param evt The event that triggered the cancellation of the edit.
     * @return The state the manager was in before it was put into this edit
     * state or the data set list state for the project if a previous state was
     * not defined.
     * @throws MasterListException when there is a problem changing state.
     **/
    public MasterListState cancel(ActionEvent evt) throws MasterListException {
        return lastState == null ? 
            new DatasetListState(manager,project,dataset) : lastState;
    }

    /**
     * Create a new data set from the current data set.  This function is not
     * implemented.
     * @param evt The event that triggered the cloning action.
     * @return The current state.
     * @throws MasterListException when there is a problem cloning the data set.
     **/
    public MasterListState clone(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Change the state to a state that deletes the current data set from the
     * database.
     * @param evt The event that caused the change of state.
     * @return The state that deletes the data set from the database.
     * @throws MasterListException when there is a problem changing the state
     * to a data set delete state.
     **/
    public MasterListState delete(ActionEvent evt) throws MasterListException {
        return new DatasetDeleteState(manager,this,getProject(),getDataset());
    }

    /**
     * Change the state to a state that edits a data set in the database.
     * @param evt The event that caused the change of state.
     * @return The current state since this is a data set edit state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState edit(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Get the currently selected category in the state.
     * @return <code>null</code> since a category cannot be selected in the
     * current state.
     **/
    public CategoryBean getCategory() { return null; }

    /**
     * Get the root node for the category tree.
     * @param treeState The container for the expansion state of the tree nodes.
     * @return <code>null</code> since a category tree is not displayed in
     * this state.
     * @throws MasterListException should never be thrown.
     **/
    public TreeNode getCategoryTreeRoot(TreeState treeState)
            throws MasterListException {
        return null;
    }

    /**
     * Get the currently selected data set that will be editted.
     * @return The data set that is to be editted.
     **/
    public DatasetBean getDataset() { return dataset; }

    /**
     * Get the root node for the data list display tree.
     * @param treeState The container for the expansion states of the tree nodes
     * @return <code>null</code> since a data set list tree is not displayed
     * in this state.
     * @throws MasterListException should never be thrown.
     **/
    public TreeNode getDatasetListRoot(TreeState treeState) 
            throws MasterListException {
        return null;
    }

    /**
     * Get the currently selected project which the data set is being edited in.
     * @return The project that is being associated with the data set.
     **/
    public ProjectBean getProject() { return project; }

    /**
     * Get the selector used for associating categories with the data set.
     * @return The Selector that associates the data set with categories.
     **/
    public Selector getSelector() { return selector; }

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
     * Determine if the current state is a state that deletes an entry from
     * the database.
     * @return <code>false</code>
     **/
    public boolean isDeleteState() { return false; }

    /**
     * Determine if the current state is a state that edits an entry in the
     * database.
     * @return <code>true</code>
     **/
    public boolean isEditState() { return true; }

    /**
     * Determine if the current state is a state that lists entries from the
     * database.
     * @return <code>false</code>
     **/
    public boolean isListState() { return false; }

    /**
     * Receive notifications from the bean when a property has changed in the
     * bean.
     * @param evt The event that triggered the property change.
     **/
    public void propertyChange(PropertyChangeEvent evt) {
        // When the data set id changes, update the selected categories.
        if (evt.getPropertyName().equals("original_id")) {
            try {
                selector.setIncluded(dataset.getCategories());
            } catch (MasterListException e) {
                manager.appendError(e);
            }
        }
    }

    /**
     * Change the state to view the list of categories in the database.
     * @param evt The event that triggered the change of state.
     * @return The current state since the category list is unreachable from the
     * current state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewCategoryList(ActionEvent evt)
            throws MasterListException {
        return this;
    }

    /**
     * Change the state to view a list of data sets.
     * @param evt The event that triggered the change of state.
     * @return The current state since a data set list is unreachable from the
     * current state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewDatasetList(ActionEvent evt)
            throws MasterListException {
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
    public MasterListState viewDatasetList(String projectId) 
            throws MasterListException {
        return this;
    }

    /**
     * Change the state to view a list of projects in the database.
     * @param evt The event that triggered the change of state.
     * @return The current state since a data set list is unreachable from the
     * current state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewProjectList(ActionEvent evt)
            throws MasterListException {
        return this;
    }
}
