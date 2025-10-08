package dmg.ml.manager.state.general;

import dmg.jsf.model.TreeNode;
import dmg.ml.MasterListException;
import dmg.ml.bean.general.CategoryBean;
import dmg.ml.bean.general.CategoryProjectBean;
import dmg.ml.bean.general.DatasetBean;
import dmg.ml.bean.general.DatasetProjectBean;
import dmg.ml.bean.general.ProjectBean;
import dmg.ml.bean.general.ProjectCategoryTreeRootBean;
import dmg.ml.bean.general.ProjectDatasetListRootBean;
import dmg.ml.manager.GeneralManager;
import dmg.ml.manager.Selector;
import dmg.ml.manager.state.MasterListState;
import dmg.ml.manager.state.TreeState;
import javax.faces.component.UIParameter;
import javax.faces.event.ActionEvent;

/**
 * <p>The DatasetListState is a DatasetState that is used by a Master List
 * manager when the display is showing a list of data sets from the 
 * database.</p>
 * <p>It is a <code>ConcreteState</code> class in the <b>State</b> pattern 
 * defined in <u>Design Patterns</u> book by Gamma, et al.</p>
 *
 * @author Joel Clawson
 */
public class DatasetListState extends DatasetState {

    private CategoryBean category;
    private DatasetBean dataset;
    private ProjectBean project;

    /**
     * Create a new instance of a DatasetListState.
     * @param manager The manager that generated this state.
     **/
    public DatasetListState(GeneralManager manager) { super(manager); }
    
    /**
     * Create a new instance of a DatasetListState.
     * @param manager The manager that generated this state.
     * @param project The project that the data set list is being displayed for.
     **/
    public DatasetListState(GeneralManager manager, ProjectBean project) {
        super(manager);
        this.project = project;
    }

    /**
     * Create a new instance of a DatasetListState.
     * @param manager The manager that generated this state.
     * @param project The project that the data set list is being displayed for.
     * @param dataset The dataset that was last changed.
     **/
    public DatasetListState(GeneralManager manager, ProjectBean project,
            DatasetBean dataset) {
        super(manager);
        this.project = project;
        this.dataset = dataset;
    }

    /**
     * Create a new instance of a DatasetListState.
     * @param manager The manager that generated this state.
     * @param project The project that the data set list is being displayed for.
     * @param category The category that is being displayed for the project.
     * @param dataset The dataset that was last changed.
     **/
    public DatasetListState(GeneralManager manager, ProjectBean project,
            CategoryBean category, DatasetBean dataset) {
        super(manager);
        this.project = project;
        this.category = category;
        this.dataset = dataset;
    }
    
    /**
     * Save the current changes in the list to the database.  This function is
     * not implemented.
     * @param evt The event that triggered the acceptance.
     * @return The current state.
     * @throws MasterListException should not be thrown.
     **/
    public MasterListState accept(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Cancel all changes that have been made in the list.
     * @param evt The event that triggered the cancellation of the edit.
     * @return The current state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState cancel(ActionEvent evt) throws MasterListException {
        return this;
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
     * Change to a state that deletes a data set from the database for the 
     * selected project.  This requires the component that generated the event
     * must have a parameter called <code>datasetId</code> with the dataset id
     * as its value.
     * @param evt The event that triggered the state change.
     * @return A state that deletes a data set from the database.
     * @throws MasterListException if there is a problem changing the state or
     * if the "datasetId" parameter cannot be found.
     **/
    public MasterListState delete(ActionEvent evt) throws MasterListException {
        DatasetProjectBean dataset = DatasetProjectBean.loadDataset(
                (String)findParameter(evt.getComponent(),
                                "datasetId").getValue(),getProject());

        return new DatasetDeleteState(manager,this,getProject(),dataset);
    }

    /**
     * Change to a state that edits a data set in the database for the 
     * selected project.  This requires the component that generated the event
     * must have a parameter called <code>datasetId</code> with the dataset id
     * as its value if a specified data set is to be editted.
     * @param evt The event that triggered the state change.
     * @return A state that edits a data set from the database.
     * @throws MasterListException if there is a problem changing the state.
     **/
    public MasterListState edit(ActionEvent evt) throws MasterListException {
        UIParameter param = null;
        
        // Search for the projectId parameter for editing the current project.
        try {
            param = findParameter(evt.getComponent(),"projectId");
            return new ProjectEditState(manager,this,
                    ProjectBean.loadProject((String)param.getValue()));
        } catch (MasterListException e) {}
        
        // Search for the datasetId parameter.
        try { param = findParameter(evt.getComponent(),"datasetId"); }
        catch (MasterListException e) {
            return new DatasetEditState(manager,this,getProject());
        }

        // Load the dataset from the database for the specified project.
        DatasetBean dataset = null;
        if (getProject() == null) {
            dataset = DatasetBean.loadDataset((String)param.getValue());
        } else {
            dataset = DatasetProjectBean.loadDataset((String)param.getValue(),
                    getProject());
        }
        return new DatasetEditState(manager,this,getProject(),dataset);
    }
    
    /**
     * Get the currently selected category.
     * @return The selected category or <code>null</code> if a category is
     * not selected.
     **/
    public CategoryBean getCategory() { return category; }

    /**
     * Get the root node that generates the category tree.
     * @param treeState The container for the expansion states for the noes in
     * the category tree.
     * @return The category tree's root node.
     * @throws MasterListException if there is a problem generating the root
     * node.
     **/
    public TreeNode getCategoryTreeRoot(TreeState treeState) 
            throws MasterListException {
        return new ProjectCategoryTreeRootBean(treeState,getProject());
    }
    
    /**
     * Get the currently selected dataset.
     * @return The currently selected dataset or <code>null</code> if a data
     * set is not selected.
     **/
    public DatasetBean getDataset() { return dataset; }

    /**
     * Get the root node that generates the dataset list.
     * @param treeState The container for the expansion states for the nodes in
     * the dataset list.
     * @return The data set list's root node.
     * @throws MasterListException if there is a problem generating the root
     * node.
     **/
    public TreeNode getDatasetListRoot(TreeState treeState)
            throws MasterListException {
        return new ProjectDatasetListRootBean(treeState,getProject(),category);
    }

    /**
     * Get the currently selected project.
     * @return The currently selected project or <code>null</code> if a data
     * set is not selected.
     **/
    public ProjectBean getProject() { return project; }

    /**
     * Get the selector used for updating the data set.
     * @return <code>null</code> since a selector is not used in this state.
     **/
    public Selector getSelector() { return null; }

    /**
     * Hide/unhide a data set or category in the data set list.
     * @param evt The event that triggered the hide/unhide.
     * @return The current state after the entry has been hidden/unhidden.
     * @throws MasterListException when there is a problem hiding/unhiding an
     * entry from the database.
     **/
    public MasterListState hide(ActionEvent evt) throws MasterListException {
        UIParameter param;
        try {
            // Search for a datasetId parameter
            param = findParameter(evt.getComponent(),"datasetId");
        } catch (MasterListException e) {
            try {
                // Search for a categoryId parameter
                param = findParameter(evt.getComponent(),"categoryId");
            } catch (MasterListException ex) {
                throw new MasterListException("Cannot find parameter datasetId"+
                        " or categoryId.");
            }
        }

        if (param.getName().equals("datasetId")) {
            // Change the hide state of a data set.
            DatasetProjectBean dataset = DatasetProjectBean.loadDataset(
                    (String)param.getValue(),getProject());
            dataset.updateHideStatus(!dataset.isHidden());
            this.dataset = dataset;
        } else if (param.getName().equals("categoryId")) {
            // Change the hide state of a category.
            CategoryProjectBean category = CategoryProjectBean.loadCategory(
                    (Integer)param.getValue(),getProject());
            category.updateHideStatus(!category.isHidden());
            regenerateMenus(getProject());
        }
        
        // Regenerate the data set list for the project.
        makeDatasetList(getProject());
        
        return this;
    }

    /**
     * Determine if this state deletes an entry from the database.
     * @return <code>false</code>
     **/
    public boolean isDeleteState() { return false; }

    /**
     * Determine if this state edits an entry from the database.
     * @return <code>false</code>
     **/
    public boolean isEditState() { return false; }

    /**
     * Determine if this state lists entries from the database.
     * @return <code>true</code>
     **/
    public boolean isListState() { return true; }

    /**
     * Change the state to view the list of categories in the database.
     * @param evt The event that triggered the change of state.
     * @return A state that views a list of categories.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewCategoryList(ActionEvent evt)
            throws MasterListException {
        return new CategoryListState(manager);
    }

    /**
     * Change the state to view a list of data sets.  Limit the list to only 
     * data sets in a category if the component that generated the event has
     * a parameter with a <code>categoryId</code> defined.
     * @param evt The event that triggered the change of state.
     * @return The current state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewDatasetList(ActionEvent evt)
            throws MasterListException {
        try {
            UIParameter param = findParameter(evt.getComponent(),"categoryId");
            category = CategoryBean.loadCategory((Integer)param.getValue());
        } catch (MasterListException e) {
            category = null;
        }
        
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
     * @return A state that displays a list of projects.
     * @throws MasterListException if there is a problem changing the state to
     * a project list state.
     **/
    public MasterListState viewProjectList(ActionEvent evt)
            throws MasterListException {
        return new ProjectListState(manager);
    }
}
