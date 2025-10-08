package dmg.ml.manager.state.general;

import dmg.jsf.model.TreeNode;
import dmg.ml.MasterListException;
import dmg.ml.bean.general.CategoryBean;
import dmg.ml.bean.general.DatasetBean;
import dmg.ml.bean.general.ProjectBean;
import dmg.ml.manager.GeneralManager;
import dmg.ml.manager.Selector;
import dmg.ml.manager.state.MasterListState;
import dmg.ml.manager.state.TreeState;
import javax.faces.event.ActionEvent;

/**
 * <p>The CategoryEditState is a CategoryState that is used by a Master List
 * manager when the display is showing a form for editting a 
 * <code>CategoryBean</code> in the database.</p>
 * <p>It is a <code>ConcreteState</code> class in the <b>State</b> pattern 
 * defined in the <u>Design Patterns</u> book by Gamma, et al.</p>
 *
 * @author Joel Clawson
 */
public class CategoryEditState extends CategoryState {
    
    private CategoryBean category;
    private MasterListState lastState;
    
    /**
     * Create a new instance of a CategoryEditState.
     * @param manager The manager that generated this state.
     * @param lastState The state of the manager that generated this state.
     * @param category The category being edited in this state.
     **/
    public CategoryEditState(GeneralManager manager, MasterListState lastState,
            CategoryBean category) {
        super(manager);
        this.lastState = lastState;
        this.category = category;
    }

    /**
     * Accept the changes in the form by inserting or updating the category as
     * appropriate.
     * @param evt The event that triggered the accept action.
     * @return A list of categories with the database changes made.
     * @throws MasterListException when there is a problem updating the
     * database.
     **/
    public MasterListState accept(ActionEvent evt) throws MasterListException {
        if (getCategory().getCategoryId() == null || 
                getCategory().getCategoryId() == 0) {
            getCategory().insert();
        } else {
            getCategory().update();

            // Make the files for all projects associated with the category.
            for (ProjectBean project: getCategory().getProjects()) {
                regenerateMenus(project);
                makeDatasetList(project);
            }
        }
               
        return new CategoryListState(manager,getCategory());
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
     * Make a copy of the current category.
     * @param evt The event that triggered the clone action.
     * @return The current state since cloning a category is not allowed.
     **/
    public MasterListState clone(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Change to a state that confirms a deletion of a category.
     * @param evt The event that triggered the delete action.
     * @return The state that allows the category to be deleted from the
     * database.
     * @throws MasterListException if there is a problem changing to the delete
     * state for the category.
     **/
    public MasterListState delete(ActionEvent evt) throws MasterListException {
        return new CategoryDeleteState(manager,this,getCategory());
    }

    /**
     * Change to a state that edits the category.
     * @param evt The event that triggered the edit action.
     * @return The current state since it is an edit state for the category.
     **/
    public MasterListState edit(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Get the category that is being editted.
     * @return The category that is being editted.
     **/
    public CategoryBean getCategory() { return category; }

    /**
     * Get the root node for the category tree.
     * @param treeState The container for tree's nodes' expansion states.
     * @return <code>null</code> since a category tree is not viewable from this
     * state.
     **/
    public TreeNode getCategoryTreeRoot(TreeState treeState) throws 
            MasterListException {
        return null;
    }

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
    public TreeNode getDatasetListRoot(TreeState treeState) throws 
            MasterListException {
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
     * Hide/unhide the currently selected category.
     * @param evt The event that triggered the change of hide status.
     * @return The current state since a category cannot be hidden.
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
     * Determine if this state lists entries from the database.
     * @return <code>false</code>
     **/
    public boolean isListState() { return false; }

    /**
     * Change the state to view the list of categories in the database.
     * @param evt The event that triggered the change of state.
     * @return The current state since the category list is unreachable from the
     * current state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewCategoryList(ActionEvent evt) throws 
            MasterListException {
        return this;
    }

    /**
     * Change the state to view a list of data sets.
     * @param evt The event that triggered the change of state.
     * @return The current state since a data set list is unreachable from the
     * current state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewDatasetList(ActionEvent evt) throws 
            MasterListException {
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
    public MasterListState viewDatasetList(String projectId) throws 
            MasterListException {
        return this;
    }

    /**
     * Change the state to view a list of projects in the database.
     * @param evt The event that triggered the change of state.
     * @return The current state since a data set list is unreachable from the
     * current state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewProjectList(ActionEvent evt) throws 
            MasterListException {
        return this;
    }
}
