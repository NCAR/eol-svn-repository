package dmg.ml.manager.state.general;

import dmg.jsf.TreeException;
import dmg.jsf.model.TreeNode;
import dmg.ml.MasterListException;
import dmg.ml.bean.general.CategoryBean;
import dmg.ml.bean.general.DatasetBean;
import dmg.ml.bean.general.ProjectBean;
import dmg.ml.bean.general.ProjectCategoryTreeRootBean;
import dmg.ml.manager.GeneralManager;
import dmg.ml.manager.Selector;
import dmg.ml.manager.state.MasterListState;
import dmg.ml.manager.state.TreeState;
import java.util.List;
import javax.faces.component.UIParameter;
import javax.faces.event.ActionEvent;

/**
 * <p>The CategoryListState is a DatasetState that is used by a Master List
 * manager when the display is showing a list of categories from the 
 * database.</p>
 * <p>It is a <code>ConcreteState</code> class in the <b>State</b> pattern 
 * defined in the <u>Design Patterns</u> book by Gamma, et al.</p>
 *
 * @author Joel Clawson
 */
public class CategoryListState extends CategoryState{
    
    private CategoryBean category;
    
    /**
     * Create a new instance of a CategoryListState.
     * @param manager The manager that generated this state.
     **/
    public CategoryListState(GeneralManager manager) { super(manager); }
    
    /**
     * Create a new instance of a CategoryListState.
     * @param manager The manager that generated this state.
     * @param category The category that is to be emphasized in this state.
     **/
    public CategoryListState(GeneralManager manager, CategoryBean category) {
        this(manager);
        this.category = category;
    }

    /**
     * Accept the changes made in the current state and save them to the 
     * database.
     * @param evt The event that triggered the accept action.
     * @return The current state since an accept action is not allowed in this
     * state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState accept(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Cancel the changes made in the current state and return to the previous
     * state of the manager.
     * @param evt The event that triggered the cancel action.
     * @return The current state since a cancel action is not allowed in this
     * state.
     * @throws MasterListException should never occur.
     **/
    public MasterListState cancel(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Clone an entry in the database.
     * @param evt The event that triggered the clone action.
     * @return The current state since a clone action is not allowed in this
     * state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState clone(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Change to a state that will delete a category from the database.
     * @param evt The event that triggered the change of state.
     * @return The state that allows a category to be deleted from the database.
     * @throws MasterListException if there is a problem loading the category
     * that is to be deleted from the database.
     **/
    public MasterListState delete(ActionEvent evt) throws MasterListException {
        UIParameter param = findParameter(evt.getComponent(),"categoryId");
        CategoryBean category =
                CategoryBean.loadCategory((Integer)param.getValue());
        return new CategoryDeleteState(manager,this,category);
    }

    /**
     * Change to a state that will edit a category in the database.
     * @param evt The event that triggered the change of state.
     * @return The state that allows a category to be edited in the database.
     * @throws MasterListException if there is a problem loading the category
     * to be edited in the database.
     **/
    public MasterListState edit(ActionEvent evt) throws MasterListException {
        try {
            UIParameter param = findParameter(evt.getComponent(),"categoryId");
            CategoryBean category = 
                    CategoryBean.loadCategory((Integer)param.getValue());
            return new CategoryEditState(manager,this,category);
        } catch (MasterListException e) {
            return new CategoryEditState(manager,this,new CategoryBean());
        }
    }

    /**
     * Get the currently selected category.
     * @return <code>null</code> since a single category cannot be selected in
     * this state.
     **/
    public CategoryBean getCategory() { return category; }

    /**
     * Get the root node of the category tree.
     * @param treeState The container for the expansion state of the nodes in
     * the category tree.
     * @return The root node of the category tree.
     * @throws MasterListException if there is a problem generating the root
     * node.
     **/
    public TreeNode getCategoryTreeRoot(TreeState treeState) throws 
            MasterListException {
        TreeNode root =  new ProjectCategoryTreeRootBean(treeState);
        
        if (getCategory() != null) {
            try {
                List<TreeNode> list = root.getChildren();
                while (!list.isEmpty()) {
                    TreeNode node = list.remove(0);
                    if (node.getId().equals(getCategory().getId())) {
                        TreeNode parent = node.getParent();
                        while (parent != null) {
                            parent.setExpanded(true);
                            parent = parent.getParent();
                        }
                    } else {
                        list.addAll(node.getChildren());
                    }
                }                
            } catch (TreeException e) {
                throw new MasterListException(e.getMessage());
            }
        }
        
        return root;
    }

    /**
     * Get the currently selected data set in the state.
     * @return <code>null</code> since a data set cannot be selected.
     **/
    public DatasetBean getDataset() { return null; }

    /**
     * Get the root node that generates the dataset list.
     * @param treeState The container for the expansion states for the nodes in
     * the dataset list.
     * @return <code>null</code> since a data set list cannot be generated from
     * this state.
     * @throws MasterListException if there is a problem generating the root
     * node.
     **/
    public TreeNode getDatasetListRoot(TreeState treeState) throws 
            MasterListException {
        return null;
    }

    /**
     * Get the currently selected project.
     * @return <code>null</code> since a project cannot be selected in this
     * state.
     **/
    public ProjectBean getProject() { return null; }

    /**
     * Get the selector used by this state.
     * @return <code>null</code> since a selector is not used by this state.
     **/
    public Selector getSelector() { return null; }

    /**
     * Hide/unhide a category in the database.
     * @param evt The event that triggered the hide status change.
     * @return The current state since hide is not allowed in this state.
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
     * @return <code>false</code>
     **/
    public boolean isEditState() { return false; }

    /**
     * Determine if this state is a state that lists entries from the database.
     * @return <code>true</code>
     **/
    public boolean isListState() { return true; }

    /**
     * Change the state to view a list of categories.
     * @param evt The event that triggered the change of state.
     * @return The current state since it is a state that displays a list of
     * categories.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewCategoryList(ActionEvent evt) throws 
            MasterListException {
        return this;
    }

    /**
     * Change the state to view a list of data sets.
     * @param evt The event that triggered the change of state.
     * @return The current state since a data set list state is not accessible
     * from this state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewDatasetList(ActionEvent evt) throws 
            MasterListException {
        return this;
    }

    /**
     * Change the state to view a list of data sets for the specified project.
     * @param projectId The id of the project the list is to display for.
     * @return The current state since a data set list state is not accessible
     * from this state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewDatasetList(String projectId) throws 
            MasterListException {
        return this;
    }

    /**
     * Change the state to view a list of projects.
     * @param evt The event that triggered the state change.
     * @return A state that displays a list of projects.
     * @throws MasterListException if there is a problem changing to a project
     * list state.
     **/
    public MasterListState viewProjectList(ActionEvent evt) throws 
            MasterListException {
        return new ProjectListState(manager);
    }
}