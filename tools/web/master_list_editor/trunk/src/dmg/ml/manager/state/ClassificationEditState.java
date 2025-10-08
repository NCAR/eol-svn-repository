package dmg.ml.manager.state;

import dmg.jsf.model.TreeNode;
import dmg.ml.MasterListException;
import dmg.ml.bean.ClassificationBean;
import dmg.ml.bean.ClassificationTypeBean;
import dmg.ml.bean.DatasetBean;
import dmg.ml.bean.PhaseBean;
import dmg.ml.bean.ProjectBean;
import dmg.ml.manager.MasterListManager;
import dmg.ml.manager.Selectable;
import dmg.ml.manager.Selector;
import dmg.ml.manager.state.MasterListState;
import dmg.ml.manager.state.TreeState;
import java.util.ArrayList;
import java.util.List;
import javax.faces.event.ActionEvent;

/**
 * <p>The ClassificationEditState is a ClassificationState that is used by a Master List
 * manager when the display is showing a form for editting a 
 * <code>ClassificationBean</code> in the database.</p>
 * <p>It is a <code>ConcreteState</code> class in the <b>State</b> pattern 
 * defined in the <u>Design Patterns</u> book by Gamma, et al.</p>
 *
 * @author Joel Clawson
 */
public class ClassificationEditState extends ClassificationState {
    
    private ClassificationBean classification;
    private MasterListState lastState;
    private Selector selector;
    
    /**
     * Create a new instance of a ClassificationEditState.
     * @param manager The manager that generated this state.
     * @param lastState The state of the manager that generated this state.
     * @param classification The classification being edited in this state.
     **/
    public ClassificationEditState(MasterListManager manager, MasterListState lastState,
            ClassificationBean classification) throws MasterListException {
        super(manager);
        this.lastState = lastState;
        this.classification = classification;
        
        List<ClassificationBean> parents = ClassificationBean.getClassifications();
        if (classification != null && classification.getClassificationId() != null) {
            parents.remove(classification);
        }      
        selector = new Selector(parents,ClassificationBean.getClassificationTypeList());
        selector.setIncluded(classification.getParentClassifications());
    }

    /**
     * Accept the changes in the form by inserting or updating the classification as
     * appropriate.
     * @param evt The event that triggered the accept action.
     * @return A list of classification with the database changes made.
     * @throws MasterListException when there is a problem updating the
     * database.
     **/
    public MasterListState accept(ActionEvent evt) throws MasterListException {
        List<ClassificationBean> items = new ArrayList<ClassificationBean>();
        for (Selectable item: selector.getIncludedItems()) {
            items.add((ClassificationBean)item);
        }
        getClassification().setParentClassifications(items);
        if (getClassification().getClassificationId() == null || 
                getClassification().getClassificationId() == 0) {
            getClassification().insert();
        } else {
            getClassification().update();

            // Make the files for all projects associated with the classification.
            for (ProjectBean project: getClassification().getProjects()) {
                regenerateMenus(project);
                makeDatasetList(project);
            }
        }
               
        return new ClassificationListState(manager,getClassification());
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
     * Change to a state that confirms a deletion of a classification.
     * @param evt The event that triggered the delete action.
     * @return The state that allows the classification to be deleted from the
     * database.
     * @throws MasterListException if there is a problem changing to the delete
     * state for the classification.
     **/
    public MasterListState delete(ActionEvent evt) throws MasterListException {
        return new ClassificationDeleteState(manager,this,getClassification());
    }

    /**
     * Change to a state that edits the classification.
     * @param evt The event that triggered the edit action.
     * @return The current state since it is an edit state for the classification.
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
     * Get the classification type that is assigned to this state.
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
     * @return The selector used for choosing the parent classifications.
     **/
    public Selector getSelector() { return selector; }

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
     * Change the state to view the list of classifications in the database.
     * @param evt The event that triggered the change of state.
     * @return The current state since the classification list is unreachable from the
     * current state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewClassificationList(ActionEvent evt) throws 
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
