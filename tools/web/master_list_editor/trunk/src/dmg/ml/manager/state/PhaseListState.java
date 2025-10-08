package dmg.ml.manager.state;

import dmg.jsf.TreeException;
import dmg.jsf.model.TreeNode;
import dmg.ml.MasterListException;
import dmg.ml.bean.ClassificationBean;
import dmg.ml.bean.ClassificationTypeBean;
import dmg.ml.bean.DatasetBean;
import dmg.ml.bean.PhaseBean;
import dmg.ml.bean.PhaseTreeRootBean;
import dmg.ml.bean.ProjectBean;
import dmg.ml.manager.MasterListManager;
import dmg.ml.manager.Selector;
import java.util.List;
import javax.faces.component.UIParameter;
import javax.faces.event.ActionEvent;

/**
 * <p>The PhaseListState is a PhaseState that is used by a Master List
 * manager when the display is showing a list of Phases.</p>
 * <p>It is a <code>ConcreteState</code> class in the <b>State</b> pattern 
 * defined in <u>Design Patterns</u> book by Gamma, et al.</p>
 *
 * @author Joel Clawson
 */
public class PhaseListState extends PhaseState {

    private PhaseBean phase;
    
    /**
     * Create a new instance of a PhaseListState.
     * @param manager The manager that generated this state.
     **/
    public PhaseListState(MasterListManager manager) { super(manager); }
    
    /**
     * Create a new instance of a PhaseListState.
     * @param manager The manager that generated this state.
     * @param phase The phase displayed in this list.
     **/
    public PhaseListState(MasterListManager manager, PhaseBean phase) {
        this(manager);
        this.phase = phase;
    }

    /**
     * Save all information in this state to the database.  This function has
     * no effect in this state.
     * @param evt The event that triggered the accepting of the page.
     * @return The current state.
     **/
    public MasterListState accept(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Cancel all changes made in the current state.  This function has no
     * effect in this state.
     * @param evt The event that triggered the cancellation.
     * @return The current state.
     **/
    public MasterListState cancel(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Make a copy of the project selected in the current state.  This function
     * is not currently implemented and returns the current state.
     * @param evt The event that triggered the cloning.
     * @return The current state.
     **/
    public MasterListState clone(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Change to a PhaseDeleteState from the current state.
     * @param evt The event that triggered the state change.
     * @return A state that deletes a Phase from the Master List.
     * @throws MasterListException when there is a problem loading the phase
     * from the database.
     **/
    public MasterListState delete(ActionEvent evt) throws MasterListException {
        UIParameter param = findParameter(evt.getComponent(),"phaseId");
        PhaseBean phase = PhaseBean.loadPhase((Integer)param.getValue());
        return new PhaseDeleteState(manager,this,phase);
    }

    /**
     * Change to a PhaseEditState from the current state.
     * @param evt The event that triggered the state change.
     * @return A state that allows for the editting of a Phase.
     * @throws MasterListException if there is a problem loading the phase
     * from the database.
     **/
    public MasterListState edit(ActionEvent evt) throws MasterListException {
        try {
            UIParameter param = findParameter(evt.getComponent(),"phaseId");
            PhaseBean phase = PhaseBean.loadPhase((Integer)param.getValue());
            return new PhaseEditState(manager,this,phase);
        } catch (MasterListException e) {
            return new PhaseEditState(manager,this,new PhaseBean());
        }
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
     * Get the currently selected Phase in the state.
     * @return The current phase.
     **/
    public PhaseBean getPhase() { return phase; }

    /**
     * Get the TreeNode that is the root of the phase list displayed in a JSF
     * page.
     * @param treeState The container for the expansion state of the nodes in 
     * the data set list.
     * @return The root node for a phase list.
     * @throws MasterListException if there is a problem generating the root
     * node for the phase list.
     **/
    public TreeNode getPhaseTreeRoot(TreeState treeState) throws MasterListException {
        TreeNode root = new PhaseTreeRootBean(treeState);
        
        if (getPhase() != null) {
            try {
                List<TreeNode> list = root.getChildren();
                while (!list.isEmpty()) {
                    TreeNode node = list.remove(0);
                    if (node.getId().equals(getPhase().getId())) {
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
     * Get the currently selected Project in the state.
     * @return The current project.
     **/
    public ProjectBean getProject() { return null; }

    /**
     * Get the current Selector in the state.
     * @return <code>null</code> since a selector is not allowed in this state.
     **/
    public Selector getSelector() { return null; }

    /**
     * Hide the phase in the Master List.
     * @param evt The event that caused the hide action.
     * @return The current state. 
     **/
    public MasterListState hide(ActionEvent evt) throws MasterListException {
        UIParameter param = findParameter(evt.getComponent(),"phaseId");
        phase = PhaseBean.loadPhase((Integer)param.getValue());
        phase.updateHideStatus(!phase.isHidden());
        
	ProjectBean project = ProjectBean.loadProject(phase.getProjectId());
	
	regenerateMenus(project);
        makeDatasetList(project);
        
        return this;
    }

    /**
     * Determine if the current state is a state that deletes a phase.
     * @return <code>false</code>
     **/
    public boolean isDeleteState() { return false; }

    /**
     * Determine if the current state is a state that edits a phase.
     * @return <code>false</code>
     **/
    public boolean isEditState() { return false; }

    /**
     * Determine if the current state is a state that lists phase information.
     * @return <code>true</code>
     **/
    public boolean isListState() { return true; }

    /**
     * Change the current state to a state that displays a list of classifications.
     * @param evt The event that caused the state change.
     * @return A state that displays a list of classifications.
     **/
    public MasterListState viewClassificationList(ActionEvent evt) throws MasterListException {
        return new ClassificationListState(manager);
    }

    /**
     * Change the state to view a list of data sets.
     * @param evt The event that triggered the change of state.
     * @return The current state since a data set list state is not accessible
     * from this state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewDatasetList(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Change the state to view a list of data sets for the specified project.
     * @param projectId The id of the project the list is to display for.
     * @return The current state since a data set list state is not accessible
     * from this state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewDatasetList(String projectId) throws MasterListException {
        return this;
    }

    /**
     * Change the state to view a list of phases in the database.
     * @param evt The event that triggered the change of state.
     * @return A current state since it is a phase list.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewPhaseList(ActionEvent evt) throws MasterListException {
        return this;
    }

    /**
     * Change the state to view a list of projects.
     * @param evt The event that triggered the state change.
     * @return A state that displays a list of projects.
     * @throws MasterListException if there is a problem changing to a project
     * list state.
     **/
    public MasterListState viewProjectList(ActionEvent evt) throws MasterListException {
        return new ProjectListState(manager);
    }
}
