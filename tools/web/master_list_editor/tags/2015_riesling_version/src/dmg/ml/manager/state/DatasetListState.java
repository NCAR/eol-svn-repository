package dmg.ml.manager.state;

import dmg.jsf.model.TreeNode;
import dmg.ml.MasterListException;
import dmg.ml.bean.ClassificationBean;
import dmg.ml.bean.ClassificationProjectBean;
import dmg.ml.bean.ClassificationTypeBean;
import dmg.ml.bean.DatasetBean;
import dmg.ml.bean.DatasetProjectBean;
import dmg.ml.bean.PhaseBean;
import dmg.ml.bean.PhaseTreeRootBean;
import dmg.ml.bean.ProjectBean;
import dmg.ml.bean.ProjectClassificationTreeRootBean;
import dmg.ml.bean.ProjectDatasetListRootBean;
import dmg.ml.manager.MasterListManager;
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

    private ClassificationBean classification;
    private ClassificationTypeBean classificationType;
    private DatasetBean dataset;
    private ProjectBean project;
    private PhaseBean phase;

    /**
     * Create a new instance of a DatasetListState.
     * @param manager The manager that generated this state.
     **/
    public DatasetListState(MasterListManager manager) { super(manager); }
    
    /**
     * Create a new instance of a DatasetListState.
     * @param manager The manager that generated this state.
     * @param project The project that the data set list is being displayed for.
     **/
    public DatasetListState(MasterListManager manager, ProjectBean project) {
        super(manager);
        this.project = project;
    }

    /**
     * Create a new instance of a DatasetListState.
     * @param manager The manager that generated this state.
     * @param project The project that the data set list is being displayed for.
     * @param dataset The dataset that was last changed.
     **/
    public DatasetListState(MasterListManager manager, ProjectBean project,
            DatasetBean dataset) {
        super(manager);
        this.project = project;
        this.dataset = dataset;
    }

    /**
     * Create a new instance of a DatasetListState.
     * @param manager The manager that generated this state.
     * @param project The project that the data set list is being displayed for.
     * @param type The type of classificatoin that is beign displayed for the project.
     * @param classification The classification that is being displayed for the project.
     * @param dataset The dataset that was last changed.
     **/
    public DatasetListState(MasterListManager manager, ProjectBean project, ClassificationTypeBean type,
            ClassificationBean classification, DatasetBean dataset) {
        super(manager);
        this.project = project;
        this.classificationType = type;
        this.classification = classification;
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
                (String)findParameter(evt.getComponent(),"datasetId").getValue(),getProject());

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
            dataset = DatasetProjectBean.loadDataset((String)param.getValue(),getProject());
        }
        return new DatasetEditState(manager,this,getProject(),dataset);
    }
    
    /**
     * Get the currently selected classification.
     * @return The selected classification or <code>null</code> if a classification is
     * not selected.
     **/
    public ClassificationBean getClassification() { return classification; }

    /**
     * Get the root node that generates the classification tree.
     * @param treeState The container for the expansion states for the nodes in
     * the classification tree.
     * @return The classification tree's root node.
     * @throws MasterListException if there is a problem generating the root
     * node.
     **/
    public TreeNode getClassificationTreeRoot(TreeState treeState) 
            throws MasterListException {
        return new ProjectClassificationTreeRootBean(treeState,getProject());
    }

    /**
     * Get the classification type selected in this list.
     * @return The type being used to limit this dataset list.
     **/
    public ClassificationTypeBean getClassificationType() { return classificationType; }
    
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
        if (getPhase() == null) {
            return new ProjectDatasetListRootBean(treeState,getProject(),classificationType,classification);
        } else {
            return new ProjectDatasetListRootBean(treeState,getProject(),getPhase());
        }
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
     * Hide/unhide a data set or classification in the data set list.
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
                // Search for a classificationId parameter
                param = findParameter(evt.getComponent(),"classificationId");
            } catch (MasterListException ex) {
                try {
                    // Search for a phaseId paramenter
                    param = findParameter(evt.getComponent(),"phaseId");
                } catch (MasterListException exc) {
                    throw new MasterListException("Cannot find parameter datasetId or classificationId.");
                }
            }
        }

        if (param.getName().equals("datasetId")) {
            // Change the hide state of a data set.
            DatasetProjectBean dataset = DatasetProjectBean.loadDataset(
                    (String)param.getValue(),getProject());
            dataset.updateHideStatus(!dataset.isHidden());
            this.dataset = dataset;
        } else if (param.getName().equals("classificationId")) {
            // Change the hide state of a classification.
            ClassificationProjectBean classification = ClassificationProjectBean.loadClassification((Integer)param.getValue(),getProject());
            classification.updateHideStatus(!classification.isHidden());
            regenerateMenus(getProject());
        } else if (param.getName().equals("phaseId")) {
            // Chnage the hide status of a phase.
            PhaseBean phase = PhaseBean.loadPhase((Integer)param.getValue());
            phase.updateHideStatus(!phase.isHidden());
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
     * Change the state to view the list of classifications in the database.
     * @param evt The event that triggered the change of state.
     * @return A state that views a list of classifications.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewClassificationList(ActionEvent evt)
            throws MasterListException {
        return new ClassificationListState(manager);
    }

    /**
     * Change the state to view a list of data sets.  Limit the list to only 
     * data sets in a classification if the component that generated the event has
     * a parameter with a <code>classificationId</code> defined.
     * @param evt The event that triggered the change of state.
     * @return The current state.
     * @throws MasterListException should never be thrown.
     **/
    public MasterListState viewDatasetList(ActionEvent evt)
            throws MasterListException {
        try {
            UIParameter param = findParameter(evt.getComponent(),"classificationId");
            classification = ClassificationBean.loadClassification((Integer)param.getValue());
        } catch (MasterListException e) {
            classification = null;
        }
        
        try {
            UIParameter param = findParameter(evt.getComponent(),"typeId");
            classificationType = ClassificationTypeBean.loadClassificationType((Integer)param.getValue());
        } catch (MasterListException e) {
            classificationType = null;
        }
        
        try {
            UIParameter param = findParameter(evt.getComponent(),"phaseId");
            if (param.getValue().toString().equals("0")) { 
                phase = new PhaseBean(getProject());
            } else { phase = PhaseBean.loadPhase((Integer)param.getValue()); }
        } catch (MasterListException e) {
            phase = null;
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

    /**
     * Get the currently selected phase in the state.
     * @return The phase that is being used to display the dataset list.
     **/
    public PhaseBean getPhase() { return phase; }

    /**
     * Change the state to view a list of phases in the database.
     * @param evt The event that triggered the change of state.
     * @return A state that displays a list of phases.
     * @throws MasterListException if there is a problem changing to a phase list state.
     **/
    public MasterListState viewPhaseList(ActionEvent evt) throws MasterListException {
        return new PhaseListState(manager);
    }

    /**
     * Get the node that is the root of a phase tree that displays datasets.
     * @param treeState The container for holding the tree expansion states.
     * @return The root of the phase limiting tree list.
     * @throws MasterListException when there is a problem loading the phase tree.
     **/
    public TreeNode getPhaseTreeRoot(TreeState treeState) throws MasterListException {
        if (!PhaseBean.getPhaseList(getProject()).isEmpty()) {
            return new PhaseTreeRootBean(treeState,getProject());
        }
        return null;
    }
}
