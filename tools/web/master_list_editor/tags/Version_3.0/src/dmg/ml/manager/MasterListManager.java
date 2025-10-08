package dmg.ml.manager;

import dmg.jsf.TreeException;
import dmg.jsf.model.TreeNode;
import dmg.ml.MasterListException;
import dmg.ml.bean.ClassificationBean;
import dmg.ml.bean.ClassificationTypeBean;
import dmg.ml.bean.DatasetBean;
import dmg.ml.bean.PhaseBean;
import dmg.ml.bean.ProjectBean;
import dmg.ml.manager.state.MasterListState;
import dmg.ml.manager.state.TreeState;
import dmg.ml.manager.state.ProjectListState;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import javax.faces.component.EditableValueHolder;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.ConverterException;
import javax.faces.convert.DateTimeConverter;
import javax.faces.event.ActionEvent;
import javax.faces.model.SelectItem;

/**
 * <p>The MasterListManager is a state container for the general Master List.  It
 * maintains a list of actions that can be performed on any given state and
 * allows the current state to be changed through the actions.</p>
 * <p>It is intended to be used as a session variable in the Java Server Faces
 * implementation of the Master List.</p>
 * <p>This class is a <code>Context</code> class of the <b>State</b> pattern
 * defined in <u>Design Patterns</u> book by Gamma, et al.</p>
 *
 * @author Joel Clawson
 */
public class MasterListManager {
    
    private MasterListState currentState;
    private TreeState classificationTreeState,datasetListState,phaseTreeState;
    
    private List<MasterListException> error;
    
    /**
     * Create a new instance of a MasterListManager.
     **/
    public MasterListManager() {
        currentState = new ProjectListState(this);
        classificationTreeState = new TreeState(false);
        datasetListState = new TreeState(true);
        phaseTreeState = new TreeState(false);
        error = new ArrayList<MasterListException>();
    }
    
    /**
     * Add an error to the list of errors to be displayed to the user.
     * @param e The exception to add to the list.
     **/
    public void appendError(MasterListException e) { error.add(e); }
    
    /**
     * Execute a cancel command on the current state.  This will terminate any
     * processing without saving it.
     * @param evt The event that triggered the cancel action.
     **/
    public void cancel(ActionEvent evt) {
        try {
            currentState = currentState.cancel(evt); 
            // Make sure that the prevous error has been removed after the
            // state change.
            error.clear();
        }
        // Store the exception to be displayed to the user.
        catch (MasterListException e) { error.add(e); }
    }
    
    /**
     * Remove all of the errors currently caught by the manager.
     **/
    public void clearErrors() { error.clear(); }

    /**
     * Execute a confirmation command on the current state.  This will accept
     * any changes the user has made and process them accordingly.
     * @param evt The event that triggered the confirmation action.
     **/
    public void confirm(ActionEvent evt) {
        try {
            currentState = currentState.accept(evt);
            // Make sure that the previous error has been removed after the
            // state change.
            error.clear();
        }
        // Store the exception to be displayed to the user.
        catch (MasterListException e) { error.add(e); }
    }
    
    /**
     * Change from the current state of the Master List to a state that allows
     * for the deletion of an entry from the Master List.
     * @param evt The event that triggered the change to a delete state.
     **/
    public void delete(ActionEvent evt) {
        try {
            currentState = currentState.delete(evt);
            // Make sure that the prevous error has been removed after the
            // state change.
            error.clear();
        }
        // Store the exception to be displayed to the user.
        catch (MasterListException e) { error.add(e); }
    }
    
    /**
     * Change from the current state of the Master List to a state that allows
     * an entry in the Master List to be editted.
     * @param evt The event that triggered the change to an edit state.
     **/
    public void edit(ActionEvent evt) {
        try {
            currentState = currentState.edit(evt);
            // Make sure that the prevous error has been removed after the
            // state change.
            error.clear();
        }
        // Store the exception to be displayed to the user.
        catch (MasterListException e) { error.add(e); }
    }
    
    /**
     * Get the currently selected classification.
     * @return The currently selected classification or <code>null</code> if there is
     * not a selected classification.
     **/
    public ClassificationBean getClassification() { return currentState.getClassification(); }
    
    /**
     * Get the list of SelectItems to display as potential parent classifications for
     * a classification.
     * @return The list of potential parent classifications for a classification as
     * SelectItems.
     **/
    public List<SelectItem> getClassificationTypeList() {
        List<SelectItem> types = new ArrayList<SelectItem>();
        
        try {
            List<ClassificationTypeBean> list = ClassificationTypeBean.getClassificationTypes();
            for (ClassificationTypeBean type: list) {
                types.add(new SelectItem(type.getTypeId().toString(),type.getName()));
            }            
        } catch (MasterListException e) { appendError(e); }
        return types;
    }
    
    /**
     * Get the TreeNode that is to be root node of the classification tree.
     * @return The classification tree root node.
     **/
    public TreeNode getClassificationTreeRoot() {
        try { return currentState.getClassificationTreeRoot(classificationTreeState); }
        // Store the exception to be displayed to the user.
        catch (MasterListException e) { error.add(e); return null; }
    }

    /**
     * Get the currently selected data set.
     * @return The currently selected data set or <code>null</code> if there is
     * not a selected data set.
     **/
    public DatasetBean getDataset() { return currentState.getDataset(); }
    
    /**
     * Get the TreeNode that is to be the root node of the data set list.
     * @return The data set list root node.
     **/
    public TreeNode getDatasetListRoot() throws MasterListException {
        try { return currentState.getDatasetListRoot(datasetListState); }
        // Store the exception to be displayed to the user.
        catch (MasterListException e) { error.add(e); return null; }
    }
    
    /**
     * Get the list of error messages to be displayed to the user.
     * @return The list of error messages in the manager.
     **/
    public List<MasterListException> getErrorMessages() { return error; }

    /**
     * Get the currently selected phase.
     * @return The currently selected phase or <code>null</code> if a pahse
     * has not been selected.
     **/
    public PhaseBean getPhase() { return currentState.getPhase(); }
    
    /**
     * Get the TreeNode that is to be the root node of a phase list.
     * @return the phase list root node.
     **/
    public TreeNode getPhaseTreeRoot() {
        try { return currentState.getPhaseTreeRoot(phaseTreeState); }
        catch (MasterListException e) { error.add(e); return null; }
    }
    
    /**
     * Get the currently selected project.
     * @return The currently selected project or <code>null</code> if there is
     * not a selected project.
     **/
    public ProjectBean getProject() { return currentState.getProject(); }
    
    /**
     * Get a list of all of the projects in the Master List database.
     * @return A list of all projects in the database.
     **/
    public List<ProjectBean> getProjectList() {
        try { return ProjectBean.getProjectList(); }
        // Store the exception to be displayed to the user.
        catch (MasterListException e) { error.add(e); return null; }
    }
    
    /**
     * Get the list of projects from the database as SelectItems for displaying in
     * a dropdown box.
     * @return The list of projects as SelectItems.
     **/
    public List<SelectItem> getProjectSelectList() {
        List<SelectItem> projects = new ArrayList<SelectItem>();
        for (ProjectBean project: getProjectList()) {
            projects.add(new SelectItem(project.getProjectId(),project.getProjectId()));
        }
        return projects;
    }
    
    /**
     * Get the selector being used in the current state for multiple 
     * associations.
     * @return The selector being used in the current state.
     **/
    public Selector getSelector() { return currentState.getSelector(); }
    
    /**
     * Hide an entry in the Master List database.
     * @param evt The event that triggered the hiding of an entry.
     **/
    public void hide(ActionEvent evt) {
        try {
            currentState = currentState.hide(evt);
            // Make sure that the prevous error has been removed after the
            // state change.
            error.clear();
        }
        // Store the exception to be displayed to the user.
        catch (MasterListException e) { error.add(e); }
    }
    
    /**
     * Insert a new entry into the database.
     * @param evt The event that triggered an entry to be added to the database.
     **/
    public void insert(ActionEvent evt) {
        try {
            currentState = currentState.accept(evt);
            // Make sure that the prevous error has been removed after the
            // state change.
            error.clear();
        }
        // Store the exception to be displayed to the user.
        catch (MasterListException e) { error.add(e); }
    }
    
    /**
     * Determine if the current state is a state that displays classification
     * information.
     * @return <code>true</code> if the current state displays classification
     * information, <code>false</code> otherwise.
     **/
    public boolean isClassificationState() { return currentState.isClassificationState(); }
        
    /**
     * Determine if the current state is a state that displays data set
     * information.
     * @return <code>true</code> if the current state displays data set
     * information, <code>false</code> otherwise.
     **/
    public boolean isDatasetState() { return currentState.isDatasetState(); }
    
    /**
     * Determine if the current state is a state that deletes an entry from 
     * the database.
     * @return <code>true</code> if the state deletes an entry, 
     * <code>false</code> otherwise.
     **/
    public boolean isDeleteState() { return currentState.isDeleteState(); }
    
    /**
     * Determine if the current state is a state that edits an entry in the
     * database.
     * @return <code>true</code> if the state edits an entry,
     * <code>false</code> otherwise.
     **/
    public boolean isEditState() { return currentState.isEditState(); }
    
    /**
     * Determine if any errors have been captured by the manager.
     * @return <code>true</code> if an error has been generated, 
     * <code>false</code> otherwise.
     **/
    public boolean isErrorState() { return !error.isEmpty(); }
    
    /**
     * Determine if the current state is a state that displays a list of 
     * database entries.
     * @return <code>true</code> if the state displays a list of entries,
     * <code>false</code> otherwise.
     **/
    public boolean isListState() { return currentState.isListState(); }
    
    /**
     * Determien fi the current state is a state that displays phase information.
     * @return <code>true</code> if the state displays phase information,
     * <code>false</code> otherwise.
     **/
    public boolean isPhaseState() { return currentState.isPhaseState(); }
    
    /**
     * Determine if the current state is a state that displays project
     * information.
     * @return <code>true</code> if the state displays project information,
     * <code>false</code> otherwise.
     **/    
    public boolean isProjectState() { return currentState.isProjectState(); }
        
    /**
     * Set the state of the Master List from a parameter that was passed in 
     * through the URL.
     * @param projectId The id of the project to be displayed.
     **/
    public void setProjectById(String projectId) {
        try {
            currentState = currentState.viewDatasetList(projectId);
            // Make sure that the prevous error has been removed after the
            // state change.
            error.clear();
        }
        // Store the exception to be displayed to the user.
        catch (MasterListException e) { error.add(e); }
    }
    
    /**
     * Update the current entry in the database.
     * @param evt The event that triggered the entry update.
     **/
    public void update(ActionEvent evt) {
        try { 
            currentState = currentState.accept(evt);
            // Make sure that the prevous error has been removed after the
            // state change.
            error.clear();
        }
        // Store the exception to be displayed to the user.
        catch (MasterListException e) { error.add(e); }
    }
    
    /**
     * Change the state to veiw a list of classification.
     * @param evt The event that triggered the change of state.
     **/
    public void viewClassificationList(ActionEvent evt) {
        try {
            currentState = currentState.viewClassificationList(evt);
            // Make sure that the prevous error has been removed after the
            // state change.
            error.clear();
        }
        // Store the exception to be displayed to the user.
        catch (MasterListException e) { error.add(e); }
    }
    
    /**
     * Change the state to view a list of data sets.
     * @param evt The event that triggered the change of state.
     **/
    public void viewDatasetList(ActionEvent evt) {
        try { 
            currentState = currentState.viewDatasetList(evt);
            // Make sure that the prevous error has been removed after the
            // state change.
            error.clear();
        }
        // Store the exception to be displayed to the user.
        catch (MasterListException e) { error.add(e); }
    }

    /**
     * Change the state to view a list of phases.
     * @param evt The event that triggered the chagne of state.
     **/
    public void viewPhaseList(ActionEvent evt) {
        try {
            currentState = currentState.viewPhaseList(evt);
            error.clear();
        } catch (MasterListException e) { error.add(e); }
    }
    
    /**
     * Change the state to view a list of projects.
     * @param evt The event that triggered teh change of state.
     **/
    public void viewProjectList(ActionEvent evt) {
        try {
            currentState = currentState.viewProjectList(evt);
            // Make sure that the prevous error has been removed after the
            // state change.
            error.clear();
        }
        // Store the exception to be displayed to the user.
        catch (MasterListException e) { error.add(e); }
    }    
}
