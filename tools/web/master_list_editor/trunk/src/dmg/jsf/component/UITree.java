package dmg.jsf.component;

import dmg.jsf.component.util.ComponentSavedState;
import dmg.jsf.component.util.TreeNodeToggler;
import dmg.jsf.event.TreeChildEvent;
import dmg.jsf.model.TreeModel;
import dmg.jsf.model.TreeNode;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.TreeMap;
import javax.faces.application.FacesMessage;
import javax.faces.component.EditableValueHolder;
import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.component.UIComponentBase;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.FacesEvent;
import javax.faces.event.PhaseId;

/**
 * <p>The UITree is a JSF component for displaying TreeNodes in a tree 
 * strucure.  It is the component behind the TreeTag.  It keeps track of which
 * node is selected in the tree and processes expansion actions through a
 * node toggler.</p>
 *
 * @author Joel Clawson
 */
public class UITree extends UIComponentBase implements NamingContainer {
    
    /** The family of components the UITree is associated with. **/
    public static final String COMPONENT_FAMILY = "dmg.jsf.Tree";
    
    /** The type of component the UITree is. **/
    public static final String COMPONENT_TYPE = "dmg.jsf.Tree";
    
    private List<String> depthClassList;
    private Map<String,ComponentSavedState> saved;
    private Object value;
    private String nodeId,rootClass,var,varNodeToggler;
    private TreeModel treeModel;
    private TreeNodeToggler nodeToggler;
    
    /**
     * Create a new UITree.
     **/
    public UITree() {
        setRendererType("dmg.jsf.Tree");
        saved = new TreeMap<String,ComponentSavedState>();
    }
    
    /**
     * Announce that an event has occurred and notify all interested listeners.
     * The function also sets up the backing TreeModel to have the selected
     * node set for any processing.
     * @param evt The event that is to be announced to the listeners.
     **/
    public void broadcast(FacesEvent evt) throws AbortProcessingException {
        if (evt instanceof TreeChildEvent) {
            // Since the event is a TreeChildEvent, the TreeModel needs to 
            // set to the node that generated the event so the processing
            // after the broadcast is done for the specified node.
            TreeChildEvent childEvent = (TreeChildEvent)evt;
            String currentId = getNodeId();
            
            setNodeId(childEvent.getNodeId());
            FacesEvent nodeEvent = childEvent.getFacesEvent();
            nodeEvent.getComponent().broadcast(nodeEvent);
            
            // Make sure to return the UITree to the state it was in before the
            // broadcast of the event.
            setNodeId(currentId);
        } else {
            super.broadcast(evt);
        }
    }
    
    /**
     * Start rendering the current state of the UITree to the response in the
     * FacesContext.
     * @param ctx The context containing the response.
     * @throws IOException when there is an error encoding the UITree.
     **/
    public void encodeBegin(FacesContext ctx) throws IOException {
        treeModel = null;
        if (!keepSaved(ctx)) { saved.clear(); }
        super.encodeBegin(ctx);
    }

    /**
     * Get the identifier that defines the current TreeNode if the current
     * TreeNode is set, or the identifier for the entire UITree component in a
     * JSF page.
     * @param ctx The context containing the UITree.
     **/
    public String getClientId(FacesContext ctx) {
        String treeId = super.getClientId(ctx);
        return nodeId == null ? treeId :
            treeId + NamingContainer.SEPARATOR_CHAR + nodeId;
    }
    
    /**
     * Get the CSS style class for the depth of a TreeNode.
     * @param depth The depth of the node.
     * @return The style class for the specified depth.
     **/
    public String getDepthClass(int depth) {
        return depth == 0 ? getRootClass() :
            depthClassList.get((depth - 1) % depthClassList.size());
    }
    
    /**
     * Get the name of the family the UITree is a member of.
     * @return dmg.jsf.Tree
     **/
    public String getFamily() { return COMPONENT_FAMILY; }
    
    /**
     * Get the currently selected TreeNode.
     * @return The TreeNode that is currently selected in the TreeModel.
     **/
    public TreeNode getNode() {
        return getTreeModel() == null ? null : getTreeModel().getNode();
    }
    
    /**
     * Get the identifier for the currently selected TreeNode.
     * @return The current TreeNode's id.
     **/
    public String getNodeId() { return nodeId; }

    /**
     * Get the object handler that deals with the expansion/contraction of the
     * TreeNodes in the UITree.
     * @return The handler of the TreeNode toggling.
     **/
    private TreeNodeToggler getNodeToggler() {
        if (nodeToggler == null) { nodeToggler = new TreeNodeToggler(this); }
        return nodeToggler;
    }
    
    /**
     * Get the CSS style class for the root node of the UITree.
     * @return The root node's style class.
     **/
    public String getRootClass() {
        return rootClass == null || rootClass.equals("") ? null : rootClass;
    }
    
    /**
     * Get the TreeModel that is backing this UITree.
     * @return The UITree's backing model.
     **/
    public TreeModel getTreeModel() {
        if (treeModel == null) {
            // There isn't a current model, so define one from the value
            // attribute set from the tag.
            Object value = getValue();
            if (value != null) {
                if (value instanceof TreeModel) {
                    treeModel = (TreeModel)value;
                } else if (value instanceof TreeNode) {
                    treeModel = new TreeModel((TreeNode)value);
                }
            }
        }
        return treeModel;
    }
    
    /**
     * Get the object that defines the backing TreeModel of the UITree.  It must
     * be either a TreeModel or a TreeNode.
     * @return The evaluation of the value attribute from the tag in the JSF
     * page.
     **/
    public Object getValue() {
        if (value == null) {
            ValueBinding binding = getValueBinding("value");
            return binding == null ? null : binding.getValue(getFacesContext());
        } else {
            return value;
        }
    }
    
    /**
     * Get the name of the attribute that defines the currently selected
     * TreeNode in the JSF page.
     * @return The var attribute name.
     **/
    public String getVar() { return var; }
    
    /**
     * Get the name of the attribute that defines the TreeNode expansion/
     * contraction handler in the JSF page.
     * @return The node toggler attribute name.
     **/
    public String getVarNodeToggler() { return varNodeToggler; }
    
    /**
     * Determine if any of the context information is worth saving for the
     * next time the UITree is displayed.
     * @param ctx The context the UITree is displayed in.
     * @return <code>true</code> if any part of the UITree or its child
     * TreeNodes has generated any FacesMessages that need to be displayed on
     * the JSF page, <code>false</code> otherwise.
     **/
    private boolean keepSaved(FacesContext ctx) {
        for (Iterator<String> clientIds = 
                saved.keySet().iterator(); clientIds.hasNext(); ) {
            for (Iterator<FacesMessage> messages = 
                    ctx.getMessages(clientIds.next()); messages.hasNext(); ) {
                if (messages.next().getSeverity().compareTo(
                        FacesMessage.SEVERITY_ERROR) >= 0) {
                    return true;
                }
            }
        }
        return false;
    }
    
    /**
     * Perform the processing of the UITree and all of its children for the
     * <i>Apply Request Values</i> phase.
     * @param ctx The context the UITree is in.
     **/
    public void processDecodes(FacesContext ctx) {
        if (isRendered()) {
            // Reset the model
            treeModel = null;
            saved.clear();
            
            // Process all of the child nodes.
            processNodes(ctx,PhaseId.APPLY_REQUEST_VALUES,
                    getTreeModel().getRoot());
            
            // Make sure that after processing, there is not a current node
            setNodeId(null);
            
            // Now decode the remainder of the component as expected
            decode(ctx);
        }
    }
    
    /**
     * Process the specified node for the current phase in the context and all
     * of its children recursively.
     * @param ctx The context the node is in.
     * @param phase The identifier of the phase being processed.
     * @param node The node to be processed along with its children.
     **/
    private void processNodes(FacesContext ctx, PhaseId phase, TreeNode node) {
        // Set up the model and UITree for the current node
        setNodeId(node.getNodeId());
        
        // Determine which facet of the tree is to be processed for the node.
        UIComponent facet = getFacet(node.getFacetName());
        if (facet != null) {
            if (phase == PhaseId.APPLY_REQUEST_VALUES) {
                facet.processDecodes(ctx);
            } else if (phase == PhaseId.PROCESS_VALIDATIONS) {
                facet.processValidators(ctx);
            } else {
                facet.processUpdates(ctx);
            }
        }
        
        // Now process all of the node's children, but only if the nodes is
        // displaying its children.
        if (node.isExpanded()) {
            for (Iterator<TreeNode> kids = 
                    node.getChildren().iterator(); kids.hasNext(); ) {
                processNodes(ctx,phase,kids.next());
            }
        }
    }
    
    /**
     * Perform the processing of the UITree and all of its children for the
     * <i>Update Model Values</i> phase.
     * @param ctx The context the UITree is in.
     **/
    public void processUpdated(FacesContext ctx) {
        if (isRendered()) {
            processNodes(ctx,PhaseId.UPDATE_MODEL_VALUES,
                    getTreeModel().getRoot());
            
            // Make sure that after processing, there is not a current node
            setNodeId(null);
        }
    }

    /**
     * Perform the processing of the UITree and all of its children for the
     * <i>Process Validations</i> phase.
     * @param ctx The context the UITree is in.
     **/
    public void processValidations(FacesContext ctx) {
        if (isRendered()) {
            processNodes(ctx,PhaseId.PROCESS_VALIDATIONS,
                    getTreeModel().getRoot());
            
            // Make sure that after processing, there is not a current node
            setNodeId(null);
        }
    }

    /**
     * Add an event to be broadcasted at the end of the processing lifecycle.
     * It wraps any event generated by the tree in a TreeChildEvent with the
     * identifier of the TreeNode that generated the event.
     * @param evt The event that is being queued for broadcast.
     **/
    public void queueEvent(FacesEvent evt) {
        super.queueEvent(new TreeChildEvent(this,evt,getNodeId()));
    }
    
    /**
     * Restore all of the form values that are nested within the TreeNodes of
     * the UITree since the last request.
     **/
    private void restoreDescendantState() {
        for (Iterator<UIComponent> itr = 
                getFacets().values().iterator(); itr.hasNext(); ) {
            restoreDescendantState(itr.next(),getFacesContext());
        }
    }
    
    /**
     * Recursively restore the the form values in the specified component and
     * all of its child components.
     * @param comp The component to have any form values restored.
     * @param ctx The context used by the form.
     **/
    private void restoreDescendantState(UIComponent comp, FacesContext ctx) {
        // Make sure the the id is actually set properly for the component.
        comp.setId(comp.getId());
        
        // Only need to worry about form components
        if (comp instanceof EditableValueHolder) {
            EditableValueHolder input = (EditableValueHolder)comp;
            ComponentSavedState state = saved.get(comp.getClientId(ctx));
            if (state == null) { state = new ComponentSavedState(); }
            
            // Load the component with the saved values.
            input.setValue(state.getValue());
            input.setSubmittedValue(state.getSubmittedValue());
            input.setValid(state.isValid());
            input.setLocalValueSet(state.isLocalValueSet());
        }
        
        // Now restore all of the component's children
        for (Iterator<UIComponent> kids = 
                comp.getChildren().iterator(); kids.hasNext(); ) {
            restoreDescendantState(kids.next(),ctx);
        }
    }
    
    /**
     * Restore the state of the UITree from its last stored state when the 
     * request has ended.
     * @param ctx The context that contains the UITree.
     * @param state The container of all of the UITree state values.
     **/
    public void restoreState(FacesContext ctx, Object state) {
        Object values[] = (Object[])state;
        super.restoreState(ctx,values[0]);
        value = values[1];
        var = (String)values[2];
        varNodeToggler = (String)values[3];
        depthClassList = (ArrayList<String>)values[4];
        rootClass = (String)values[5];
    }
    
    /**
     * Save all of the form values in the UITree and its children.
     **/
    private void saveDescendantState() {
        for (Iterator<UIComponent> kids =
                getFacets().values().iterator(); kids.hasNext(); ) {
            saveDescendantState(kids.next(),getFacesContext());
        }
    }
    
    /**
     * Save the form values for the specified component and all of its children
     * for the specified context.
     * @param comp The component having its values saved.
     * @param ctx The context of the component.
     **/
    private void saveDescendantState(UIComponent comp, FacesContext ctx) {
        // Only care about components that store values
        if (comp instanceof EditableValueHolder) {
            EditableValueHolder input = (EditableValueHolder)comp;
            ComponentSavedState state = 
                    (ComponentSavedState)saved.get(comp.getClientId(ctx));
            if (state == null) {
                state = new ComponentSavedState();
                saved.put(comp.getClientId(ctx),state);
            }
            
            // Store the component values in the state container
            state.setValue(input.getValue());
            state.setSubmittedValue(input.getSubmittedValue());
            state.setValid(input.isValid());
            state.setLocalValueSet(input.isLocalValueSet());
        }
        
        // Now process all child components recursively
        for (Iterator<UIComponent> kids = (Iterator<UIComponent>)
                comp.getChildren().iterator(); kids.hasNext(); ) {
            saveDescendantState(kids.next(),ctx);
        }
    }

    public String getCOMPONENT_FAMILY() {
        return COMPONENT_FAMILY;
    }
    
    /**
     * Save the UITree component's state so it can be restored on a later 
     * request.
     * @param ctx The context the UITree is in.
     * @return The state of the UITree.
     **/
    public Object saveState(FacesContext ctx) {
        Object values[] = new Object[6];
        values[0] = super.saveState(ctx);
        values[1] = value;
        values[2] = var;
        values[3] = varNodeToggler;
        values[4] = depthClassList;
        values[5] = rootClass;
        return values;
    }
    
    /**
     * Set the list of CSS style classes for the non-root TreeNodes.
     * @param depthClasses A comma separated list of TreeNode depth classes.
     **/
    public void setDepthClasses(String depthClasses) {
        depthClassList = new ArrayList<String>();
        StringTokenizer tokenizer = new StringTokenizer(depthClasses,",");
        while (tokenizer.hasMoreTokens()) {
            depthClassList.add(tokenizer.nextToken());
        }
    }
    
    /**
     * Set the current TreeNode in the component.
     * @param nodeId The identifier for the node to be set.
     **/
    public void setNodeId(String nodeId) {
        // Save the current state of the tree before making any changes.
        saveDescendantState();
        
        // Set up the model for the new node.
        this.nodeId = nodeId;
        TreeModel model = getTreeModel();
        if (model != null) {
            model.setNodeId(nodeId);
            // Restore the tree state from before the node was changed.
            restoreDescendantState();
            
            // Assign variables into the scope so they can be used in the page.
            Map reqMap = getFacesContext().getExternalContext().getRequestMap();
            if (var != null) {
                if (nodeId == null) { reqMap.remove(var); }
                else { reqMap.put(var,getNode()); }
            }
            if (varNodeToggler != null) {
                if (nodeId == null) { reqMap.remove(varNodeToggler); }
                else { reqMap.put(varNodeToggler,getNodeToggler()); }
            }
        }
    }
    
    /**
     * Set the CSS style class to use for the root node of the UITree.
     * @param rootClass The root node's style class.
     **/
    public void setRootClass(String rootClass) { this.rootClass = rootClass; }
    
    /**
     * Set the value of the UITree.  This must be a TreeModel or a TreeNode to
     * use as the root node.
     * @param value The model or root node of the tree.
     **/
    public void setValue(Object value) {
        // Reset the model since a new value is being set.
        treeModel = null;
        this.value = value;
    }
    
    /**
     * Set the value binding for a variable name.  This prevents certain tree
     * variables from being set as value bindings.
     * @param name The name of the attribute being given a binding.
     * @param binding The binding value for the attribute.
     **/
    public void setValueBinding(String name, ValueBinding binding) {
        if ("value".equals(name)) {
            // Reset the model since a new value is being set.
            treeModel = null;
        }
        
        // Prevent the following attributes from getting a value binding set.
        else if ("var".equals(name) || "varNodeToggler".equals(name) || 
                "nodeId".equals(name)) {
            throw new IllegalArgumentException("Attribute ("+name+") is not "+
                    "allowed to have a value binding.");
        }
        
        // Since other values are okay, set the binding
        super.setValueBinding(name,binding);
    }
    
    /**
     * Set the name of the var attribute of the UITree.
     * @param var The name of the var attribute.
     **/
    public void setVar(String var) { this.var = var; }
 
    /**
     * Set the name of the varNodeToggler attribute of the UITree.
     * @param varNodeToggler The name of the toggler attribute.
     **/
    public void setVarNodeToggler(String varNodeToggler) {
        this.varNodeToggler = varNodeToggler;
    }
}