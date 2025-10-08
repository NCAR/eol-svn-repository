package dmg.jsf.event;

import javax.faces.component.UIComponent;
import javax.faces.event.FacesEvent;
import javax.faces.event.FacesListener;
import javax.faces.event.PhaseId;

/**
 * <p>The TreeChildEvent class is a specialized FacesEvent for UITrees.  It 
 * is a Decorator of a FacesEvent to allow more information to be stored with
 * the event than is allowed by a basic FacesEvent.</p>
 * <p>This class is an adaptation of the ChildEvent class described in the 
 * <u>JavaServer Faces</u> (O'Reilly) book by <i>Hans Bergsten</i>.
 *
 * @author Joel Clawson
 */
public class TreeChildEvent extends FacesEvent {
    
    private FacesEvent event;
    private String nodeId;
    
    /**
     * Create a new TreeChildEvent.
     * @param comp The component that generated the event.
     * @param evt The FacesEvent that was originally generated for this event.
     * @param nodeId The id of the node that caused this event to be generated.
     **/
    public TreeChildEvent(UIComponent comp, FacesEvent evt, String nodeId) {
        super(comp);
        this.event = evt;
        this.nodeId = nodeId;
    }
    
    /**
     * Get the FacesEvent that this TreeChildEvent decorates.
     * @return The event that caused this event to be generated.
     **/
    public FacesEvent getFacesEvent() { return event; }
    
    /**
     * Get the identifier of the node that generated this event.
     * @return The id of the TreeNode that caused the event.
     **/
    public String getNodeId() { return nodeId; }
    
    /**
     * Get the id of the phase when this event was generated.
     * @return The phase when the event occurred.
     **/
    public PhaseId getPhaseId() { return event.getPhaseId(); }
    
    /**
     * Determine if this event is an appropriate listener for the page.
     * @param listener The listener to be tested.
     * @return <code>false</code>
     **/
    public boolean isAppropriateListener(FacesListener listener) {
        return false;
    }
    
    /**
     * Process the specified listener for the event.
     * @param listener The listener to be processed.
     * @throws IllegalStateException every time this is called since the 
     * TreeChildEvent does not allow listeners.
     **/
    public void processListener(FacesListener listener) {
        throw new IllegalStateException("processListener is not allowed for a "+
                "TreeChildEvent.");
    }
    
    /**
     * Set the phase identifier for the event.
     * @param id The phase identifier for the event.
     **/
    public void setPhaseId(PhaseId id) { event.setPhaseId(id); }
}
