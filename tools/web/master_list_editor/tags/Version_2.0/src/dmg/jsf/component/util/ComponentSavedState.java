package dmg.jsf.component.util;

/**
 * <p>The ComponentSavedState class is a container for holding the values of
 * EditableValueHolder UIComponents between state changes.  It provides a means
 * to ensure that the component is in the same state before and after some set
 * of functionality has occurred.</p>
 * <p>This class is an adaptation of the SavedState class described in the 
 * <u>JavaServer Faces</u> (O'Reilly) book by <i>Hans Bergsten</i>.
 *
 * @author Joel Clawson
 */
public class ComponentSavedState {

    private boolean localValueSet,valid = true;
    private Object submittedValue,value;
    
    /**
     * Get the value that was submitted to the component.
     * @return The submitted value.
     **/
    public Object getSubmittedValue() { return submittedValue; }
    
    /**
     * Get the value that was in the component.
     * @return The component's value.
     **/
    public Object getValue() { return value; }
    
    /**
     * Determine if the local value was set for the component.
     * @return <code>true</code> if the local value was set, <code>false</code>
     * othwerwise.
     **/
    public boolean isLocalValueSet() { return localValueSet; }
    
    /**
     * Determien if the submitted value to the component is valid.
     * @return <code>true</code> if the value is valid, <code>false</code>
     * otherwise.
     **/
    public boolean isValid() { return valid; }
    
    /**
     * Set the state of the assigning of the local value for the component.
     * @param localValueSet The set state of the local value.
     **/
    public void setLocalValueSet(boolean localValueSet) {
        this.localValueSet = localValueSet;
    }
    
    /**
     * Set the submitted value of the component.
     * @param submittedValue The submitted value.
     **/
    public void setSubmittedValue(Object submittedValue) {
        this.submittedValue = submittedValue;
    }
    
    /**
     * Set the flag if the component was valid.
     * @param valid The valid flag.
     **/
    public void setValid(boolean valid) { this.valid = valid; }
    
    /**
     * Set the value that was in the component.
     * @param value The component's value.
     **/
    public void setValue(Object value) { this.value = value; }
}
