package dmg.jsf;

import javax.faces.FacesException;

/**
 * <p>The TreeException is a specialized exception for dealing with problems
 * with a TreeModel or a UITree.</p>
 *
 * @author Joel Clawson
 */
public class TreeException extends FacesException {
    
    /**
     * Creates a new instance of <code>TreeException</code> without detail
     * message.
     */
    public TreeException() {}
    
    
    /**
     * Constructs an instance of <code>TreeException</code> with the specified
     * detail message.
     * @param msg the detail message.
     */
    public TreeException(String msg) { super(msg); }
}
