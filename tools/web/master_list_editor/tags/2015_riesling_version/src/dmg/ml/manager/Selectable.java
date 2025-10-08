package dmg.ml.manager;

import dmg.ml.MasterListException;
import java.util.List;

/**
 * The Selectable interface is to be implemented by any item that is to be
 * used in a Selector.
 *
 * @author Joel Clawson
 */
public interface Selectable {

    /**
     * Get the display name of the selectable item.
     * @return the name of the item.
     **/
    public String getName();
    
    /**
     * Get the list of parents of the item.
     * @return The list of the item's parents.
     * @throws MasterListException when there is a problem accessing the
     * item's parents.
     **/
    public List<Selectable> getParents() throws MasterListException;
    
    /**
     * Get the unique id for the item.
     * @return The id of the item.
     **/
    public Integer getSelectableId();
    
    /**
     * Get the name of the type of the item.
     * @return The type of the item.
     **/
    public String getType();
    
    /**
     * Determine if this item has at least one parent.
     * @return <code>true</code> if this item has a parent,
     * <code>false</code> otherwise.
     * @throws MasterListException when there is a problem determining if the item
     * has any parents.
     **/
    public boolean hasParent() throws MasterListException;
}
