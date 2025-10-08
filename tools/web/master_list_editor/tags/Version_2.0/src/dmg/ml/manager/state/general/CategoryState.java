package dmg.ml.manager.state.general;

import dmg.ml.manager.GeneralManager;
import dmg.ml.manager.state.MasterListState;

/**
 * <p>The CategoryState class is an abstract state used by a Master List manager
 * when the primary display is to be for a <code>CategoryBean</code>.
 * <p>It is a <code>ConcreteState</code> class in the <b>State</b> pattern 
 * defined in the <u>Design Patterns</u> book by Gamma, et al.</p>
 *
 * @author Joel Clawson
 */
public abstract class CategoryState extends MasterListState {
    
    /**
     * Create a new instance of a CategoryState.
     * @param manager The manager that generated this state.
     **/
    public CategoryState(GeneralManager manager) { super(manager); }
    
    /**
     * Determine if this state's primary display is for category information.
     * @return <code>true</code> since this is a category state.
     **/
    public boolean isCategoryState() { return true; }

    /**
     * Determine if this state's primary display is for data set information.
     * @return <code>false</code> since this is not a data set state.
     **/
    public boolean isDatasetState() { return false; }

    /**
     * Determine if this state's primary display is for project information.
     * @return <code>false</code> since this is not a project state.
     **/
    public boolean isProjectState() { return false; }
}
