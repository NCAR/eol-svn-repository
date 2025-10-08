package dmg.jsf.tag;

import dmg.jsf.component.UITree;
import javax.faces.component.UIComponent;
import javax.faces.webapp.UIComponentTag;

/**
 * <p>The TreeTag is the Java definition of the <code>tree</code> tag in the
 * JSF pages.  It accesses all of the attributes of the tag and assigns them
 * to the proper places of the <code>UITree</code> backing component.</p>
 * <p>This class is an adaptation of the TreeTag class described in the 
 * <u>JavaServer Faces</u> (O'Reilly) book by <i>Hans Bergsten</i>.</p>
 *
 * @author Joel Clawson
 *
 * @see dmg.jsf.component.UITree
 */
public class TreeTag extends UIComponentTag {
    
    private String depthClasses,rootClass,value,var,varNodeToggler;
    
    /**
     * Get the type of component represented by this tag.
     * @return dmg.jsf.Tree
     **/
    public String getComponentType() { return "dmg.jsf.Tree"; }
    
    /**
     * Get the type of renderer used to display the component when this tag
     * is used.
     * @return dmg.jsf.Tree
     **/
    public String getRendererType() { return "dmg.jsf.Tree"; }
    
    /**
     * Set the list of CSS classes to be used for the non-root nodes in the
     * component tree.
     * @param depthClasses The non-root depth classes for displaying the tree.
     **/
    public void setDepthClasses(String depthClasses) {
        this.depthClasses = depthClasses;
    }
    
    /**
     * Assign all of the attributes defined in the JSF tag to the specified
     * component.
     * @param component The component being assigned the properties.
     **/
    protected void setProperties(UIComponent component) {
        super.setProperties(component);
        
        if (value != null) {
            component.setValueBinding("value",getFacesContext().
                    getApplication().createValueBinding(value));
        }
        
        UITree tree = (UITree)component;
        if (depthClasses != null) { tree.setDepthClasses(depthClasses); }
        if (rootClass != null) { tree.setRootClass(rootClass); }
        if (var != null) { tree.setVar(var); }
        if (varNodeToggler != null) { tree.setVarNodeToggler(varNodeToggler); }
    }
    
    /**
     * Set the CSS class to be used for displaying the root node of the tree.
     * @param rootClass The root node style class.
     **/
    public void setRootClass(String rootClass) { this.rootClass = rootClass; }
    
    /**
     * Set the root node of the component tree to be displayed.
     * @param value The component tree root node.
     **/
    public void setValue(String value) { this.value = value; }
    
    /**
     * Set the variable name to be used in the component's scope for accessing
     * the current node in the tree.
     * @param var The node variable name.
     **/
    public void setVar(String var) { this.var = var; }
    
    /**
     * Set the variable name for the object that controls the expansion and
     * contraction of nodes in the tree.
     * @param varNodeToggler The expansion toggler variable name.
     **/
    public void setVarNodeToggler(String varNodeToggler) {
        this.varNodeToggler = varNodeToggler;
    }
}
