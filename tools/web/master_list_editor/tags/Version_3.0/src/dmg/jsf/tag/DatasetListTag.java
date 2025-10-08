package dmg.jsf.tag;

/**
 * <p>The DatasetListTag is the Java definition of the <code>datasetList</code>
 * tag in the JSF pages.  It is an extension of the TreeTag class that defines
 * a different renderer for the data in a <code>UITree</code>.</p>
 *
 * @author Joel Clawson
 * @see dmg.jsf.tag.TreeTag
 * @see dmg.jsf.component.UITree
 */
public class DatasetListTag extends TreeTag {

    /**
     * Get the type of renderer used to display the component when this tag
     * is used.
     * @return dmg.jsf.DatasetList
     **/
    public String getRendererType() { return "dmg.jsf.DatasetList"; }
}
