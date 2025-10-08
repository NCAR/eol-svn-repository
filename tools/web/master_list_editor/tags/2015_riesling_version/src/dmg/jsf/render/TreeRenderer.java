package dmg.jsf.render;

import dmg.jsf.component.UITree;
import dmg.jsf.model.TreeNode;
import java.io.IOException;
import java.util.Iterator;
import javax.faces.FacesException;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

/**
 * <p>The TreeRenderer class is the Renderer used by the JSF to generate the 
 * HTML display of a UITree.</p>
 * <p>This interface is an adaptation of the TreeRenderer class described in the 
 * <u>JavaServer Faces</u> (O'Reilly) book by <i>Hans Bergsten</i>.</p>
 *
 * @author Joel Clawson
 * @see dmg.jsf.component.UITree
 */
public class TreeRenderer extends BaseRenderer {
    
    /**
     * Generate the display for all of the children for the specified component.
     * @param ctx The context the children are to be rendered in.
     * @param comp The component to have its children rendered.
     * @throws IOException when there is a problem encoding the children of the
     * component.
     **/
    public void encodeChildren(FacesContext ctx, UIComponent comp) throws
            IOException {
        if (comp.isRendered() && ((UITree)comp).getValue() != null) {
            ResponseWriter out = ctx.getResponseWriter();
       
            encodeNodes(ctx,out,(UITree)comp,
                    ((UITree)comp).getTreeModel().getRoot());
        
            ((UITree)comp).setNodeId(null);
        }
    }
    
    /**
     * Recursively generate the display for the nodes in the tree.
     * @param ctx The context the TreeNodes are to be rendered in.
     * @param out The stream the display is to be written to.
     * @param tree The UITree that contains all of the nodes to be displayed.
     * @param node The current to node to be encoded.
     * @throws IOException when there is a problem generating the display for
     * the node.
     **/
    protected void encodeNodes(FacesContext ctx, ResponseWriter out,
            UITree tree, TreeNode node) throws IOException {
        // Set the node to the current node in the tree.
        tree.setNodeId(node.getNodeId());
        UIComponent facet = tree.getFacet(node.getFacetName());
        
        if (!"".equals(node.getFacetName())) {
            // Handle the case where the facet cannot be found.
            if (facet == null) {
                throw new FacesException("Cannot find required facet: "+
                        node.getFacetName());
            }
            
            // Define the enclosing div element for the node.
            out.startElement("div",tree);
            out.writeAttribute("id",tree.getClientId(ctx),"id");
            
            // Handle the style class for the node
            if (node.getDepth() == 0) {
                if (tree.getRootClass() != null && 
                        !tree.getRootClass().equals("")) {
                    out.writeAttribute("class",tree.getRootClass(),
                            "styleClass");
                }
            } else {
                String styleClass = tree.getDepthClass(node.getDepth());
                if (styleClass != null && !styleClass.equals("")) {
                    out.writeAttribute("class",styleClass,"styleClass");
                }
            }
            
            // Encode the facet itself.
            encodeRecursive(ctx,facet);
            
            // Encode the children of this node if it is to display its
            // children.
            if (node.isExpanded()) {
                for (Iterator<TreeNode> kids =
                        node.getChildren().iterator(); kids.hasNext(); ) {
                    encodeNodes(ctx,out,tree,kids.next());        
                }
            }
            
            out.endElement("div");
            out.write("\n\t");            
        } else {
            for (Iterator<TreeNode> kids =
                    node.getChildren().iterator(); kids.hasNext(); ) {
                encodeNodes(ctx,out,tree,kids.next());
            }
        }
    }
    
    /**
     * Determine if this renderer is suppose to render its child components.
     * @return <code>true</code>
     **/
    public boolean getRendersChildren() { return true; }
}
