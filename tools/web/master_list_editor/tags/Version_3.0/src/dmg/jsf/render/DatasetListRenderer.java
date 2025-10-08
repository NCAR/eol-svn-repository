package dmg.jsf.render;

import dmg.jsf.component.UITree;
import dmg.jsf.model.TreeNode;
import dmg.ml.bean.ClassificationBean;
import dmg.ml.bean.ClassificationProjectBean;
import dmg.ml.bean.DatasetBean;
import dmg.ml.bean.DatasetProjectBean;
import dmg.ml.bean.PhaseBean;
import java.io.IOException;
import java.util.Iterator;
import javax.faces.FacesException;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

/**
 * <p>The DatasetListRenderer is a specialized TreeRenderer for displaying a 
 * project tree containing both categories and data sets.</p>
 *
 * @author Joel Clawson
 */
public class DatasetListRenderer extends TreeRenderer {
    
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
                    if (node instanceof ClassificationBean) {
                        styleClass += "Category";
                        if (node instanceof ClassificationProjectBean &&
                                ((ClassificationProjectBean)node).isHidden()) {
                            styleClass += "Hidden";
                        }
                    } else if (node instanceof PhaseBean) {
                        styleClass += "Category";
                        if (((PhaseBean)node).isHidden()) {
                            styleClass += "Hidden";
                        }
                    } else if (node instanceof DatasetBean) {
                        styleClass += "Dataset";
                        if (node instanceof DatasetProjectBean &&
                                ((DatasetProjectBean)node).isHidden()) {
                            styleClass += "Hidden";
                        }
                    }
                    
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
}
