package dmg.jsf.render;

import java.io.IOException;
import java.util.Iterator;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.render.Renderer;

/**
 * <p>The BaseRenderer class is an extension of the Renderer class to allow
 * more general functionality to new renderers.</p>
 * <p>This interface is an extraction from the TreeRenderer class described in 
 * the <u>JavaServer Faces</u> (O'Reilly) book by <i>Hans Bergsten</i> to allow
 * other renderers to use common functionality.</p>
 *
 * @author Joel Clawson
 */
public abstract class BaseRenderer extends Renderer {

    /**
     * Encode all of the specified components children recursively.
     * @param ctx The context to render the components in.
     * @param comp The component to be rendered recursively.
     * @throws IOException when there is a problem encoding the component or its
     * children.
     **/
    protected void encodeRecursive(FacesContext ctx, UIComponent comp) throws
            IOException {
        if (comp.isRendered()) {
            comp.encodeBegin(ctx);
            if (comp.getRendersChildren()) {
                comp.encodeChildren(ctx);
            } else {
                for (Iterator<UIComponent> kids =
                        comp.getChildren().iterator(); kids.hasNext(); ) {
                    encodeRecursive(ctx,kids.next());
                }
            }
            comp.encodeEnd(ctx);
        }
    }
}
