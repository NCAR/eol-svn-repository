package chatscrubber;

import java.awt.Container;
import java.awt.Image;
import java.awt.Toolkit;
import java.io.IOException;
import java.net.URL;
import javax.swing.JOptionPane;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;

/**
 *
 * @author pmartin
 */
public class HelpFrame extends TextScrollPane implements HyperlinkListener{

    //Container location is used to set where the frame is generated on the screeen
    //url is the location of the help document to be displayed
    public HelpFrame(Container location, URL url) {
        super(location);
        //resize frame
        int sizeX = 5 * location.getWidth() / 6;
        int sizeY = 7 * location.getHeight() / 8;

        this.setSize(sizeX, sizeY);

        //center frame on location container
        this.setLocationRelativeTo(location);


        try {
            //set icon
            Image img = Toolkit.getDefaultToolkit().getImage(getClass().getResource("/resources/images/help.png"));
            setIconImage(img);


            this.setTitle("Help for ChatScrubber");

            jEditorPane1.setContentType("text/html");
            jEditorPane1.setFont(new java.awt.Font("Utopia", 0, 13));
            jEditorPane1.addHyperlinkListener(this);


            //set text to help file
            jEditorPane1.setPage(url);
            jEditorPane1.setEditable(false);

        } catch (Exception ex) {
            JOptionPane.showMessageDialog(this.getParent(), ex.getMessage(), "ERROR", JOptionPane.ERROR_MESSAGE);
        }
    }

    public void hyperlinkUpdate(HyperlinkEvent event) {
        if (event.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
            try {
                jEditorPane1.setPage(event.getURL());
            } catch (Exception ex) {
                JOptionPane.showMessageDialog(this.getParent(), "Can't follow link to " + event.getURL().toExternalForm() + ": " + ex, "ERROR", JOptionPane.ERROR_MESSAGE);
            }
        }
    }
}
