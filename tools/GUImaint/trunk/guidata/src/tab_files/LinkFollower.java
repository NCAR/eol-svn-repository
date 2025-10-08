package tab_files;

import javax.swing.*;
import javax.swing.event.*;

/**
 * <p>Title: GUImaint for CODIAC</p>
 * <p>Description: mySQL db version</p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Company: UCAR/JOSS</p>
 * @author Don Stott
 * @version 1.5
 */


public class LinkFollower implements HyperlinkListener {

  private JEditorPane pane;

  public LinkFollower(JEditorPane pane) {
    this.pane = pane;
  }

  public void hyperlinkUpdate(HyperlinkEvent evt) {

    if (evt.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
      try {
        pane.setPage(evt.getURL());
      }
      catch (Exception ex) {
        pane.setText("<html>Could not load " + evt.getURL() + "</html>");
      }
    }

  }

}
