package tab_files;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import com.borland.dbswing.*;
//import javax.swing.text.html.HTMLFrameHyperlinkEvent;
import javax.swing.text.html.*;

/**
 * <p>Title: GUImaint for CODIAC</p>
 * <p>Description: MySQL db version</p>
 * <p>Copyright: Copyright (c) 2004, 2005, 2006, 2007</p>
 * <p>Company: NCAR/EOL</p>
 * @author Don Stott
 * @version 2.5- test
 */

public class NotesDialog extends JDialog {
  JPanel panel1 = new JPanel();
  BorderLayout borderLayout1 = new BorderLayout();
  TableScrollPane tableScrollPane1 = new TableScrollPane();
  JEditorPane notesPane = new JEditorPane();

  public NotesDialog(Frame parent) {
//    super(parent);    // commented out so parent frame can be brought forward
    try {
      jbInit();
      pack();
    }
    catch (Exception ex) {
      ex.printStackTrace();
    }
  }

  private void jbInit() throws Exception {
    this.setTitle("Notes");
    this.setModal(false);
    setResizable(true);
    panel1.setLayout(borderLayout1);
    notesPane.setMinimumSize(new Dimension(627, 400));
    notesPane.setPreferredSize(new Dimension(900, 700));
    notesPane.setRequestFocusEnabled(false);
    notesPane.setVerifyInputWhenFocusTarget(false);
    notesPane.setEditable(false);
    notesPane.setText("notesPane");
    notesPane.setContentType("text/html");
    notesPane.setPage(
        "http://www.eol.ucar.edu/projects/guimaint/GUImaint_Help_Notes/index.htm");
//  from tsunami it is at: /net/www/docs/projects/guimaint (this one is what the URL goes to)
    notesPane.addHyperlinkListener(new HyperlinkListener() {
      public void hyperlinkUpdate(HyperlinkEvent event) {
        if (event.getEventType()
            == HyperlinkEvent.EventType.ACTIVATED) {
          if (event instanceof HTMLFrameHyperlinkEvent) {
            HTMLFrameHyperlinkEvent evt = (HTMLFrameHyperlinkEvent) event;
            HTMLDocument doc = (HTMLDocument) notesPane.getDocument();
            doc.processHTMLFrameHyperlinkEvent(evt);
          }
          else {
            try {
              notesPane.setPage(event.getURL());
            }
            catch (IOException e) {
              notesPane.setText("Error: " + e);
            }
          }
        }
      }
    });

    tableScrollPane1.setAutoscrolls(true);
    tableScrollPane1.setPreferredSize(new Dimension(900, 700));
    tableScrollPane1.setRequestFocusEnabled(false);
    getContentPane().add(panel1);
    panel1.add(tableScrollPane1, BorderLayout.NORTH);
    tableScrollPane1.getViewport().add(notesPane, null);

  }
}


