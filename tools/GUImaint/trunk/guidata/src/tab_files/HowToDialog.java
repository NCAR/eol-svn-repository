package tab_files;

import java.awt.*;
import java.io.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.html.*;
import com.borland.dbswing.*;

/**
 * <p>Title: GUImaint for CODIAC</p>
 * <p>Description: MySQL db version</p>
 * <p>Copyright: Copyright (c) 2004, 2005, 2006, 2007</p>
 * <p>Company: NCAR/EOL</p>
 * @author Don Stott
 * @version 2.5- test
 */

public class HowToDialog extends JDialog {
  JPanel panel1 = new JPanel();
  BorderLayout borderLayout1 = new BorderLayout();
  TableScrollPane tableScrollPane1 = new TableScrollPane();
  JEditorPane helpPane = new JEditorPane();

  public HowToDialog(Frame parent) {
//    super(parent);    // commented out so parent frame can be brought forward
    try {
      jbInit();
      pack();
    }
    catch(Exception ex) {
      ex.printStackTrace();
    }
  }

  private void jbInit() throws Exception {
    this.setTitle("Help");
    this.setModal(false);
    setResizable(true);
    panel1.setLayout(borderLayout1);
    helpPane.setMinimumSize(new Dimension(627, 400));
    helpPane.setPreferredSize(new Dimension(900, 700));
    helpPane.setRequestFocusEnabled(false);
    helpPane.setEditable(false);
    helpPane.setContentType("text/html");
    helpPane.setPage("http://www.eol.ucar.edu/projects/guimaint/guimaint_help.htm");
//  from tsunami it is at: /net/www/docs/projects/guimaint (this one is what the URL goes to)
    helpPane.getPage();
    tableScrollPane1.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
    tableScrollPane1.setAutoscrolls(true);
    tableScrollPane1.setPreferredSize(new Dimension(900, 700));
    tableScrollPane1.setRequestFocusEnabled(false);
    panel1.setPreferredSize(new Dimension(900, 700));
    getContentPane().add(panel1);
    panel1.add(tableScrollPane1, BorderLayout.NORTH);
    tableScrollPane1.getViewport().add(helpPane, null);

    helpPane.addHyperlinkListener(new HyperlinkListener() {
      public void hyperlinkUpdate(HyperlinkEvent event) {
        if (event.getEventType()
            == HyperlinkEvent.EventType.ACTIVATED) {
          if (event instanceof HTMLFrameHyperlinkEvent) {
            HTMLFrameHyperlinkEvent evt = (HTMLFrameHyperlinkEvent) event;
            HTMLDocument doc = (HTMLDocument) helpPane.getDocument();
            doc.processHTMLFrameHyperlinkEvent(evt);
          }
          else {
            try {
              helpPane.setPage(event.getURL());
            }
            catch (IOException e) {
              helpPane.setText("Error: " + e);
            }
          }
        }
      }
    });

  }
}