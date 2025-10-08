package tab_files;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.html.*;
import com.borland.dbswing.*;

/**
 * <p>Title: GUImaint for CODIAC</p>
 * <p>Description: mySQL db version</p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Company: UCAR/JOSS</p>
 * @author Don Stott
 * @version 1.5
 */

public class pswordDialog extends JDialog {
  JPanel panel1 = new JPanel();
  BorderLayout borderLayout1 = new BorderLayout();
  TableScrollPane tableScrollPane1 = new TableScrollPane();
  JEditorPane helpPane = new JEditorPane();
  JPanel jPanel1 = new JPanel();
  JButton okButton = new JButton();
  static boolean dialogup = false;

  public pswordDialog(Frame frame, String title, boolean modal) {
    super(frame, title, modal);
    if (dialogup == false) {
      try {
        jbInit();
        pack();
      }
      catch (Exception ex) {
        ex.printStackTrace();
      }
    }
  }
  private void jbInit() throws Exception {
    this.setTitle("Using the Password Tab");
    this.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
    this.setModal(false);
    setResizable(true);
    dialogup = true;
    panel1.setLayout(borderLayout1);
    helpPane.setMinimumSize(new Dimension(627, 400));
    helpPane.setPreferredSize(new Dimension(640, 480));
    helpPane.setRequestFocusEnabled(false);
    helpPane.setEditable(false);
    helpPane.setContentType("text/html");
    helpPane.setPage("http://www.eol.ucar.edu/projects/guimaint/pswd_help.htm");
//   actual location is on chandon. To get to from tsunami, cd to /export/web/joss/docs
//   and then cd to /net/www/docs/projects/guimaint
    helpPane.getPage();
    tableScrollPane1.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
    tableScrollPane1.setAutoscrolls(true);
    tableScrollPane1.setPreferredSize(new Dimension(640, 480));
    tableScrollPane1.setRequestFocusEnabled(false);
    panel1.setPreferredSize(new Dimension(640, 480));
    jPanel1.setMinimumSize(new Dimension(10, 10));
    jPanel1.setPreferredSize(new Dimension(60, 40));
    okButton.setText("Close");
    okButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        okButton_actionPerformed(e);
      }
    });
    getContentPane().add(panel1,  BorderLayout.CENTER);
    panel1.add(tableScrollPane1, BorderLayout.NORTH);
    this.getContentPane().add(jPanel1,  BorderLayout.SOUTH);
    jPanel1.add(okButton, null);
    tableScrollPane1.getViewport().add(helpPane, null);

    helpPane.addHyperlinkListener(new HyperlinkListener() {
      public void hyperlinkUpdate(HyperlinkEvent event) {
        if (event.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
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

  public pswordDialog() {
    this(null, "", false);
  }

  protected void processWindowEvent(WindowEvent e) {
    if(e.getID() == WindowEvent.WINDOW_CLOSING) {
//      dialogup = false;
      cancel();
    }
    super.processWindowEvent(e);
  }

  void cancel() {
    dispose();
  }

  void okButton_actionPerformed(ActionEvent e) {
//    dialogup = false;
    cancel();
  }

  public static boolean setDialogup(boolean val) {
    dialogup = val;
    return(true);
  }

  public static boolean getDialogup() {
    return dialogup;
  }
}