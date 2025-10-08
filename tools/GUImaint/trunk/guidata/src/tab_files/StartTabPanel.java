package tab_files;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

import com.borland.jbcl.layout.*;
import com.borland.dbswing.*;

/**
 * <p>Title: GUImaint for CODIAC</p>
 * <p>Description: MySQL db version</p>
 * <p>Copyright: Copyright (c) 2004, 2005, 2006, 2007</p>
 * <p>Company: NCAR/EOL</p>
 * @author Don Stott
 * @version 2.5- test
 */

public class StartTabPanel extends JPanel {

  Border border1;
  JPanel listPanel = new JPanel();
  VerticalFlowLayout verticalFlowLayout1 = new VerticalFlowLayout();
  DataModule1 dataModule11;
  static short thisUserID;
  static Color tabGray = new Color(230, 225, 215);
  static Color thisTabColor = tabGray;

  // radiobuttons
  JToggleButton loginButton = new JToggleButton();
  VerticalFlowLayout verticalFlowLayout2 = new VerticalFlowLayout();
//  DBPasswordPrompter passwordPrompter = new DBPasswordPrompter();

  public StartTabPanel() {
    try  {
      jbInit();
    }
    catch(Exception ex) {
      ex.printStackTrace();
    }
  }

  private void jbInit() throws Exception {
    dataModule11 = tab_files.DataModule1.getDataModule();
    verticalFlowLayout2.setAlignment(VerticalFlowLayout.MIDDLE);
    border1 = BorderFactory.createEmptyBorder(85,0,0,30);
    this.setBackground(thisTabColor);
    this.setBorder(border1);
    this.setPreferredSize(new Dimension(301, 450));
    listPanel.setBackground(thisTabColor);
    listPanel.setBorder(BorderFactory.createEtchedBorder());
    listPanel.setPreferredSize(new Dimension(325, 105));
    listPanel.setLayout(verticalFlowLayout2);

    loginButton.setBackground(UIManager.getColor("CheckBoxMenuItem.selectionBackground"));
    loginButton.setFont(new java.awt.Font("Dialog", 1, 18));
    loginButton.setRequestFocusEnabled(true);
    loginButton.setToolTipText("Click here to start...");
    loginButton.setVerifyInputWhenFocusTarget(true);
    loginButton.setText("Log in");
    loginButton.addActionListener(new StartTabPanel_loginButton_actionAdapter(this));

//------------------------------------------------------------
//  To make password prompter functional, take comment tags
//    off lines with 'passwordPrompter' in them.
//------------------------------------------------------------
//    passwordPrompter.setFrame(Frame1);
//    passwordPrompter.setDatabase(dataModule11.getDatabase1());
//    passwordPrompter.setTitle("enter password");
//    passwordPrompter.setMaxAttempts(4);
//    passwordPrompter.setPasswordRequired(false);

    this.add(listPanel, null);
    listPanel.add(loginButton, null);
  }

// In case MySQL connections have timed out and closed our databases,
//  reopen them here, as soon as Start button is clicked.
  void loginButton_actionPerformed(ActionEvent e) {
//    dataModule11.queryDataset1.open();
//    dataModule11.queryDSPlatform.open();
//    dataModule11.queryDatasetProj.open();
    String user_name = JOptionPane.showInputDialog("Please enter your CODIAC User ID (no caps):");
    dataModule11.paramRowUser.setString("username", user_name);
    dataModule11.queryUser.executeQuery();
    thisUserID = dataModule11.queryUser.getShort("contact_id");
    if (!user_name.equals(dataModule11.queryUser.getString("contact_short_name"))) {
      JOptionPane.showMessageDialog(null, "ERROR! User ID is not in database. Click Log in and enter again!", "Bad User ID", JOptionPane.ERROR_MESSAGE);
      dataModule11.paramRowUser.setString("username", "notagoodname");
      SideTabbedPages.sideTabPane.setSelectedIndex(0);          // first have to search for dataset_id
    } else {
      SearchTabs.topTabPane.setSelectedIndex(0);
      dataModule11.paramRowSearchID.setString("search_id", "83.013");
      SideTabbedPages.sideTabPane.setSelectedIndex(1);          // first have to search for dataset_id
//      passwordPrompter.setUserName(dataModule11.queryUser.getString("contact_short_name"));
//      passwordPrompter.setPassword(dataModule11.queryUser.getString("email"));
//      if ( ! passwordPrompter.showDialog()) {
//        System.exit(0);
      }
    }
  }

class StartTabPanel_loginButton_actionAdapter implements java.awt.event.ActionListener {
  StartTabPanel adaptee;

  StartTabPanel_loginButton_actionAdapter(StartTabPanel adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.loginButton_actionPerformed(e);
  }
}
