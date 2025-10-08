package tab_files;

import java.awt.*;
import javax.swing.*;
import com.borland.jbcl.layout.*;
import com.borland.dbswing.*;
import javax.swing.border.*;
import java.awt.event.*;
import com.borland.dx.dataset.DataSet;

/**
 * <p>Title: GUImaint for CODIAC</p>
 * <p>Description: MySQL db version</p>
 * <p>Copyright: Copyright (c) 2004, 2005, 2006, 2007</p>
 * <p>Company: NCAR/EOL</p>
 * @author Don Stott
 * @version 2.5- test
 */

public class PasswordTabPanel extends JPanel {
  static Color tabPale = new Color(202, 207, 200);
  static Color thisTabColor = tabPale;
  static Color thisBkgrnd = new Color(231, 231, 223);
  XYLayout xYLayout1 = new XYLayout();
  JPanel jPanel1 = new JPanel();
  XYLayout xYLayout2 = new XYLayout();
  JdbLabel passwordLabel = new JdbLabel();
  DataModule1 dataModule11;
  JdbTextField passwordTextField = new JdbTextField();
  JdbTextField usernameTextField = new JdbTextField();
  JdbLabel usernameLabel1 = new JdbLabel();
  JPanel jPanel2 = new JPanel();
  TitledBorder titledBorder1;
  TitledBorder titledBorder2;
  XYLayout xYLayout3 = new XYLayout();
  JdbTextField datasetTextField = new JdbTextField();
  JdbLabel datasetIDLabel = new JdbLabel();
  JdbLabel usernameLabel = new JdbLabel();
  JPanel jPanel3 = new JPanel();
  JdbComboBox loginComboBox = new JdbComboBox();
  JToggleButton editToggleButton = new JToggleButton();
  boolean editon = true;

  public PasswordTabPanel() {
    try {
      jbInit();
    }
    catch(Exception ex) {
      ex.printStackTrace();
    }
  }
  void jbInit() throws Exception {
    dataModule11 = tab_files.DataModule1.getDataModule();
    titledBorder1 = new TitledBorder("");
    titledBorder2 = new TitledBorder(new EtchedBorder(EtchedBorder.RAISED,Color.white,new Color(161, 161, 150)),"Password");
    this.setBackground(new Color(231, 231, 215));
    this.setBorder(null);
    this.addComponentListener(new PasswordTabPanel_this_componentAdapter(this));
    this.setLayout(xYLayout1);
    jPanel1.setBackground(thisBkgrnd);
    jPanel1.setBorder(BorderFactory.createLoweredBevelBorder());
    jPanel1.setLayout(xYLayout2);
    passwordLabel.setFont(new java.awt.Font("Tahoma", 0, 12));
    passwordLabel.setHorizontalAlignment(SwingConstants.TRAILING);
    passwordLabel.setHorizontalTextPosition(SwingConstants.LEFT);
    passwordLabel.setText("Password:");
    passwordTextField.setText("");
    passwordTextField.setEnabled(true);
    passwordTextField.setEditable(false);
    passwordTextField.setText("");
    passwordTextField.setColumnName("password");
    passwordTextField.setDataSet(dataModule11.getQueryPassword());
    passwordTextField.setPostOnFocusLost(true);
    usernameTextField.setDataSet(dataModule11.getQueryPassword());
    usernameTextField.setPostOnFocusLost(true);
    usernameTextField.setColumnName("username");
    usernameTextField.setText("");
    usernameTextField.setEditable(false);
    usernameTextField.setText("");
    usernameLabel1.setText("Login Name:");
    usernameLabel1.setHorizontalTextPosition(SwingConstants.LEFT);
    usernameLabel1.setHorizontalAlignment(SwingConstants.TRAILING);
    usernameLabel1.setFont(new java.awt.Font("Tahoma", 0, 12));
    jPanel2.setBackground(new Color(223, 239, 220));
    jPanel2.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.RAISED,Color.white,new Color(148, 145, 140)),"Login name"));
    jPanel2.setLayout(xYLayout3);
    datasetTextField.setText("");
    datasetTextField.setColumnName("dataset_id");
    datasetTextField.setDataSet(dataModule11.getQueryDSUser());
    datasetIDLabel.setFont(new java.awt.Font("Tahoma", 0, 12));
    datasetIDLabel.setHorizontalAlignment(SwingConstants.TRAILING);
    datasetIDLabel.setHorizontalTextPosition(SwingConstants.LEFT);
    datasetIDLabel.setText("Data set ID:");
    usernameLabel.setFont(new java.awt.Font("Tahoma", 0, 12));
    usernameLabel.setHorizontalAlignment(SwingConstants.TRAILING);
    usernameLabel.setHorizontalTextPosition(SwingConstants.LEFT);
    usernameLabel.setText("Login Name:");
    jPanel3.setBackground(new Color(223, 231, 231));
    jPanel3.setAlignmentY((float) 0.5);
    jPanel3.setBorder(titledBorder2);
    loginComboBox.setBackground(Color.white);
    loginComboBox.setToolTipText("password is linked to Login Name; to change, select new Login name");
    loginComboBox.setColumnName("username");
    loginComboBox.setDataSet(dataModule11.getQueryDSUser());
    loginComboBox.addItemListener(new PasswordTabPanel_loginComboBox_itemAdapter(this));
    editToggleButton.setBackground(thisBkgrnd);
    editToggleButton.setBorder(BorderFactory.createRaisedBevelBorder());
    editToggleButton.setOpaque(true);
    editToggleButton.setToolTipText("Clicking this button will enable editing of Login and Password");
    editToggleButton.setText("Click Twice to Edit Login/Password");
    editToggleButton.addActionListener(new PasswordTabPanel_editToggleButton_actionAdapter(this));
    xYLayout1.setWidth(748);
    xYLayout1.setHeight(684);
    jPanel2.add(usernameLabel,  new XYConstraints(5, 0, 148, 25));
    jPanel2.add(loginComboBox,  new XYConstraints(246, 0, 217, 28));
    jPanel1.add(jPanel2,  new XYConstraints(56, 17, 507, 119));
    jPanel2.add(datasetTextField, new XYConstraints(246, 52, 219, 27));
    jPanel2.add(datasetIDLabel, new XYConstraints(19, 52, 134, 25));
    jPanel1.add(usernameLabel1,  new XYConstraints(56, 184, 148, 25));
    jPanel1.add(passwordLabel, new XYConstraints(54, 225, 148, 25));
    jPanel1.add(usernameTextField, new XYConstraints(297, 184, 219, 27));
    jPanel1.add(passwordTextField,  new XYConstraints(297, 224, 219, 27));
    jPanel1.add(jPanel3, new XYConstraints(58, 165, 504, 124));
    this.add(jPanel1, new XYConstraints(26, 39, 591, 343));
    jPanel1.add(editToggleButton, new XYConstraints(290, 296, 235, 25));
  }

  void loginComboBox_itemStateChanged(ItemEvent e) {
    dataModule11.queryPassword.refresh();
  }

  void editToggleButton_actionPerformed(ActionEvent e) {
    if (editon == false) {
      editToggleButton.setBackground(Color.pink);
      editToggleButton.setText("Editing Enabled for Login/Password");
      usernameTextField.setEditable(true);
      passwordTextField.setEditable(true);
      editon = true;
    }
    else {
//      editToggleButton.setBackground(Color.white);
      editToggleButton.setText("Click to Edit Login/Password");
      usernameTextField.setEditable(false);
      passwordTextField.setEditable(false);
      editon = false;
    }
  }


  void this_componentShown(ComponentEvent e) {
    if (pswordDialog.getDialogup() == false) {
      pswordDialog dlg4 = new pswordDialog();
      Dimension dlgSize = dlg4.getPreferredSize();
      Dimension frmSize = getSize();
      dlg4.show();
    }
  }

}

class PasswordTabPanel_loginComboBox_itemAdapter implements java.awt.event.ItemListener {
  PasswordTabPanel adaptee;

  PasswordTabPanel_loginComboBox_itemAdapter(PasswordTabPanel adaptee) {
    this.adaptee = adaptee;
  }
  public void itemStateChanged(ItemEvent e) {
    adaptee.loginComboBox_itemStateChanged(e);
  }
}


class PasswordTabPanel_editToggleButton_actionAdapter implements java.awt.event.ActionListener {
  PasswordTabPanel adaptee;

  PasswordTabPanel_editToggleButton_actionAdapter(PasswordTabPanel adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.editToggleButton_actionPerformed(e);
  }
}

class PasswordTabPanel_this_componentAdapter extends java.awt.event.ComponentAdapter {
  PasswordTabPanel adaptee;

  PasswordTabPanel_this_componentAdapter(PasswordTabPanel adaptee) {
    this.adaptee = adaptee;
  }
  public void componentShown(ComponentEvent e) {
    adaptee.this_componentShown(e);
  }
}
