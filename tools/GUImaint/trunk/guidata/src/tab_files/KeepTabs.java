package tab_files;

import java.awt.*;
import javax.swing.*;
import com.borland.jbcl.layout.*;
import java.awt.event.*;

/**
 * <p>Title: GUImaint for CODIAC</p>
 * <p>Description: MySQL db version</p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Company: UCAR/JOSS</p>
 * @Author: Don Stott
 * @version 1.5
 */

public class KeepTabs extends JPanel {
//  static Color tabGray = new Color(230, 225, 215);
  static Color tabGreen = new Color(221, 228, 209);
//  static Color tabGreen = new Color(225, 235, 215);
  static Color thisTabColor = tabGreen;
  static JCheckBox keepIDCheckBox = new JCheckBox();
  XYLayout xYLayout1 = new XYLayout();
  JLabel jLabel1 = new JLabel();
  JLabel jLabel2 = new JLabel();
  JLabel jLabel3 = new JLabel();
  JLabel jLabel4 = new JLabel();

  public KeepTabs() {
    try {
      jbInit();
    }
    catch (Exception e) {
      e.printStackTrace();
    }
  }

  private void jbInit() throws Exception {
    keepIDCheckBox.setFont(new java.awt.Font("Dialog", 1, 12));
    keepIDCheckBox.setForeground(Color.black);
    keepIDCheckBox.setMaximumSize(new Dimension(185, 24));
    keepIDCheckBox.setOpaque(false);
    keepIDCheckBox.setPreferredSize(new Dimension(165, 24));
    keepIDCheckBox.setHorizontalAlignment(SwingConstants.CENTER);
    keepIDCheckBox.setHorizontalTextPosition(SwingConstants.RIGHT);
    keepIDCheckBox.setText("Keep Dataset ID in Tabs");
    keepIDCheckBox.addItemListener(new KeepTabs_keepIDCheckBox_itemAdapter(this));
    this.setLayout(xYLayout1);
    this.setBackground(thisTabColor);
    this.setPreferredSize(new Dimension(760, 660));
    jLabel1.setText("");
    jLabel1.setFont(new java.awt.Font("Dialog", 0, 12));
    jLabel1.setText("Check this box and the dataset ID will not change when");
    jLabel2.setFont(new java.awt.Font("Dialog", 0, 12));
    jLabel2.setText("switching tabs, even when the dataset is saved with a new");
    jLabel3.setFont(new java.awt.Font("Dialog", 0, 12));
    jLabel3.setForeground(Color.black);
    jLabel3.setText("ID. This is useful for copying all the tables for a dataset in");
    jLabel4.setText("order to save them with a new dataset ID.");
    jLabel4.setFont(new java.awt.Font("Dialog", 0, 12));
    jLabel4.setHorizontalAlignment(SwingConstants.LEADING);
    this.add(jLabel1, new XYConstraints(70, 125, 359, 15));
    this.add(keepIDCheckBox, new XYConstraints(167, 40, -1, 52));
    this.add(jLabel3, new XYConstraints(70, 160, 359, 15));
    this.add(jLabel2, new XYConstraints(70, 142, 359, 15));
    this.add(jLabel4, new XYConstraints(70, 177, 359, 15));
  }

  public static Color getTabColor() {
    return thisTabColor;
  }

  void keepIDCheckBox_itemStateChanged(ItemEvent e) {
    if (keepIDCheckBox.isSelected()) {
      DatasetTabs.topTabPane.setTitleAt(7, "Tabs keep Dataset ID");
    } else {
      DatasetTabs.topTabPane.setTitleAt(7, "Tabs Change with Dataset");
    }
  }
}

class KeepTabs_keepIDCheckBox_itemAdapter implements java.awt.event.ItemListener {
  KeepTabs adaptee;

  KeepTabs_keepIDCheckBox_itemAdapter(KeepTabs adaptee) {
    this.adaptee = adaptee;
  }
  public void itemStateChanged(ItemEvent e) {
    adaptee.keepIDCheckBox_itemStateChanged(e);
  }
}




