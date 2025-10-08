package tab_files;

import java.awt.*;
import javax.swing.*;
import com.borland.jbcl.layout.*;
import com.borland.dbswing.*;
import java.awt.event.*;
import java.beans.*;
import javax.swing.border.*;

/**
 * <p>Title: GUImaint for CODIAC</p>
 * <p>Description: MySQL db version</p>
 * <p>Copyright: Copyright (c) 2004, 2005, 2006, 2007</p>
 * <p>Company: NCAR/EOL</p>
 * @author Don Stott
 * @version 2.5- test
 */

public class CategoryPanel extends JPanel {
  JPanel linksPanel = new JPanel();
  DataModule1 dataModule11;
  static Color tabLtBlue = new Color(220, 241, 250);
  static Color thisTabColor = tabLtBlue;
  XYLayout xYLayout2 = new XYLayout();
  GridBagLayout gridBagLayout1 = new GridBagLayout();
  JPanel platPanel = new JPanel();
  JPanel catPanel = new JPanel();
  XYLayout xYLayout1 = new XYLayout();
  XYLayout xYLayout3 = new XYLayout();
  TableScrollPane catScrollPane = new TableScrollPane();
  JdbTable catTable = new JdbTable();
  TableScrollPane platScrollPane = new TableScrollPane();
  JdbTable platTable = new JdbTable();
  static JdbTextField idTextField = new JdbTextField();
  static JdbTextField idTextField1 = new JdbTextField();
  JdbLabel catLabel2 = new JdbLabel();
  JdbTextField parentCatTextField = new JdbTextField();
  TitledBorder titledBorder1;
  Border border1;
  TitledBorder titledBorder2;
  TitledBorder titledBorder3;
  Border border2;
  TitledBorder titledBorder4;

  public CategoryPanel() {
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
    border1 = BorderFactory.createEtchedBorder(Color.white,new Color(125, 137, 178));
    titledBorder2 = new TitledBorder(border1,"Category:");
    titledBorder3 = new TitledBorder("");
    border2 = BorderFactory.createEtchedBorder(Color.white,new Color(156, 161, 178));
    titledBorder4 = new TitledBorder(border2,"Platform:");
    linksPanel.setBackground(thisTabColor);
    linksPanel.setBorder(BorderFactory.createEtchedBorder());
    linksPanel.setMinimumSize(new Dimension(715, 362));
    linksPanel.setPreferredSize(new Dimension(500, 424));
    linksPanel.setLayout(xYLayout2);
    this.setLayout(gridBagLayout1);
    this.setBackground(thisTabColor);
    this.setPreferredSize(new Dimension(691, 697));
    platPanel.setBackground(new Color(223, 231, 255));
    platPanel.setBorder(titledBorder4);
    platPanel.setOpaque(true);
    platPanel.setLayout(xYLayout1);
    catPanel.setBackground(new Color(180, 197, 255));
    catPanel.setBorder(titledBorder2);
    catPanel.setLayout(xYLayout3);
    catTable.setBackground(new Color(240, 240, 240));
    catTable.setFont(new java.awt.Font("Dialog", 1, 12));
    catTable.setToolTipText("Use dataset  ID box on left to add a new row.");
    catTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
    catTable.setSelectionBackground(new Color(225, 242, 242));
    catTable.setColumnHeaderVisible(true);
    catTable.setDataSet(dataModule11.getQueryDSCategory());
    catTable.setHiddenColumns(new int[] { 2 });
    catScrollPane.getViewport().setBackground(new Color(180, 197, 255));
    catScrollPane.setOpaque(false);
    platScrollPane.getViewport().setBackground(new Color(223, 231, 255));
    platScrollPane.setOpaque(false);
    platTable.setBackground(new Color(240, 240, 240));
    platTable.setFont(new java.awt.Font("Dialog", 1, 12));
    platTable.setToolTipText("Use dataset  ID box on left to add a new row.");
    platTable.setAutoCreateColumnsFromModel(true);
    platTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
    platTable.setSelectionBackground(new Color(225, 242, 242));
    platTable.setColumnHeaderVisible(true);
    platTable.setDataSet(dataModule11.getQueryDSPlatform());
    platTable.setHiddenColumns(new int[] { 2 });
    platTable.setAutoSelection(true);
    idTextField.setBackground(new Color(180, 197, 255));
    idTextField.setBorder(BorderFactory.createEtchedBorder());
    idTextField.setToolTipText("Click here and insert Dataset ID, hit Enter");
    idTextField.setText("");
    idTextField.setColumnName("dataset_id");
    idTextField.setDataSet(dataModule11.getQueryDSCategory());
    idTextField1.setDataSet(dataModule11.getQueryDSPlatform());
    idTextField1.setText("");
    idTextField1.setBackground(new Color(223, 231, 255));
    idTextField1.setBorder(BorderFactory.createEtchedBorder());
    idTextField1.setToolTipText("Click here and insert Dataset ID, hit Enter");
    idTextField1.setColumnName("dataset_id");
    catLabel2.setText("Parent Category:");
    catLabel2.setFont(new java.awt.Font("Dialog", 1, 12));
    parentCatTextField.setDataSet(dataModule11.getQueryCatDS());
    parentCatTextField.setColumnName("parent_category_id");
    parentCatTextField.setText("");
    parentCatTextField.setHorizontalAlignment(SwingConstants.CENTER);
    parentCatTextField.setBorder(BorderFactory.createEtchedBorder());
    parentCatTextField.setToolTipText("Categories can be children of other categories.");
    parentCatTextField.setBackground(new Color(180, 197, 255));
    this.add(linksPanel,                     new GridBagConstraints(0, 0, 1, 1, 1.0, 1.0
            ,GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(22, 4, 75, 4), 220, 154));
    platPanel.add(platScrollPane,    new XYConstraints(157, 12, 410, 144));
    platPanel.add(idTextField1, new XYConstraints(33, 15, 95, -1));
    platScrollPane.getViewport().add(platTable, null);
    linksPanel.add(catPanel,      new XYConstraints(16, 7, 670, 204));
    catPanel.add(catScrollPane,      new XYConstraints(154, 12, 412, 141));
    catPanel.add(parentCatTextField,   new XYConstraints(32, 123, 95, -1));
    catPanel.add(catLabel2,       new XYConstraints(30, 100, 111, 18));
    catPanel.add(idTextField,     new XYConstraints(33, 15, 94, -1));
    linksPanel.add(platPanel,                                                                                                                                                             new XYConstraints(15, 235, 670, 256));
    catScrollPane.getViewport().add(catTable, null);
  }

  public static Color getTabColor() {
    return thisTabColor;
  }

}




