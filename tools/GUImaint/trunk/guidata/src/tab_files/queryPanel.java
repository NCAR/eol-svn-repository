package tab_files;

import java.awt.*;
import javax.swing.*;
import com.borland.jbcl.layout.*;
import com.borland.dbswing.*;
import javax.swing.border.*;

/**
 * <p>Title: GUImaint for CODIAC</p>
 * <p>Description: MySQL db version</p>
 * <p>Copyright: Copyright (c) 2004, 2005, 2006, 2007</p>
 * <p>Company: NCAR/EOL</p>
 * @author Don Stott
 * @version 2.5- test
 */

public class queryPanel extends JPanel {
  JPanel jPanel2 = new JPanel();
  DataModule1 dataModule11;
  static Color tabPurple = new Color(240, 234, 242);
  static Color thisTabColor = tabPurple;
  BorderLayout borderLayout1 = new BorderLayout();
  XYLayout xYLayout1 = new XYLayout();
  JPanel linksPanel = new JPanel();
  XYLayout xYLayout2 = new XYLayout();
  TableScrollPane tableScrollPane1 = new TableScrollPane();
  JdbTable linksTable = new JdbTable();
  static JdbTextField idTextField = new JdbTextField();
  TitledBorder titledBorder2;
  TitledBorder titledBorder3;
  JPanel refPanel = new JPanel();
  JLabel dsRefsLabel3 = new JLabel();
  JdbTable dsRefsTable = new JdbTable();
  JLabel dsRefsLabel1 = new JLabel();
  JLabel dsRefsLabel2 = new JLabel();
  JdbComboBox refTypeComboBox = new JdbComboBox();
  TableScrollPane dsRefsScrollPane = new TableScrollPane();
  XYLayout xYLayout3 = new XYLayout();
  JLabel helpLabel = new JLabel();
  JLabel helpLabel1 = new JLabel();

  public queryPanel() {
    try {
      jbInit();
    }
    catch(Exception ex) {
      ex.printStackTrace();
    }
  }
  void jbInit() throws Exception {
    dataModule11 = tab_files.DataModule1.getDataModule();
    titledBorder2 = new TitledBorder(BorderFactory.createEtchedBorder(Color.white,new Color(156, 161, 161)),"Projects Referenced");
    titledBorder3 = new TitledBorder(BorderFactory.createEtchedBorder(Color.white,new Color(156, 161, 161)),"Datasets Referenced");
    jPanel2.setBackground(thisTabColor);
    jPanel2.setBorder(BorderFactory.createLoweredBevelBorder());
    jPanel2.setLayout(xYLayout1);
    this.setLayout(borderLayout1);
    linksPanel.setBackground(new Color(240, 234, 242));
    linksPanel.setAlignmentY((float) 0.5);
    linksPanel.setBorder(titledBorder2);
    linksPanel.setOpaque(true);
    linksPanel.setLayout(xYLayout2);
    linksTable.setHiddenColumns(new int[] { 2 });
    linksTable.setBackground(new Color(250, 250, 242));
    linksTable.setBorder(BorderFactory.createEtchedBorder());
    linksTable.setToolTipText("Use ID box on left to add row");
    linksTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
    linksTable.setDataSet(dataModule11.getQueryDatasetProj());
    linksTable.setSmartColumnWidths(false);
    idTextField.setDataSet(dataModule11.getQueryDatasetProj());
    idTextField.setText("");
    idTextField.setBackground(new Color(240, 234, 242));
    idTextField.setBorder(BorderFactory.createEtchedBorder());
    idTextField.setToolTipText("Click here and insert Dataset ID, hit Enter");
    idTextField.setColumnName("dataset_id");
    titledBorder2.setTitle("Projects Referenced");
    titledBorder3.setTitle("Datasets Referenced");
    tableScrollPane1.getViewport().setBackground(new Color(240, 234, 242));
    refPanel.setLayout(xYLayout3);
    refPanel.setOpaque(true);
    refPanel.setAlignmentY((float) 0.5);
    refPanel.setBorder(titledBorder3);
    refPanel.setBackground(new Color(223, 231, 231));
    dsRefsLabel3.setText("Reference Type:");
    dsRefsLabel3.setHorizontalAlignment(SwingConstants.RIGHT);
    dsRefsLabel3.setFont(new java.awt.Font("SansSerif", 0, 11));
    dsRefsTable.setToolTipText("Other datasets this one references. Pick reference type from dropdown " +
    "box to add a new row.");
    dsRefsTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
    dsRefsTable.setRowSelectionAllowed(false);
    dsRefsTable.setColumnHeaderVisible(false);
    dsRefsTable.setDataSet(dataModule11.getQueryDSReferences());
    dsRefsTable.setHiddenColumns(new int[] { 0,2,3 });
    dsRefsTable.setEditable(true);
    dsRefsLabel1.setText("Other Datasets this  ");
    dsRefsLabel1.setHorizontalAlignment(SwingConstants.RIGHT);
    dsRefsLabel1.setHorizontalTextPosition(SwingConstants.RIGHT);
    dsRefsLabel1.setFont(new java.awt.Font("SansSerif", 0, 11));
    dsRefsLabel2.setFont(new java.awt.Font("SansSerif", 0, 11));
    dsRefsLabel2.setHorizontalAlignment(SwingConstants.RIGHT);
    dsRefsLabel2.setHorizontalTextPosition(SwingConstants.RIGHT);
    dsRefsLabel2.setText("Dataset References:");
    refTypeComboBox.setBackground(Color.white);
    refTypeComboBox.setFont(new java.awt.Font("SansSerif", 0, 11));
    refTypeComboBox.setToolTipText("Choose from companion or superceded. Click on dropdown box to add " +
    "a row.");
    refTypeComboBox.setColumnName("reference_type");
    refTypeComboBox.setDataSet(dataModule11.getQueryDSReferences());
    refTypeComboBox.setItems(new String[] {"companion", "superceded_by"});
    dsRefsScrollPane.getViewport().setBackground(Color.white);
    dsRefsScrollPane.setFont(new java.awt.Font("SansSerif", 0, 11));
    dsRefsScrollPane.setOpaque(false);
    helpLabel.setText("(To add a row, choose)");
    helpLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    helpLabel.setHorizontalTextPosition(SwingConstants.RIGHT);
    helpLabel.setFont(new java.awt.Font("SansSerif", 0, 11));
    helpLabel1.setFont(new java.awt.Font("SansSerif", 0, 11));
    helpLabel1.setHorizontalAlignment(SwingConstants.RIGHT);
    helpLabel1.setHorizontalTextPosition(SwingConstants.RIGHT);
    helpLabel1.setText("(a Reference Type, first)");
    this.add(jPanel2,  BorderLayout.NORTH);
    jPanel2.add(linksPanel,     new XYConstraints(11, 9, 704, 307));
    linksPanel.add(tableScrollPane1,    new XYConstraints(219, 9, 271, 244));
    linksPanel.add(idTextField,             new XYConstraints(35, 15, 101, -1));
    this.add(refPanel, BorderLayout.CENTER);
//    dsRefsScrollPane.add(dsRefsTable, null);
//    dsRefsScrollPane.getViewport();
    tableScrollPane1.getViewport().add(linksTable, null);
    refPanel.add(dsRefsLabel2,   new XYConstraints(45, 39, 176, 21));
    refPanel.add(helpLabel,   new XYConstraints(45, 110, 176, 21));
    refPanel.add(helpLabel1,   new XYConstraints(45, 128, 176, 21));
    refPanel.add(dsRefsLabel3,   new XYConstraints(355, 26, 108, 20));
    refPanel.add(refTypeComboBox,          new XYConstraints(468, 26, 125, -1));
    refPanel.add(dsRefsLabel1,      new XYConstraints(45, 21, 176, 21));
    refPanel.add(dsRefsScrollPane,        new XYConstraints(234, 24, 129, 132));
    dsRefsScrollPane.getViewport().add(dsRefsTable, null);
//    summaryScrollPane.getViewport().add(summaryTextArea, null);
  }

  public static Color getTabColor() {
    return thisTabColor;
  }
}

//---------------------------------------------------------------
// Use getValueAt() with table to set idTextField visible
// if there is no value in the first row, first column.
// Otherwise, the idTextField should not be visible
//---------------------------------------------------------------
