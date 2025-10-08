package tab_files;

import java.awt.*;
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

public class XlinkTabPanel extends JPanel {
  BorderLayout borderLayout1 = new BorderLayout();
  static Color tabAqua = new Color(192, 233, 240);
  static Color thisTabColor = tabAqua;
  DataModule1 dataModule11;
  JPanel linkPanel = new JPanel();
  JPanel xlinkPanel = new JPanel();
  TitledBorder titledBorder1;
  TitledBorder titledBorder2;
  XYLayout xYLayout2 = new XYLayout();
  TableScrollPane xlinkScrollPane = new TableScrollPane();
  JdbTable xlinkDSTable = new JdbTable();
  static JdbTextField dsIDTextField = new JdbTextField();
  XYLayout xYLayout1 = new XYLayout();
  TitledBorder titledBorder3;
  Border border1;
  TitledBorder titledBorder4;
  JdbComboBox purposeComboBox1 = new JdbComboBox();
  JdbLabel xlinkHrefLabel = new JdbLabel();
  JdbTextField xlinkHrefTextField = new JdbTextField();
  JdbLabel xlinkTitleLabel = new JdbLabel();
  JdbLabel xlinkIDLabel3 = new JdbLabel();
  JdbTextField xlinkTitleTextField = new JdbTextField();
  JdbCheckBox xlinkHideCheckBox = new JdbCheckBox();
  JdbLabel linkIDLabel = new JdbLabel();
  JdbTextField linkIDTextField = new JdbTextField();
  TableScrollPane xtableScrollPane = new TableScrollPane();
  JdbTable xlinkTable = new JdbTable();
  Border border2;
  TitledBorder titledBorder5;

  public XlinkTabPanel() {
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
    titledBorder2 = new TitledBorder(BorderFactory.createEtchedBorder(Color.white,new Color(134, 163, 168)),"External Links for this Dataset");
    titledBorder3 = new TitledBorder("");
    border1 = BorderFactory.createEtchedBorder(Color.white,new Color(145, 162, 151));
    titledBorder4 = new TitledBorder(BorderFactory.createEtchedBorder(Color.white,new Color(145, 162, 151)),"XLinks");
    border2 = BorderFactory.createEtchedBorder(Color.white,new Color(134, 163, 168));
    titledBorder5 = new TitledBorder(border2,"External Links for this Dataset");
    this.setMinimumSize(new Dimension(800, 700));
    this.setPreferredSize(new Dimension(800, 700));
    this.setLayout(borderLayout1);
    linkPanel.setBackground(thisTabColor);
    linkPanel.setBorder(titledBorder5);
    linkPanel.setMinimumSize(new Dimension(625, 278));
    linkPanel.setPreferredSize(new Dimension(625, 278));
    linkPanel.setLayout(xYLayout2);
    titledBorder1.setTitle("GCMD");
    titledBorder1.setBorder(BorderFactory.createEtchedBorder());
    titledBorder2.setTitle("External Links");
    titledBorder2.setBorder(BorderFactory.createEtchedBorder());
    titledBorder2.setTitleColor(Color.black);
    xlinkDSTable.setToolTipText("External links for this dataset. Use dataset ID box on left to add a new row.");
    xlinkDSTable.setAutoResizeMode(JTable.AUTO_RESIZE_LAST_COLUMN);
    xlinkDSTable.setDataSet(dataModule11.getQueryDatasetXlink());
    xlinkDSTable.setHiddenColumns(new int[] { 2 });
    dsIDTextField.setBackground(thisTabColor);
    dsIDTextField.setToolTipText("Click here and insert Dataset ID, hit Enter");
    dsIDTextField.setText("");
    dsIDTextField.setColumnName("dataset_id");
    dsIDTextField.setDataSet(dataModule11.getQueryDatasetXlink());
    xlinkScrollPane.getViewport().setBackground(thisTabColor);
    xlinkPanel.setBackground(new Color(208, 232, 216));
    xlinkPanel.setBorder(titledBorder4);
    xlinkPanel.setPreferredSize(new Dimension(100, 240));
    xlinkPanel.setLayout(xYLayout1);
    purposeComboBox1.setBackground(Color.white);
    purposeComboBox1.setColumnName("purpose");
    purposeComboBox1.setDataSet(dataModule11.getQueryXlink());
    purposeComboBox1.setItems(new String[] {"info", "map", "homepage", "download", "catalog"});
    purposeComboBox1.setSelectedIndex(0);
    xlinkHrefLabel.setText("External Link HREF:");
    xlinkHrefLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    xlinkHrefTextField.setText("");
    xlinkHrefTextField.setColumnName("href");
    xlinkHrefTextField.setDataSet(dataModule11.getQueryXlink());
    xlinkTitleLabel.setText("Link Title:");
    xlinkTitleLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    xlinkIDLabel3.setText("Link Purpose:");
    xlinkIDLabel3.setHorizontalAlignment(SwingConstants.RIGHT);
    xlinkTitleTextField.setText("");
    xlinkTitleTextField.setColumnName("title");
    xlinkTitleTextField.setDataSet(dataModule11.getQueryXlink());
    xlinkTitleTextField.setSelectionStart(13);
    xlinkHideCheckBox.setBackground(new Color(208, 232, 216));
    xlinkHideCheckBox.setHorizontalAlignment(SwingConstants.LEFT);
    xlinkHideCheckBox.setHorizontalTextPosition(SwingConstants.RIGHT);
    xlinkHideCheckBox.setText("hide");
    xlinkHideCheckBox.setColumnName("hide");
    xlinkHideCheckBox.setDataSet(dataModule11.getQueryXlink());
    linkIDLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    linkIDLabel.setHorizontalTextPosition(SwingConstants.TRAILING);
    linkIDLabel.setText("Link ID:");
    linkIDTextField.setBackground(new Color(221, 214, 214));
    linkIDTextField.setText("");
    linkIDTextField.setColumnName("xlink_id");
    linkIDTextField.setDataSet(dataModule11.getQueryXlink());
    xlinkTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
    xlinkTable.setDataSet(dataModule11.getQueryXlinkName());
    xlinkTable.setHiddenColumns(new int[] { 4, 5 });
    xlinkPanel.add(xlinkHrefTextField,  new XYConstraints(135, 74, 565, -1));
    xlinkPanel.add(xlinkHrefLabel, new XYConstraints(3, 74, 123, 19));
    xlinkPanel.add(linkIDTextField, new XYConstraints(71, 19, 46, -1));
    xlinkPanel.add(linkIDLabel,   new XYConstraints(8, 19, 56, 19));
    xlinkPanel.add(xtableScrollPane,       new XYConstraints(33, 106, 694, 102));
    xlinkPanel.add(xlinkIDLabel3,  new XYConstraints(448, 20, 94, 19));
    xlinkPanel.add(purposeComboBox1, new XYConstraints(547, 20, 94, 20));
    xlinkPanel.add(xlinkHideCheckBox,     new XYConstraints(669, 23, 55, 19));
    xlinkPanel.add(xlinkTitleLabel,   new XYConstraints(124, 20, 78, 19));
    xlinkPanel.add(xlinkTitleTextField, new XYConstraints(208, 20, 234, -1));
    xtableScrollPane.getViewport().add(xlinkTable, null);
    this.add(linkPanel,  BorderLayout.CENTER);
    linkPanel.add(xlinkScrollPane,   new XYConstraints(130, 46, 627, 132));
    linkPanel.add(dsIDTextField,  new XYConstraints(15, 43, 102, 19));
    xlinkScrollPane.getViewport().add(xlinkDSTable, null);
    this.add(xlinkPanel, BorderLayout.NORTH);
  }

  public static Color getTabColor() {
    return thisTabColor;
  }

}

