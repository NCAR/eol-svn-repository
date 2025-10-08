package tab_files;

import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;

import com.borland.dbswing.*;
import com.borland.jbcl.layout.*;
import java.awt.event.*;

/**
 * <p>Title: GUImaint for CODIAC</p>
 * <p>Description: MySQL db version</p>
 * <p>Copyright: Copyright (c) 2004, 2005, 2006, 2007</p>
 * <p>Company: NCAR/EOL</p>
 * @author Don Stott
 * @version 2.5- test
 */

public class FormatPanel extends JPanel {
  JPanel convPanel = new JPanel();
  DataModule1 dataModule11;
  TitledBorder titledBorder1;
  TitledBorder titledBorder2;
  Border border1;
  TitledBorder titledBorder3;
  XYLayout xYLayout2 = new XYLayout();
  JdbComboBox srcFormatComboBox1 = new JdbComboBox();
  JLabel srcFormatLabel1 = new JLabel();
  JLabel expLabel1 = new JLabel();
  JdbComboBox targetFormatComboBox1 = new JdbComboBox();
  JLabel targetFormatLabel1 = new JLabel();
  JLabel commandLabel = new JLabel();
  JdbTextField commandTextField1 = new JdbTextField();
  JdbTextField expTextField1 = new JdbTextField();
  JdbStatusLabel formatStatusLabel = new JdbStatusLabel();
  static Color tabPurple = new Color(240, 234, 242);
  static Color thisTabColor = tabPurple;
  TableScrollPane formatScrollPane = new TableScrollPane();
  JdbTable formatTable = new JdbTable();
  TitledBorder titledBorder4;
  GridBagLayout gridBagLayout1 = new GridBagLayout();
  JdbTextField idTextField = new JdbTextField();

  public FormatPanel() {
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
    titledBorder2 = new TitledBorder("");
    border1 = BorderFactory.createBevelBorder(BevelBorder.LOWERED,Color.white,Color.white,new Color(103, 101, 98),new Color(148, 145, 140));
    titledBorder3 = new TitledBorder(BorderFactory.createEtchedBorder(Color.white,new Color(168, 163, 169)),"Dataset Plots");
    titledBorder4 = new TitledBorder(BorderFactory.createEtchedBorder(Color.white,new Color(168, 163, 169)),"Format Conversions");
    convPanel.setBackground(thisTabColor);
    convPanel.setBorder(titledBorder4);
    convPanel.setOpaque(true);
    convPanel.setPreferredSize(new Dimension(762, 300));
    convPanel.setLayout(xYLayout2);
    this.setLayout(gridBagLayout1);
    srcFormatComboBox1.setDataSet(dataModule11.getQueryFormatConv());
    srcFormatComboBox1.setColumnName("source_format_id");
    srcFormatComboBox1.setBackground(Color.white);
    srcFormatComboBox1.setPreferredSize(new Dimension(224, 20));
    srcFormatLabel1.setHorizontalAlignment(SwingConstants.RIGHT);
    srcFormatLabel1.setText("source_format_id:");
    expLabel1.setHorizontalAlignment(SwingConstants.RIGHT);
    expLabel1.setText("expansion_factor:");
    targetFormatComboBox1.setDataSet(dataModule11.getQueryFormatConv());
    targetFormatComboBox1.setColumnName("target_format_id");
    targetFormatComboBox1.setPreferredSize(new Dimension(81, 20));
    targetFormatComboBox1.setFont(new java.awt.Font("Dialog", 0, 12));
    targetFormatComboBox1.setBackground(Color.white);
    targetFormatLabel1.setHorizontalAlignment(SwingConstants.RIGHT);
    targetFormatLabel1.setText("target_format_id:");
    commandLabel.setText("command:");
    commandLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    commandTextField1.setDataSet(dataModule11.getQueryFormatConv());
    commandTextField1.setText("");
    commandTextField1.setColumnName("command");
    expTextField1.setHorizontalAlignment(SwingConstants.CENTER);
    expTextField1.setColumnName("expansion_factor");
    expTextField1.setDataSet(dataModule11.getQueryFormatConv());
    formatStatusLabel.setBackground(thisTabColor);
    formatStatusLabel.setAlignmentX((float) 0.5);
    formatStatusLabel.setAutoscrolls(true);
    formatStatusLabel.setBorder(BorderFactory.createRaisedBevelBorder());
    formatStatusLabel.setMaximumSize(new Dimension(700, 18));
    formatStatusLabel.setPreferredSize(new Dimension(640, 18));
    formatStatusLabel.setText("Type Project name in ID textbox then hit Enter");
    formatStatusLabel.setDataSet(dataModule11.getQueryFormatConv());
    this.setBackground(thisTabColor);
    formatTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
    formatTable.setDataSet(dataModule11.getQueryFormatConv());
    formatTable.setHiddenColumns(new int[] { 3, 4, 5 });
    formatScrollPane.getViewport().setBackground(thisTabColor);
    formatScrollPane.setPreferredSize(new Dimension(467, 240));
    idTextField.setDataSet(dataModule11.getQueryFormatConv());
    idTextField.setText("");
    idTextField.setBackground(new Color(240, 234, 242));
    idTextField.setBorder(BorderFactory.createEtchedBorder());
    idTextField.setColumnName("dataset_id");
    this.add(convPanel,    new GridBagConstraints(0, 0, 1, 1, 1.0, 1.0
            ,GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), -179, 0));
    convPanel.add(formatStatusLabel,     new XYConstraints(39, 207, -1, -1));
    convPanel.add(srcFormatComboBox1,    new XYConstraints(241, 34, 304, 24));
    convPanel.add(srcFormatLabel1,   new XYConstraints(83, 38, 144, 17));
    convPanel.add(targetFormatComboBox1,  new XYConstraints(241, 68, 304, 24));
    convPanel.add(targetFormatLabel1,  new XYConstraints(83, 72, 144, 17));
    convPanel.add(commandTextField1,  new XYConstraints(240, 137, 304, 24));
    convPanel.add(commandLabel,  new XYConstraints(83, 141, 144, 17));
    convPanel.add(expLabel1,   new XYConstraints(83, 107, 144, 17));
    convPanel.add(expTextField1,  new XYConstraints(241, 103, 49, 24));
    convPanel.add(idTextField,    new XYConstraints(37, 6, 100, -1));
    this.add(formatScrollPane,            new GridBagConstraints(0, 1, 1, 1, 1.0, 1.0
            ,GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 0, 100, 0), 116, 0));
    formatScrollPane.getViewport().add(formatTable, null);
  }

  public static Color getTabColor() {
    return thisTabColor;
  }

}
