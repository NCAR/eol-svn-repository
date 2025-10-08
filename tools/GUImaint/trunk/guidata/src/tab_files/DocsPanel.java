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

public class DocsPanel extends JPanel {
  static Color tabYellow = new Color(250, 235, 188);
  static Color thisTabColor = tabYellow;
  DataModule1 dataModule11;
  TitledBorder titledBorder1;
  TitledBorder titledBorder2;
  JPanel readmePanel = new JPanel();
  XYLayout xYLayout2 = new XYLayout();
  JdbLabel dsIDLabel = new JdbLabel();
  JdbLabel hostLabel = new JdbLabel();
  JdbLabel dirLabel = new JdbLabel();
  JdbLabel fileNameLabel = new JdbLabel();
  JdbLabel beginDateLabel = new JdbLabel();
  JdbLabel endDateLabel = new JdbLabel();
  JdbLabel fileFormatLabel = new JdbLabel();
  JdbLabel sizeLabel = new JdbLabel();
  JdbLabel archiveDateLabel = new JdbLabel();
  JdbCheckBox hideCheckBox = new JdbCheckBox();
  static JdbTextField dsIDTextField = new JdbTextField();
  JdbTextField hostTextField = new JdbTextField();
  JdbTextField dirTextField = new JdbTextField();
  JdbTextField fileNameTextField = new JdbTextField();
  JdbTextField beginDateTextField = new JdbTextField();
  JdbTextField endDateTextField = new JdbTextField();
  JdbTextField sizeTextField = new JdbTextField();
  JdbTextField archiveDateTextField = new JdbTextField();
  JdbStatusLabel jdbStatusLabel1 = new JdbStatusLabel();
  JdbComboBox fileFormatComboBox = new JdbComboBox();
  TableScrollPane docScrollPane = new TableScrollPane();
  JdbTable docTable = new JdbTable();
  ButtonGroup docGroup = new ButtonGroup();
  BorderLayout borderLayout1 = new BorderLayout();
  JdbTextField eventTextField = new JdbTextField();
  JLabel eventLabel = new JLabel();
  JdbRadioButton eulaRadioButton = new JdbRadioButton();
  JdbRadioButton docRadioButton = new JdbRadioButton();
  JLabel typeLabel = new JLabel();

  public DocsPanel() {
    try {
      jbInit();
    }
    catch(Exception ex) {
      ex.printStackTrace();
    }
  }
  void jbInit() throws Exception {
    dataModule11 = tab_files.DataModule1.getDataModule();
    titledBorder1 = new TitledBorder(BorderFactory.createEtchedBorder(Color.white,new Color(148, 145, 140)),"Documentation files");
    titledBorder2 = new TitledBorder("");
    this.setLayout(borderLayout1);
    this.setBackground(thisTabColor);
    this.setEnabled(true);
    readmePanel.setBackground(thisTabColor);
    readmePanel.setBorder(titledBorder1);
    readmePanel.setMinimumSize(new Dimension(500, 375));
    readmePanel.setPreferredSize(new Dimension(669, 375));
    readmePanel.setLayout(xYLayout2);
    dsIDLabel.setText("Dataset ID:");
    hostLabel.setText("Host:");
    dirLabel.setText("Directory:");
    fileNameLabel.setText("Filename:");
    beginDateLabel.setText("Begin Date:");
    endDateLabel.setText("End Date:");
    fileFormatLabel.setText("File Format:");
    sizeLabel.setText("Size in KB:");
    archiveDateLabel.setText("Archive Date:");
    hideCheckBox.setBackground(thisTabColor);
    hideCheckBox.setText("hide");
    hideCheckBox.setColumnName("hide");
    hideCheckBox.setDataSet(dataModule11.getQueryReadme());
    hideCheckBox.setUnknownDataValueMode(DBDataBinder.CLEAR_VALUE);
    dsIDTextField.setMinimumSize(new Dimension(11, 20));
    dsIDTextField.setPreferredSize(new Dimension(11, 20));
    dsIDTextField.setSelectionEnd(5);
    dsIDTextField.setSelectionStart(1);
    dsIDTextField.setHorizontalAlignment(SwingConstants.CENTER);
    dsIDTextField.setColumnName("dataset_id");
    dsIDTextField.setDataSet(dataModule11.getQueryReadme());
    hostTextField.setDataSet(dataModule11.getQueryReadme());
    hostTextField.setColumnName("host");
    hostTextField.setMinimumSize(new Dimension(11, 20));
    hostTextField.setPreferredSize(new Dimension(11, 20));
    hostTextField.setText("localhost");
    hostTextField.setHorizontalAlignment(SwingConstants.CENTER);
    dirTextField.setDataSet(dataModule11.getQueryReadme());
    dirTextField.setColumnName("directory");
    dirTextField.setMinimumSize(new Dimension(11, 20));
    dirTextField.setPreferredSize(new Dimension(11, 20));
    dirTextField.setToolTipText("Most docs are in /net/archive/data/proj_name/");
    dirTextField.setText("/web/data/projname/docs");
    dirTextField.setHorizontalAlignment(SwingConstants.LEFT);
    fileNameTextField.setDataSet(dataModule11.getQueryReadme());
    fileNameTextField.setColumnName("filename");
    fileNameTextField.setMinimumSize(new Dimension(11, 20));
    fileNameTextField.setPreferredSize(new Dimension(11, 20));
    fileNameTextField.setText("jdbTextField1");
    fileNameTextField.setHorizontalAlignment(SwingConstants.LEFT);
    beginDateTextField.setDataSet(dataModule11.getQueryReadme());
    beginDateTextField.addActionListener(new DocsPanel_beginDateTextField_actionAdapter(this));
    beginDateTextField.setColumnName("begin_date");
    beginDateTextField.setMinimumSize(new Dimension(11, 20));
    beginDateTextField.setPreferredSize(new Dimension(11, 20));
    beginDateTextField.setText("0001-01-01 00:00");
    beginDateTextField.setHorizontalAlignment(SwingConstants.LEFT);
    endDateTextField.setDataSet(dataModule11.getQueryReadme());
    endDateTextField.setColumnName("end_date");
    endDateTextField.setMinimumSize(new Dimension(11, 20));
    endDateTextField.setPreferredSize(new Dimension(11, 20));
    endDateTextField.setText("jdbTextField1");
    endDateTextField.setHorizontalAlignment(SwingConstants.LEFT);
    sizeTextField.setDataSet(dataModule11.getQueryReadme());
    sizeTextField.setColumnName("size_kb");
    sizeTextField.setMinimumSize(new Dimension(11, 20));
    sizeTextField.setPreferredSize(new Dimension(11, 20));
    sizeTextField.setText("jdbTextField1");
    sizeTextField.setHorizontalAlignment(SwingConstants.CENTER);
    archiveDateTextField.setDataSet(dataModule11.getQueryReadme());
    archiveDateTextField.setColumnName("data_archive_date");
    archiveDateTextField.setMinimumSize(new Dimension(11, 20));
    archiveDateTextField.setPreferredSize(new Dimension(11, 20));
    archiveDateTextField.setText("2009-05-24");
    archiveDateTextField.setHorizontalAlignment(SwingConstants.LEFT);
    jdbStatusLabel1.setDataSet(dataModule11.getQueryReadme());
    jdbStatusLabel1.setText("Type Project name in ID textbox then hit Enter");
    jdbStatusLabel1.setPreferredSize(new Dimension(640, 18));
    jdbStatusLabel1.setMaximumSize(new Dimension(700, 18));
    jdbStatusLabel1.setBorder(BorderFactory.createRaisedBevelBorder());
    jdbStatusLabel1.setAutoscrolls(true);
    jdbStatusLabel1.setBackground(thisTabColor);
    jdbStatusLabel1.setAlignmentX((float) 0.5);
    fileFormatComboBox.setBackground(Color.white);
    fileFormatComboBox.setFont(new java.awt.Font("SansSerif", 0, 11));
    fileFormatComboBox.setBorder(null);
    fileFormatComboBox.setPreferredSize(new Dimension(21, 18));
    fileFormatComboBox.setColumnName("format_id");
    fileFormatComboBox.setDataSet(dataModule11.getQueryReadme());
    docTable.setBackground(Color.white);
    docTable.setAutoscrolls(true);
    docTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
    docTable.setColumnHeaderVisible(true);
    docTable.setDataSet(dataModule11.getQueryReadme());
//  Next line is for table in jedi7, only on chinook for now, but with expanded fields for future use
    docTable.setHiddenColumns(new int[] { 0,2,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20 });
//    docTable.setHiddenColumns(new int[] { 0,2,5,6,7,8,9,10,11,12 });
    docScrollPane.getViewport().setBackground(new Color(250, 235, 188));
    docScrollPane.setPreferredSize(new Dimension(455, 205));
    eventTextField.setText("850");
    eventTextField.setPreferredSize(new Dimension(55, 21));
    eventTextField.setDataSet(dataModule11.getQueryFile());
    eventTextField.setColumnName("event");
    eventTextField.setHorizontalAlignment(SwingConstants.CENTER);
    eventLabel.setText("Event Note:");
    eventLabel.setHorizontalAlignment(SwingConstants.LEFT);
    eventLabel.setPreferredSize(new Dimension(110, 21));
    eventLabel.setOpaque(false);
    eventLabel.setText("Event Note:");
    eventTextField.setColumnName("event");
    eventTextField.setDataSet(dataModule11.getQueryReadme());
    eulaRadioButton.setBackground(Color.white);
    eulaRadioButton.setOpaque(false);
    eulaRadioButton.setToolTipText("select for End User License Agreement");
    eulaRadioButton.setSelected(false);
    eulaRadioButton.setText("EULA");
    eulaRadioButton.setButtonGroup(docGroup);
    eulaRadioButton.setColumnName("purpose");
    eulaRadioButton.setDataSet(dataModule11.getQueryReadme());
    eulaRadioButton.setSelectedDataValue("eula");
    docRadioButton.setBackground(Color.white);
    docRadioButton.setOpaque(false);
    docRadioButton.setToolTipText("select for readme");
    docRadioButton.setHorizontalAlignment(SwingConstants.RIGHT);
    docRadioButton.setText("Doc");
    docRadioButton.setButtonGroup(docGroup);
    docRadioButton.setColumnName("purpose");
    docRadioButton.setDataSet(dataModule11.getQueryReadme());
    docRadioButton.setSelected(false);
    docRadioButton.setSelectedDataValue("doc");
    typeLabel.setPreferredSize(new Dimension(110, 21));
    typeLabel.setHorizontalAlignment(SwingConstants.LEFT);
    typeLabel.setText("File type:");
    readmePanel.add(jdbStatusLabel1, new XYConstraints(17, 331, -1, -1));
    readmePanel.add(fileFormatLabel,  new XYConstraints(78, 203, 88, 16));
    readmePanel.add(fileFormatComboBox,    new XYConstraints(202, 201, 360, 21));
    readmePanel.add(sizeLabel,  new XYConstraints(78, 231, 88, 16));
    readmePanel.add(sizeTextField,  new XYConstraints(201, 229, 59, 21));
    readmePanel.add(archiveDateLabel,   new XYConstraints(78, 262, 88, 16));
    readmePanel.add(archiveDateTextField, new XYConstraints(201, 257, 122, 21));
    readmePanel.add(eventLabel,     new XYConstraints(78, 293, 88, 16));
    readmePanel.add(eventTextField,    new XYConstraints(201, 290, 124, -1));
    readmePanel.add(hideCheckBox,                                  new XYConstraints(482, 292, 88, 16));
    readmePanel.add(dsIDTextField,   new XYConstraints(201, 11, 124, 21));
    readmePanel.add(dsIDLabel, new XYConstraints(78, 13, 88, 16));
    readmePanel.add(hostLabel, new XYConstraints(78, 38, 88, 16));
    readmePanel.add(hostTextField,    new XYConstraints(201, 36, 124, 21));
    readmePanel.add(dirLabel, new XYConstraints(78, 67, 88, 16));
    readmePanel.add(dirTextField,   new XYConstraints(201, 65, 358, 21));
    readmePanel.add(fileNameLabel, new XYConstraints(78, 95, 88, 16));
    readmePanel.add(fileNameTextField,    new XYConstraints(201, 93, 359, 21));
    readmePanel.add(beginDateLabel, new XYConstraints(78, 123, 88, 16));
    readmePanel.add(beginDateTextField, new XYConstraints(201, 121, 122, 21));
    readmePanel.add(endDateLabel, new XYConstraints(78, 151, 88, 16));
    readmePanel.add(endDateTextField, new XYConstraints(201, 149, 122, 21));
    readmePanel.add(docRadioButton,  new XYConstraints(178, 175, 65, 20));
    readmePanel.add(eulaRadioButton, new XYConstraints(247, 175, 65, 20));
    readmePanel.add(typeLabel, new XYConstraints(78, 175, 88, -1));
    this.add(docScrollPane, BorderLayout.SOUTH);
    docScrollPane.getViewport().add(docTable, null);
    this.add(readmePanel, BorderLayout.CENTER);
  }

  public static Color getTabColor() {
    return thisTabColor;
  }

  void beginDateTextField_actionPerformed(ActionEvent e) {

  }

}

class DocsPanel_beginDateTextField_actionAdapter implements java.awt.event.ActionListener {
  DocsPanel adaptee;

  DocsPanel_beginDateTextField_actionAdapter(DocsPanel adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.beginDateTextField_actionPerformed(e);
  }
}
