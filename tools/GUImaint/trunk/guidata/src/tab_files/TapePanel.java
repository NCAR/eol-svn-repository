package tab_files;

import java.awt.*;
import javax.swing.JPanel;
import com.borland.dbswing.*;
import com.borland.jbcl.layout.*;
import javax.swing.SwingConstants;
import javax.swing.JTable;

/**
 * <p>Title: GUImaint for CODIAC</p>
 * <p>Description: MySQL db version</p>
 * <p>Copyright: Copyright (c) 2004, 2005, 2006, 2007</p>
 * <p>Company: NCAR/EOL</p>
 * @author Don Stott
 * @version 2.5- test
 */

public class TapePanel extends JPanel {
  static Color tabGenta = new Color(220, 195, 200);
  static Color tabBlue = new Color(235, 235, 255);
  static Color tabYellow = new Color(255, 252, 213);
  static Color thisTabColor = tabYellow;
  DataModule1 dataModule11;
  JdbLabel tapeIDLabel = new JdbLabel();
  XYLayout xYLayout1 = new XYLayout();
  JdbLabel datasetIDLabel = new JdbLabel();
  JdbLabel beginDateLabel = new JdbLabel();
  JdbLabel endDateLabel = new JdbLabel();
  JdbLabel formatLabel = new JdbLabel();
  JdbLabel sizeLabel = new JdbLabel();
  JdbLabel numFilesLabel = new JdbLabel();
  JdbLabel seqNumLabel = new JdbLabel();
  JdbLabel primaryLabel = new JdbLabel();
  JdbLabel backupLabel = new JdbLabel();
  JdbLabel tapeIDLabel8 = new JdbLabel();
  JdbLabel primMedLabel = new JdbLabel();
  JdbLabel archiveDateLabel = new JdbLabel();
  TableScrollPane tapeScrollPane = new TableScrollPane();
  JdbTable tapeTable = new JdbTable();
  JdbTextField tapeIDTextField = new JdbTextField();
  JdbTextField dsIDTextField = new JdbTextField();
  JdbTextField seqNumTextField = new JdbTextField();
  JdbTextField numFilesTextField = new JdbTextField();
  JdbTextField sizeTextField = new JdbTextField();
  JdbTextField archiveDateTextField = new JdbTextField();
  JdbCheckBox jdbCheckBox1 = new JdbCheckBox();
  JdbTextField beginDateTextField = new JdbTextField();
  JdbTextField endDateTextField = new JdbTextField();
  JdbTextField primLabelTextField = new JdbTextField();
  JdbTextField backupLabelTextField = new JdbTextField();
  JdbComboBox formatComboBox = new JdbComboBox();
  JdbComboBox primMedComboBox = new JdbComboBox();
  JdbComboBox backupMedComboBox = new JdbComboBox();

  public TapePanel() {
    try {
      jbInit();
    }
    catch(Exception ex) {
      ex.printStackTrace();
    }
  }
  void jbInit() throws Exception {
    dataModule11 = tab_files.DataModule1.getDataModule();
    tapeIDLabel.setHorizontalAlignment(SwingConstants.TRAILING);
    tapeIDLabel.setText("Tape ID:");
    this.setBackground(thisTabColor);
    this.setLayout(xYLayout1);
    datasetIDLabel.setText("Dataset ID:");
    datasetIDLabel.setHorizontalAlignment(SwingConstants.TRAILING);
    beginDateLabel.setText("Begin Date:");
    beginDateLabel.setHorizontalAlignment(SwingConstants.TRAILING);
    endDateLabel.setText("End Date:");
    endDateLabel.setHorizontalAlignment(SwingConstants.TRAILING);
    formatLabel.setText("Format:");
    formatLabel.setHorizontalAlignment(SwingConstants.TRAILING);
    sizeLabel.setText("Size (in KB):");
    sizeLabel.setHorizontalAlignment(SwingConstants.TRAILING);
    numFilesLabel.setText("Number of Files:");
    numFilesLabel.setHorizontalAlignment(SwingConstants.TRAILING);
    seqNumLabel.setText("Sequence Number:");
    seqNumLabel.setHorizontalAlignment(SwingConstants.TRAILING);
    primaryLabel.setText("Primary Label:");
    primaryLabel.setHorizontalAlignment(SwingConstants.TRAILING);
    backupLabel.setText("Backup Label:");
    backupLabel.setHorizontalAlignment(SwingConstants.TRAILING);
    tapeIDLabel8.setText("Backup Medium:");
    tapeIDLabel8.setHorizontalAlignment(SwingConstants.TRAILING);
    primMedLabel.setText("Primary Medium:");
    primMedLabel.setHorizontalAlignment(SwingConstants.TRAILING);
    archiveDateLabel.setText("Archive Date:");
    archiveDateLabel.setHorizontalAlignment(SwingConstants.TRAILING);
    xYLayout1.setWidth(801);
    xYLayout1.setHeight(586);
    tapeTable.setMinimumSize(new Dimension(80, 80));
    tapeTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
    tapeTable.setCellSelectionEnabled(true);
    tapeTable.setRowSelectionAllowed(false);
    tapeTable.setDataSet(dataModule11.getQueryTape());
    tapeTable.setHiddenColumns(new int[] { 0,2,3,4,5,6,7,10,11,12,13,14,15 });
    tapeScrollPane.getViewport().setBackground(thisTabColor);
    tapeIDTextField.setBackground(new Color(240, 240, 240));
    tapeIDTextField.setEditable(false);
    tapeIDTextField.setHorizontalAlignment(SwingConstants.CENTER);
    tapeIDTextField.setColumnName("tape_id");
    tapeIDTextField.setDataSet(dataModule11.getQueryTape());
    dsIDTextField.setHorizontalAlignment(SwingConstants.CENTER);
    dsIDTextField.setColumnName("dataset_id");
    dsIDTextField.setDataSet(dataModule11.getQueryTape());
    seqNumTextField.setHorizontalAlignment(SwingConstants.CENTER);
    seqNumTextField.setColumnName("sequence_number");
    seqNumTextField.setDataSet(dataModule11.getQueryTape());
    numFilesTextField.setHorizontalAlignment(SwingConstants.CENTER);
    numFilesTextField.setColumnName("num_files");
    numFilesTextField.setDataSet(dataModule11.getQueryTape());
    sizeTextField.setHorizontalAlignment(SwingConstants.CENTER);
    sizeTextField.setColumnName("size_kb");
    sizeTextField.setDataSet(dataModule11.getQueryTape());
    archiveDateTextField.setHorizontalAlignment(SwingConstants.CENTER);
    archiveDateTextField.setColumnName("data_archive_date");
    archiveDateTextField.setDataSet(dataModule11.getQueryTape());
    jdbCheckBox1.setBackground(thisTabColor);
    jdbCheckBox1.setFont(new java.awt.Font("Dialog", 0, 12));
    jdbCheckBox1.setHorizontalAlignment(SwingConstants.LEADING);
    jdbCheckBox1.setHorizontalTextPosition(SwingConstants.RIGHT);
    jdbCheckBox1.setText("hide");
    jdbCheckBox1.setColumnName("hide");
    jdbCheckBox1.setDataSet(dataModule11.getQueryTape());
    beginDateTextField.setDataSet(dataModule11.getQueryTape());
    beginDateTextField.setHorizontalAlignment(SwingConstants.CENTER);
    beginDateTextField.setColumnName("begin_date");
    endDateTextField.setDataSet(dataModule11.getQueryTape());
    endDateTextField.setHorizontalAlignment(SwingConstants.CENTER);
    endDateTextField.setColumnName("end_date");
    primLabelTextField.setDataSet(dataModule11.getQueryTape());
    primLabelTextField.setHorizontalAlignment(SwingConstants.CENTER);
    primLabelTextField.setColumnName("primary_label");
    backupLabelTextField.setDataSet(dataModule11.getQueryTape());
    backupLabelTextField.setHorizontalAlignment(SwingConstants.CENTER);
    backupLabelTextField.setColumnName("backup_label");
    formatComboBox.setBackground(Color.white);
    formatComboBox.setPopupVisible(false);
    formatComboBox.setColumnName("format_id");
    formatComboBox.setDataSet(dataModule11.getQueryTape());
    formatComboBox.setItems(null);
    formatComboBox.setSelectedIndex(-1);
    formatComboBox.setSelectedItem(null);
    primMedComboBox.setDataSet(dataModule11.getQueryTape());
    primMedComboBox.setSelectedIndex(-1);
    primMedComboBox.setBackground(Color.white);
    primMedComboBox.setColumnName("primary_medium_id");
    backupMedComboBox.setBackground(Color.white);
    backupMedComboBox.setColumnName("backup_medium_id");
    backupMedComboBox.setDataSet(dataModule11.getQueryTape());
    this.add(tapeIDLabel,  new XYConstraints(23, 59, 127, 23));
    this.add(datasetIDLabel,  new XYConstraints(23, 93, 127, 23));
    this.add(seqNumLabel,  new XYConstraints(23, 127, 127, 23));
    this.add(formatLabel,  new XYConstraints(23, 161, 127, 23));
    this.add(numFilesLabel,  new XYConstraints(23, 194, 127, 23));
    this.add(sizeLabel,  new XYConstraints(23, 228, 127, 23));
    this.add(archiveDateLabel,  new XYConstraints(23, 262, 127, 23));
    this.add(tapeIDTextField, new XYConstraints(169, 59, 160, 24));
    this.add(dsIDTextField, new XYConstraints(169, 93, 160, 24));
    this.add(seqNumTextField, new XYConstraints(169, 127, 160, 24));
    this.add(numFilesTextField, new XYConstraints(169, 194, 160, 24));
    this.add(sizeTextField, new XYConstraints(169, 228, 160, 24));
    this.add(archiveDateTextField, new XYConstraints(169, 262, 160, 24));
    this.add(formatComboBox,  new XYConstraints(169, 161, 230, 23));
    this.add(endDateLabel,  new XYConstraints(385, 93, 132, 23));
    this.add(primaryLabel,  new XYConstraints(385, 127, 132, 23));
    this.add(primMedLabel,  new XYConstraints(385, 161, 132, 23));
    this.add(backupLabel,  new XYConstraints(385, 194, 132, 23));
    this.add(tapeIDLabel8,  new XYConstraints(385, 228, 132, 23));
    this.add(endDateTextField, new XYConstraints(541, 93, 160, 24));
    this.add(primLabelTextField, new XYConstraints(541, 127, 160, 24));
    this.add(backupLabelTextField, new XYConstraints(541, 194, 160, 24));
    this.add(jdbCheckBox1, new XYConstraints(541, 262, 141, 23));
    this.add(primMedComboBox,   new XYConstraints(541, 161, 230, 23));
    this.add(backupMedComboBox,   new XYConstraints(541, 228, 230, 23));
    this.add(beginDateTextField,  new XYConstraints(541, 58, 160, 24));
    this.add(beginDateLabel,     new XYConstraints(385, 59, 132, 23));
    this.add(tapeScrollPane,                new XYConstraints(59, 333, 661, 187));
    tapeScrollPane.getViewport().add(tapeTable, null);
  }

  public static Color getTabColor() {
    return thisTabColor;
  }
}
