package tab_files;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

import com.borland.dbswing.*;
import com.borland.jbcl.layout.*;

/**
 * <p>Title: GUImaint for CODIAC</p>
 * <p>Description: MySQL db version</p>
 * <p>Copyright: Copyright (c) 2004, 2005, 2006, 2007</p>
 * <p>Company: NCAR/EOL</p>
 * @author Don Stott
 * @version 2.5- test
 */

public class PhysDirPanel extends JPanel {

  JPanel detailsPanel = new JPanel();
  static JPanel findFilePanel = new JPanel();
  static Color tabGreen = new Color(221, 228, 209);
  static Color thisTabColor = tabGreen;

  BorderLayout borderLayout1 = new BorderLayout();

  // -------------------------------------------------------------------
  // date/time panel
  // -------------------------------------------------------------------


  // -------------------------------------------------------------------
  // directory/filename panel
  // -------------------------------------------------------------------

  // -------------------------------------------------------------------
  // other details
  // -------------------------------------------------------------------
  BorderLayout borderLayout3 = new BorderLayout();
  JPanel details1Panel = new JPanel();
  FlowLayout flowLayout1 = new FlowLayout();
  JLabel descriptionLabel = new JLabel();
  JdbTextField descTextField = new JdbTextField();

  JPanel details2Panel = new JPanel();
  FlowLayout flowLayout2 = new FlowLayout();
  JLabel machineLabel = new JLabel();
  JdbTextField machineTextField = new JdbTextField();
  JLabel archiveDateLabel = new JLabel();
  JdbTextField archiveDateTextField = new JdbTextField();
  JdbCheckBox hideCheckBox = new JdbCheckBox();
  JdbStatusLabel jdbStatusLabel1 = new JdbStatusLabel();
  BorderLayout borderLayout4 = new BorderLayout();
  JPanel headerPanel = new JPanel();
  JLabel dirFileLabel = new JLabel();
  TableScrollPane tableScrollPane1 = new TableScrollPane();
  XYLayout xYLayout2 = new XYLayout();
  JdbTable physDirTable = new JdbTable();
  DataModule1 dataModule11;
  XYLayout xYLayout1 = new XYLayout();
  JPanel dateTimePanel = new JPanel();
  JdbComboBox jdbComboBox1 = new JdbComboBox();
  JLabel formatLabel = new JLabel();
  JPanel datePanel = new JPanel();
  XYLayout xYLayout3 = new XYLayout();
  JdbTextField beginDateTextField = new JdbTextField();
  JLabel beginDateLabel = new JLabel();
  JLabel endDateLabel = new JLabel();
  JdbTextField endDateTextField = new JdbTextField();
  JLabel timeLabel = new JLabel();
  JLabel dateLabel = new JLabel();
  JPanel latLonPanel = new JPanel();
  JdbLabel minLatLabel = new JdbLabel();
  JdbTextField minLatTextField = new JdbTextField();
  JdbLabel minLonLabel = new JdbLabel();
  JdbTextField minLonTextField = new JdbTextField();
  JdbLabel minAltLabel = new JdbLabel();
  JdbTextField minAltTextField = new JdbTextField();
  JdbLabel maxLatLabel = new JdbLabel();
  JdbTextField maxLatTextField = new JdbTextField();
  JdbLabel maxLonLabel = new JdbLabel();
  JdbTextField maxLonTextField = new JdbTextField();
  JdbLabel maxAltLabel = new JdbLabel();
  JdbTextField maxAltTextField = new JdbTextField();
  JLabel eventLabel = new JLabel();
  JdbTextField eventTextField = new JdbTextField();
  JdbLabel altUnitsLabel = new JdbLabel();
  JdbComboBox unitsComboBox = new JdbComboBox();
  JLabel sizeLabel = new JLabel();
  JdbTextField sizeTextField = new JdbTextField();
  GridBagLayout gridBagLayout1 = new GridBagLayout();

  public PhysDirPanel() {
    try  {
      jbInit();
    }
    catch(Exception ex) {
      ex.printStackTrace();
    }
  }

  private void jbInit() throws Exception {
    this.setLayout(borderLayout1);
    borderLayout1.setVgap(10);
    this.setBackground(thisTabColor);
    this.setPreferredSize(new Dimension(675, 500));
    this.addComponentListener(new ComponentAdapter(this));
    this.addComponentListener(new java.awt.event.ComponentAdapter() {
      public void componentShown(ComponentEvent e) {
        this_componentShown(e);
      }
    });

    dataModule11 = tab_files.DataModule1.getDataModule();

    findFilePanel.setBackground(thisTabColor);
    findFilePanel.setBorder(BorderFactory.createEtchedBorder());
    findFilePanel.setMinimumSize(new Dimension(150, 20));
    findFilePanel.setPreferredSize(new Dimension(300, 250));
    findFilePanel.setLayout(borderLayout4);

    detailsPanel.setLayout(borderLayout3);
    detailsPanel.setBackground(thisTabColor);
    detailsPanel.setMinimumSize(new Dimension(447, 85));
    detailsPanel.setPreferredSize(new Dimension(675, 130));


    // -------------------------------------------------------------------
    // date and time
    // -------------------------------------------------------------------

    // -------------------------------------------------------------------

    // -------------------------------------------------------------------
    // directory and file name
    // -------------------------------------------------------------------

    // -------------------------------------------------------------------
    // media, format, size, etc
    // -------------------------------------------------------------------
    details1Panel.setBackground(thisTabColor);
    details1Panel.setMinimumSize(new Dimension(530, 37));
    details1Panel.setPreferredSize(new Dimension(390, 10));
    details1Panel.setLayout(flowLayout2);
    flowLayout2.setAlignment(FlowLayout.RIGHT);
    flowLayout2.setHgap(10);
    flowLayout2.setVgap(7);

    descriptionLabel.setPreferredSize(new Dimension(290, 17));
    descriptionLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    descriptionLabel.setText("Type of file:");
    descTextField.setBackground(Color.white);
    descTextField.setPreferredSize(new Dimension(40, 21));
    descTextField.setToolTipText("data, usually");
    descTextField.setEditable(true);
    descTextField.setText("data");
    descTextField.setHorizontalAlignment(SwingConstants.CENTER);
    descTextField.setColumnName("purpose");
    descTextField.setDataSet(dataModule11.getQueryFile());

    details2Panel.setBackground(thisTabColor);
    details2Panel.setPreferredSize(new Dimension(290, 10));
    details2Panel.setLayout(flowLayout1);
    flowLayout1.setAlignment(FlowLayout.LEFT);
    flowLayout1.setHgap(6);
    flowLayout1.setVgap(7);


    machineLabel.setPreferredSize(new Dimension(110, 21));
    machineLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    machineLabel.setText("Machine Address:");
    machineTextField.setPreferredSize(new Dimension(135, 21));
    machineTextField.setToolTipText("localhost (usual) or mass_store");
    machineTextField.setText("localhost");
    machineTextField.setColumnName("host");
    machineTextField.setDataSet(dataModule11.getQueryFile());

    archiveDateLabel.setForeground(Color.black);
    archiveDateLabel.setPreferredSize(new Dimension(110, 21));
    archiveDateLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    archiveDateLabel.setText("Archive Date:");
    archiveDateTextField.setPreferredSize(new Dimension(135, 21));
    archiveDateTextField.setToolTipText("today, usually");
    archiveDateTextField.setHorizontalAlignment(SwingConstants.CENTER);
    archiveDateTextField.setColumnName("data_archive_date");
    archiveDateTextField.setDataSet(dataModule11.getQueryFile());
    archiveDateTextField.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        archiveDateTextField_actionPerformed(e);
      }
    });
    archiveDateTextField.setText("");

    hideCheckBox.setBackground(thisTabColor);
    hideCheckBox.setPreferredSize(new Dimension(180, 25));
    hideCheckBox.setHorizontalAlignment(SwingConstants.RIGHT);
    hideCheckBox.setHorizontalTextPosition(SwingConstants.LEFT);
    hideCheckBox.setText("       Hide from Public");
    hideCheckBox.setColumnName("hide");
    hideCheckBox.setDataSet(dataModule11.getQueryFile());
    hideCheckBox.setUnknownDataValueMode(DBDataBinder.CLEAR_VALUE);

    // north panel
    jdbStatusLabel1.setBackground(thisTabColor);
    jdbStatusLabel1.setAutoscrolls(true);
    jdbStatusLabel1.setPreferredSize(new Dimension(100, 21));
    jdbStatusLabel1.setText("jdbStatusLabel1");
    dirFileLabel.setPreferredSize(new Dimension(600, 17));
    dirFileLabel.setText("Data Directory and File:");
    headerPanel.setLayout(xYLayout2);
    physDirTable.setToolTipText("click on row to select");
    physDirTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
    physDirTable.setColumnSelectionAllowed(true);
    physDirTable.setRowSelectionAllowed(true);
    physDirTable.setDataSet(dataModule11.getQueryFile());
    dateTimePanel.setLayout(xYLayout1);
    dateTimePanel.setBackground(thisTabColor);
    dateTimePanel.setPreferredSize(new Dimension(420, 80));

    // center panel
    tableScrollPane1.getViewport().setBackground(thisTabColor);
    headerPanel.setBackground(thisTabColor);
    jdbComboBox1.setBackground(Color.white);
    jdbComboBox1.setFont(new java.awt.Font("SansSerif", 0, 11));
    jdbComboBox1.setMinimumSize(new Dimension(225, 19));
    jdbComboBox1.setPreferredSize(new Dimension(265, 21));
    jdbComboBox1.setColumnName("format_id");
    jdbComboBox1.setDataSet(dataModule11.getQueryFile());
    formatLabel.setPreferredSize(new Dimension(95, 17));
    formatLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    formatLabel.setText("File Format:");
    datePanel.setLayout(xYLayout3);
    datePanel.setBackground(new Color(221, 228, 209));
    beginDateTextField.setText("0001-01-01 00:00");
    beginDateTextField.setColumns(0);
    beginDateTextField.setHorizontalAlignment(SwingConstants.CENTER);
    beginDateTextField.setColumnName("begin_date");
    beginDateTextField.setDataSet(dataModule11.getQueryFile());
    beginDateLabel.setBackground(thisTabColor);
    beginDateLabel.setPreferredSize(new Dimension(54, 17));
    beginDateLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    beginDateLabel.setText("Begin:");
    endDateLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    endDateLabel.setText("End:");
    endDateTextField.setText("1992/02/01");
    endDateTextField.setHorizontalAlignment(SwingConstants.CENTER);
    endDateTextField.setColumnName("end_date");
    endDateTextField.setDataSet(dataModule11.getQueryFile());
    timeLabel.setHorizontalAlignment(SwingConstants.LEFT);
    timeLabel.setText("    TIME");
    dateLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    dateLabel.setText("       DATE");
    latLonPanel.setBackground(new Color(221, 228, 209));
    latLonPanel.setLayout(gridBagLayout1);
    minLatLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    minLatLabel.setText("min lat:");
    minLatTextField.setText("");
    minLatTextField.setColumnName("minlat");
    minLatTextField.setDataSet(dataModule11.getQueryFile());
    minLonLabel.setText("min lon:");
    minLonLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    minLonTextField.setDataSet(dataModule11.getQueryFile());
    minLonTextField.setColumnName("minlon");
    minLonTextField.setText("");
    minAltLabel.setText("min alt:");
    minAltLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    minAltTextField.setDataSet(dataModule11.getQueryFile());
    minAltTextField.setColumnName("minalt");
    minAltTextField.setText("");
    maxLatLabel.setText("max lat:");
    maxLatLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    maxLatTextField.setDataSet(dataModule11.getQueryFile());
    maxLatTextField.setColumnName("maxlat");
    maxLatTextField.setText("");
    maxLonLabel.setText("max lon:");
    maxLonLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    maxLonTextField.setDataSet(dataModule11.getQueryFile());
    maxLonTextField.setColumnName("maxlon");
    maxLonTextField.setText("");
    maxAltLabel.setText("max alt:");
    maxAltLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    maxAltTextField.setDataSet(dataModule11.getQueryFile());
    maxAltTextField.setColumnName("maxalt");
    maxAltTextField.setText("");
    eventLabel.setText("Event Note:");
    eventLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    eventLabel.setPreferredSize(new Dimension(110, 21));
    eventTextField.setText("850");
    eventTextField.setPreferredSize(new Dimension(135, 21));
    eventTextField.setToolTipText("15 char Max! Use for flight numbers, other notes.");
    eventTextField.setDataSet(dataModule11.getQueryFile());
    eventTextField.setColumnName("event");
    eventTextField.setHorizontalAlignment(SwingConstants.CENTER);
    altUnitsLabel.setText("alt units:");
    altUnitsLabel.setOpaque(false);
    altUnitsLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    unitsComboBox.setColumnName("alt_units");
    unitsComboBox.setDataSet(dataModule11.getQueryFile());
    unitsComboBox.setItems(new String[] {"meters", "mb"});
    unitsComboBox.setBackground(Color.white);
    sizeLabel.setPreferredSize(new Dimension(110, 21));
    sizeLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    sizeLabel.setText("File Size (KB):");
    sizeTextField.setHorizontalAlignment(SwingConstants.CENTER);
    sizeTextField.setColumnName("size_kb");
    sizeTextField.setDataSet(dataModule11.getQueryFile());
    sizeTextField.setPreferredSize(new Dimension(75, 21));
    sizeTextField.setText("850");
    findFilePanel.add(jdbStatusLabel1, BorderLayout.SOUTH);
    findFilePanel.add(headerPanel, BorderLayout.NORTH);
    headerPanel.add(dirFileLabel, new XYConstraints(19, 5, 275, -1));
    findFilePanel.add(tableScrollPane1, BorderLayout.CENTER);
    this.add(dateTimePanel, BorderLayout.NORTH);
    tableScrollPane1.getViewport().add(physDirTable, null);
    physDirTable.setHiddenColumns(new int[] { 0, 2, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 } );
    // south panel
    this.add(detailsPanel, BorderLayout.SOUTH);
    // left side of lower panel
    detailsPanel.add(details1Panel, BorderLayout.WEST);
    details1Panel.add(descriptionLabel, null);
    details1Panel.add(descTextField, null);
    details1Panel.add(formatLabel, null);
    details1Panel.add(jdbComboBox1, null);
    details1Panel.add(hideCheckBox, null);
    // right side of lower panel
    detailsPanel.add(details2Panel, BorderLayout.EAST);
    details2Panel.add(sizeLabel, null);
    details2Panel.add(sizeTextField, null);
    details2Panel.add(eventLabel, null);
    details2Panel.add(eventTextField, null);
    details2Panel.add(machineLabel, null);
    details2Panel.add(machineTextField, null);
    details2Panel.add(archiveDateLabel, null);
    details2Panel.add(archiveDateTextField, null);
    this.add(findFilePanel, BorderLayout.CENTER);
    datePanel.add(beginDateTextField, new XYConstraints(80, 27, 147, 20));
    datePanel.add(dateLabel,  new XYConstraints(29, 1, 109, 24));
    datePanel.add(timeLabel, new XYConstraints(160, 0, 60, 24));
    datePanel.add(beginDateLabel, new XYConstraints(19, 24, 40, 24));
    datePanel.add(endDateLabel, new XYConstraints(19, 53, 40, 24));
    datePanel.add(endDateTextField, new XYConstraints(80, 55, 147, 20));
    dateTimePanel.add(latLonPanel,                            new XYConstraints(303, 3, 431, 86));
    dateTimePanel.add(datePanel,                               new XYConstraints(32, 4, 239, 82));
    latLonPanel.add(maxLonTextField,  new GridBagConstraints(5, 1, 1, 1, 1.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(6, 0, 0, 35), 47, -4));
    latLonPanel.add(maxLatLabel,  new GridBagConstraints(4, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(9, 9, 0, 0), 8, 2));
    latLonPanel.add(maxLatTextField,  new GridBagConstraints(5, 0, 1, 1, 1.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(9, 0, 0, 35), 47, -4));
    latLonPanel.add(maxLonLabel,  new GridBagConstraints(4, 1, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(6, 9, 0, 0), 5, 2));
    latLonPanel.add(maxAltLabel,  new GridBagConstraints(4, 2, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 9, 13, 0), 8, 2));
    latLonPanel.add(maxAltTextField,  new GridBagConstraints(5, 2, 1, 1, 1.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 13, 35), 47, -4));
    latLonPanel.add(minLonTextField,  new GridBagConstraints(3, 1, 1, 1, 1.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(7, 0, 0, 0), 47, -4));
    latLonPanel.add(minLatLabel,  new GridBagConstraints(2, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(9, 8, 0, 0), 12, 2));
    latLonPanel.add(minLatTextField,  new GridBagConstraints(3, 0, 1, 1, 1.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(9, 0, 0, 0), 47, -4));
    latLonPanel.add(minLonLabel,  new GridBagConstraints(2, 1, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(7, 8, 0, 0), 9, 2));
    latLonPanel.add(minAltLabel,  new GridBagConstraints(2, 2, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(6, 8, 13, 0), 12, 2));
    latLonPanel.add(minAltTextField,  new GridBagConstraints(3, 2, 1, 1, 1.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(6, 0, 13, 0), 47, -4));
    latLonPanel.add(unitsComboBox,  new GridBagConstraints(1, 2, 1, 1, 1.0, 0.0
            ,GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(6, 6, 13, 0), 20, -1));
    latLonPanel.add(altUnitsLabel,  new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(6, 6, 13, 0), 20, 2));
  }

//-----------------------------------------------------------------
// If the file tab is clicked, check last files retrieved and
// if different from the dataset ID, get files for the dataset
// ID. Show hourglass cursor until done loading files into table.
// Has test for keepIDCheckBox.isSelected() here so files do
// NOT change when using a dataset to copy.
//-----------------------------------------------------------------

  void this_componentShown(ComponentEvent e) {
    if (!SearchDatasetPanel.keepIDCheckBox.isSelected()) {
      if (!dataModule11.queryFile.getString("dataset_id").equals(dataModule11.queryDataset1.getString("dataset_id"))) {
        Cursor waitCursor = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR);
        findFilePanel.setCursor(waitCursor);
        dataModule11.paramRowFile.setString("ds_id", dataModule11.queryDataset1.getString("dataset_id"));
        dataModule11.paramRowFile.setString("purpose", "data");
        dataModule11.queryFile.refresh();
      }
    }
    else {
      System.out.println("We are keeping the dataset ID, so will skip updating of tabs.");
    }
  }

  public static Color getTabColor() {
    return thisTabColor;
  }

  void archiveDateTextField_actionPerformed(ActionEvent e) {

  }

}

class ComponentAdapter extends java.awt.event.ComponentAdapter {
  PhysDirPanel adaptee;

  ComponentAdapter(PhysDirPanel adaptee) {
    this.adaptee = adaptee;
  }
  public void componentShown(ComponentEvent e) {
    adaptee.this_componentShown(e);
  }
}

