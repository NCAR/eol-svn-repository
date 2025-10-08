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

public class ProjectTabPanel extends JPanel {
  static Color tabPale = new Color(202, 207, 200);
  static Color thisTabColor = tabPale;
  JPanel projTopPanel = new JPanel();
  BorderLayout borderLayout2 = new BorderLayout();
  JdbTextField endDateTextField = new JdbTextField();
  JLabel beginDateLabel = new JLabel();
  JPanel datesLLPanel = new JPanel();
  JdbTextField beginDateTextField = new JdbTextField();
  JLabel endDateLabel = new JLabel();
  TitledBorder titledBorder1;
  Border border2;
  JPanel detailsPanel = new JPanel();
  JLabel projNameLabel = new JLabel();
  JdbTextField projNameTextField = new JdbTextField();
  JLabel intContactLabel = new JLabel();
  JPanel descTextPanel = new JPanel();
  Border border3;
  JdbTextPane descTextPane = new JdbTextPane();
  JdbLabel descLabel = new JdbLabel();
  JScrollPane descScrollPane = new JScrollPane(descTextPane);
  DataModule1 dataModule11;
  JdbComboBox intContactComboBox = new JdbComboBox();
  JLabel gcmdLabel = new JLabel();
  JdbComboBox parentComboBox = new JdbComboBox();
  JLabel parentLabel = new JLabel();
  XYLayout xYLayout2 = new XYLayout();
  JPanel latlonPanel = new JPanel();
  XYLayout xYLayout3 = new XYLayout();
  JLabel wLonLabel = new JLabel();
  JdbTextField nLatTextField = new JdbTextField();
  JdbTextField sLatTextField = new JdbTextField();
  JdbTextField eLonTextField = new JdbTextField();
  JdbTextField wLonTextField = new JdbTextField();
  JLabel eLonLabel = new JLabel();
  JLabel sLatLabel = new JLabel();
  JLabel nLatLabel = new JLabel();
  XYLayout xYLayout1 = new XYLayout();
  XYLayout xYLayout4 = new XYLayout();
  JdbCheckBox hideCheckBox = new JdbCheckBox();
  XYLayout xYLayout5 = new XYLayout();
  JdbTextField gcmdNameTextField = new JdbTextField();
  JdbLabel prefixComboLabel = new JdbLabel();
  TableScrollPane prefixScrollPane = new TableScrollPane();
  JdbTable prefixTable = new JdbTable();
  JdbLabel navComboLabel = new JdbLabel();
  JdbNavComboBox linkProjNavComboBox = new JdbNavComboBox();
  JdbLabel projPrefixLabel = new JdbLabel();
  JdbTextField prefixTextField = new JdbTextField();

  public ProjectTabPanel() {
    try  {
      jbInit();
    }
    catch(Exception ex) {
      ex.printStackTrace();
    }
  }

  private void jbInit() throws Exception {
    titledBorder1 = new TitledBorder("Details");
    border2 = BorderFactory.createEmptyBorder(0,15,0,0);
    border3 = BorderFactory.createEmptyBorder(10,0,0,0);
    dataModule11 = tab_files.DataModule1.getDataModule();
    this.setLayout(xYLayout5);
    projTopPanel.setBackground(thisTabColor);
    projTopPanel.setBorder(titledBorder1);
    projTopPanel.setMinimumSize(new Dimension(660, 310));
    projTopPanel.setPreferredSize(new Dimension(662, 355));
    projTopPanel.setLayout(borderLayout2);
    endDateTextField.setBorder(BorderFactory.createLoweredBevelBorder());
    endDateTextField.setText("1999/08/31");
    endDateTextField.setHorizontalAlignment(SwingConstants.CENTER);
    endDateTextField.setColumnName("end_date");
    endDateTextField.setDataSet(dataModule11.getQueryProject());
    beginDateLabel.setForeground(Color.black);
    beginDateLabel.setMinimumSize(new Dimension(26, 14));
    beginDateLabel.setPreferredSize(new Dimension(36, 14));
    beginDateLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    beginDateLabel.setHorizontalTextPosition(SwingConstants.LEFT);
    beginDateLabel.setText("Begin Date:");
    datesLLPanel.setBackground(thisTabColor);
    datesLLPanel.setBorder(border2);
    datesLLPanel.setPreferredSize(new Dimension(330, 200));
    datesLLPanel.setLayout(xYLayout2);
    beginDateTextField.setBorder(BorderFactory.createLoweredBevelBorder());
    beginDateTextField.setPreferredSize(new Dimension(4, 20));
    beginDateTextField.setText("1999/01/01");
    beginDateTextField.setHorizontalAlignment(SwingConstants.CENTER);
    beginDateTextField.setColumnName("begin_date");
    beginDateTextField.setDataSet(dataModule11.getQueryProject());
    endDateLabel.setForeground(Color.black);
    endDateLabel.setMinimumSize(new Dimension(26, 14));
    endDateLabel.setPreferredSize(new Dimension(36, 14));
    endDateLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    endDateLabel.setText("End Date:");
    projNameLabel.setBackground(thisTabColor);
    projNameLabel.setForeground(Color.blue);
    projNameLabel.setMaximumSize(new Dimension(150, 17));
    projNameLabel.setPreferredSize(new Dimension(100, 17));
    projNameLabel.setHorizontalAlignment(SwingConstants.LEFT);
    projNameLabel.setText("Project Name:");
    projNameTextField.setPreferredSize(new Dimension(340, 21));
    projNameTextField.setText("GCIP Enhanced Seasonal Observing Period - 1995");
    projNameTextField.setColumnName("full_name");
    projNameTextField.setDataSet(dataModule11.getQueryProject());
    intContactLabel.setForeground(Color.black);
    intContactLabel.setMaximumSize(new Dimension(150, 17));
    intContactLabel.setMinimumSize(new Dimension(83, 14));
    intContactLabel.setPreferredSize(new Dimension(85, 17));
    intContactLabel.setHorizontalAlignment(SwingConstants.LEFT);
    intContactLabel.setText("Internal Contact:");
    detailsPanel.setBackground(thisTabColor);
    detailsPanel.setPreferredSize(new Dimension(355, 155));
    detailsPanel.setLayout(xYLayout1);
    descTextPanel.setBackground(thisTabColor);
    descTextPanel.setEnabled(false);
    descTextPanel.setBorder(BorderFactory.createEtchedBorder());
    descTextPanel.setMaximumSize(new Dimension(800, 390));
    descTextPanel.setMinimumSize(new Dimension(311, 220));
    descTextPanel.setPreferredSize(new Dimension(662, 330));
    descTextPanel.setLayout(xYLayout4);
    descTextPane.setPreferredSize(new Dimension(640, 400));
    descTextPane.setCaretColor(new java.awt.Color(0, 50, 150));
    descTextPane.setEditable(true);
    descTextPane.setText("The Global Energy and Water Cycle Experiment (GEWEX) Continental-scale " +
    "International Seasonal Observing Period - 1995 (ESOP-95) takes place " +
    "in...");
    descTextPane.setColumnName("description");
    descTextPane.setDataSet(dataModule11.getQueryProject());
    descLabel.setBackground(thisTabColor);
    descLabel.setAutoscrolls(false);
    descLabel.setMinimumSize(new Dimension(108, 17));
    descLabel.setPreferredSize(new Dimension(640, 22));
    descLabel.setText("Project Description:");
    descScrollPane.setPreferredSize(new Dimension(640, 210));

    intContactComboBox.setBackground(Color.white);
    intContactComboBox.setMaximumSize(new Dimension(220, 21));
    intContactComboBox.setMinimumSize(new Dimension(110, 21));
    intContactComboBox.setPreferredSize(new Dimension(340, 21));
    intContactComboBox.setPopupVisible(false);
    intContactComboBox.setColumnName("internal_contact_id");
    intContactComboBox.setDataSet(dataModule11.getQueryProject());
    this.setBackground(new Color(202, 207, 200));
    this.setMinimumSize(new Dimension(800, 700));
    this.setPreferredSize(new Dimension(800, 700));
    this.addComponentListener(new ProjectTabPanel_this_componentAdapter(this));


    gcmdLabel.setText("GCMD name:");
    gcmdLabel.setHorizontalAlignment(SwingConstants.LEFT);
    gcmdLabel.setPreferredSize(new Dimension(100, 17));
    gcmdLabel.setMinimumSize(new Dimension(45, 14));
    gcmdLabel.setForeground(Color.black);
    gcmdLabel.setMaximumSize(new Dimension(50, 17));

    parentComboBox.setDataSet(dataModule11.getQueryProject());
    parentComboBox.setSelectedIndex(-1);
    parentComboBox.setSelectedItem(null);
    parentComboBox.setColumnName("parent_project_id");
    parentComboBox.setBackground(Color.lightGray);
    parentComboBox.setVisible(true);
    parentComboBox.setMinimumSize(new Dimension(314, 19));
    parentComboBox.setToolTipText("");

    parentLabel.setText("Parent Project:");
    parentLabel.setHorizontalAlignment(SwingConstants.LEFT);
    parentLabel.setPreferredSize(new Dimension(100, 17));
    parentLabel.setMinimumSize(new Dimension(45, 14));
    parentLabel.setBackground(thisTabColor);
    parentLabel.setMaximumSize(new Dimension(50, 17));

    latlonPanel.setBackground(thisTabColor);
    latlonPanel.setBorder(BorderFactory.createEtchedBorder());
    latlonPanel.setLayout(xYLayout3);
    wLonLabel.setHorizontalAlignment(SwingConstants.CENTER);
    wLonLabel.setHorizontalTextPosition(SwingConstants.LEFT);
    wLonLabel.setText("W. Longitude:");
    nLatTextField.setBorder(BorderFactory.createLoweredBevelBorder());
    nLatTextField.setText("50.00");
    nLatTextField.setHorizontalAlignment(SwingConstants.CENTER);
    nLatTextField.setColumnName("maxlat");
    nLatTextField.setDataSet(dataModule11.getQueryProject());
    sLatTextField.setBorder(BorderFactory.createLoweredBevelBorder());
    sLatTextField.setText("28.00");
    sLatTextField.setHorizontalAlignment(SwingConstants.CENTER);
    sLatTextField.setColumnName("minlat");
    sLatTextField.setDataSet(dataModule11.getQueryProject());
    eLonTextField.setBorder(BorderFactory.createLoweredBevelBorder());
    eLonTextField.setText("-80.00");
    eLonTextField.setHorizontalAlignment(SwingConstants.CENTER);
    eLonTextField.setColumnName("maxlon");
    eLonTextField.setDataSet(dataModule11.getQueryProject());
    wLonTextField.setBorder(BorderFactory.createLoweredBevelBorder());
    wLonTextField.setText("-115.00");
    wLonTextField.setHorizontalAlignment(SwingConstants.CENTER);
    wLonTextField.setColumnName("minlon");
    wLonTextField.setDataSet(dataModule11.getQueryProject());
    eLonLabel.setHorizontalAlignment(SwingConstants.CENTER);
    eLonLabel.setText("E. Longitude:");
    sLatLabel.setMinimumSize(new Dimension(26, 14));
    sLatLabel.setPreferredSize(new Dimension(36, 14));
    sLatLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    sLatLabel.setText("S. Latitude:");
    nLatLabel.setMinimumSize(new Dimension(26, 14));
    nLatLabel.setPreferredSize(new Dimension(36, 14));
    nLatLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    nLatLabel.setText("N. Latitude:");
    hideCheckBox.setBackground(thisTabColor);
    hideCheckBox.setBorder(null);
    hideCheckBox.setBorderPainted(true);
    hideCheckBox.setContentAreaFilled(true);
    hideCheckBox.setHorizontalAlignment(SwingConstants.CENTER);
    hideCheckBox.setHorizontalTextPosition(SwingConstants.LEFT);
    hideCheckBox.setText("hide project:");
    hideCheckBox.setVerticalAlignment(SwingConstants.CENTER);
    hideCheckBox.setVerticalTextPosition(SwingConstants.CENTER);
    hideCheckBox.setColumnName("hide");
    hideCheckBox.setDataSet(dataModule11.getQueryProject());
    gcmdNameTextField.setText("");
    gcmdNameTextField.setColumnName("gcmd_name");
    gcmdNameTextField.setDataSet(dataModule11.getQueryProject());
    prefixComboLabel.setText("Project Prefixes:");
    prefixComboLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    prefixComboLabel.setPreferredSize(new Dimension(250, 14));
    prefixTable.setAutoResizeMode(JTable.AUTO_RESIZE_NEXT_COLUMN);
    prefixTable.setColumnHeaderVisible(false);
    prefixTable.setDataSet(dataModule11.getQueryProjPrefix());
    prefixTable.setRowHeader(null);
    prefixTable.setRowHeaderVisible(false);
    prefixScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
    navComboLabel.setPreferredSize(new Dimension(250, 14));
    navComboLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    navComboLabel.setText("Datasets linked to this project:");
    linkProjNavComboBox.setBackground(new Color(223, 231, 206));
    linkProjNavComboBox.setMinimumSize(new Dimension(21, 18));
    linkProjNavComboBox.setOpaque(true);
    linkProjNavComboBox.setPreferredSize(new Dimension(100, 18));
    linkProjNavComboBox.setColumnName("dataset_id");
    linkProjNavComboBox.setDataSet(dataModule11.getQueryProjDataset());
    linkProjNavComboBox.setDropDownWidth(-1);
    projPrefixLabel.setText("Project prefix:");
    projPrefixLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    projPrefixLabel.setPreferredSize(new Dimension(250, 14));
    prefixTextField.setFont(new java.awt.Font("Dialog", 1, 12));
    prefixTextField.setEditable(false);
    prefixTextField.setText("");
    prefixTextField.setColumnName("dataset_id_prefix");
    prefixTextField.setDataSet(dataModule11.getQueryThisPrefix());
    xYLayout5.setWidth(810);
    xYLayout5.setHeight(680);
    this.add(projTopPanel,    new XYConstraints(0, 2, 692, 288));
    projTopPanel.add(datesLLPanel,  BorderLayout.WEST);
    latlonPanel.add(wLonLabel,  new XYConstraints(0, 62, 91, 30));
    latlonPanel.add(nLatTextField, new XYConstraints(110, 6, 58, 21));
    latlonPanel.add(sLatTextField, new XYConstraints(110, 39, 58, -1));
    latlonPanel.add(wLonTextField, new XYConstraints(5, 87, 68, 22));
    latlonPanel.add(sLatLabel,  new XYConstraints(8, 35, 97, 30));
    latlonPanel.add(nLatLabel,  new XYConstraints(6, 0, 99, 30));
    latlonPanel.add(eLonTextField, new XYConstraints(99, 86, 70, 22));
    latlonPanel.add(eLonLabel, new XYConstraints(91, 62, 95, 30));
    datesLLPanel.add(beginDateLabel,  new XYConstraints(5, 15, 92, 30));
    datesLLPanel.add(endDateTextField,     new XYConstraints(108, 50, 108, 22));
    datesLLPanel.add(latlonPanel, new XYConstraints(47, 109, 198, 131));
    projTopPanel.add(detailsPanel, BorderLayout.CENTER);
    descTextPanel.add(descLabel,  new XYConstraints(24, 5, -1, -1));
    descTextPanel.add(descScrollPane,  new XYConstraints(24, 32, -1, -1));
    detailsPanel.add(projNameLabel, new XYConstraints(2, 7, -1, -1));
    detailsPanel.add(projNameTextField, new XYConstraints(2, 29, 341, -1));
    detailsPanel.add(intContactLabel, new XYConstraints(2, 55, 138, -1));
    detailsPanel.add(intContactComboBox, new XYConstraints(2, 77, 341, -1));
    detailsPanel.add(gcmdLabel, new XYConstraints(1, 110, -1, -1));
    detailsPanel.add(gcmdNameTextField, new XYConstraints(2, 128, 341, -1));
    detailsPanel.add(parentLabel, new XYConstraints(1, 157, -1, -1));
    detailsPanel.add(parentComboBox, new XYConstraints(1, 177, 341, -1));
    detailsPanel.add(linkProjNavComboBox, new XYConstraints(203, 233, -1, -1));
    detailsPanel.add(navComboLabel, new XYConstraints(-32, 236, 225, -1));
    detailsPanel.add(prefixTextField, new XYConstraints(203, 207, 57, 18));
    detailsPanel.add(projPrefixLabel, new XYConstraints(92, 209, 101, -1));
    this.add(descTextPanel,                      new XYConstraints(-2, 290, 692, 358));
    datesLLPanel.add(beginDateTextField, new XYConstraints(108, 19, 108, 22));
    datesLLPanel.add(endDateLabel,   new XYConstraints(15, 48, 82, 30));
    descTextPanel.add(prefixScrollPane,   new XYConstraints(378, 248, 286, 45));
    descTextPanel.add(hideCheckBox,                               new XYConstraints(56, 249, 104, 18));
    descTextPanel.add(prefixComboLabel,            new XYConstraints(238, 250, 129, -1));
    prefixScrollPane.getViewport().add(prefixTable, null);
  }

  void this_componentShown(ComponentEvent e) {
    if (!dataModule11.getParamRowID().getString("dataset_id").equals(dataModule11.queryDataset1.getString("dataset_id"))) {
      dataModule11.paramRowID.setString("dataset_id", dataModule11.queryDataset1.getString("dataset_id"));
      dataModule11.queryDatasetProj.refresh(); // update dataset ID/proj ID link table
      dataModule11.paramRowProjID.setString("proj_id", dataModule11.queryDatasetProj.getString("project_id"));
      dataModule11.queryProject.refresh();
      dataModule11.queryProjDataset.refresh();
    }
  }

}

class ProjectTabPanel_this_componentAdapter extends java.awt.event.ComponentAdapter {
  ProjectTabPanel adaptee;

  ProjectTabPanel_this_componentAdapter(ProjectTabPanel adaptee) {
    this.adaptee = adaptee;
  }
  public void componentShown(ComponentEvent e) {
    adaptee.this_componentShown(e);
  }
}

