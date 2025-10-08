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

public class SubBrowsePanel extends JPanel {
  BorderLayout borderLayout1 = new BorderLayout();
  TitledBorder titledBorder1;
  Border border1;
  TitledBorder titledBorder2;
  TitledBorder titledBorder3;
  TitledBorder titledBorder4;
  JPanel subOptionsPanel = new JPanel();
  JPanel browseProgsPanel = new JPanel();
  JdbCheckBox pCheckBox = new JdbCheckBox();
  JdbTextField paramProgTextField = new JdbTextField();
  JLabel paramProgLabel = new JLabel();
  JdbTextField idListTextField = new JdbTextField();
  JLabel stnScanLabel = new JLabel();
  JdbTextField stnScanTextField = new JdbTextField();
  JdbCheckBox zCheckBox = new JdbCheckBox();
  JdbTextField extractTextField = new JdbTextField();
  JLabel selectProgLabel3 = new JLabel();
  JdbCheckBox xCheckBox = new JdbCheckBox();
  JLabel parmListLabel = new JLabel();
  VerticalFlowLayout verticalFlowLayout3 = new VerticalFlowLayout();
  JLabel mergeProgLabel = new JLabel();
  JdbTextField parmListTextField = new JdbTextField();
  JdbCheckBox idCheckBox = new JdbCheckBox();
  JdbTextField mergeProgTextField = new JdbTextField();
  JdbCheckBox tCheckBox = new JdbCheckBox();
  JLabel extractLabel = new JLabel();
  JdbTextField selectProgTextField = new JdbTextField();
  JdbCheckBox yCheckBox = new JdbCheckBox();
  JLabel idListLabel = new JLabel();
  JPanel subProgsPanel = new JPanel();
  DataModule1 dataModule11;
  JdbLabel fgrLabel = new JdbLabel();
  JdbTextField fgrProgTextField = new JdbTextField();
  TitledBorder titledBorder5;
  XYLayout xYLayout2 = new XYLayout();
//  static Color tabPink = new Color(255, 215, 215);
  static Color tabPink = new Color(244, 223, 223);
  static Color thisTabColor = tabPink;
  XYLayout xYLayout1 = new XYLayout();
  JPanel orderOptionsPanel = new JPanel();
  JdbCheckBox compressCheckBox = new JdbCheckBox();
  TableScrollPane tableScrollPane1 = new TableScrollPane();
  JdbTable dsOptionsTable = new JdbTable();
  JdbCheckBox listFilenamesCheckBox = new JdbCheckBox();
  JdbTextField dirLevelsTextField = new JdbTextField();
  XYLayout xYLayout3 = new XYLayout();
  JLabel dirLevelsLabel1 = new JLabel();
  JLabel dirLevelsLabel2 = new JLabel();
  JdbTextField maxSizeTextField = new JdbTextField();
  JLabel maxSizeLabel = new JLabel();
  JdbCheckBox eventCheckBox = new JdbCheckBox();
  JdbTextField idTextField = new JdbTextField();

  public SubBrowsePanel() {
    try {
      jbInit();
    }
    catch(Exception ex) {
      ex.printStackTrace();
    }
  }
  void jbInit() throws Exception {
    titledBorder1 = new TitledBorder(BorderFactory.createEtchedBorder(Color.white,new Color(148, 145, 140)),"browse programs");
    border1 = BorderFactory.createEtchedBorder(Color.white,new Color(148, 145, 140));
    titledBorder2 = new TitledBorder(border1,"subsetting programs");
    titledBorder3 = new TitledBorder(BorderFactory.createEtchedBorder(Color.white,new Color(148, 145, 140)),"browse types");
    titledBorder4 = new TitledBorder(BorderFactory.createEtchedBorder(Color.white,new Color(148, 145, 140)),"subsetting options");
    dataModule11 = tab_files.DataModule1.getDataModule();
    titledBorder5 = new TitledBorder(BorderFactory.createEtchedBorder(Color.white,new Color(148, 145, 140)),"Order Options");
    this.setLayout(borderLayout1);
    this.setBackground(thisTabColor);
    this.setPreferredSize(new Dimension(800, 700));

    subOptionsPanel.setLayout(verticalFlowLayout3);
    subOptionsPanel.setBackground(thisTabColor);
    subOptionsPanel.setBorder(titledBorder4);
    subOptionsPanel.setMinimumSize(new Dimension(232, 220));
    subOptionsPanel.setPreferredSize(new Dimension(390, 100));
    browseProgsPanel.setLayout(xYLayout2);
    browseProgsPanel.setBackground(thisTabColor);
    browseProgsPanel.setBorder(titledBorder1);
    browseProgsPanel.setMinimumSize(new Dimension(755, 210));
    browseProgsPanel.setPreferredSize(new Dimension(155, 255));
    pCheckBox.setBackground(thisTabColor);
    pCheckBox.setMaximumSize(new Dimension(90, 22));
    pCheckBox.setMinimumSize(new Dimension(75, 25));
    pCheckBox.setPreferredSize(new Dimension(120, 22));
    pCheckBox.setText("p_subset");
    pCheckBox.setColumnName("p_subset");
    pCheckBox.setDataSet(dataModule11.getQueryDatasetOptions());
    paramProgTextField.setPreferredSize(new Dimension(400, 21));
    paramProgTextField.setColumnName("browse_param_prog");
    paramProgTextField.setDataSet(dataModule11.getQueryDatasetOptions());
    paramProgLabel.setPreferredSize(new Dimension(160, 17));
    paramProgLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    paramProgLabel.setText("param_prog");
    idListTextField.setPreferredSize(new Dimension(400, 21));
    idListTextField.setColumnName("order_stnid_list_prog");
    idListTextField.setDataSet(dataModule11.getQueryDatasetOptions());
    stnScanLabel.setText("stn_scan_prog");
    stnScanLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    stnScanLabel.setPreferredSize(new Dimension(160, 17));
    stnScanTextField.setPreferredSize(new Dimension(400, 21));
    stnScanTextField.setColumnName("browse_stn_scan_prog");
    stnScanTextField.setDataSet(dataModule11.getQueryDatasetOptions());
    zCheckBox.setBackground(thisTabColor);
    zCheckBox.setPreferredSize(new Dimension(120, 22));
    zCheckBox.setHorizontalTextPosition(SwingConstants.TRAILING);
    zCheckBox.setText("z_subset");
    zCheckBox.setColumnName("z_subset");
    zCheckBox.setDataSet(dataModule11.getQueryDatasetOptions());
    extractTextField.setPreferredSize(new Dimension(400, 21));
    extractTextField.setColumnName("browse_extract_prog");
    extractTextField.setDataSet(dataModule11.getQueryDatasetOptions());
    selectProgLabel3.setText("select_prog");
    selectProgLabel3.setHorizontalAlignment(SwingConstants.RIGHT);
    selectProgLabel3.setPreferredSize(new Dimension(160, 17));
    xCheckBox.setText("x_subset");
    xCheckBox.setColumnName("x_subset");
    xCheckBox.setDataSet(dataModule11.getQueryDatasetOptions());
    xCheckBox.setUnknownDataValueMode(DBDataBinder.CLEAR_VALUE);
    xCheckBox.setBackground(thisTabColor);
    xCheckBox.setPreferredSize(new Dimension(120, 22));
    xCheckBox.setHorizontalTextPosition(SwingConstants.TRAILING);
    parmListLabel.setText("parm_list_prog");
    parmListLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    parmListLabel.setPreferredSize(new Dimension(160, 17));
    verticalFlowLayout3.setAlignment(VerticalFlowLayout.TOP);
    verticalFlowLayout3.setHgap(40);
    verticalFlowLayout3.setVgap(6);
    verticalFlowLayout3.setHorizontalFill(false);
    mergeProgLabel.setText("merge_prog");
    mergeProgLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    mergeProgLabel.setPreferredSize(new Dimension(160, 17));
    parmListTextField.setPreferredSize(new Dimension(400, 21));
    parmListTextField.setColumnName("order_parm_list_prog");
    parmListTextField.setDataSet(dataModule11.getQueryDatasetOptions());
    idCheckBox.setBackground(thisTabColor);
    idCheckBox.setMaximumSize(new Dimension(100, 22));
    idCheckBox.setMinimumSize(new Dimension(75, 22));
    idCheckBox.setPreferredSize(new Dimension(120, 22));
    idCheckBox.setText("stnid_subset");
    idCheckBox.setColumnName("stnid_subset");
    idCheckBox.setDataSet(dataModule11.getQueryDatasetOptions());
    mergeProgTextField.setPreferredSize(new Dimension(400, 21));
    mergeProgTextField.setColumnName("order_merge_prog");
    mergeProgTextField.setDataSet(dataModule11.getQueryDatasetOptions());
    tCheckBox.setText("time_subset");
    tCheckBox.setColumnName("t_subset");
    tCheckBox.setDataSet(dataModule11.getQueryDatasetOptions());
    tCheckBox.addActionListener(new SubBrowsePanel_tCheckBox_actionAdapter(this));
    tCheckBox.setBackground(thisTabColor);
    tCheckBox.setMaximumSize(new Dimension(95, 25));
    tCheckBox.setMinimumSize(new Dimension(75, 25));
    tCheckBox.setPreferredSize(new Dimension(120, 22));
    extractLabel.setText("extract_prog");
    extractLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    extractLabel.setPreferredSize(new Dimension(160, 17));
    selectProgTextField.setPreferredSize(new Dimension(400, 21));
    selectProgTextField.setColumnName("order_select_prog");
    selectProgTextField.setDataSet(dataModule11.getQueryDatasetOptions());
    yCheckBox.setBackground(thisTabColor);
    yCheckBox.setPreferredSize(new Dimension(120, 22));
    yCheckBox.setText("y_subset");
    yCheckBox.setColumnName("y_subset");
    yCheckBox.setDataSet(dataModule11.getQueryDatasetOptions());
    idListLabel.setPreferredSize(new Dimension(160, 17));
    idListLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    idListLabel.setText("stnid_list_prog");
    subProgsPanel.setBackground(thisTabColor);
    subProgsPanel.setBorder(titledBorder2);
    subProgsPanel.setPreferredSize(new Dimension(400, 160));
    subProgsPanel.setLayout(xYLayout1);

    fgrLabel.setPreferredSize(new Dimension(160, 17));
    fgrLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    fgrLabel.setText("fgr_prog");
    fgrProgTextField.setPreferredSize(new Dimension(400, 21));
    fgrProgTextField.setColumns(0);
    fgrProgTextField.setColumnName("order_fgr_prog");
    fgrProgTextField.setDataSet(dataModule11.getQueryDatasetOptions());
    titledBorder3.setTitle("format conversions");
    orderOptionsPanel.setBackground(thisTabColor);
    orderOptionsPanel.setAlignmentX((float) 0.5);
    orderOptionsPanel.setBorder(titledBorder5);
    orderOptionsPanel.setDebugGraphicsOptions(0);
    orderOptionsPanel.setPreferredSize(new Dimension(100, 100));
    orderOptionsPanel.setLayout(xYLayout3);
    compressCheckBox.setColumnName("order_allow_compress");
    compressCheckBox.setDataSet(dataModule11.getQueryDatasetOptions());
    compressCheckBox.setText("allow compress");
    compressCheckBox.setBackground(thisTabColor);
    compressCheckBox.setPreferredSize(new Dimension(69, 22));
    compressCheckBox.setToolTipText("");
    compressCheckBox.setHorizontalAlignment(SwingConstants.LEFT);
    compressCheckBox.setSelected(true);
    dsOptionsTable.setOpaque(false);
    dsOptionsTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
    dsOptionsTable.setDataSet(dataModule11.getQueryDatasetOptions());
    dsOptionsTable.setHiddenColumns(new int[] { 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20 });
    tableScrollPane1.getViewport().setBackground(new Color(244, 223, 223));
    tableScrollPane1.setOpaque(true);
    listFilenamesCheckBox.setSelected(true);
    listFilenamesCheckBox.setHorizontalAlignment(SwingConstants.LEFT);
    listFilenamesCheckBox.setToolTipText("");
    listFilenamesCheckBox.setFocusPainted(true);
    listFilenamesCheckBox.setPreferredSize(new Dimension(69, 22));
    listFilenamesCheckBox.setBackground(thisTabColor);
    listFilenamesCheckBox.setText("allow picking from list of filenames");
    listFilenamesCheckBox.setDataSet(dataModule11.getQueryDatasetOptions());
    listFilenamesCheckBox.setColumnName("file_subset");
    dirLevelsTextField.setSelectionStart(0);
    dirLevelsTextField.setText("");
    dirLevelsTextField.setColumnName("order_directory_levels");
    dirLevelsTextField.setDataSet(dataModule11.getQueryDatasetOptions());
    dirLevelsLabel1.setBackground(Color.white);
    dirLevelsLabel1.setOpaque(false);
    dirLevelsLabel1.setHorizontalAlignment(SwingConstants.LEFT);
    dirLevelsLabel1.setText("how many components of the directory path");
    dirLevelsLabel2.setText("    to include when creating unique filenames");
    dirLevelsLabel2.setHorizontalAlignment(SwingConstants.LEFT);
    dirLevelsLabel2.setBackground(Color.white);
    dirLevelsLabel2.setOpaque(false);
    maxSizeTextField.setDataSet(dataModule11.getQueryDatasetOptions());
    maxSizeTextField.setColumnName("order_max_size_gb");
    maxSizeTextField.setSelectionStart(0);
    maxSizeTextField.setText("");
    maxSizeLabel.setOpaque(false);
    maxSizeLabel.setBackground(Color.white);
    maxSizeLabel.setHorizontalAlignment(SwingConstants.LEFT);
    maxSizeLabel.setText("max. size of data order in GB");
    eventCheckBox.setBackground(new Color(244, 223, 223));
    eventCheckBox.setText("subset data order by the \"event\" string");
    eventCheckBox.setColumnName("event_subset");
    eventCheckBox.setDataSet(dataModule11.getQueryDatasetOptions());
    idTextField.setBackground(new Color(244, 223, 223));
    idTextField.setBorder(BorderFactory.createEtchedBorder());
    idTextField.setToolTipText("Click here and insert Dataset ID, hit Enter");
    idTextField.setColumnName("dataset_id");
    idTextField.setDataSet(dataModule11.getQueryDatasetOptions());
    idTextField.setText("");
    this.add(subProgsPanel, BorderLayout.NORTH);
    subProgsPanel.add(selectProgLabel3,  new XYConstraints(47, 6, -1, -1));
    subProgsPanel.add(selectProgTextField,  new XYConstraints(232, 4, -1, -1));
    subProgsPanel.add(mergeProgLabel,  new XYConstraints(47, 31, -1, -1));
    subProgsPanel.add(mergeProgTextField,  new XYConstraints(232, 29, -1, -1));
    subProgsPanel.add(parmListLabel,  new XYConstraints(47, 56, -1, -1));
    subProgsPanel.add(parmListTextField,  new XYConstraints(232, 54, -1, -1));
    subProgsPanel.add(idListLabel,  new XYConstraints(47, 81, -1, -1));
    subProgsPanel.add(idListTextField,  new XYConstraints(232, 79, -1, -1));
    subProgsPanel.add(fgrLabel,    new XYConstraints(47, 106, -1, -1));
    subProgsPanel.add(fgrProgTextField,  new XYConstraints(232, 104, -1, -1));
    subProgsPanel.add(idTextField,       new XYConstraints(11, 5, 107, -1));

    this.add(browseProgsPanel,  BorderLayout.SOUTH);
    browseProgsPanel.add(stnScanLabel,  new XYConstraints(58, 20, 145, -1));
    browseProgsPanel.add(stnScanTextField,    new XYConstraints(215, 20, 528, -1));
    browseProgsPanel.add(extractTextField,    new XYConstraints(215, 53, 528, -1));
    browseProgsPanel.add(paramProgLabel, new XYConstraints(58, 85, 145, -1));
    browseProgsPanel.add(paramProgTextField,    new XYConstraints(215, 86, 528, -1));
    browseProgsPanel.add(extractLabel, new XYConstraints(58, 52, 145, -1));
    browseProgsPanel.add(tableScrollPane1, new XYConstraints(40, 126, 703, 67));
    tableScrollPane1.getViewport().add(dsOptionsTable, null);


    this.add(subOptionsPanel, BorderLayout.WEST);
    subOptionsPanel.add(xCheckBox, null);
    subOptionsPanel.add(yCheckBox, null);
    subOptionsPanel.add(zCheckBox, null);
    subOptionsPanel.add(tCheckBox, null);
    subOptionsPanel.add(pCheckBox, null);
    subOptionsPanel.add(idCheckBox, null);
    orderOptionsPanel.add(compressCheckBox,    new XYConstraints(50, 6, 319, -1));
    orderOptionsPanel.add(dirLevelsTextField,               new XYConstraints(53, 90, 34, -1));
    this.add(orderOptionsPanel, BorderLayout.CENTER);
    orderOptionsPanel.add(dirLevelsLabel1,     new XYConstraints(92, 85, 292, 18));
    orderOptionsPanel.add(dirLevelsLabel2,                    new XYConstraints(92, 100, 294, 17));
    orderOptionsPanel.add(maxSizeLabel,    new XYConstraints(94, 124, 294, 17));
    orderOptionsPanel.add(eventCheckBox,           new XYConstraints(51, 34, 382, 20));
    orderOptionsPanel.add(listFilenamesCheckBox,     new XYConstraints(51, 60, 319, -1));
    orderOptionsPanel.add(maxSizeTextField, new XYConstraints(53, 122, 35, -1));

  }

  public static Color getTabColor() {
    return thisTabColor;
  }

  void tCheckBox_actionPerformed(ActionEvent e) {

  }

}

class SubBrowsePanel_tCheckBox_actionAdapter implements java.awt.event.ActionListener {
  SubBrowsePanel adaptee;

  SubBrowsePanel_tCheckBox_actionAdapter(SubBrowsePanel adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.tCheckBox_actionPerformed(e);
  }
}
