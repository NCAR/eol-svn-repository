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

public class PlotPanel extends JPanel {
  JPanel plotsPanel = new JPanel();
  DataModule1 dataModule11;
  TitledBorder titledBorder1;
  TitledBorder titledBorder2;
  Border border1;
  TitledBorder titledBorder3;
  JdbComboBox timeSelComboBox = new JdbComboBox();
  XYLayout xYLayout1 = new XYLayout();
  JdbComboBox stnSelComboBox = new JdbComboBox();
  JLabel plotTypeLabel = new JLabel();
  JLabel timeSelLabel = new JLabel();
  JLabel stnSelLabel = new JLabel();
  JdbComboBox plotTypeComboBox = new JdbComboBox();
  JdbStatusLabel jdbStatusLabel1 = new JdbStatusLabel();
  static Color tabLtYellow =  new Color(242, 242, 215);
  static Color thisTabColor = tabLtYellow;
  TableScrollPane plotScrollPane = new TableScrollPane();
  JdbTable plotTable = new JdbTable();
  TitledBorder titledBorder4;
  TitledBorder titledBorder5;
  TitledBorder titledBorder6;
  TitledBorder titledBorder7;
  TitledBorder titledBorder8;
  GridBagLayout gridBagLayout1 = new GridBagLayout();
  static JdbTextField idTextField = new JdbTextField();

  public PlotPanel() {
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
    titledBorder4 = new TitledBorder("");
    titledBorder5 = new TitledBorder("");
    titledBorder6 = new TitledBorder("");
    titledBorder7 = new TitledBorder("");
    titledBorder8 = new TitledBorder(BorderFactory.createLineBorder(new Color(153, 153, 153),2),"Plots");
    plotsPanel.setBackground(thisTabColor);
    plotsPanel.setBorder(titledBorder3);
    plotsPanel.setMaximumSize(new Dimension(2147483647, 2147483647));
    plotsPanel.setMinimumSize(new Dimension(557, 251));
    plotsPanel.setPreferredSize(new Dimension(762, 300));
    plotsPanel.setLayout(xYLayout1);
    this.setLayout(gridBagLayout1);
    timeSelComboBox.setBackground(Color.white);
    timeSelComboBox.setFont(new java.awt.Font("Dialog", 0, 12));
    timeSelComboBox.setPreferredSize(new Dimension(81, 16));
    timeSelComboBox.setColumnName("time_sel_level");
    timeSelComboBox.setDataSet(dataModule11.getQueryPlots());
    timeSelComboBox.setItems(new String[] {"sc", "mn", "hr", "dy", "mo", "yr", "na"});
    timeSelComboBox.addActionListener(new PlotPanel_timeSelComboBox_actionAdapter(this));
    stnSelComboBox.setItems(new String[] {"height", "flight_nomap", "single_nomap", "level", "single", "no"});
    stnSelComboBox.setDataSet(dataModule11.getQueryPlots());
    stnSelComboBox.setColumnName("stn_select");
    stnSelComboBox.setPreferredSize(new Dimension(81, 16));
    stnSelComboBox.setFont(new java.awt.Font("Dialog", 0, 12));
    stnSelComboBox.setBackground(Color.white);
    plotTypeLabel.setText("codiac_plot_type_id:");
    plotTypeLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    timeSelLabel.setText("time_sel_level:");
    timeSelLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    stnSelLabel.setText("stn_select:");
    stnSelLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    plotTypeComboBox.setBackground(Color.white);
    plotTypeComboBox.setFont(new java.awt.Font("Dialog", 0, 12));
    plotTypeComboBox.setPreferredSize(new Dimension(124, 20));
    plotTypeComboBox.setColumnName("codiac_plot_type_id");
    plotTypeComboBox.setDataSet(dataModule11.getQueryPlots());
    jdbStatusLabel1.setDataSet(dataModule11.getQueryPlots());
    jdbStatusLabel1.setText("Type Project name in ID textbox then hit Enter");
    jdbStatusLabel1.setPreferredSize(new Dimension(640, 18));
    jdbStatusLabel1.setMaximumSize(new Dimension(700, 18));
    jdbStatusLabel1.setBorder(BorderFactory.createRaisedBevelBorder());
    jdbStatusLabel1.setAutoscrolls(true);
    jdbStatusLabel1.setBackground(thisTabColor);
    jdbStatusLabel1.setAlignmentX((float) 0.5);
    this.setBackground(thisTabColor);
    this.setMinimumSize(new Dimension(378, 432));
    this.setPreferredSize(new Dimension(583, 695));
    plotTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
    plotTable.setRowSelectionAllowed(false);
    plotTable.setDataSet(dataModule11.getQueryPlots());
    plotTable.setHiddenColumns(new int[] { 4 });
    plotScrollPane.getViewport().setBackground(tabLtYellow);
    plotScrollPane.setPreferredSize(new Dimension(467, 240));
    idTextField.setDataSet(dataModule11.getQueryPlots());
    idTextField.setText("");
    idTextField.setBackground(new Color(242, 242, 215));
    idTextField.setBorder(BorderFactory.createEtchedBorder());
    idTextField.setColumnName("dataset_id");
    this.add(plotsPanel,  new GridBagConstraints(0, 0, 1, 1, 1.0, 1.0
            ,GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), -193, 0));
    plotsPanel.add(plotTypeComboBox,     new XYConstraints(242, 51, 174, 22));
    plotsPanel.add(plotTypeLabel, new XYConstraints(82, 54, 146, 17));
    plotsPanel.add(timeSelLabel,  new XYConstraints(107, 86, 121, 17));
    plotsPanel.add(timeSelComboBox,  new XYConstraints(242, 83, 131, 22));
    plotsPanel.add(stnSelComboBox,  new XYConstraints(241, 113, 131, 22));
    plotsPanel.add(stnSelLabel,  new XYConstraints(111, 116, 117, 17));
    plotsPanel.add(jdbStatusLabel1,     new XYConstraints(43, 207, -1, -1));
    plotsPanel.add(idTextField,                          new XYConstraints(37, 6, 100, -1));
    this.add(plotScrollPane,      new GridBagConstraints(0, 1, 1, 1, 1.0, 1.0
            ,GridBagConstraints.NORTH, GridBagConstraints.BOTH, new Insets(4, 0, 100, 0), 102, 0));
    plotScrollPane.getViewport().add(plotTable, null);
  }

  public static Color getTabColor() {
    return thisTabColor;
  }

  void timeSelComboBox_actionPerformed(ActionEvent e) {

  }

}

class PlotPanel_timeSelComboBox_actionAdapter implements java.awt.event.ActionListener {
  PlotPanel adaptee;

  PlotPanel_timeSelComboBox_actionAdapter(PlotPanel adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.timeSelComboBox_actionPerformed(e);
  }
}
