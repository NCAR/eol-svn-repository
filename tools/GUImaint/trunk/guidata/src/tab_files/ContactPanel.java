package tab_files;

import java.awt.*;
import javax.swing.*;

import com.borland.dbswing.*;
import com.borland.jbcl.layout.*;
import javax.swing.border.*;

/**
 * <p>Title: GUImaint for CODIAC</p>
 * <p>Description: MySQL db version</p>
 * <p>Copyright: Copyright (c) 2004, 2005, 2006, 2007</p>
 * <p>Company: NCAR/EOL</p>
 * @author Don Stott
 * @version 2.5- test
 */

public class ContactPanel extends JPanel {
  BorderLayout borderLayout1 = new BorderLayout();
  TableScrollPane tableScrollPane1 = new TableScrollPane();
  JdbTable contactTable = new JdbTable();
  DataModule1 dataModule11;
  static Color tabGenta = new Color(220, 195, 200);
  static Color thisTabColor = tabGenta;
  static Color tabPurple = new Color(240, 234, 242);
  JPanel jPanel1 = new JPanel();
  XYLayout xYLayout1 = new XYLayout();
  JdbLabel addrLabel = new JdbLabel();
  TableScrollPane addrScrollPane = new TableScrollPane();
  JdbTextArea addressTextArea = new JdbTextArea();
  JPanel piPanel = new JPanel();
  XYLayout xYLayout2 = new XYLayout();
  JLabel piLabel = new JLabel();
  JLabel piTableLabel = new JLabel();
  JdbTable piTable = new JdbTable();
  TitledBorder titledBorder1;
  JTextField jTextField1 = new JTextField();

  public ContactPanel() {
    try {
      jbInit();
    }
    catch(Exception ex) {
      ex.printStackTrace();
    }
  }
  void jbInit() throws Exception {
    titledBorder1 = new TitledBorder(new EtchedBorder(EtchedBorder.RAISED,Color.white,new Color(154, 136, 140)),"Dataset PIs");
    this.setLayout(borderLayout1);
    dataModule11 = tab_files.DataModule1.getDataModule();
    contactTable.setDataSet(dataModule11.getQueryContact());
    contactTable.setAutoCreateColumnsFromModel(true);
    contactTable.setCellSelectionEnabled(true);
    contactTable.setRowSelectionAllowed(false);
    tableScrollPane1.getViewport().setBackground(thisTabColor);
    tableScrollPane1.setOpaque(true);
    tableScrollPane1.setPreferredSize(new Dimension(472, 275));
    this.setBackground(thisTabColor);
    this.setOpaque(true);
    this.setPreferredSize(new Dimension(300, 300));
    jPanel1.setLayout(xYLayout1);
    addrLabel.setFont(new java.awt.Font("Dialog", 1, 11));
    addrLabel.setForeground(Color.black);
    addrLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    addrLabel.setText("Edit Address:");
    jPanel1.setBackground(thisTabColor);
    jPanel1.setMinimumSize(new Dimension(532, 230));
    jPanel1.setOpaque(true);
    jPanel1.setPreferredSize(new Dimension(532, 300));
    addressTextArea.setBackground(tabPurple);
    addressTextArea.setBorder(BorderFactory.createLoweredBevelBorder());
    addressTextArea.setText("address");
    addressTextArea.setLineWrap(false);
    addressTextArea.setWrapStyleWord(false);
    addressTextArea.setColumnName("address");
    addressTextArea.setDataSet(dataModule11.getQueryContact());
    piPanel.setLayout(xYLayout2);
    piLabel.setMaximumSize(new Dimension(55, 17));
    piLabel.setPreferredSize(new Dimension(60, 17));
    piLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    piLabel.setText("Data set PI:");
    piTableLabel.setBackground(Color.white);
    piTableLabel.setForeground(Color.black);
    piTableLabel.setOpaque(false);
    piTableLabel.setHorizontalTextPosition(SwingConstants.LEFT);
    piTableLabel.setText("     dataset                PI name  ");
    piTable.setBackground(tabPurple);
    piTable.setBorder(BorderFactory.createLineBorder(Color.black));
    piTable.setAutoResizeMode(JTable.AUTO_RESIZE_NEXT_COLUMN);
    piTable.setDataSet(dataModule11.getQueryDatasetPI());
    piTable.setHiddenColumns(new int[] { 2 });
    piPanel.setBackground(new Color(235, 210, 215));
    piPanel.setAlignmentY((float) 0.5);
    piPanel.setBorder(BorderFactory.createEtchedBorder());
    jTextField1.setBackground(new Color(235, 210, 215));
    jTextField1.setBorder(null);
    jTextField1.setText("(This table is separate from the Users\' table)");
    jTextField1.setHorizontalAlignment(SwingConstants.CENTER);
    this.add(tableScrollPane1,  BorderLayout.CENTER);
    this.add(jPanel1,  BorderLayout.NORTH);
    jPanel1.add(addrLabel, new XYConstraints(90, 181, 126, 18));
    jPanel1.add(addrScrollPane, new XYConstraints(222, 182, 308, 99));
    jPanel1.add(piPanel, new XYConstraints(130, 4, 501, 159));
    piPanel.add(piTable, new XYConstraints(132, 35, 230, 87));
    piPanel.add(piLabel, new XYConstraints(28, 34, 96, 23));
    piPanel.add(jTextField1, new XYConstraints(91, 126, 314, 18));
    piPanel.add(piTableLabel,       new XYConstraints(138, 12, 207, 18));
    addrScrollPane.getViewport().add(addressTextArea, null);
    tableScrollPane1.getViewport().add(contactTable, null);
    contactTable.setHiddenColumns(new int[] { 16 } );
  }

  public static Color getTabColor() {
    return thisTabColor;
  }

}