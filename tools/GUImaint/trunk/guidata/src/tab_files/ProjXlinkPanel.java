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

public class ProjXlinkPanel extends JPanel {
  BorderLayout borderLayout1 = new BorderLayout();
  static Color tabPink = new Color(255, 215, 215);
  static Color thisTabColor = tabPink;
  DataModule1 dataModule11;
  JPanel xlinkPanel = new JPanel();
  TitledBorder titledBorder2;
  XYLayout xYLayout2 = new XYLayout();
  JdbTextField projIDTextField = new JdbTextField();
  TableScrollPane xlinkScrollPane = new TableScrollPane();
  JdbTable xlinkTable = new JdbTable();
  Border border1;
  TitledBorder titledBorder1;

  public ProjXlinkPanel() {
    try {
      jbInit();
    }
    catch(Exception ex) {
      ex.printStackTrace();
    }
  }
  void jbInit() throws Exception {
    dataModule11 = tab_files.DataModule1.getDataModule();
    titledBorder2 = new TitledBorder(BorderFactory.createEtchedBorder(Color.white,new Color(135, 149, 161)),"External Links for this Project");
    border1 = BorderFactory.createEtchedBorder(Color.white,new Color(135, 149, 161));
    titledBorder1 = new TitledBorder(border1,"External Links for this Project");
    this.setMinimumSize(new Dimension(800, 700));
    this.setPreferredSize(new Dimension(800, 700));
    this.setLayout(borderLayout1);
    xlinkPanel.setBackground(new Color(193, 214, 231));
    xlinkPanel.setBorder(titledBorder1);
    xlinkPanel.setLayout(xYLayout2);
    titledBorder2.setTitle("External Links");
    titledBorder2.setBorder(BorderFactory.createEtchedBorder());
    titledBorder2.setTitleColor(Color.black);
    projIDTextField.setDataSet(dataModule11.getQueryProjXlink());
    projIDTextField.setColumnName("project_id");
    projIDTextField.setText("");
    projIDTextField.setHorizontalAlignment(SwingConstants.CENTER);
    projIDTextField.setBackground(new Color(193, 214, 231));
    projIDTextField.setFont(new java.awt.Font("Dialog", 1, 11));
    xlinkTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
    xlinkTable.setSelectionBackground(new Color(200, 200, 200));
    xlinkTable.setDataSet(dataModule11.getQueryProjXlink());
    xlinkTable.setHiddenColumns(new int[] { 2 });
    xlinkScrollPane.getViewport().setBackground(new Color(193, 214, 231));
    this.add(xlinkPanel,  BorderLayout.CENTER);
    xlinkPanel.add(xlinkScrollPane,        new XYConstraints(141, 117, 565, 145));
    xlinkPanel.add(projIDTextField,   new XYConstraints(17, 116, 105, 19));
    xlinkScrollPane.getViewport().add(xlinkTable, null);
  }

  public static Color getTabColor() {
    return thisTabColor;
  }

}

