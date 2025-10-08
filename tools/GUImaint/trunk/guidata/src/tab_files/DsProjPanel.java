package tab_files;

import java.awt.*;
import com.borland.jbcl.layout.*;
import com.borland.dbswing.*;
import javax.swing.*;

/**
 * <p>Title: GUImaint for CODIAC</p>
 * <p>Description: mySQL db version</p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Company: UCAR/JOSS</p>
 * @author Don Stott
 * @version 1.5
 */

public class DsProjPanel extends JPanel {
  BorderLayout borderLayout1 = new BorderLayout();
  static Color tabPurple = new Color(240, 234, 242);
  static Color thisTabColor = tabPurple;
  DataModule1 dataModule11;
  BorderLayout borderLayout2 = new BorderLayout();
  XYLayout xYLayout2 = new XYLayout();
  JPanel jPanel1 = new JPanel();
  JdbTable dsprojTable = new JdbTable();
  TableScrollPane dsProjScrollPane = new TableScrollPane();
  XYLayout xYLayout3 = new XYLayout();
  XYLayout xYLayout1 = new XYLayout();

  public DsProjPanel() {
    try {
      jbInit();
    }
    catch(Exception ex) {
      ex.printStackTrace();
    }
  }
  void jbInit() throws Exception {
    dataModule11 = tab_files.DataModule1.getDataModule();
    this.setLayout(xYLayout1);
    this.setBackground(thisTabColor);
    this.setPreferredSize(new Dimension(760, 1100));
    jPanel1.setBackground(new Color(240, 234, 242));
    jPanel1.setBorder(BorderFactory.createEtchedBorder());
    jPanel1.setLayout(xYLayout3);
    dsprojTable.setBackground(Color.white);
    dsprojTable.setBorder(null);
    dsprojTable.setAutoCreateColumnsFromModel(true);
    dsprojTable.setAutoResizeMode(JTable.AUTO_RESIZE_SUBSEQUENT_COLUMNS);
    dsprojTable.setRowMargin(3);
    dsprojTable.setDataSet(dataModule11.getQueryDatasetProj());
//    dsprojTable.setHiddenColumns(new int[] { 2 });
    dsprojTable.setPopupMenuEnabled(true);
    dsprojTable.setRowHeader(null);
    dsprojTable.setRowHeaderVisible(false);
    dsProjScrollPane.setRowHeader(null);
    dsProjScrollPane.getViewport().setBackground(Color.white);
    dsProjScrollPane.setOpaque(false);
    dsProjScrollPane.setPreferredSize(new Dimension(467, 419));
    dsProjScrollPane.setRequestFocusEnabled(true);
    this.add(jPanel1,  new XYConstraints(38, 21, 719, 315));
    jPanel1.add(dsProjScrollPane,    new XYConstraints(119, 33, -1, 232));
    dsProjScrollPane.add(dsprojTable, null);
  }

  public static Color getTabColor() {
    return thisTabColor;
  }

}
