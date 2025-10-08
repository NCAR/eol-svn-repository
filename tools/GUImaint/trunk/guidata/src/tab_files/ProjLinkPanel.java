package tab_files;

import java.awt.*;
import javax.swing.*;
import com.borland.jbcl.layout.*;
import com.borland.dbswing.*;

/**
 * <p>Title: GUI only test</p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Company: </p>
 * @author not attributable
 * @version 1.0
 */

public class ProjLinkPanel extends JPanel {
  JPanel linksPanel = new JPanel();
  XYLayout xYLayout1 = new XYLayout();
  DataModule1 dataModule11;
  static Color tabPurple = new Color(240, 234, 242);
//  static Color tabPurple = new Color(240, 231, 248);
//  static Color tabPurple = new Color(230, 209, 250);
//  static Color thisTabColor = new Color(235, 255, 255);
  static Color thisTabColor = tabPurple;
  JdbTable dsProjTable = new JdbTable();
  TableScrollPane linkTableScrollPane = new TableScrollPane();

  public ProjLinkPanel() {
    try {
      jbInit();
    }
    catch(Exception ex) {
      ex.printStackTrace();
    }
  }
  void jbInit() throws Exception {
    dataModule11 = tab_files.DataModule1.getDataModule();
    linksPanel.setBackground(tabPurple);
    linksPanel.setBorder(BorderFactory.createRaisedBevelBorder());
    linksPanel.setPreferredSize(new Dimension(472, 424));
    this.setLayout(xYLayout1);
    xYLayout1.setWidth(691);
    xYLayout1.setHeight(697);
    dsProjTable.setDataSet(dataModule11.getQueryDatasetProj());
    dsProjTable.setSmartColumnWidths(false);
    dsProjTable.setHiddenColumns(new int[] { 2 });
    linkTableScrollPane.getViewport().setBackground(new Color(240, 234, 242));
    linkTableScrollPane.setPreferredSize(new Dimension(330, 250));
    this.setBackground(tabPurple);
    this.add(linksPanel, new XYConstraints(-2, 32, 692, 389));
    linksPanel.add(linkTableScrollPane, null);
    linkTableScrollPane.add(dsProjTable, null);
  }

  public static Color getTabColor() {
    return thisTabColor;
  }

}