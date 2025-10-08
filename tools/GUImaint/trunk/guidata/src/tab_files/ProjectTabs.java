package tab_files;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;

/**
 * <p>Title: GUImaint for CODIAC</p>
 * <p>Description: MySQL db version</p>
 * <p>Copyright: Copyright (c) 2004, 2005, 2006, 2007</p>
 * <p>Company: NCAR/EOL</p>
 * @author Don Stott
 * @version 2.5- test
 */

public class ProjectTabs extends JPanel {
  static JTabbedPane topTabPane = new JTabbedPane();
  static Color tabGray = new Color(230, 225, 215);
  static Color thisTabColor = tabGray;
  GridBagLayout gridBagLayout1 = new GridBagLayout();
  DataModule1 dataModule11;
  int proj_tab = 0;
  int xlink_tab = 1;
  int cat_tab = 2;

  public ProjectTabs() {
    try {
      jbInit();
    }
    catch (Exception e) {
      e.printStackTrace();
    }
  }

  private void jbInit() throws Exception {
    dataModule11 = tab_files.DataModule1.getDataModule();
    this.setLayout(gridBagLayout1);
    this.setBackground(thisTabColor);
    this.setPreferredSize(new Dimension(760, 1600));
    topTabPane.setBackground(thisTabColor);
    this.add(topTabPane, new GridBagConstraints(0, 0, 1, 1, 1.0, 1.0
            ,GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(8, 8, 0, 8), 0, 0));
    topTabPane.add(new ProjectTabPanel(), "Project");             // index = 0 tabBlue
    proj_tab = 0;
    topTabPane.add(new ProjXlinkPanel(), "Ext. Links");           // index = 1 tabLilac
    xlink_tab = 1;
    topTabPane.setBackgroundAt(proj_tab, DatasetTabPanel.getTabColor());
    topTabPane.setBackgroundAt(xlink_tab, queryPanel.getTabColor());
 }


}


