package tab_files;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import com.borland.javax.sql.*;

/**
 * <p>Title: GUImaint for CODIAC</p>
 * <p>Description: MySQL db version</p>
 * <p>Copyright: Copyright (c) 2004, 2005, 2006, 2007</p>
 * <p>Company: NCAR/EOL</p>
 * @author Don Stott
 * @version 2.5- test
 */

public class dtsTabs extends JPanel {
  static JTabbedPane topTabPane = new JTabbedPane();
  static Color tabGray = new Color(230, 225, 215);
  static Color thisTabColor = tabGray;
  GridBagLayout gridBagLayout1 = new GridBagLayout();
  DataModule1 dataModule11;
  int dtsPanel_tab = 0;
  int mlPanel_tab = 1;
  int dtsProject_tab = 2;
  JdbcDataSource jdbcDataSource1 = new JdbcDataSource();
  DataModule1 dataModule_codiac;

  public dtsTabs() {
    try {
      jbInit();
    }
    catch (Exception e) {
      e.printStackTrace();
    }
  }

  private void jbInit() throws Exception {
    dataModule_codiac = tab_files.DataModule1.getDataModule();
    this.setLayout(gridBagLayout1);
    this.setBackground(thisTabColor);
    this.setPreferredSize(new Dimension(760, 1600));
    topTabPane.setBackground(thisTabColor);
    this.add(topTabPane, new GridBagConstraints(0, 0, 1, 1, 1.0, 1.0
            ,GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(8, 8, 0, 8), 0, 0));
//    topTabPane.add(new dtsPanel(), "DTS");                             // index = 0 tabBlue
//    topTabPane.add(new mlPanel(), "ML");                               // index = 1 tabLilac
//    topTabPane.add(new dtsProjPanel(), "DTS Project");                 // index = 2 tabBlue
//    topTabPane.setBackgroundAt(dtsPanel_tab, dtsPanel.getTabColor());
//    topTabPane.setBackgroundAt(mlPanel_tab, mlPanel.getTabColor());
 }


}


