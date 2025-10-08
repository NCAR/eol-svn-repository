package tab_files;

import java.awt.*;
import javax.swing.*;

/**
 * <p>Title: GUImaint for CODIAC</p>
 * <p>Description: MySQL db version</p>
 * <p>Copyright: Copyright (c) 2004, 2005, 2006, 2007</p>
 * <p>Company: NCAR/EOL</p>
 * @author Don Stott
 * @version 2.5- test
 */

public class SearchTabs extends JPanel {
  static JTabbedPane topTabPane = new JTabbedPane();
  GridBagLayout gridBagLayout1 = new GridBagLayout();
  static Color tabGray = new Color(230, 225, 215);
  static Color thisTabColor = tabGray;

  public SearchTabs() {
    try {
      jbInit();
    }
    catch (Exception e) {
      e.printStackTrace();
    }
  }

  private void jbInit() throws Exception {
    this.setLayout(gridBagLayout1);
    this.setBackground(thisTabColor);
    this.setPreferredSize(new Dimension(760, 1100));
    topTabPane.setBackground(thisTabColor);
    this.add(topTabPane, new GridBagConstraints(0, 0, 1, 1, 1.0, 1.0
            ,GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(8, 8, 0, 8), 0, 0));
    topTabPane.add(new SearchDatasetPanel(), "Choose Dataset");            // index = 0
  }
}


