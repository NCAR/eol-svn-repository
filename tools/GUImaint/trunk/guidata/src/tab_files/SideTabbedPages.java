package tab_files;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;

import com.borland.dbswing.*;
import com.borland.jbcl.layout.*;
import java.beans.*;

/**
 * <p>Title: GUImaint for CODIAC</p>
 * <p>Description: MySQL db version</p>
 * <p>Copyright: Copyright (c) 2004, 2005, 2006, 2007</p>
 * <p>Company: NCAR/EOL</p>
 * @author Don Stott
 * @version 2.5- test
 */

public class SideTabbedPages extends JPanel {
  static JTabbedPane sideTabPane = new JTabbedPane();
  BorderLayout borderLayout1 = new BorderLayout();
  ImageIcon icon = new ImageIcon("middle.gif");
  ImageIcon searchIcon = new ImageIcon("search24.gif");
  static Color tabGray = new Color(230, 225, 215);
  static Color thisTabColor = tabGray;

  // -------------------------------------------------------------------
  // top panel for ID and project names
  // -------------------------------------------------------------------
  JPanel idPanel = new JPanel();
  XYLayout xYLayout1 = new XYLayout();
  JLabel idLabel = new JLabel();
  static JdbTextField idTextField = new JdbTextField();
  static JdbTextField titleTextField = new JdbTextField();
  JLabel projLabel = new JLabel();
  static boolean headerHidden = false;
  static boolean dittoMade = false;
  JdbNavComboBox projNavComboBox = new JdbNavComboBox();
  JLabel titleLabel = new JLabel();

//--------------------------------
// data modules of databases used
//--------------------------------
  DataModule1 dataModule_codiac;
//  DataModule_DTS dataModule_dts;

//--------------------------------
// order of left side tabs; must
// match order when added below
//--------------------------------
  int start_sidetab = 0;
  int search_sidetab = 1;
  int ds_sidetab = 2;
  int pswd_sidetab = 3;
  int proj_sidetab = 4;
  static int dts_sidetab = 5;

  public SideTabbedPages() {
    try {
      jbInit();
    }
    catch (Exception e) {
      e.printStackTrace();
    }
  }

  private void jbInit() throws Exception {
    dataModule_codiac = tab_files.DataModule1.getDataModule();
//    dataModule_dts = tab_files.DataModule_DTS.getDataModule();
    this.setLayout(borderLayout1);
    sideTabPane.setTabPlacement(JTabbedPane.LEFT);
    sideTabPane.setBackground(thisTabColor);

    // add all tab panels; order determines placement of tab
    this.setBackground(thisTabColor);
    this.setPreferredSize(new Dimension(780, 700));

    sideTabPane.addChangeListener(new javax.swing.event.ChangeListener() {
      public void stateChanged(ChangeEvent ce) {
        this_changeEvent(ce);
      }
    });

    sideTabPane.addTab("Log in", icon, new StartTabPanel());          // 0
    sideTabPane.addTab("Choose", searchIcon, new SearchTabs());       // 1
    sideTabPane.addTab("Dataset", new DatasetTabs());                 // 2
    sideTabPane.addTab("Password", new PasswordTabPanel());           // 3
    sideTabPane.addTab("Project", new ProjectTabs());                 // 4
    sideTabPane.addTab("DTS", new dtsTabs());

// Dataset ID and name in header
    idPanel.setBackground(thisTabColor);
    idPanel.setBorder(BorderFactory.createRaisedBevelBorder());
    idPanel.setMinimumSize(new Dimension(50, 50));
    idPanel.setPreferredSize(new Dimension(60, 40));
    idPanel.setLayout(xYLayout1);
    titleTextField.setEditable(false);
    titleTextField.setColumnName("name");
    titleTextField.setDataSet(dataModule_codiac.getQueryDataset1());
    titleTextField.setBackground(new Color(255, 255, 225));
    titleTextField.setVisible(false);
    titleTextField.setAutoscrolls(true);
    titleTextField.setPreferredSize(new Dimension(550, 21));
    titleTextField.setRequestFocusEnabled(false);
    titleTextField.setText("XXXX xxxxxx xxxxxxx xxxxxxx");
    idLabel.setText("Dataset ID:");
    idLabel.setBackground(new Color(230, 225, 215));
    idLabel.setForeground(Color.black);
    idLabel.setVisible(false);
    idLabel.setOpaque(true);
    idLabel.setHorizontalTextPosition(SwingConstants.LEFT);
    idTextField.setColumnName("dataset_id");
    idTextField.setDataSet(dataModule_codiac.getQueryDataset1());
    idTextField.setBackground(new java.awt.Color(255, 255, 225));
    idTextField.setVisible(false);
    idTextField.setMaximumSize(new Dimension(20, 21));
    idTextField.setRequestFocusEnabled(false);
    idTextField.setEditable(false);

    projLabel.setBackground(thisTabColor);
    projLabel.setOpaque(true);
    projLabel.setHorizontalAlignment(SwingConstants.RIGHT);
    projLabel.setText("Project:");
    projNavComboBox.setVisible(false);
    projNavComboBox.setColumnName("project_id");
    projNavComboBox.setDataSet(dataModule_codiac.getQueryProjName());
    projNavComboBox.addItemListener(new java.awt.event.ItemListener() {
      public void itemStateChanged(ItemEvent e) {
        projNavComboBox_itemStateChanged(e);
      }
    });
    titleLabel.setBackground(thisTabColor);
    titleLabel.setVisible(false);
    titleLabel.setOpaque(true);
    titleLabel.setText("Title:");
    this.add(sideTabPane, BorderLayout.CENTER);
    this.add(idPanel, BorderLayout.NORTH);
    idPanel.add(projLabel,  new XYConstraints(231, 8, 78, 21));
    idPanel.add(idTextField,        new XYConstraints(90, 5, 127, 23));
    idPanel.add(idLabel,  new XYConstraints(24, 9, -1, -1));
    idPanel.add(projNavComboBox,    new XYConstraints(313, 6, 164, 23));
    idPanel.add(titleLabel,             new XYConstraints(230, 10, 34, -1));
    idPanel.add(titleTextField,        new XYConstraints(269, 7, 610, -1));
    headerHidden = true;

    // dts starts out disabled, but can be enabled through menu
    sideTabPane.setEnabledAt(5, false);

    // set Choose tab to blue, so it stands out
    sideTabPane.setForegroundAt(1, Color.blue);
    sideTabPane.setSelectedIndex(0);
  }

 public void setIndex(int ndx) {
    sideTabPane.setSelectedIndex(ndx);
 }

  // hide dataset_id, title when on project page
  // and project_id when on other pages
  void this_changeEvent(ChangeEvent ce) {
    dataModule_codiac.paramRowProjID.setString("proj_id", dataModule_codiac.queryDatasetProj.getString("project_id"));
    if (sideTabPane.getSelectedIndex() == start_sidetab) {
//    start page, hide all in header
      projLabel.setVisible(false);
      projNavComboBox.setVisible(false);
      idLabel.setVisible(false);
      idTextField.setVisible(false);
      titleLabel.setVisible(false);
      titleTextField.setVisible(false);
      headerHidden = true;
    }
    else if (sideTabPane.getSelectedIndex() == search_sidetab) {
//    search page, show ID, title in header
      projLabel.setVisible(false);
      projNavComboBox.setVisible(false);
      idLabel.setVisible(true);
      idTextField.setVisible(true);
      SearchDatasetPanel.selectIDField();
      titleLabel.setVisible(true);
      titleTextField.setVisible(true);
      headerHidden = false;
    }
    else if (sideTabPane.getSelectedIndex() == ds_sidetab) {
//    dataset page, show ID, title in header
      projLabel.setVisible(false);
      projNavComboBox.setVisible(false);
      idLabel.setVisible(true);
      idTextField.setVisible(true);
      titleLabel.setVisible(true);
      titleTextField.setVisible(true);
      headerHidden = false;
      SearchDatasetPanel.setResultsEdit(true);
      if (dittoMade == true) {
         SideTabbedPages.idTextField.setRequestFocusEnabled(true);
         SideTabbedPages.idTextField.setEditable(true);
         SideTabbedPages.titleTextField.setRequestFocusEnabled(true);
         SideTabbedPages.titleTextField.setEditable(true);
      }
    }
    else if (sideTabPane.getSelectedIndex() == pswd_sidetab) {
//    password page, show ID, title in header
      dataModule_codiac.queryDSUser.refresh();
      dataModule_codiac.queryPassword.refresh();
      idLabel.setVisible(true);
      idTextField.setVisible(true);
      titleLabel.setVisible(true);
      titleTextField.setVisible(true);
      projLabel.setVisible(false);
      projNavComboBox.setVisible(false);
      headerHidden = true;
    }
    else if (sideTabPane.getSelectedIndex() == proj_sidetab) {
//    project page, show project drop down box in header
      dataModule_codiac.queryProject.executeQuery();
      dataModule_codiac.queryDatasetProj.refresh();
      dataModule_codiac.queryThisPrefix.refresh();
      idLabel.setVisible(false);
      idTextField.setVisible(false);
      titleLabel.setVisible(false);
      titleTextField.setVisible(false);
      projLabel.setVisible(true);
      projNavComboBox.setVisible(true);
      headerHidden = true;
      projNavComboBox.setSelectedItem(dataModule_codiac.paramRowProjID.getString("proj_id"));
      dataModule_codiac.queryProjDataset.refresh();
      dataModule_codiac.queryProjXlink.refresh();
    }
    else if (sideTabPane.getSelectedIndex() == dts_sidetab) {
//    DTS page
//      dataModule_codiac.dts_dataset.executeQuery();
    }
    else if (headerHidden) {
      projLabel.setVisible(false);
      projNavComboBox.setVisible(false);
      idLabel.setVisible(true);
      idTextField.setVisible(true);
      titleLabel.setVisible(true);
      titleTextField.setVisible(true);
      headerHidden = false;
    }
  }

  void projNavComboBox_itemStateChanged(ItemEvent e) {
    try {
      if (projNavComboBox.getSelectedIndex() != -1) {
        if (!dataModule_codiac.paramRowProjID.getString("proj_id").equals(projNavComboBox.getSelectedItem().toString())) {
          dataModule_codiac.paramRowProjID.setString("proj_id", projNavComboBox.getSelectedItem().toString());
          dataModule_codiac.queryProject.refresh();
          dataModule_codiac.queryProjDataset.refresh();
          dataModule_codiac.queryThisPrefix.refresh();
          dataModule_codiac.queryProjXlink.refresh();        }
      } else {
        System.out.println("Using " + dataModule_codiac.paramRowProjID.getString("proj_id") + " at SideTabbedPages");
      }
    }
    catch (Exception ex){
      ex.printStackTrace();
    }
  }

  public static void setDitto(boolean ans) {
    dittoMade = ans;
  }

}

