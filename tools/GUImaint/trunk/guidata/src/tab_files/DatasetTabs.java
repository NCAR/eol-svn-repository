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

public class DatasetTabs extends JPanel {
  static JTabbedPane topTabPane = new JTabbedPane();
  static Color tabGray = new Color(230, 225, 215);
  static Color thisTabColor = tabGray;
  GridBagLayout gridBagLayout1 = new GridBagLayout();
  DataModule1 dataModule11;
  int desc_tab = 0;
  int links_tab = 1;
  int cat_tab = 2;
  int doc_tab = 3;
  int file_tab = 4;
  int xlink_tab = 5;
  int browse_tab = 6;
  int format_tab = 7;
  int plot_tab = 8;
  int user_tab = 9;
  int tape_tab = 10;

  public DatasetTabs() {
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
    this.addComponentListener(new java.awt.event.ComponentAdapter() {
      public void componentShown(ComponentEvent e) {
        this_componentShown(e);
      }
    });
    topTabPane.addChangeListener(new javax.swing.event.ChangeListener() {
      public void stateChanged(ChangeEvent e) {
        topTabPane_stateChanged(e);
      }
    });
    topTabPane.setBackground(thisTabColor);
    this.add(topTabPane, new GridBagConstraints(0, 0, 1, 1, 1.0, 1.0
            ,GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(8, 8, 0, 8), 0, 0));
    topTabPane.add(new DatasetTabPanel(), "Description");         // index = 0 tabBlue
    desc_tab = 0;
    topTabPane.add(new queryPanel(), "References");               // index = 1 tabLilac
    links_tab = 1;
    topTabPane.add(new CategoryPanel(), "Cat/Plat");              // index = 2 tabLtBlue
    cat_tab = 2;
    topTabPane.add(new DocsPanel(), "Docs");                      // index = 3 tabGolden
    doc_tab = 3;
    topTabPane.add(new PhysDirPanel(), "Files");                  // index = 4 tabGreen
    file_tab = 4;
    topTabPane.add(new XlinkTabPanel(), "XLinks");                 // index = 5 tab
    browse_tab = 5;
    topTabPane.add(new SubBrowsePanel(), "Subset/Browse");        // index = 6 tabPink
    browse_tab = 6;
    topTabPane.add(new FormatPanel(), "Format");                  // index = 7 tabPurple
    format_tab = 7;
    topTabPane.add(new PlotPanel(), "Plot");                      // index = 8 tabLtGreen
    plot_tab = 8;
    topTabPane.add(new ContactPanel(), "Users & PIs");            // index = 9 tabGenta
    user_tab = 9;
    topTabPane.add(new TapePanel(), "Tapes & CDs");               // index = 10 tabNaplesYellow
    tape_tab = 10;
    topTabPane.setBackgroundAt(desc_tab, DatasetTabPanel.getTabColor());
    topTabPane.setBackgroundAt(links_tab, queryPanel.getTabColor());
    topTabPane.setBackgroundAt(cat_tab, CategoryPanel.getTabColor());
    topTabPane.setBackgroundAt(doc_tab, DocsPanel.getTabColor());
    topTabPane.setBackgroundAt(file_tab, PhysDirPanel.getTabColor());
    topTabPane.setBackgroundAt(xlink_tab, XlinkTabPanel.getTabColor());
    topTabPane.setBackgroundAt(browse_tab, SubBrowsePanel.getTabColor());
    topTabPane.setBackgroundAt(format_tab, FormatPanel.getTabColor());
    topTabPane.setBackgroundAt(plot_tab, PlotPanel.getTabColor());
    topTabPane.setBackgroundAt(user_tab, ContactPanel.getTabColor());
    topTabPane.setBackgroundAt(tape_tab, TapePanel.getTabColor());
 }

//-------------------------------------------------------------------------------------
//   Refreshing most dataset display fields is done here, so time isn't wasted when
//   the Search results are browsed, and the response is quick on that page. We update
//   the queries for the selected dataset ID when the dataset tab is clicked.
//-------------------------------------------------------------------------------------

//-------------------------------------------------------------------------------------
//   If the Dataset tab is clicked, update ID/proj link, the subset and browse pages.
//   The paramRowID is used to keep track of need for syncing; it holds the dataset ID
//   once syncing is done.
//-------------------------------------------------------------------------------------
  void this_componentShown(ComponentEvent e) {
      if (!dataModule11.getParamRowID().getString("dataset_id").equals(dataModule11.queryDataset1.getString("dataset_id"))) {
        dataModule11.paramRowID.setString("dataset_id", dataModule11.queryDataset1.getString("dataset_id"));
        dataModule11.paramRowID.setString("spatial_type", dataModule11.queryDataset1.getString("spatial_type"));
        dataModule11.queryDatasetProj.refresh(); // update dataset ID/proj ID link table
        dataModule11.paramRowProjID.setString("proj_id", dataModule11.queryDatasetProj.getString("project_id"));
        dataModule11.queryProjDataset.refresh();
        dataModule11.queryDSReferences.refresh();
        dataModule11.queryDatasetPI.refresh();
        dataModule11.queryReadme.refresh();
        dataModule11.queryDSCategory.refresh();     // update category
        dataModule11.queryDSPlatform.refresh();     // update platform
        dataModule11.queryDatasetXlink.refresh();
        dataModule11.queryDatasetOptions.refresh();
        dataModule11.queryPlots.refresh();
        dataModule11.queryFormatConv.refresh(); // update format conversions page
        dataModule11.queryTape.refresh();       // update tape page

        //--------------------------------------------------------------------------------
        // set Neither radio button on dataset page if this is neither online nor offline
        //--------------------------------------------------------------------------------
          if ((dataModule11.queryDataset1.getByte("onlineorderable")==0) && (dataModule11.queryDataset1.getByte("offlineorderable")==0)) {
            DatasetTabPanel.linkRadioButton.setSelected(true);
            System.out.println("Setting the Neither radio button.");
        }

        //------------------------------------------------------------
        // put dataset ID in boxes beside tables when no rows show
        //------------------------------------------------------------
        if (queryPanel.idTextField.getText().equalsIgnoreCase("")) {
          queryPanel.idTextField.setText(dataModule11.getParamRowID().getString("dataset_id"));
        }
        if (CategoryPanel.idTextField.getText().equalsIgnoreCase("")) {
          CategoryPanel.idTextField.setText(dataModule11.getParamRowID().getString("dataset_id"));
        }
        if (CategoryPanel.idTextField1.getText().equalsIgnoreCase("")) {
          CategoryPanel.idTextField1.setText(dataModule11.getParamRowID().getString("dataset_id"));
        }
        if (DocsPanel.dsIDTextField.getText().equalsIgnoreCase("")) {
          DocsPanel.dsIDTextField.setText(dataModule11.getParamRowID().getString("dataset_id"));
        }
        if (XlinkTabPanel.dsIDTextField.getText().equalsIgnoreCase("")) {
          XlinkTabPanel.dsIDTextField.setText(dataModule11.getParamRowID().getString("dataset_id"));
        }
        if (PlotPanel.idTextField.getText().equalsIgnoreCase("")) {
          PlotPanel.idTextField.setText(dataModule11.getParamRowID().getString("dataset_id"));
        }
        //------------------------------------------------------------
        // update file listing if PhysDir panel is already showing
        // OR when KeepIDs is checked and we change dataset
        //------------------------------------------------------------
        if ((topTabPane.getSelectedIndex() == file_tab) || (SearchDatasetPanel.keepIDCheckBox.isSelected()))  {
          dataModule11.paramRowFile.setString("ds_id", dataModule11.queryDataset1.getString("dataset_id"));
          dataModule11.paramRowFile.setString("purpose", "data");
          dataModule11.queryFile.refresh();
        }
      }
  }

//-----------------------------------------------------------------------
//  If dataset tabs are clicked, we check here to make sure the
//  fields for the tab match the dataset ID displayed at the top
//  of the form, and update them all if they don't.
//
//  Note: Consider combining with lines above, into 1 routine
//-----------------------------------------------------------------------
  void topTabPane_stateChanged(ChangeEvent e) {
    if (!SearchDatasetPanel.keepIDCheckBox.isSelected()){
      if (!dataModule11.getParamRowID().getString("dataset_id").equals(dataModule11.queryDataset1.getString("dataset_id"))) {
        dataModule11.paramRowID.setString("dataset_id", dataModule11.queryDataset1.getString("dataset_id"));
        dataModule11.paramRowID.setString("spatial_type", dataModule11.queryDataset1.getString("spatial_type"));
        dataModule11.queryDatasetProj.refresh();                 // update dataset ID/proj ID link table
        dataModule11.paramRowProjID.setString("proj_id", dataModule11.queryDatasetProj.getString("project_id"));
        dataModule11.queryProjDataset.refresh();
        dataModule11.queryDSReferences.refresh();
        dataModule11.queryDatasetPI.refresh();
        dataModule11.queryReadme.refresh();
        dataModule11.queryDSCategory.refresh();       // update category
        dataModule11.queryDSPlatform.refresh();       // update platform
        dataModule11.queryDatasetXlink.refresh();
        dataModule11.queryDatasetOptions.refresh();
        dataModule11.queryPlots.refresh();
        dataModule11.queryFormatConv.refresh(); // update format conversions page
        dataModule11.queryTape.refresh();       // update tape page
        //------------------------------------------------------------
        // put dataset ID in boxes beside tables when no rows show
        //------------------------------------------------------------
        if (queryPanel.idTextField.getText().equalsIgnoreCase("")) {
          queryPanel.idTextField.setText(dataModule11.getParamRowID().getString("dataset_id"));
        }
        if (CategoryPanel.idTextField.getText().equalsIgnoreCase("")) {
          CategoryPanel.idTextField.setText(dataModule11.getParamRowID().getString("dataset_id"));
        }
        if (CategoryPanel.idTextField1.getText().equalsIgnoreCase("")) {
          CategoryPanel.idTextField1.setText(dataModule11.getParamRowID().getString("dataset_id"));
        }
        if (DocsPanel.dsIDTextField.getText().equalsIgnoreCase("")) {
          DocsPanel.dsIDTextField.setText(dataModule11.getParamRowID().getString("dataset_id"));
        }
        if (XlinkTabPanel.dsIDTextField.getText().equalsIgnoreCase("")) {
          XlinkTabPanel.dsIDTextField.setText(dataModule11.getParamRowID().getString("dataset_id"));
        }
        if (PlotPanel.idTextField.getText().equalsIgnoreCase("")) {
          PlotPanel.idTextField.setText(dataModule11.getParamRowID().getString("dataset_id"));
        }
        //  update file listing if PhysDir panel is already showing,
        if (topTabPane.getSelectedIndex() == file_tab) {
          dataModule11.paramRowFile.setString("ds_id", dataModule11.queryDataset1.getString("dataset_id"));
          dataModule11.paramRowFile.setString("purpose", "data");
          dataModule11.queryFile.refresh();
        }
      }
    } else {
      System.out.println("We are keeping the dataset ID, so will skip updating of tabs.");
    }
  }

}


