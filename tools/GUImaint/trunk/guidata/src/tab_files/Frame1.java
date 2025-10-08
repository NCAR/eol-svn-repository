package tab_files;

import java.beans.*;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;

import com.borland.dbswing.*;
import com.borland.dx.dataset.*;
import com.borland.dx.sql.dataset.*;

/**
 * <p>Title: GUImaint for CODIAC</p>
 * <p>Description: MySQL db version</p>
 * <p>Copyright: Copyright (c) 2004, 2005, 2006, 2007</p>
 * <p>Company: NCAR/EOL</p>
 * @author Don Stott
 * @version 2.5- test
 */

public class Frame1 extends JFrame {
  BorderLayout borderLayout1 = new BorderLayout();
  static Color tabGray = new Color(230, 225, 215);
  static Color thisTabColor = tabGray;

  // -------------------------------------------------------------------
  // menu bar
  // -------------------------------------------------------------------
  JMenuBar menuBar1 = new JMenuBar();
  JMenu menuFile = new JMenu();
  JRadioButtonMenuItem menuDTS = new JRadioButtonMenuItem("Data Tracking System", false);
  JRadioButtonMenuItem menuMaint = new JRadioButtonMenuItem("Work on Projects and Datasets", true);
  JMenuItem menuFileExit = new JMenuItem();
  JMenu menuHelp = new JMenu();
  JMenuItem menuHelpAbout = new JMenuItem();

  // -------------------------------------------------------------------
  // toolbar
  // -------------------------------------------------------------------
  JToolBar toolBar = new JToolBar();

  // -------------------------------------------------------------------
  // our main tab group
  // -------------------------------------------------------------------
  static SideTabbedPages leftTabPane = new SideTabbedPages();
  DBDisposeMonitor dBDisposeMonitor1 = new DBDisposeMonitor();
  JdbNavToolBar navToolBar = new JdbNavToolBar();
  DataModule1 dataModule11;
  static DataSet focusDs;
  static int thisRow;
  TitledBorder titledBorder1;
  JTextField statusText = new JTextField();
  JMenuItem menuHowTo = new JMenuItem();
  JMenuItem menuNotes = new JMenuItem();
  JMenuItem menuPswd = new JMenuItem();

  //Construct the frame
  public Frame1() {
    enableEvents(AWTEvent.WINDOW_EVENT_MASK);
    try  {
    }
    catch(Exception e) {
      e.printStackTrace();
    }
  }

  // -------------------------------------------------------------------
  //Component initialization
  // -------------------------------------------------------------------
  private void jbInit() throws Exception  {
    titledBorder1 = new TitledBorder("");
    this.getContentPane().setLayout(borderLayout1);
//    this.getContentPane().setBackground(SystemColor.info);
    this.setResizable(true);
    this.setSize(new Dimension(1000, 780)); // good size for notebook computers
//    this.setSize(new Dimension(900, 727)); // original size, version 1.3 release
    this.setTitle("GUImaint for CODIAC");

    // -------------------------------------------------------------------
    // toolbar
    // -------------------------------------------------------------------
    toolBar.setBackground(thisTabColor);
    toolBar.setAlignmentX((float) 1.0);
    toolBar.setAlignmentY((float) 1.0);
    dBDisposeMonitor1.setDataAwareComponentContainer(this);
//    navToolBar.setBackground(thisTabColor);
    navToolBar.setBackground(new Color(230, 225, 215));
    navToolBar.setForeground(Color.black);
    navToolBar.setBorder(BorderFactory.createRaisedBevelBorder());
    navToolBar.setMinimumSize(new Dimension(361, 32));
    navToolBar.setPreferredSize(new Dimension(375, 40));
    navToolBar.setToolTipText("");
//  when post button is clicked, save button is enabled
    navToolBar.setButtonStateSave(JdbNavToolBar.DISABLED);
//  keep post button enabled, so auto posting doesn't disable it
//  and prevent enabling the save button by clicking on post
    navToolBar.setButtonStatePost(JdbNavToolBar.ENABLED);

    statusText.setBackground(new Color(255, 255, 245));
    statusText.setFont(new java.awt.Font("Dialog", 1, 12));
    statusText.setMaximumSize(new Dimension(400, 28));
    statusText.setMinimumSize(new Dimension(350, 20));
    statusText.setPreferredSize(new Dimension(350, 28));
    statusText.setEditable(false);
    statusText.setText("statusText");
    leftTabPane.setBackground(thisTabColor);

    menuDTS.setText("Data Tracking System");
    toolBar.add(statusText, null);
    toolBar.add(navToolBar, null);
    this.getContentPane().add(toolBar, BorderLayout.SOUTH);

    menuMaint.setToolTipText("Enable Project and Dataset tabs");
    menuMaint.addMenuKeyListener(new javax.swing.event.MenuKeyListener() {

      public void menuKeyPressed(MenuKeyEvent e) {
        menuMaint_menuKeyPressed(e);
      }

      public void menuKeyReleased(MenuKeyEvent e) {
      }

      public void menuKeyTyped(MenuKeyEvent e) {
      }
    });
    menuMaint.addMouseListener(new java.awt.event.MouseAdapter() {

      public void mousePressed(MouseEvent e) {
        menuMaint_mousePressed(e);
      }
    });

    leftTabPane.setPreferredSize(new Dimension(800, 1000));
    this.getContentPane().add(leftTabPane, BorderLayout.CENTER);

    // -------------------------------------------------------------------
    // menu
    // -------------------------------------------------------------------
    menuFile.setText("File");
    menuFileExit.setText("Exit");
    menuFileExit.addActionListener(new ActionListener()  {
      public void actionPerformed(ActionEvent e) {
        fileExit_actionPerformed(e);
      }
    });

    menuHelp.setText("Help");
    menuHelpAbout.setText("About");
    menuHelpAbout.addActionListener(new ActionListener()  {
      public void actionPerformed(ActionEvent e) {
        helpAbout_actionPerformed(e);
      }
    });

    menuHowTo.setText("How-To");
    menuHowTo.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        menuHowTo_actionPerformed(e);
      }
    });

    menuNotes.setText("Notes");
    menuNotes.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        menuNotes_actionPerformed(e);
      }
    });

    menuPswd.setText("Password");
    menuPswd.setToolTipText("Help with setting passwords");
    menuPswd.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        menuPswd_actionPerformed(e);
      }
    });


    menuDTS.setToolTipText("Enable the \"DTS\" tab");
    menuDTS.setRolloverEnabled(true);
    menuDTS.setActionCommand(" Data Tracking System");
    menuDTS.addMouseListener(new java.awt.event.MouseAdapter() {
      public void mousePressed(MouseEvent e) {
        menuDTS_mousePressed(e);
      }
    });

    menuDTS.addMenuKeyListener(new javax.swing.event.MenuKeyListener() {
      public void menuKeyPressed(MenuKeyEvent e) {
        menuDTS_menuKeyPressed(e);
      }
      public void menuKeyReleased(MenuKeyEvent e) {
      }
      public void menuKeyTyped(MenuKeyEvent e) {
      }
    });

    ButtonGroup menuGroup = new ButtonGroup();
    menuGroup.add(menuDTS);
    menuGroup.add(menuMaint);

    menuFile.add(menuDTS);
    menuFile.add(menuMaint);
    menuFile.add(menuFileExit);
    menuHelp.add(menuHelpAbout);
    menuHelp.addSeparator();
    menuHelp.add(menuHowTo);
    menuHelp.add(menuNotes);
    menuHelp.addSeparator();
    menuHelp.add(menuPswd);
    menuBar1.add(menuFile);
    menuBar1.add(menuHelp);
    this.setJMenuBar(menuBar1);

  // -------------------------------------------------------------------
  // add actionListeners to navToolBar buttons
  // -------------------------------------------------------------------
  navToolBar.getFirstButton().addActionListener(new java.awt.event.ActionListener() {
    public void actionPerformed(ActionEvent e) {
      focusDs = navToolBar.getFocusedDataSet();
      if (focusDs.getTableName().equalsIgnoreCase("dataset")) {
        SideTabbedPages.setDitto(false);
        SideTabbedPages.idTextField.setRequestFocusEnabled(false);
        SideTabbedPages.idTextField.setEditable(false);
        SideTabbedPages.titleTextField.setRequestFocusEnabled(false);
        SideTabbedPages.titleTextField.setEditable(false);
      }
    }
  });

  navToolBar.getNextButton().addActionListener(new java.awt.event.ActionListener() {
    public void actionPerformed(ActionEvent e) {
      focusDs = navToolBar.getFocusedDataSet();
      if (focusDs.getTableName().equalsIgnoreCase("dataset")) {
//        dataModule11.queryDSReferences.refresh();
        SideTabbedPages.setDitto(false);
        SideTabbedPages.idTextField.setRequestFocusEnabled(false);
        SideTabbedPages.idTextField.setEditable(false);
        SideTabbedPages.titleTextField.setRequestFocusEnabled(false);
        SideTabbedPages.titleTextField.setEditable(false);
      }
    }
  });

  navToolBar.getPriorButton().addActionListener(new java.awt.event.ActionListener() {
    public void actionPerformed(ActionEvent e) {
      focusDs = navToolBar.getFocusedDataSet();
      if (focusDs.getTableName().equalsIgnoreCase("dataset")) {
        SideTabbedPages.setDitto(false);
        SideTabbedPages.idTextField.setRequestFocusEnabled(false);
        SideTabbedPages.idTextField.setEditable(false);
        SideTabbedPages.titleTextField.setRequestFocusEnabled(false);
        SideTabbedPages.titleTextField.setEditable(false);
      }
    }
  });

  navToolBar.getLastButton().addActionListener(new java.awt.event.ActionListener() {
    public void actionPerformed(ActionEvent e) {
      focusDs = navToolBar.getFocusedDataSet();
      if (focusDs.getTableName().equalsIgnoreCase("dataset")) {
        SideTabbedPages.setDitto(false);
        SideTabbedPages.idTextField.setRequestFocusEnabled(false);
        SideTabbedPages.idTextField.setEditable(false);
        SideTabbedPages.titleTextField.setRequestFocusEnabled(false);
        SideTabbedPages.titleTextField.setEditable(false);
      }
    }
  });

  navToolBar.getInsertButton().removeActionListener(navToolBar);
  navToolBar.getInsertButton().addActionListener(new java.awt.event.ActionListener() {
    public void actionPerformed(ActionEvent e) {
      insertButton_actionPerformed(e);
    }
  });

  navToolBar.getDeleteButton().addActionListener(new java.awt.event.ActionListener() {
    public void actionPerformed(ActionEvent e) {
      navToolBar.setButtonStateSave(JdbNavToolBar.ENABLED);
    }
  });

  navToolBar.getDittoButton().removeActionListener(navToolBar);
  navToolBar.getDittoButton().addActionListener(new java.awt.event.ActionListener() {
    public void actionPerformed(ActionEvent e) {
      dittoButton_actionPerformed(e);
    }
  });

  navToolBar.getRefreshButton().removeActionListener(navToolBar);
  navToolBar.getRefreshButton().addActionListener(new java.awt.event.ActionListener() {
    public void actionPerformed(ActionEvent e) {
      refreshButton_actionPerformed(e);
    }
  });

  navToolBar.getSaveButton().addActionListener(new java.awt.event.ActionListener() {
    public void actionPerformed(ActionEvent e) {
     saveButton_actionPerformed(e);
    }
  });

  navToolBar.getPostButton().removeActionListener(navToolBar);
  navToolBar.getPostButton().addActionListener(new java.awt.event.ActionListener() {
    public void actionPerformed(ActionEvent e) {
     postButton_actionPerformed(e);
    }
  });

  navToolBar.addPropertyChangeListener(new java.beans.PropertyChangeListener() {
    public void propertyChange(PropertyChangeEvent e) {
      navToolBar_propertyChange(e);
    }
  });

  }   // end init section

  // -------------------------------------------------------------------
  //File | Exit action performed
  // -------------------------------------------------------------------
  public void fileExit_actionPerformed(ActionEvent e) {
    System.exit(0);
  }

  // -------------------------------------------------------------------
  //Help | About action performed
  // -------------------------------------------------------------------
  public void helpAbout_actionPerformed(ActionEvent e) {
    Frame1_AboutBox dlg = new Frame1_AboutBox(this);
    Dimension dlgSize = dlg.getPreferredSize();
    Dimension frmSize = getSize();
    Point loc = getLocation();
    dlg.setLocation((frmSize.width - dlgSize.width) / 2 + loc.x, (frmSize.height - dlgSize.height) / 2 + loc.y);
    dlg.setModal(true);
    dlg.show();
  }

  // -------------------------------------------------------------------
  //Help | How-To action performed
  // -------------------------------------------------------------------
  void menuHowTo_actionPerformed(ActionEvent e) {
    HowToDialog dlg2 = new HowToDialog(this);
    Dimension dlgSize = dlg2.getPreferredSize();
    Dimension frmSize = getSize();
    Point loc = getLocation();
    dlg2.setLocation((frmSize.width - dlgSize.width) / 2 + loc.x, (frmSize.height - dlgSize.height) / 2 + loc.y);
    dlg2.setModal(false);
    dlg2.show();
  }

  // -------------------------------------------------------------------
  //Help | Notes action performed
  // -------------------------------------------------------------------
  void menuNotes_actionPerformed(ActionEvent e) {
    NotesDialog dlg3 = new NotesDialog(this);
    Dimension dlgSize = dlg3.getPreferredSize();
    Dimension frmSize = getSize();
    Point loc = getLocation();
    dlg3.setLocation((frmSize.width - dlgSize.width) / 2 + loc.x, (frmSize.height - dlgSize.height) / 2 + loc.y);
    dlg3.setModal(false);
    dlg3.show();
  }

  // -------------------------------------------------------------------
  //Help | Password action performed
  // -------------------------------------------------------------------
  void menuPswd_actionPerformed(ActionEvent e) {
    pswordDialog.setDialogup(false);
    pswordDialog dlg4 = new pswordDialog();
    Dimension dlgSize = dlg4.getPreferredSize();
    Dimension frmSize = getSize();
    dlg4.show();
  }

  // -------------------------------------------------------------------
  //Overridden so we can exit on System Close
  // -------------------------------------------------------------------
  protected void processWindowEvent(WindowEvent e) {
    super.processWindowEvent(e);
    if(e.getID() == WindowEvent.WINDOW_CLOSING) {
      fileExit_actionPerformed(null);
    }
  }

  // -------------------------------------------------------------------
  // menu choice: Access DB - disable all but Search and db Maint choices
  // -------------------------------------------------------------------
  void menuDTS_menuKeyPressed(MenuKeyEvent e) {
     leftTabPane.sideTabPane.setEnabledAt(0, false);
     leftTabPane.sideTabPane.setEnabledAt(1, true);
     leftTabPane.sideTabPane.setEnabledAt(2, false);
     leftTabPane.sideTabPane.setEnabledAt(3, false);
     leftTabPane.sideTabPane.setEnabledAt(4, false);
     leftTabPane.sideTabPane.setEnabledAt(5, true);
     SideTabbedPages.sideTabPane.setSelectedIndex(5);
  }

  void menuDTS_mousePressed(MouseEvent e) {
     leftTabPane.sideTabPane.setEnabledAt(0, false);
     leftTabPane.sideTabPane.setEnabledAt(1, true);
     leftTabPane.sideTabPane.setEnabledAt(2, false);
     leftTabPane.sideTabPane.setEnabledAt(3, false);
     leftTabPane.sideTabPane.setEnabledAt(4, false);
     leftTabPane.sideTabPane.setEnabledAt(5, true);
     SideTabbedPages.sideTabPane.setSelectedIndex(5);
  }

  // -------------------------------------------------------------------
  // menu choice: Maint - disable the db Maint choice
  // -------------------------------------------------------------------
  void menuMaint_menuKeyPressed(MenuKeyEvent e) {
     leftTabPane.sideTabPane.setEnabledAt(0, true);
     leftTabPane.sideTabPane.setEnabledAt(1, true);
     leftTabPane.sideTabPane.setEnabledAt(2, true);
     leftTabPane.sideTabPane.setEnabledAt(3, true);
     leftTabPane.sideTabPane.setEnabledAt(4, true);
     leftTabPane.sideTabPane.setEnabledAt(5, false);
     leftTabPane.sideTabPane.setSelectedIndex(2);
  }

  void menuMaint_mousePressed(MouseEvent e) {
     leftTabPane.sideTabPane.setEnabledAt(0, true);
     leftTabPane.sideTabPane.setEnabledAt(1, true);
     leftTabPane.sideTabPane.setEnabledAt(2, true);
     leftTabPane.sideTabPane.setEnabledAt(3, true);
     leftTabPane.sideTabPane.setEnabledAt(4, true);
     leftTabPane.sideTabPane.setEnabledAt(5, false);
     leftTabPane.sideTabPane.setSelectedIndex(2);
  }

  // -------------------------------------------------------------------
  // Actions for actionListeners on navToolBar buttons
  // -------------------------------------------------------------------

  // -------------------------------------------------------------------
  // post:
  //  If not on the DST side tab, set the row revise time to null;
  //  and if a CODIAC dataset, the row revise contact to this user;
  //  and if on the dataset-references table set the dataset ID in it.
  // -------------------------------------------------------------------
  void postButton_actionPerformed(ActionEvent e) {
    try {
      DataSet ds_tosave = navToolBar.getFocusedDataSet();
//      int tabIndex = DatasetTabs.topTabPane.getSelectedIndex();
      int tabIndex = SideTabbedPages.sideTabPane.getSelectedIndex();
      if (tabIndex != SideTabbedPages.dts_sidetab) {
        ds_tosave.setTimestamp("row_revise_time", null);
        if (ds_tosave.getTableName().equalsIgnoreCase("dataset")) {
          ds_tosave.setShort("row_revise_contact_id", StartTabPanel.thisUserID);
          if (SideTabbedPages.idTextField.getText().trim().equals("")) {
            throw new DataSetException("Error: Dataset ID is an empty string. Enter a valid Dataset ID.");
          }
        }
        if (ds_tosave.getTableName().equalsIgnoreCase("dataset_references")) {
          ds_tosave.setString("dataset_id", SideTabbedPages.idTextField.getText());
          System.out.println("dataset id = " + SideTabbedPages.idTextField.getText());
        }
      }
      ds_tosave.post();
      navToolBar.setButtonStateSave(JdbNavToolBar.ENABLED);
      System.out.println("Focused dataset: " + ds_tosave.getTableName() + " posted.");
    } catch (DataSetException dse) {
      DBExceptionHandler.handleException(dse);
    }
  }

  //------------------------------------------
  // refresh:
  //  Ask for confirmation before refreshing
  //  if there are unsaved changes, and then
  //  use cancel, instead of refresh, to keep
  //  the focus and the row. Also, make ID and
  //  title fields at the top uneditable, in case
  //  a new dataset was being edited, but make the
  //  IDs and titles in the rows editable.
  //------------------------------------------
  void refreshButton_actionPerformed(ActionEvent e) {
    try {
      focusDs = navToolBar.getFocusedDataSet();
      StorageDataSet sds = focusDs.getStorageDataSet();
      thisRow = focusDs.getRow();
      if (sds.getDeletedRowCount() > 0 || sds.getInsertedRowCount() > 0 || sds.getUpdatedRowCount() > 0 || sds.isEditing()) {
        if (JOptionPane.showConfirmDialog(this, "Refresh will discard unsaved changes.  Continue?",
          "You have unsaved changes", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE) == JOptionPane.YES_OPTION) {
          SideTabbedPages.setDitto(false);
          SideTabbedPages.idTextField.setRequestFocusEnabled(false);
          SideTabbedPages.idTextField.setEditable(false);
          SideTabbedPages.titleTextField.setRequestFocusEnabled(false);
          SideTabbedPages.titleTextField.setEditable(false);
          focusDs.refresh();
          SearchDatasetPanel.setResultsEdit(true);
        } else {
          System.out.println("Refresh cancelled.");
        }
      } else {
        focusDs.refresh();
        SearchDatasetPanel.setResultsEdit(true);
      }
    } catch (DataSetException dse) {
      DBExceptionHandler.handleException(dse);
    }
  }

  //------------------------------------------
  // insert:
  //   make dataset ID and Name editable,
  //   if on dataset page; and insert row
  //   after the current row
  //------------------------------------------
  void insertButton_actionPerformed(ActionEvent e) {
    try {
      focusDs = navToolBar.getFocusedDataSet();
      if (focusDs.getTableName().equalsIgnoreCase("dataset")) {
        SideTabbedPages.setDitto(false);
        SideTabbedPages.idTextField.setRequestFocusEnabled(true);
        SideTabbedPages.idTextField.setEditable(true);
        SideTabbedPages.titleTextField.setRequestFocusEnabled(true);
        SideTabbedPages.titleTextField.setEditable(true);
      }
      focusDs.insertRow(false);
    } catch (DataSetException dse) {
      DBExceptionHandler.handleException(dse);
    }
  }

  //------------------------------------------
  // ditto:
  //   Copy previous row. Does not copy over
  //   existing row.  Automatically inserts a
  //   new row if the DataSet is not in edit mode,
  //   then copies the contents of the previous
  //   row to the new row. Also, will make
  //   dataset ID and Name editable, if on
  //   dataset page; and clear file ID if
  //   on file table. SideTabbedPages and SearchDatasetPanel
  //   are notified and have variables set which control
  //   the ability to edit the dataset ID and title.
  //------------------------------------------
  void dittoButton_actionPerformed(ActionEvent e) {
    try {
      focusDs = navToolBar.getFocusedDataSet();
      focusDs.dittoRow(false, true);
      if (focusDs.getTableName().equalsIgnoreCase("dataset")) {
        SearchDatasetPanel.setResultsEdit(false);
        SideTabbedPages.setDitto(true);
      } else if (focusDs.getTableName().equalsIgnoreCase("file")) {
        Variant fileIDvar = new Variant();
        focusDs.getVariant("file_id", focusDs.getRow(), fileIDvar);
        System.out.println("file id =" + fileIDvar + ", is being set to null on the dittoed row");
        fileIDvar.setAssignedNull();
        focusDs.setVariant("file_id", fileIDvar);
      } else if (focusDs.getTableName().equalsIgnoreCase("tape")) {
        Variant tapeIDvar = new Variant();
        focusDs.getVariant("tape_id", focusDs.getRow(), tapeIDvar);
        System.out.println("file id =" + tapeIDvar + ", is being set to null on the dittoed row");
        tapeIDvar.setAssignedNull();
        focusDs.setVariant("tape_id", tapeIDvar);
      } else if (focusDs.getTableName().equalsIgnoreCase("contact")) {
        Variant contactIDvar = new Variant();
        focusDs.getVariant("contact_id", focusDs.getRow(), contactIDvar);
        System.out.println("contact id =" + contactIDvar + ", is being set to null on the dittoed row");
        contactIDvar.setAssignedNull();
        focusDs.setVariant("contact_id", contactIDvar);
      }
    } catch (DataSetException dse) {
      DBExceptionHandler.handleException(dse);
    }
  }

  //------------------------------------------
  // save:
  //   make dataset ID and Name non-editable
  //------------------------------------------
  void saveButton_actionPerformed(ActionEvent e) {
    try {
      focusDs = navToolBar.getFocusedDataSet();
      navToolBar.setButtonStateSave(JdbNavToolBar.DISABLED);
      SideTabbedPages.setDitto(false);
      SideTabbedPages.idTextField.setRequestFocusEnabled(false);
      SideTabbedPages.idTextField.setEditable(false);
      SideTabbedPages.titleTextField.setRequestFocusEnabled(false);
      SideTabbedPages.titleTextField.setEditable(false);
      System.out.println("Focused dataset: " + focusDs.getTableName() + " saved.");
    } catch (DataSetException dse) {
      DBExceptionHandler.handleException(dse);
    }
  }

  public void setModule(DataModule1 dataModule11) {
    this.dataModule11 = dataModule11;
    try {
      jbInit();
    }
    catch(Exception e) {
      e.printStackTrace();
    }
  }

  //------------------------------------------
  // Enable Post button.
  //------------------------------------------
  public void enablePost() {
    statusText.setText("Post button enabled");
    navToolBar.setButtonStatePost(JdbNavToolBar.ENABLED);
  }

  //------------------------------------------
  // Put focused dataset name in status bar.
  // If on Project page, disable delete button.
  //------------------------------------------
  void navToolBar_propertyChange(PropertyChangeEvent e) {
    if (e.getPropertyName().equals("focusedDataSet")) {
      focusDs = (DataSet) e.getNewValue();
      if (focusDs != null) {
        statusText.setText("  focus is on: " + focusDs.getTableName());
        if (focusDs.getTableName().equalsIgnoreCase("project")) {
          navToolBar.setButtonStateDelete(JdbNavToolBar.DISABLED);
          navToolBar.setButtonStateDitto(JdbNavToolBar.DISABLED);
          navToolBar.setButtonStateInsert(JdbNavToolBar.DISABLED);
//          System.out.println("in project, delete button disabled...");
        } else {
          navToolBar.setButtonStateDelete(JdbNavToolBar.AUTO_ENABLED);
          navToolBar.setButtonStateDitto(JdbNavToolBar.AUTO_ENABLED);
          navToolBar.setButtonStateInsert(JdbNavToolBar.AUTO_ENABLED);
//          System.out.println("in any table other than project, delete button enabled...");
        }
      }
    }
  }

}
