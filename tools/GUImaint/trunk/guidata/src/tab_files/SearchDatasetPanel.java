package tab_files;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

import com.borland.dbswing.*;
import com.borland.jbcl.layout.*;

/**
 * <p>Title: GUImaint for CODIAC</p>
 * <p>Description: MySQL db version</p>
 * <p>Copyright: Copyright (c) 2004, 2005, 2006, 2007</p>
 * <p>Company: NCAR/EOL</p>
 * @author Don Stott
 * @version 2.5- test
 */

public class SearchDatasetPanel extends JPanel {
  BorderLayout borderLayout1 = new BorderLayout();
  static JPanel searchPanel1 = new JPanel();
  static Color tabPalePurple = new Color(196, 199, 200);
  static Color thisTabColor = tabPalePurple;
  JdbStatusLabel jdbStatusLabel = new JdbStatusLabel();
  static TableScrollPane tableScrollPane1 = new TableScrollPane();
  static JdbTable resultsTable = new JdbTable();
  static JdbTextField IDTextField = new JdbTextField();
  JdbLabel idLabel = new JdbLabel();
  static boolean editState = true;

//  Button button1 = new Button();
  DataModule1 dataModule11;
  XYLayout xYLayout1 = new XYLayout();
  static JCheckBox keepIDCheckBox = new JCheckBox();

  public SearchDatasetPanel() {
    try  {
      jbInit();
    }
    catch(Exception ex) {
      ex.printStackTrace();
    }
  }

  private void jbInit() throws Exception {
    dataModule11 = tab_files.DataModule1.getDataModule();
    this.setLayout(borderLayout1);
    jdbStatusLabel.setBackground(thisTabColor);
    jdbStatusLabel.setAlignmentX((float) 0.5);
    jdbStatusLabel.setAutoscrolls(true);
    jdbStatusLabel.setBorder(BorderFactory.createRaisedBevelBorder());
    jdbStatusLabel.setHorizontalAlignment(SwingConstants.CENTER);
    jdbStatusLabel.setHorizontalTextPosition(SwingConstants.LEFT);
    jdbStatusLabel.setText("Type number to search from in dataset ID textbox then hit Enter");
    jdbStatusLabel.setDataSet(dataModule11.getQueryDataset1());

    resultsTable.setMinimumSize(new Dimension(148, 200));
    resultsTable.setToolTipText("");
    resultsTable.setAutoResizeMode(JTable.AUTO_RESIZE_SUBSEQUENT_COLUMNS);
    resultsTable.setColumnSelectionAllowed(true);
    resultsTable.setIntercellSpacing(new Dimension(3, 3));
    resultsTable.setDataSet(dataModule11.getQueryDataset1());
    resultsTable.setAutoSelection(true);

    //--------------------------------------------------------------------------
    // Start tab
    //--------------------------------------------------------------------------
    this.setEnabled(true);
    this.setAlignmentY((float) 0.5);

    IDTextField.addKeyListener(new java.awt.event.KeyAdapter() {
      public void keyPressed(KeyEvent e) {
        IDTextField_keyPressed(e);
      }
    });

    IDTextField.setRequestFocusEnabled(true);
    IDTextField.setFocusAccelerator('s');
    IDTextField.setSelectionEnd(1);
    tableScrollPane1.getViewport().setBackground(thisTabColor);
    searchPanel1.setBackground(thisTabColor);
    keepIDCheckBox.setFont(new java.awt.Font("Dialog", 1, 12));
    keepIDCheckBox.setForeground(Color.black);
    keepIDCheckBox.setMaximumSize(new Dimension(185, 24));
    keepIDCheckBox.setOpaque(false);
    keepIDCheckBox.setPreferredSize(new Dimension(165, 24));
    keepIDCheckBox.setHorizontalAlignment(SwingConstants.LEFT);
    keepIDCheckBox.setHorizontalTextPosition(SwingConstants.RIGHT);
    keepIDCheckBox.setText(" Keep Dataset ID in Tabs");
    keepIDCheckBox.addItemListener(new java.awt.event.ItemListener() {
      public void itemStateChanged(ItemEvent e) {
        keepIDCheckBox_itemStateChanged(e);
      }
    });
    idLabel.setText("");
    idLabel.setTextWithMnemonic("Find data&set ID:");
    tableScrollPane1.getViewport().add(resultsTable, null);
    resultsTable.setHiddenColumns(new int[] { 2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22 });
    searchPanel1.setLayout(xYLayout1);
    searchPanel1.setPreferredSize(new Dimension(504, 53));
    IDTextField.setBackground(new java.awt.Color(255, 255, 225));
    IDTextField.setMaximumSize(new Dimension(70, 21));
    IDTextField.setPreferredSize(new Dimension(60, 21));
    IDTextField.setToolTipText("Type ID and hit Enter--Use % for wildcard");
    IDTextField.setText("83.01%");
//    IDTextField.setText("87.01%");
    IDTextField.selectAll();
    searchPanel1.add(idLabel,  new XYConstraints(36, 19, -1, -1));
    searchPanel1.add(keepIDCheckBox, new XYConstraints(410, 0, 226, 52));
    searchPanel1.add(IDTextField,    new XYConstraints(139, 16, 122, -1));
    this.add(jdbStatusLabel, BorderLayout.SOUTH);
    this.add(tableScrollPane1, BorderLayout.CENTER);
    this.add(searchPanel1, BorderLayout.NORTH);
  }

  void IDTextField_keyPressed(KeyEvent e) {
    try {
      char ch = e.getKeyChar();
      if (ch == '\n') {
        jdbStatusLabel.setText("Retrieving Requested Datasets...");
        dataModule11.paramRowSearchID.setString("search_id", IDTextField.getText());
        dataModule11.queryDataset1.refresh();
//        dataModule11.queryDSReferences.refresh();
        dataModule11.queryDatasetProj.refresh();
      }
    }
    catch (Exception ex){
      ex.printStackTrace();
    }
  }

  void keepIDCheckBox_itemStateChanged(ItemEvent e) {
    if (keepIDCheckBox.isSelected()) {
//      DatasetTabs.topTabPane.setTitleAt(7, "Tabs keep Dataset ID");
    } else {
//      DatasetTabs.topTabPane.setTitleAt(7, "Tabs Change with Dataset");
    }
  }

  public static void setResultsEdit(boolean ans2) {
    editState = ans2;
    resultsTable.setEditable(ans2);
  }

  public static void selectIDField() {
    IDTextField.selectAll();
  }
}

