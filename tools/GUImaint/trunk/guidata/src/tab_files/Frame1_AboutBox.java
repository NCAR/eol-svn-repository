package tab_files;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

/**
 * <p>Title: GUImaint for CODIAC</p>
 * <p>Description: MySQL db version</p>
 * <p>Copyright: Copyright (c) 2004-2008</p>
 * <p>Company: NCAR/EOL</p>
 * @author Don Stott
 * @version 2.20
 */

public class Frame1_AboutBox extends JDialog implements ActionListener {

  JPanel panel1 = new JPanel();
  JPanel panel2 = new JPanel();
  JPanel insetsPanel1 = new JPanel();
  JPanel insetsPanel2 = new JPanel();
  JPanel insetsPanel3 = new JPanel();
  JButton button1 = new JButton();
  JLabel imageControl1 = new JLabel();
  ImageIcon imageIcon;
  JLabel label1 = new JLabel();
  JLabel label2 = new JLabel();
  JLabel label3 = new JLabel();
  JLabel label4 = new JLabel();
  BorderLayout borderLayout1 = new BorderLayout();
  BorderLayout borderLayout2 = new BorderLayout();
  FlowLayout flowLayout1 = new FlowLayout();
  FlowLayout flowLayout2 = new FlowLayout();
  GridLayout gridLayout1 = new GridLayout();
  String product = "GUImaint program for CODIAC";
  String version = "2.20";
  String copyright = "Copyright (c) 2004-2008";
  String comments = "works with riesling zedi8 MySQL database";
  JLabel label5 = new JLabel();

  public Frame1_AboutBox(Frame parent) {
    super(parent);
    enableEvents(AWTEvent.WINDOW_EVENT_MASK);
    try  {
      jbInit();
    }
    catch(Exception e) {
      e.printStackTrace();
    }
    //imageControl1.setIcon(imageIcon);
    pack();
  }

  private void jbInit() throws Exception  {
    //imageIcon = new ImageIcon(getClass().getResource("your image name goes here"));
    this.setTitle("About");
    setResizable(false);
    panel1.setLayout(borderLayout1);
    panel2.setLayout(borderLayout2);
    insetsPanel1.setLayout(flowLayout1);
    insetsPanel2.setLayout(flowLayout1);
    insetsPanel2.setBorder(new EmptyBorder(10, 10, 10, 10));
    gridLayout1.setRows(5);
    gridLayout1.setColumns(1);
    label1.setFont(new java.awt.Font("Dialog", 1, 13));
    label1.setText(product);
    label2.setFont(new java.awt.Font("Dialog", 1, 12));
    label2.setText("ver 2.88 (May 2009)");
    label3.setFont(new java.awt.Font("Dialog", 0, 12));
    label3.setRequestFocusEnabled(true);
    label3.setText("2004-2009 Copyright (c) UCAR");
    label4.setFont(new java.awt.Font("Dialog", 0, 12));
    label4.setText("works with zedi8 MySQL database on tsunami");
    insetsPanel3.setLayout(gridLayout1);
    insetsPanel3.setBorder(new EmptyBorder(10, 60, 10, 10));
    button1.setText("OK");
    button1.addActionListener(this);
    panel1.setBorder(BorderFactory.createRaisedBevelBorder());
    panel1.setMinimumSize(new Dimension(400, 145));
    panel1.setPreferredSize(new Dimension(420, 275));
    label5.setFont(new java.awt.Font("Dialog", 0, 12));
    label5.setText("author: Don Stott");
    insetsPanel2.add(imageControl1, null);
    panel2.add(insetsPanel2, BorderLayout.WEST);
    this.getContentPane().add(panel1, null);
    insetsPanel3.add(label1, null);
    insetsPanel3.add(label2, null);
    insetsPanel3.add(label4, null);
    insetsPanel3.add(label3, null);
    insetsPanel3.add(label5, null);
    this.getContentPane().add(insetsPanel1, BorderLayout.SOUTH);
    insetsPanel1.add(button1, null);
    panel2.add(insetsPanel3, BorderLayout.CENTER);
    panel1.add(panel2, BorderLayout.NORTH);
  }

  protected void processWindowEvent(WindowEvent e) {
    if(e.getID() == WindowEvent.WINDOW_CLOSING) {
      cancel();
    }
    super.processWindowEvent(e);
  }

  void cancel() {
    dispose();
  }

  public void actionPerformed(ActionEvent e) {
    if(e.getSource() == button1) {
      cancel();
    }
  }
}
