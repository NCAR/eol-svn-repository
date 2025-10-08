package dts.port;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/**
 * The PortFrame class is window for the porting GUI.  It provides a means for running
 * the software, displaying the current spot in the process of the software, and displaying
 * any problems that were found during the porting.
 * 
 * @author jclawson
 */
public class PortFrame extends JFrame implements ActionListener, Runnable {

	private static final long serialVersionUID = -1044263232884142319L;

	private JButton startButton;
	private JTextArea exceptionArea;
	private JTextField statusField;
	private ToDmgDts porter;
	
	/**
	 * Create a new instance of a PortFrame.
	 * @param porter The controller for the porting process.
	 */
	public PortFrame(ToDmgDts porter) {
		this.porter = porter;
		porter.setDisplayFrame(this);
		setSize(1024, 768);
		getContentPane().add(buildDisplayPanel());
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	}
	
	/**
	 * Handle events such as button clicks, etc.
	 * @param evt The event that triggered the action.
	 */
	public void actionPerformed(ActionEvent evt) {
		// Start the porting and turn of the start button when the start button has been pressed.
		if (evt.getSource().equals(startButton)) {
			// Turn off the start button so multiple ports don't end up running.
			startButton.setEnabled(false);
			// Clear the exception area of all text so it only contains the newest run.
			exceptionArea.setText("");
			// Clear the data already in the porting class.
			porter.clearAllData();
			// Need to start a new thread so the GUI can update during the run of the porting.
			(new Thread(this)).start();
		}
	}

	/**
	 * Add a message to the display log when an exception was thrown.
	 * @param msg The error message to display.
	 */
	public void appendExceptionMessage(String msg) {
		exceptionArea.append("\t");
		exceptionArea.append(msg);
		exceptionArea.append("\n");
	}
	
	/**
	 * Append a general message to the displayed log.
	 * @param msg The message to display.
	 */
	public void appendMessage(String msg) {	
		exceptionArea.append(msg);
		exceptionArea.append("\n");
	}

	/**
	 * Create the display for the frame.
	 * @return The panel containing the frame's display.
	 */
	private JPanel buildDisplayPanel() {
		// Initialize the panel.
		JPanel panel = new JPanel();
		GridBagConstraints gbc = new GridBagConstraints();
		panel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		panel.setLayout(new GridBagLayout());
		
		// Add the status field to the panel.
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.weightx = 1;
		gbc.weighty = 0;
		gbc.anchor = GridBagConstraints.CENTER;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		panel.add(statusField = new JTextField("Waiting to start the porting..."), gbc);
		statusField.setEditable(false);
		
		// Add the start button to the panel.
		gbc.gridx = 1;
		gbc.weightx = 0;
		gbc.fill = GridBagConstraints.NONE;
		panel.add(startButton = new JButton("Start"), gbc);
		startButton.addActionListener(this);
		
		// Add the log area to the panel.
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.gridwidth = 2;
		gbc.weightx = 1;
		gbc.weighty = 1;
		gbc.fill = GridBagConstraints.BOTH;
		panel.add(new JScrollPane(exceptionArea = new JTextArea()), gbc);
		exceptionArea.setEditable(false);
		exceptionArea.setWrapStyleWord(true);
		exceptionArea.setLineWrap(true);
		
		
		return panel;
	}

	/**
	 * Create a dialog for the user to input the name of the machine the DTS database
	 * is to be ported to.
	 * @return The name of the machine entered by the user.
	 */
	public String machineInquire() {
		String machineName = "";
		// Continue asking the question until the user actually inputs a value.
		while (machineName == null || machineName.trim().equalsIgnoreCase("")) {
			machineName =  JOptionPane.showInputDialog(this, "Enter the name of the machine to contain the DTS database", "Database Machine Entry", JOptionPane.QUESTION_MESSAGE);
		}
		return machineName;
	}
	
	/**
	 * Run the port script as a part of a new Thread.  This allows the GUI to
	 * update as the porting progresses and doesn't wait until it has finished.
	 */
	public void run() {
		try {
			porter.port();			
			startButton.setEnabled(true);
		} catch (Exception e) {
			exceptionArea.append(e.getMessage());
			e.printStackTrace();
		}
	}

	/**
	 * Set the text to be displayed in the status text field.
	 * @param msg The message to be displayed.
	 */
	public void setStatus(String msg) { statusField.setText(msg); }
}
