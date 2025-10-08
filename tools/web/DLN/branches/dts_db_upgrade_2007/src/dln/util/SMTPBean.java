package dln.util;

/**
 * The SMTPBean is a class pulled from the internet to help send e-mails using 
 * JSP.  An e-mail is sent, if requested, to the ingest, load, and check contacts
 * when a data set is added, updated, or versioned in the DLN.
 * 
 * @author jclawson
 * @author Dan Sullivan
 */
// ******************************************************************** 
// *  This code is distributed in the hope that it will be useful,    *
// *  but WITHOUT ANY WARRANTY; without even the implied warranty of  *
// *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.            *
// ********************************************************************
// Authored 8/3/00 by Jayson Falkner - Jayson@jspinsider.com
//                                                         
//   For a comprehensive tutorial as well as the current SMTP
// protocal go to http://www.jspinsider.com/tutorials/.    
//
///////////////////////////////////////////////////////////////////////

import java.io.*;
import java.net.*;
import java.sql.*;
import java.util.*;

import dln.beans.*;
 
public class SMTPBean implements Serializable {
	
	private static final long serialVersionUID = -8144726685234780062L;

	private Socket smtp;
	private BufferedReader input;
	private PrintStream output;
	private String smtpServer = "smtp.eol.ucar.edu";
	private String serverReply;
	private int port = 25;
	
	private String subject, message;
	private UserBean from;
	private List<UserBean> to;
	
	
	/**
	 * Create a new instance of a SMTPBean.
	 */
	public SMTPBean() {
		to = new ArrayList<UserBean>();
		subject = "";
		message = "";
	}
	
	/**
	 * Initialize the email message by determining who the message is from and who it should
	 * be sent to.
	 * @param dataset The data set containing the contacts who may send/receive the message.
	 * @param note The note to send with the message.
	 * @param users The mapping of known users in the DTS.
	 * @param noteTypes The String containing the types of the note.
	 */
	private void initialize(DatasetBean dataset, NoteBean note, Map<Integer, UserBean> users, String noteTypes) {
		// Determine who the email is going to be from.
		// 1.  Should be the author of the note if there is a note to be sent with the message. 
		if (note != null && note.getAuthor(users) != null) { from = note.getAuthor(users); }
		// 2.  If there is no note, then try to use the check contact for the data set.
		else if (dataset.getChecker(users) != null) { from = dataset.getChecker(users); }
		// 3.  Finally, user the internal contact since this must be set in the DTS.
		else { from = dataset.getInternalContact(users); }
		
		// Determine which contacts should receive the email.  All will receive the message
		// if the note does not exist or has a general type, otherwise it will be specific to
		// individual note types.
		if ((noteTypes.equals("") || noteTypes.contains("General") || noteTypes.contains("Ingest")) && dataset.getIngester(users) != null) {
			to.add(dataset.getIngester(users));
		}		
		if ((noteTypes.equals("") || noteTypes.contains("General") || noteTypes.contains("Load")) && dataset.getLoader(users) != null) {
			to.add(dataset.getLoader(users));
		}
		if ((noteTypes.equals("") || noteTypes.contains("General") || noteTypes.contains("Check")) && dataset.getChecker(users) != null) {
			to.add(dataset.getChecker(users));
		}
	}
	
	/**
	 * Send an email message when a new data set has been added to the DTS.
	 * @param dataset The data set added to the DTS.
	 * @param note The note generated when the data set was added.
	 * @return <code>true</code> if the email was sent successfully, <code>false</code>
	 * if it failed.
	 * @throws SMTPException if there was a problem sending the email.
	 * @throws SQLException if there was a problem loading information from the database
	 * needed to send the email.
	 */
	public boolean sendNew(DatasetBean dataset, NoteBean note) throws SMTPException, SQLException {
		Map<Integer, UserBean> users = (new UserBean()).getAllUsersMap();
		String noteTypes = note.getTypesAsString(NoteTypeBean.getNoteTypesMap());
		
		// Set up the to and from fields of the email.
		initialize(dataset, note, users, noteTypes);

		// Define the subject line for the email.
		subject = String.format("Dataset %s has been Added", dataset.getDatasetId());

		// Create the content of the email.
		message = "A dataset has been entered into the data loading notes page and you have been assigned as one of the contacts.  " +
				"Below is a summary of what has been entered into the notes page.  "+
				"For more information refer to the data loading notes page at "+
				"http://dmg.eol.ucar.edu/dln.\n\n" +
				"Name:           " + dataset.getName() + "\n" +
				"Project(s):     " + dataset.getProjects() + "\n" +
				"Entry Date:     " + dataset.getEntryDate().toString() + "\n" + 
				"Ingest Contact: " + (dataset.getIngester(users) == null ? "unassigned" : dataset.getIngester(users).getPersonName()) + "\n" +
				"Load Contact:   " + (dataset.getLoader(users) == null ? "unassigned" : dataset.getLoader(users).getPersonName()) + "\n" +
				"Check Contact:  " + (dataset.getChecker(users) == null ? "unassigned" : dataset.getChecker(users).getPersonName()) + "\n\n";

		// Add the note (if there is one) to the message.
		if (note.getNoteText().trim().equals("")) {
			message += "Please see the dataset page in the DLN for more information.";
		} else {
			message += "Note: ("+note.getAuthor(users).getPersonName()+")\n" + note.getNoteText();
		}

		// Send the email message.
		return sendMail();
	}
	
	/**
	 * Send an email message when a new version of a data set has been added to the DTS.
	 * @param dataset The data set versioned in the DTS.
	 * @param note The note generated when the data set was versioned.
	 * @return <code>true</code> if the email was sent successfully, <code>false</code>
	 * if it failed.
	 * @throws SMTPException if there was a problem sending the email.
	 * @throws SQLException if there was a problem loading information from the database
	 * needed to send the email.
	 */
	public boolean sendNewVersion(DatasetBean dataset, NoteBean note) throws SMTPException, SQLException {
		Map<Integer, UserBean> users = (new UserBean()).getAllUsersMap();
		String noteTypes = note.getTypesAsString(NoteTypeBean.getNoteTypesMap());
		
		// Set up the to and from fields for the email header.
		initialize(dataset, note, users, noteTypes);

		// Define the subject line for the email.
		subject = String.format("Dataset %s has a new Version", dataset.getDatasetId());

		// Define the content of the message.
		message = "Dataset " + dataset.getDatasetId() + " has been given a new version in the data loading notes and you are been assigned as one of the contacts.  " +
				"Below is a summary of what has been entered into the notes page.  "+
				"For more information refer to the data loading notes page at "+
				"http://dmg.eol.ucar.edu/dln.\n\n" +
				"Name:           " + dataset.getName() + "\n" +
				"Project(s):     " + dataset.getProjects() + "\n" +
				"Entry Date:     " + dataset.getEntryDate().toString() + "\n" + 
				"Ingest Contact: " + (dataset.getIngester(users) == null ? "unassigned" : dataset.getIngester(users).getPersonName()) + "\n" +
				"Load Contact:   " + (dataset.getLoader(users) == null ? "unassigned" : dataset.getLoader(users).getPersonName()) + "\n" +
				"Check Contact:  " + (dataset.getChecker(users) == null ? "unassigned" : dataset.getChecker(users).getPersonName()) + "\n\n";

		// Add the note to the message if there is one.
		if (note.getNoteText().trim().equals("")) {
			message += "Please see the dataset page in the DLN for more information.";
		} else {
			message += "Note: ("+note.getAuthor(users).getPersonName()+")\n" + note.getNoteText();
		}
		
		// Send the mail.
		return sendMail();
	}
	
	/**
	 * Send an email message when a data set has been updated in the DTS.
	 * @param dataset The data set updated in the DTS.
	 * @param note The note generated when the data set was updated.
	 * @return <code>true</code> if the email was sent successfully, <code>false</code>
	 * if it failed.
	 * @throws SMTPException if there was a problem sending the email.
	 * @throws SQLException if there was a problem loading information from the database
	 * needed to send the email.
	 */
	public boolean sendUpdate(DatasetBean dataset, NoteBean note) throws SMTPException, SQLException {
		Map<Integer, UserBean> users = (new UserBean()).getAllUsersMap();
		String noteTypes = note.getTypesAsString(NoteTypeBean.getNoteTypesMap());
		
		// Set up the to and from fields for the email header.
		initialize(dataset, note, users, noteTypes);

		// Define the subject line for the email.
		subject = String.format("Dataset %s has been Updated", dataset.getDatasetId());

		// Define the content of the message.
		message = "Dataset " + dataset.getDatasetId() + " has been updated in the data loading notes and you are assigned as one of the contacts.  " +
				"Below is a summary of what has been entered into the notes page.  "+
				"For more information refer to the data loading notes page at "+
				"http://dmg.eol.ucar.edu/dln.\n\n" +
				"Name:           " + dataset.getName() + "\n" +
				"Project(s):     " + dataset.getProjects() + "\n" +
				"Entry Date:     " + dataset.getEntryDate().toString() + "\n" + 
				"Ingest Contact: " + (dataset.getIngester(users) == null ? "unassigned" : dataset.getIngester(users).getPersonName()) + "\n" +
				"Load Contact:   " + (dataset.getLoader(users) == null ? "unassigned" : dataset.getLoader(users).getPersonName()) + "\n" +
				"Check Contact:  " + (dataset.getChecker(users) == null ? "unassigned" : dataset.getChecker(users).getPersonName()) + "\n\n";

		// Add the note to the message if there is one.
		if (note.getNoteText().trim().equals("")) {
			message += "Please see the dataset page in the DLN for more information.";
		} else {
			message += "Note: ("+note.getAuthor(users).getPersonName()+")\n" + note.getNoteText();
		}

		
		return sendMail();
	}
	
	/**
	 * Send the email message to the server.
	 * @return <code>true</code> if the sending of the email was successful, <code>false</code>
	 * if it failed.
	 * @throws SMTPException if there was a problem sending the message.
	 */
	private boolean sendMail() throws SMTPException{
		// Only try to send a message if there is someone to send it to!
		if (to.size() != 0) {		
			connect();
			hail();
			sendMessage();
			logout();
			return true;
		} else {
			return false;
		}
	}

	/**
	 * Establish a connection to the SMTP server.
	 * @throws SMTPException if there is a problem connecting to the server.
	 */
	public void connect() throws SMTPException {
		try {
			smtp = new Socket(smtpServer, port);
			input = new BufferedReader(new InputStreamReader(smtp.getInputStream()));
			output = new PrintStream(smtp.getOutputStream());
			serverReply = input.readLine();
			if (serverReply.charAt(0) == '2' || serverReply.charAt(0) == '3') {
			}
			else {
				throw new SMTPException("Error connecting to SMTP server " + smtpServer + " on port " + port);
			}
		}
		catch(Exception e) {
			throw new SMTPException(e.getMessage());
		}
	}	
	
	public void hail() throws SMTPException {
		if (submitCommand("HELO " + smtpServer))
			throw new SMTPException("Error occured during HELO command.");
		if (submitCommand("MAIL FROM: " + from.getEmail()))
			throw new SMTPException("Error during MAIL command.");
		
		for (UserBean user: to) {
			if (submitCommand("RCPT TO: " + user.getEmail()))
				throw new SMTPException("Error during RCPT command.");
		}
	}	
	
	public void sendMessage() throws SMTPException {

		if (submitCommand("DATA"))
			throw new SMTPException("Error during DATA command.");
		String header = "From: " + from.getPersonName() + " <" + from.getEmail() + ">\r\n";
		for (UserBean user: to) {
			header += "To: " + user.getPersonName() + " <" + user.getEmail() + ">\r\n";
		}
		header += "Subject: " + subject + "\r\n";
		
		if (submitCommand(header + message + "\r\n."))
			throw new SMTPException("Error during mail transmission.");
	}
	
	
	private boolean submitCommand(String command) throws SMTPException {
		try {
			output.print(command + "\r\n");
			serverReply = input.readLine();
			if (serverReply.charAt(0) == '4' || serverReply.charAt(0) == '5')
				return true;
			else
				return false;
		}
		catch(Exception e) {
			throw new SMTPException(e.getMessage());
		}
	} 
	
	public String getServerReply() {
		return serverReply;
	}

	public int getPort(){
		return port;
	}

	public void setPort(int newPort){
		port = newPort;
	}

	public String getSmtpServer(){
		return smtpServer;
	}

	public void setSmtpServer(String newSmtpServer){
		smtpServer = newSmtpServer;
	}
	
	public void logout() throws SMTPException {
		try {
			if (submitCommand("Quit"))
				throw new SMTPException("Error during QUIT command");
			input.close();
			output.flush();
			output.close();
			smtp.close();
		}
		catch(Exception e) {
		}
	}
}
