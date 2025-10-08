package dln.util;

import java.io.*;

/**
 * The DLNConstants class is a collection of access methods used by the DLN
 * to display certain information to the user.  This is to allow the developer
 * to change the value in a single location and it will be updated everywhere
 * it is used within the tool.
 * 
 * @author jclawson
 */
public class DLNConstants implements Serializable {

	private static final long serialVersionUID = 3628370515899967174L;

	/**
	 * Get the full name of the group.
	 * @return The group's full name.
	 */
	public String getGroupName() { return "Data Management Group"; }
	
	/**
	 * Get the abbreviation for the group's name.
	 * @return The group's short name.
	 */
	public String getGroupShortName() { return "DMG"; }
	
	/**
	 * Get the abbreviation for the name of the tool.
	 * @return The tool's short name.
	 */
	public String getShortTitle() { return "DTS - DLN"; }
	
	/**
	 * Get the full name for the the tool.
	 * @return The tool's full name.
	 */
	public String getTitle() { return "Data Tracking System - Data Loading Notes"; }
	
	/**
	 * Get the current version of the tool.
	 * @return The tool's current version.
	 */
	public String getVersion() { return "2.0"; }
}
