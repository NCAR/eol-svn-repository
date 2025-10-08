package dln.beans;

/******************************************************************************

ProjectBean.java: The ProjectBean is a simple container class to hold a single
record in the dln:project table.

Functions to Note:
-----------------
	public int populate( ResultSet rs ) throws SQLException
		-Populate this ProjectBean with the given ResultSet starting with index 1

	public int populate( int index, ResultSet rs ) throws SQLException
		-Populate this ProjectBean with the given ResultSet starting with the
			given index

Author: Dan Sullivan
Date: ??
******************************************************************************/

import java.io.Serializable;
import java.sql.*;

public class ProjectBean implements Serializable {

    private String project_id,prefix;
    private int active;

    public ProjectBean() {}

    public ProjectBean(String id) { setId(id); }

    public int getActive() { return active; }

    public String getId() { return project_id; }

    public String getPrefix() { return prefix; }

    public boolean isActive() { return active == 1; }

    public void setActive(boolean active) { setActive(active ? 1 : 0); }

    public void setActive(int active) { this.active = active == 1 ? 1 : 0; }

    public void setId(String id) { project_id = id; }

    public void setPrefix(String prefix) { this.prefix = prefix; }

    public String toString() { return getId(); }
}
