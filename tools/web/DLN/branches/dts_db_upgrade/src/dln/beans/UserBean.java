package dln.beans;

/******************************************************************************

UserBean.java: The UserBean class is a simple container class to hold one 
record of the dln:user table.

Functions to Note:
------------------
	public int populate( ResultSet rs ) throws SQLException
		-Populates this UserBean with the data in the ResultSet starting with index 1

	public int populate( int index, ResultSet rs ) throws SQLException
		-Populates this UserBean with the data in the ResultSet starting with the
			given index

Author: Dan Sullivan
Date: ??

******************************************************************************/

import java.util.*;
import java.io.Serializable;
import java.sql.*;

public class UserBean implements Serializable {

    private String name, email;
    private int id, active;

    public UserBean() {}

    public UserBean(int id, String name, String email, boolean active) {
	setId(id);
	setName(name);
	setEmail(email);
	setActive(active);
    }

    public int getActive() { return active; }

    public String getEmail() { return email == null ? "" : email; }

    public int getId() { return id; }

    public String getName() { return name == null ? "" : name; }

    public boolean isActive() { return active == 1; }

    public void setActive(boolean active) { setActive(active ? 1 : 0); }

    public void setActive(int active) { this.active = active == 1 ? 1 : 0; }

    public void setEmail(String email) { this.email = email; }

    public void setId(int id) { this.id = id; }

    public void setName(String name) { this.name = name; }

    public String toString() { return getName(); }
}
