<%---------------------------------------------------------------------------------
  edit_dataset.jsp:  This jsp allows the user to update an existing dataset or
   add a new one to the database.  Displays an HTML form that allows the user
   to enter in the desired information.  The form is submitted to the 
   determine_action.jsp page.  

  Parameters:
   id - the dataset id number within the database
   mode - add/update
 
  Other files included:

  Author: Dan Sullivan
  Date 1-2-2003
---------------------------------------------------------------------------------%>


<html>
<head>
	<title>DTS: Edit Window</title>
	<script language=javascript src="../jscripts/misc.js"></script>
</head>

<%@ page errorPage="/dln/error.jsp" %>

<%@ page import="java.util.*,dln.format.*,dln.beans.*,dln.dba.*" %>

<body bgcolor=#e9e9e9 alink=#FFFFFF vlink=#FFFFFF link=#FFFFFF onLoad="showStatus( top.opener.parent.top_frame, 'Edit+Existing+Dataset' )">

<jsp:useBean id="dataset" scope="page" class="dln.beans.DatasetBean"/>
<jsp:useBean id="display" scope="session" class="dln.format.DisplayBean"/>

<%
	Vector projects = ProjectDBA.getAllActiveProjects();
	Vector users = UserDBA.getAllActiveUsers();

	String mode = request.getParameter( "mode" );
	if( mode == null )
		mode = "new";
%>

<%	
		if( request.getParameter( "error" ) == null )
		{ 
			if( mode.equals( "update" ) )
				DatasetDBA.getFromDB( dataset, Integer.parseInt( request.getParameter("id") ) );
			else
			{
				if( display.getDisplayView() == DisplayBean.PROJECT )
					dataset.setProject( display.getDisplayId() );
				else if( display.getDisplayView() == DisplayBean.LOADERS )
					dataset.setLoader( display.getDisplayId() );
				else if( display.getDisplayView() == DisplayBean.CHECKERS )
					dataset.setChecker( display.getDisplayId() );
			
				dataset.setDateToday();
			}
		}
		else
		{ 
			//dataset.reset(); // Not sure why?
			%><jsp:setProperty name="dataset" property="*"/><%
		}
%>

<center>


<table width=95% border=0 cellpadding=8 cellspacing=0 bgcolor=#dcdcdc>
	<form method="POST" action="determine_action.jsp">
	<% if( request.getParameter( "error" ) != null )
		{ %>
		<tr>
			<td colspan=4 bgcolor=#e9e9e9>
				<table border=0 cellpadding=0 cellspacing=0>
					<tr>
						<td><font color="red" align=right><b>Error: </font></b></td>
						<td>&nbsp;</td>
					</tr>
					<tr>
						<td>&nbsp;</td>
						<td><%= request.getParameter( "error" ) %></td>
					</tr>
				</table>
			</td>
		</tr>
	<% } %>
	<tr bgcolor=#336699>
		<td colspan=3>
			<font color=white size=+1>Data Tracking System: 
				<% if( mode.equals( "add" ) ) out.print( "Add Dataset" ); else out.print( "Edit Dataset" ); %>
			</font>
		</td>
		<td align=right><font color=white size=-1><%= dataset.getDate() %></td>
	</tr>
	<tr><td colspan=4 height=5 bgcolor=#e9e9e9></td></tr>
	<tr>
		<td align=right><b>Project: </b></td>
		<td>
			<select name="project">
			<%
				String pname = dataset.getProject();
				Enumeration penum = projects.elements();

				while( penum.hasMoreElements() )
				{
					ProjectBean proj = (ProjectBean) penum.nextElement();

					if( pname.equals( proj.getPname() ) )
						out.print( "<option value=\"" + proj.getPname() + "\" selected>" + proj.getPname()+ "</option>" );
					else
						out.print( "<option value=\"" + proj.getPname() + "\">" + proj.getPname() + "</option>" );
				}	
			%>
			</select>
		</td>
		<td align=right><b>Storm Id: </b></td>
		<td><input name="stormId" type=text value="<%= dataset.getStormId() %>" size=10 maxlength=16></td>
	</tr>
	<tr>
		<td align=right><b>Title: </b></td>
		<td colspan=3><input name="title" type=text value="<%= dataset.getTitle() %>" size=70 maxlength=255></td>
	</tr>
	<tr><td colspan=4 height=3 bgcolor=#e9e9e9></td></tr>
	<tr>
		<td align=right nowrap><b>Ingest Loc: </b></td>
		<td colspan=3><input type=text size=70 maxlength=255 name="ingest" value="<%= dataset.getIngest() %>"></td>
	</tr>
	<tr>
		<td align=right nowrap><b>Archive Loc: </b></td>
		<td colspan=3><input type=text size=70 maxlength=255 name="archive" value="<%= dataset.getArchive() %>"></td>
	</tr>

	<tr><td colspan=4 height=3 bgcolor=#e9e9e9></td></tr>

	<tr>
		<td align=right nowrap><b>External Contact: </b></td>
		<td colspan=3>
			<input type=text size=20 maxlength=32 name="extContact" value="<%= dataset.getExtContact() %>"> 
				&nbsp;&nbsp; <b>E-mail: </b> 
			<input type=text size=20 maxlength=100 name="extEmail" value="<%= dataset.getExtEmail() %>">
		</td>
	</tr>

	<tr>
		<td align=right><b>Internal Contact: </b></td>
		<td>
			<select name="intContact">
			<%
				if( mode.equals( "add" ) )
					out.print( "<option value=\"blank\">---Select---</option>" );

				boolean co_found = false;
				String int_contact = dataset.getIntContact(); 
				Enumeration co_enum = users.elements();

				while( co_enum.hasMoreElements() )
				{
					UserBean co = (UserBean) co_enum.nextElement();
					if( int_contact.equals( co.getUid() ) )
					{
						out.print( "<option value=" + co.getUid() + " selected>" + co.getFirstName() + "</option>" );
						co_found = true;
					}
					else
						out.print( "<option value=" + co.getUid() + ">" + co.getFirstName() +  "</option>" );
				}	

				if( !co_found && !int_contact.equals( "" ) )
				{
					UserBean co = UserDBA.getFromDB( int_contact );
					out.print( "<option value=" + co.getUid() + " selected>" + co.getFirstName() + "</option>" );
				}
			%>
			</select>
		</td>
		<td align=right nowrap><b>In Master List: </b></td>
		<td>
			<%
				if( dataset.isMaster() )
					out.print( "<input name=\"master\" type=checkbox checked>" );
				else
					out.print( "<input name=\"master\" type=checkbox>" );
			%>
		</td>
	</tr>
	<tr>
		<td colspan=2>&nbsp;</td>
		<td align=right nowrap><b>Readme Complete: </b></td>
		<td>
			<%
				if( dataset.isReadme() )
					out.print( "<input name=\"readme\" type=checkbox checked>" );
				else
					out.print( "<input name=\"readme\" type=checkbox>" );
			%>
		</td>
	</tr>

	<tr>
		<td align=right><b>Person Loading: </b></td>
		<td>
			<select name="loader">
			<%
				if( mode.equals( "add" ) )
					out.print( "<option value=\"blank\">---Select---</option>" );

				boolean l_found = false;
				String loader = dataset.getLoader();
				Enumeration l_enum = users.elements();

				while( l_enum.hasMoreElements() )
				{
					UserBean l = (UserBean) l_enum.nextElement();
					if( loader.equals( l.getUid() ) )
					{
						out.print( "<option value=" + l.getUid() + " selected>" + l.getFirstName() + "</option>" );
						l_found = true;
					}
					else
						out.print( "<option value=" + l.getUid() + ">" + l.getFirstName() + "</option>" );
				}	

				if( !l_found && !loader.equals( "" ) )
				{
					UserBean l = UserDBA.getFromDB( loader );
					out.print( "<option value=" + l.getUid() + " selected>" + l.getFirstName() + "</option>" );
				}
			%>
			</select>
		</td>
		<td align=right nowrap><b>Loading Complete: </b></td>
		<td>
			<%
				if( dataset.isLoaded() )
					out.print( "<input name=\"loaded\" type=checkbox checked>" );
				else
					out.print( "<input name=\"loaded\" type=checkbox>" );
			%>
		</td>
	</tr>

	<tr>
		<td align=right><b>Person Checking: </b></td>
		<td>
			<select name="checker">
			<%
				if( mode.equals( "add" ) )
					out.print( "<option value=\"blank\">---Select---</option>" );

				boolean ch_found = false;
				String checker = dataset.getChecker();
				Enumeration c_enum = users.elements();

				while( c_enum.hasMoreElements() )
				{
					UserBean c = (UserBean) c_enum.nextElement();
					if( checker.equals( c.getUid() ) )
					{
						out.print( "<option value=" + c.getUid() + " selected>" + c.getFirstName() + "</option>" );
						ch_found = true;
					}
					else
						out.print( "<option value=" + c.getUid() + ">" + c.getFirstName() + "</option>" );
				}	

				if( !ch_found && !checker.equals( "" ) )
				{
					UserBean c = UserDBA.getFromDB( checker );
					out.print( "<option value=" + c.getUid() + " selected>" + c.getFirstName() + "</option>" );
				}
			%>
			</select>
		</td>
		<td align=right nowrap><b>Codiac Checked: </b></td>
		<td>
			<%
				if( dataset.isChecked() )
					out.print( "<input name=\"checked\" type=checkbox checked>" );
				else
					out.print( "<input name=\"checked\" type=checkbox>" );
			%>
		</td>
	</tr>
	<tr>
	</tr>
	<tr>
	</tr>
	<tr><td colspan=4 height=3 bgcolor=#e9e9e9></td></tr>
	<tr>
		<td align=right nowrap><b>Remote URL: </b></td>
		<td colspan=3><input type=text size=70 maxlength=255 name="remoteUrl" value="<%= dataset.getRemoteUrl() %>"></td>
	</tr>
	<tr>
		<td valign=top colspan=4>
			<b>Notes: </b><br>
			<textarea name="notes" rows=12 cols=75><%= dataset.getNotes() %></textarea>
		</td>
	</tr>
	<tr><td colspan=4 height=3 bgcolor=#e9e9e9></td></tr>
	<tr bgcolor=#336699>
		<td colspan=4 align=right>
			<input type=hidden name="dsid" value="<%= dataset.getDsid() %>">
			<input type=hidden name="send_mail" value="n">

			<%
				if( mode.equals( "update" ) )
				{
					%>
						<input type=submit name=action value="Update"/>&nbsp;
						<input type=submit name=action value="Add Dataset" onClick="if( verify_add()) { return checkSendMail( this.form ); } else { return false; }"/>&nbsp;
						<input type=submit name=action value="Delete" onClick="return verify_delete()"/>&nbsp;&nbsp;
						<input type=submit name=action value="Cancel"/>
					<%
				}
				else
				{
					%>
						<input type=submit name=action value="Add Dataset" onClick="if( checkUsers(this.form) ) { return checkSendMail( this.form ); } else { return false; }"/>&nbsp;
						<input type=submit name=action value="Cancel"/>
					<%
				}
			%>
		</td>
	</tr>
	</form>
</table>
</center>

</html>
