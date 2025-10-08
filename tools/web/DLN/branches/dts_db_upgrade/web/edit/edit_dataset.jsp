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
	Vector statuses = StatusDBA.getAllStatuses();
	
	statuses.add(0,new StatusBean(0,"unassigned",false));
	users.add(0,new UserBean(0,"unassigned",(String)null,true));
	
	HashMap userMap = UserDBA.createUserMap();
	HashMap statusMap = StatusDBA.createStatusMap();

	String mode = request.getParameter( "mode" );
	if( mode == null )
		mode = "new";
%>

<%	
		if( request.getParameter( "error" ) == null )
		{ 
			if( mode.equals( "update" ) )
				DatasetDBA.getFromDB(dataset,request.getParameter("id"));
			else
			{
				if( display.getDisplayView() == DisplayBean.PROJECT )
					dataset.addProject(display.getDisplayId());
				else if( display.getDisplayView() == DisplayBean.LOADERS )
					dataset.setLoaderId(Integer.parseInt(display.getDisplayId()));
				else if( display.getDisplayView() == DisplayBean.CHECKERS )
					dataset.setCheckerId(Integer.parseInt(display.getDisplayId()));
			}
		}
		else
		{ 
			//dataset.reset(); // Not sure why?
			%><jsp:setProperty name="dataset" property="*"/><%
		}
%>

<center>
<form method="POST" action="determine_action.jsp">
   <table width=95% border=0 cellpadding=8 cellspacing=0 bgcolor=#dcdcdc>
   <% if( request.getParameter( "error" ) != null ) { %>
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

      <tr bgcolor=#660000>
         <td colspan=3>
            <font color=white size=+1><b>Data Tracking System:</b>
               <% out.print(mode.equals("add") ? "Add Dataset" : "Edit Dataset" ); %>
            </font>
         </td>
         <td align=right><font color=white size=-1><%= dataset.getEnteredDate() == null ? "" : dataset.getEnteredDate() %></td>
      </tr>

      <tr><td colspan=4 height=5 bgcolor=#e9e9e9></td></tr>

      <tr>
         <td align=right><b>Dataset Id: </b></td>
         <td>
<%--
            <input type="text" name="datasetId" value="<%= dataset.getDatasetId() %>">
--%>
            <%= dataset.getDatasetId().equals("") ? "<input type=text name=datasetId>" : dataset.getDatasetId() %>
         </td>
         <td align=right><b>Dataset Type: </b></td>
         <td>
            <select name="datasetType">
               <option value="Source"<%= dataset.getDatasetType().equals("Source") ? " selected" : "" %>>Source</option>
               <option value="Processed"<%= dataset.getDatasetType().equals("Processed") ? " selected" : "" %>>Processed</option>
               <option value="Composite"<%= dataset.getDatasetType().equals("Composite") ? " selected" : "" %>>Composite</option>
               <option value="Imagery"<%= dataset.getDatasetType().equals("Imagery") ? " selected" : "" %>>Imagery</option>
               <option value="Link"<%= dataset.getDatasetType().equals("Link") ? " selected" : "" %>>Link</option>
            </select>
         </td>
      </tr>
      <tr>
         <td align=right><b>Title: </b></td>
         <td colspan=3><input name="name" type=text value="<%= dataset.getName() %>" size=65 maxlength=255></td>
      </tr>

      <tr><td colspan=4 height=3 bgcolor=#e9e9e9></td></tr>

      <tr>
         <td align=right nowrap><b>Ingest Loc: </b></td>
         <td colspan=3><input type=text size=65 maxlength=255 name="ingestDirectory" value="<%= dataset.getIngestDirectory() %>"></td>
      </tr>
      <tr>
         <td align=right nowrap><b>Data to Archive Loc: </b></td>
         <td colspan=3><input type=text size=65 maxlength=255 name="loadDirectory" value="<%= dataset.getLoadDirectory() %>"></td>
      </tr>
      <tr>
         <td align=right nowrap><b>Archive Loc: </b></td>
         <td colspan=3><input type=text size=65 maxlength=255 name="archiveDirectory" value="<%= dataset.getArchiveDirectory() %>"></td>
      </tr>

      <tr><td colspan=4 height=3 bgcolor=#e9e9e9></td></tr>

      <tr>
         <td align=right><b>Internal Contact: </b></td>
         <td>
            <select name="internalContactId">
            <%
               if(mode.equals("add"))
                  out.print( "<option value=0>---Select---</option>" );

               boolean co_found = false;
               UserBean int_contact = (UserBean)userMap.get(new Integer(dataset.getInternalContactId())); 
               Enumeration co_enum = users.elements();
               co_enum.nextElement(); // Need to skip the unassigned element here!
               while (co_enum.hasMoreElements()) {
                  UserBean co = (UserBean) co_enum.nextElement();
                  if (int_contact.getId() == co.getId()) {
                     out.print( "<option value=" + co.getId() + " selected>" + co.getName() + "</option>" );
                     co_found = true;
                  } else {
                     out.print( "<option value=" + co.getId() + ">" + co.getName() +  "</option>" );
                  }
               }	

               if (!co_found && !int_contact.getName().equals("")) {
                  out.print( "<option value=" + int_contact.getId() + " selected>" + int_contact.getName() + "</option>" );
               }
            %>
            </select>
         </td>
         <td align=right nowrap><b>Readme Complete: </b></td>
         <td>
            <input name="readme" type=checkbox <%= dataset.getReadme() ? "checked" : "" %>>
         </td>
      </tr>
      <tr>
         <td align=right><b>Person Loading: </b></td>
         <td>
            <select name="loaderId">
            <%
               if (mode.equals("add"))
                  out.print( "<option value=0>---Select---</option>" );

               boolean l_found = false;
               UserBean loader = (UserBean)userMap.get(new Integer(dataset.getLoaderId()));
               Enumeration l_enum = users.elements();
               while (l_enum.hasMoreElements()) {
                  UserBean l = (UserBean) l_enum.nextElement();
                  if (loader.getId() == l.getId()) {
                     out.print("<option value="+l.getId()+" selected>"+l.getName()+"</option>");
                     l_found = true;
                  } else {
                     out.print("<option value="+l.getId()+">"+l.getName()+"</option>");
                  }
               }

               if (!l_found && !loader.getName().equals("")) {
                  out.print("<option value="+loader.getId()+" selected>"+loader.getName()+"</option>" );
               }
            %>
            </select>
         </td>
         <td align=right nowrap><b>Loading Status: </b></td>
         <td>
            <select name="loadedStatusId">
            <%
               StatusBean loadStatus = (StatusBean)statusMap.get(new Integer(dataset.getLoadedStatusId()));
               Enumeration sEnum = statuses.elements();
               while (sEnum.hasMoreElements()) {
                  StatusBean s = (StatusBean)sEnum.nextElement();
                  out.println("<option value="+s.getId()+ (s.getId()==loadStatus.getId() ? " selected" : "") +">"+s.getName()+"</option>");
               }
            %>
            </select>
         </td>
      </tr>
      <tr>
         <td align=right><b>Person Checking: </b></td>
         <td>
            <select name="checkerId">
            <%
               if (mode.equals("add")) { out.print( "<option value=0>---Select---</option>" ); }
               boolean ch_found = false;
               UserBean checker = (UserBean)userMap.get(new Integer(dataset.getCheckerId()));
               Enumeration c_enum = users.elements();
               while (c_enum.hasMoreElements()) {
                  UserBean c = (UserBean) c_enum.nextElement();
                  if (checker.getId() == c.getId()) {
                     out.print("<option value="+c.getId()+" selected>"+c.getName()+"</option>");
                     ch_found = true;
                  } else {
                     out.print("<option value="+c.getId()+">"+c.getName()+"</option>");
                  }
               }

               if (!ch_found && !checker.getName().equals("")) {
                  out.print("<option value="+checker.getId()+" selected>"+checker.getName()+"</option>");
               }
            %>
            </select>
         </td>
         <td align=right nowrap><b>Checked Status: </b></td>
         <td>
            <select name="checkedStatusId">
            <%
               StatusBean checkedStatus = (StatusBean)statusMap.get(new Integer(dataset.getCheckedStatusId()));
               Enumeration cEnum = statuses.elements();
               while (cEnum.hasMoreElements()) {
                  StatusBean s = (StatusBean)cEnum.nextElement();
                  out.println("<option value="+s.getId()+ (s.getId()==checkedStatus.getId() ? " selected" : "") +">"+s.getName()+"</option>");
               }
            %>
            </select>
         </td>
      </tr>
	
      <tr><td colspan=4 height=3 bgcolor=#e9e9e9></td></tr>

      <tr>
         <td valign=top colspan=3><b>Notes: </b><br>
             <textarea name="note" rows=10 cols=60 wrap="virtual"><%= dataset.getNote() %></textarea>
         </td>
         <td valign=top colspan=1><b>Projects: </b><br>
             <select multiple size=10 name="projectId">
             <%
                Enumeration pEnum = projects.elements();
                while (pEnum.hasMoreElements()) {
                   ProjectBean p = (ProjectBean)pEnum.nextElement();
                   out.println("<option value="+p.getId()+ (dataset.hasProject(p.getId()) ? " selected" : "") +">"+p.getId()+"</option>");
                }
             %>
             </select>
         </td>
      </tr>
	
      <tr><td colspan=4 height=3 bgcolor=#e9e9e9></td></tr>

      <tr bgcolor=#660000>
         <td colspan=4 align=right>
            <input type=hidden name="datasetId" value="<%= dataset.getDatasetId() %>">
            <input type=hidden name="noteId" value="<%= dataset.getNoteId() %>">
            <input type=hidden name="send_mail" value="n">
            <% if (mode.equals("update")) { %>
                  <input type=submit name=action value="Update" onClick="if (checkUsers(this.form)) { return checkSendMail(this.form); } else { return false; }" />&nbsp;
                  <input type=submit name=action value="Add Dataset" onClick="if( verify_add(this.form) && checkUsers(this.form)) { return checkSendMail( this.form ); } else { return false; }"/>&nbsp;
                  <input type=submit name=action value="Delete" onClick="return verify_delete()"/>&nbsp;&nbsp;
                  <input type=submit name=action value="Cancel"/>
            <% } else {	%>
                  <input type=submit name=action value="Add Dataset" onClick="if( checkUsers(this.form) ) { return checkSendMail( this.form ); } else { return false; }"/>&nbsp;
                  <input type=submit name=action value="Cancel"/>
            <% } %>
         </td>
      </tr>
   </table>
</form>
</center>

</html>
