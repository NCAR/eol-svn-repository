<%-------------------------------------------------------------------
	NOTE: this page is no longer in use!

  add_new.jsp:  This jsp is part of the 'folder' style navigation in
   the left frame of the dln app.  The page displays all of the projects 
   the user can choose from.  All of the active projects are queried 
   from the database and displayed as subfolders of the Projects folder.
   Each loader is then linked to the loader_init.jsp page to display
   the Project listing view.

  Other files included:
    /dln/connect.jsp
 
  Author: Dan Sullivan
  Date: 1-2-2003
-------------------------------------------------------------------%>
<html>
<head>
	<title>DTS: Dataset by project</title>
	<script language=javascript src="../jscripts/misc.js"></script>

	<link rel="STYLESHEET" type="text/css" href="../css/main.css">
</head>

<body bgcolor=#660000 text=#FFFFFF link=white vlink=white alink=white>

<%@ page errorPage="/dln/error.jsp" %>
<%@ taglib uri="/WEB-INF/tlds/dbtaglib.tld" prefix="db" %>
<%@ page import="java.util.Vector" %>
<%@ page import="java.util.Hashtable" %>
<%@ page import="java.util.Enumeration" %>

<jsp:useBean id="proj_result" scope="page" class="database.DBQueryResult"/>

<%@ include file="../connect.jsp" %>

<db:selectDB connection="connection" result="proj_result">
	SELECT name FROM project ORDER BY name
</db:selectDB>

<br><br>
<table border=0 cellpadding=5 cellspacing=5>
	<tr><td>
		<table border=0 cellpadding=0 cellspacing=0>		
			<tr>
				<td><img src=../images/project.gif width=25>&nbsp;&nbsp;</td>
				<td id=folder_item><b><a href=project.jsp>Project</a></td></tr>
		</table>
	</td></tr>


	<tr><td>
		<table border=0 cellpadding=0 cellspacing=0>
			<tr>
				<td><img src=../images/loader.gif width=25></img>&nbsp;&nbsp;</td>
				<td id=folder_item><b><a href=loaders.jsp>Dataset Loaders</a>
			</tr>
		</table>
	</td></tr>

	<tr><td>
		<table border=0 cellpadding=0 cellspacing=0>
			<tr>
				<td><img src=../images/checker.gif width=25></img>&nbsp;&nbsp;</td>
				<td id=folder_item><b><a href=checkers.jsp>Checkers</a>
			</tr>
		</table>
	</td></tr>

	<tr><td>&nbsp;</td></tr>
	<tr><td>&nbsp;</td></tr>

	<tr><td>
		<table border=0 cellpadding=0 cellspacing=0>
			<tr>
				<td><img src=../images/new.gif width=25></img>&nbsp;&nbsp;</td>
				<td id=folder_item><b><a href=left_root.html>Add New</a>
		<%
			Enumeration projs = proj_result.getAllData().elements();
			while( projs.hasMoreElements() )
			{
				Hashtable row = (Hashtable) projs.nextElement();
				out.print( "<tr><td>&nbsp;</td><td id=folder_item><font size=-1><li>" +
					"<a href=\"javascript: editWindow( \'../edit/add_dataset.jsp?method=new&" +
							"project=" + row.get( "name" ) + "\')\">" +
					     row.get( "name" )+ "</a></b></td></tr>\n" );
			}	
		%>
			</tr>
		</table>
	</td></tr>
</table>

<% connection.closeConnection(); %>
</body></html>
