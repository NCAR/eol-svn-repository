<%-------------------------------------------------------------------
  loaders.jsp:  This jsp is part of the 'folder' style navigation in
   the left frame of the dln app.  The page displays all of the loaders 
   the user can choose from.  All of the active loaders are queried 
   from the database and displayed as subfolders of the Loaders folder.
   Each loader is then linked to the loader_init.jsp page to display
   the Loader listing view.

  Other files included:
    /dln/connect.jsp
 
  Author: Dan Sullivan
  Date: 1-2-2003
-------------------------------------------------------------------%>
<html>
<head>
	<title>DTS: Datasets by loaders</title>
	<script language=javascript src="../jscripts/misc.js"></script>

	<link rel="STYLESHEET" type="text/css" href="../main.css">
</head>

<body bgcolor=#336699 text=#FFFFFF link=white vlink=white alink=white>

<%@ page errorPage="/dln/error.jsp" %>
<%@ page import="java.util.*,dln.beans.*,dln.dba.*" %>

<br><br>
<table border=0 cellpadding=5 cellspacing=5>

	<tr><td>
		<table border=0 cellpadding=0 cellspacing=0>
			<tr>
				<td><img src=../images/project.gif width=25></img>&nbsp;&nbsp;</td>
				<td id=folder_item><b><a href=project.jsp>Project</a>
			</tr>
		</table>
	</td></tr>

	<tr><td>
		<table border=0 cellpadding=0 cellspacing=0>
			<tr>
				<td><img src=../images/loader.gif width=25></img>&nbsp;&nbsp;</td>
				<td id=folder_item><b><a href=left_root.html>Loaders</a></td>
			</tr>

			<%
				Enumeration loaders = UserDBA.getAllActiveUsers().elements();
				while( loaders.hasMoreElements() )
				{
					UserBean user = (UserBean) loaders.nextElement();
					out.print( "<tr><td>&nbsp;</td>" );
					out.print( "<td id=folder_item><font size=-1><li>" );
						out.print( "<a href=../body/loader_init.jsp?loader=" +
							user.getUid() + " target=main>" + user.getFirstName() + "</a>" );
					out.print( "</b></td></tr>" );
				}	
			%>

		</table>
	</td></tr>

	<tr><td>
		<table border=0 cellpadding=0 cellspacing=0>
			<tr>
				<td><img src=../images/checker.gif width=25></img>&nbsp;&nbsp;</td>
				<td id=folder_item><b><a href=checkers.jsp>Checkers</a></td>
			</tr>
		</table>
	</td></tr>
	<tr><td>&nbsp;</td></tr>
	<tr><td>&nbsp;</td></tr>
	<tr><td>
		<table border=0 cellpadding=0 cellspacing=0>
			<tr>
				<td><img src=../images/new.gif width=25></img>&nbsp;&nbsp;</td>
				<td id=folder_item><b><a href="javascript: editWindow( '../edit/edit_dataset.jsp?mode=add' )">Add New</a>
			</tr>
		</table>
	</td></tr>

	<tr><td>&nbsp;</td></tr>
	<tr><td id=folder_item align=center>
		<b><a href="../body/welcome.html" target=main>DTS Home</a></b>
	</td></tr>
</table>

</body></html>
