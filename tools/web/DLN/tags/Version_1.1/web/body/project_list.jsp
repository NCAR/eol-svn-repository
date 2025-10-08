<%------------------------------------------------------------------------
 project_list.jsp: displays all of the projects in the DTS for editting
------------------------------------------------------------------------%>

<%@ page errorPage="/dln/error.jsp" %>
<%@ page import="java.util.*,dln.beans.*,dln.dba.*" %>

<html>
<head>
	<title>DTS: Project List</title>
	<script language=javascript src=../jscripts/misc.js></script>
	<link rel="STYLESHEET" type="text/css" href="../dln_body.css">
</head>

<%
	Vector projects = ProjectDBA.getAllProjects();

	String hlight = request.getParameter( "hlight" );
	if( hlight == null ) hlight = ""; 	
%>

<body>
<br>
<br>
<br>
<center>
<table border=0 cellpadding=2 cellspacing=1 class=listTable width=50%>

	<tr>
		<td colspan=4>
			<table width=100% border=0 cellpadding=0 cellspacing=0>
				<tr class=titleBar>
					<td width=50%>
						Data Tracking System Projects
					</td>
					<td width=50% align=right>
						<%= projects.size() %> Projects
					</td>
				</tr>
			</table>
		</td>
	</tr>

	<tr class=blankBar>	
		<td colspan=5>
			<a href="javascript: editWindow( '../edit/edit_project.jsp?mode=add' );">Add New</a>
		</td>
	</tr>

	<tr>
		<td class=noSortHeading nowrap width=35%> Project Name </td>
		<td class=noSortHeading nowrap width=35%> CODIAC Prefix </td>
		<td class=noSortHeading nowrap width=15%> Active </td>
		<td class=noSortHeading nowrap width=15%> &nbsp; </td>
	</tr>

	<%
		Enumeration pjs = projects.elements();

		while( pjs.hasMoreElements() )
		{
			ProjectBean pj = (ProjectBean) pjs.nextElement();
			String trclass = "listRow";
			if( pj.getPname().equals( hlight ) )
				trclass = "hlightRow";
			%>
				<tr class=<%= trclass %> onmouseover="cursorOver(this);" onmouseout="cursorOut(this);">
					<td><a target=_top href=/dln/?project=<%= pj.getPname() %>><%= pj.getPname() %></a></td>
					<td align=center><%= pj.getStormIdPrefix() %>.xxx</td>
					<td align=center>
						<%
							if( pj.isActive() )
								out.println( "<font color=blue>Y</font>" );
							else
								out.println( "<font color=red>N</font>" );
						%>
					</td>

				<td align=center>
					<a href="javascript: editWindow( '../edit/edit_project.jsp?project=<%= pj.getPname() %>&mode=edit' );">
						<img src=../images/edit.gif border=0>
					</a>
				</td>
				</tr>
			<%	

		}
	%>	

	<tr class=blankBar>	
		<td colspan=5>
			<a href="javascript: editWindow( '../edit/edit_project.jsp?mode=add' );">Add New</a>
		</td>
	</tr>

	<tr>
		<td colspan=4>
			<table width=100% border=0 cellpadding=0 cellspacing=0>
				<tr class=titleBar>
					<td width=50%>
						Data Tracking System Projects
					</td>
					<td width=50% align=right>
						<%= projects.size() %> Projects
					</td>
				</tr>
			</table>
		</td>
	</tr>
</table>

</body>
</html>
		
