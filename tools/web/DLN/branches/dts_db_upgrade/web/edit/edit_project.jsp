
<%--------------------------------------------------------------------------
edit.project.jsp - displays the form to edit or add a project.
--------------------------------------------------------------------------%>

<%@ page errorPage="/dln/error.jsp" %>
<%@ page import="java.util.*,dln.dba.*,dln.beans.*" %>

<%
	String mode = request.getParameter( "mode" );
	
	if( mode == null )
		mode = "add";

	String title = "Add Project";
	ProjectBean project;

	if( mode.equals( "edit" ) )
	{
		title = "Edit Project";

		project = ProjectDBA.getFromDB( request.getParameter( "project" ) );		
	}
	else
	{
		project = new ProjectBean();
		project.setId( "" );
		project.setPrefix( "" );
	}
%>

<html>
<head>
	<title>DTS: Edit Project</title>
	<script language=javascript src="../jscripts/misc.js"></script>
	<link rel="STYLESHEET" type="text/css" href="../css/dln_body.css">
</head>	

<body onLoad="showStatus( top.opener.parent.top_frame, 'Edit+Project' )">

<br><br><br>
<center>
<form method="POST" action="proc_project.jsp">

<table class=editTable width=95% border=0 cellpadding=8 cellspacing=0>

	<tr class=titleBar>
		<td colspan=2>
			Data Tracking System: <%= title %>
		</td>
	</tr>

	<tr><td colspan=2 class=spacer></td></tr>

	<tr>
		<td align=right><b>Project Name: </b></td>
		<td>
			<%= project.getId() %>
		</td>
	</tr>

	<tr>
		<td align=right><b>CODIAC Prefix: </b></td>
		<td>
			<%= project.getPrefix() == null ? "" : project.getPrefix() %>
		</td>
	</tr>

	<tr>
		<td align=right><b>Active: </b></td>
		<td>
		 	<%
				if( project.isActive() )
					out.println( "<input type=checkbox name=active checked>" );
				else
					out.println( "<input type=checkbox name=active>" );
			%>	
		</td>
	</tr>

	<tr><td colspan=2 class=spacer></td></tr>

	<tr class=titleBar>
		<td align=right colspan=2>
			<input type=hidden name=mode value="<%= mode %>">
			<%
				if( mode.equals( "add" ) )
				{
					%>
						<input type=submit name=action value="Add Project">
						<input type=submit name=action value="Cancel">
					<%
				}
				else
				{
					%>
						<input type=submit name=action value="Update Project">
						<input type=submit name=action value="Cancel">
						<input type=hidden name=source_pname value="<%= project.getId() %>">
					<%
				}
			%>
		</td>
	</tr>	
</table>
</form>
</body>
</html>	
