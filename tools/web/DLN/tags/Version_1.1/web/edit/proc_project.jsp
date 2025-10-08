<%---------------------------------------------------------------------
proc_project.jsp: process user's input in the edit_project.jsp form.
---------------------------------------------------------------------%>


<%@ page errorPage="/dln/error.jsp" %>
<%@ page import="java.util.*,dln.dba.*,dln.beans.*" %>

<jsp:useBean id="project" scope="request" class="dln.beans.ProjectBean"/>
<jsp:setProperty name="project" property="*"/>

<%
	String action = request.getParameter( "action" );	
	String mode = request.getParameter( "mode" );	
	String sourcePname = request.getParameter( "source_pname" );
	String onLoad = "";
	String msg = "";

	if( action == null || action.equals( "" ) )
	{
		if( mode.equals( "add" ) )
			action = "Add Project";
		else
			action = "Update Project";
	}


	if( action.equals( "Add Project" ) )
	{
		boolean good = ProjectDBA.insertDB( project );
		if( good )
		{
			onLoad = "top.opener.parent.main.location=\'/dln/body/project_list.jsp?hlight=" + project.getPname() + "\'; showStatus( top.opener.parent.top_frame, \'Project+Has+Been+Added\' ); close();";
			msg = "Project Has Been Added";
		}
		else
		{
			msg = "There was an Error Adding the Project.  Contact the developer.";	
		}
	}
	else if( action.equals( "Update Project" ) )
	{
		boolean good = ProjectDBA.updateDB( project, sourcePname );
		if( good )
		{
			onLoad = "top.opener.parent.main.location=\'/dln/body/project_list.jsp?hlight=" + project.getPname() + "\'; showStatus( top.opener.parent.top_frame, \'Project+Has+Been+Updated\' ); close();";
			msg = "Project Has Been Updated";
		}
		else
		{
			msg = "There was an Error Updating the Project.  Contact the developer.";	
		}
	}
	else
	{
		onLoad = "showStatus( top.opener.parent.top_frame, \'Edit+Cancelled\' ); close();";
		msg = "Edit Cancelled";
	}
%>

<html>
<head>
	<title>Project Edit Complete</title>
	<link rel="STYLESHEET" type="text/css" href="../dln_body.css">
	<script language=javascript src="../jscripts/misc.js"></script>
</head>
<body onLoad="<%= onLoad %>">
<center>
<br><br>
<h2><%= msg %></h2>
<br><br>
<input type=submit value="Close Window" onClick="self.close(); return false;">
</body>
</html>
