<%----------------------------------------------------------------
  project_init.jsp: This jsp initializes the display bean in order
   to view the project listing.  This page sets the default values
   for the display and forwards the user to the list_view.jsp
   page.

  Parameters:
   project - this parameter denotes which project to show in the 
    listing view

  Other files included:
   /dln/connect.jsp

  Session/application beans used:
   display
	
  Author: Dan Sullivan
  Date:  12-31-02
----------------------------------------------------------------%>


<%-- Import tag library and needed classes --%>
<%@ page errorPage="/dln/error.jsp" %>
<%@ page import="dln.format.DisplayBean,dln.dba.*,dln.beans.*" %>

<%-- Init the needed beans and objects --%>
<jsp:useBean id="display" scope="session" class="dln.format.DisplayBean" />


<%
	String pname = request.getParameter( "project" );
	ProjectBean project = ProjectDBA.getFromDB( pname );

	if( project != null )
	{	

		if( display.getDisplayView() != DisplayBean.PROJECT )
		{
			display.setShowLoaded( true );
			display.setShowDocumented( true );
			display.setShowChecked( true );
		}

		display.setDisplayView( DisplayBean.PROJECT );

		%><jsp:setProperty name="display" property="displayId" param="project"/><%

		%><jsp:forward page="list_view.jsp"/><%
	}
	else
	{
		%>
			<html>
			<head>
				<title>Project Init</title>
				<link rel="STYLESHEET" type="text/css" href="../css/dln_body.css">
			</head>
			<body>

			<center>
			<br><br>
			<font size=+2>Error:</font> <font size=+1>Project <%= pname %> not found in the database.

			</body></html>
		<%
	}
%>
