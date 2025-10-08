<%----------------------------------------------------------------
  loader_init.jsp: This jsp initializes the display bean in order
   to view the loader listing.  This page sets the default values
   for the display and forwards the user to the loader_view.jsp
   page.

  Parameters:
   loader - this parameter denotes which loader to show in the 
    listing view

  Other files included:
   /dln/connect.jsp

  Session/application beans used:
   display
	
  Author: Dan Sullivan
  Date:  12-31-02
----------------------------------------------------------------%>

<%-- Import tag library and needed classes --%>
<%@ page errorPage="/error.jsp" %>
<%@ page import="dln.format.DisplayBean,dln.beans.*,dln.dba.*" %>

<%-- Init the needed beans and objects --%>
<jsp:useBean id="display" scope="session" class="dln.format.DisplayBean" />

<%
	String lname = request.getParameter( "loader" );
	UserBean loader = UserDBA.getFromDB( lname );

	if( loader != null )
	{
		if( display.getDisplayView() != DisplayBean.LOADERS )
		{
			display.setShowLoaded( false );
			display.setShowDocumented( true );
			display.setShowChecked( true );
		}
		display.setDisplayView( DisplayBean.LOADERS );
		
		%><jsp:setProperty name="display" property="displayId" param="loader"/><%

		%><jsp:forward page="list_view.jsp"/><%
	}
	else
	{
		%>
			<html>
			<head>
				<title>Loader Init</title>
				<link rel="STYLESHEET" type="text/css" href="../dln_body.css">
			</head>
			<body>

			<center>
			<br><br>
			<font size=+2>Error:</font> <font size=+1>Loader <%= lname %> not found in the database.

			</body></html>
		<%
	}
%>

