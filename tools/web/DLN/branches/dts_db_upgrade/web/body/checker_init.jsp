<%----------------------------------------------------------------
  checker_init.jsp: This jsp initializes the display bean in order
   to view the checkers listing.  This page sets the default values
   for the display and forwards the user to the checker_view.jsp
   page.

  Parameters:
   checker - this parameter denotes which checker to show in the 
    listing view

  Other files included:
   /dln/connect.jsp

  Session/application beans used:
   display
	
  Author: Dan Sullivan
  Date:  12-31-02
----------------------------------------------------------------%>

<%@ page errorPage="/error.jsp" %>
<%@ page import="dln.format.DisplayBean,dln.beans.*,dln.dba.*" %>

<body bgcolor=#e9e9e9 alink=#000066 vlink=#000066 link=#000066>

<%-- Init the needed beans and objects --%>
<jsp:useBean id="display" scope="session" class="dln.format.DisplayBean" />

<%
	String cname = request.getParameter( "checker" );
	UserBean checker = UserDBA.getFromDB( (new Integer(cname)).intValue() );

	if( checker != null )
	{
		if( display.getDisplayView() != DisplayBean.CHECKERS )
		{
			display.setShowLoaded( true );
			display.setShowDocumented( true );
			display.setShowChecked( false );
		}

		display.setDisplayView( DisplayBean.CHECKERS );

		%><jsp:setProperty name="display" property="displayId" param="checker"/><%

		%><jsp:forward page="list_view.jsp"/><%
	}
	else
	{
		%>
			<html>
			<head>
				<title>Checker Init</title>
				<link rel="STYLESHEET" type="text/css" href="../css/dln_body.css">
			</head>
			<body>

			<center>
			<br><br>
			<font size=+2>Error:</font> <font size=+1>Checker <%= cname %> not found in the database.

			</body></html>
		<%
	}
%>
