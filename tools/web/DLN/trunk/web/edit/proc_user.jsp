<%---------------------------------------------------------------------
proc_user.jsp: process user's input in the edit_user.jsp form.
---------------------------------------------------------------------%>


<%@ page errorPage="/dln/error.jsp" %>
<%@ page import="java.util.*,dln.dba.*,dln.beans.*" %>

<jsp:useBean id="user" scope="request" class="dln.beans.UserBean"/>
<jsp:setProperty name="user" property="*"/>

<%
	String action = request.getParameter( "action" );	
	String mode = request.getParameter( "mode" );	
	String sourceUid = request.getParameter( "source_uid" );
	String onLoad = "";
	String msg = "";

	if( action == null || action.equals( "" ) )
	{
		if( mode.equals( "add" ) )
			action = "Add User";
		else
			action = "Update User";
	}


	if( action.equals( "Add User" ) )
	{
		boolean good = UserDBA.insertDB( user );
		if( good )
		{
			onLoad = "top.opener.parent.main.location=\'/dln/body/user_list.jsp?hlight=" + user.getUid() + "\'; showStatus( top.opener.parent.top_frame, \'User+Has+Been+Added\' ); close();";
			msg = "User Has Been Added";
		}
		else
		{
			msg = "There was an Error Adding the User.  Contact the developer.";	
		}
	}
	else if( action.equals( "Update User" ) )
	{
		boolean good = UserDBA.updateDB( user, sourceUid );
		if( good )
		{
			onLoad = "top.opener.parent.main.location=\'/dln/body/user_list.jsp?hlight=" + user.getUid() + "\'; showStatus( top.opener.parent.top_frame, \'User+Has+Been+Updated\' ); close();";
			msg = "User Has Been Updated";
		}
		else
		{
			msg = "There was an Error Updating the User.  Contact the developer.";	
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
	<title>User Edit Complete</title>
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
