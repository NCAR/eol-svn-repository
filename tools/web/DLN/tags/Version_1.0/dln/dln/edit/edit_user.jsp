
<%--------------------------------------------------------------------------
edit.user.jsp - displays the form to edit or add a user.
--------------------------------------------------------------------------%>

<%@ page errorPage="/dln/error.jsp" %>
<%@ page import="java.util.*,dln.dba.*,dln.beans.*" %>

<%
	String mode = request.getParameter( "mode" );
	
	if( mode == null )
		mode = "add";

	String title = "Add User";
	UserBean user;

	if( mode.equals( "edit" ) )
	{
		title = "Edit User";

		user = UserDBA.getFromDB( request.getParameter( "user" ) );		
		if( user.getEmail() == null )
			user.setEmail( "" );
	}
	else
	{
		user = new UserBean();
		user.setUid( "" );
		user.setFirstName( "" );
		user.setLastName( "" );
		user.setEmail( "" );
	}
%>

<html>
<head>
	<title>DTS: Edit User</title>
	<script language=javascript src="../jscripts/misc.js"></script>

	<link rel="STYLESHEET" type="text/css" href="../dln_body.css">
</head>	

<body onLoad="showStatus( top.opener.parent.top_frame, 'Edit+User' )">

<br><br><br>
<center>
<form method="POST" action="proc_user.jsp">

<table class=editTable width=95% border=0 cellpadding=8 cellspacing=0>

	<tr class=titleBar>
		<td colspan=2>
			Data Tracking System: <%= title %>
		</td>
	</tr>

	<tr><td colspan=2 class=spacer></td></tr>

	<tr>
		<td align=right><b>User Id: </b></td>
		<td>
			<input type=text name=uid size=32 maxsize=32 value="<%= user.getUid() %>">
		</td>
	</tr>

	<tr>
		<td align=right><b>First Name: </b></td>
		<td>
			<input type=text name=firstName size=50 maxsize=50 value="<%= user.getFirstName() %>">
		</td>
	</tr>

	<tr>
		<td align=right><b>Last Name: </b></td>
		<td>
			<input type=text name=lastName size=50 maxsize=50 value="<%= user.getLastName() %>">
		</td>
	</tr>

	<tr>
		<td align=right><b>E-mail: </b></td>
		<td>
			<input type=text name=email size=50 maxsize=100 value="<%= user.getEmail() %>">
		</td>
	</tr>


	<tr>
		<td align=right><b>Active: </b></td>
		<td>
		 	<%
				if( user.isActive() )
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
						<input type=submit name=action value="Add User">
						<input type=submit name=action value="Cancel">
					<%
				}
				else
				{
					%>
						<input type=submit name=action value="Update User">
						<input type=submit name=action value="Cancel">
						<input type=hidden name=source_uid value="<%= user.getUid() %>">
					<%
				}
			%>
		</td>
	</tr>	
</table>
</form>
</body>
</html>	
