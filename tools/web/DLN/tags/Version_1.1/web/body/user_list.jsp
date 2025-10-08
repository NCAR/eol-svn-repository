<%------------------------------------------------------------------------
 user_list.jsp: displays all of the users in the DTS for editting
------------------------------------------------------------------------%>

<%@ page errorPage="/dln/error.jsp" %>
<%@ page import="java.util.*,dln.beans.*,dln.dba.*" %>

<html>
<head>
	<title>DTS: User List</title>
	<script language=javascript src=../jscripts/misc.js></script>
	<link rel="STYLESHEET" type="text/css" href="../dln_body.css">
</head>

<%
	String tmp = UserDBA.orderBy;
	UserDBA.orderBy = "last_name ASC";
	Vector users = UserDBA.getAllUsers();
	UserDBA.orderBy = tmp;

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
		<td colspan=5>
			<table width=100% border=0 cellpadding=0 cellspacing=0>
				<tr class=titleBar>
					<td width=50%>
						Data Tracking System Users
					</td>
					<td width=50% align=right>
						<%= users.size() %> Users
					</td>
				</tr>
			</table>
		</td>
	</tr>

	<tr class=blankBar>	
		<td colspan=5>
			<a href="javascript: editWindow( '../edit/edit_user.jsp?mode=add' );">Add New</a>
		</td>
	</tr>

	<tr>
		<td class=noSortHeading nowrap width=35%> User Id </td>
		<td class=noSortHeading nowrap width=35%> Name </td>
		<td class=noSortHeading nowrap width=15%> E-mail </td>
		<td class=noSortHeading nowrap width=15%> Active </td>
		<td class=noSortHeading nowrap width=15%> &nbsp; </td>
	</tr>

	<%
		Enumeration usrs = users.elements();

		while( usrs.hasMoreElements() )
		{
			UserBean user = (UserBean) usrs.nextElement();
			String trclass = "listRow";
			if( user.getUid().equals( hlight ) )
				trclass = "hlightRow";
			%>
				<tr class=<%= trclass %> onmouseover="cursorOver(this);" onmouseout="cursorOut(this);">
					<td><a target=_top href=/dln/?loader=<%= user.getUid() %>><%= user.getUid() %></a></td>
					<td><%= user.getFirstName() %> <%= user.getLastName() %></td>
					<td>
						<%
							if( user.getEmail() != null )
								out.println( user.getEmail() );
							else
								out.println( "&nbsp;" );
						%>
					</td>
					<td align=center>
						<%
							if( user.isActive() )
								out.println( "<font color=blue>Y</font>" );
							else
								out.println( "<font color=red>N</font>" );
						%>
					</td>

					<td align=center>
						<a href="javascript: editWindow( '../edit/edit_user.jsp?user=<%= user.getUid() %>&mode=edit' );">
							<img src=../images/edit.gif border=0>
						</a>
					</td>
				</tr>
			<%	

		}
	%>	

	<tr class=blankBar>	
		<td colspan=5>
			<a href="javascript: editWindow( '../edit/edit_user.jsp?mode=add' );">Add New</a>
		</td>
	</tr>

	<tr>
		<td colspan=5>
			<table width=100% border=0 cellpadding=0 cellspacing=0>
				<tr class=titleBar>
					<td width=50%>
						Data Tracking System Users
					</td>
					<td width=50% align=right>
						<%= users.size() %> Users
					</td>
				</tr>
			</table>
		</td>
	</tr>
</table>

</body>
</html>
		
