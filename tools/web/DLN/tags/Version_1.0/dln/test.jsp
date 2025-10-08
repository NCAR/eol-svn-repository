<html>
<head>
	<title>Test Page</title>
</head>

<%@ page import="dln.beans.*,dln.dba.*,java.util.*"%>
<jsp:useBean id="user" class="dln.beans.UserBean" scope="page"/>
<body>

<table>
	<tr>
		<th>uid</th>
		<th>first_name</th>
		<th>last_name</th>
	</tr>
<%
	Vector vec = UserDBA.getAllUsers();
	
	
	for( int x = 0; x < vec.size(); x++ )
	{
		user = (UserBean) vec.get(x);

		out.print( user.getUid() + "<br>" );
		%>
				<tr>
					<td><jsp:getProperty name="user" property="uid"/></td>
					<td><jsp:getProperty name="user" property="firstName"/></td>
					<td><jsp:getProperty name="user" property="lastName"/></td>
				</tr>
		<%
	}
%>

</table>
</body></html>
