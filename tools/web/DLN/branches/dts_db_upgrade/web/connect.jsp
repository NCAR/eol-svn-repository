
<%-- This is no longer in use see the dln.dba.DLNConnection class --%>

<%-- Establish a connection to the database --%>
<jsp:useBean id="connection" scope="page" class="database.DBConnection">
	<%
		connection.setUrl( "jdbc:mysql://chinook/dts?user=dts&password=manage-all" );
		connection.connect();
	%>
</jsp:useBean>
