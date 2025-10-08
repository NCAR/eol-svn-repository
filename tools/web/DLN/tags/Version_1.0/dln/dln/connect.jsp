
<%-- This is no longer in use see the dln.dba.DLNConnection class --%>

<%-- Establish a connection to the database --%>
<jsp:useBean id="connection" scope="page" class="database.DBConnection">
	<%
		connection.setUrl( "jdbc:mysql://hurricane/dln?user=dln&password=codiac" );
		connection.connect();
	%>
</jsp:useBean>
