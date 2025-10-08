<%-----------------------------------------------------------------------
  refresh_view.js: This jsp simply refreshes the current view (either
   listing view or dataset view) to reflect new changes.  The parameter
   'id' is either the dataset to highlight in the listing view or the
   dataset to display in the dataset view.  This page forwards to one
   of the listing view pages or the view_dataset.jsp page.  This page
   is referenced by the javascript function refresh() in misc.js.

  Parameters:
   id - the id number othe dataset to highlight or to display, depending
    on the view.  If in listing view this number is simply forwarded to
    the ?????_view.jsp page.
 
  Author: Dan Sullivan
  Date: 12-31-2002
   
-----------------------------------------------------------------------%>

<%@ page errorPage="/error.jsp" %>
<%@ page import="dln.format.DisplayBean" %>

<jsp:useBean id="display" class="dln.format.DisplayBean" scope="session"/>
<jsp:useBean id="ds_id" class="java.lang.String" scope="page"/>

<%
	ds_id = request.getParameter( "id" );
	if( ds_id == null )
		ds_id = "-1";
%>

<% 
	if( display.getCurrentView() == DisplayBean.DATASET )
	{ 
		%>
			<jsp:forward page="view_dataset.jsp">
				<jsp:param name="id" value="<%= ds_id %>"/>
				<jsp:param name="number" value="<%= display.getCurrentDatasetNumber() %>"/>
			</jsp:forward>
		<%
	}
	else
	{
		%>
			<jsp:forward page="list_view.jsp"> 
				<jsp:param name="hlight" value="<%= ds_id %>"/>
			</jsp:forward>
		<% 
	} 
%>

<html>
<head>
	<title>Error: Cannot refresh</title>
</head>

<body bgcolor=white>

<font size=+2>Error: </font> Unable to refresh the current view. (refresh_view.jsp)
</body>
</html>
