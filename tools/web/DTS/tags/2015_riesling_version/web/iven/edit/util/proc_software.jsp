<%@ page import="dln.util.*,java.sql.*" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<%-- Define the beans used in this page. --%>
<jsp:useBean id="software" scope="request" class="dln.beans.SoftwareBean" />
<jsp:setProperty name="software" property="*" />

<%-- Need to load the source data set so the notes get loaded properly first. --%>
<%-- dataset.loadSourceDataset(request.getParameter("datasetId"), request.getParameter("selectedProduct")); --%>



<%
	String action = request.getParameter( "action" );
	String onLoad = "";
	String msg = "";

	// Update the current data set in the database.
	if (action != null && action.equals( "Update" )) {
		try {
			software.update(request.getParameter("datasetId"));
            onLoad = "top.opener.location.reload(true); close();";
		} catch (SQLException e) {
            onLoad = "";
            msg = "<p>There has been an error trying to update the software for the dataset.  Please contact the DTS developer.<p>";
            msg += "<p>"+e.getMessage()+"</p>";
		}
	}
	else if (action != null && action.equals("Add")) {
		try {
			software.insert(request.getParameter("datasetId"));
            onLoad = "top.opener.location.reload(true); close();";
		} catch (SQLException e) {
            onLoad = "";
            msg = "<p>There has been an error trying to insert the software for the dataset.  Please contact the DTS developer.<p>";
            msg += "<p>"+e.getMessage()+"</p>";
		}
	}
	else if (action != null && action.equals("Delete")) {
		try {
			software.delete(request.getParameter("datasetId"));
            onLoad = "top.opener.location.reload(true); close();";
		} catch (SQLException e) {
			onLoad = "";
            msg = "<p>There has been an error trying to remove the software dataset association.  Please contact the DTS developer.<p>";
            msg += "<p>"+e.getMessage()+"</p>";
		}
	}
	// Cancel any change done in the edit form.
	else if (action != null && action.equals("Cancel")) {
		onLoad = "close();";
	}
%>

<%-- Return to the data set edit form to show errors or close the window --%>
<jsp:forward page="/iven/edit/software.jsp">
    <jsp:param name="onLoad" value="<%= onLoad %>" />
    <jsp:param name="error" value="<%= msg %>" />
    <jsp:param name="selectionId" value="${param.selectionId}" />
    <jsp:param name="mode" value="${param.mode}" />
</jsp:forward>
