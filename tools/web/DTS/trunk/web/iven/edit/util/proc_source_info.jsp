<%@ page import="dln.util.*,java.sql.*" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<%-- Define the beans used in this page. --%>
<jsp:useBean id="dataset" scope="request" class="dln.beans.DatasetBean" />

<%-- Need to load the source data set so the notes get loaded properly first. --%>
<% dataset.loadSourceDataset(request.getParameter("datasetId"), request.getParameter("selectedProduct")); %>


<jsp:setProperty name="dataset" property="*"/>
<%-- This is needed to properly clear the platform string if it is empty. --%>
<c:if test="${empty param.platformString}">
	<jsp:setProperty name="dataset" property="platformString" value="" />
</c:if>
<c:if test="${empty param.excluded}">
	<jsp:setProperty name="dataset" property="excluded" value="false" />
</c:if>

<%
	String action = request.getParameter( "action" );
	String onLoad = "";
	String msg = "";

	// Update the current data set in the database.
	if (action.equals( "Update" )) {
		try {
			dataset.updateSourceInfo();
            onLoad = "top.opener.location.reload(true); close();";
		} catch (SQLException e) {
            onLoad = "";
            msg = "<p>There has been an error trying to update the dataset.  Please contact the DTS developer.<p>";
            msg += "<p>"+e.getMessage()+"</p>";
		}
	}  
	//
	else if (action.equals("Delete")) {
		try {
			dataset.removeSource();
            onLoad = "top.opener.location.reload(true); close();";
		} catch (SQLException e) {
			onLoad = "";
            msg = "<p>There has been an error trying to remove the source dataset association.  Please contact the DTS developer.<p>";
            msg += "<p>"+e.getMessage()+"</p>";
		}
	}
	// Cancel any change done in the edit form.
	else {
		onLoad = "close();";
	}
%>

<%-- Return to the data set edit form to show errors or close the window --%>
<jsp:forward page="/iven/edit/source_info.jsp">
    <jsp:param name="onLoad" value="<%= onLoad %>" />
    <jsp:param name="error" value="<%= msg %>" />
</jsp:forward>
