<%--------------------------------------------------------------------------
* The edit_project.jsp page is the editor form for adding or changing a
* DTS project.
--------------------------------------------------------------------------%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">

<%@ page errorPage="/error.jsp" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<%-- Define the beans used in this page. --%>
<jsp:useBean id="constants" scope="page" class="dln.util.DLNConstants" />
<jsp:useBean id="project" scope="request" class="dln.beans.ProjectBean" />

<%-- Load the project from the database if it is in edit mode. --%>
<c:if test="${param.mode == 'edit'}">
    <% project.loadProject(request.getParameter("project")); %>
</c:if>

<html>
<head>
	<title>${constants.shortTitle}: Edit Project</title>
	<script language=javascript src="${pageContext.request.contextPath}/dln.js"></script>
	<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/dln.css">
	<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/edit.css">
</head>	

<body onLoad="${param.onLoad == null ? 'showStatus( top.opener.parent.top_frame, \'Edit+Project\' )' : param.onLoad}">
<form action="${pageContext.request.contextPath}/edit/util/proc_project.jsp" method="POST">

<%-- Need to store the mode so the correct buttons are displayed on a reload. --%>
<input type="hidden" name="mode" value="${param.mode}">
<%-- Need to store the original project id in case the real project id changes 
     so the database can update the correct row --%>
<input type="hidden" name="originalProjectId" value="${project.originalProjectId}">

<div class="headerbar">
    ${constants.title}: 
    <c:choose>
       <c:when test="${param.mode == null || param.mode == 'add'}">Add Project</c:when>
       <c:otherwise>Edit Project</c:otherwise>
    </c:choose>
</div>
<div class="spacer"></div>

<%-- Display the error message if there was one passed back to the form. --%>
<c:if test="${!empty param.error && param.error != ''}">
    <div class="error">${param.error}</div>
    <div class="spacer"></div>
</c:if>

<div class="form">
	<table class="projectForm">
	    <tr>
	        <th>Project ID:</th>
	        <td><input type="text" name="projectId" size=16 maxlength=15 value="${project.projectId}" ></td>
	    </tr>
	    <tr>
	       <th>Full Name:</th>
	       <td><input type="text" name="name" size=50 maxlength=255 value="${project.name}"></td>
        </tr>
        <tr>
            <th>Begin Date:</th>
            <td><input type="text" name="beginDate" size=16 maxlength=10 value="${project.beginDate}"></td>
        </tr>
        <tr>
            <th>End Date:</th>
            <td><input type="text" name="endDate" size=16 maxlength=10 value="${project.endDate}"></td>
        </tr>
        <tr>
            <th>Min Latitude:</th>
            <td><input type="text" name="minLatitude" size=10 maxlength=10 value="${project.minLatitude}"></td>
        </tr>
        <tr>
            <th>Max Latitude:</th>
            <td><input type="text" name="maxLatitude" size=10 maxlength=10 value="${project.maxLatitude}"></td>
        </tr>
        <tr>
            <th>Min Longitude:</th>
            <td><input type="text" name="minLongitude" size=10 maxlength=10 value="${project.minLongitude}"></td>
        </tr>
        <tr>
            <th>Max Longitude:</th>
            <td><input type="text" name="maxLongitude" size=10 maxlength=10 value="${project.maxLongitude}"></td>
        </tr>
        <tr>
            <th>Charge Number:</th>
            <td><input type="text" name="chargeNumber" size=10 maxlength=10 value="${project.chargeNumber}"></td>
        </tr>
        <tr>
            <th>EMDAC Prefix(es):<br />(<span class="small">Separate w/Semicolons</span>)</th>
            <td><input type="text" name="datasetPrefixes" size=50 maxlength=255 value="${project.datasetPrefixes}"></td>
        </tr>
        <tr>
            <th>Active:</th>
            <td>
                <input type="checkbox" name="active" <c:if test="${project.active}">checked</c:if>>
            </td>
        </tr>
	</table>
</div>

<div class="spacer"></div>
<div class="headerbar">
    <div class="buttons">
	    <c:choose>
	        <c:when test="${param.mode == 'add'}">
	            <input type=submit name=action value="Add Project">
	            <input type=submit name=action value="Cancel">
	        </c:when>
	        <c:otherwise>
	            <input type=submit name=action value="Update Project">
	            <input type=submit name=action value="Cancel">
	        </c:otherwise>
	    </c:choose>
    </div>
</div>
</form>

</body>
</html>
