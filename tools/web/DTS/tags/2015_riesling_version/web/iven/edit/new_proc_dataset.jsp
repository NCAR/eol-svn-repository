<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<jsp:useBean id="dataset" scope="request" class="dln.beans.DatasetBean" />
<c:if test="${!empty param.datasetId}">
	<% dataset.loadDataset(request.getParameter("datasetId")); %>
</c:if>

<html>
<head>
	<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
	<title>Process Data Set Loader</title>
	<script language="javascript" src="${pageContext.request.contextPath}/dts.js"></script>
	<script language="javascript" src="${pageContext.request.contextPath}/iven/iven.js"></script>
	<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/dts.css" />
	<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/edit.css" />
</head>
<body onLoad="${empty param.onLoad ? '' : param.onLoad}">

<form action="${pageContext.request.contextPath}/iven/edit/util/proc_new_proc_dataset.jsp" method="POST">

<input type="hidden" name="projectId" value="${param.projectId}" />

<div class="headerbar">
    <span style="padding-left: 2px;">
        Load Process Data Set
    </span>
</div>
<div class="spacer"></div>

<%-- Only display the error section if there is an error to display. --%>
<c:if test="${!empty param.error && param.error != ''}">
    <div class="error">${param.error}</div>
    <div class="spacer"></div>
</c:if>

<div class="form">
	<table>
		<tr>
			<th class="required">Dataset ID:</th>
			<td><input size="15" maxlength="16" name="datasetId" type="text" value="${dataset.datasetId}" /></td>
		</tr>
	<c:if test="${!empty param.datasetId && empty param.error}">
		<tr>
			<th>Name:</th>
			<td>${dataset.name}</td>
		</tr>
	</c:if>
	</table>
</div>




<div class="spacer"></div>
<div class="headerbar">
    <div class="buttons">
		<input type="submit" name="action" value="Load" />
		<c:if test="${!empty param.datasetId && empty param.error}">
			<input type="submit" name="action" value="Add" />
		</c:if>
        <input type=submit name=action value="Cancel"/>
    </div>
</div>
</form>

	

</body>
</html>