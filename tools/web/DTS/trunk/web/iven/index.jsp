<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<%
    response.addHeader("Pragma","no-cache");
    response.setHeader("Cache-Control","no-cache, no-store, must-revalidate");
    response.addHeader("Cache-Control", "pre-check=0, post-check=0");
    response.setDateHeader("Expires",0);
    response.addHeader("Refresh","1800");
%>

<jsp:useBean id="project" scope="page" class="dln.beans.ProjectBean" />

<html>
<head>
	<title>DMG Processing Inventory</title>
	<script language="javascript" src="${pageContext.request.contextPath}/dts.js"></script>
	<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/dts.css" />
	<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/iven/iven.css" />
</head>

<body>

<div id="titlebar">
    <p class="mainTitle">DTS: Processing Inventory</p>
    <p class="subTitle">Welcome to the DMG Data Tracking System - Processing Inventory and Status System</p>
</div>

<div class="smallHeaderMenu">
	<table class="navBar">
		<tr>
			<td><a href="${pageContext.request.contextPath}/dln/">DLN</a></td>
			<td class="dropdown"><a href="pre_iven">Status Notes for Projects Prior to IVEN</a></td>
			<td><a href="${pageContext.request.contextPath}">DTS Home</a></td>
			<td><a target="_blank" href="http://www.eol.ucar.edu/about/our-organization/cds/dmg/dmg-internal/dmg-documentation/manual/data-tracking-system-guide">
				<img src="${pageContext.request.contextPath}/images/help.gif" />
				</a></td>
		</tr>
	</table>
</div>

<table class="projectList">
	<tr class="header">
		<th>Available Projects:</th>
	</tr>

<c:forEach items="${project.activeProjects}" var="project">
	<tr class="listRow" onmouseover="javascript: rowHover(this, 'hoverRow');" onmouseout="javascript: rowHover(this, 'listRow');">
		<td class="projectCell">
			<div class="link"><a href="${pageContext.request.contextPath}/iven/dataset_list.jsp?projectId=${project.projectId}">${project.projectId}</a></div>
			<div class="name">${project.name}</div>
		</td>
	</tr>	
</c:forEach>
</table>


</body>
</html>