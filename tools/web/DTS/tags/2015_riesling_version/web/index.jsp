<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<jsp:useBean id="project" scope="page" class="dln.beans.ProjectBean" />

<html>
<head>
	<title>DMG: Data Tracking System</title>
	<script language="javascript" src="${pageContext.request.contextPath}/dts.js"></script>
	<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/dts.css" />
</head>

<body>

<div id="titlebar">
    <p class="mainTitle">DMG Data Tracking System</p>
    <p class="subTitle">Welcome to the DMG Data Tracking System</p>
</div>
<div class="smallHeaderMenu">
	<table class="navBar">
		<tr>
		<%--
			<td><a href="logout.jsp">Logout</a></td>
		--%>
			<td class="dropdown"></td>
			<td><a target="_blank" href="http://www.eol.ucar.edu/about/our-organization/cds/dmg/dmg-internal/dmg-documentation/manual/data-tracking-system-guide">
				<img src="${pageContext.request.contextPath}/images/help.gif" />
				</a></td>
		</tr>
	</table>
</div>

<table class="toolList">
	<tr class="header">
		<th>Active Projects</th>
		<th><a href="${pageContext.request.contextPath}/dln">DLN</a></th>
		<th><a href="${pageContext.request.contextPath}/iven">IVEN</a></th>
                <th>Stats</th>
	</tr>

<c:forEach items="${project.activeProjects}" var="project">
	<tr class="listRow" onmouseover="javascript: rowHover(this, 'hoverRow');" onmouseout="javascript: rowHover(this, 'listRow');">
		<td>${project.projectId}</td>
		<td class="toolLink"><a href="${pageContext.request.contextPath}/dln/?project=${project.projectId}">Enter</a></td>
		<td class="toolLink"><a href="${pageContext.request.contextPath}/iven/dataset_list.jsp?projectId=${project.projectId}">Enter</a></td>
                <td class="toolLink"><a href="${pageContext.request.contextPath}/stats/project_stats.jsp?projectId=${project.projectId}">Stats</a></td>
	</tr>
</c:forEach>
</table>


</body>
</html>
