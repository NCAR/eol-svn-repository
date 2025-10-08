<%------------------------------------------------------------------------
* The project_list.jsp file displays a list for all of the projects in the
* DTS database.  It is the only means of adding and editing a project in
* the database.
------------------------------------------------------------------------%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">

<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ page errorPage="/dln/error.jsp" %>
<%@ page import="java.util.*,dln.beans.*,dln.display.*" %>

<%
    response.addHeader("Pragma","no-cache");
    response.setHeader("Cache-Control","no-cache, no-store, must-revalidate");
    response.addHeader("Cache-Control", "pre-check=0, post-check=0");
    response.setDateHeader("Expires",0);
    response.addHeader("Refresh","1800");
%>

<%-- Define the beans used in this page. --%>
<jsp:useBean id="constants" scope="page" class="dln.util.DLNConstants" />
<jsp:useBean id="projects" scope="page" class="dln.beans.ProjectBean" />
<jsp:useBean id="projectDisplay" scope="session" class="dln.display.ProjectDisplayBean" />
<jsp:setProperty name="projectDisplay" property="*" />

<html>
<head>
	<title>${constants.shortTitle}: Project List</title>
	<script language="javascript" src="${pageContext.request.contextPath}/dln/dln.js"></script>
	<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/dts.css" />
</head>


<body>
    <jsp:include page="/dln/body/fragment/project_titlebar.jsp">
        <jsp:param name="projectCount" value="<%= projects.getAllProjects().size() %>" />
    </jsp:include>
    <jsp:include page="/dln/body/fragment/new_project_bar.jsp" />

    <div class="listing">
	    <table class="projectListing">
	        <tr class="header">
	            <th>
	               <a href="?sortField=<%= ProjectDisplayBean.PROJECT_ID_SORT %>">Project ID</a>
                   <c:if test="${projectDisplay.sortByProjectId}">
                        <img src="${pageContext.request.contextPath}/images/${projectDisplay.reverseSort ? 'sorted_reverse.gif' : 'sorted.gif'}" />
                   </c:if>
	            </th>
	            <th>
	                <a href="?sortField=<%= ProjectDisplayBean.BEGIN_DATE_SORT %>">Begin Date</a>
	                <c:if test="${projectDisplay.sortByBeginDate}">
	                    <img src="${pageContext.request.contextPath}/images/${projectDisplay.reverseSort ? 'sorted_reverse.gif' : 'sorted.gif'}" />
	                </c:if>
	            </th>
	            <th>
	                <a href="?sortField=<%= ProjectDisplayBean.END_DATE_SORT %>">End Date</a>
                    <c:if test="${projectDisplay.sortByEndDate}">
                        <img src="${pageContext.request.contextPath}/images/${projectDisplay.reverseSort ? 'sorted_reverse.gif' : 'sorted.gif'}" />
                    </c:if>
                </th>
	            <th>Latitude</th>
	            <th>Longitude</th>
	            <th>Dataset ID Prefix(es)</th>
	            <th>Active</th>
	            <th>&nbsp;</th>
	        </tr>
	        
	        <%
	        // Determine the sorting for the project list.
	        List<ProjectBean> projectList = projects.getAllProjects();
	        projectDisplay.sort(projectList);
	        %>
	        
	        <c:forEach var="project" items="<%= projectList %>">
	            <c:set var="trclass" value="${project.projectId == param.hlight ? 'highlightRow' : 'listRow'}" />
	            <tr class="${trclass}" onmouseover="javascript: rowHover(this, 'hoverRow');" onmouseout="javascript: rowHover(this, '${trclass}');">
	                <td><a href="${pageContext.request.contextPath}/dln/body/dataset_list.jsp?project=${project.projectId}">${project.projectId}</a></td>
	                <td>${project.beginDate}</td>
	                <td>${project.endDate}</td>
	                <td>${project.minLatitude} / ${project.maxLatitude}</td>
	                <td>${project.minLongitude} / ${project.maxLongitude}</td>
	                <td>${project.datasetPrefixes}</td>
	                <td>
	                    <c:choose>
	                        <c:when test="${project.active}">
	                            <span class="yes">Y</span>
	                        </c:when>
	                        <c:otherwise>
	                            <span class="no">N</span>
	                        </c:otherwise>
	                    </c:choose>
	                </td>
	                <td>
	                    <a href="javascript: editProject('${pageContext.request.contextPath}/dln/edit/edit_project.jsp?mode=edit&project=${project.projectId}');">
	                        <img src="${pageContext.request.contextPath}/images/edit.gif" />
	                    </a>
	                </td>
	            </tr>
	        </c:forEach>
	        
	    </table>
    </div>

    <jsp:include page="/dln/body/fragment/new_project_bar.jsp" />
    <jsp:include page="/dln/body/fragment/project_titlebar.jsp">
        <jsp:param name="projectCount" value="<%= projects.getAllProjects().size() %>" />
    </jsp:include>

</body>
</html>
