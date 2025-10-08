<!---------------------------------------------------------------------
  left_root.html: The first page displayed in the left frame.  Allows
    the user to select Project, Loaders or Checkers.
--------------------------------------------------------------------->
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">

<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<jsp:useBean id="projects" scope="page" class="dln.beans.ProjectBean" />
<jsp:useBean id="users" scope="page" class="dln.beans.UserBean" />

<html>
<head>
	<title>Left Root</title>
	<script language="javascript" src="${pageContext.request.contextPath}/dln.js"></script>
	<link rel="STYLESHEET" type="text/css" href="${pageContext.request.contextPath}/dln.css">
</head>

<body class="color">

    <div class="folder" style="padding-left: 30px">
        <a onClick="javascript: location='menu.jsp'" href="${pageContext.request.contextPath}/body/welcome.jsp" target="main">DTS Home</a>
    </div>


	<div class="folder">
	    <img class="folderIcon" src="${pageContext.request.contextPath}/images/project.gif" />
	    <a href="menu.jsp?expand=project">Project</a>
	</div>
	<c:if test="${param.expand == 'project'}">
	    <c:forEach items="${projects.activeProjects}" var="project">
	        <div class="folderEntry"><a href="${pageContext.request.contextPath}/body/dataset_list.jsp?project=${project.projectId}" target="main">${project.projectId}</a></div>
	    </c:forEach>
	</c:if>


	<div class="folder">
	    <img class="folderIcon" src="${pageContext.request.contextPath}/images/ingest.gif" />
	    <a href="menu.jsp?expand=ingest">Ingest</a>
	</div>
	<c:if test="${param.expand == 'ingest'}">
	    <c:forEach items="${users.activeUsers}" var="user">
	        <div class="folderEntry"><a href="${pageContext.request.contextPath}/body/dataset_list.jsp?ingest=${user.contactId}" target="main">${user.personName}</a></div>
	    </c:forEach>
	</c:if>


    <div class="folder">
        <img class="folderIcon" src="${pageContext.request.contextPath}/images/loader.gif" />
        <a href="menu.jsp?expand=load">Load</a>
    </div>
    <c:if test="${param.expand == 'load'}">
        <c:forEach items="${users.activeUsers}" var="user">
            <div class="folderEntry"><a href="${pageContext.request.contextPath}/body/dataset_list.jsp?load=${user.contactId}" target="main">${user.personName}</a></div>
        </c:forEach>
    </c:if>


    <div class="folder">
        <img class="folderIcon" src="${pageContext.request.contextPath}/images/checker.gif" />
        <a href="menu.jsp?expand=check">Check</a>
    </div>
    <c:if test="${param.expand == 'check'}">
        <c:forEach items="${users.activeUsers}" var="user">
            <div class="folderEntry"><a href="${pageContext.request.contextPath}/body/dataset_list.jsp?check=${user.contactId}" target="main">${user.personName}</a></div>
        </c:forEach>
    </c:if>
    
</body></html>
