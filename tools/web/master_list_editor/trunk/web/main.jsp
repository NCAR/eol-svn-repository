<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core" %>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html" %>

<%
    response.addHeader("Pragma","no-cache");
    response.setHeader("Cache-Control","no-cache, no-store, must-revalidate");
    response.addHeader("Cache-Control", "pre-check=0, post-check=0");
    response.setDateHeader("Expires",0);
    response.addHeader("Refresh","1800");
%>

<f:view>
    <f:loadBundle basename="resources" var="resources" />
    
    <html>
    <head>
        <title><h:outputText value="#{resources.title}" /></title>
        <link rel="stylesheet" type="text/css"
            href="<h:outputText 
                value="#{facesContext.externalContext.requestContextPath}#{resources.css}" />" />
        <script language="JavaScript" src="<h:outputText 
                value="#{facesContext.externalContext.requestContextPath}#{resources.js}" />"></script>
    </head>

    <body onload="scrollTo('','','<h:outputText value="#{manager.project.projectId}" />','<h:outputText value="#{manager.classification.classificationId}" />','<h:outputText value="#{manager.dataset.datasetId}" />','<h:outputText value="#{manager.phase.phaseId}" />');">

        <jsp:include page="header.jsp" />
        <jsp:include page="menu.jsp" />

        <div id="main">
            <jsp:include flush="true" page="error_report.jsp" />
            <c:choose>
                <c:when test="${manager.projectState && manager.listState}">
                    <jsp:include page="project/list.jsp" />
                </c:when>
                <c:when test="${manager.projectState && manager.editState}">
                    <jsp:include page="project/edit.jsp" />
                </c:when>
                <c:when test="${manager.projectState && manager.deleteState}">
                    <jsp:include page="project/delete.jsp" />
                </c:when>
                <c:when test="${manager.datasetState && manager.listState}">
                    <jsp:include page="dataset/list.jsp" />
                </c:when>
                <c:when test="${manager.datasetState && manager.editState}">
                    <jsp:include page="dataset/edit.jsp" />
                </c:when>
                <c:when test="${manager.datasetState && manager.deleteState}">
                    <jsp:include page="dataset/delete.jsp" />
                </c:when>
                <c:when test="${manager.classificationState && manager.listState}">
                    <jsp:include page="classification/list.jsp" />
                </c:when>
                <c:when test="${manager.classificationState && manager.editState}">
                    <jsp:include page="classification/edit.jsp" />
                </c:when>
                <c:when test="${manager.classificationState && manager.deleteState}">
                    <jsp:include page="classification/delete.jsp" />
                </c:when>
                <c:when test="${manager.phaseState && manager.listState}">
                    <jsp:include page="phase/list.jsp" />
                </c:when>
                <c:when test="${manager.phaseState && manager.editState}">
                    <jsp:include page="phase/edit.jsp" />
                </c:when>
                <c:when test="${manager.phaseState && manager.deleteState}">
                    <jsp:include page="phase/delete.jsp" />
                </c:when>
                <c:otherwise>
                    <p>There is an issue.  The page could not be found.</p>
                </c:otherwise>
            </c:choose>
        </div>

    </body>
    </html>
</f:view>
