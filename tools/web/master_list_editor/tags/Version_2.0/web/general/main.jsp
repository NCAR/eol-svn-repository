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

    <body onload="scrollTo('','','<h:outputText value="#{generalManager.project.projectId}" />','<h:outputText value="#{generalManager.category.categoryId}" />','<h:outputText value="#{generalManager.dataset.datasetId}" />');">

        <jsp:include page="pieces/header.jsp" />
        <jsp:include page="pieces/menu.jsp" />

        <div id="main">
            <jsp:include flush="true" page="pieces/error_report.jsp" />
            <c:choose>
                <c:when test="${generalManager.projectState && generalManager.listState}">
                    <jsp:include page="view/project.jsp" />
                </c:when>
                <c:when test="${generalManager.projectState && generalManager.editState}">
                    <jsp:include page="edit/project.jsp" />
                </c:when>
                <c:when test="${generalManager.projectState && generalManager.deleteState}">
                    <jsp:include page="delete/project.jsp" />
                </c:when>
                <c:when test="${generalManager.datasetState && generalManager.listState}">
                    <jsp:include page="view/dataset.jsp" />
                </c:when>
                <c:when test="${generalManager.datasetState && generalManager.editState}">
                    <jsp:include page="edit/dataset.jsp" />
                </c:when>
                <c:when test="${generalManager.datasetState && generalManager.deleteState}">
                    <jsp:include page="delete/dataset.jsp" />
                </c:when>
                <c:when test="${generalManager.categoryState && generalManager.listState}">
                    <jsp:include page="view/category.jsp" />
                </c:when>
                <c:when test="${generalManager.categoryState && generalManager.editState}">
                    <jsp:include page="edit/category.jsp" />
                </c:when>
                <c:when test="${generalManager.categoryState && generalManager.deleteState}">
                    <jsp:include page="delete/category.jsp" />
                </c:when>
                <c:otherwise>
                    <p>There is an issue.  The page could not be found.</p>
                </c:otherwise>
            </c:choose>
        </div>
    </body>
    </html>
</f:view>
