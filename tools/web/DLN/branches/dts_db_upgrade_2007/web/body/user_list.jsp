<%------------------------------------------------------------------------
* The user_list.jsp file displays a list of all of the users/contacts in the
* DTS database.  It is the only means of adding and editing a contact in
* the database.
------------------------------------------------------------------------%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">

<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ page errorPage="/error.jsp" %>
<%@ page import="java.util.*,dln.beans.*, dln.display.*" %>

<%-- Define the beans used in this page. --%>
<jsp:useBean id="constants" scope="page" class="dln.util.DLNConstants" />
<jsp:useBean id="users" scope="page" class="dln.beans.UserBean" />
<jsp:useBean id="userDisplay" scope="session" class="dln.display.UserDisplayBean" />
<jsp:setProperty name="userDisplay" property="*" />

<html>
<head>
	<title>${constants.shortTitle}: User List</title>
	<script language="javascript" src="${pageContext.request.contextPath}/dln.js"></script>
	<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/dln.css">
</head>

<body>
    <jsp:include page="/body/fragment/user_titlebar.jsp">
        <jsp:param name="userCount" value="<%= users.getAllUsers().size() %>" />
    </jsp:include>
    <jsp:include page="/body/fragment/new_user_bar.jsp" />
    
    <div class="listing">
	    <table class="userListing">
	        <tr class="header">
	            <th>
	                <a href="?sortField=<%= UserDisplayBean.SHORT_NAME_SORT %>">Short Name</a>
                    <c:if test="${userDisplay.sortByShortName}">
                        <img src="${pageContext.request.contextPath}/images/${userDisplay.reverseSort ? 'sorted_reverse.gif' : 'sorted.gif'}" />
                    </c:if>
	            </th>
	            <th>
	                <a href="?sortField=<%= UserDisplayBean.NAME_SORT %>">Name</a>
                    <c:if test="${userDisplay.sortByName}">
                        <img src="${pageContext.request.contextPath}/images/${userDisplay.reverseSort ? 'sorted_reverse.gif' : 'sorted.gif'}" />
                    </c:if>
	            </th>
	            <th>
                    <a href="?sortField=<%= UserDisplayBean.EMAIL_SORT %>">Email</a>
                    <c:if test="${userDisplay.sortByEmail}">
                        <img src="${pageContext.request.contextPath}/images/${userDisplay.reverseSort ? 'sorted_reverse.gif' : 'sorted.gif'}" />
                    </c:if>
	            </th>
	            <th>
                    <a href="?sortField=<%= UserDisplayBean.ACTIVE_SORT %>">Active</a>
                    <c:if test="${userDisplay.sortByActiveUser}">
                        <img src="${pageContext.request.contextPath}/images/${userDisplay.reverseSort ? 'sorted_reverse.gif' : 'sorted.gif'}" />
                    </c:if>
	            </th>
	            <th>&nbsp;</th>
	        </tr>
	        
	        <%
	        List<UserBean> userList = users.getAllUsers();
	        userDisplay.sort(userList);
	        %>
	        
	        <c:forEach var="user" items="<%= userList %>">
	           <c:set var="trclass" value="${user.contactIdString == param.hlight ? 'highlightRow' : 'listRow'}" />
	            <tr class="${trclass}" onmouseover="javascript: rowHover(this, 'hoverRow');" onmouseout="javascript: rowHover(this, '${trclass}');">
	                <td>${user.shortName}</td>
                    <td>${user.personName}</td>
                    <td>${user.email}</td>
	                <td>
	                    <c:choose>
	                        <c:when test="${user.active}">
	                            <span class="yes">Y</span>
	                        </c:when>
	                        <c:otherwise>
	                            <span class="no">N</span>
	                        </c:otherwise>
	                    </c:choose>
	                </td>
	                <td>
	                    <a href="javascript: editUser('${pageContext.request.contextPath}/edit/edit_user.jsp?mode=edit&contactId=${user.contactId}');">
	                        <img src="${pageContext.request.contextPath}/images/edit.gif" />
	                    </a>
	                </td>
	            </tr>
	        </c:forEach>
	        
	    </table>
    </div>
    

    <jsp:include page="/body/fragment/new_user_bar.jsp" />
    <jsp:include page="/body/fragment/user_titlebar.jsp">
        <jsp:param name="userCount" value="<%= users.getAllUsers().size() %>" />
    </jsp:include>
    
</body>
</html>
