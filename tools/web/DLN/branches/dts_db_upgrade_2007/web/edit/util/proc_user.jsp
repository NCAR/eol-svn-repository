<%---------------------------------------------------------------------
* The proc_user.jsp file is a utility to process updates to users
* which includes adding a new user/contact.
---------------------------------------------------------------------%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ page import="java.sql.*" %>

<%-- Define the beans used on the page. --%>
<jsp:useBean id="user" scope="request" class="dln.beans.UserBean"/>
<jsp:setProperty name="user" property="*"/>
<jsp:useBean id="onLoad" scope="page" class="java.lang.String" />
<jsp:useBean id="msg" scope="page" class="java.lang.String" />

<%-- Determine which action was called in the edit form and update the database accordingly. --%>
<c:choose>
    <c:when test="${param.action == 'Add User'}">
        <%
        try {
        	user.insert();
            onLoad = "top.opener.parent.left.location.reload(true); top.opener.parent.main.location=\'/dln/body/user_list.jsp?hlight=" + user.getContactId() + "\'; showStatus( top.opener.parent.top_frame, \'User+Has+Been+Added\' ); close();";
            //msg = "User Has Been Added";
        } catch (SQLException e) {
            msg = "<p>There was an Error Adding the User.  Contact the DTS developer.</p>";
            msg += "<p>"+e.getMessage()+"</p>";
        }
        %>
    </c:when>
    <c:when test="${param.action == 'Update User'}">
        <%
        try {
        	user.update();
            onLoad = "top.opener.parent.left.location.reload(true); top.opener.parent.main.location=\'/dln/body/user_list.jsp?hlight=" + user.getContactId() + "\'; showStatus( top.opener.parent.top_frame, \'User+Has+Been+Updated\' ); close();";
            //msg = "User Has Been Updated";
        } catch (SQLException e) {
            msg = "<p>There was an Error Updating the User.  Contact the DTS developer.</p>";  
            msg += "<p>"+e.getMessage()+"</p>";
        }
        %>    
    </c:when>
    <c:otherwise>
        <%
        onLoad = "showStatus( top.opener.parent.top_frame, \'Edit+Canceled\' ); close();";
        //msg = "Edit Canceled";
        %>
    </c:otherwise>
</c:choose>


<%-- The action was attempted, so return to the editor to provide the user with the necessary information. --%>
<jsp:forward page="/edit/edit_user.jsp">
    <jsp:param name="onLoad" value="<%= onLoad %>" />
    <jsp:param name="error" value="<%= msg %>" />
    <jsp:param name="mode" value="${param.mode}" />
</jsp:forward>
