<%--------------------------------------------------------------------------
* The edit_user.jsp page is the editor form for adding or changing a
* DTS user/contact.
--------------------------------------------------------------------------%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">

<%@ page errorPage="/dln/error.jsp" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<%-- Define the beans used by this page. --%>
<jsp:useBean id="constants" scope="page" class="dln.util.DLNConstants" />
<jsp:useBean id="user" scope="request" class="dln.beans.UserBean" />

<%-- Load the user from the database if it is in edit mode. --%>
<c:if test="${param.mode == 'edit'}">
    <% user.loadUser(Integer.parseInt(request.getParameter("contactId"))); %>
</c:if>

<html>
<head>
    <title>${constants.shortTitle}: Edit Project</title>
    <script language=javascript src="${pageContext.request.contextPath}/dln/dln.js"></script>
    <link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/dts.css">
    <link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/edit.css">
</head> 

<body onLoad="${param.onLoad == null ? 'showStatus( top.opener.parent.top_frame, \'Edit+User\' )' : param.onLoad}">
<form action="${pageContext.request.contextPath}/dln/edit/util/proc_user.jsp" method="POST">

<%-- Need to store the mode so the correct buttons are displayed on a reload. --%>
<input type="hidden" name="mode" value="${param.mode}">
<%-- Need to store the contact id so it gets passed with the rest of the parameters. --%>
<input type="hidden" name="contactId" value="${user.contactId}">

<div class="headerbar">
    <span style="padding-left: 2px;">
        ${constants.title}: 
        <c:choose>
           <c:when test="${param.mode == null || param.mode == 'add'}">Add User</c:when>
           <c:otherwise>Edit User</c:otherwise>
        </c:choose>
    </span>
</div>
<div class="spacer"></div>

<%-- Display the error message if there was one passed back to the form. --%>
<c:if test="${!empty param.error && param.error != ''}">
    <div class="error">${param.error}</div>
    <div class="spacer"></div>
</c:if>

<div class="form">
    <table class="userForm">
        <tr>
            <th class="required">Short Name:</th>
            <td><input type="text" name="shortName" size=16 maxlength=15 value="${user.shortName}" ></td>
        </tr>
        <tr>
           <th class="required">Person Name:</th>
           <td><input type="text" name="personName" size=50 maxlength=255 value="${user.personName}"></td>
        </tr>
        <tr>
            <th>Email:</th>
            <td><input type="text" name="email" size=50 maxlength=255 value="${user.email}"></td>
        </tr>
        <tr>
            <th>Active:</th>
            <td>
                <input type="checkbox" name="active" <c:if test="${user.active}">checked</c:if>>
            </td>
        </tr>
    </table>
</div>

<c:if test="${pageContext.request.remoteUser == user.shortName}">
<div class="spacer"></div>
<div class="form">
    <table class="userForm">
		<tr>
			<th>New Password:</th>
			<td><input type="password" name="password" size="50" maxlength="255" value="" /></td>
		</tr>
		<tr>
			<th>Re-enter Password:</th>
			<td><input type="password" name="repeatPassword" size="50" maxlength="255" value="" /></td>
		</tr>
    </table>
</div>
</c:if>


<c:if test="<%= request.isUserInRole(\"super\") %>">
<div class="spacer"></div>
<div class="form">
    <table class="userForm">
		<%
			for (String role: user.getAllRoles()) {
				out.println("<tr>");
				out.print  ("    <th>");
				out.print  (role);
				out.println("</th>");
				out.print  ("    <td>");
				out.print  ("<input type=\"checkbox\" name=\"roles\" value=\"");
				out.print  (role);
				out.print  ("\"");
				out.print  (user.isInRole(role) ? " checked>" : ">");
				out.println("</td>");
				out.println("</tr>");
			}
		%>
		<tr>
			<th>Overwrite Password:</th>
			<td><input type="checkbox" name="overwritePassword" /></td>
		</tr>
		<tr>
			<th>Default Password:</th>
			<td><input type="text" name="defaultPassword" size="50" maxlength="255" value="keepyouout" /></td>
		</tr>
    </table>
</div>
</c:if>


<div class="spacer"></div>
<div class="headerbar">
    <div class="buttons">
        <c:choose>
            <c:when test="${param.mode == 'add'}">
                <input type=submit onClick="javascript: return validateUser(this.form);" name=action value="Add User">
                <input type=submit name=action value="Cancel">
            </c:when>
            <c:otherwise>
                <input type=submit onClick="javascript: return validateUser(this.form);" name=action value="Update User">
                <input type=submit name=action value="Cancel">
            </c:otherwise>
        </c:choose>
    </div>
</div>
</form>

</body>
</html>
