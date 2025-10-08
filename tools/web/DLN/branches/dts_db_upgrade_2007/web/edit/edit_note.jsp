<%---------------------------------------------------------------------------------
* The edit_note.jsp page is the editor form for adding or editing a single note
* for a data set.
---------------------------------------------------------------------------------%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">

<%@ page errorPage="/error.jsp" %>
<%@ page import="java.util.*,dln.beans.*" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<%-- Define the beans used in the page. --%>
<jsp:useBean id="constants" scope="page" class="dln.util.DLNConstants" />
<jsp:useBean id="note" scope="request" class="dln.beans.NoteBean" />
<jsp:setProperty name="note" property="*" />

<%
    Map<Integer, UserBean> users = (new UserBean()).getAllUsersMap();
    note.load(request.getParameter("datasetId"));
%>

<html>
<head>
    <title>${constants.shortTitle}: Edit Note</title>
    <script language=javascript src="${pageContext.request.contextPath}/dln.js"></script>
    <link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/dln.css">
    <link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/edit.css">
</head> 

<body onLoad="${empty param.onLoad ? 'showStatus(top.opener.parent.top_frame, \'Edit+Note\')' : param.onLoad}">
<form action="${pageContext.request.contextPath}/edit/util/proc_note.jsp" method="POST">

<%-- Define unmodifiable parameters needed for processing the form action correctly. --%>
<input type="hidden" name="mode" value="${param.mode == null ? 'add' : param.mode}">
<input type="hidden" name="datasetId" value="${param.datasetId}" />
<input type="hidden" name="noteId" value="${param.noteId}" />

<div class="headerbar">
    <span style="padding-left: 2px;">
        ${constants.title}: 
        <c:choose>
           <c:when test="${param.mode == null || param.mode == 'add'}">Add Note</c:when>
           <c:otherwise>Edit Note</c:otherwise>
        </c:choose>
        (${param.datasetId})
    </span>
</div>
<div class="spacer"></div>

<%-- Only display an error if there is one to display. --%>
<c:if test="${!empty param.error && param.error != ''}">
    <div class="error">${param.error}</div>
    <div class="spacer"></div>
</c:if>

<div class="form">
    <table class="noteForm">
        <tr>
            <th>Type(s):</th>
            <td>
                <table class="noteForm">
			        <%
			        List<NoteTypeBean> types = new ArrayList<NoteTypeBean>(NoteTypeBean.getNoteTypesMap().values());
			        Collections.sort(types);
			        
			        int rows = types.size() / 3 + 1;
			        
			        for (int i = 0; i < rows; i++) {
			            out.print("<tr>");
			            out.print("<td><input type=\"checkbox\" name=\"noteTypes\" value=\""+types.get(i).getTypeId()+"\"");
			            if (note.isNoteType(types.get(i))) { out.println(" checked"); }
			            out.print("></td>");
			            out.print("<td>"+types.get(i).getName()+"</td>");
			            
			            if (i + rows < types.size()) {
                            out.print("<td><input type=\"checkbox\" name=\"noteTypes\" value=\""+types.get(i+rows).getTypeId()+"\"");
                            if (note.isNoteType(types.get(i+rows))) { out.println(" checked"); }
                            out.print("></td>");
                            out.print("<td>"+types.get(i+rows).getName()+"</td>");
                        } else {
                            out.print("<td></td><td></td>");			            	
			            }
			            
			            if (i + rows * 2 < types.size()) {
                            out.print("<td><input type=\"checkbox\" name=\"noteTypes\" value=\""+types.get(i+rows*2).getTypeId()+"\"");
                            if (note.isNoteType(types.get(i+rows*2))) { out.println(" checked"); }
                            out.print("></td>");
                            out.print("<td>"+types.get(i+rows*2).getName()+"</td>");
                        } else {
                            out.print("<td></td><td></td>");                            
			            }
			            
			            out.println("</tr>");
			        }
			        %>
    	        </table>
    	    </td>
    	</tr>
    </table>
</div>
    
<div class="spacer"></div>

<div class="form">
    <table class="noteForm">
        <tr>
            <th>Author:</th>
            <td>
                <select name="author">
                    <option value="-1">---Select---</option>
                    <%
                       List<UserBean> sortedUsers = new ArrayList<UserBean>(users.values());
                       Collections.sort(sortedUsers, new dln.util.UserNameComparator(false));
                       for (UserBean user : sortedUsers) {
                            if (user.isActive()) {
                                out.print("<option value=" + user.getContactId() + (note.getAuthor() == user.getContactId() ? " selected" : "") + ">" + user.getPersonName() + "</option>");
                            }
                         }
                    %>
                </select>
            </td>
        </tr>
    </table>
</div>
    
<div class="spacer"></div>

<div class="form">
    <table class="noteForm">
        <tr>
            <th>Note Text:</th>
            <td><textarea cols="75" rows="15" name="noteText">${note.noteText}</textarea></td>
        </tr>
    </table>
</div>

<div class="spacer"></div>
<div class="headerbar">
    <div class="buttons">
        <c:choose>
            <c:when test="${param.mode == null || param.mode == 'add'}">
                <input type=submit name=action value="Add Note" onClick="return validateNoteForm(this.form);">
                <input type=submit name=action value="Cancel">
            </c:when>
            <c:otherwise>
                <input type=submit name=action value="Update Note" onClick="if( validateForm(this.form) ) { return checkSendMail( this.form ); } else { return false; }">
                <input type=submit name=action value="Cancel">
            </c:otherwise>
        </c:choose>
    </div>
</div>
</form>

</body>
</html> 
