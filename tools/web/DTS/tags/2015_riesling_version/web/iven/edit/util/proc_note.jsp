<%---------------------------------------------------------------------
* The proc_note.jsp page is the utility page for updating the database
* with the values from the note editor form.
---------------------------------------------------------------------%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ page import="java.sql.*" %>

<%-- Define the beans used in this page. --%>
<jsp:useBean id="note" scope="request" class="dln.beans.NoteBean"/>
<jsp:setProperty name="note" property="*"/>
<jsp:useBean id="onLoad" scope="page" class="java.lang.String" />
<jsp:useBean id="msg" scope="page" class="java.lang.String" />

<%-- Determine which action needs to be done for the action from the editor. --%>
<c:choose>
    <c:when test="${param.action == 'Add Note'}">
        <% 
        	if (request.getParameter("type").equals("project")) {
        		try {
	        		if (note.insertProject(request.getParameter("projectId"))) {
	        			onLoad = "top.opener.location.reload(true); close();";
	        		} else {
	        			msg = "<p>There was an error adding the note or the note was emtpy.  Contact the DTS developer.</p>";
	        		}
        		} catch (SQLException e) {
        			msg = "<p>There was an Error inserting the note or the note was empty.  Contact the DTS developer.</p>";
        			msg += "<p>"+e.getMessage()+"</p>";
        		}
        	} else {
	            if (note.insert(request.getParameter("datasetId"))) {
	                onLoad = "top.opener.location.reload(true); close();";
	            } else {
	                msg = "There was an Error Adding the Note or the Note was empty.  Contact the DTS developer.";    
	            }
        	}
        %>
    </c:when>
    <c:when test="${param.action == 'Update Note'}">
        <%
        	if (request.getParameter("type").equals("project")) {
        		try {
	        		if (note.updateProject(request.getParameter("projectId"))) {
	        			onLoad = "top.opener.location.reload(true); close();";
	        		} else {
	        			msg = "<p>There was an error updating the note or the note was emtpy.  Contact the DTS developer.</p>";
	        		}
        		} catch (SQLException e) {
        			msg = "<p>There was an error updating the note or the note was empty.  Contact the DTS developer.</p>";
        			msg += "<p>"+e.getMessage()+"</p>";
        		}
        	} else {
	            if (note.update(request.getParameter("datasetId"))) {
	                onLoad = "top.opener.location.reload(true); close();";
	            } else {
	                msg = "There was an Error Updating the Note or the Note was empty.  Contact the DTS developer.";  
	            }
        	}
        %>    
    </c:when>
    <c:otherwise>
        <% onLoad = "close();"; %>
    </c:otherwise>
</c:choose>

<%-- Pass the information along to the note editor form for final display. --%>
<jsp:forward page="/iven/edit/note.jsp">
    <jsp:param name="onLoad" value="<%= onLoad %>" />
    <jsp:param name="error" value="<%= msg %>" />
    <jsp:param name="type" value="${param.type}" />
</jsp:forward>
