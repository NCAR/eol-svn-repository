<%---------------------------------------------------------------------
* The proc_note.jsp page is the utility page for updating the database
* with the values from the note editor form.
---------------------------------------------------------------------%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ page errorPage="/dln/error.jsp" %>
<%@ page import="dln.display.*,dln.util.*,java.sql.*" %>

<%-- Define the beans used in this page. --%>
<jsp:useBean id="email" scope="page" class="dln.util.SMTPBean"/>
<jsp:useBean id="note" scope="request" class="dln.beans.NoteBean"/>
<jsp:setProperty name="note" property="*"/>
<jsp:useBean id="onLoad" scope="page" class="java.lang.String" />
<jsp:useBean id="msg" scope="page" class="java.lang.String" />

<%-- Determine which action needs to be done for the action from the editor. --%>
<c:choose>
    <c:when test="${param.action == 'Add Note'}">
        <% 
            if (note.insert(request.getParameter("datasetId"))) {
                onLoad = "top.opener.parent.left.location.reload(true); top.opener.parent.main.location.reload(true); showStatus( top.opener.parent.top_frame, \'Note+Has+Been+Added\' ); close();";
                //msg = "Note Has Been Added";
             try {
                if (request.getParameter("send_mail").equals("y")) {
                	if (!email.sendNewNote(request.getParameter("datasetId"), note)) {
                	    onLoad = "alert('There are no contacts to send the email to.');" + onLoad;
                	}
                }
            } catch (SMTPException e) {
                onLoad = "top.opener.parent.left.location.reload(true); top.opener.parent.main.location.reload(true); showStatus( top.opener.parent.top_frame, \'Note+Has+Been+Added\' );";
                msg += "<br>"+e.getMessage();
            }               
            } else {
                onLoad = "";
                msg = "There was an Error Adding the Note or the Note was empty.  Contact the DTS developer.";    
            }
        %>
    </c:when>
    <c:when test="${param.action == 'Update Note'}">
        <%
            if (note.update(request.getParameter("datasetId"))) {
                onLoad = "top.opener.parent.left.location.reload(true); top.opener.parent.main.location.reload(true); showStatus( top.opener.parent.top_frame, \'Note+Has+Been+Updated\' ); close();";
                //msg = "Note Has Been Updated";
             try {
                if (request.getParameter("send_mail").equals("y")) {
                	if (!email.sendUpdateNote(request.getParameter("datasetId"), note)) {
                	    onLoad = "alert('There are no contacts to send the email to.');" + onLoad;
                	}
                }
            } catch (SMTPException e) {
                onLoad = "top.opener.parent.left.location.reload(true); top.opener.parent.main.location.reload(true); showStatus( top.opener.parent.top_frame, \'Note+Has+Been+Updated\' );";
                msg += "<br>"+e.getMessage();
            }                              
            } else {
                onLoad = "";
                msg = "There was an Error Updating the Note or the Note was empty.  Contact the DTS developer.";  
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

<%-- Pass the information along to the note editor form for final display. --%>
<jsp:forward page="/dln/edit/edit_note.jsp">
    <jsp:param name="onLoad" value="<%= onLoad %>" />
    <jsp:param name="error" value="<%= msg %>" />
    <jsp:param name="mode" value="${param.mode}" />
</jsp:forward>
