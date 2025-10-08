<%@ page import="dln.util.*,java.sql.*" %>

<%-- Define the beans used in this page. --%>
<jsp:useBean id="email" scope="page" class="dln.util.SMTPBean"/>
<jsp:useBean id="dataset" scope="request" class="dln.beans.DatasetBean" />
<jsp:useBean id="note" scope="request" class="dln.beans.NoteBean" />
<jsp:setProperty name="dataset" property="*"/>
<jsp:setProperty name="note" property="*" />


<%
	String action = request.getParameter( "action" );
	String onLoad = "";
	String msg = "";

	// Update the current data set in the database.
	if (action.equals( "Update" )) {
		try {
			dataset.updateProcess(note);
            onLoad = "top.opener.location.reload(true); close();";
            try {
                if (request.getParameter("send_mail").equals("y")) {
                	if (!email.sendProcessUpdate(dataset, note, request.getParameter("projectId"))) {
                	    onLoad = "alert('There are no contacts to send the email to.');" + onLoad;
                	}
                }
            } catch (SMTPException e) {
                onLoad = "top.opener.location.reload(true);";
                msg += "<br>"+e.getMessage();
            }
		} catch (SQLException e) {
            onLoad = "";
            msg = "<p>There has been an error trying to update the dataset.  Please contact the DTS developer.<p>";
            msg += "<p>"+e.getMessage()+"</p>";
		}
	}  
	// Add a new version to the current data set.
	else if (action.equals("Add New Version")) {
        try {
            dataset.updateProcessWithNewVersion(note);
            onLoad = "top.opener.location.reload(true); close();";
            try {
                if (request.getParameter("send_mail").equals("y")) {
                	if (!email.sendProcessNewVersion(dataset, note, request.getParameter("projectId"))) {
                        onLoad = "alert('There are no contacts to send the email to.');" + onLoad;
                	}
                }
            } catch (SMTPException e) {
                onLoad = "refresh( top.opener.parent.main, '" + dataset.getDatasetId() +"', '" + dataset.getEntryDate().toString() + "');";
                msg += "<br>"+e.getMessage();
            }
        } catch (SQLException e) {
            onLoad = "";
            msg = "<p>There has been an error trying to version the dataset.  Please contact the DTS developer.<p>";
            msg += "<p>"+e.getMessage()+"</p>";
        }
	}
	
	else if (action.equals("Delete")) {
		try {
			dataset.deleteProcessed();
			onLoad = "top.opener.location.href='"+request.getContextPath()+"/iven/dataset_list.jsp?projectId="+request.getParameter("projectId")+"'; close();";
		} catch (SQLException e) {
              onLoad = "";
              msg = "<p>There has been an error trying to delete the processed information for the  dataset.  Please contact the DTS developer.<p>";
              msg += "<p>"+e.getMessage()+"</p>";
		}
	}
	// Cancel any change done in the edit form.
	else {
		onLoad = "close();";
	}
%>

<%-- Return to the data set edit form to show errors or close the window --%>
<jsp:forward page="/iven/edit/process_status.jsp">
    <jsp:param name="onLoad" value="<%= onLoad %>" />
    <jsp:param name="error" value="<%= msg %>" />
</jsp:forward>
