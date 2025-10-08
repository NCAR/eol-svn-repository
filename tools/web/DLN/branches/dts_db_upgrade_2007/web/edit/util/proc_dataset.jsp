<%-------------------------------------------------------------------------
* The proc_dataset.jsp file is a utility to process the commands from the
* data set editor form.
-------------------------------------------------------------------------%>
<%@ page import="dln.display.*,dln.util.*,java.sql.*" %>

<%-- Define the beans used in this page. --%>
<jsp:useBean id="email" scope="page" class="dln.util.SMTPBean"/>
<jsp:useBean id="dataset" scope="request" class="dln.beans.DatasetBean" />
<jsp:useBean id="note" scope="request" class="dln.beans.NoteBean" />
<jsp:useBean id="display" scope="session" class="dln.display.DatasetDisplayBean" />
<jsp:setProperty name="dataset" property="*"/>
<jsp:setProperty name="note" property="*" />


<%
	if( display.getDisplayView() == -1 )
	{
		/*
		display.setDisplayView( DatasetDisplayBean.PROJECT );
		display.setDisplayId( dataset.getProjectsAsList().get(0));
		display.setCurrentView( DatasetDisplayBean.LISTING );
		*/
	}

	String action = request.getParameter( "action" );
	String onLoad = "";
	String msg = "";

	// Associate a new project to the data set.
	if (action.equals("Add Project")) {
		// Don't do anything.  This adding project is done in the bean.
	} 
	// Update the current data set in the database.
	else if (action.equals( "Update" )) {
		try {
			dataset.update(note);
            onLoad = "refresh( top.opener.parent.main, '" + dataset.getDatasetId() +"', '" + dataset.getEntryDate().toString() + "'); showStatus( top.opener.parent.top_frame, \'Dataset+Has+Been+Updated\' ); close();";
            //msg = "Dataset Updated";
            try {
                if (request.getParameter("send_mail").equals("y")) {
                	if (!email.sendUpdate(dataset, note)) {
                	    onLoad = "alert('There are no contacts to send the email to.');" + onLoad;
                	}
                }
            } catch (SMTPException e) {
                onLoad = "refresh( top.opener.parent.main, '" + dataset.getDatasetId() +"', '" + dataset.getEntryDate().toString() + "'); showStatus( top.opener.parent.top_frame, \'Dataset+Has+Been+Updated\' );";
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
            dataset.updateWithNewVersion(note);
            onLoad = "refresh( top.opener.parent.main, '" + dataset.getDatasetId() +"', '" + dataset.getEntryDate().toString() + "'); showStatus( top.opener.parent.top_frame, \'Dataset+Has+Been+Versioned\' ); close();";
            //msg = "Dataset Versioned";
            try {
                if (request.getParameter("send_mail").equals("y")) {
                	if (!email.sendNewVersion(dataset, note)) {
                        onLoad = "alert('There are no contacts to send the email to.');" + onLoad;
                	}
                }
            } catch (SMTPException e) {
                onLoad = "refresh( top.opener.parent.main, '" + dataset.getDatasetId() +"', '" + dataset.getEntryDate().toString() + "'); showStatus( top.opener.parent.top_frame, \'Dataset+Has+Been+Versioned\' );";
                msg += "<br>"+e.getMessage();
            }
        } catch (SQLException e) {
            onLoad = "";
            msg = "<p>There has been an error trying to version the dataset.  Please contact the DTS developer.<p>";
            msg += "<p>"+e.getMessage()+"</p>";
        }
	}
	// Add a brand new data set to the database
	else if (action.equals( "Add Dataset" )) {
		try {
			dataset.insert(note);
            onLoad = "refresh( top.opener.parent.main, '" + dataset.getDatasetId() +"', '" + dataset.getEntryDate().toString() + "'); showStatus( top.opener.parent.top_frame, \'Dataset+Has+Been+Added\' ); close();";
            //msg = "Dataset Added";
            try {
                if (request.getParameter("send_mail").equals("y")) { 
                	if (!email.sendNew(dataset, note)) {
                        onLoad = "alert('There are no contacts to send the email to.');" + onLoad;
                	}
                }
            } catch (SMTPException e) {
                onLoad = "refresh( top.opener.parent.main, '" + dataset.getDatasetId() +"', '" + dataset.getEntryDate().toString() + "'); showStatus( top.opener.parent.top_frame, \'Dataset+Has+Been+Added\' );";
                msg += "<br>"+e.getMessage();
            }
		} catch (SQLException e) {
            onLoad = "";
            msg = "<p>There has been an error trying to add the dataset.  Please contact the DTS developer.</p>";
            msg += "<p>"+e.getMessage()+"</p>";
		}
	} 
	// Remove the data set from the database.
	else if (action.equals( "Delete" )) {
		try {
			dataset.delete();
            onLoad = "refresh( top.opener.parent.main ); showStatus( top.opener.parent.top_frame, \'Dataset+Has+Been+Deleted\' ); close();";
            //msg = "Dataset Deleted";
            display.setListView();
		} catch (SQLException e) {
            onLoad = "";
            msg = "<p>There has been an error trying to delete the dataset.  Please contact the DTS developer.</p>";
            msg += "<p>"+e.getMessage()+"</p>";
		}
	}
	// Cancel any change done in the edit form.
	else {
		onLoad = "showStatus( top.opener.parent.top_frame, \'Edit+Cancelled\' ); close();";
		//msg = "Edit Cancelled";
	}
%>

<%-- Return to the data set edit form to show errors or close the window --%>
<jsp:forward page="/edit/edit_dataset.jsp">
    <jsp:param name="onLoad" value="<%= onLoad %>" />
    <jsp:param name="error" value="<%= msg %>" />
</jsp:forward>
