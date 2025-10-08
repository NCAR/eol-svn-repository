<%---------------------------------------------------------------------------------
* The edit_dataset.jsp page is the editor form for manipulating data sets in the
* database.  It allows for the adding of new data sets, updating existing data sets,
* adding new versions to existing data sets, and deleting data sets from the
* database.
---------------------------------------------------------------------------------%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">

<%@ page errorPage="/dln/error.jsp" %>
<%@ page import="java.util.*,dln.display.*,dln.beans.*,dln.util.*" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<%-- Define the beans used in the page. --%>
<jsp:useBean id="constants" scope="page" class="dln.util.DLNConstants" />
<jsp:useBean id="dataset" scope="request" class="dln.beans.DatasetBean" />
<jsp:useBean id="note" scope="request" class="dln.beans.NoteBean" />
<jsp:useBean id="display" scope="session" class="dln.display.DatasetDisplayBean"/>

<html>
<head>
	<title>${constants.shortTitle}: Edit Window</title>
	<script language="javascript" src="${pageContext.request.contextPath}/dts.js"></script>
	<script language=javascript src="${pageContext.request.contextPath}/dln/dln.js"></script>
	<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/dts.css" />
	<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/edit.css" />
</head>

<body class="dataset" onLoad="${empty param.onLoad ? 'showStatus(top.opener.parent.top_frame, \'Edit+Existing+Dataset\')' : param.onLoad}">

<%
    // Initialize the variables used in the page.
	List<ProjectBean> projects = (new ProjectBean()).getAllProjects();
	List<ProjectBean> sortedProjects = new ArrayList<ProjectBean>(projects);
	Collections.sort(sortedProjects, new ProjectIdComparator(false));
	
	Map<Integer, UserBean> users = (new UserBean()).getAllUsersMap();
	List<UserBean> sortedUsers = new ArrayList<UserBean>(users.values());
	Collections.sort(sortedUsers, new UserNameComparator(false));

	Map<Integer, StatusBean> statuses = StatusBean.getAllStatusesMap();
	List<StatusBean> sortedStatuses = new ArrayList<StatusBean>(statuses.values());
	Collections.sort(sortedStatuses);

	Map<Integer, IngestTypeBean> ingestTypes = IngestTypeBean.getAllIngestTypesMap();
	List<IngestTypeBean> sortedIngestTypes = new ArrayList<IngestTypeBean>(ingestTypes.values());
	Collections.sort(sortedIngestTypes);


	if (request.getParameter("error") == null) {
		
		// Load the data set from the database.
		if (request.getParameter("mode").equals("update") || request.getParameter("mode").equals("load")) {
			dataset.loadDataset(request.getParameter("datasetId"));
		}
		// Initialize the data set based on the current view.
		else {
			if (display.isDisplayProjectList())
				dataset.addProject(display.getDisplayId());
			else if (display.isDisplayIngesterList())
				dataset.setIngester(Integer.parseInt(display.getDisplayId()));
			else if (display.isDisplayLoaderList())
				dataset.setLoader(Integer.parseInt(display.getDisplayId()));
			else if (display.isDisplayCheckerList())
				dataset.setChecker(Integer.parseInt(display.getDisplayId()));
		}
	}
%>

<%-- This must be done here to set the parameters coming in from the request after initialization. --%>
<c:if test="${param.mode != 'load'}">
	<jsp:setProperty name="dataset" property="*"/>
	<jsp:setProperty name="note" property="*" />
</c:if>

<form action="${pageContext.request.contextPath}/dln/edit/util/proc_dataset.jsp" method="POST">

<%-- Define non-editable fields needed for correctly handling the action. --%>
<input type="hidden" name="mode" value="${param.mode}">
<input type="hidden" name="originalDatasetId" value="${dataset.originalDatasetId}">
<input type="hidden" name="send_mail" value="n">
<input type="hidden" name="datasetEntryDate" value="${dataset.entryDate}" />
<input type="hidden" name="type" value="dataset" />

<div class="headerbar">
    <div style="float: right; padding: 2px 10px 0 0;">
        <%= dataset.getEntryDate() == null ? "Today" : dataset.getEntryDate().toString().substring(0, 19) %>
    </div>
    <span style="padding-left: 2px;">
        ${constants.title}: 
        <c:choose>
           <c:when test="${param.mode == null || param.mode == 'add'}">Add Dataset</c:when>
           <c:otherwise>Edit Dataset</c:otherwise>
        </c:choose>
    </span>
</div>
<div class="spacer"></div>

<%-- Only display the error section if there is an error to display. --%>
<c:if test="${!empty param.error && param.error != ''}">
    <div class="error">${param.error}</div>
    <div class="spacer"></div>
</c:if>


<div class="datasetForm">
    <div class="sides">
        <div class="formSide">
		    <table class="datasetForm">
		        <tr>
		            <th>Dataset ID:</th>
		            <td>
		            	<input type="text" name="datasetId" size=16 maxlength=15 value="${dataset.datasetId}" >
		            	<c:if test="${empty dataset.originalDatasetId}">
		   		            <input type="submit" name="action" value="Load"/>
		   		        </c:if>
		            </td>
		        </tr>
		        <tr>
		            <th class="required">Name:</th>
		            <td><input type="text" name="name" size=75 maxlength=255 value="${dataset.name}"></td>
		        </tr>
		    </table>
            <div class="shortSpacer"></div>
		    <table class="datasetForm">
		        <tr>
		            <th class="required">Internal Contact:</th>
		            <td>
		                <select name="internalContact">
		                    <option value="-1">---Select---</option>
		                    <%
		                    UserBean intContact = dataset.getInternalContact(users);
		                    for (UserBean user : sortedUsers) {
		                        if (intContact != null && intContact.getContactId().equals(user.getContactId())) {
		                            out.print("<option value=" + user.getContactId() + " selected>" + user.getPersonName() + "</option>");
		                        } else if (user.isActive()) {
		                            out.print("<option value=" + user.getContactId() + ">" + user.getPersonName() + "</option>");
		                        }
		                    }
		                    %>
		                </select>
		            </td>
		            <th>Questions:</th>
		            <td><input name="questions" type="checkbox"<%= dataset.hasQuestions() ? " checked" : "" %> /></td>
		        </tr>
		        <tr>
		            <th>Source Contact:</th>
		            <td colspan="3">
		                <select name="externalContact">
		                    <option value="-1">Unassigned</option>
		                    <%
		                    for (UserBean user : sortedUsers) {
		                        if (dataset.getExternalContact(users) != null && dataset.getExternalContact(users).getContactId().equals(user.getContactId())) {
		                            out.print("<option value=" + user.getContactId() + " selected>" + user.getPersonName() + " &lt;" + user.getEmail() + "&gt;</option>");
		                        } else {
		                            out.print("<option value=" + user.getContactId() + ">" + user.getPersonName() + " &lt;" + user.getEmail() + "&gt;</option>");
		                        }
		                    }
		                    %>
		               </select>
		            </td>
		        </tr>    
			<tr>
			    <th>Readme URL:</th>
			    <td colspan="3"><input type="text" size="70" maxlength="255" name="readmeURL" value="${dataset.readmeURL}" /></td>
			</tr>
		        <tr>
		            <th>Remote URL:</th>
		            <td colspan="3"><input type=text size=70 maxlength=255 name="remoteURL" value="${dataset.remoteURL}"></td>
		        </tr>
		    </table>
		    <div class="shortSpacer"></div>
		    <table class="datasetForm">
		        <tr>
		            <th>Ingest Location:</th>
		            <td colspan="3"><input type=text size=60 maxlength=255 name="ingestLocation" value="${dataset.ingestLocation}"></td>
		        </tr>
		        <tr>
		            <th>Ingest Type:</th>
		            <td colspan="3">
		                <select name="ingestType">
		                    <option value="-1">Unknown</option>
		                    <%                                                                     
		                    for (IngestTypeBean type : sortedIngestTypes) {
		                        out.print("<option value=" + type.getTypeId() + 
		                                (dataset.getIngestType(ingestTypes) != null && dataset.getIngestType(ingestTypes).getTypeId() == (type.getTypeId()) ? " selected" : "")
		                                + ">" + type.getName() + "</option>");
		                    }
		                    %>                                                                       
		                </select>
		            </td>
		        </tr>
		        <tr>
		            <th>Ingest Contact:</th>
		            <td>
		                <select name="ingester">
		                    <option value="-1">Unassigned</option>
		                    <%
		                    for (UserBean user : sortedUsers) {
		                        if (dataset.getIngester(users) != null && dataset.getIngester(users).getContactId().equals(user.getContactId())) {
		                            out.print("<option value=" + user.getContactId() + " selected>" + user.getPersonName() + "</option>");
		                        } else if (user.isActive()) {
		                            out.print("<option value=" + user.getContactId() + ">" + user.getPersonName() + "</option>");
		                        }
		                    }
		                    %>
		                </select>
		            </td>
		            <th>Ingest Status:</th>
		            <td>
		                <select name="ingestStatus">
		                    <option value="-1">No Status</option>
		                    <%
		                    for (StatusBean status : sortedStatuses) {
		                        out.print("<option value=" + status.getStatusId() + 
		                                (dataset.getIngestStatus(statuses) != null && dataset.getIngestStatus(statuses).getStatusId() == status.getStatusId() ? " selected" : "")
		                                + ">" + status.getName() + "</option>");
		                    }
		                    %>
		                </select>
		            </td>
		        </tr>
		    </table>
		    <div class="shortSpacer"></div>
		    <table class="datasetForm">
		        <tr>
		            <th>Data to be Archived:</th>
		            <td colspan="3"><input type=text size=60 maxlength=255 name="loadDataLocation" value="${dataset.loadDataLocation}" /></td>
		        </tr>
		        <tr>
		            <th>Archive Location:</th>
		            <td colspan="3"><input type=text size=60 maxlength=255 name="archiveLocation" value="${dataset.archiveLocation}"></td>
		        </tr>
		        <tr>
		            <th>Has Documentation:</th>
		            <td colspan="3">
		                <c:choose>
		                    <c:when test="${dataset.documented}">
		                        <input name="documented" type="checkbox" checked />
		                    </c:when>
		                    <c:otherwise>
		                        <input name="documented" type="checkbox">
		                    </c:otherwise>
		                </c:choose>
		            </td>
		        </tr>
		        <tr>
		            <th>Load Contact:</th>
		            <td>
		                <select name="loader">
		                    <option value="-1">Unassigned</option>
		                    <%
		                    for (UserBean user : sortedUsers) {
		                        if (dataset.getLoader(users) != null && dataset.getLoader(users).getContactId().equals(user.getContactId())) {
		                            out.print("<option value=" + user.getContactId() + " selected>" + user.getPersonName() + "</option>");
		                        } else if (user.isActive()) {
		                            out.print("<option value=" + user.getContactId() + ">" + user.getPersonName() + "</option>");
		                        }
		                    }
		                    %>
		                </select>
		            </td>
		            <th>Load Status:</th>
		            <td>
		                <select name="loadStatus">
		                    <option value="-1">No Status</option>
		                    <%
		                    for (StatusBean status : sortedStatuses) {
		                        out.print("<option value=" + status.getStatusId() +
		                                (dataset.getLoadStatus(statuses) != null && dataset.getLoadStatus(statuses).getStatusId() == status.getStatusId() ? " selected" : "") 
		                                + ">" + status.getName() + "</option>");
		                    }
		                    %>
		                </select>
		            </td>
		        </tr>    
		    </table>
		    <div class="shortSpacer"></div>
		    <table class="datasetForm">
		        <tr>
		            <th>Check Contact:</th>
		            <td>
		                <select name="checker">
		                    <option value="-1">Unassigned</option>
		                    <%
		                    for (UserBean user : sortedUsers) {
		                        if (dataset.getChecker(users) != null && dataset.getChecker(users).getContactId().equals(user.getContactId())) {
		                            out.print("<option value=" + user.getContactId() + " selected>" + user.getPersonName() + "</option>");
		                        } else if (user.isActive()) {
		                            out.print("<option value=" + user.getContactId() + ">" + user.getPersonName() + "</option>");
		                        }
		                    }
		                    %>
		                </select>
		            </td>
		            <th>Check Status:</th>
		            <td>
		                <select name="checkStatus">
		                    <option value="-1">No Status</option>
		                    <%
		                    for (StatusBean status : sortedStatuses) {
		                        out.print("<option value=" + status.getStatusId() +
		                                (dataset.getCheckStatus(statuses) != null && dataset.getCheckStatus(statuses).getStatusId() == status.getStatusId() ? " selected" : "") 
		                                + ">" + status.getName() + "</option>");
		                    }
		                    %>
		                </select>
		            </td>
		        </tr>
		    </table>
        </div>




        <div class="projectListSide">
		    <table class="datasetProjectTable">
		        <tr>
		            <th>Associated Projects</th>
		            <th>In Project's ML</th>
		            <th>Delete Association</th>
		        </tr>
		        <%
 	            for (ProjectBean project : sortedProjects) {
	        	    if (dataset.getProjectsAsList().contains(project.getProjectId())) {	        	   
			            out.print("<tr><td align=center valign=middle>");
			            out.print("<input name=\"project\" type=\"hidden\" value=\"" + project.getProjectId() + "\">");
			            out.print(project.getProjectId());
			            out.print("</td><td align=center valign=middle><input name=\"masterListFlags\" type=\"checkbox\" value=" + project.getProjectId());
			            if (dataset.isInMasterList(project.getProjectId())) { out.print(" checked"); }
			            out.print("><td align=center valign=middle>");
			            out.print("<input name=\"deletedProjects\" type=\"checkbox\" value=\"" + project.getProjectId() + "\"");
			            if (dataset.isDeletedProject(project.getProjectId())) { out.print(" checked"); }
			            out.println("></td></tr>");
	        	    }
		        }
		        %>
		        <tr>
		            <td><input type=submit name=action value="Add Project"/></td>
		            <td colspan="2">
		                <select name="project">
		                    <option value="null">Select Project</option>
		                    <%
		                    for (ProjectBean project : sortedProjects) {
		                        if (!dataset.getProjectsAsList().contains(project.getProjectId()) && project.isActive()) {
		                            out.print("<option value=" + project.getProjectId() + ">" + project.getProjectId() + "</option>");
		                        }
		                    }
		                    %>
		                </select>
		            </td>
		        </tr>
		    </table>
        </div>
    </div>
</div>

<div class="spacer"></div>
<div class="noteBlock">
    <table class="datasetForm">
        <tr>
            <td rowspan="2">
                <table>
                    <th colspan="2">Note Type(s):</th>
                    <%
                    List<NoteTypeBean> sortedNoteTypes = new ArrayList<NoteTypeBean>(NoteTypeBean.getNoteTypesMap().values());
                    Collections.sort(sortedNoteTypes);
                    for (NoteTypeBean type: sortedNoteTypes) {
                    	out.print("<tr>");
                    	out.print("<td><input type=\"checkbox\" name=\"noteTypes\" value=\""+type.getTypeId()+"\"");
                    	if (note.isNoteType(type)) { out.println(" checked"); }
                    	out.print("></td>");
                    	out.print("<td>"+type.getName()+"</td>");
                    	out.println("</tr>");
                    }
                    %>
                </table>
            </td>
            <th>Author:</th>
            <td>
                <select name="author">
                    <%
                       boolean selected = false;
                       for (UserBean user : sortedUsers) {
                            if (user.isActive() || user.getContactId() == note.getAuthor()) {
                            	if (user.getContactId() == note.getAuthor()) { 
	                                out.print("<option value=" + user.getContactId() + " selected>" + user.getPersonName() + "</option>");
	                                selected = true;
                            	} else if (!selected && user.getShortName().equals(request.getRemoteUser())) {
	                                out.print("<option value=" + user.getContactId() + " selected>" + user.getPersonName() + "</option>");
	                                selected = true;
                            	} else {
	                                out.print("<option value=" + user.getContactId()  + ">" + user.getPersonName() + "</option>");
                            	}
                            }
                         }
                    %>
                </select>
            </td>
        </tr>
        <tr>
            <th>Text:</th>
            <td><textarea cols="80" rows="8" name="noteText">${note.noteText}</textarea></td>
        </tr>
    </table>
</div>


<div class="spacer"></div>
<div class="headerbar">
    <div class="buttons">
        <c:choose>
            <c:when test="${param.mode == 'add'}">
                <input type="submit" name="action" value="Add Dataset" onClick="if( validateForm(this.form) ) { return checkSendMail( this.form ); } else { return false; }"/>
                <input type="submit" name="action" value="Cancel"/>
            </c:when>
            <c:otherwise>
            	<c:if test="<%= request.isUserInRole(\"super\") %>">
	                <input style="background-color: red; color: white; float:left;" type=submit name=action value="Delete" onClick="return verify_delete()"/>
	            </c:if>
                <input type=submit name=action value="Update" onClick="if( validateForm(this.form) ) { return checkSendMail( this.form ); } else { return false; }" />
                <input type=submit name=action value="Add New Version" onClick="if (verify_add_version()) { if( validateForm(this.form) ) { return checkSendMail( this.form ); } else { return false; } } else { return false; }" />
                <input type=submit name=action value="Add Dataset" onClick="if( verify_add()) { if( validateForm(this.form) ) { return checkSendMail( this.form ); } else { return false; } } else { return false; }"/>
                <input type=submit name=action value="Cancel"/>
            </c:otherwise>
        </c:choose>
    </div>
</div>
</form>

</body>
</html>
