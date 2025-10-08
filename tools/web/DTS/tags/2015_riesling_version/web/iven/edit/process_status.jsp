<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<jsp:useBean id="dataset" scope="request" class="dln.beans.DatasetBean" />
<jsp:useBean id="note" scope="request" class="dln.beans.NoteBean" />
<%@ page import="java.util.*,dln.beans.*, dln.util.*" %>

<% 
//	project.loadProject(request.getParameter("projectId"));
	dataset.loadProcessDataset(request.getParameter("datasetId"));
	Map<Integer, UserBean> users = (new UserBean()).getAllUsersMap();
	List<UserBean> sortedUsers = new ArrayList<UserBean>(users.values());
	Collections.sort(sortedUsers, new UserNameComparator(false));

	Map<Integer, StatusBean> statuses = StatusBean.getAllStatusesMap();
	List<StatusBean> sortedStatuses = new ArrayList<StatusBean>(statuses.values());
	Collections.sort(sortedStatuses);
	
	Map<Integer, NoteTypeBean> noteTypes = NoteTypeBean.getNoteTypesMap();
%>

<html>
<head>
	<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
	<title>${dataset.datasetId} Process Status Editor</title>
	<script language="javascript" src="${pageContext.request.contextPath}/dts.js"></script>
	<script language="javascript" src="${pageContext.request.contextPath}/iven/iven.js"></script>
	<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/dts.css" />
	<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/edit.css" />
</head>
<body onLoad="${empty param.onLoad ? '' : param.onLoad}">

<%-- This must be done here to set the parameters coming in from the request after initialization. --%>
<jsp:setProperty name="dataset" property="*"/>
<jsp:setProperty name="note" property="*" />

<form action="${pageContext.request.contextPath}/iven/edit/util/proc_process_status.jsp" method="POST">

<%-- Define non-editable fields needed for correctly handling the action. --%>
<input type="hidden" name="mode" value="${param.mode}">
<input type="hidden" name="datasetId" value="${dataset.datasetId}">
<input type="hidden" name="originalDatasetId" value="${dataset.originalDatasetId}">
<input type="hidden" name="send_mail" value="n">
<input type="hidden" name="datasetEntryDate" value="${dataset.entryDate}" />
<input type="hidden" name="internalContact" value="${dataset.internalContact}" />
<input type="hidden" name="projectId" value="${param.projectId}" />
<input type="hidden" name="type" value="dataset" />

<div class="headerbar">
    <div style="float: right; padding: 2px 10px 0 0;">
        <%= dataset.getEntryDate() == null ? "Today" : dataset.getEntryDate().toString().substring(0, 19) %>
    </div>
    <span style="padding-left: 2px;">
        DTS: ${dataset.datasetId} Processing Status
    </span>
</div>
<div class="spacer"></div>

<%-- Only display the error section if there is an error to display. --%>
<c:if test="${!empty param.error && param.error != ''}">
    <div class="error">${param.error}</div>
    <div class="spacer"></div>
</c:if>

<div class="form">
	<table>
		<tr><td>${dataset.datasetId}: ${dataset.name}</td></tr>
	</table>
	<div class="shortSpacer"></div>
	<table class="datasetForm">
		<tr>
			<th>Process Contact:</th>
			<td>
                <select name="processer">
                    <option value="-1">Unassigned</option>
                    <%
                    UserBean contact = dataset.getProcesser(users);
                    for (UserBean user : sortedUsers) {
                        if (contact != null && contact.getContactId().equals(user.getContactId())) {
                            out.print("<option value=" + user.getContactId() + " selected>" + user.getPersonName() + "</option>");
	                    } else if (user.isActive()) {
	                        out.print("<option value=" + user.getContactId() + ">" + user.getPersonName() + "</option>");
	                    }
	                }
	                %>
	            </select>
			</td>
		</tr>
		<tr>
			<th>Process Status:</th>
		    <td>
		    	<select name="processStatus">
		        	<option value="-1">No Status</option>
		            <%
		            for (StatusBean status : sortedStatuses) {
		            	out.print("<option value=" + status.getStatusId() + 
		                          (dataset.getProcessStatus(statuses) != null && dataset.getProcessStatus(statuses).getStatusId() == status.getStatusId() ? " selected" : "")
		                          + ">" + status.getName() + "</option>");
		            }
		            %>
		        </select>
		    </td>
		</tr>
		<tr>
			<th>Questions:</th>
		    <td><input name="questions" type="checkbox"<%= dataset.hasQuestions() ? " checked" : "" %> /></td>
		</tr>
		<tr>
			<th>How To:</th>
			<td><input size="60" maxlength="255" name="howtoURL" type="text" value="${dataset.howtoURL}" /></td>
		</tr>
		<tr>
			<th>Work Location:</th>
			<td><input size=60 maxlength=255 name="workLocation" type="text" value="${dataset.workLocation}" /></td>
		</tr>
		<tr>
			<th>Final Data Location:</th>
			<td><input size="60" maxlength="255" name="finalProcessedDataLocation" type="text" value="${dataset.finalProcessedDataLocation}" /></td>
		</tr>
		<tr>
			<th>Station List File:</th>
			<td><input size="60" maxlength="255" name="stationListFile" type="text" value="${dataset.stationListFile}" /></td>
		</tr>
		<tr>
			<th>Plots:</th>
			<td><input size="60" maxlength="255" name="plotsURL" type="text" value="${dataset.plotsURL}" /></td>
		</tr>
	</table>
</div>

<div class="spacer"></div>
<div class="noteBlock">
    <table class="datasetForm">
        <tr>
            <td rowspan="2">
                <table>
                	<tr>
	                    <th colspan="2">Note Type(s):</th>
	                </tr>
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
            <td><textarea cols="60" rows="8" name="noteText">${note.noteText}</textarea></td>
        </tr>
    </table>
</div>


<div class="spacer"></div>
<div class="headerbar">
    <div class="buttons">
    	<input type=submit name=action value="Update" onClick="if( validateProcessStatusForm(this.form) ) { return checkSendMail( this.form ); } else { return false; }" />
        <input type=submit name=action value="Add New Version" onClick="if (verify_add_version()) { if( validateProcessStatusForm(this.form) ) { return checkSendMail( this.form ); } else { return false; } } else { return false; }" />
        <input type=submit name=action value="Delete" onClick="javascript: return verify_delete();" ${dataset.deletable ? "" : "disabled"} />
        <input type=submit name=action value="Cancel"/>
    </div>
</div>
</form>

	

</body>
</html>