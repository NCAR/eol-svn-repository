<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<jsp:useBean id="dataset" scope="request" class="dln.beans.DatasetBean" />
<%@ page import="java.util.*,dln.beans.*, dln.util.*" %>

<% 
	dataset.loadSourceDataset(request.getParameter("datasetId"), request.getParameter("selectedProduct"));
	Map<Integer, UserBean> users = (new UserBean()).getAllUsersMap();
	List<UserBean> sortedUsers = new ArrayList<UserBean>(users.values());
	Collections.sort(sortedUsers, new UserNameComparator(false));

	Map<Integer, StatusBean> statuses = StatusBean.getAllStatusesMap();
	List<StatusBean> sortedStatuses = new ArrayList<StatusBean>(statuses.values());
	Collections.sort(sortedStatuses);
%>

<html>
<head>
	<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
	<title>${dataset.datasetId} Source Information Editor</title>
	<script language="javascript" src="${pageContext.request.contextPath}/dts.js"></script>
	<script language="javascript" src="${pageContext.request.contextPath}/iven/iven.js"></script>
	<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/dts.css" />
	<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/edit.css" />
</head>
<body onLoad="${empty param.onLoad ? '' : param.onLoad}">

<%-- This must be done here to set the parameters coming in from the request after initialization. --%>
<jsp:setProperty name="dataset" property="*"/>
<%-- Need to do this so whatever platforms that are currently assigned in the database don't appear here if they have been deleted. --%>
<c:if test="${!empty param.error && empty param.platformString}">
	<jsp:setProperty name="dataset" property="platformString" value="" />
</c:if>

<form action="${pageContext.request.contextPath}/iven/edit/util/proc_source_info.jsp" method="POST">

<%-- Define non-editable fields needed for correctly handling the action. --%>
<input type="hidden" name="mode" value="${param.mode}" />
<input type="hidden" name="datasetId" value="${dataset.datasetId}" />
<input type="hidden" name="selectedProduct" value="${dataset.selectedProduct}" />
<input type="hidden" name="name" value="${dataset.name}" />

<div class="headerbar">
    <span style="padding-left: 2px;">
        DTS: ${dataset.datasetId} Source Information
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
		<tr><td class="uneditableName">${dataset.datasetId}: ${dataset.name}</td></tr>
	</table>
	<div class="shortSpacer"></div>
	<table class="datasetForm">
		<tr>
			<th>Data Directory:</th>
			<td colspan="5"><input size="60" maxlength="255" name="dataLocation" type="text" value="${dataset.dataLocation}" /></td>
		</tr>
		<tr>
			<th>UTC Offset:</th>
			<td><input size="20" maxlength="16" name="utcOffset" type="text" value="${dataset.utcOffset}" /></td>
			<th>Uses DST:</th>
		    <td>
				<select name="dstFlagString">
					<option value="null" <%= dataset.isDSTFlag() == null ? "selected" : "" %>>Unknown</option>
					<option value="true" <%= dataset.isDSTFlag() != null && dataset.isDSTFlag() ? "selected" : "" %>>Yes</option>
					<option value="false" <%= dataset.isDSTFlag() != null && !dataset.isDSTFlag() ? "selected" : "" %>>No</option>
				</select>
			</td>
			<th>Excluded:</th>
			<td><input type="checkbox" name="excluded" <%= dataset.isExcluded() ? "checked" : "" %> /></td>
		</tr>
		<tr>
			<th>EMDAC Platforms:<br /><span class="small">(Separate w/Semicolons)</span></th>
			<td colspan="5"><input size="60" maxlength="255" name="platformString" type="text" value="${dataset.platformString}" /></td>
		</tr>
	</table>
	<div class="shortSpacer"></div>
	<table class="datasetForm">
		<tr>
            <th>"Raw Data Frequency/Time Note" Author:</th>
            <td width="100%">
                <select name="collectionTimeNoteAuthor">
                    <%
                       boolean selected = false;
                       for (UserBean user : sortedUsers) {
                            if (user.isActive() || user.getContactId() == dataset.getCollectionTimeNote().getAuthor()) {
                            	if (user.getContactId() == dataset.getCollectionTimeNote().getAuthor()) { 
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
   </table>
   <table class="datasetForm">
		<tr>
			<th>Raw Data Frequency/<br />Time Info:</th>
			<td><textarea cols="55" rows="8" name="collectionTimeNoteText">${dataset.collectionTimeNote.noteText}</textarea></td>
		</tr>
	</table>
	<div class="shortSpacer"></div>
	<table class="datasetForm">
		<tr>
            <th>"Source of Info Note" Author:</th>
            <td width="100%">
                <select name="sourceInfoNoteAuthor">
                    <%
                    selected = false;
                    for (UserBean user : sortedUsers) {
                         if (user.isActive() || user.getContactId() == dataset.getSourceInfoNote().getAuthor()) {
                         	if (user.getContactId() == dataset.getSourceInfoNote().getAuthor()) { 
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
                    %>
                </select>
            </td>
        </tr>
    </table>
    <table class="datasetForm">
		<tr>
			<th>Source of Information:</th>
			<td><textarea cols="60" rows="8" name="sourceInfoNoteText">${dataset.sourceInfoNote.noteText}</textarea></td>
		</tr>
	</table>
</div>

<div class="spacer"></div>
<div class="headerbar">
    <div class="buttons">
    	<input type=submit name=action value="Update" />
    	<input type="submit" name="action" value="Delete" onClick="javascript: return confirmSourceDelete();" />
        <input type=submit name=action value="Cancel"/>
    </div>
</div>
</form>

	

</body>
</html>