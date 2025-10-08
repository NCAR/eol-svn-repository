<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<jsp:useBean id="dataset" scope="request" class="dln.beans.DatasetBean" />
<jsp:useBean id="software" scope="request" class="dln.beans.SoftwareBean" />
<jsp:useBean id="selection" scope="request" class="dln.beans.SoftwareBean" />
<%@ page import="java.util.*,dln.beans.*, dln.util.*" %>

<% 
	dataset.loadProcessDataset(request.getParameter("datasetId"));
	if (request.getParameter("softwareId") != null && !request.getParameter("softwareId").equals("")) {
		software.load(Integer.parseInt(request.getParameter("softwareId")), dataset);
	}
	if (request.getParameter("selectionId") != null && !request.getParameter("selectionId").equals("") && Integer.parseInt(request.getParameter("selectionId")) > 0) {
		software.load(Integer.parseInt(request.getParameter("selectionId")), dataset);
	}
	
	List<SoftwareBean> softwarePackages = SoftwareBean.getAllSoftwarePackages();
	Iterator<SoftwareBean> itr = softwarePackages.iterator();
	while (itr.hasNext()) {
		SoftwareBean entry = itr.next();
		for (SoftwareBean sw: dataset.getSoftware()) {
			if (sw.getName().equals(entry.getName())) { itr.remove(); }
		}
	}
	Collections.sort(softwarePackages);
%>

<html>
<head>
	<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
	<title>${dataset.datasetId} Software Editor</title>
	<script language="javascript" src="${pageContext.request.contextPath}/dts.js"></script>
	<script language="javascript" src="${pageContext.request.contextPath}/iven/iven.js"></script>
	<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/dts.css" />
	<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/edit.css" />
</head>
<body onLoad="${empty param.onLoad ? '' : param.onLoad}">

<form action="${pageContext.request.contextPath}/iven/edit/util/proc_software.jsp" method="POST">

<%-- Define non-editable fields needed for correctly handling the action. --%>
<input type="hidden" name="datasetId" value="${dataset.datasetId}" />
<input type="hidden" name="softwareId" value="${software.softwareId}" />
<c:if test="${!empty param.mode || empty software.softwareId}">
    <input type="hidden" name="mode" value="new" />
</c:if>

<div class="headerbar">
    <span style="padding-left: 2px;">
        DTS: ${dataset.datasetId} Software Information
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
			<th>Software Selector:</th>
			<td>
				<select ${!empty param.softwareId && param.mode != "new" ? "disabled" : ""} name="selectionId" onchange="javascript: return form.submit();">
					<%
						if (selection.getSoftwareId() == null || selection.getSoftwareId() <= 0) {
							%><option value="0">--- Select ---</option><%
						}
						for (SoftwareBean entry: softwarePackages) {
							%><option value="<%= entry.getSoftwareId() %>" <%= software.getSoftwareId() == entry.getSoftwareId() ? "selected" : "" %>><%= entry.getName() %></option><%
						}
					%>
				</select>
			</td>
		</tr>
		<tr>
			<th class="required">Name:</th>
			<td>
				<input name="name" size="65" maxlength="255" value="${software.name}" />
			</td>
		</tr>
		<tr>
			<th>Language(s):</th>
			<td><input name="language" size="65" maxlength="255" value="${software.language}" />
		</tr>
		<tr>
			<th>Deployed Directory:<br /><span class="small">This is not project specific.</span></th>
			<td><input name="deployLocation" size="65" maxlength="255" value="${software.deployLocation}" /></td>
		</tr>
		<tr>
			<th>Description:</th>
			<td><textarea name="description" rows="6" cols="60">${software.description}</textarea></td>
		</tr>
		<tr>
			<th>Repository:</th>
			<td>
				<input name="repository" size="65" maxlength="255" value="${software.repository}" />
			</td>
		</tr>
		<tr>
			<th>Repository Tag Name:</th>
			<td><input name="tagName" value="${software.tagName}" />(e.g. T-REX_2006 or Version_1.0)</td>
		</tr>
	</table>
</div>


<div class="spacer"></div>
<div class="headerbar">
    <div class="buttons">
    	<c:choose>
    		<c:when test="${empty param.softwareId}">
    			<input type="submit" name="action" value="Add" onClick="javascript: return validateSoftwareForm(this.form);" />
    		</c:when>
    		<c:otherwise>
		    	<input type=submit name=action value="Update" onClick="javascript: return validateSoftwareForm(this.form);" />
		    	<input type="submit" name="action" value="Delete" onClick="javascript: return confirmSoftwareDelete();" />
			</c:otherwise>
		</c:choose>
        <input type=submit name=action value="Cancel"/>
    </div>
</div>
</form>

	

</body>
</html>
