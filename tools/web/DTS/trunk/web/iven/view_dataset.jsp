<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<%
    response.addHeader("Pragma","no-cache");
    response.setHeader("Cache-Control","no-cache, no-store, must-revalidate");
    response.addHeader("Cache-Control", "pre-check=0, post-check=0");
    response.setDateHeader("Expires",0);
    response.addHeader("Refresh","1800");
%>

<jsp:useBean id="project" scope="page" class="dln.beans.ProjectBean" />
<jsp:useBean id="dataset" scope="page" class="dln.beans.DatasetBean" />
<%@ page import="java.util.*,dln.beans.*, dln.util.*" %>

<% 
	project.loadProject(request.getParameter("projectId"));
	dataset.loadProcessDataset(request.getParameter("datasetId"), project);
	Map<Integer, UserBean> users = (new UserBean()).getAllUsersMap();
	Map<Integer, StatusBean> statuses = StatusBean.getAllStatusesMap();
	Map<Integer, NoteTypeBean> noteTypes = NoteTypeBean.getNoteTypesMap();
%>

<html>
<head>
	<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
	<title>${project.projectId} Data Set Processing Inventory</title>
	<script language="javascript" src="${pageContext.request.contextPath}/dts.js"></script>
	<script language="javascript" src="${pageContext.request.contextPath}/iven/iven.js"></script>
	<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/dts.css" />
	<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/iven/iven.css" />
</head>
<body>
	
<div class="headerMenu">
	<table class="infoBar">
		<tr>
			<td class="sidePane">
				<div><b>TOI:</b> ${project.beginDate} through ${project.endDate}</div>
				<div><b>Charge Number:</b> ${project.chargeNumber}</div>
			</td>
			<td class="centerPane">${project.projectId} Processed Data Set ${dataset.datasetId}</td>
			<td class="sidePane">
				<div><b>AOI:</b></div>
				<div class="aoi"><b>Min Lat:</b> ${project.minLatitude} <b>Max Lat:</b> ${project.maxLatitude}</div>
				<div class="aoi"><b>Min Lon:</b> ${project.minLongitude} <b>Max Lon:</b> ${project.maxLongitude}</div>
			</td>
		</tr>
	</table>
	<table class="navBar">
		<tr>
			<td><a href="${pageContext.request.contextPath}/dln/?project=${project.projectId}">DLN for ${project.projectId}</a></td>
			<td class="dropdown">
			
			</td>
			<td><a href="${pageContext.request.contextPath}/iven/dataset_list.jsp?projectId=${project.projectId}">Return to Project List</a></td>
			<td><a href="${pageContext.request.contextPath}/iven">IVEN Home</a></td>
			<td><a href="${pageContext.request.contextPath}">DTS Home</a></td>
			<td><a target="_blank" href="http://www.eol.ucar.edu/about/our-organization/cds/dmg/dmg-internal/dmg-documentation/manual/data-tracking-system-guide">
				<img src="${pageContext.request.contextPath}/images/help.gif" />
				</a></td>
		</tr>
	</table>
</div>

<div class="headerbar">
	<div class="floatRight"><a href="javascript: openProcessStatusWindow('${pageContext.request.contextPath}/iven/edit/process_status.jsp?datasetId=${dataset.datasetId}&projectId=${project.projectId}');"><img src="${pageContext.request.contextPath}/images/edit.gif"></a></div>
	${dataset.datasetId}: ${dataset.name}
</div>
<div class="spacer"></div>

<div class="listing">
	<table class="info">
		<tr>
			<th>Process Contact:</th>
			<td><%= dataset.getProcesser(users) == null ? "unassigned" : dataset.getProcesser(users).getPersonName() %></td>
			<th>Process Status:</th>
			<td>
				<span style="<%= dataset.getProcessStatus(statuses) == null ? "No Status" : dataset.getProcessStatus(statuses).getStyle() %>"><%= dataset.getProcessStatus(statuses) == null ? "No Status" : dataset.getProcessStatus(statuses).getName() %></span>
				<% if (dataset.hasQuestions()) { %>
					<img src="${pageContext.request.contextPath}/images/question.gif" />
				<% } %>
			</td>
		</tr>
		<tr>
			<th>Product(s):</th>
			<td colspan="3" class="fullWidth">
				<ul style="list-style-type: none; margin: 0; padding: 0;">
				<% for (DatasetBean product: dataset.getProducts()) { %>
					<li><a href="${pageContext.request.contextPath}/iven/view_dataset.jsp?projectId=${project.projectId}&datasetId=<%= product.getDatasetId() %>"><%= product.getName() %></a></li>
				<% } %>
				</ul>
			</td>
		</tr>
	</table>
</div>
	
<div class="spacer"></div>

<div class="listing">	
	<table class="info">
		<tr>
			<th>How To:</th>
			<td class="fullWidth">
				<% if (dataset.getHowtoURL().startsWith("http://")) { %>
					<a href="${dataset.howtoURL}">${dataset.howtoURL}</a>
				<% } else { %>			
					${dataset.howtoURL}		
				<% } %>
			</td>
		</tr>
		<tr>
			<th>Work Directory:</th>
			<td class="fullWidth">${dataset.workLocation}</td>
		</tr>
		<tr>
			<th>Final Data Directory:</th>
			<td class="fullWidth">${dataset.finalProcessedDataLocation}</td>
		</tr>
		<tr>
			<th>Station List File:</th>
			<td class="fullWidth">${dataset.stationListFile}</td>
		</tr>
		<tr>
			<th>Plots:</th>
			<td class="fullWidth">
				<% if (dataset.getPlotsURL().startsWith("http://")) { %>
					<a href="${dataset.plotsURL}">${dataset.plotsURL}</a>
				<% } else { %>			
					${dataset.plotsURL}		
				<% } %>
			</td>
		</tr>
		<tr>
			<th>Readme URL:</th>
			<td class="fullWidth">
				<% if (dataset.getReadmeURL().startsWith("http://")) { %>
					<a href="${dataset.readmeURL}">${dataset.readmeURL}</a>
				<% } else { %>
					${dataset.readmeURL}
				<% } %>
			</td>
		</tr>
		<tr>
			<th>EMDAC Platforms:</th>
			<td class="fullWidth">${dataset.platformString}</td>
		</tr>
	</table>
</div>

<div class="headerbar">
	Software
	<input onclick="javascript: openSoftwareWindow('${pageContext.request.contextPath}/iven/edit/software.jsp?datasetId=${dataset.datasetId}');" type="button" value="Add Software" />
</div>
<div class="spacer"></div>
<div class="listing">
	<table class="infoListing">
		<% for (SoftwareBean software: dataset.getSoftware()) { %>
			<tr class="listRow" onmouseover="javascript: rowHover(this, 'hoverRow');" onmouseout="javascript: rowHover(this, 'listRow')">
				<td>
					<div class="floatRight"><a href="javascript: openSoftwareWindow('${pageContext.request.contextPath}/iven/edit/software.jsp?datasetId=${dataset.datasetId}&softwareId=<%= software.getSoftwareId() %>');"><img src="${pageContext.request.contextPath}/images/edit.gif"></a></div>
					<% if (software.getRepository().contains("svn.eol.ucar.edu")) { %>
						<a href="<%= software.getRepository()+"/tags/"+software.getTagName() %>"><%= software.getName()+": "+software.getRepository()+"/tags/"+software.getTagName() %></a>
					<% } else { %>
						<%= software.getName()+": "+software.getRepository() %>
					<% } %>
				</td>
			</tr>
		<% } %>
	</table>
</div>

<div class="headerbar">
	<div class="floatRight" style="padding-right: 5px;"><input type="button" onclick="javascript: sourceExpander(this, 'sourceRow', 'hiddenRow', 'sourceRow');" value="Expand"></div>
	Source Data Sets
	<input onclick="javascript: openSourceWindow('${pageContext.request.contextPath}/iven/edit/new_source.jsp?projectId=${param.projectId}&productId=${dataset.datasetId}');" type="button" value="Add Source" />
</div>
<div class="spacer"></div>

<div class="listing">
	<% for (DatasetBean source: dataset.getSourceDatasets()) { %>
		<table class="infoListing">
			<tr class="<%= dataset.isExcluded(source.getDatasetId()) ? "excludeListRow" : "listRow" %>" onmouseover="javascript: rowHover(this, '<%= dataset.isExcluded(source.getDatasetId()) ? "excludeHoverRow" : "hoverRow" %>');" onmouseout="javascript: rowHover(this, '<%= dataset.isExcluded(source.getDatasetId()) ? "excludeListRow" : "listRow" %>')">
				<td colspan="2" style="text-align: left;">
					<div class="floatRight"><a href="javascript: openSourceWindow('${pageContext.request.contextPath}/iven/edit/source_info.jsp?datasetId=<%= source.getDatasetId() %>&selectedProduct=${dataset.datasetId}');"><img src="${pageContext.request.contextPath}/images/edit.gif" /></a></div>
					<% if (source.hasQuestions()) { %>
						<img src="${pageContext.request.contextPath}/images/question.gif" />
					<% } else { %>
						<img src="${pageContext.request.contextPath}/images/blank.gif" />
					<% } %>
					<% if (source.getSourceDatasets().size() > 0) { %>
						<a href="${pageContext.request.contextPath}/iven/view_dataset.jsp?projectId=${project.projectId}&datasetId=<%= source.getDatasetId() %>"><%= source.getDatasetId() %>: <%= source.getName() %></a>
					<% } else { %>
						<%= source.getDatasetId() %>: <%= source.getName() %>
					<% } %>
				</td>
			</tr>
			<tr name="sourceRow" class="hiddenRow">
				<td style="padding-left: 50px;">Readme URL:</td>
				<td class="fullWidth">
					<% if (source.getReadmeURL().startsWith("http://")) { %>
						<a href="<%= source.getReadmeURL() %>"><%= source.getReadmeURL() %></a>
					<% } else { %>
						<%= source.getReadmeURL() %>
					<% } %>
				</td>
			</tr>
			<tr name="sourceRow" class="hiddenRow">
				<td style="padding-left: 50px;">Data Directory:</td>
				<%-- This next line seems backwards, but this is how it works... --%>
				<td class="fullWidth"><%= dataset.getDataLocation(source.getDatasetId()) %></td>
			</tr>
			<tr name="sourceRow" class="hiddenRow">
				<td style="padding-left: 50px; white-space: nowrap;">UTC Offset:</td>
				<td class="fullWidth"><%= source.getUtcOffset() == null ? "Unknown" : source.getUtcOffset() %></td>
			</tr>
			<tr name="sourceRow" class="hiddenRow">
				<td style="padding-left: 50px; white-space: nowrap;">DST Flag:</td>
				<td class="fullWidth"><span class="<%= source.isDSTFlag() == null ? "" : (source.isDSTFlag() ? "yes" : "no") %>"><%= source.isDSTFlag() == null ? "Unknown" : (source.isDSTFlag() ? "Y" : "N") %></span></td>
			</tr>
			<tr name="sourceRow" class="hiddenRow">
				<td style="padding-left: 50px; white-space: nowrap;">EMDAC Platforms:</td>
				<td class="fullWidth"><%= source.getPlatforms().size() == 0 ? "" : source.getPlatformString() %></td>
			</tr>
			<tr name="sourceRow" class="hiddenRow">
				<td style="padding-left: 50px; white-space: nowrap;">Raw Data Frequency/Time Info:</td>
				<td class="fullWidth">
					<% if (source.getCollectionTimeNote().getEntryDate() != null) { %>
						<p><b><%= source.getCollectionTimeNote().getEntryDateString() %> (<%= source.getCollectionTimeNote().getAuthor(users) == null ? "" : source.getCollectionTimeNote().getAuthor(users).getPersonName() %>)</b></p>
						<%= source.getCollectionTimeNote().getNoteText() %>
					<% } %>
				</td>
			</tr>
			<tr name="sourceRow" class="hiddenRow">
				<td style="padding-left: 50px; white-space: nowrap;">Source of Information:</td>
				<td class="fullWidth">
					<% if (source.getSourceInfoNote().getEntryDate() != null) { %>
						<p><b><%= source.getSourceInfoNote().getEntryDateString() %> (<%= source.getSourceInfoNote().getAuthor(users) == null ? "" : source.getSourceInfoNote().getAuthor(users).getPersonName() %>)</b></p>
						<%= source.getSourceInfoNote().getNoteText() %>
					<% } %>
				</td>
			</tr>
		</table>
	<% } %>
</div>


    <div class="headerbar">
        Notes
        <input onClick="javascript: openNoteWindow('${pageContext.request.contextPath}/iven/edit/note.jsp?type=dataset&datasetId=${dataset.datasetId}');" type="button" value="New Note">
    </div>
    <div class="spacer"></div>
    
    <div class="noteArea">
       	<% 
       	int versionCount = 0;
       	for (NoteBean note : dataset.getReverseProcessNotes()) {
       		if (note.getAuthor(users) == null) { versionCount++; }
       	}
       	
       	
       	for (NoteBean note : dataset.getReverseProcessNotes()) {
       		out.print("<div class=\"noteDate\">");
       		out.print(note.getEntryDate().toString().substring(0, 19));
       		if (note.getAuthor(users) != null) {%>
       		    &nbsp;&nbsp;<a href="javascript: openNoteWindow('${pageContext.request.contextPath}/iven/edit/note.jsp?type=dataset&datasetId=<%= dataset.getDatasetId() %>&noteId=<%= note.getNoteId() %>')"><img src="${pageContext.request.contextPath}/images/edit.gif" /></a>&nbsp;&nbsp;
       		<%}
       		out.print(" (");
       		if (note.getAuthor(users) == null) {
       			out.print("DTS Generated");
       		} else {
       			out.print(note.getAuthor(users).getPersonName());
       		}
       		out.print(") &nbsp;<span class=\"noteType\">");
       		if (note.getAuthor(users) == null) {
                   out.print("Version ");
                   out.print(versionCount--);
       		} else {
                   out.print(note.getTypesAsString(noteTypes));
       		}
       		out.print("</span>");
       		out.print("</div>");
                
            if (note.isRevised()) {
               out.print(" <div class=\"revisedNote\">Revised ");
               out.print(note.getReviseTime().toString().substring(0, 19));
               out.print("</div>");
            }
                

        	out.println("<div class=\"noteText\">");
        	out.println(note.getNoteText());
        	out.println("</div>");
        } %>
    </div>

</body>
</html>
