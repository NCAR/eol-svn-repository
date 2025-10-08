<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<jsp:useBean id="project" scope="page" class="dln.beans.ProjectBean" />
<%@ page import="java.util.*,dln.beans.*, dln.util.*" %>

<%
    response.addHeader("Pragma","no-cache");
    response.setHeader("Cache-Control","no-cache, no-store, must-revalidate");
    response.addHeader("Cache-Control", "pre-check=0, post-check=0");
    response.setDateHeader("Expires",0);
    response.addHeader("Refresh","1800");
%>

<% 
	project.loadProject(request.getParameter("projectId"));
	Map<Integer, UserBean> users = (new UserBean()).getAllUsersMap();
	Map<Integer, StatusBean> statuses = StatusBean.getAllStatusesMap();
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
			<td class="centerPane">${project.projectId} Processed Data Set Listing</td>
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
			<%-- 
			<td><a href="${pageContext.request.contextPath}/iven/dataset_list.jsp?projectId=${project.projectId}">Project Page</a></td>
			--%>
			<td><a href="${pageContext.request.contextPath}/iven">IVEN Home</a></td>
			<td><a href="${pageContext.request.contextPath}">DTS Home</a></td>
			<td><a target="_blank" href="http://www.eol.ucar.edu/about/our-organization/cds/dmg/dmg-internal/dmg-documentation/manual/data-tracking-system-guide">
				<img src="${pageContext.request.contextPath}/images/help.gif" />
				</a></td>
		</tr>
	</table>
</div>


<div class="noteHeaderBar">
	<div class="projectNoteExpander"><input type="button" onClick="javascript: expand(this, 'statusTableBody', 'small', 'full');" value="Expand"></div>
	Processing Status <input type="button" onClick="javascript: openNewProcessedDataset('${pageContext.request.contextPath}/iven/edit/new_proc_dataset.jsp?projectId=${project.projectId}');" value="Add New" />
</div>
<div class="statusBlock">
	<table class="statusTable">
		<thead>
			<tr class="header">
				<th>Name</th>
				<th>Status</th>
				<th>Progress</th>
				<th>Counts</th>
				<th>Excluded/Total</th>
			</tr>
		</thead>
		<tbody id="statusTableBody" class="small">
			<%
			for (DatasetBean dataset: project.getProductDatasets()) {
				int percentage = 0;
				// Calculate the percentage for the width of the progress bar.
				if (dataset.getTotalProgressBarCount() > 0) {
					percentage = 100 * dataset.getCompletedProgressBarCount(statuses) / dataset.getTotalProgressBarCount();
				} else {
					percentage = dataset.getProcessStatus(statuses) != null && dataset.getProcessStatus(statuses).isResolved() ? 100 : 0; 
				}
				
				// Determine the color for the progress bar based on the style of the product's status
				String color = dataset.getProcessStatus(statuses) == null ? " color: #000033;" : dataset.getProcessStatus(statuses).getStyle();
				color = color.substring(color.indexOf(" color: ") + 8, color.indexOf(";", color.indexOf(" color: ") + 8));
				
				out.println("<tr class=\"listRow\" onmouseover=\"javascript: rowHover(this, 'hoverRow');\" onmouseout=\"javascript: rowHover(this, 'listRow');\">");
				out.println("<td class=\"name\"><a href=\"view_dataset.jsp?datasetId="+dataset.getDatasetId()+"&projectId="+project.getProjectId()+"\">"+dataset.getName()+"</a></td>");
				out.println("<td class=\"status\"><span style=\""+(dataset.getProcessStatus(statuses) == null ? "" : dataset.getProcessStatus(statuses).getStyle())+"\">"+(dataset.getProcessStatus(statuses) == null ? "" : dataset.getProcessStatus(statuses).getName())+"</span></td>");
				out.println("<td class=\"progressBar\">");
				out.println("    <div class=\"progressBarBackground\">");
				out.println("        <div class=\"progressBarForeground\" style=\"background-color: "+color+"; width: "+percentage+"%;\"></div>");
				out.println("    </div>");
				out.println("</td>");
				out.println("<td class=\"numerical\">("+dataset.getCompletedProgressBarCount(statuses)+"/"+dataset.getTotalProgressBarCount()+")</td>");
				out.println("<td class=\"numerical\">"+dataset.getExcludedDatasetCount()+"/"+dataset.getTotalDatasetCount()+"</td>");
				out.println("</tr>");
			}
			%>
		</tbody>
	</table>
</div>

<div class="noteHeaderBar">
	<div class="projectNoteExpander"><input type="button" onClick="javascript: expand(this, 'projectNoteExpansionBlock', 'projectNoteArea', 'fullNoteArea');" value="Expand"></div>
	${project.projectId} Project Notes
	<button onClick="javascript: openNoteWindow('${pageContext.request.contextPath}/iven/edit/note.jsp?type=project&projectId=${project.projectId}');" style="margin-left: 20px">New Note</button>
</div>
<div class="projectNoteArea" id="projectNoteExpansionBlock">
        <%
        	for (NoteBean note : project.getReverseNoteList()) {
        		out.print("<div class=\"noteDate\">");
        		out.print(note.getEntryDateString());
        		if (note.getAuthor(users) != null) {%>
        		    <span class="editNoteIcon"><a href="javascript: openNoteWindow('${pageContext.request.contextPath}/iven/edit/note.jsp?type=project&projectId=${project.projectId}&noteId=<%= note.getNoteId() %>');"><img src="${pageContext.request.contextPath}/images/edit.gif" /></a></span>
        		<%}
        		out.print(" (");
        		if (note.getAuthor(users) == null) {
        			out.print("DTS Generated");
        		} else {
        			out.print(note.getAuthor(users).getPersonName());
        		}
        		out.print(")</div>");
                
                if (note.isRevised()) {
                    out.print(" <div class=\"revisedNote\">Revised ");
                    out.print(note.getReviseTime().toString().substring(0, 19));
                    out.print("</div>");
                }
                

        		out.println("<div class=\"noteText\">");
        		out.println(note.getNoteText());
        		out.println("</div>");
        	}
        %>
</div>

<% for(DatasetBean product: project.getProductDatasets()) { %>
	<div class="productTree">
		<table>
			<tr class="productTitle">
				<th class="productName" colspan="3">
					<div style="float: right;"><%= product.getProcesser(users) == null ? "unassigned" : product.getProcesser(users).getPersonName() %> <span style="<%= product.getProcessStatus(statuses) == null ? "" : product.getProcessStatus(statuses).getStyle() %>"><%= product.getProcessStatus(statuses) == null ? "No Status" : product.getProcessStatus(statuses).getName() %></span></div>
					<% if (product.hasQuestions()) { %>
						<img src="${pageContext.request.contextPath}/images/question.gif" />
					<% } %>
					<a href="${pageContext.request.contextPath}/iven/view_dataset.jsp?datasetId=<%= product.getDatasetId() %>&projectId=<%= project.getProjectId() %>"><%= product.getName() %></a>
					<!--<span class="editIcon"><a href=""><img src="${pageContext.request.contextPath}/images/edit.gif"></a></span>-->
				</th>
			</tr>
			<tr class="header">
				<th>Source Dataset</th>
				<th>Contact</th>
				<th>Status</th>
			</tr>
			<% 
			List<DatasetBean> sorted = product.getSourceDatasets();
			Collections.sort(sorted, new DatasetNameComparator(false));
			for (DatasetBean source: sorted) { 
				if (!source.getDatasetId().equals(product.getDatasetId())) { %>
					<tr class="<%= product.isExcluded(source.getDatasetId()) ? "excludeListRow" : "listRow" %>" onmouseover="javascript: rowHover(this, '<%= product.isExcluded(source.getDatasetId()) ? "excludeHoverRow" : "hoverRow" %>');" onmouseout="javascript: rowHover(this, '<%= product.isExcluded(source.getDatasetId()) ? "excludeListRow" : "listRow" %>');">
						<td class="sourceName">
							<% if (source.hasQuestions()) { %>
								<img src="${pageContext.request.contextPath}/images/question.gif" />
							<% } else { %>
								<img src="${pageContext.request.contextPath}/images/blank.gif" />
							<% } %>
							<!-- <span class="editIcon"><a href=""><img src="${pageContext.request.contextPath}/images/edit.gif"></a></span>-->
							<a href="${pageContext.request.contextPath}/iven/view_dataset.jsp?datasetId=<%= source.getDatasetId() %>&projectId=<%= project.getProjectId() %>"><%= source.getName() %></a>
						</td>
						<td class="processContact"><%= source.getProcesser(users) == null ? "unassigned" : source.getProcesser(users).getPersonName() %></td>
						<td class="status">
							<% if (source.getProcessStatus(statuses) == null) { %>
								No Status
							<% } else { %>
								<span style="<%= source.getProcessStatus(statuses).getStyle() %>"><%= source.getProcessStatus(statuses).getName() %></span>
							<% } %>
						</td>
					</tr>
			<%   }
			}%>
		</table>
	</div>
<% } %>



</body>
</html>