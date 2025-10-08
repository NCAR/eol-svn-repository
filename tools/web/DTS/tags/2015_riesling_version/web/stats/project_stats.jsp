<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<%@ page language="java" contentType="text/html; charset=ISO-8859-1" pageEncoding="ISO-8859-1"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">

<%
	response.addHeader("Pragma","no-cache");
	response.setHeader("Cache-Control","no-cache, no-store, must-revalidate");
	response.addHeader("Cache-Control", "pre-check=0, post-check=0");
	response.setDateHeader("Expires",0);
	response.addHeader("Refresh","1800");
%>

<%-- Init the needed beans and objects --%>
<jsp:useBean id="stats" class="dts.stat.ProjectStat" scope="page" />

<% stats.load(request.getParameter("projectId"));
	if (request.getParameter("maximumDatasetOrderCountListSize") != null) {
		stats.setMaximumDatasetOrderCountListSize(Integer.parseInt(request.getParameter("maximumDatasetOrderCountListSize")));
	} else {
		stats.setMaximumDatasetOrderCountListSize(10);
	}
%>


<html>
<head>
	<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
	<title>${stats.projectId} Stats</title>
	<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/dts.css" />
	<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/stat.css" />
</head>

<body>
<div class="headerMenu">
	<table class="infoBar">
		<tr>
			<td class="centerPane">${stats.projectId} Statistics</td>
		</tr>
	</table>
	<table class="navBar">
		<tr>
			<td><a href="${pageContext.request.contextPath}/dln/?project=${stats.projectId}">DLN for ${stats.projectId}</a></td>
			<td><a href="${pageContext.request.contextPath}/iven/dataset_list.jsp?projectId=${stats.projectId}">Iven for ${stats.projectId}</a></td>
			<td class="dropdown">
			
			</td>
			<td><a href="${pageContext.request.contextPath}">DTS Home</a></td>
			<td><a target="_blank" href="http://www.eol.ucar.edu/about/our-organization/cds/dmg/dmg-internal/dmg-documentation/manual/data-tracking-system-guide">
					<img src="${pageContext.request.contextPath}/images/help.gif" />
				</a>
			</td>
		</tr>
	</table>
</div>

<div class="form">
<form action="project_stats.jsp">
<table>
     <th>Project:</th>
     <td>
         <select name="projectId">
<c:forEach var="project" items="${stats.projectList}">
             <option value="${project}" ${project == stats.projectId ? "selected" : ""}>${project}</option>
</c:forEach>
         </select>
     </td>
     <th>Maximum Number of Datasets to Diplay in Order Count Lists:</th>
     <td><input type="text" name="maximumDatasetOrderCountListSize" size="5" value="${stats.maximumDatasetOrderCountListSize}" /></td>
     <td><input type="submit" value="Update" /></td>
</table>
</form>
</div>

<div class="content">

<p style="font-weight: bold;">Some of the statistics on this page were not always collected by the "EMDAC" database. This may cause smaller/inaccurate counts especially for older projects. These statistics are generated dynamically from the EMDAC database and are a snapshot of the current statistics for this project.</p>

<h2>Top ${stats.datasetCountListSize} Dataset By Order Count</h2>
<p>These are the top ${stats.datasetCountListSize} datasets by order count.  It includes <u>all</u> datasets associated with the ${stats.projectId} project.  The rightmost column is the total number of unique email addresses that have requested the dataset.</p>
<table border="1" class="stats">
	<tr>
		<th class="header">Dataset ID</th>
		<th class="header">Name</th>
		<th class="header"># of Orders</th>
		<th class="header"># of Unique Emails</th>
	</tr>
	<c:forEach var="dataset" items="${stats.mostOrderedDatasets}">
	<tr>
		<th>${dataset.datasetId}</th>
		<td class="name">${dataset.name}</td>
		<td>${dataset.orderCount}</td>
		<td>${dataset.uniqueUserCount}</td>
	</tr>
	</c:forEach>
</table>

<h2>Top ${stats.nonOperationalDatasetCountListSize} Dataset By Order Count (non-Operational)</h2>
<p>These are the top ${stats.nonOperationalDatasetCountListSize} datasets by order count.  It includes <u>only</u> datasets associated with the ${stats.projectId} project, but are not associated with the Operational project.  The rightmost column is the total number of unique email addresses that have requested the dataset.</p>
<table border="1" class="stats">
	<tr>
		<th class="header">Dataset ID</th>
		<th class="header">Name</th>
		<th class="header"># of Orders</th>
		<th class="header"># of Unique Emails</th>
	</tr>
	<c:forEach var="dataset" items="${stats.mostOrderedNonOperationalDatasets}">
	<tr>
		<th>${dataset.datasetId}</th>
		<td class="name">${dataset.name}</td>
		<td>${dataset.orderCount}</td>
		<td>${dataset.uniqueUserCount}</td>
	</tr>
	</c:forEach>
</table>

<h2>General Stats</h2>
<p>Below are the general statistics for the ${stats.projectId} project.  The definitions for each column follow.</p>
<ul>
    <li><b># of Datasets</b>: Contains the number of data sets associated with ${stats.projectId}.  The numbers in parentheses are the number of online orderable data sets/number of offline orderable data sets/number of remote link data sets which are data sets not orderable through EMDAC.</li>
    <li><b>Total Disk Space Used (GB)</b>:  Total amount of disk space usage (including both locally and on the HPSS).</li>
    <li><b>Localhost Disk Space (GB)</b>:  Total local disk space usage.</li>
    <li><b>HPSS Disk Space (GB)</b>:  Total HPSS disk space usage.</li>
    <li><b>Total File Count</b>:  Total file counts (including data, docs, and EULAs) for the files on both the local disk and HPSS.</li>
    <li><b>Localhost File Count</b>:  File count for local disk only.</li>
    <li><b>HPSS File Count</b>:  File count for HPSS only.</li>
    <li><b># of Tapes</b>:  Number of offline tapes and other external media storage devices.</li>
    <li><b>Tape Size (GB)</b>:  Total size of data on external media storage devices.</li>
</ul>
<table border="1" class="stats">
    <tr>
        <th class="header">Datasets</th>
	<th class="header"># of Datasets</th>
	<th class="header">Total Disk Space Used (GB)</th>
	<th class="header">Localhost Disk Space (GB)</th>
	<th class="header">HPSS Disk Space (GB)</th>
	<th class="header">Total File Count</th>
	<th class="header">Localhost File Count</th>
	<th class="header">HPSS File Count</th>
	<th class="header"># of Tapes</th>
	<th class="header">Tape Size (GB)</th>
    </tr>
    <tr>
        <th>All Datasets</th>
	<td>${stats.datasetCount} (${stats.onlineDatasetCount}/${stats.offlineDatasetCount}/${stats.remoteLinkDatasetCount})</td>
	<td>${stats.onlineSize}</td>
	<td>${stats.localhostSize}</td>
	<td>${stats.massStoreSize}</td>
	<td>${stats.onlineFileCount}</td>
	<td>${stats.localhostFileCount}</td>
	<td>${stats.massStoreFileCount}</td>
	<td>${stats.offlineMediaCount}</td>
	<td>${stats.offlineMediaSize}</td>
    </tr>
    <tr>
        <th>Non-Operational</th>
	<td>${stats.nonOperationalDatasetCount} (${stats.nonOperationalOnlineDatasetCount}/${stats.nonOperationalOfflineDatasetCount}/${stats.nonOperationalRemoteLinkDatasetCount})</td>
	<td>${stats.nonOperationalOnlineSize}</td>
	<td>${stats.nonOperationalLocalhostSize}</td>
	<td>${stats.nonOperationalMassStoreSize}</td>
	<td>${stats.nonOperationalOnlineFileCount}</td>
	<td>${stats.nonOperationalLocalhostFileCount}</td>
	<td>${stats.nonOperationalMassStoreFileCount}</td>
	<td>${stats.nonOperationalOfflineMediaCount}</td>
	<td>${stats.nonOperationalOfflineMediaSize}</td>
    </tr>
</table>

<h2>Online Order Stats</h2>
<p>This table contains counts for the online (web) orders placed for ${stats.projectId}.  It contains the number of datasets that have been ordered at least once, the number of unique email addresses that have requested the datasets, the total number of times users have requested the datasets, the total size of all of the orders placed, and the total number of files included in the orders.</p>
<table border="1" class="stats">
    <tr>
        <th class="header">Datasets</th>
	<th class="header"># of Unique Datasets Ordered</th>
        <th class="header"># of Unique Emails</th>
        <th class="header">Total # of Orders</th>
        <th class="header">Total Order Size (GB)</th>
	<th class="header">Total # of Files Ordered</th>
    </tr>
    <tr>
        <th>All Datasets</th>
	<td>${stats.uniqueOnlineOrderedDatasetCount} out of ${stats.onlineDatasetCount}</td>
        <td>${stats.uniqueOnlineUserCount}</td>
        <td>${stats.onlineOrderCount}</td>
        <td>${stats.onlineOrderSize}</td>
        <td>${stats.orderedFileCount}</td>
    </tr>
    <tr>
        <th>Non-Operational</th>
	<td>${stats.nonOperationalUniqueOnlineOrderedDatasetCount} out of ${stats.nonOperationalOnlineDatasetCount}</td>
        <td>${stats.nonOperationalUniqueOnlineUserCount}</td>
        <td>${stats.nonOperationalOnlineOrderCount}</td>
        <td>${stats.nonOperationalOnlineOrderSize}</td>
	<td>${stats.nonOperationalOrderedFileCount}</td>
    </tr>
</table>
	

<h2>Offline Media Order Stats</h2>
<p>This table contains counts for the offline media (tape) orders placed for ${stats.projectId}.  It contains the number of datasets that have been ordered at least once, the number of unique email addresses that have requested the datasets, the total number of times users have requested the datasets, and the total number of tapes included in the orders.
<table border="1" class="stats">
    <tr>
        <th class="header">Datasets</th>
	<th class="header"># of Unique Datasets Ordered</th>
	<th class="header"># of Unique Emails</th>
	<th class="header">Total # of Orders</th>
	<th class="header">Total # of Tapes Ordered</th>
    </tr>
    <tr>
        <th>All Datasets</th>
	<td>${stats.uniqueOfflineOrderedDatasetCount} out of ${stats.offlineDatasetCount}</td>
	<td>${stats.uniqueOfflineUserCount}</td>
	<td>${stats.offlineOrderCount}</td>
	<td>${stats.orderedOfflineMediaCount}</td>
    </tr>
    <tr>
        <th>Non-Operational</th>
	<td>${stats.nonOperationalUniqueOfflineOrderedDatasetCount} out of ${stats.nonOperationalOfflineDatasetCount}</td>
	<td>${stats.nonOperationalUniqueOfflineUserCount}</td>
	<td>${stats.nonOperationalOfflineOrderCount}</td>
	<td>${stats.nonOperationalOrderedOfflineMediaCount}</td>
    </tr>
</table>


<h2>Breakdown By Email</h2>
<p>This table contains a breakdown of the ${stats.projectId} dataset orders by the final extension of the ordering email addresses.  It contains both the online and offline orders.  The table contains the number of orders placed, the number of unique data sets that were ordered, and the number of unique email addresses with that extension that have requested data sets.</p>
<p>In each column, there are three numbers.  The first number is the total number of unique email addresses found with that email extension that ordered data.  The second number (first in parentheses) is the count of unique emails that only ordered project (non-operational) associated data sets.  The third number (second in parentheses) is the count of unique emails that only ordered Operational data sets.</p>
<p><b>The total email count (first number in each column) may not equal the sum of the project and operational counts (second and third numbers in each column) because the same email address (user) may have ordered both project and operational data sets.</b></p>
<table border="1" class="stats">
    <tr>
        <th class="header">Email Extension</th>
        <th class="header"># of Orders</th>
        <th class="header"># of Unique Datasets Ordered</th>
        <th class="header"># of Unique Emails Ordered</th>
    </tr>
<c:forEach var="entry" items="${stats.emailExtensionBreakdowns}">
    <tr>
        <th>${entry.name}</th>
        <td>${entry.datasetOrderCount} (${entry.nonOperationalDatasetOrderCount} / ${entry.operationalDatasetOrderCount})</td>
	<td>${entry.orderedDatasetCount} (${entry.nonOperationalOrderedDatasetCount} / ${entry.operationalOrderedDatasetCount})</td>
	<td>${entry.userCount} (${entry.nonOperationalUserCount} / ${entry.operationalUserCount})</td>
    </tr>
</c:forEach>
</table>

<h2>Breakdown By Category</h2>
<p>This is a breakdown of the ${stats.projectId} datasets by their associated EMDAC categories.  The table below contains the total number of datasets in the database for the category, the number of times users have placed orders for datasets in the category, the number of unique datasets that have been ordered, and the number of unique email addresses that have placed the orders.</p>
<p>In each column, there are three numbers.  The first number is the total number of unique email addresses found with that email extension that ordered data.  The second number (first in parentheses) is the count of unique emails that only ordered project (non-operational) associated data sets.  The third number (second in parentheses) is the count of unique emails that only ordered Operational data sets.</p>
<p><b>The total email count (first number in each column) may not equal the sum of the project and operational counts (second and third numbers in each column) because the same email address (user) may have ordered both project and operational data sets.</b></p>
<table border="1" class="stats">
	<tr>
		<th class="header">Category</th>
		<th class="header"># of Datasets</th>
		<th class="header"># of Orders</th>
		<th class="header"># of Unique Datasets Ordered</th>
		<th class="header"># of Unique Emails Ordered</th>
	</tr>
<c:forEach var="category" items="${stats.categoryBreakdowns}">
    <tr>
        <th>${category.name}</th>
	<td>${category.datasetCount}</th>
        <td>${category.datasetOrderCount} (${category.nonOperationalDatasetOrderCount} / ${category.operationalDatasetOrderCount})</td>
	<td>${category.orderedDatasetCount} (${category.nonOperationalOrderedDatasetCount} / ${category.operationalOrderedDatasetCount})</td>
	<td>${category.userCount} (${category.nonOperationalUserCount} / ${category.operationalUserCount})</td>
    </tr>
</c:forEach>
</table>

<h2>Breakdown By Data Transfer Type</h2>
<p>This is a breakdown of orders for ${stats.projectId} by the medium in which the data was provided to the user.  The table below contains the number of times users have placed orders for the datasets, the number of unique datasets that have been ordered, and the number of unique email addresses that have placed the orders.</p>
<p>In each column, there are three numbers.  The first number is the total number of unique email addresses found with that email extension that ordered data.  The second number (first in parentheses) is the count of unique emails that only ordered project (non-operational) associated data sets.  The third number (second in parentheses) is the count of unique emails that only ordered Operational data sets.</p>
<p><b>The total email count (first number in each column) may not equal the sum of the project and operational counts (second and third numbers in each column) because the same email address (user) may have ordered both project and operational data sets.</b></p>
<table border="1" class="stats">
	<tr>
		<th class="header">Data Transfer Type</th>
		<th class="header"># of Orders</th>
		<th class="header"># of Unique Datasets Ordered</th>
		<th class="header"># of Unique Emails Ordered</th>
	</tr>
<c:forEach var="medium" items="${stats.mediumBreakdowns}">
    <tr>
        <th>${medium.name}</th>
        <td>${medium.datasetOrderCount} (${medium.nonOperationalDatasetOrderCount} / ${medium.operationalDatasetOrderCount})</td>
	<td>${medium.orderedDatasetCount} (${medium.nonOperationalOrderedDatasetCount} / ${medium.operationalOrderedDatasetCount})</td>
	<td>${medium.userCount} (${medium.nonOperationalUserCount} / ${medium.operationalUserCount})</td>
    </tr>
</c:forEach>
</table>


</div>	
</body>
</html>
