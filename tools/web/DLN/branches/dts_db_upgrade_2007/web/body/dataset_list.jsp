<%----------------------------------------------------------------
* The dataset_list.jsp file displays a list for data sets by project or
* task contact.  It is the primary view of the DLN.
----------------------------------------------------------------%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">

<html>
<head>
	<title>List View</title>
	<script language=javascript src="${pageContext.request.contextPath}/dln.js"></script>
	<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/dln.css" />
</head>

<%-- Import tag library and needed classes --%>
<%@ page errorPage="/error.jsp" %>
<%@ page import="java.util.*,dln.beans.*,dln.display.*" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<%-- Init the needed beans and objects --%>
<jsp:useBean id="display" class="dln.display.DatasetDisplayBean" scope="session" />


<%
   // Determine which type of list is to be shown
   if (request.getParameter("project") != null && !request.getParameter("project").equals("")) {
	   display.setProjectDisplayView(request.getParameter("project"));
   } else if (request.getParameter("ingest") != null && !request.getParameter("ingest").equals("")) {
	   display.setIngestDisplayView(Integer.parseInt(request.getParameter("ingest")));
   } else if (request.getParameter("load") != null && !request.getParameter("load").equals("")) {
	   display.setLoadDisplayView(Integer.parseInt(request.getParameter("load")));
   } else if (request.getParameter("check") != null && !request.getParameter("check").equals("")) {
	   display.setCheckDisplayView(Integer.parseInt(request.getParameter("check")));
   }

   // Determine which field is to be sorted.
   if (request.getParameter("sortField") != null) {
	   display.setSortField(Integer.parseInt(request.getParameter("sortField")));
   }
   
   // Determine which data sets should be visible.
   if (request.getParameter("allFlags") != null) {
	   display.setShowChecked(request.getParameter("allFlags").equals("true") ? true : false);
	   display.setShowDocumented(request.getParameter("allFlags").equals("true") ? true : false);
	   display.setShowLoaded(request.getParameter("allFlags").equals("true") ? true : false);
	   display.setShowMaster(request.getParameter("allFlags").equals("true") ? true : false);
   }
   if (request.getParameter("checkFlag") != null) {
	   display.setShowChecked(request.getParameter("checkFlag").equals("true") ? true : false);
   }
   if (request.getParameter("docFlag") != null) {
	   display.setShowDocumented(request.getParameter("docFlag").equals("true") ? true : false);
   }
   if (request.getParameter("loadFlag") != null) {
	   display.setShowLoaded(request.getParameter("loadFlag").equals("true") ? true : false);
   }
   if (request.getParameter("masterFlag") != null) {
	   display.setShowMaster(request.getParameter("masterFlag").equals("true") ? true : false);
   }
   
   // Determine which kind of view is to be displayed.
    //display.setCurrentView( DatasetDisplayBean.LISTING ); 
%>


<% 
    // Set up variables used by the rest of the page.
    Map<Integer, UserBean> users = (new UserBean()).getAllUsersMap();
    Map<Integer, StatusBean> statuses = StatusBean.getAllStatusesMap();

	String title = "";
	if (display.isDisplayProjectList())
		title = "Datasets by Project: " + display.getDisplayId();
	else if (display.isDisplayIngesterList()) {
		UserBean ingest = users.get(Integer.parseInt(display.getDisplayId()));
		title = "Datasets by Ingest: " + ingest.getPersonName() + " ("+ ingest.getShortName() + ")";
	}
	else if (display.isDisplayLoaderList()) {
		UserBean loader = users.get(Integer.parseInt(display.getDisplayId()));
		title = "Datasets by Loader: " + loader.getPersonName() + " ("+loader.getShortName()+")";
	}
	else if (display.isDisplayCheckerList()) {
		UserBean checker = users.get(Integer.parseInt(display.getDisplayId()));
		title = "Datasets by Checker: " + checker.getPersonName() + " ("+checker.getShortName()+")";
	}
	
	List<DatasetBean> datasetList = display.update(DatasetBean.loadDatasets(display), users, statuses);
%>

<body>
    <jsp:include page="/body/fragment/dataset_titlebar.jsp">
        <jsp:param name="title" value="<%= title %>" />
        <jsp:param name="datasetCount" value="<%= datasetList.size() %>" />
    </jsp:include>
    <jsp:include page="/body/fragment/drop_downs.jsp" />


    <div class="listing">
        <table class="datasetListing">
            <tr class="header">
                <th><a href="?sortField=<%= DatasetDisplayBean.QUESTION_SORT %>">
                    <%
                    out.print("<img src=\"");%>${pageContext.request.contextPath}/images/question.gif<%
                    out.print("\">");                        
                    if (display.isSortByQuestions()) {
                        out.print("<img src=\"");%>${pageContext.request.contextPath}/images/<%
                        out.print(display.isReverseSort() ? "sorted_reverse.gif" : "sorted.gif");
                        out.print("\">");
                    }
                    %>
                    </a>
                </th>
                <th>Status</th>
                <th>
                    <a href="?sortField=<%= DatasetDisplayBean.DATE_SORT %>">Date
                    <%
                    if (display.isSortByDate()) {
                        out.print("<img src=\"");%>${pageContext.request.contextPath}/images/<%
                        out.print(display.isReverseSort() ? "sorted_reverse.gif" : "sorted.gif");
                        out.print("\">");
                    }
                    %>
                    </a>
                </th>
                <th><a href="?sortField=<%= DatasetDisplayBean.DATASET_ID_SORT %>">ID
                    <%
                    if (display.isSortById()) {
                    	out.print("<img src=\"");%>${pageContext.request.contextPath}/images/<%
                    	out.print(display.isReverseSort() ? "sorted_reverse.gif" : "sorted.gif");
                    	out.print("\">");
                    }
                    %>
                    </a>
                </th>
                <th><a href="?sortField=<%= DatasetDisplayBean.NAME_SORT %>">Name
                    <%
                    if (display.isSortByName()) {
                        out.print("<img src=\"");%>${pageContext.request.contextPath}/images/<%
                        out.print(display.isReverseSort() ? "sorted_reverse.gif" : "sorted.gif");
                        out.print("\">");
                    }
                    %>
                    </a>
                </th>
                
                <%
                if (display.getDisplayView() != DatasetDisplayBean.INGEST_CONTACT_DISPLAY_VIEW) {
                	out.print("<th><a href=\"?sortField=");
                	out.print(DatasetDisplayBean.INGEST_CONTACT_SORT);
                	out.print("\">Ingest");
                    if (display.isSortByIngester()) {
                        out.print("<img src=\"");%>${pageContext.request.contextPath}/images/<%
                        out.print(display.isReverseSort() ? "sorted_reverse.gif" : "sorted.gif");
                        out.print("\">");
                    }
                    out.println("</a></th>");
                }
                %>
                
                <%
                if (display.getDisplayView() != DatasetDisplayBean.LOAD_CONTACT_DISPLAY_VIEW) {
	                out.print("<th><a href=\"?sortField=");
	                out.print(DatasetDisplayBean.LOAD_CONTACT_SORT);
	                out.print("\">Load");
                    if (display.isSortByLoader()) {
                        out.print("<img src=\"");%>${pageContext.request.contextPath}/images/<%
                        out.print(display.isReverseSort() ? "sorted_reverse.gif" : "sorted.gif");
                        out.print("\">");
                    }
                    out.println("</a></th>");
                }
                %>

                <%
                if (display.getDisplayView() != DatasetDisplayBean.CHECK_CONTACT_DISPLAY_VIEW) {
                    out.print("<th><a href=\"?sortField=");
                    out.print(DatasetDisplayBean.CHECK_CONTACT_SORT);
                    out.print("\">Checking");
                    if (display.isSortByChecker()) {
                        out.print("<img src=\"");%>${pageContext.request.contextPath}/images/<%
                        out.print(display.isReverseSort() ? "sorted_reverse.gif" : "sorted.gif");
                        out.print("\">");
                    }
                    out.println("</a></th>");
                }
                %>
                 
                <th>&nbsp;</th>
            </tr>
            
            
            <%
            for (DatasetBean dataset: datasetList) {
            	// Print out the start of the tr tag.
            	
            	String styleClass = dataset.getDatasetId().equals(request.getParameter("datasetId")) ? "highlightRow" : "listRow";
				out.print("<tr onmouseover=\"rowHover(this, 'hoverRow');\" onmouseout=\"rowHover(this, '");
				out.print(styleClass);
				out.print("');\" class=\"");
				out.print(styleClass);
            	out.println("\">");

            	
            	// Print out the issues table cell.
                out.print("<td class=\"icon\">");
                out.print("<a name=\"" + dataset.getDatasetId() + "\"></a>");
            	out.print("<img src=\""); %>${pageContext.request.contextPath}/images/<% 
            	out.print(dataset.hasQuestions() ? "question.gif" : "blank.gif");
            	out.println("\"></td>");
            	
            	// Print out the documented, loaded, checked, and ml table cell.
                out.print("<td class=\"icon\" style=\"width: ");
                out.print(20 * (display.isDisplayProjectList() ? 4 : 3));
                out.println("px;\">");
            	out.print("<img src=\""); %>${pageContext.request.contextPath}/images/<%
                out.print(dataset.isDocumented() ? "doc.gif" : "blank.gif");
            	out.println("\">");
            	out.print("<img src=\""); %>${pageContext.request.contextPath}/images/<%
            	out.print(dataset.isLoaded(statuses) ? "loaded.gif" : "blank.gif");
            	out.println("\">");
            	out.print("<img src=\""); %>${pageContext.request.contextPath}/images/<%
            	out.print(dataset.isChecked(statuses) ? "checked.gif" : "blank.gif");
            	out.println("\">");
            	if (display.isDisplayProjectList()) {
                    out.print("<img src=\""); %>${pageContext.request.contextPath}/images/<%
                    out.print(dataset.isInMasterList(display.getDisplayId()) ? "ml.gif" : "blank.gif");
                    out.println("\">");
            	}
            	out.println("</td>");
            	
            	// Print out the date table cell.
            	out.print("<td class=\"date\">");
            	out.print(dataset.getEntryDate().toString().substring(0, 10));
            	out.println("</td>");
            	
            	// Print out the dataset id table cell.
                out.print("<td class=\"id\">");
            	if (!dataset.getDatasetId().startsWith("DTS")) {
            		out.print("<a target=\"_blank\" href=\"http://data.eol.ucar.edu/codiac/dss/id=");
            		out.print(dataset.getDatasetId());
            		out.print("\">");
            		out.print(dataset.getDatasetId());
            		out.print("</a>");
            	} else {
            		out.print(dataset.getDatasetId());
            	}
                out.println("</td>");
                
                // Print out the dataset name table cell.
                out.print("<td><a href=\"view_dataset.jsp?datasetId=");
                out.print(dataset.getDatasetId());
                out.print("&entryDate=");
                out.print(dataset.getEntryDate());
                out.print("\">");
                out.print(dataset.getName());
                out.println("</a></td>");

                // Print out the dataset ingest contact table cell.
                if (!display.isDisplayIngesterList()) {
	                out.print("<td class=\"contact\">");
	                out.print(dataset.getIngester(users) == null ? "" : dataset.getIngester(users).getShortName());
	                out.println("</td>");
                }

                // Print out the dataset load contact table cell.
                if (!display.isDisplayLoaderList()) {
	                out.print("<td class=\"contact\">");
	                out.print(dataset.getLoader(users) == null ? "" : dataset.getLoader(users).getShortName());
	                out.println("</td>");
                }

                // Print out the dataset check contact table cell.
                if (!display.isDisplayCheckerList()) {
	                out.print("<td class=\"contact\">");
	                out.print(dataset.getChecker(users) == null ? "" : dataset.getChecker(users).getShortName());
	                out.println("</td>");
                }
                
                // Print out the edit icon table cell.
                out.print("<td class=\"icon\">");
                out.print("<a href=\"javascript: editDataset('");%>${pageContext.request.contextPath}/edit/edit_dataset.jsp?mode=update&datasetId=<%
                out.print(dataset.getDatasetId());
                out.print("');\">");
                out.print("<img src=\""); %>${pageContext.request.contextPath}/images/edit.gif<%
                out.println("\">");
                out.println("</td>");

            	
            	// End the tr tag.
            	out.println("</tr>");
            }
            %>            
            
        </table>
    </div>


    <jsp:include page="/body/fragment/drop_downs.jsp" />
    <jsp:include page="/body/fragment/dataset_titlebar.jsp">
        <jsp:param name="title" value="<%= title %>" />
        <jsp:param name="datasetCount" value="<%= datasetList.size() %>" />
    </jsp:include>

</body>
</html>
