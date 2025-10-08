<%------------------------------------------------------------------------
* The view_dataset.jsp page displays the detailed view of a single data set.  It
* includes the specific details for the data set, each task of the data set, the
* projects associated with the data set, and all notes for the data set.
------------------------------------------------------------------------%>

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">

<%@ page errorPage="/error.jsp" %>
<%@ page import="java.util.*,dln.display.*,dln.beans.*" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<jsp:useBean id="display" scope="session" class="dln.display.DatasetDisplayBean">
    <jsp:forward page="/end_session.jsp"/>
</jsp:useBean>

<html>
<head>
	<title>View Dataset</title>
	<script language="javascript" src="${pageContext.request.contextPath}/dln.js"></script>
    <link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/dln.css">
</head>



<%
    // Set up the display bean to filter the notes displayed on the page.
	if (request.getParameter("authorFilterId") != null) {
	    display.setAuthorFilterSelection(Integer.parseInt(request.getParameter("authorFilterId")));
	}
	if (request.getParameter("noteTypeFilterId") != null) {
	    display.setNoteTypeFilterSelection(Integer.parseInt(request.getParameter("noteTypeFilterId")));
	}



	Map<Integer, UserBean> users = (new UserBean()).getAllUsersMap();
	Map<Integer, StatusBean> statuses = StatusBean.getAllStatusesMap();
	Map<Integer, IngestTypeBean> ingestTypes = IngestTypeBean.getAllIngestTypesMap();
	Map<Integer, NoteTypeBean> noteTypes = NoteTypeBean.getNoteTypesMap();

	DatasetBean dataset = DatasetBean.loadDataset(request.getParameter("datasetId"), request.getParameter("entryDate"), display);

	String title = "";
	if (display.isDisplayProjectList())
		title = "Datasets by Project: " + display.getDisplayId();
	else if (display.isDisplayIngesterList()) {
		UserBean ingest = users.get(Integer.parseInt(display.getDisplayId()));
		title = "Datasets by Ingest: " + ingest.getPersonName() + " (" + ingest.getShortName() + ")";
	} else if (display.isDisplayLoaderList()) {
		UserBean loader = users.get(Integer.parseInt(display.getDisplayId()));
		title = "Datasets by Loader: " + loader.getPersonName() + " (" + loader.getShortName() + ")";
	} else if (display.isDisplayCheckerList()) {
		UserBean checker = users.get(Integer.parseInt(display.getDisplayId()));
		title = "Datasets by Checker: " + checker.getPersonName() + " (" + checker.getShortName() + ")";
	}

    //display.setCurrentView(DatasetDisplayBean.DATASET);
    display.setDatasetDisplayView();
%>

<body>
    <jsp:include page="fragment/dataset_titlebar.jsp">
        <jsp:param name="title" value="<%= title %>" />
        <jsp:param name="datasetCount" value="${display.datasetCount}" />
        <jsp:param name="datasetIndex" value="<%= dataset.getListIndex() %>" />
    </jsp:include>
    
    <div class="dropdownBar">
        <div class="dropdown">
            <span class="small">
                <button onClick="javascript: location='${pageContext.request.contextPath}/body/dataset_list.jsp';">
                    RETURN TO 
                    <%
                	if (display.isDisplayProjectList()) {
                		out.print("PROJECT");
                	} else if (display.isDisplayIngesterList()) {
                		out.print("INGEST CONTACT");
                	} else if (display.isDisplayLoaderList()) {
                		out.print("LOAD CONTACT");
                	} else if (display.isDisplayCheckerList()) {
                		out.print("CHECK CONTACT");
                	}
                %>
                    LIST
                </button>
            </span>
        </div>
        <div class="button">
            <span class="small">
                <button onClick="javascript: editDataset( '${pageContext.request.contextPath}/edit/edit_dataset.jsp?mode=update&datasetId=<%= dataset.getDatasetId() %>' )">EDIT</button>
                 &nbsp; 
                <button onClick="javascript: openQCODIAC('/cgi-bin/qcodiac/supervisor?results=ds_description&results=file_list&results=tape_list&dataset_id=<%= dataset.getDatasetId() %>');">QCODIAC</button>
            </span>
        </div>
        <div class="status">
            <%
            	if (dataset.getListIndex() == 1) {
            %>
                <img src="${pageContext.request.contextPath}/images/prev-grey.gif">
            <%
            	} else {
            %>
                <a href="view_dataset.jsp?datasetId=<%= dataset.getPreviousDataset().getDatasetId() %>&entryDate=<%= dataset.getPreviousDataset().getEntryDate() %>"><img src="${pageContext.request.contextPath}/images/prev.gif"></a>
            <%
            	}
            %>
            &nbsp;&nbsp;&nbsp;
            <%
            	if (dataset.getListIndex() == display.getDatasetCount()) {
            %>
                <img src="${pageContext.request.contextPath}/images/next-grey.gif">
            <%
            	} else {
            %>
                <a href="view_dataset.jsp?datasetId=<%= dataset.getNextDataset().getDatasetId() %>&entryDate=<%= dataset.getNextDataset().getEntryDate() %>"><img src="${pageContext.request.contextPath}/images/next.gif"></a>
            <%
            	}
            %>
        </div>
    </div>
    
    
    <div class="listing">
        <table class="info">
            <tr>
                <th>Name:</th>
                <td><%=dataset.getName()%></td>
            </tr>
            <tr>
                <th>Dataset ID:</th>
                <td>
                    <div class="floatRight">
                        <b>Status:</b>
                        <%
                           if (!dataset.hasQuestions() && !dataset.isDocumented() && !dataset.isLoaded(statuses) && !dataset.isChecked(statuses)) {
                        	   out.print("No Status");
                           }
                        %>
                        <%
                        	if (dataset.hasQuestions()) {
                        %><img src="${pageContext.request.contextPath}/images/question.gif"><%
                        	}
                        %>
                        <%
                        	if (dataset.isDocumented()) {
                        %><img src="${pageContext.request.contextPath}/images/doc.gif"><%
                        	}
                        %>
                        <%
                        	if (dataset.isLoaded(statuses)) {
                        %><img src="${pageContext.request.contextPath}/images/loaded.gif"><%
                        	}
                        %>
                        <%
                        	if (dataset.isChecked(statuses)) {
                        %><img src="${pageContext.request.contextPath}/images/checked.gif"><%
                        	}
                        %>
                    </div>
                    <a target="_blank" href="http://data.eol.ucar.edu/codiac/dss/id=<%= dataset.getDatasetId() %>"><%=dataset.getDatasetId()%></a>
                    &nbsp;&nbsp;&nbsp;&nbsp;
                    <a class="small" target="_blank" href="http://data.eol.ucar.edu/codiac/dss/unhide/id=<%= dataset.getDatasetId() %>">Back Door</a>
                </td>
            </tr>
            <tr>
                <th>Contacts:</th>
                <td>
	                <div class="floatRight">
		                <%
		                	UserBean intContact = dataset.getInternalContact(users);
		                	if (intContact.getEmail() != null
		                			&& !intContact.getEmail().equals("")) {
		                		out.print("<a href=\"mailto:" + intContact.getEmail() + "\">"
		                				+ intContact.getPersonName() + "</a>&nbsp;&nbsp");
		                		out.print("&lt;" + intContact.getEmail() + "&gt;");
		                	} else {
		                		out.print(intContact == null ? "" : intContact.getPersonName());
		                	}
		                %> (<b>Internal</b>)
	                </div>
                    <%
                    	UserBean extContact = dataset.getExternalContact(users);
                    	if (extContact != null && extContact.getEmail() != null
                    			&& !extContact.getEmail().equals("")) {
                    		out.print("<a href=\"mailto:" + extContact.getEmail() + "\">"
                    				+ extContact.getPersonName() + "</a>&nbsp;&nbsp");
                    		out.print("&lt;" + extContact.getEmail() + "&gt;");
                    	} else {
                    		out.print(extContact == null ? "unassigned" : extContact
                    				.getPersonName());
                    	}
                    %> (<b>External</b>)
	            </td>
            </tr>
            <tr>
                <th>Remote URL:</th>
                <td>
                    <%
                    	if (dataset.getRemoteURL() != null) {
                    		out.print("<a href=\"");
                    		out.print(dataset.getRemoteURL());
                    		out.print("\">");
                    		out.print(dataset.getRemoteURL());
                    		out.print("</a>");
                    	}
                    %>
                </td>
            </tr>
        </table>
    </div>
    
    <div class="spacer"></div>
    
    <div class="listing">
        <table class="info">
            <tr>
                <td class="datasetProject" rowspan="3">
                    <table class="info">
                        <tr>
                            <th class="datasetProject">Projects</th>
                            <th class="datasetProject">In ML</th>
                        </tr>
                        <%
                        	List<String> sorted = dataset.getProjectsAsList();
                        	Collections.sort(sorted);
                        	for (String project : sorted) {
                        		out.println("<tr>");
                        		out.print("    <td class=\"projectName\">");
                        		if (dataset.isInMasterList(project)) {
                        			out.print("<a target=\"_blank\" href=\"http://data.eol.ucar.edu/master_list/?project=");
                        			out.print(project);
                        			out.print("\">");
                        			out.print(project);
                        			out.println("</a>");
                        		} else {
                        			out.print(project);
                        		}
                        		%><a target="_blank" href="http://dmg.eol.ucar.edu/master_list/?project=<%= project %>"><img src="${pageContext.request.contextPath}/images/edit.gif"></a><%
                        		out.print("</td>");
                        		out.print("    <td class=\"ml\">");
                        		if (dataset.isInMasterList(project)) { 
                        			%>
                        			<img src="${pageContext.request.contextPath}/images/checked.gif" />
                        			<%
                            	} else {
                            			out.print("&nbsp;");
                            		}
                            		out.print("</td>");
                            		out.println("</tr>");
                            	}
                            %>
                    </table>
                </td>
                <td>
                    <table class="info">
                        <tr>
                            <td><div class="floatLeft"><b>Ingest Location:</b> <%=dataset.getIngestLocation()%></div></td>
                        </tr>
                        <tr>
                            <td><div class="floatLeft"><b>Ingest Type:</b> <%= dataset.getIngestType(ingestTypes) == null ? "Unknown" : dataset.getIngestType(ingestTypes).getName() %></div></td>
                        </tr>
                        <tr>
                            <td>
                                <div class="floatRight">
                                    <b>Ingest Status: </b>
			                        <%
			                        	if (dataset.getIngestStatus(statuses) != null) {
			                        		out.print("<span style=\""
			                        				+ dataset.getIngestStatus(statuses).getStyle() + "\">");
			                        		out.print(dataset.getIngestStatus(statuses).getName());
			                        		out.print("</span>");
			                        	} else {
			                        		out.print("No Status");
			                        	}
			                        %>
                                </div>
                                <div class="floatLeft">
                                    <b>Ingest Contact: </b>
			                        <%
			                        	if (dataset.getIngester(users) != null) {
			                        		if (dataset.getIngester(users).getEmail() == null
			                        				|| dataset.getIngester(users).getEmail().equals("")) {
			                        			out.print(dataset.getIngester(users).getPersonName());
			                        		} else {
			                        			out.print("<a href=mailto:"
			                        					+ dataset.getIngester(users).getEmail() + ">"
			                        					+ dataset.getIngester(users).getPersonName()
			                        					+ "</a>&nbsp;&nbsp;");
			                        			out.print("&lt;" + dataset.getIngester(users).getEmail()
			                        					+ "&gt;");
			                        		}
			                        	}
			                        %>
			                    </div>
			                 </td>
                        </tr>
                    </table>
                </td>
            </tr>
            <tr>
                <td>
                    <table class="info">
                        <tr>
                            <td><div class="floatLeft"><b>Data to be Archived:</b> <%=dataset.getLoadDataLocation()%></div></td>
                        </tr>
                        <tr>
                            <td><div class="floatLeft"><b>Archive Location:</b> <%=dataset.getArchiveLocation()%></div></td>
                        </tr>
                        <tr>
                            <td>
                                <div class="floatRight"><b>Readme Complete: </b>
                                    <%
                                    	if (dataset.isDocumented()) {
                                    		out.print("<span class=\"yes\">YES</span>");
                                    	} else {
                                    		out.print("<span class=\"no\">NO</span>");
                                    	}
                                    %>
                                </div>
                            </td>
                        </tr>
                        <tr>
                            <td>
                                <div class="floatRight">
                                    <b>Load Status: </b>
                                    <%
                                    	if (dataset.getLoadStatus(statuses) != null) {
                                    		out.print("<span style=\""
                                    				+ dataset.getLoadStatus(statuses).getStyle() + "\">");
                                    		out.print(dataset.getLoadStatus(statuses).getName());
                                    		out.print("</span>");
                                    	} else {
                                    		out.print("No Status");
                                    	}
                                    %>
                                </div>
                                <div class="floatLeft">
                                    <b>Load Contact: </b>
	                                <%
	                                	if (dataset.getLoader(users) != null) {
	                                		if (dataset.getLoader(users).getEmail() == null
	                                				|| dataset.getLoader(users).getEmail().equals("")) {
	                                			out.print(dataset.getLoader(users).getPersonName());
	                                		} else {
	                                			out.print("<a href=mailto:"
	                                					+ dataset.getLoader(users).getEmail() + ">"
	                                					+ dataset.getLoader(users).getPersonName()
	                                					+ "</a>&nbsp;&nbsp;");
	                                			out.print("&lt;" + dataset.getLoader(users).getEmail()
	                                					+ "&gt;");
	                                		}
	                                	}
	                                %>
	                            </div>
	                         </td>
                        </tr>
                    </table>
                </td>
            </tr>
            <tr>
                <td>
	                <table class="info">
                        <tr>
                            <td>
                                <div class="floatRight">
                                    <b>Check Status: </b>
                                    <%
                                    	if (dataset.getCheckStatus(statuses) != null) {
                                    		out.print("<span style=\""
                                    				+ dataset.getCheckStatus(statuses).getStyle() + "\">");
                                    		out.print(dataset.getCheckStatus(statuses).getName());
                                    		out.print("</span>");
                                    	} else {
                                    		out.print("No Status");
                                    	}
                                    %>
                                </div>
                                <div class="floatLeft">
                                    <b>Check Contact: </b>
	                                <%
	                                	if (dataset.getChecker(users) != null) {
	                                		if (dataset.getChecker(users).getEmail() == null
	                                				|| dataset.getChecker(users).getEmail().equals("")) {
	                                			out.print(dataset.getChecker(users).getPersonName());
	                                		} else {
	                                			out.print("<a href=mailto:"
	                                					+ dataset.getChecker(users).getEmail() + ">"
	                                					+ dataset.getChecker(users).getPersonName()
	                                					+ "</a>&nbsp;&nbsp;");
	                                			out.print("&lt;" + dataset.getChecker(users).getEmail()
	                                					+ "&gt;");
	                                		}
	                                	}
	                                %>
	                            </div>
	                        </td>
                        </tr>
	                </table>
                </td>
            </tr>
        </table>
    </div>
    
    <div class="noteHeaderBar">
        <div class="floatRight" style="padding-right: 10px">
            <select name="authorFilterId" onChange="javascript: changeAuthorFilter(this, '${param.datasetId}', '${param.entryDate}');">
                <option value="-1">---Show All---</option>
               <%
                List<UserBean> sortedAuthors = new ArrayList<UserBean>(dataset.getNoteAuthors(users));
                Collections.sort(sortedAuthors, new dln.util.UserNameComparator(false));
                for (UserBean author: sortedAuthors) {
                    out.print("<option value=\"");
                    out.print(author.getContactId());
                    out.print("\"");
                    if (display.getAuthorFilterSelection() == author.getContactId()) {
                        out.print(" selected");
                    }
                    out.print(">");
                    out.print(author.getPersonName());
                    out.println("</option>");
                }
                %>
                <option value="0"<%= display.getAuthorFilterSelection() == 0 ? " selected" : "" %>>DTS Generated</option>
             </select>
         </div>
        <div class="floatLeft" style="padding-left: 10px">
            <select name="noteTypeFilterId" onChange="javascript: changeNoteTypeFilter(this, '${param.datasetId}', '${param.entryDate}');">
                <option value="-1">---Show All---</option>
                <%
                List<NoteTypeBean> sortedNoteTypes = new ArrayList<NoteTypeBean>(noteTypes.values());
                Collections.sort(sortedNoteTypes);
                for (NoteTypeBean type: sortedNoteTypes) {
                	out.print("<option value=\"");
                	out.print(type.getTypeId());
                	out.print("\"");
                	if (display.getNoteTypeFilterSelection() == type.getTypeId()) {
                		out.print(" selected");
                	}
                	out.print(">");
                	out.print(type.getName());
                	out.println("</option>");
                }
                %>
                <option value="0"<%= display.getNoteTypeFilterSelection() == 0 ? " selected" : "" %>>Version</option>
            </select>
        </div>
        Notes
        <button onClick="javascript: editNote( '${pageContext.request.contextPath}/edit/edit_note.jsp?datasetId=<%= dataset.getDatasetId() %>' )">Add Note</button></div>
    <div class="spacer"></div>
    
    <div class="noteArea">
        <%
        	for (NoteBean note : dataset.getReverseNoteList(display.getNoteTypeFilterSelection(), display.getAuthorFilterSelection())) {
        		out.print("<div class=\"noteDate\">");
        		out.print(note.getEntryDate().toString().substring(0, 19));
        		if (note.getAuthor(users) != null) {%>
        		    &nbsp;&nbsp;<a href="javascript: editNote('${pageContext.request.contextPath}/edit/edit_note.jsp?mode=update&datasetId=<%= dataset.getDatasetId() %>&noteId=<%= note.getNoteId() %>')"><img src="${pageContext.request.contextPath}/images/edit.gif" /></a>&nbsp;&nbsp;
        		<%}
        		out.print(" (");
        		if (note.getAuthor(users) == null) {
        			out.print("DTS Generated");
        		} else {
        			out.print(note.getAuthor(users).getPersonName());
        		}
        		out.print(") &nbsp;<span class=\"noteType\">");
        		if (note.getAuthor(users) == null) {
                    out.print("Version");
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
        	}
        %>
    </div>
    
    <div class="spacer"></div>
    
    <jsp:include page="fragment/dataset_titlebar.jsp">
        <jsp:param name="title" value="<%= title %>" />
        <jsp:param name="datasetCount" value="${display.datasetCount}" />
        <jsp:param name="datasetIndex" value="<%= dataset.getListIndex() %>" />
    </jsp:include>
</body>
</html>
