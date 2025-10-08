<%@ page import="dln.util.*,java.sql.*" %>

<%-- Define the beans used in this page. --%>
<jsp:useBean id="dataset" scope="page" class="dln.beans.DatasetBean" />
<jsp:useBean id="product" scope="page" class="dln.beans.DatasetBean" />

<%
	product.loadProcessDataset(request.getParameter("productId"));


	String action = request.getParameter( "action" );
	String onLoad = "";
	String msg = "";

	// Load the data set into the form.
	if (action.equals( "Load" )) {
		try {
			dataset.loadDataset(request.getParameter("datasetId"));
			if (!dataset.getProjectsAsList().contains(request.getParameter("projectId"))) {
				msg = "<p>Dataset "+dataset.getDatasetId()+" is not in project "+request.getParameter("projectId")+".</p>";
			} else if (product.hasSource(dataset.getDatasetId())) {
				msg = "<p>Dataset "+dataset.getDatasetId()+" is already a source dataset.</p>";
			}
		} catch (SQLException e) {
			msg = "<p>There has been an error trying to load the dataset.  The dataset may not be in the DLN.</p>";
			msg += "<p>"+e.getMessage()+"</p>";
		}
	}  
	// Add a new version to the current data set.
	else if (action.equals("Add")) {
        try {
        	product.assignSourceDataset(request.getParameter("datasetId"));
        	onLoad = "top.opener.location.reload(true);";
        	%>
        	<jsp:forward page="/iven/edit/source_info.jsp">
        		<jsp:param name="onLoad" value="<%= onLoad %>" />
        		<jsp:param name="datasetId" value="${param.datasetId}" />
        		<jsp:param name="selectedProduct" value="${param.productId}" />
        	</jsp:forward>
        	<%
        } catch (SQLException e) {
            onLoad = "";
            msg = "<p>There has been an error trying to add the dataset.  Please contact the DTS developer.</p>";
            msg += "<p>"+e.getMessage()+"</p>";
        }
	}
	// Cancel any change done in the edit form.
	else {
		onLoad = "close();";
	}
%>

<%-- Return to the data set edit form to show errors or close the window --%>
<jsp:forward page="/iven/edit/new_source.jsp">
    <jsp:param name="onLoad" value="<%= onLoad %>" />
    <jsp:param name="error" value="<%= msg %>" />
    <jsp:param name="productId" value="<%= product.getDatasetId() %>" />
</jsp:forward>
