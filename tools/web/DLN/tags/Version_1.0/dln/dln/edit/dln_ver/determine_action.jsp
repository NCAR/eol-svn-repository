<%-------------------------------------------------------------------------
  determine_action.jsp:  This jsp determines which button was pressed in 
   user in the edit_dataset.jsp page and performs the appropriate action.
   
  Author: Dan Sullivan
  Date: 2-4-2004
-------------------------------------------------------------------------%>


<%@ page import="dln.beans.*,dln.dba.*" %>

<jsp:useBean id="dataset" scope="request" class="dln.beans.DatasetBean"/>
<jsp:setProperty name="dataset" property="*"/>

<jsp:useBean id="display" scope="session" class="dln.format.DisplayBean">
	<%
		display.setDisplayView( DisplayBean.PROJECT );
		display.setDisplayId( dataset.getProject() );
		display.setCurrentView( DisplayBean.LISTING );
	%>
</jsp:useBean>

<%
	if( display.getDisplayView() == -1 )
	{
		display.setDisplayView( DisplayBean.PROJECT );
		display.setDisplayId( dataset.getProject() );
		display.setCurrentView( DisplayBean.LISTING );
	}
%>

<%
	String action = request.getParameter( "action" );
	String onLoad = "";
	String msg = "";

	if( action.equals( "Update" ) )
	{
		dataset.setNotes( DatasetBean.formatTextArea( dataset.getNotes(), 74 ) );
		boolean good = DatasetDBA.updateDB( dataset.getDBPreparedBean() );
		if( good )
		{
			onLoad = "refresh( top.opener.parent.main, " + dataset.getDsid() + "); showStatus( top.opener.parent.top_frame, \'Dataset+Has+Been+Updated\' ); close();";
			msg = "Dataset Updated";	
			%><%@ include file="reset_numbers.jsp" %><%
		}
		else
		{
			onLoad = "";
			msg = "There has been an error trying to update the dataset.  Please contact <a href=mailto: suldan@ucar.edu>Dan Sullivan</a>.";
		}
	}  
	else if( action.equals( "Add Dataset" ) )
	{
		dataset = dataset.getDBPreparedBean();
		dataset.setNotes( DatasetBean.formatTextArea( dataset.getNotes(), 74 ) );
		boolean good = DatasetDBA.insertDB( dataset );
		if( good )
		{
			onLoad = "refresh( top.opener.parent.main, " + dataset.getDsid() + "); showStatus( top.opener.parent.top_frame, \'Dataset+Has+Been+Added\' ); close();";
			msg = "Dataset Added";	

			display.setDisplayView( DisplayBean.PROJECT );
			display.setDisplayId( dataset.getProject() );
			display.setCurrentView( DisplayBean.LISTING );

			%><%@ include file="reset_numbers.jsp" %><%
			%><%@ include file="send_mail.jsp" %><%
		}
		else
		{
			onLoad = "";
			msg = "There has been an error trying to add the dataset.  Please contact <a href=mailto: suldan@ucar.edu>Dan Sullivan</a>.";
		}
	} 
	else if( action.equals( "Delete" ) )
	{
		boolean good = DatasetDBA.deleteFromDB( dataset );
		if( good )
		{
			onLoad = "refresh( top.opener.parent.main, " + dataset.getDsid() + "); showStatus( top.opener.parent.top_frame, \'Dataset+Has+Been+Deleted\' ); close();";
			msg = "Dataset Deleted";	
			%><%@ include file="reset_numbers.jsp" %><%
		}
		else
		{
			onLoad = "";
			msg = "There has been an error trying to delete the dataset.  Please contact <a href=mailto: suldan@ucar.edu>Dan Sullivan</a>.";
		}
	}
	else
	{
		onLoad = "showStatus( top.opener.parent.top_frame, \'Edit+Cancelled\' ); close();";
		msg = "Edit Cancelled";
	}
%>

<html>
<head>
	<title>DTS Edit: <%=msg %></title>
	<script language=javascript src="../jscripts/misc.js"></script>
</head>


<body onLoad="<%= onLoad %>">
<center>
<br><br>
<h2><%= msg %></h2>
<br>
<input type=submit value="Close Window" onClick="self.close(); return false;">

</body>
</html>
