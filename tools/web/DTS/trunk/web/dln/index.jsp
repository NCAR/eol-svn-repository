<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">

<jsp:useBean id="constants" scope="page" class="dln.util.DLNConstants" />

<html>
<head><title>${constants.title} v${constants.version}</title></head>



<frameset rows="40,*" border=0>

	<%
		String left = "menu.jsp";
		String body = "body/welcome.jsp";

		String project = request.getParameter( "project" );
		if( project != null )
		{
			left += "?expand=project";
			body = "body/dataset_list.jsp?project=" + project;
		}

		String ingester = request.getParameter("ingest");
		if (ingester != null) {
			left += "?expand=ingest";
			body = "body/dataset_list.jsp?ingest="+ingester;
		}
		
		String loader = request.getParameter( "loader" );
		if( loader != null )
		{
			left += "?expand=load";
			body = "body/dataset_list.jsp?loader=" + loader;
		}

		String checker = request.getParameter( "checker" );
		if( checker != null )
		{
			left += "?expand=check";
			body = "body/dataset_list.jsp?checker=" + checker;
		}
	%>
	
	<frameset cols="*,0" framespacing=0 border=0>
		<frame name="top_frame" frameborder=0 src="top.jsp" marginwidth="0" marginheight="0"> 
	</frameset>

	<FRAMESET rows="100%" cols="200,*" framespacing=0 border=0> 
		<frame name="left" frameborder=0 src="<%= left %>" marginwidth="0" marginheight="0"> 
		<frame name="main" src="<%= body %>" frameborder=0 marginwidth="0" marginheight="0"> 
	</FRAMESET>

</frameset>
</html>
