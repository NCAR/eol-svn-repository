<html>
<head><title>Data Loading Notes v1.1</title>


<frameset rows="40,*" border=0 name=main_set>

	<%
		String left = "left/left_root.html";
		String body = "body/welcome.html";

		String project = request.getParameter( "project" );
		if( project != null )
		{
			left = "left/project.jsp";
			body = "body/project_init.jsp?project=" + project;
		}

		String loader = request.getParameter( "loader" );
		if( loader != null )
		{
			left = "left/loaders.jsp";
			body = "body/loader_init.jsp?loader=" + loader;
		}

		String checker = request.getParameter( "checker" );
		if( checker != null )
		{
			left = "left/checkers.jsp";
			body = "body/checker_init.jsp?checker=" + checker;
		}
	%>
	<frameset cols="*,0" framespacing=0 border=0 name=top_set>
		<frame name="top_frame" border=0 frameborder=0 src="top/welcome.html" marginwidth="0" marginheight="0"> 
	</frameset>

	<FRAMESET rows="100%" cols="150,*" framespacing=0 border=0 name=bottom_set> 
		<frame name="left" border=0 frameborder=0 src="<%= left %>" marginwidth="0" marginheight="0"> 
		<frame name="main" src="<%= body %>" frameborder=0 marginwidth="0" marginheight="0"> 
	</FRAMESET>

</frameset>
</head>
</html>
