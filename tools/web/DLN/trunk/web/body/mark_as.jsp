
<%@ page errorPage="/dln/error.jsp" %>
<%@ page import="dln.beans.*,dln.dba.*,java.util.*" %>

<jsp:useBean id="display" scope="session" class="dln.format.DisplayBean">
	<jsp:forward page="/dln/end_session.jsp"/>
</jsp:useBean>

<%
	String dsids[] = request.getParameterValues( "dscheckbox" );
	String param = request.getParameter( "param" );
	int fval = 1;

	if( param != null && dsids != null )
	{
		if( param.startsWith( "not" ) )
		{
			param = param.substring( 3 );
			fval = 0;
		} 

		for( int x = 0; x < dsids.length; x++ )
			DatasetDBA.setFlag( Integer.parseInt( dsids[x] ), param, fval );
	}
%>


<jsp:forward page="list_view.jsp"/>
