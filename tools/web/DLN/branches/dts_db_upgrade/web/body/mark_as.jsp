
<%@ page errorPage="/dln/error.jsp" %>
<%@ page import="dln.beans.*,dln.dba.*,java.util.*" %>

<jsp:useBean id="display" scope="session" class="dln.format.DisplayBean">
	<jsp:forward page="/dln/end_session.jsp"/>
</jsp:useBean>

<%
	String dsids[] = request.getParameterValues( "dscheckbox" );
	String param = request.getParameter( "param" );
	int fval = 1;

	if( param != null && dsids != null ) {
		if (param.equals("readme") || param.equals("notreadme")) {
		   for (int i = 0; i < dsids.length; i++) {
		      DatasetDBA.setReadmeFlag(dsids[i],param.equals("readme"));
		   }
		} else if (param.startsWith("load")) {
                   for (int i = 0; i < dsids.length; i++) {
                      DatasetDBA.setLoadStatus(dsids[i],Integer.parseInt(param.substring(5)));
                   }
		} else if (param.startsWith("chk")) {
		   for (int i = 0; i < dsids.length; i++) {
		      DatasetDBA.setCheckStatus(dsids[i],Integer.parseInt(param.substring(4)));
                   }
                }
	}
%>


<jsp:forward page="list_view.jsp"/>
