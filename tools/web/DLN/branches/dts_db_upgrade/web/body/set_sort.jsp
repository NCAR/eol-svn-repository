<%----------------------------------------------------------------------
  set_sort.jsp: This jsp sets the desired sort field in the display bean.
  
  Parameters:
   field - the field number by which to set the sort, these numbers 
    correspond to the constants defined in DisplayBean.java

  Other included files:
   utils/redirect.jsp

  Author: Dan Sullivan
  Date: 12-31-2002
----------------------------------------------------------------------%>

<%@ page errorPage="/error.jsp" %>
<%@ page import="dln.format.DisplayBean" %>

<jsp:useBean id="display" class="dln.format.DisplayBean" scope="session">
	<jsp:forward page="/dln/end_session.jsp"/>
</jsp:useBean>

<%
	int field = Integer.parseInt( request.getParameter( "field" ) );
 
	if( display.getSortField() == field )
	{
		if( display.getSortDirection() == DisplayBean.ASC )
			display.setSortDirection( DisplayBean.DESC );
		else
			display.setSortDirection( DisplayBean.ASC );
	}
	else
	{
		if( field == DisplayBean.DATE )
			display.setSortDirection( DisplayBean.DESC );
		else
			display.setSortDirection( DisplayBean.ASC );
			
		display.setSortField( field );
	}
%>


<jsp:forward page="list_view.jsp"/>

