<%-------------------------------------------------------------------
  set_hide.jsp:  This page sets the showChecked or the showDocumented
    flags within the display.  i.e. this either hides or unhides datasets
    that have been checked or their readme files are complete.

  Parameters:
   **Either one of these paramaters can be used, but only one at a time
   load - true/false whether or not to HIDE the loaded datasets
   check - true/false whether or not to HIDE the checked datasets
   doc - true/false whether or not to HIDE the documented datasets

  Other files included
   /dln/connect.jsp
   utils/redirect.jsp

  Author: Dan Sullivan
  Date: 12-31-2002 
-------------------------------------------------------------------%>

<%@ page errorPage="/error.jsp" %>
<%@ page import="dln.format.DisplayBean" %>
<%@ page import="java.util.*" %>


<jsp:useBean id="display" class="dln.format.DisplayBean" scope="session">
	<jsp:forward page="/dln/end_session.jsp"/>
</jsp:useBean>

<% 
	String load = request.getParameter( "load" );
	String check = request.getParameter( "check" );
	String doc = request.getParameter( "doc" );
	String master = request.getParameter( "master" );
	String all = request.getParameter( "all" );

	if( all != null )
	{
		if( all.equals( "true" ) )
		{
			load = "true"; check="true"; doc="true"; master="true";
		}
		else
		{
			load = "false"; check="false"; doc="false"; master="false";
		}
	}

	if( check != null )
	{
		if( check.equals( "true" ) ) 
			display.setShowChecked( false ); 
		else
			display.setShowChecked( true ); 
	}

	if( doc != null )
	{
		if( doc.equals( "true" ) ) 
			display.setShowDocumented( false ); 
		else
			display.setShowDocumented( true ); 
	}

	if( load != null )
	{
		if( load.equals( "true" ) ) 
			display.setShowLoaded( false ); 
		else
			display.setShowLoaded( true ); 
	}

	if( master != null )
	{
		if( master.equals( "true" ) ) 
			display.setShowMaster( false ); 
		else
			display.setShowMaster( true ); 
	}

%>

<jsp:forward page="list_view.jsp"/>
