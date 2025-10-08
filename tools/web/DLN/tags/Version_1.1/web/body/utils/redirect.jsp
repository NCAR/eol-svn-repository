<%--------------------------------------------------------------
  redirect.jsp:  This is a simple block of code that checks the
   status of the current listing view and determines which page
   to forward the user to.

  * This must be included using the <jsp:include ...> action 

  Sessoin/application beans used:
     display

  Author: Dan Sullivan
  Date: 1-2-2003
--------------------------------------------------------------%>

<%@ page import="dln.format.DisplayBean" %>

<jsp:useBean id="display" class="dln.format.DisplayBean" scope="session"/>

<% if( display.getDisplayView() == DisplayBean.PROJECT )
	{ %>
			<jsp:forward page="../project_view.jsp"/> 
	<%}%>

<% if( display.getDisplayView() == DisplayBean.LOADERS ) 
	{ %>
		<jsp:forward page="../loader_view.jsp"/>
	<%}%>

<% if( display.getDisplayView() == DisplayBean.CHECKERS ) 
	{ %>
			<jsp:forward page="../checker_view.jsp"/>
	<%}%>

<% if( display.getDisplayView() == DisplayBean.RECENT ) 
	{ %>
		<jsp:forward page="../recent_view.jsp"/>
	<%}%>
