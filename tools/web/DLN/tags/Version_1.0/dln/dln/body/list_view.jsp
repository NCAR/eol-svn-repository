<%----------------------------------------------------------------
  list_view.jsp: This jsp generates the listing view.  The listing
   view can be by either project, loader or checker.  The type
   of view to display is determined from the DisplayBean which
   is generally created by the project_init.jsp, loader_init.jsp
   and checker_init.jsp pages.  

  Parameters:
   hLight (optional) - this parameter specifies the id number of the
    dataset to highlight in the table to denote a change has been made
    to that dataset.
   
  Other files included:
    drop_downs.jsp

  Session/application beans used:
    display 
	
  Author: Dan Sullivan
  Date:  12-31-02 - 1-2-2004
----------------------------------------------------------------%>

<html>
<head>
	<title>Project View</title>
	<script language=javascript src=../jscripts/misc.js></script>

	<link rel="STYLESHEET" type="text/css" href="../dln_body.css">
</head>

<%-- Import tag library and needed classes --%>
<%@ page errorPage="/dln/error.jsp" %>
<%@ page import="java.util.*,dln.beans.*,dln.dba.*,dln.format.*" %>

<%-- Init the needed beans and objects --%>
<jsp:useBean id="display" class="dln.format.DisplayBean" scope="session">
	<jsp:forward page="/dln/end_session.jsp"/>
</jsp:useBean>

<body>

<% 
	display.setCurrentView( DisplayBean.LISTING ); 
	display.setCurrentDatasetNumber( 0 ); 

	String title = "";
	if( display.getDisplayView() == DisplayBean.PROJECT )
		title = "Datasets by Project: " + display.getDisplayId();
	else if( display.getDisplayView() == DisplayBean.LOADERS )
	{
		UserBean loader = UserDBA.getFromDB( display.getDisplayId() );
		title = "Datasets by Loader: " + loader.getFirstName() + " " + loader.getLastName();
	}
	else if( display.getDisplayView() == DisplayBean.CHECKERS )
	{
		UserBean checker = UserDBA.getFromDB( display.getDisplayId() );
		title = "Datasets by Checker: " + checker.getFirstName() + " " + checker.getLastName();
	}
%>

<%
	Vector datasets = DatasetDBA.getDatasets( display );
	display.setDatasetCount( datasets.size() );
%>

<%-- Display the table --%>
<center>
<form method="POST" action="mark_as.jsp" name=mark_as_form>
<input type=hidden valuen="none" name="param">

<table border=0 width=100% cellpadding=1 cellspacing=1 class=listTable> 
	<tr><td colspan=7><font size=+1>&nbsp;</font></td></tr>

	<tr class=titleBar>
		<td colspan=8>
			<table border=0 width=100% cellpadding=0 cellspacing=0>
				<tr class=titleBar>
					<td width=50%>
						&nbsp;&nbsp;<b><%= title %>
					</td>
					<td width=50% align=right><b>
						<%= display.getDatasetCount() %> Datasets&nbsp;&nbsp;
					</td>
				</tr>
			</table>
		</td>
	</tr>

	<tr class=blankBar>
		<td colspan=8 align=center>
			&nbsp;
		</td>
	</tr>

	<jsp:include page="drop_downs.jsp"/>

	<%-- Column titles --%>
	<tr>
		<td colspan=2 width=6% class=noSortHeading>&nbsp;</td>
		<%
			String sfield = "sortHeading";
			String nosfield = "noSortHeading"; 
			String fcolor = nosfield;
		%>

		<% if( display.getSortField() == DisplayBean.DATE ) fcolor = sfield; else fcolor = nosfield; %>
		<td class=<%= fcolor %> width=7% nowrap>
			<a href=set_sort.jsp?field=<% out.print(DisplayBean.DATE); %>>Date</a>
		</td>

		<% if( display.getSortField() == DisplayBean.TITLE ) fcolor = sfield; else fcolor = nosfield; %>
		<td class=<%= fcolor %> width=45% nowrap>
			<a href=set_sort.jsp?field=<% out.print(DisplayBean.TITLE); %>>Title</a>
		</td>

		<% if( display.getSortField() == DisplayBean.STORM_ID ) fcolor = sfield; else fcolor = nosfield; %>
		<td class=<%= fcolor %> width=6% nowrap>
			<a href=set_sort.jsp?field=<% out.print(DisplayBean.STORM_ID); %>>Storm id</a>
		</td>

		<%
			if( display.getDisplayView() == DisplayBean.LOADERS || display.getDisplayView() == DisplayBean.CHECKERS )
			{
				if( display.getSortField() == DisplayBean.PROJ ) fcolor = sfield; else fcolor = nosfield;
				%>
					<td class=<%= fcolor %> width=7% nowrap>
						<a href=set_sort.jsp?field=<% out.print(DisplayBean.PROJ); %>>Project</a>
					</td>
				<%
			}	
		%>

		<%
			if( display.getDisplayView() == DisplayBean.PROJECT || display.getDisplayView() == DisplayBean.CHECKERS )
			{
				if( display.getSortField() == DisplayBean.LOADING ) fcolor = sfield; else fcolor = nosfield; 
				%>
					<td class=<%= fcolor %> width=7% nowrap>
						<a href=set_sort.jsp?field=<% out.print(DisplayBean.LOADING); %>>Loading</a>
					</td>
				<%
			}
		%>

		<%
			if( display.getDisplayView() == DisplayBean.PROJECT || display.getDisplayView() == DisplayBean.LOADERS )
			{
				if( display.getSortField() == DisplayBean.CHECKING ) fcolor = sfield; else fcolor = nosfield; 
				%>
					<td class=<%= fcolor %> width=7% nowrap>
						<a href=set_sort.jsp?field=<% out.print(DisplayBean.CHECKING); %>>Checking</a>
					</td>
				<%
			}
		%>

		<td class=noSortHeading width=4% nowrap><font size=+1>&nbsp;</td>
	</tr>	
	<%---------------------%>

	<%-- Loop through each dataset and display it in the table --%>
	<%
		Enumeration dss = datasets.elements();

		String hlight = request.getParameter( "hlight" );
		if( hlight == null )
			hlight = "-1";

		int count = 0;

		while( dss.hasMoreElements() ) 
		{
			DatasetBean ds = (DatasetBean) dss.nextElement();

			String trclass = "listRow";
			if( ds.getDsid() == Integer.parseInt( hlight ) )
				trclass="hlightRow";
	%> 
	<tr id=row_<%= count %> class="<%= trclass %>" onmouseover="cursorOver(this);" onmouseout="cursorOut( this );">
		<td width=2%>
			<input type=checkbox name=dscheckbox value="<%= ds.getDsid() %>" onClick="selectRow( '<%= count %>', this );">
		</td>
		<td nowrap width=4%>
			<table border=0 cellpadding=0 cellspacing=1 width=100%>
				<tr>
			<%
				if( ds.isReadme() )
					out.print( "<td width=25% align=center><img src=../images/doc.gif alt=\"Dataset Documented\"></td>" );
				else
					out.print( "<td width=25% align=center><img src=../images/blank.gif></td>" );

				if( ds.isLoaded() )
					out.print( "<td width=25% align=center><img src=../images/loaded.gif alt=\"Dataset Loaded\"></img></td>" );
				else
					out.print( "<td width=25% align=center><img src=../images/blank.gif></td>" );

				if( ds.isChecked() )
					out.print( "<td width=25% align=center><img src=../images/checked.gif alt=\"Dataset Checked\"></img></td>" );
				else
					out.print( "<td width=25% align=center><img src=../images/blank.gif></td>" );

				if( ds.isMaster() )
					out.print( "<td width=25% align=center><img src=../images/ml.gif alt=\"Dataset In Master List\"></img></td>" );
				else
					out.print( "<td width=25% align=center><img src=../images/blank.gif></td>" );
			%>
		
				</tr>
			</table>
		</td>


		<td nowrap align=center>
			<%
				if( ds.getDate() == null )
					out.print( "&nbsp;" );
				else
					out.print( ds.getDate() );
			%>

		</td>
		<td>
			<a 
				href=view_dataset.jsp?number=<%= count %>
				onMouseOver="status='View Data Tracking System Details'; return true;"
				onMouseOut="status=''">	
				<%= ds.getTitle()  %>
			</a>
		</td>
		<td nowrap>
			<a 
				href=http://www.joss.ucar.edu/cgi-bin/codiac/dss?<%= ds.getStormId() %> 
				target=_blank
				onMouseOver="status='View Dataset in Codiac'; return true;"
				onMouseOut="status=''">
				<%= ds.getStormId() %>
			</a>
		</td>

		<%
			if( display.getDisplayView() == DisplayBean.LOADERS || display.getDisplayView() == DisplayBean.CHECKERS )
			{
				%>
					<td><%= ds.getProject() %></td>
				<%
			}
		%>

		<%
			if( display.getDisplayView() == DisplayBean.PROJECT || display.getDisplayView() == DisplayBean.CHECKERS )
			{
				%>
					<td><%= ds.getLoaderBean().getFirstName() %></td>
				<%
			}
		%>
		<%
			if( display.getDisplayView() == DisplayBean.PROJECT || display.getDisplayView() == DisplayBean.LOADERS )
			{
				%>
					<td><%= ds.getCheckerBean().getFirstName() %></td>
				<%
			}
		%>

		<td align=center valign=center>
			<a
				href="javascript: editWindow( '../edit/edit_dataset.jsp?mode=update&id=<%= ds.getDsid() %>')"
				onMouseOver="status='Edit Dataset'; return true;"
				onMouseOut="status=''">
				<img src=../images/edit.gif border=0 alt="Edit Dataset">
			</a>
		</td>
	</tr>

	<% count++; } %>


	<jsp:include page="drop_downs.jsp"/>

	<tr class=blankBar>
		<td colspan=8 align=center>
			&nbsp;
		</td>
	</tr>

	<tr class=titleBar>
		<td colspan=8>
			<table border=0 width=100% cellpadding=0 cellspacing=0>
				<tr class=titleBar>
					<td width=50%>
						&nbsp;&nbsp;<b><%= title %>
					</td>
					<td width=50% align=right><b>
						<%= display.getDatasetCount() %> Datasets&nbsp;&nbsp;
					</td>
				</tr>
			</table>
		</td>
	</tr>

	<tr><td colspan=8>&nbsp;</td></tr>
	<%
		if( !display.getShowLoaded() )
			out.print( "<tr><td colspan=8 align=center><img src=../images/loaded.gif>&nbsp;&nbsp;<i>Loaded Datasets are not displayed.</b></i></td></tr>" );

		if( !display.getShowChecked() )
			out.print( "<tr><td colspan=8 align=center><img src=../images/checked.gif>&nbsp;&nbsp;<i>Checked Datasets are not displayed.</b></i></td></tr>" );

		if( !display.getShowDocumented() )
			out.print( "<tr><td colspan=8 align=center><img src=../images/doc.gif>&nbsp;&nbsp;<i>Datasets with completed readme files are not displayed.</b></i></td></tr>" );

		if( !display.getShowMaster() )
			out.print( "<tr><td colspan=8 align=center><img src=../images/ml.gif>&nbsp;&nbsp;<i>Datasets that have been entered into the Master List are not displayed.</b></i></td></tr>" );
	%>

</table></form>
</body></html>
