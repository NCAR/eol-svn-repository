<%------------------------------------------------------------------------
  view_dataset.jsp:  This jsp generates the dataset view.  This page
   displays all of the details in the data loading notes for a particular
   dataset.  It alows the user to edit the dataset and navigate between
   individual datasets with simple arrows.

  Parameters:
   number - The listing number for the current view, used to determine
    the next/prev dataset in the list and which number this dataset is in
    the current list.

  Session/application beans used:
   display
 
  Other files included:

  Author: Dan Sullivan
  Date: 12-31-2002 
------------------------------------------------------------------------%>

<html>
<head>
	<title>View Dataset</title>
	<script language="javascript" src="../jscripts/misc.js"></script>
	<link rel="STYLESHEET" type="text/css" href="../dln_body.css">
</head>


<body bgcolor=#e9e9e9 alink=#000066 vlink=#000066 link=#000066>

<%@ page errorPage="/dln/error.jsp" %>
<%@ page import="java.util.*,dln.format.*,dln.beans.*,dln.dba.*" %>

<jsp:useBean id="display" scope="session" class="dln.format.DisplayBean">
	<jsp:forward page="/dln/end_session.jsp"/>
</jsp:useBean>

<jsp:setProperty name="display" property="currentDatasetNumber" param="number"/>

<% 
	display.setCurrentView( DisplayBean.DATASET ); 
	
	int dsNumber = display.getCurrentDatasetNumber();
%>

<%
	Vector dss = DatasetDBA.getDatasets( display );
	DatasetBean dataset = (DatasetBean) dss.get( dsNumber );

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

<center>
<table border = 0 width=100% cellpadding=1 cellspacing=1> 
	<tr><td colospan=7><font size=+1>&nbsp;</font></td></tr>

	<tr><td>
			<table border=0 width=100% cellpadding=0 cellspacing=0>
				<tr class=titleBar>
					<td width=50%>&nbsp;&nbsp;<%= title %></td>
					<td width=50% align=right>
						<%= dsNumber + 1 %> of <%= display.getDatasetCount() %> Datasets &nbsp;&nbsp;
					</td>
			</table>
		</td>
	</tr>

	<tr><td width=100% bgcolor=#dcdcdc>
		<table border = 0 width=100% cellpadding=1 cellspacing=1> 
		<tr bgcolor=#dcdcdc class=navBar>
			<td align=left width=33%>
				<font size=-1>
				<a href="javascript: editWindow( '../edit/edit_dataset.jsp?mode=update&id=<%= dataset.getDsid() %>' )">EDIT</a>
				</font>
				&nbsp;&nbsp;
				<font size=-1>
					<a target=_blank href=/cgi-bin/dpg/qcodiac/supervisor?results=ds_description&results=on_ln_pd&results=off_ln_pd&storm_id=<%= dataset.getStormId() %>>QCODIAC</a>	
				</font>
			</td>
					<td align=center bgcolor=#dcdcdc width=33%>
						<%
							if( dsNumber == 0 )
								out.print( "<img src=../images/prev-grey.gif border=0>" );
							else 
								out.print( "<a href=view_dataset.jsp?number=" + String.valueOf(dsNumber - 1) + "><img src=../images/prev.gif border=0></a>" );
							out.print( "&nbsp;&nbsp;&nbsp;" );
							if( dsNumber == display.getDatasetCount() - 1 )
								out.print( "<img src=../images/next-grey.gif border=0>" );
							else
								out.print( "<a href=view_dataset.jsp?number=" + String.valueOf(dsNumber+1) + "><img src=../images/next.gif border=0></a>" );
						%>
					</td>
			<td align=right width=33%><font size=-1>
				<%
					int view = display.getDisplayView();
					if( view == DisplayBean.PROJECT )
						out.print( "<a href=list_view.jsp>Return to Project</a>" );
					else if( view == DisplayBean.LOADERS )
						out.print( "<a href=list_view.jsp>Return to Loaders</a>" );
					else if( view == DisplayBean.CHECKERS )
						out.print( "<a href=list_view.jsp>Return to Checkers</a>" );
				%>
			</font>
			</td>
		</tr>
		</table>
	</td></tr>

	<tr><td width=100%>
		<table width=100% border = 0 cellpadding=4 cellspacing=1> 
			<tr bgcolor=white>
				<td align=right width=15%><b>Title: </b></td>
				<td width=100%>
					<table border=0 bgcolor=white cellpadding=0 cellspacing=0 width=100%>
						<tr>
							<td width=75% align=left><%= dataset.getTitle() %></td>
							<td width=75% align=left>
								<%

									if( dataset.isReadme() )
										out.print( "<img src=../images/doc.gif alt=\"Dataset Documented\">" );

									if( dataset.isLoaded() )
										out.print( "<img src=../images/loaded.gif alt=\"Dataset Loaded\">&nbsp;" );

									if( dataset.isChecked() )
										out.print( "<img src=../images/checked.gif alt=\"Dataset Checked\">&nbsp;" );

									if( dataset.isMaster() )
										out.print( "<img src=../images/ml.gif alt=\"Dataset In Master List\">&nbsp;" );

								%>
							</td>
						</tr>
					</table>
				</td>
			</tr>
			<tr bgcolor=white>
				<td align=right width=15%><b>Date: </b></td>
				<td width=100%>
					<%
						if( dataset.getDate() == null )
							out.print( "&nbsp;" );
						else
							out.print( dataset.getDate() );
					%>
				</td>
			</tr>
			<tr bgcolor=white>
				<td align=right width=15%><b>Storm Id: </b></td>
				<td width=100%>
					<a target=_blank href=http://www.joss.ucar.edu/cgi-bin/codiac/dss?<%= dataset.getStormId() %>><%= dataset.getStormId() %></a>
					&nbsp;&nbsp;&nbsp;&nbsp;
					<i>Back Door</i>: <a target=_blank href=http://www.joss.ucar.edu/cgi-bin/codiac/dss/unhide?<%= dataset.getStormId() %>><%= dataset.getStormId() %></a>
				</td>
			</tr>
			<tr bgcolor=white>
				<td align=right width=15%><b>Project: </b></td>
				<td width=100%><%=  dataset.getProject() %></td>
			</tr>
		</table>
	</td></tr>


	<tr><td width=100% align=center bgcolor=white>
		<table width=95% border=0 cellpadding=4 cellspacing=0>
			<tr>
				<td align=right width=15%> <b>Ingest Loc:</b> </td>	
				<td colspan=3><%= dataset.getIngest() %></td>
			</tr>
			<tr>
				<td align=right width=15%> <b>Archive Loc:</b> </td>	
				<td colspan=3><%= dataset.getArchive() %></td>
			</tr>
			<tr>
				<td align=right width=15%> <b>External Contact:</b> </td>	
				<td colspan=3>
				<%
					if( !((dataset.getExtEmail()).equals("")  || (dataset.getExtEmail()).equals("E-mail") )) 
					{
						out.print( "<a href=mailto:" + dataset.getExtEmail() + ">" + dataset.getExtContact() + "</a>&nbsp;&nbsp;" );
						out.print( "&lt;" + dataset.getExtEmail() + "&gt;" );
					}
					else
					{ out.print( dataset.getExtContact() ); }
				%>
				</td>
			</tr>
			<tr>
				<td align=right width=15%> <b>Internal Contact:</b> </td>	
				<td width=30%>
				<%
					UserBean int_contact = dataset.getIntContactBean();
					if( int_contact.getEmail() == null || int_contact.getEmail().equals("") )
					{
						out.print( int_contact.getFirstName() + " " + int_contact.getLastName()  );
					}
					else
					{
						out.print( "<a href=mailto:" + int_contact.getEmail() + ">" + int_contact.getFirstName() + " " + int_contact.getLastName() + "</a>&nbsp;&nbsp;" );
						out.print( "&lt;" + int_contact.getEmail() + "&gt;" );
					}
				%>
				</td>
				<td align=right width=15%> <b>In Master List:</b> </td>	
				<td width=30%>
				<%
					if( dataset.isMaster() )
						out.print( "<font color=blue><b>YES</font></b>" );
					else
						out.print( "<font color=red><b>NO</font></b>" );
				%>
				</td>
			</tr>
			<tr>
			<tr>
				<td align=right width=15% nowrap> &nbsp;</td>	
				<td width=30%>
					&nbsp;
				</td>
				<td align=right width=15% nowrap> <b>Readme Complete:</b> </td>	
				<td width=30%>
				<%
					if( dataset.isReadme() )
						out.print( "<font color=blue><b>YES</font></b>" );
					else
						out.print( "<font color=red><b>NO</font></b>" );
				%>
				</td>
			</tr>
				<td align=right width=15% nowrap> <b>Person Loading:</b></td>	
				<td width=30%>
				<%
					UserBean loader = dataset.getLoaderBean();
					if( loader.getEmail() == null || loader.getEmail().equals("") )
					{
						out.print( loader.getFirstName() + " " + loader.getLastName()  );
					}
					else
					{
						out.print( "<a href=mailto:" + loader.getEmail() + ">" + loader.getFirstName() + " " + loader.getLastName() + "</a>&nbsp;&nbsp;" );
						out.print( "&lt;" + loader.getEmail() + "&gt;" );
					}
				%>
				</td>
				<td align=right width=15% nowrap> <b>Loading Complete:</b> </td>	
				<td width=30%>
				<%
					if( dataset.isLoaded() )
						out.print( "<font color=blue><b>YES</font></b>" );
					else
						out.print( "<font color=red><b>NO</font></b>" );
				%>
				</td>
			</tr>
			<tr>
				<td align=right width=15%> <b>Person Checking:</b> </td>	
				<td width=30%>
				<%
					UserBean checker = dataset.getCheckerBean();

					if( checker.getEmail() == null || checker.getEmail().equals( "" )  )
					{ 
						out.print( checker.getFirstName() + " " + checker.getLastName() ); 
					}
					else
					{
						out.print( "<a href=mailto:" + checker.getEmail() + ">" + checker.getFirstName() + " " + checker.getLastName() + "</a>&nbsp;&nbsp;" );
						out.print( "&lt;" + checker.getEmail() + "&gt;" );
					}
				%>
				</td>
				<td align=right width=15%> <b>Dataset Checked:</b> </td>	
				<td width=30%>
				<%
					if( dataset.isChecked() )
						out.print( "<font color=blue><b>YES</font></b>" );
					else
						out.print( "<font color=red><b>NO</font></b>" );
				%>
				</td>
			</tr>
			<tr>
				<td align=right width=15%> <b>Remote URL:</b> </td>	
				<td colspan=3>
				<%
					if( !((dataset.getRemoteUrl()).equals( "none" ) || (dataset.getRemoteUrl()).equals("") )) 
					{
						out.print( "<a href=" + dataset.getRemoteUrl() + ">" + dataset.getRemoteUrl() + "</a>" );
					}
				%>
				</td>
			</tr>
			</td>
			</tr>
		</table>	
	</td></tr>
	
	<tr><td width=100% bgcolor=white align=center>
		<table width=100% border=0 cellpadding=4 cellspacing=0>
			<tr>
				<td bgcolor=#dcdcdc align=center width=100%><b>Notes</b></td>
			</tr>
			<tr>
				<td width=95%><pre><%= dataset.getNotes() %></pre></td>
			</tr>
			</tr>
		</table>
	</td></tr>

	<tr><td width=100%>
		<table border = 0 width=100% cellpadding=1 cellspacing=1> 
		<tr bgcolor=#dcdcdc>
			<td align=left width=100%>&nbsp;</td>
		</tr>
		</table>
	</td></tr>

	<tr><td>
			<table border=0 width=100% cellpadding=0 cellspacing=0>
				<tr class=titleBar>
					<td width=50%>&nbsp;&nbsp;<%= title %></td>
					<td width=50% align=right>
						<%= dsNumber + 1 %> of <%= display.getDatasetCount() %> Datasets &nbsp;&nbsp;
					</td>
			</table>
		</td>
	</tr>
</table>

</body></html>
