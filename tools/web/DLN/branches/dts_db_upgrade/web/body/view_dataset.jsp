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
	<link rel="STYLESHEET" type="text/css" href="../css/dln_body.css">
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

	StatusBean loadedStatus = StatusDBA.getFromDB(dataset.getLoadedStatusId());
	StatusBean checkedStatus = StatusDBA.getFromDB(dataset.getCheckedStatusId());
	UserBean int_contact = UserDBA.getFromDB(dataset.getInternalContactId());
	UserBean externalContact = UserDBA.getFromDB(dataset.getExternalContactId());
	UserBean loader = UserDBA.getFromDB(dataset.getLoaderId());
	UserBean checker = UserDBA.getFromDB(dataset.getCheckerId());

	if (externalContact == null) { externalContact = new UserBean(); }
	if (loader == null) { loader = new UserBean(); }
	if (checker == null) { checker = new UserBean(); }
	if (loadedStatus == null) { loadedStatus = new StatusBean(); }
	if (checkedStatus == null) { checkedStatus = new StatusBean(); }

	String title = "";
	if( display.getDisplayView() == DisplayBean.PROJECT )
		title = "Datasets by Project: " + display.getDisplayId();
	else if( display.getDisplayView() == DisplayBean.LOADERS )
	{
		//UserBean loader = UserDBA.getFromDB( (new Integer(display.getDisplayId())).intValue() );
		title = "Datasets by Loader: " + loader.getName();
	}
	else if( display.getDisplayView() == DisplayBean.CHECKERS )
	{
		//UserBean checker = UserDBA.getFromDB( (new Integer(display.getDisplayId())).intValue() );
		title = "Datasets by Checker: " + checker.getName();
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
				<a href="javascript: editWindow( '../edit/edit_dataset.jsp?mode=update&id=<%= dataset.getDatasetId() %>' )">EDIT</a>
				</font>
				&nbsp;&nbsp;
				<font size=-1>
					<a target=_blank href=/cgi-bin/dpg/qcodiac/supervisor?results=ds_description&results=on_ln_pd&results=off_ln_pd&storm_id=<%= dataset.getDatasetId() %>>QCODIAC</a>	
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
							<td width=75% align=left><%= dataset.getName() %></td>
							<td width=75% align=left>
								<%

									if( dataset.getReadme() )
										out.print( "<img src=../images/doc.gif alt=\"Dataset Documented\">" );

									if( loadedStatus.isDone() )
										out.print( "<img src=../images/loaded.gif alt=\"Dataset Loaded\">&nbsp;" );

									if( checkedStatus.isDone() )
										out.print( "<img src=../images/checked.gif alt=\"Dataset Checked\">&nbsp;" );

									if( dataset.getMaster() )
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
						if( dataset.getEnteredDate() == null )
							out.print( "&nbsp;" );
						else
							out.print( dataset.getEnteredDate() );
					%>
				</td>
			</tr>
			<tr bgcolor=white>
				<td align=right width=15%><b>Storm Id: </b></td>
				<td width=100%>
					<a target=_blank href=http://www.joss.ucar.edu/cgi-bin/codiac/dss?<%= dataset.getDatasetId() %>><%= dataset.getDatasetId() %></a>
					&nbsp;&nbsp;&nbsp;&nbsp;
					<i>Back Door</i>: <a target=_blank href=http://www.joss.ucar.edu/cgi-bin/codiac/dss/unhide?<%= dataset.getDatasetId() %>><%= dataset.getDatasetId() %></a>
				</td>
			</tr>
			<tr bgcolor=white>
				<td align=right width=15%><b>Project: </b></td>
				<td width=100%><%=  dataset.getProjects().toString() %></td>
			</tr>
			<tr bgcolor=white>
				<td align=right width=15%><b>Dataset Type: </b></td>
				<td width=100%><%= dataset.getDatasetType() %></td>
			</tr>
		</table>
	</td></tr>


	<tr><td width=100% align=center bgcolor=white>
		<table width=95% border=0 cellpadding=4 cellspacing=0>
			<tr>
				<td align=right width=15%> <b>Ingest Loc:</b> </td>	
				<td colspan=3><%= dataset.getIngestDirectory() %></td>
			</tr>
			<tr>
				<td align=right width=15%> <b>Data to Archive:</b></td>
				<td colspan=3><%= dataset.getLoadDirectory() %></td>
			</tr>
			<tr>
				<td align=right width=15%> <b>Archive Loc:</b> </td>	
				<td colspan=3><%= dataset.getArchiveDirectory() %></td>
			</tr>
			<tr>
				<td align=right width=15%> <b>External Contact:</b> </td>	
				<td colspan=3>
				<%
					if( !(externalContact.getEmail().equals("") || externalContact.getEmail().equals("E-mail") )) 
					{
						out.print( "<a href=mailto:" + externalContact.getEmail() + ">" + externalContact.getName() + "</a>&nbsp;&nbsp;" );
						out.print( "&lt;" + externalContact.getEmail() + "&gt;" );
					}
					else
					{ out.print( externalContact.getName() ); }
				%>
				</td>
			</tr>
			<tr>
				<td align=right width=15%> <b>Internal Contact:</b> </td>	
				<td width=30%>
				<%
					if( int_contact.getEmail() == null || int_contact.getEmail().equals("") )
					{
						out.print( int_contact.getName()  );
					}
					else
					{
						out.print( "<a href=mailto:" + int_contact.getEmail() + ">" + int_contact.getName() + "</a>&nbsp;&nbsp;" );
						out.print( "&lt;" + int_contact.getEmail() + "&gt;" );
					}
				%>
				</td>
				<td align=right width=15%> <b>In Master List:</b> </td>	
				<td width=30%>
				<%
					if( dataset.getMaster() )
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
					if( dataset.getReadme() )
						out.print( "<font color=blue><b>YES</font></b>" );
					else
						out.print( "<font color=red><b>NO</font></b>" );
				%>
				</td>
			</tr>
				<td align=right width=15% nowrap> <b>Person Loading:</b></td>	
				<td width=30%>
				<%
					if (loader != null) {
						if( loader.getEmail() == null || loader.getEmail().equals("") )
						{
							out.print( loader.getName()  );
						}
						else
						{
							out.print( "<a href=mailto:" + loader.getEmail() + ">" + loader.getName() + "</a>&nbsp;&nbsp;" );
							out.print( "&lt;" + loader.getEmail() + "&gt;" );
						}
					}
				%>
				</td>
				<td align=right width=15% nowrap> <b>Loading Complete:</b> </td>	
				<td width=30%>
				<%
					if( loadedStatus.isDone() )
						out.print( "<font color=blue><b>"+loadedStatus.getName()+"</font></b>" );
					else
						out.print( "<font color=red><b>"+loadedStatus.getName()+"</font></b>" );
				%>
				</td>
			</tr>
			<tr>
				<td align=right width=15%> <b>Person Checking:</b> </td>	
				<td width=30%>
				<%
					if (checker != null) {
						if( checker.getEmail() == null || checker.getEmail().equals( "" )  )
						{ 
							out.print( checker.getName() ); 
						}
						else
						{
							out.print( "<a href=mailto:" + checker.getEmail() + ">" + checker.getName() + "</a>&nbsp;&nbsp;" );
							out.print( "&lt;" + checker.getEmail() + "&gt;" );
						}
					}
				%>
				</td>
				<td align=right width=15%> <b>Dataset Checked:</b> </td>	
				<td width=30%>
				<%
					if( checkedStatus.isDone() )
						out.print( "<font color=blue><b>"+checkedStatus.getName()+"</font></b>" );
					else
						out.print( "<font color=red><b>"+checkedStatus.getName()+"</font></b>" );
				%>
				</td>
			</tr>

			<tr>
				<td align=right width=15%> <b>Remote URLs:</b> </td>	
				<td colspan=3>
					<table border=1>
				<%
					java.util.Iterator urls = dataset.getLinks().iterator();
					while (urls.hasNext()) {
						XlinkBean link = (XlinkBean)urls.next();
						out.print("<tr><td>"+link.getPurpose()+"</td>");
						out.print("    <td><a target=\"_blank\" href=\""+link.getUrl()+"\">"+
							(link.getTitle().equals("") ? link.getUrl() : link.getTitle()) +
							"</a></td></tr>");
					}
/*
					if( !(dataset.getRemoteUrl() == null || dataset.getRemoteUrl().equals( "none" ) || dataset.getRemoteUrl().equals("") )) 
					{
						out.print( "<a href=" + dataset.getRemoteUrl() + ">" + dataset.getRemoteUrl() + "</a>" );
					}
*/
				%>
					</table>
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
				<td width=95%><pre><%= dataset.getNote() %></pre></td>
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
