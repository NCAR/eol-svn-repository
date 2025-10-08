<%-------------------------------------------------------------------
  drop_downs.jsp:  

  * The <jsp:include .../> action must be used.

  Including files:
   project_view.jsp
   loader_view.jsp
   checker_view.jsp

  Session/application beans used:
   display
  
  Author:  Dan Sullivan
  Date:  12-31-02
-------------------------------------------------------------------%>

<%@ page import="dln.format.DisplayBean" %>

<jsp:useBean id="display" class="dln.format.DisplayBean" scope="session"/>	

	<tr><td width=100% colspan=8>
		<table border=0 width=100% cellpadding=0 cellspacing=0>
			<tr class=hideBar>
				<td align=left width=50%>
					<select name=params onChange="setParam( this, parent, this.form)">
						<option value=prompt selected>Mark as:</option>
						<option value=master>In ML</p>
						<option value=readme>Documented</p>
						<option value=loaded>Loaded</p>
						<option value=checked>Checked</p>
						<option value=notmaster>NOT In ML</p>
						<option value=notreadme>NOT Documented</p>
						<option value=notloaded>NOT Loaded</p>
						<option value=notchecked>NOT Checked</p>
					</select>
						&nbsp;
				</td>
				<td align=right width=50%>
					<select name=hide onChange="setHide( this, 'true', parent)">
						<option value=prompt selected>------Hide------</option>
						<% if( display.getShowLoaded() )
							{ %>
								<option value=load>Loaded</option>
							<% }

						  if( display.getShowChecked() )
							{ %>
								<option value=check>Checked</option>
							<% }

						  if( display.getShowDocumented() )
							{ %>
								<option value=doc>Documented</option>

							<% }

						  if( display.getShowMaster() )
							{ %>
								<option value=master>In ML</option>
							<% }
						%>
					</select>&nbsp;&nbsp;&nbsp;

					<select name=show onChange="setHide( this, 'false', parent)">
						<option value=prompt selected>------Show------</option>
						<% if( !display.getShowLoaded() )
							{ %>
								<option value=load>Loaded</option>
							<% }

						  if( !display.getShowChecked() )
							{ %>
								<option value=check>Checked</option>
							<% }

						  if( !display.getShowDocumented() )
							{ %>
								<option value=doc>Documented</option>
							<% }

						  if( !display.getShowMaster() )
							{ %>
								<option value=master>In Master</option>
							<% }
						%>
							<option value=all>All</option>
						</select>&nbsp;&nbsp;
				</td>
			</tr>
		</table>
	</td><tr>
