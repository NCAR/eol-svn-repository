<%---------------------------------------------------------------
  status.jsp:  This jsp simply displays a little status page.
   The page is generally called by the showStatus() javascript
   function in the /dln/jscripts/misc.js file.

  Parameters:
   message - the message to display

  Author:  Dan Sullivan
  Date: 1-2-2003 
---------------------------------------------------------------%>


<html>
<head><title>Status</title></head>
<body bgcolor=#336699 text=#FFFFFF>

<table border=0 width=100%>
<tr width=20%><td>&nbsp; </td>
<td align=left valign=top width=80%>
	<font size="+1">
		<%= request.getParameter( "message" ) %>
	</b></font>
</td>
</table>

</body></html>
