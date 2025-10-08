<%@ page isErrorPage="true" %>
<%@ page import="java.io.*" %>

<html><head><title>DLN: Error</title></head>
<body bgcolor=#e9e9e9 alink=#000066 vlink=#000066 link=#000066 text=black>

<br><center>
<table border=0 cellpadding=8 cellspacing=0 bgcolor=#336699 width=50%>
	<tr><td align=center><font color=white size=+3><b>Data Loading Notes v1.1</font></td></tr>
</table>
</center>
<br><br>
<font size=+2>Error: </font> The request could not be processed due to an error:
<dd><pre><%= exception.getMessage() %></pre>
<br><br>
Please contact <a href=mailto:suldan@ucar.edu>Dan Sullivan</a> and describe what you were
doing when the error occured.  Thanks.
<br><br>

<b><u>Stack Trace:</b></u><br>
<br>
<pre>
<%
	exception.printStackTrace( new PrintWriter( out, true ) );
%>

</pre>
</body>
</html>
