<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">

<%@ page isErrorPage="true" %>
<%@ page import="java.io.*" %>

<jsp:useBean id="constants" scope="page" class="dln.util.DLNConstants" />

<html>
<head>
    <title>${constants.shortTitle}: Error</title>
    <link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/dln.css" />
</head>

<body>

<div id="titlebar">${constants.title} v${constants.version}</div>
<div class="infobox">
    <p><span class="large">Error:</span>  Your request could not be processed due to an error:</p>
    <pre><%= exception.getMessage() %></pre>
</div>
<br><br>
<u><b>Stack Trace:</b></u><br>
<br>
<pre><% exception.printStackTrace( new PrintWriter( out, true ) ); %></pre>

</body>
</html>
