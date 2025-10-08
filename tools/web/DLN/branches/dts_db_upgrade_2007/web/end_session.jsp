<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">

<jsp:useBean id="constants" scope="page" class="dln.util.DLNConstants" />

<html>
<head>
    <title>${constants.shortTitle}: End Session</title>
    <link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/dln.css" />
</head>

<body>

<div id="titlebar">${constants.title} v${constants.version}</div>
<div class="infobox">
    <p><span class="large">End of Session:</span>  Your session has expired.  Select a
        <b>Project</b>, <b>Ingest Contact</b>, <b>Load Contact</b>, or <b>Check Contact</b>
        from the menu to start a new session.</p>
    <p>The browser was left open and untouched for over an hour to cause the session to
        expire.</p>
</div>

</body>
</html>
