<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">

<jsp:useBean id="constants" scope="page" class="dln.util.DLNConstants" />

<html>
<head>
	<title>${constants.title}</title>
	<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/dln.css" />
</head>

<body>

<div style="padding-bottom: 50px"></div>

<div id="titlebar">
    <p class="mainTitle">${constants.title}</p>
    <p class="subTitle">Welcome to the ${constants.groupShortName} ${constants.title} System</p>
</div>

<div style="margin: 50px 25%; text-align: center;">
    <span style="padding-right: 20px;"><a href="project_list.jsp">Add/Edit Projects</a></span>
    <span style="padding-left: 20px;"><a href="user_list.jsp">Add/Edit Users</a></span>
</div>

</body>
</html>
