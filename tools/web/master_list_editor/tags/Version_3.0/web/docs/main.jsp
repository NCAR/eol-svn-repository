<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core" %>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html" %>

<f:view>
<f:loadBundle basename="resources" var="resources" />
<html>
<head>
    <title>Master List Documentation</title>
    <link rel="stylesheet" type="text/css" 
	href="${pageContext.request.contextPath}/${resources.css}" />
</head>
<body>
    <h1>Master List Documentation</h1>
    <ul>
        <li><a href="help/index.jsf">Master List Help</a></li>
	<li><a href="requirements/general">General Master List Requirements</a></li>
	<h:panelGroup rendered="false">
	   <f:verbatim>
           <li><a href="requirements/arctic">Arctic Master List Requirements</a></li>
           </f:verbatim>
        </h:panelGroup>
        <li><a href="general">Dan Sullivan's General Master List Documentation</a></li>
        <li><a href="arctic">Dan Sullivan's Arctic Master List Documentation</a></li>
    </ul>
</body>
</html>
</f:view>