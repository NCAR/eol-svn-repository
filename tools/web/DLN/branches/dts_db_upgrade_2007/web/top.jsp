<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">

<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<jsp:useBean id="constants" scope="page" class="dln.util.DLNConstants" />

<html>
<head>
    <title>Top</title>
    <link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/dln.css" />
</head>

<body class="color">


<table border=0 width=100%>
    <tr>
        <td width="20%">&nbsp;</td>
        <td style="font-size: 14pt; font-weight: bold; vertical-align: middle; width: 60%;">
            <c:choose>
	            <c:when test="${empty param.message}">
	                ${constants.title}
	            </c:when>
	            <c:otherwise>
	                ${param.message}
	            </c:otherwise>
	        </c:choose>
        </td>
        <td style="font-size: 12pt; vertical-align: middle; width: 20%">
            ${constants.groupShortName} ${constants.shortTitle} v${constants.version}
        </td>
</table>

</body></html>
