<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">

<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<jsp:useBean id="constants" scope="page" class="dln.util.DLNConstants" />

<html>
<head>
    <title>Top</title>
    <link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/dts.css" />
</head>

<body class="color">


<table border=0 width=100%>
    <tr>
        <td style="padding-left: 30px; padding-top: 10px;" width="20%">
        	<a target="_blank" href="http://www.eol.ucar.edu/about/our-organization/cds/dmg/dmg-internal/dmg-documentation/manual/data-tracking-system-guide"><img alt="Help" src="${pageContext.request.contextPath}/images/help.gif" /></a>
        </td>
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
