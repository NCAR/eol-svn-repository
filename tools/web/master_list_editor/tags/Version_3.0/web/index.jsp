<%@ taglib prefix="c" uri="http://java.sun.com/jstl/core" %>

<jsp:useBean id="manager" class="dmg.ml.manager.MasterListManager" scope="session" />
<jsp:setProperty name="manager" property="projectById" value="${param.project}" />    

<c:redirect url="main.jsf" />
