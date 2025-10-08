<%@ taglib prefix="c" uri="http://java.sun.com/jstl/core" %>

<jsp:useBean id="generalManager" class="dmg.ml.manager.GeneralManager"
    scope="session" />
<jsp:setProperty name="generalManager" property="projectById"
    value="${param.project}" />    

<c:redirect url="general/main.jsf" />