<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core" %>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html" %>

<%
    response.addHeader("Pragma","no-cache");
    response.setHeader("Cache-Control","no-cache, no-store, must-revalidate");
    response.addHeader("Cache-Control", "pre-check=0, post-check=0");
    response.setDateHeader("Expires",0);
    response.addHeader("Refresh","1800");
%>

<f:view>
    <f:loadBundle basename="resources" var="resources" />
    <f:loadBundle basename="login" var="login" />
    <html>
    <head>
        <title>
            <h:outputText value="#{resources.title}: #{login.title}" />            
        </title>
        <link rel="stylesheet" type="text/css" href="<h:outputText value="#{facesContext.externalContext.requestContextPath}" /><h:outputText value="#{login.css}" />" />
    </head>
    <body>
        <h1><h:outputText value="#{resources.title}: #{login.title}" /></h1>
        <p><h:outputText rendered="false" value="#{login.reason}" /></p>
        <h:panelGroup rendered="#{facesContext.externalContext.requestParameterMap.error == 1}">
            <f:verbatim><p class="error"></f:verbatim>
            <h:outputText value="#{login.errorMessage}" />
            <f:verbatim></p></f:verbatim>
        </h:panelGroup>
        <form action="${pageContext.request.contextPath}/login/j_security_check" id="loginForm" method="POST">
            <h:panelGrid columnClasses="formLabel,formField" columns="2">
                <h:outputText value="#{login.username}" />
                <h:inputText id="j_username" />
                <h:outputText value="#{login.password}" />
                <h:inputSecret id="j_password" />
            </h:panelGrid>
            <h:panelGrid columns="1" styleClass="buttonTable">
                <h:panelGroup>
                    <h:commandButton alt="#{resources.loginIconAlt}"
                        image="#{facesContext.externalContext.requestContextPath}/#{resources.loginIcon}"
                        type="submit" />
                </h:panelGroup>
            </h:panelGrid>
        </form>
    </body>
    </html>
</f:view>
