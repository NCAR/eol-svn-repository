<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core" %>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html" %>

<f:subview id="HeaderSubview">
    <f:loadBundle basename="resources" var="resources" />
    <h:form id="HeaderForm">
        <f:verbatim><div id="header"></f:verbatim>
            <h:outputText styleClass="headerTitle" value="#{resources.title}" />
            <h:outputText rendered="#{!empty manager.project}"
                styleClass="headerTitle"
                value=": #{manager.project.projectId}" />
        <f:verbatim></div></f:verbatim>
    </h:form>
</f:subview>
