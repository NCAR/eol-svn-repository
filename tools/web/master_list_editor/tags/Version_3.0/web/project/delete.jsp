<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core" %>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html" %>

<f:subview id="ProjectDeleteView">
    <f:loadBundle basename="projectForm" var="projectForm" />
    <f:loadBundle basename="resources" var="resources" />
    
    <h:form id="ProjectDeleteForm" style="background-color: red">
        <h:inputHidden value="#{manager.project.originalId}" />

        <h:panelGrid columns="1" styleClass="dataForm">
            <h:panelGrid columns="1" styleClass="centered">
                <h:outputText styleClass="formHeader" value="Project Delete Confirmation" />
            </h:panelGrid>

            <h:panelGrid columnClasses="formTitle" columns="1" styleClass="centered">
                <h:outputText value="#{manager.project.projectId}" />
            </h:panelGrid>
            
            <h:outputText escape="false"
                    value="<p style=\"text-align: center;\"><b>Selecting <font color=\"red\"><i>Confirm</i></font> will <font color=\"red\">PERMANENTLY delete</font> all metadata for #{manager.project.projectId}.</b><br>Data sets <b>ONLY</b> associated with <b>#{manager.project.projectId}</b> will be <b>COMPLETELY</b> deleted.<br>Data sets associated with other projects are retained." />
            
            <h:panelGrid columnClasses="formTitle" columns="2" styleClass="centered">
                <h:outputText styleClass="warning" value="This will NOT affect any data in the data archive system." />
            </h:panelGrid>

            <h:panelGrid columnClasses="formTitle" columns="2" styleClass="centered">
                <h:outputText escape="false" styleClass="warning" value="<font size=+2>Are you sure you want to delete #{manager.project.projectId}?</font>" />
            </h:panelGrid>            
            
            <h:panelGrid columns="3" styleClass="righted">
                <h:outputText value="" />
                <h:commandButton action="self" actionListener="#{manager.confirm}" alt="#{resources.confirmIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.confirmIcon}" />
                <h:commandButton action="self" actionListener="#{manager.cancel}" alt="#{resources.cancelIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.cancelIcon}" immediate="true" />
            </h:panelGrid>
        </h:panelGrid>
        
    </h:form>
</f:subview>
