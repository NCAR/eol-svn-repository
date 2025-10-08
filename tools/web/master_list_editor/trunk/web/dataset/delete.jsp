<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core" %>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html" %>

<f:subview id="DatasetDeleteView">
    <f:loadBundle basename="datasetForm" var="datasetForm" />
    <f:loadBundle basename="resources" var="resources" />
    
    <h:form id="DatasetDeleteForm">
        <h:inputHidden value="#{manager.dataset.originalId}" />

        <h:panelGrid columns="1" styleClass="dataForm">
            <h:panelGrid columns="1" styleClass="centered">
                <h:outputText styleClass="formHeader" value="Data Set Delete Confirmation" />
            </h:panelGrid>

            <h:panelGrid columnClasses="formTitle" columns="2" styleClass="centered">
                <h:outputText value="#{manager.dataset.datasetId}:" />
                <h:outputText value="#{manager.dataset.name}" />
            </h:panelGrid>
            
            <h:outputText escape="false" 
                    rendered="#{manager.dataset.multiProject}"
                    value="<p style=\"text-align: center;\"><b>Selecting <font color=\"red\"><i>Confirm</i></font> will <font color=\"red\">delete</font> the metadata for #{manager.dataset.datasetId} specific to #{manager.project.projectId}</b>.<br>Metadata for all other projects will remain unchanged.</p>" />
            <h:outputText escape="false"
                    rendered="#{!manager.dataset.multiProject}"
                    value="<p style=\"text-align: center;\"><b>Selecting <font color=\"red\"><i>Confirm</i></font> will <font color=\"red\">delete</font> all metadata for #{manager.dataset.datasetId}</b>.<br>This data set is only in #{manager.project.projectId}.</p>" />
            
            <h:panelGrid columnClasses="formTitle" columns="2" styleClass="centered">
                <h:outputText styleClass="warning" value="This will NOT affect any data in the data archive system." />
            </h:panelGrid>
            
            <h:panelGrid columns="3" styleClass="righted">
                <h:outputText value="" />
                <h:commandButton action="self" actionListener="#{manager.confirm}" alt="#{resources.confirmIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.confirmIcon}" />
                <h:commandButton action="self" actionListener="#{manager.cancel}" alt="#{resources.cancelIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.cancelIcon}" immediate="true" />
            </h:panelGrid>
        </h:panelGrid>
        
    </h:form>
</f:subview>
