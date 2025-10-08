<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core" %>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html" %>

<f:subview id="DatasetDeleteView">
    <f:loadBundle basename="datasetForm" var="datasetForm" />
    <f:loadBundle basename="resources" var="resources" />
    
    <h:form id="DatasetDeleteForm">
        <h:inputHidden value="#{generalManager.dataset.originalId}" />

        <h:panelGrid columns="1" styleClass="dataForm">
            <h:panelGrid columns="1" styleClass="centered">
                <h:outputText styleClass="formHeader" value="Data Set Delete Confirmation" />
            </h:panelGrid>

            <h:panelGrid columnClasses="formTitle" columns="2" styleClass="centered">
                <h:outputText value="#{generalManager.dataset.datasetId}:" />
                <h:outputText value="#{generalManager.dataset.name}" />
            </h:panelGrid>
            
            <h:outputText escape="false" 
                    rendered="#{generalManager.dataset.multiProject}"
                    value="<p style=\"text-align: center;\"><b>Selecting <font color=\"red\"><i>Confirm</i></font> will <font color=\"red\">delete</font> the metadata for #{generalManager.dataset.datasetId} specific to #{generalManager.project.projectId}</b>.<br>Metadata for all other projects will remain unchanged.</p>" />
            <h:outputText escape="false"
                    rendered="#{!generalManager.dataset.multiProject}"
                    value="<p style=\"text-align: center;\"><b>Selecting <font color=\"red\"><i>Confirm</i></font> will <font color=\"red\">delete</font> all metadata for #{generalManager.dataset.datasetId}</b>.<br>This data set is only in #{generalManager.project.projectId}.</p>" />
            
            <h:panelGrid columnClasses="formTitle" columns="2" styleClass="centered">
                <h:outputText styleClass="warning" value="This will NOT affect any data in the data archive system." />
            </h:panelGrid>
            
            <h:panelGrid columns="3" styleClass="righted">
                <h:outputText value="" />
                <h:commandButton action="self" actionListener="#{generalManager.confirm}" alt="#{resources.confirmIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.confirmIcon}" />
                <h:commandButton action="self" actionListener="#{generalManager.cancel}" alt="#{resources.cancelIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.cancelIcon}" immediate="true" />
            </h:panelGrid>
        </h:panelGrid>
        
    </h:form>
</f:subview>
