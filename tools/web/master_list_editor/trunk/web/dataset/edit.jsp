<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core" %>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html" %>

<f:subview id="DatasetEditView">
    <f:loadBundle basename="datasetForm" var="datasetForm" />
    <f:loadBundle basename="resources" var="resources" />

    <h:form id="DatasetEditForm">
        
        <h:panelGrid columns="1" styleClass="dataForm">
            <h:inputHidden id="originalId"
                    rendered="#{!empty manager.dataset.originalId}"
                    value="#{manager.dataset.originalId}" />

            <h:panelGrid columns="1" styleClass="centered">
                <h:outputText styleClass="formHeader" value="Global Dataset Metadata" />
            </h:panelGrid>

            <h:panelGrid columnClasses="formTitle,formValue" columns="2" styleClass="dataFormRow">
                <h:outputText value="#{datasetForm.datasetId}" />
                <h:panelGroup>
                    <h:inputText value="#{manager.dataset.datasetId}" />
                    <h:commandButton action="self"
                            actionListener="#{manager.dataset.load}" 
                            alt="#{resources.loadIconAlt}"
                            image="#{facesContext.externalContext.requestContextPath}/#{resources.loadIcon}"
                            rendered="#{empty manager.dataset.originalId}" />
                </h:panelGroup>
                <h:outputText value="#{datasetForm.name}" />
                <h:inputText styleClass="fullField" value="#{manager.dataset.name}" />
                <h:outputText value="#{datasetForm.author}" />
                <h:inputText styleClass="fullField" value="#{manager.dataset.authorPi}" />
                <h:outputText value="#{datasetForm.url}" />
                <h:panelGrid columns="2" columnClasses="fullField,icon">
                    <h:inputText styleClass="fullField" value="#{manager.dataset.url}" />
                    <h:commandButton action="self"
                            actionListener="#{manager.dataset.setDefaultUrl}"
                            alt="#{resources.defaultIconAlt}"
                            image="#{facesContext.externalContext.requestContextPath}/#{resources.defaultIcon}" />
                </h:panelGrid>
                <h:outputText value="#{datasetForm.docUrl}" />
                <h:panelGrid columns="2" columnClasses="fullField,icon">
                    <h:inputText styleClass="fullField" value="#{manager.dataset.docUrl}" />
                    <h:commandButton action="self"
                            actionListener="#{manager.dataset.setDefaultDocUrl}"
                            alt="#{resources.defaultIconAlt}"
                            image="#{facesContext.externalContext.requestContextPath}/#{resources.defaultIcon}" />
                </h:panelGrid>
            </h:panelGrid>

            <h:panelGrid columnClasses="formTitle,dateField,todayIcon" columns="3" styleClass="centered">
                <h:outputText value="#{datasetForm.preliminary}" />
                <h:selectBooleanCheckbox value="#{manager.dataset.preliminary}" />
                <h:outputText value="" />

                <h:outputText value="#{datasetForm.expectedDate}" />
                <h:inputText styleClass="dateField" value="#{manager.dataset.dateExpected}" />
                <h:outputText value="" />

                <h:outputText value="#{datasetForm.updatedDate}" />
                <h:inputText styleClass="dateField" value="#{manager.dataset.dateUpdated}" />
                <h:commandButton action="self" actionListener="#{manager.dataset.setUpdatedToToday}" alt="#{resources.todayIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.todayIcon}" />
            </h:panelGrid>
            
            <h:panelGrid columns="1" styleClass="centered">
                <h:outputText styleClass="formHeader" value="#{manager.project.projectId} Specific Metadata" />
            </h:panelGrid>
            
            
            <h:panelGrid columnClasses="formTitle,dateField,todayIcon" columns="3" styleClass="centered">
                <h:outputText value="#{datasetForm.enteredDate}" />
                <h:inputText styleClass="dateField" value="#{manager.dataset.datePosted}" />
                <h:commandButton action="self" actionListener="#{manager.dataset.setEnteredToToday}" alt="#{resources.todayIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.todayIcon}" />
            </h:panelGrid>
                
            <h:panelGrid columns="1" styleClass="centered">
                <h:panelGrid columnClasses="formTitle,formValue" columns="4">
                    <h:outputText value="#{datasetForm.inProgress}" />
                    <h:selectBooleanCheckbox value="#{manager.dataset.inProgress}" />
                    <h:outputText value="#{datasetForm.hidden}" />
                    <h:selectBooleanCheckbox value="#{manager.dataset.hidden}" />
                </h:panelGrid>
            </h:panelGrid>

            <h:panelGrid columns="3" styleClass="dataFormRow">
                <h:outputText styleClass="listHeader" value="#{datasetForm.selectorExcludeTitle}" />
                <h:outputText styleClass="listHeader" value="" />
                <h:outputText styleClass="listHeader" value="#{datasetForm.selectorIncludeTitle}" />
                <h:selectManyListbox size="10" styleClass="categoryListBox" value="#{manager.selector.selectedExcluded}">
                    <f:selectItems value="#{manager.selector.excluded}" />
                </h:selectManyListbox>
                <h:panelGrid columns="1">
                    <h:commandButton action="self" actionListener="#{manager.selector.includeSelected}" styleClass="categoryListButton" value=">" />
                    <h:commandButton action="self" actionListener="#{manager.selector.includeAll}" styleClass="categoryListButton" value=">>" />
                    <h:commandButton action="self" actionListener="#{manager.selector.excludeAll}" styleClass="categoryListButton" value="<<" />
                    <h:commandButton action="self" actionListener="#{manager.selector.excludeSelected}" styleClass="categoryListButton" value="<" />
                </h:panelGrid>
                <h:selectManyListbox size="10" styleClass="categoryListBox" value="#{manager.selector.selectedIncluded}">
                    <f:selectItems value="#{manager.selector.included}" />
                </h:selectManyListbox>
            </h:panelGrid>
            
            <h:panelGrid columns="3" styleClass="righted">
                <h:commandButton action="self" actionListener="#{manager.insert}" alt="#{resources.insertIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.insertIcon}" rendered="#{empty manager.dataset.originalId}" />
                <h:commandButton action="self" actionListener="#{manager.update}" alt="#{resources.updateIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.updateIcon}" rendered="#{!empty manager.dataset.originalId}" />
                <h:commandButton action="self" actionListener="#{manager.delete}" alt="#{resources.deleteIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.deleteIcon}" immediate="true" rendered="#{!empty manager.dataset.originalId}" />
                <h:commandButton action="self" actionListener="#{manager.cancel}" alt="#{resources.cancelIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.cancelIcon}" immediate="true" />
            </h:panelGrid>
        </h:panelGrid>
        
    </h:form>
</f:subview>
