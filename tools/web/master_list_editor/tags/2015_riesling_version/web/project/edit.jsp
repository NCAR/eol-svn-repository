<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core" %>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html" %>

<f:subview id="ProjectEditView">
    <f:loadBundle basename="projectForm" var="projectForm" />
    <f:loadBundle basename="resources" var="resources" />
    
    <h:form id="ProjectEditForm">
        <h:inputHidden id="originalId"
                rendered="#{!empty manager.project.originalId}"
                value="#{manager.project.originalId}" />
        
        <h:panelGrid columns="1" styleClass="dataForm">

            <h:panelGrid columns="1" styleClass="centered">
                <h:outputText styleClass="formHeader" value="Project Metadata" />
            </h:panelGrid>

            <h:panelGrid columnClasses="formTitle,formValue" columns="2" styleClass="dataFormRow">
                <h:outputText value="#{projectForm.projectId}" />
                <h:inputText value="#{manager.project.projectId}" />
                <h:outputText value="#{projectForm.systemDirectory}" />
                <h:panelGrid columns="2" columnClasses="fullField,icon">
                    <h:inputText styleClass="fullField" value="#{manager.project.systemDirectory}" />
                    <h:commandButton action="self"
                            actionListener="#{manager.project.setDefaultSystemDirectory}"
                            alt="#{resources.defaultIconAlt}"
                            image="#{facesContext.externalContext.requestContextPath}/#{resources.defaultIcon}" />
                </h:panelGrid>
                <h:outputText value="#{projectForm.url}" />
                <h:panelGrid columns="2" columnClasses="fullField,icon">
                    <h:inputText styleClass="fullField" value="#{manager.project.url}" />
                    <h:commandButton action="self"
                            actionListener="#{manager.project.setDefaultUrl}"
                            alt="#{resources.defaultIconAlt}"
                            image="#{facesContext.externalContext.requestContextPath}/#{resources.defaultIcon}" />
                </h:panelGrid>
                <h:outputText value="#{projectForm.homePageUrl}" />
                <h:panelGrid columns="2" columnClasses="fullField,icon">
                    <h:inputText styleClass="fullField" value="#{manager.project.homePageUrl}" />
                    <h:commandButton action="self"
                            actionListener="#{manager.project.setDefaultHomePageUrl}"
                            alt="#{resources.defaultIconAlt}"
                            image="#{facesContext.externalContext.requestContextPath}/#{resources.defaultIcon}" />
                </h:panelGrid>
                <h:outputText value="#{projectForm.logoUrl}" />
                <h:inputText styleClass="fullField" value="#{manager.project.logoUrl}" />                
                <h:outputText value="#{projectForm.cssUrl}" />
                <h:inputText styleClass="fullField" value="#{manager.project.cssUrl}" />
            </h:panelGrid>


            <h:panelGrid columns="3" styleClass="dataFormRow">
                <h:outputText styleClass="listHeader" value="#{projectForm.selectorExcludeTitle}" />
                <h:outputText styleClass="listHeader" value="" />
                <h:outputText styleClass="listHeader" value="#{projectForm.selectorIncludeTitle}" />
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
                <h:commandButton action="self" actionListener="#{manager.insert}" alt="#{resources.insertIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.insertIcon}" rendered="#{empty manager.project.originalId}" />
                <h:commandButton action="self" actionListener="#{manager.update}" alt="#{resources.updateIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.updateIcon}" rendered="#{!empty manager.project.originalId}" />
                <h:commandButton action="self" actionListener="#{manager.delete}" alt="#{resources.deleteIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.deleteIcon}" immediate="true" rendered="#{!empty manager.project.originalId}" />
                <h:commandButton action="self" actionListener="#{manager.cancel}" alt="#{resources.cancelIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.cancelIcon}" immediate="true" />
            </h:panelGrid>
        </h:panelGrid>
        
    </h:form>
</f:subview>
