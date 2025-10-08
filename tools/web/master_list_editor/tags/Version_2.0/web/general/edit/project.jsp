<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core" %>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html" %>

<f:subview id="ProjectEditView">
    <f:loadBundle basename="projectForm" var="projectForm" />
    <f:loadBundle basename="resources" var="resources" />
    
    <h:form id="ProjectEditForm">
        <h:inputHidden id="originalId"
                rendered="#{!empty generalManager.project.originalId}"
                value="#{generalManager.project.originalId}" />
        
        <h:panelGrid columns="1" styleClass="dataForm">

            <h:panelGrid columns="1" styleClass="centered">
                <h:outputText styleClass="formHeader" value="Project Metadata" />
            </h:panelGrid>

            <h:panelGrid columnClasses="formTitle,formValue" columns="2" styleClass="dataFormRow">
                <h:outputText value="#{projectForm.projectId}" />
                <h:inputText value="#{generalManager.project.projectId}" />                
                <h:outputText value="#{projectForm.displayName}" />
                <h:inputText styleClass="fullField" value="#{generalManager.project.displayName}" />
                <h:outputText value="#{projectForm.systemDirectory}" />
                <h:panelGrid columns="2" columnClasses="fullField,icon">
                    <h:inputText styleClass="fullField" value="#{generalManager.project.systemDirectory}" />
                    <h:commandButton action="self"
                            actionListener="#{generalManager.project.setDefaultSystemDirectory}"
                            alt="#{resources.defaultIconAlt}"
                            image="#{facesContext.externalContext.requestContextPath}/#{resources.defaultIcon}" />
                </h:panelGrid>
                <h:outputText value="#{projectForm.url}" />
                <h:panelGrid columns="2" columnClasses="fullField,icon">
                    <h:inputText styleClass="fullField" value="#{generalManager.project.url}" />
                    <h:commandButton action="self"
                            actionListener="#{generalManager.project.setDefaultUrl}"
                            alt="#{resources.defaultIconAlt}"
                            image="#{facesContext.externalContext.requestContextPath}/#{resources.defaultIcon}" />
                </h:panelGrid>
                <h:outputText value="#{projectForm.homePageUrl}" />
                <h:panelGrid columns="2" columnClasses="fullField,icon">
                    <h:inputText styleClass="fullField" value="#{generalManager.project.homePageUrl}" />
                    <h:commandButton action="self"
                            actionListener="#{generalManager.project.setDefaultHomePageUrl}"
                            alt="#{resources.defaultIconAlt}"
                            image="#{facesContext.externalContext.requestContextPath}/#{resources.defaultIcon}" />
                </h:panelGrid>
                <h:outputText value="#{projectForm.logoUrl}" />
                <h:inputText styleClass="fullField" value="#{generalManager.project.logoUrl}" />                
                <h:outputText value="#{projectForm.newLength}" />
                <h:inputText value="#{generalManager.project.newLength}" />
            </h:panelGrid>


            <h:panelGrid columns="3" styleClass="dataFormRow">
                <h:outputText styleClass="listHeader" value="#{projectForm.selectorExcludeTitle}" />
                <h:outputText styleClass="listHeader" value="" />
                <h:outputText styleClass="listHeader" value="#{projectForm.selectorIncludeTitle}" />
                <h:selectManyListbox size="10" styleClass="categoryListBox" value="#{generalManager.selector.selectedExcluded}">
                    <f:selectItems value="#{generalManager.selector.excluded}" />
                </h:selectManyListbox>
                <h:panelGrid columns="1">
                    <h:commandButton action="self" actionListener="#{generalManager.selector.includeSelected}" styleClass="categoryListButton" value=">" />
                    <h:commandButton action="self" actionListener="#{generalManager.selector.includeAll}" styleClass="categoryListButton" value=">>" />
                    <h:commandButton action="self" actionListener="#{generalManager.selector.excludeAll}" styleClass="categoryListButton" value="<<" />
                    <h:commandButton action="self" actionListener="#{generalManager.selector.excludeSelected}" styleClass="categoryListButton" value="<" />
                </h:panelGrid>
                <h:selectManyListbox size="10" styleClass="categoryListBox" value="#{generalManager.selector.selectedIncluded}">
                    <f:selectItems value="#{generalManager.selector.included}" />
                </h:selectManyListbox>
            </h:panelGrid>
            
            <h:panelGrid columns="3" styleClass="righted">
                <h:commandButton action="self" actionListener="#{generalManager.insert}" alt="#{resources.insertIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.insertIcon}" rendered="#{empty generalManager.project.originalId}" />
                <h:commandButton action="self" actionListener="#{generalManager.update}" alt="#{resources.updateIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.updateIcon}" rendered="#{!empty generalManager.project.originalId}" />
                <h:commandButton action="self" actionListener="#{generalManager.delete}" alt="#{resources.deleteIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.deleteIcon}" immediate="true" rendered="#{!empty generalManager.project.originalId}" />
                <h:commandButton action="self" actionListener="#{generalManager.cancel}" alt="#{resources.cancelIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.cancelIcon}" immediate="true" />
            </h:panelGrid>
        </h:panelGrid>
        
    </h:form>
</f:subview>
