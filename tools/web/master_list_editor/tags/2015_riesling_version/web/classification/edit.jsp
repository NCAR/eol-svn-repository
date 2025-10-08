<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core" %>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html" %>

<f:subview id="classificationEditView">
    <f:loadBundle basename="classificationForm" var="classificationForm" />
    <f:loadBundle basename="resources" var="resources" />

    <h:form id="classificationEditForm">
        
        <h:panelGrid columns="1" styleClass="dataForm">
            <h:inputHidden id="classificationId" value="#{manager.classification.classificationId}" />

            <h:panelGrid columns="1" styleClass="centered">
                <h:outputText styleClass="formHeader" value="Classification Metadata" />
            </h:panelGrid>

            <h:panelGrid columnClasses="formTitle,formValue" columns="2" styleClass="dataFormRow">
                <h:outputText value="#{classificationForm.name}" />
                <h:inputText styleClass="fullField" value="#{manager.classification.name}" />
                <h:outputText value="#{classificationForm.type}" />
                <h:selectOneMenu value="#{manager.classification.typeIdString}">
                    <f:selectItems value="#{manager.classificationTypeList}" />
                </h:selectOneMenu>
            </h:panelGrid>

            <h:panelGrid columns="3" styleClass="dataFormRow">
                <h:outputText styleClass="listHeader" value="#{classificationForm.selectorExcludeTitle}" />
                <h:outputText styleClass="listHeader" value="" />
                <h:outputText styleClass="listHeader" value="#{classificationForm.selectorIncludeTitle}" />
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
                <h:commandButton action="self" actionListener="#{manager.insert}" alt="#{resources.insertIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.insertIcon}" rendered="#{empty manager.classification.classificationId}" />
                <h:commandButton action="self" actionListener="#{manager.update}" alt="#{resources.updateIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.updateIcon}" rendered="#{!empty manager.classification.classificationId}" />
                <h:commandButton action="self" actionListener="#{manager.delete}" alt="#{resources.deleteIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.deleteIcon}" immediate="true" rendered="#{!empty manager.classification.classificationId}" />
                <h:commandButton action="self" actionListener="#{manager.cancel}" alt="#{resources.cancelIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.cancelIcon}" immediate="true" />
            </h:panelGrid>
        </h:panelGrid>
        
    </h:form>
</f:subview>
