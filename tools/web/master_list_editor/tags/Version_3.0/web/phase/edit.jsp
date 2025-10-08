<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core" %>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html" %>

<f:subview id="PhaseEditView">
    <f:loadBundle basename="phaseForm" var="phaseForm" />
    <f:loadBundle basename="resources" var="resources" />
    
    <h:form id="classificationEditForm">
        
        <h:panelGrid columns="1" styleClass="dataForm">
            <h:inputHidden id="phaseId" value="#{manager.phase.phaseId}" />

            <h:panelGrid columns="1" styleClass="centered">
                <h:outputText styleClass="formHeader" value="Phase Metadata" />
            </h:panelGrid>

            <h:panelGrid columnClasses="formTitle,formValue" columns="2" styleClass="dataFormRow">
                <h:outputText value="#{phaseForm.name}" />
                <h:inputText styleClass="fullField" value="#{manager.phase.name}" />
                <h:outputText value="#{phaseForm.project}" />
                <h:selectOneMenu value="#{manager.phase.projectId}">
                    <f:selectItems value="#{manager.projectSelectList}" />
                </h:selectOneMenu>
                <h:outputText value="#{phaseForm.hidden}" />
                <h:selectBooleanCheckbox value="#{manager.phase.hidden}" />
            </h:panelGrid>
            
            <h:panelGrid columns="3" styleClass="righted">
                <h:commandButton action="self" actionListener="#{manager.insert}" alt="#{resources.insertIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.insertIcon}" rendered="#{empty manager.phase.phaseId}" />
                <h:commandButton action="self" actionListener="#{manager.update}" alt="#{resources.updateIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.updateIcon}" rendered="#{!empty manager.phase.phaseId}" />
                <h:commandButton action="self" actionListener="#{manager.delete}" alt="#{resources.deleteIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.deleteIcon}" immediate="true" rendered="#{!empty manager.phase.phaseId}" />
                <h:commandButton action="self" actionListener="#{manager.cancel}" alt="#{resources.cancelIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.cancelIcon}" immediate="true" />
            </h:panelGrid>
        </h:panelGrid>
        
    </h:form>
</f:subview>
