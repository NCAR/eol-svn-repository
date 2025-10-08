<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core" %>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html" %>

<f:subview id="CategoryEditView">
    <f:loadBundle basename="categoryForm" var="categoryForm" />
    <f:loadBundle basename="resources" var="resources" />

    <h:form id="CategoryEditForm">
        
        <h:panelGrid columns="1" styleClass="dataForm">
            <h:inputHidden id="categoryId"
                    value="#{generalManager.category.categoryId}" />

            <h:panelGrid columns="1" styleClass="centered">
                <h:outputText styleClass="formHeader" value="Category Metadata" />
            </h:panelGrid>

            <h:panelGrid columnClasses="formTitle,formValue" columns="2" styleClass="dataFormRow">
                <h:outputText value="#{categoryForm.name}" />
                <h:inputText styleClass="fullField" value="#{generalManager.category.name}" />
                <h:outputText value="#{categoryForm.parentCategory}" />
                <h:selectOneMenu value="#{generalManager.category.parentCategoryIdString}">
                    <f:selectItems value="#{generalManager.categoryParentList}" />
                </h:selectOneMenu>
            </h:panelGrid>
            
            <h:panelGrid columns="3" styleClass="righted">
                <h:commandButton action="self" actionListener="#{generalManager.insert}" alt="#{resources.insertIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.insertIcon}" rendered="#{empty generalManager.category.categoryId}" />
                <h:commandButton action="self" actionListener="#{generalManager.update}" alt="#{resources.updateIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.updateIcon}" rendered="#{!empty generalManager.category.categoryId}" />
                <h:commandButton action="self" actionListener="#{generalManager.delete}" alt="#{resources.deleteIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.deleteIcon}" immediate="true" rendered="#{!empty generalManager.category.categoryId}" />
                <h:commandButton action="self" actionListener="#{generalManager.cancel}" alt="#{resources.cancelIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{resources.cancelIcon}" immediate="true" />
            </h:panelGrid>
        </h:panelGrid>
        
    </h:form>
</f:subview>
