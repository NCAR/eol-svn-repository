<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core" %>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html" %>

<f:subview id="CategoryDeleteView">
    <f:loadBundle basename="categoryForm" var="categoryForm" />
    <f:loadBundle basename="resources" var="resources" />
    
    <h:form id="CategoryDeleteForm">
        <h:inputHidden value="#{generalManager.category.categoryId}" />

        <h:panelGrid columns="1" styleClass="dataForm">
            <h:panelGrid columns="1" styleClass="centered">
                <h:outputText styleClass="formHeader" value="Category Delete Confirmation" />
            </h:panelGrid>

            <h:panelGrid columnClasses="formTitle" columns="1" styleClass="centered">
                <h:outputText value="#{generalManager.category.name}:" />
            </h:panelGrid>
            
            <h:outputText escape="false"
                    value="<p style=\"text-align: center;\"><b>Selecting <font color=\"red\"><i>Confirm</i></font> will <font color=\"red\">PERMANENTLY delete</font> all metadata for category: #{generalManager.category.name}.</b></p>" />
            
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
